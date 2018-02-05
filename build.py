import argparse
import github3
import os
import platform
import subprocess

production_branch = 'production'


def write_version(v: str, path: str) -> bool:
    """Write a new version to the `package.yaml` file.

    :param v: the version to write
    :param path: the path to the `package.yaml` file
    :return: True if the version already exists, false otherwise.
    """
    lines = None
    with open(path, 'r') as f:
        lines = f.readlines()

    if not lines:
        raise Exception('Failed to read the package.yaml file.')

    try:
        assert lines[1].startswith('version')
        h, old_v, n = lines[1].split('"')
        if old_v == v:
            return True
        lines[1] = '"'.join([h, v, n])
    except Exception as e:
        raise Exception('Failed to substitute the version') from e

    with open(path, 'w') as f:
        f.writelines(lines)

    return False


def git(command: str, *args):
    """Run a git command using the provided args"""
    subprocess.run(['git', command] + list(args))


def commit(v: str, path: str) -> None:
    """Handle the git part of creating the new version

    :param v: the version to write
    :param path: the path to the `package.yaml` file
    """
    git('checkout', production_branch)
    git('add', path)
    commit_msg = 'New version: ' + v
    git('commit', '-m', commit_msg)
    git('tag', v)
    git('push', 'origin', production_branch)
    git('push', 'origin', '--tags')


def get_name(version: str) -> str:
    """Get the name for the new manager, based on the OS and the version."""
    base_name = 'luna-manager'
    osname = platform.system().lower()
    return "-".join([base_name, osname, version])


def get_release(repo: str, tag: str, draft: bool=False) -> github3.repos.release.Release:
    """If the release already exists on GitHub, fetch and return it. Create
    a new release otherwise.

    :param repo: a `Repository` object obtained by calling: github3.login.repository
    :param tag: the tag with the release version to find
    :param draft: whether this release should be a draft (not visible to other users)
    :return : a `Release` object.
    """
    for r in repo.iter_releases():
        if r.tag_name == tag:
            return r

    return repo.create_release(tag_name=tag, draft=draft, prerelease=True)


def deploy(version: str, draft: bool=False) -> None:
    """Create a GitHub release for the newly built package."""
    binary_path = os.path.join('executables', 'luna-manager')
    tkn = os.environ.get('GITHUB_TOKEN')
    if not tkn:
        raise Exception('The GITHUB_TOKEN env variable not set.')
    print('token: ', tkn)
    gh = github3.login(token=tkn)
    if not gh:
        raise Exception('Failed to login to GitHub')
    repo = gh.repository('luna', 'luna-manager')
    if not repo:
        raise Exception('Failed to find the repository')
    release = get_release(repo, version, draft)
    if not release:
        raise Exception('Failed to get the release from GitHub')
    name = get_name(version)
    with open(binary_path, 'rb') as asset:
        release.upload_asset(name=name, asset=asset, content_type='application_binary')


def new_version(version: str) -> None:
    """Create the new version (modify package.yaml, tag, commit and push)"""
    package_yaml_path = os.path.join('luna-manager', 'package.yaml')
    print('Creating a new version: {}.'.format(version))
    ver_exists = write_version(version, package_yaml_path)

    if ver_exists:
        print('Version is already commited.')
    else:
        print('Commiting and tagging the new version.')
        commit(version, package_yaml_path)

    print('Building the application.')
    subprocess.run(['stack', 'install'])


def run(version: str, dry_run: bool=False, draft: bool=False, deploy_only: bool=False) -> None:
    if not deploy_only:
        new_version(version)

    if not dry_run:
        print('Deploying the release to GitHub.')
        deploy(version, draft=draft)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Build and deploy the Luna Manager')
    parser.add_argument('version', metavar='VERSION',
                        help='The version to create in the plaintext form.')
    parser.add_argument('--draft', dest='draft', default=False, action='store_true',
                        help='Create a draft release instead of the real one')
    parser.add_argument('--dry-run', dest='dry_run', default=False, action='store_true',
                        help='Create the new version without deploying anything')
    parser.add_argument('--deploy-only', dest='deploy_only', default=False, action='store_true',
                        help='Deploy the existing version to GitHub.')
    args = parser.parse_args()

    run(args.version, dry_run=args.dry_run, draft=args.draft, deploy_only=args.deploy_only)
