import os
import sys

sys.path.append(os.getcwd())

from argparse import ArgumentParser
from subprocess import run

from deploy.common import fail, get_repo_path, application_version
from deploy.build_package import deploy_to_s3, build_package


def prepare_version(luna_studio_path):
    """Get the next version number using luna manager and tag the appropriate commit"""
    luna_package_yaml = os.path.join(luna_studio_path, 'luna-package.yaml')
    try:
        run(['executables/luna-manager', 'next-version', luna_package_yaml])
    except:
        fail('Status: failed to get the next version with luna-manager')

    version = application_version(luna_studio_path)

    try:
        run(['git', 'tag', version])
        run(['git', 'push', 'origin', 'tags'])
    except:
        fail('Status: failed to tag the commit. Please try to do so manually.')


def run_deploy(luna_studio_path, upgrade_version=False):
    # only bump the version if explicitly ordered to do so:
    if upgrade_version:
        prepare_version(luna_studio_path)

    # TODO[piotrMocz]: here is a good place to launch the builds
    # on the remote machines.
    build_package(luna_studio_path)
    deploy_to_s3(luna_studio_path)

def main():
    parser = ArgumentParser(description='Deploy luna studio end-to-end.')
    parser.add_argument('luna_studio_path', help='Path to the Luna Studio repository')
    parser.add_argument('-u', '--upgrade-version',
                      action='store_true', dest='upgrade_version', default=False,
                      help='Do we want to create a new, upgraded version number for this one?')
    args = parser.parse_args()
    luna_studio_path = get_repo_path(args)

    run_deploy(luna_studio_path, upgrade_version=args.upgrade_version)


if __name__ == '__main__':
    main()