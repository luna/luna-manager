import contextlib
import os
import traceback
import sys


@contextlib.contextmanager
def working_directory(path):
    """A context manager which changes the working directory to the given
    path, and then changes it back to its previous value on exit.

    """
    prev_cwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)


def fail(msg, no_traceback=False):
    traceback.print_exc()
    print(msg)
    sys.exit(1)


def get_repo_path(options):
    if options.luna_studio_path:
        return options.luna_studio_path

    try:
        return os.environ['LUNA_STUDIO_PATH']
    except KeyError:
        fail('Status: failed to locate the Luna Studio repo.'
              ' Set the LUNA_STUDIO_PATH or supply the --luna-studio-path option')


def application_version(luna_studio_path):
    luna_yaml_path = os.path.join(luna_studio_path, 'luna-package.yaml')
    with open(luna_yaml_path, 'r') as f:
        try:
            conf = yaml.load(f)
            name = application_name(luna_studio_path)
            versions = list(conf['packages'][name]['versions'])
            return versions[0]
        except:
            fail('Status: failed to determine the version to upload')


def application_name(luna_studio_path):
    luna_yaml_path = os.path.join(luna_studio_path, 'luna-package.yaml')
    with open(luna_yaml_path, 'r') as f:
        try:
            conf = yaml.load(f)
            name = list(conf['apps'])
            return name[0]
        except:
            fail('Status: failed to determine the application to upload')


def package_name(luna_studio_path):
    name = application_name(luna_studio_path)
    extension = '.AppImage' if linux() else '.tar.gz'
    return name + extension


def package_path(luna_studio_path):
    name = package_name(luna_studio_path)
    if darwin():
        return os.path.join(luna_studio_path, 'dist-package', name)
    elif linux():
        return os.path.join(luna_studio_path, 'dist-package', 'appimage', 'out', name)
    else:
        return os.path.join('c', 'tmp', 'luna-package', name)
