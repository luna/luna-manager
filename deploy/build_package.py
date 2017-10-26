# work around the cwd-not-in-path problem
import os
import sys

sys.path.append(os.getcwd())

import boto3
from optparse import OptionParser
from subprocess import run
import time
import yaml

from deploy.system import osname, linux, windows, darwin
from deploy.common import fail


luna_studio_path = None
s3 = boto3.resource('s3')


def get_repo_path(options):
    if options.luna_studio_path:
        return options.luna_studio_path

    try:
        return os.environ['LUNA_STUDIO_PATH']
    except KeyError:
        fail('Status: failed to locate the Luna Studio repo.'
              ' Set the LUNA_STUDIO_PATH or supply the --luna-studio-path option')


def build_package():
    config_path = os.path.join(luna_studio_path, 'luna-package.yaml')
    try:
        print('Building Luna Manager...')
        run(['stack', 'install'], check=True)
        print('Running Luna Manager (make-package)...')
        run(['executables/luna-manager', 'make-package', config_path], check=True)
    except:
        fail('Status: failed to build package')
    else:
        print('Successfully built a Luna package.')


def application_version():
    luna_yaml_path = os.path.join(luna_studio_path, 'luna-package.yaml')
    with open(luna_yaml_path, 'r') as f:
        try:
            conf = yaml.load(f)
            name = application_name()
            versions = list(conf['packages'][name]['versions'])
            return versions[0]
        except:
            fail('Status: failed to determine the version to upload')


def application_name():
    luna_yaml_path = os.path.join(luna_studio_path, 'luna-package.yaml')
    with open(luna_yaml_path, 'r') as f:
        try:
            conf = yaml.load(f)
            name = list(conf['apps'])
            return name[0]
        except:
            fail('Status: failed to determine the application to upload')


def package_name():
    name = application_name()
    extension = '.AppImage' if linux() else '.tar.gz'
    return name + extension


def package_path():
    name = package_name()
    if darwin():
        return os.path.join(luna_studio_path, 'dist-package', name)
    elif linux():
        return os.path.join(luna_studio_path, 'dist-package', 'appimage', 'out', name)
    else:
        return os.path.join('c', 'tmp', 'luna-package', name)


def upload_to_s3():
    try:
        version = application_version()
        name = application_name()
        package = package_name()
        tarball_path = package_path()
        s3_key_path = '/'.join([osname, name, version, package])
        s3.Object('packages-luna', s3_key_path).put(Body=open(tarball_path, 'rb'))
    except:
        fail('Status: failed to upload the tarball to S3')


def upload_config_to_s3():
    try:
        config_path = os.path.join(luna_studio_path, 'config.yaml')
        s3.Object('packages-luna', 'config.yaml').put(Body=open(config_path, 'rb'))
    except:
        fail('Status: failed to upload the config to S3')


def invalidate_config():
    try:
        cf = boto3.client('cloudfront')
        cf.create_invalidation(
            DistributionId='E205Q6AEU3UZFR',
            InvalidationBatch={
                'Paths': { 'Items': ['/config.yaml'], 'Quantity': 1 },
                'CallerReference': str(time.time()),
            })
    except:
        fail('Status: failed to invalidate the config on S3')


def main():
    global luna_studio_path

    parser = OptionParser()
    parser.add_option("-p", "--luna-studio-path",
                      action="store", dest="luna_studio_path",
                      help="Path to the Luna Studio repository")
    parser.add_option("-d", "--deploy-only",
                      action="store_true", dest="deploy_only", default=False,
                      help="Don't rebuild the manager, deploy to S3 only.")
    (options, _) = parser.parse_args()
    luna_studio_path = get_repo_path(options)
    deploy_only = options.deploy_only

    # rebuild package if needed:
    if not deploy_only:
        build_package()

    # handle all the S3 deployment:
    upload_to_s3()
    upload_config_to_s3()
    invalidate_config()

    print('Status: DONE')


if __name__ == '__main__':
    main()
