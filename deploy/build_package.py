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
from deploy.common import (
    fail, get_repo_path, application_version, application_name,
    package_name, package_path
)


s3 = boto3.resource('s3')


def build_package(luna_studio_path):
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


def upload_to_s3(luna_studio_path):
    try:
        version = application_version(luna_studio_path)
        name = application_name(luna_studio_path)
        package = package_name(luna_studio_path)
        tarball_path = package_path(luna_studio_path)
        s3_key_path = '/'.join([osname, name, version, package])
        s3.Object('packages-luna', s3_key_path).put(Body=open(tarball_path, 'rb'))
    except:
        fail('Status: failed to upload the tarball to S3')


def upload_config_to_s3(luna_studio_path):
    try:
        config_path = os.path.join(luna_studio_path, 'config.yaml')
        s3.Object('packages-luna', 'config.yaml').put(Body=open(config_path, 'rb'))
    except:
        fail('Status: failed to upload the config to S3')


def invalidate_config(luna_studio_path):
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

def deploy_to_s3(luna_studio_path):
    upload_to_s3(luna_studio_path)
    upload_config_to_s3(luna_studio_path)
    invalidate_config(luna_studio_path)

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
        build_package(luna_studio_path)

    # handle all the S3 deployment:
    deploy_to_s3(luna_studio_path)

    print('Status: DONE')


if __name__ == '__main__':
    main()
