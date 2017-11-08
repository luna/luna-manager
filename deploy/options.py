import os


def get_repo_path(options):
    if options.luna_studio_path:
        return options.luna_studio_path

    try:
        return os.environ['LUNA_STUDIO_PATH']
    except KeyError:
        fail('Status: failed to locate the Luna Studio repo.'
              ' Set the LUNA_STUDIO_PATH or supply the --luna-studio-path option')

