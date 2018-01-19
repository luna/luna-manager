<p align="center">
  <img src="https://github.com/luna/luna-studio/raw/master/resources/logo.ico" style="margin: 0 auto;">
</p>
<h1 align="center">Luna Manager</h1>
The Luna Manager provides various utilities for managing Luna installations as well as creating and managing new versions.

Even though most of the users should simply use the graphical installer, for Luna development and more detailed management, the command line tool provides a variety of additional commands.

### Quick download

The graphical installer can be downloaded using the following links:
* Mac OS: https://s3-us-west-2.amazonaws.com/packages-luna/darwin/lunaInstaller.zip
* Linux: https://s3-us-west-2.amazonaws.com/packages-luna/linux/lunaInstaller.zip
* Windows: https://s3-us-west-2.amazonaws.com/packages-luna/windows/lunaInstaller.zip

If you only need the command line installer, we are maintaining up-to-date prebuilt binaries for each system:
* Mac OS: https://s3-us-west-2.amazonaws.com/packages-luna/darwin/lunaManager.zip
* Linux: https://s3-us-west-2.amazonaws.com/packages-luna/linux/luna-manager
* Windows: https://s3-us-west-2.amazonaws.com/packages-luna/windows/luna-manager.exe

## Command reference

### Install
The primary command provided by the Manager, used to download and install the packages. The basic usage is the following:
```
$ luna-manager install
```
This will result in an interactive installation process, allowing you to specify the component to install, the version and the path under which to install it. It will also ask for your email, for analytics sake (you can leave it blank, although it is not recommended: knowing the email allows us to identify the users and address the individual issues, while not gathering any private information). Alternatively, each of the aforementioned parameters can be explicitly passed to the command:
```
$ luna-manager install --component luna-studio --path /Applications --version 0.9.9
```
By default, Luna Manager only shows the stable, `release` builds when asking for a version. You can override this behaviour by supplying the `--nightly` or `--dev` flag (to also include the nightly and developer builds, respectively).

The installation process may take up to several minutes, depending on your internet connection and computer speed.

### Develop

Used for painlessly spinning up the development environment. The default usage:
```
$ luna-manager develop COMPONENT
```
where component can be either `luna-studio` or `luna`. It will create a fully functional repository under `${HOME}/luna-develop/apps/${COMPONENT}`. If you want to customize the path, you can supply the `--path` option. In the following sections we will be referring to this path (i.e. `${HOME}/luna-develop/apps/luna-studio` or your custom location) as `${LUNA_STUDIO_REPO}`.

When you already have your repo and simply want to download the external dependencies, you can use the `--download-dependencies` flag.

### Make package

Used for creating the package with the Luna (Studio) distribution. A package, once published, can be later downloaded and installed using the Luna Installer's `install` command. In order to create a package, you need to have a fully configured `luna-studio` repo (the easiest way to obtain it is by using the Luna Manager's `develop` command). The information about the version is stored in the `luna-studio` repo in the `luna-package.yaml` version. Next, you need to tag with the version number the commit you wish to create the version from. You can create the version number and tag manually, but if you want to bump the currently newest version, you can use the `luna-manager next-version` command described below, which will take care of all the "accounting". You can make the package with the following command:
```
$ luna-manager make-package ${LUNA_STUDIO_REPO}/luna-package.yaml --verbose
```

The `verbose` option is useful when debugging problems with the build.

#### Tag-based flow and versions
The Luna ecosystem uses a specific form of versioning:
* `x.y` to mark stable releases
* `x.y.z` for nightly builds (tested to some extent, but not recommended for production environments)
* `x.y.z.w` for developer builds, built frequently and not tested thoroughly.

When creating a new version, we use `git` tags to link the versions with the commits they were built from. This means that, for example, when creating a `1.0.0.1` version, you would checkout to a commit you want to publish and run `git tag 1.0.0.1`.

If you want to skip the tagging flow altogether, you can supply the `--build-from-head` parameter to the `make-package` command, which will cause the package to be built from the latest commit.

### Next version
As described above, the process of creating a new version can be quite cumbersome. To address that, we created a `luna-manager next-version` command, which greatly simplifies the whole process. When you run:
```
$ luna-manager next-version ${LUNA_STUDIO_REPO}/luna-package.yaml
```
You will get the repo ready to create a next developer build (the number will be set automatically, and a new commit with the appropriate tag containing the version bump will be made). If you want to create a nightly or a release build, you can supply the `--nightly` and `--release` options, respectively.

## Building
To build Luna Manager all you need is `stack` installed on your machine. To install stack, simply follow the instructions at https://docs.haskellstack.org/en/stable/README/. Remember that in order to run stack-installed executables, you need to add `${HOME}/.local/bin` to your `${PATH}`.

One prerequisite that will not be installed by `stack` as a dependency is the `happy` executable, which you can obtain by typing (inside your home directory):
```
$ cd ${HOME}
$ stack install happy
```

Then you can clone the repository and simply run:
```
$ git clone https://github.com/luna/luna-manager
$ cd luna-manager
$ stack install
```
The resulting `luna-manager` binary will be created in the `executables` directory in the repo; you may choose to add it to your path or invoke it directly.
