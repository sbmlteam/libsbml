# Developer documentation of CI/CD for libSBML
Continuous integration/continuous deployment (CI/CD) for libSMBL is implemented in [GitHub actions](https://docs.github.com/en/actions). 

This file provides a description of the jobs that make up this CI/CD system, as documentation for developers - a description of the artefacts provided to users can be found in [artefacts.md](https://github.com/sbmlteam/libsbml/blob/development/artefacts.md).

The jobs run differ depending on the event (push, PR, nightly build) and are summarised below. The YAML files triggering each CI/CD run can be found under [`./github/workflows/`](https://github.com/sbmlteam/libsbml/tree/development/.github/workflows).

Note that links herein point to the SBML Team GitHub repo default branch, and may differ in branches and forks.
## Nightly builds ([store-artefact.yml](https://github.com/sbmlteam/libsbml/actions/workflows/store-artefact.yml))

### Configurations
Nightly builds are provided for the latest Ubuntu/MacOS and Windows environments, as well as a `pep 513`-compliant build (`manylinux2010`) for backwards compatibility with older CentOS-based linux systems (CentOS 6 and more recent). All nightly builds are available via the [Actions tab of the libSBML Github repository site](https://github.com/sbmlteam/libsbml/actions/workflows/store-artefact.yml). The varying parameters are summarised below, resulting in 8 artefacts being rebuilt and uploaded every 24 hours, at 5 AM GMT.

| parameters | value(s) taken                                                     |
| ---------- | ------------------------------------------------------------------ |
| OS         | `windows-latest`, `macos-latest`, `ubuntu-16.04`, `manylinux2010`* |
| packages   | all, stable                                                        |

<sub>\*Note on ManyLinux: `manylinux2010` runs as a separate job (not as part of the strategy matrix) in the same workflow. This is because it runs inside a container, and not directly in a virtual environment provided by GitHub actions. `manylinux2010` also requires `checkout@v1` and `upload-artefact@v1`. Older ManyLinux is incompatible with GitHub Actions, as they require even older versions of `node`. A future workaround would be to run the ManyLinux job "manually" by using `docker` calls inside a more modern GitHub actions runner, instead of the current solution of using the `container: ` syntax.</sub>

The parameters kept constant in the nightly builds are:

| constant parameters | value               |
| ------------------- | ------------------- |
| bindings            | Java, C#, Python, R |
| XML parser          | libxml2             |
| namespaces          | true                |
| with examples       | true                |
| strict includes     | true                |
| C++ standard        | 98                  |

Note that the nightly builds are run only on the default branch, and only on the SBML team repo (and not on its forks).


## On push ([brief.yml](https://github.com/sbmlteam/libsbml/actions/workflows/brief.yml))
On every push, we run 8 tests configurations. These differ by the following parameters:

| parameters   | value(s) taken                                                    |
| ------------ | ----------------------------------------------------------------- |
| OS           | `windows-latest`, `macos-latest`, `ubuntu-16.04`, `manylinux2010` |
| C++ standard | 98, 20                                                            |

while these build parameters are kept constant:

| constant parameters | value                |
| ------------------- | -------------------- |
| bindings            | Java, C#, Python, R* |
| XML parser          | libxml2              |
| namespaces          | true                 |
| with examples       | true                 |
| strict includes     | true                 |
| packages            | all                  |

<sub>\* R builds only run for Ubuntu, to provide feedback more quickly.</sub>

## On PR ([extensive.yml](https://github.com/sbmlteam/libsbml/actions/workflows/extensive.yml))
Testing is more extensive when a full (non-draft) PR is opened.

This step includes 24 different configurations, 8 per OS (Windows, Mac, Ubuntu). No `manylinux` build is currently run on PR.

This is the stage where the two non-default XML parsers (`xerces`, `expat`) are also tested. As the XML parsers are largely independent of the rest of the codebase, they are only tested in one configuration/OS - with all packages, but without language bindings.

| parameters | value(s) taken                                   |
| ---------- | ------------------------------------------------ |
| OS         | `windows-latest`, `macos-latest`, `ubuntu-16.04` |
| packages   | all, stable, none                                |
| namespaces | true, false                                      |
| XML parser | libxml2, expat*, xerces*                         |

<sub>\* only with all packages and no bindings, for all OS.</sub>

| constant parameters | value               |
| ------------------- | ------------------- |
| bindings            | Java, C#, Python, R |
| C++ standard        | 98                  |
| with examples       | true                |
| strict includes     | true                |


## Notes on the CI/CD system

### Understanding the structure of the YAML files
The GitHub Actions `cmake` template was used as a basis. [The GitHub Actions introduction page](https://docs.github.com/en/actions/learn-github-actions/introduction-to-github-actions) defines the fundamental terminology needed to understand the YAML files: job, step, action. Understanding [a strategy matrix](https://docs.github.com/en/actions/learn-github-actions/managing-complex-workflows#using-a-build-matrix) is also necessary.

For libSBML, the YAML files are generally structured into:
- define strategy matrix
- checkout the repository
- dependency installation and system setup
- configure with `cmake`
- build
- test with `ctest`

The nightly build additionally uploads some artefacts at the end.

### Dependency installation
Dependency installation varies across operating systems, and this is implemented via conditions on the strategy matrix entry, e.g.
```yaml
      - name: Do something Windows-specific
        if: matrix.platform == 'windows-latest'
        run: # some windows-specific commands
```
For convenience, we set up the `ninja` generator for all runs using the [`seanmiddleditch/gha-setup-ninja`](https://github.com/marketplace/actions/install-ninja-build-tool) action.

On Unix-based systems, missing dependences are typically installed via standard package managers (`brew`, `apt-get`, `yum`). In `manylinux2010`, recent enough versions of `cmake` (via `pip`) and `swig` (compile from source) are installed in a non-standard way.

Windows builds require a precompiled dependencies folder, which is downloaded from SourceForge and cached, as well as a "manual" swig set-up by the CI. Additionally, we use the [`ilammy/msvc-dev-cmd`](https://github.com/marketplace/actions/enable-developer-command-prompt) action to ensure `msbuild` remains accessible to `cmake` (it's not by default because we use the `ninja` generator).

### CMake configuration
All runs use the `ninja` generator to configure the `cmake` build. This has the advantage of automatically parallelising the build under the hood.

### ccache
We further use `ccache` to take advantage of several runs repeatedly calling the same compilation command (currently on MacOS and Ubuntu only). Essentially, [`ccache` caches compilation outputs and creates a unique hash for each of them](https://ccache.dev/manual/4.3.html#_how_ccache_works). If there is a cache hit, the compilation is not run, but is replaced by the compilation output.
Windows builds are compiled using the MSVC static runtime (`-DWITH_STATIC_RUNTIME`). In our setup, this means that we use [GitHub actions `cache` action](https://docs.github.com/en/actions/guides/caching-dependencies-to-speed-up-workflows) to cache the `.ccache` folder (where the hashes and compilation outputs are stored) with an OS-specific key, meaning runs can take advantage of the `ccache` information from previous CI runs on the same OS to speed up compilation.

### namespaces and strict includes
All artefacts are compiled with namespaces and "strict includes" enabled. Using strict includes preserves backwards compatibility with previous routine practice to `#include <SBMLTypes.h>` to access the entirety of SBML, which was achieved via indirect includes (i.e. `SBMLTypes` would call further `#include`s). This provides a simple access to `libSBML`, but leads to longer include chains, and therefore unnecessary re-compilations, when partially re-building.

For quicker re-builds, not using strict includes accelerates compilation through constructs like
```
#ifndef LIBSBML_USE_STRICT_INCLUDES
#include not_totally_necessary.h
#endif
```
but you need to be sure that you're including everything you need directly.

### Python

Unix-based builds use the `PYTHON_USE_DYNAMIC_LOOKUP` option, which allows libSBML binaries to dynamically link to available Python libraries, instead of being tied to the Python libraries of the OS the binaries were compiled on.

### R package and bindings
Unix-based systems additionally build a binary R package in two steps: first the compiliation is configured to skip building the R binaries and create a source package instead (`-DWITH_CREATE_R_SOURCE=ON -DWITH_SKIP_R_BINARY=ON`). The binary R package is the created from the source package using the `R CMD INSTALL`.See the documentation for [creating R source packages and binary packages](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Building-binary-packages).

# Using the CI system to make a libSBML release
This section of the documentation is work-in-progress. Knowing the differences will help us develop a process to release libSBML more automatically in the future.

## Differences to last manual release
Up to libSBML 5.19.0, releases were done manually &mdash; this took a lot of time. These can be found on [Sourceforge](https://sourceforge.net/projects/sbml/files/libsbml/5.19.0/) and comprised: 

### Structure of Sourceforge libSBML release 5.19.0
- stable
    - MATLAB interface
        - MATLAB binaries for all OS (zip + tar)
    - R interface
        - R Source Package (tar)
    - Windows 
        - R installer for 32/64 bit Windows (zip)
        - 64-bit
            - Windows installer with MATLAB (exe)
            - Windows installer without MATLAB (exe)
            - Python
                - python interface installers for py2.7 and py3.6-3.9 (exe)
        - 32-bit
            - [analogous to 64-bit]
    - MacOS
        - installer (dmg, tar)
    - Linux
        - 64-bit
            - Ubuntu binaries (tar)
            - CentOS binaries (tar)
            - Debian installer (deb)
            - RPM installer (rpm)
        - 32-bit
            - [analogous to 64-bit]
    - core libSMBL source code (zip+tar)
    - full libSBML source code (zip+tar)
    - individual accepted level-3 packages source code (zip+tar)
    - libSBML documentation (zip+tar)
- experimental
    - source
        - R interface
            - R source package (tar)
        - libSBML source code (zip, tar)
        - experimental packages source code (spatial, req, dyn, arrays) (zip)
    - binaries
        - Windows
            - R interface
                - R interface windows installer (zip)
            - python
                - python interface installers (32+64-bit and py2.7+3.6-3.9) (exe)
            - installer 64-bit (zip)
            - installer 32-bit (zip)
        - MacOS
            - binaries (tar)
        - Linux
            - binaries (32+64 bit, for centOS and Ubuntu) (tar)
            - installers (32+64 bit, for Debian- and RPM-based) (deb, rpm)

Each folder also contains a `ReadMe.md` describing the contents of the folder.
All builds contain bindings for C/C++/C#(Mono)/Python/Java/Perl and most of the Ruby.

### Summary of differences of manual releases to nightly builds
The latest versions of many of the artefact provided there are now available through the nightly build of the CI system. Differences are documented below. The nightly build does **not** currently contain
- Installers (dmg, msi, rpm, deb), 
- Ruby bindings
- Perl bindings
- mechanisms to provide libSBML-python via conda or pyPI
- MATLAB interface
- precompiled dependencies for Windows
- tar archives
- ReadMe files
- 32-bit versions
- python interface installers