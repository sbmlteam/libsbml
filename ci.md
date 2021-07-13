
# Developer documentation of CI/CD for libSBML

Continuous integration/continuous deployment (CI/CD) for libSMBL is implemented in [GitHub actions](https://docs.github.com/en/actions). 

This file provides a description of the jobs that make up this CI/CD system, as documentation for developers - a description of the artefacts provided to users can be found in [artefacts.md](https://github.com/sbmlteam/libsbml/blob/development/artefacts.md).

The jobs run differ depending on the event (push, PR, nightly build) and are summarised below. The YAML files triggering each CI/CD run can be found under [`./github/workflows/`](https://github.com/sbmlteam/libsbml/tree/development/.github/workflows).

Note that links herein point to the SBML Team GitHub repo default branch, and may differ in branches and forks.
## Nightly builds ([store-artefact.yml](https://github.com/sbmlteam/libsbml/actions/workflows/store-artefact.yml))

### Configurations
Nightly builds are provided for the latest Ubuntu/MacOS and Windows environments, as well as a `pep 513`-compliant build (`manylinux2010`) for backwards compatibility with older CentOS-based linux systems (CentOS 6 and more recent). All nightly builds are available via the [Actions tab of the libSBML Github repository site](https://github.com/sbmlteam/libsbml/actions/workflows/store-artefact.yml). The varying parameters are summarised below, resulting in 8 artefacts being rebuilt and uploaded every 24 hours, at 5 AM GMT.

| parameters | value(s) taken |
|-----|------------|
| OS | `windows-latest`, `macos-latest`, `ubuntu-latest`, `manylinux2010`* |
| packages | all, stable |

<sub>\*Note on ManyLinux: `manylinux2010` runs as a separate job (not as part of the strategy matrix) in the same workflow. This is because it runs inside a container, and not directly in a virtual environment provided by GitHub actions. `manylinux2010` also requires `checkout@v1` and `upload-artefact@v1`. Older ManyLinux is incompatible with GitHub Actions, as they require even older versions of `node`. A future workaround would be to run the ManyLinux job "manually" by using `docker` calls inside a more modern GitHub actions runner, instead of the current solution of using the `container: ` syntax.</sub>

The parameters kept constant in the nightly builds are:

| constant parameters | value |
|-------------|------------|
| bindings    | Java, C#, Python, R|
| XML parser  | libxml2 |
| namespaces  | true |
| with examples    | true |
| strict includes  | true |

Note that the nightly builds are run only on the default branch, and only on the SBML team repo (and not on its forks). For Unix-based systems, R binaries are also provided.


## On push (brief.yml)

The 6 most important configurations are tested at every push. These are:

OS
C++ standard
Packages
Bindings
XML parser

The workflow file describing these configurations can be found [here](https://github.com/sbmlteam/libsbml/actions/workflows/brief.yml).

## On PR (extensive.yml)

Testing is more extensive when a full (non-draft) PR is opened. This includes 24 different configurations, 8 per operating system.

beyond the 6, two additional runs are made to check the compatibility of the PR with the two non-default XML parsers.

The workflow file describing these configurations can be found [here](https://github.com/sbmlteam/libsbml/actions/workflows/extensive.yml).

What is the reasoning behind the more extensive testing.

## Understanding the structure of the YAML files

Ninja + other third-party actions used.
ccache.

### Further reading

Documentation for [creating R source packages and binary packages](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Building-binary-packages).


# Using the CI system to make a libSBML release

This section of the documentation is work-in-progress. Knowing the differences will help us develope a process to release libSBML more automatically in the future.

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