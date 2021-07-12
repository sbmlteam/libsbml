
# CI/CD for libSBML documentation

Continuous integration/continuous deployment (CI/CD) for libSMBL is implemented in [GitHub actions](https://docs.github.com/en/actions). This file provides a description of the jobs that make up this CI/CD system. Note that links herein point to the SBML Team GitHub repo default branch, and may differ in branches and forks.

The jobs run differ depending on the event (push, PR, nightly build) and are summarised below. The YAML files triggering each CI/CD run can be found under [`./github/workflows/`](https://github.com/sbmlteam/libsbml/tree/development/.github/workflows).

## Nightly builds ([store-artefact.yml](https://github.com/sbmlteam/libsbml/actions/workflows/store-artefact.yml))

### Configurations
Nightly builds are provided for the latest Ubuntu/MacOS and Windows environments, as well as a `pep 513`-compliant build (`manylinux2010`) for backwards compatibility with older CentOS-based linux systems (CentOS 6 and more recent). All nightly builds are available via the [Actions tab of the libSBML Github repository site](https://github.com/sbmlteam/libsbml/actions/workflows/store-artefact.yml). The varying parameters are summarised below, resulting in 8 artefacts being rebuilt and uploaded every 24 hours.

| parameters | value(s) taken |
|-----|------------|
| OS | `windows-latest`, `macos-latest`, `ubuntu-latest`, `manylinux2010`* |
| packages | all, stable |

<sub>\* `manylinux2010` runs as a separate job (not as part of the strategy matrix) in the same workflow. This is because it runs inside a container, and not directly in a virtual environment provided by GitHub actions. </sub>

The parameters kept constant in the nightly builds are:

| constant parameters | value |
|-------------|------------|
| bindings    | Java, C#, Python|
| XML parser  | libxml2 |
| namespaces  | true |
| with examples    | true |
| strict includes  | true |

Note that the nightly builds are run only on the default branch, and only on the SBML team repo (and not on its forks).


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
Manylinux2010 needs v1 of upload-artefact, which complicated things a little bit.

### Further reading

Documentation for [creating R source packages and binary packages](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Building-binary-packages).
