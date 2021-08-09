# Description of artefacts provided by CI/CD system
The nightly build artefacts are available via the [Actions tab of the libSBML Github repository site](https://github.com/sbmlteam/libsbml/actions/workflows/store-artefact.yml) and are regenerated every 24 hours at 5 AM GMT. Their contents are described below - the [developer documentation](https://github.com/sbmlteam/libsbml/blob/development/ci.md) contains more details about the underlying CI/CD system. 

Each artefact is provided as a zip file, and has a version that includes all packages (stable and experimental) and another version that contains only the stable packages. Stable packages include the SBML `comp`, `distrib`, `fbc`, `groups`, `l3v2extendedmath`, `layout`, `multi`, `qual`, and `render` packages, while experimental packages additionally include the SBML `arrays`, `dyn`, `req`, and `spatial` packages.

Writing compressed SBML is possible with all artefacts in `.zip` and `.gz` format, and in `.bz2` format in all OS except CentOS 6.

The artefacts are built using [virtual machines provided by GitHub Actions](https://github.com/actions/virtual-environments).

## Windows
- Precompiled version of libSBML, built using the [libxml2](http://xmlsoft.org/) parser. Contains interfaces to C, C++, C# (.NET), Python, and R.

## Mac OS
- Precompiled version of libSBML, built using the [libxml2](http://xmlsoft.org/) parser. Contains interfaces to C, C++, C# (Mono), Python, and R.
- Precompiled R package for libSBML. 

## Ubuntu 16.04
- Precompiled version of libSBML, built using the [libxml2](http://xmlsoft.org/) parser. Contains interfaces to C, C++, C# (Mono), Python, and R.
- Precompiled R package for libSBML. 
  
## CentOS 6 ([ManyLinux2010](quay.io/pypa/manylinux2010_x86_64))
- Precompiled version of libSBML, built using the [libxml2](http://xmlsoft.org/) parser. Contains interfaces to C, C++, C# (Mono), Python, and R.
- Precompiled R package for libSBML (as a separate artefact). 