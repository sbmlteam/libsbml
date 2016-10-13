## Dependencies
For the build to be working you will need to have the following installed: 

* cmake. After the installation the `cmake` executable is expected to be in the `PATH`. 
* Visual Studio: by default these scripts assume VS 2015 will be used. 
* libSBML-dependencies built for the VS version you are using (it will be using the static runtime) by default the scripts are expected to be in `\Development\libSBML-dependencies\install_vs14_release_x86_static` and `\Development\libSBML-dependencies\install_vs14_release_x86_static`. 
* python 2.6, 2.7, 3.0, 3.1, 3.2, 3.3, 3.4, 3.5 (both 32 nd 64 bit): The scripts will assume that those files are in `c:\python%version%_%arch%`, so for example `c:\python27_32` for the python 2.7 32 bit version. 
* for the python version2 2.6, 2.7, 3.3, 3.4 and 3.5 you will need to have wheel installed, to create windows binary wheels. 

## Customizing the scripts
Some customization of the scripts will be needed in case the conventions are not met. Specifically: 

* `build_libsbml.bat`: here you want to adjust the `SWIG`, `CMAKE_OPTIONS` for the build, the `PYTHON` location as well as the `DEPENDENCY_DIR` variables. The script also uses Ninja as build tool which allows for parallel compilation of libsbml. If you would prefer to use NMake, you want to adjust the `BUILD_COMMAND` and `GENERATOR` variable as well.
* `build_python.bat`: by default three python builds will happen in parallel. If you prefer those builds to be run sequentially, then change the `start` statements to be `call` statements. 
* `python-src\buildV.bat`: this script is given the python major and minor version. Based on these you should be setting the `PYTHON_32` and `PYTHON_64` bit variable.  

## Creating Windows Python binaries
The normal setuptools build process does not work for windows (as we have too many files that are passed along on the command line by setuptools). Thus these scripts are used to create the python binaries: 


### build_libsbml.bat
This script basically creates a static build of libSBML (32 and 64 bit). The top of the file contains a number of variables that have to be adapted to the individual platform.    

### build_python.bat
This script will create the different python binaries out of the files created by `build_libsbml.bat`, and once done will create the windows installers and python wheels.

### build_conda.bat
This script transforms the binaries build by `build_python.bat` and wraps them into anaconda files. 

### remove_files.bat
This script removes all the temporary files created in the process of building the python bindings. The only exception being the `dist` folder that will hold the result of the build.    

### build_all.bat
This script will call all the other scripts in order, building libSBML, creating the python binaries, and the conda packages. Finally all temporary files will be removed. 

## Support files
Additionally this directory contains a number of support files and directories: 

* `python-src` this directory contains all the files needed to build the python files, installers and wheels
* `create-conda-archives.cmake` and the `source` directories are there to create the conda binaries. 


---
10/13/2016 3:33:18 PM Frank T. Bergmann