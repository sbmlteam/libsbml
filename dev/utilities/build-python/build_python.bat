@echo off 
Setlocal EnableDelayedExpansion
SET THIS_DIR=%~dp0
SET LIBSBML_SRC=%THIS_DIR%\..\..\..\
SET BUILD_DIR=%THIS_DIR%\build_32
SET NAME=python-libsbml

pushd %THIS_DIR%\python-src

:: configure files
cmake -DSRC_DIR=%LIBSBML_SRC% -DBUILD_DIR=%BUILD_DIR% -DOUT_DIR=%THIS_DIR%\out -DNAME=%NAME% -P configure-files.cmake

:: create builds for different python versions
:: build in parallel (means that installers | wheels have to be created later)
::
:: if start is being used, the builds will be carried out in :: parallel. start needs the the argument EXIT_WHEN_DONE, 
:: otherwise the console window will remain open. 
::
:: if call is being used that argument cannot be given 
:: otherwise the build will stop. 
:: 
:: the lines below carry out the 2.5, 2.6 and 2.7 build 
:: in parallel, as well the 3.1, 3.2 and 3.3 build and the 
:: 3.4 and 3.5 build. 
::
::start buildV.bat 2 5 BUILD EXIT_WHEN_DONE
start buildV.bat 2 6 BUILD EXIT_WHEN_DONE
call buildV.bat 2 7 BUILD 
::start buildV.bat 3 1 BUILD EXIT_WHEN_DONE
::start buildV.bat 3 2 BUILD EXIT_WHEN_DONE
call buildV.bat 3 3 BUILD
start buildV.bat 3 4 BUILD EXIT_WHEN_DONE
call buildV.bat 3 5 BUILD
call buildV.bat 3 6 BUILD
call buildV.bat 3 7 BUILD

if not exist %THIS_DIR%\dist mkdir %THIS_DIR%\dist

:: wheels
call buildV.bat 2 7 WHEEL
call buildV.bat 3 3 WHEEL
call buildV.bat 3 4 WHEEL
call buildV.bat 3 5 WHEEL
call buildV.bat 3 6 WHEEL
call buildV.bat 3 7 WHEEL
move dist\*.whl %THIS_DIR%\dist

:: create installers and wheels
::call buildV.bat 2 5 INSTALLER
call buildV.bat 2 6 INSTALLER
call buildV.bat 2 7 INSTALLER
::call buildV.bat 3 1 INSTALLER
::call buildV.bat 3 2 INSTALLER
call buildV.bat 3 3 INSTALLER
call buildV.bat 3 4 INSTALLER
call buildV.bat 3 5 INSTALLER
call buildV.bat 3 6 INSTALLER
call buildV.bat 3 7 INSTALLER

:: move files
move dist\*.exe %THIS_DIR%\dist
:: rename files
cmake -DSRC_DIR=%LIBSBML_SRC% -DOUT_DIR=%THIS_DIR%\dist -P rename-files.cmake

popd