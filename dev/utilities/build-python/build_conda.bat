@echo off 
Setlocal EnableDelayedExpansion
SET THIS_DIR=%~dp0
SET LIBSBML_SRC=%THIS_DIR%\..\..\..\
SET NAME=python-libsbml

pushd %THIS_DIR%

if not exist dist mkdir dist
if not exist dist\conda-x86 mkdir dist\conda-x86
if not exist dist\conda-x64 mkdir dist\conda-x64

cmake -DSRC_DIR=%LIBSBML_SRC% -DBUILD_DIR=%THIS_DIR%\python-src\build\lib.win-amd64-3.7 -DOUT_DIR=%THIS_DIR%\out -DNAME=%NAME% -P source\create-conda-archives.cmake
cmake -DSRC_DIR=%LIBSBML_SRC% -DBUILD_DIR=%THIS_DIR%\python-src\build\lib.win-amd64-3.6 -DOUT_DIR=%THIS_DIR%\out -DNAME=%NAME% -P source\create-conda-archives.cmake
cmake -DSRC_DIR=%LIBSBML_SRC% -DBUILD_DIR=%THIS_DIR%\python-src\build\lib.win-amd64-3.5 -DOUT_DIR=%THIS_DIR%\out -DNAME=%NAME% -P source\create-conda-archives.cmake
cmake -DSRC_DIR=%LIBSBML_SRC% -DBUILD_DIR=%THIS_DIR%\python-src\build\lib.win-amd64-3.3 -DOUT_DIR=%THIS_DIR%\out -DNAME=%NAME% -P source\create-conda-archives.cmake
cmake -DSRC_DIR=%LIBSBML_SRC% -DBUILD_DIR=%THIS_DIR%\python-src\build\lib.win-amd64-2.7 -DOUT_DIR=%THIS_DIR%\out -DNAME=%NAME% -P source\create-conda-archives.cmake
move *.bz2 dist\conda-x64

cmake -DSRC_DIR=%LIBSBML_SRC% -DBUILD_DIR=%THIS_DIR%\python-src\build\lib.win32-3.7 -DOUT_DIR=%THIS_DIR%\out -DNAME=%NAME% -P source\create-conda-archives.cmake
cmake -DSRC_DIR=%LIBSBML_SRC% -DBUILD_DIR=%THIS_DIR%\python-src\build\lib.win32-3.6 -DOUT_DIR=%THIS_DIR%\out -DNAME=%NAME% -P source\create-conda-archives.cmake
cmake -DSRC_DIR=%LIBSBML_SRC% -DBUILD_DIR=%THIS_DIR%\python-src\build\lib.win32-3.5 -DOUT_DIR=%THIS_DIR%\out -DNAME=%NAME% -P source\create-conda-archives.cmake
cmake -DSRC_DIR=%LIBSBML_SRC% -DBUILD_DIR=%THIS_DIR%\python-src\build\lib.win32-3.3 -DOUT_DIR=%THIS_DIR%\out -DNAME=%NAME% -P source\create-conda-archives.cmake
cmake -DSRC_DIR=%LIBSBML_SRC% -DBUILD_DIR=%THIS_DIR%\python-src\build\lib.win32-2.7 -DOUT_DIR=%THIS_DIR%\out -DNAME=%NAME% -P source\create-conda-archives.cmake
move *.bz2 dist\conda-x86

rd /s /q out
popd