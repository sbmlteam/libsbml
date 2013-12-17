@echo off
REM Description       : batch file creating the python source package on windows
REM Original author(s): Frank Bergmann <fbergman@caltech.edu>
REM Organization      : California Institute of Technology
REM 

SET CURRENT_DIR=%~dp0
SET OUT_DIR=%CURRENT_DIR%\out
SET SRC_DIR=%CURRENT_DIR%\..\..\..\src
SET BIN_DIR=C:\Development\libsbml\build\ninja
SET VCVARS=vs9_32
SET CMAKE=cmake
SET PYTHON=c:\python27_32\python


%CMAKE% -DOUT_DIR="%OUT_DIR%" -DSRC_DIR="%SRC_DIR%" -DBIN_DIR="%BIN_DIR%" -P %CURRENT_DIR%\create_package.cmake

pushd %OUT_DIR%
%PYTHON% setup.py sdist
call "%VCVARS%"
%PYTHON% setup.py bdist_windist
popd

pause 
