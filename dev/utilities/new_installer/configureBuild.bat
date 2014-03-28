@echo off 

REM This file will configure the libSBML build for windows (32 and 64 bit)
REM
REM 2011/08/17 Frank T. Bergmann
REM

REM ensure visual studio is in the path
if "%INCLUDE%"=="" call vs10.bat

REM set up directory variables
SET BASE_DIR=%~dp0
SET LIBSBML_ROOT=%BASE_DIR%\..\..\..\

cd /D %BASE_DIR%
cmake -P configure-build.cmake
