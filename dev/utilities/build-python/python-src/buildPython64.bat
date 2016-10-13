@echo off 
Setlocal EnableDelayedExpansion

REM This file will build the python source dist 64 bit
REM
REM 2011/02/26 Frank Bergmann
REM

REM ensure visual studio is in the path
if "%VS_VERSION%"=="" SET VS_VERSION=14
if "%INCLUDE%"=="" call "%ProgramFiles(x86)%\Microsoft Visual Studio %VS_VERSION%.0\VC\vcvarsall.bat" amd64

REM set up directory variables
SET BASE_DIR=%~dp0

REM check argument
SET PYTHON_INTERP=%1
if "%PYTHON_INTERP%" == "" goto MISSING_INTERP


SET DEP_DIR=%2
if "%DEP_DIR%" == "" SET DEP_DIR=../../../win32/

SET MAJOR=%3
SET MINOR=%4

SET BUILD_DIR=b64_%MAJOR%%MINOR%

REM goto path
pushd %BASE_DIR%

REM create build dir
if not exist %BUILD_DIR% mkdir %BUILD_DIR%
cd %BUILD_DIR%
cmake -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release -DPYTHON_EXECUTABLE="%PYTHON_INTERP%"  -DDEP_DIR="%DEP_DIR%" ..
nmake

goto ALL_DONE

:MISSING_INTERP
echo.
echo Please provide the python interpreter to use as first argument.
echo. 
goto ALL_DONE

:ALL_DONE
popd
