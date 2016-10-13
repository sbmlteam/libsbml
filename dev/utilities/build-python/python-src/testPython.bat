@echo off 
Setlocal EnableDelayedExpansion
REM This file tests the python build
REM it needs three arguments
REM 
REM - python interpreter
REM - directory of python build
REM - test directory
REM
REM 2011/02/26 Frank Bergmann
REM

REM ensure visual studio is in the path
if "%VS_VERSION%"=="" SET VS_VERSION=14
if "%INCLUDE%"=="" call "%ProgramFiles(x86)%\Microsoft Visual Studio %VS_VERSION%.0\VC\vcvarsall.bat" x86


REM set up directory variables
SET BASE_DIR=%~dp0
SET PYTHON_INTERP=%1
SET PYTHON_BUILD=%2
SET TEST_DIR=%3

if "%PYTHON_INTERP%" == "" goto MISSING_ARGS
if "%PYTHON_BUILD%" == "" goto MISSING_ARGS
if "%TEST_DIR%" == "" goto MISSING_ARGS

cd /d "%TEST_DIR%"

echo.
echo Testing Python bindings from: %PYTHON_BUILD%
echo with python:                  %PYTHON_INTERP%
echo and tests  :                  %TEST_DIR%
echo.
SET PYTHONPATH=%PYTHON_BUILD%;%PYTHON_BUILD%\libsbml;%TEST_DIR%;%TEST_DIR%\test;.;test;test\sbml;test\annotation;test\math;test\xml;
"%PYTHON_INTERP%" test.py

goto ALL_DONE

:MISSING_ARGS
echo.
echo Please call this script with three arguments: 
echo. 
echo - python interpreter
echo - libSBML python build
echo - libSBML python test directory
echo. 
goto ALL_DONE

:ALL_DONE
cd  /d %BASE_DIR%
