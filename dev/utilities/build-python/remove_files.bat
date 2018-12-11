@echo off 
:: removes all files that are no longer needed

Setlocal EnableDelayedExpansion
SET THIS_DIR=%~dp0

SET MODE=%1
:: supported Modes are
::          ALL     - delete all files created
::          BUILD   - delete build dirs
::          INSTALL - delete install dirs
::          TEMP    - delete python temp files and dirs
if "%MODE%"=="" SET MODE=ALL


pushd %THIS_DIR%

if "%MODE%"=="BUILD" goto BUILD
if "%MODE%"=="INSTALL" goto INSTALL


if exist swig rd /s /q swig
del /q python-src\*.cpp
del /q python-src\*.h
del /q python-src\*.py
del /q python-src\*.bak
del /q python-src\*.pth

if exist python-src\b32_25 rd /s /q python-src\b32_25
if exist python-src\b32_26 rd /s /q python-src\b32_26
if exist python-src\b32_27 rd /s /q python-src\b32_27
if exist python-src\b32_31 rd /s /q python-src\b32_31
if exist python-src\b32_32 rd /s /q python-src\b32_32
if exist python-src\b32_33 rd /s /q python-src\b32_33
if exist python-src\b32_34 rd /s /q python-src\b32_34
if exist python-src\b32_35 rd /s /q python-src\b32_35
if exist python-src\b32_36 rd /s /q python-src\b32_36
if exist python-src\b32_37 rd /s /q python-src\b32_37
if exist python-src\b64_25 rd /s /q python-src\b64_25
if exist python-src\b64_26 rd /s /q python-src\b64_26
if exist python-src\b64_27 rd /s /q python-src\b64_27
if exist python-src\b64_31 rd /s /q python-src\b64_31
if exist python-src\b64_32 rd /s /q python-src\b64_32
if exist python-src\b64_33 rd /s /q python-src\b64_33
if exist python-src\b64_34 rd /s /q python-src\b64_34
if exist python-src\b64_35 rd /s /q python-src\b64_35
if exist python-src\b64_36 rd /s /q python-src\b64_36
if exist python-src\b64_37 rd /s /q python-src\b64_37
if exist python-src\build rd /s /q python-src\build
if exist python-src\libsbml rd /s /q python-src\libsbml
if exist python-src\libsbml.egg-info rd /s /q python-src\libsbml.egg-info
if exist python-src\dist rd /s /q python-src\dist
if "%MODE%"=="TEMP" goto DONE

:BUILD
if exist build_32 rd /s /q build_32
if exist build_64 rd /s /q build_64
if "%MODE%"=="BUILD" goto DONE

:INSTALL
if exist install_32 rd /s /q install_32
if exist install_64 rd /s /q install_64
if "%MODE%"=="INSTALL" goto DONE


:DONE
if exist out rd /s /q out

popd