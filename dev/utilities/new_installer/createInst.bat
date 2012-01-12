@echo off 

REM This file will create the installers
REM
REM 2011/02/26 Frank Bergmann
REM

SET COMPILER=C:\Program Files (x86)\Inno Setup 5\ISCC.exe
REM SET DROP_DIR=C:\Users\fbergmann\Documents\My Dropbox\libsbml-dist
SET DROP_DIR=C:\Development\libsbml-dist

REM ensure visual studio is in the path
if "%INCLUDE%"=="" call vs10.bat

REM set up directory variables
SET THIS_BASE_DIR=%~dp0

REM if arg1 is skip build we don't have to build 
if "%1" == "--skip-build" goto BUILD_COMPLETE
if "%1" == "--skip-all-build" goto BUILD_ALL_COMPLETE
cd /d %THIS_BASE_DIR%

call buildAll.bat
call buildPython.bat

:BUILD_COMPLETE
REM copy files
cd /d %THIS_BASE_DIR%

REM configure
cmake .

REM create matlab installers
cd /d %THIS_BASE_DIR%\matlab
call createInstallers.bat
REM copy matlab installers to share
copy /y Output\*.exe "%DROP_DIR%\matlab-win-installer"

:BUILD_ALL_COMPLETE
cd /d %THIS_BASE_DIR%
REM create 32 bit inst
rd /s /q libsbml
call createXMLRel_x86.bat
"%COMPILER%" install_libsbml_script_xml_win32.iss

REM create 64 bit inst
rd /s /q libsbml
call createXMLRel_x64.bat
"%COMPILER%" install_libsbml_script_xml_win64.iss

REM copy files
copy /y Output\*.exe "%DROP_DIR%"
