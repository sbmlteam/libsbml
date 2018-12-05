@ echo off
REM 
REM This script creates the directory structure for SF
REM 

SET THIS_DIR=%~dp0
set VERSION=5.17.2-experimental
SET DIST_DIR=%~dp0\%VERSION%
SET TEST_DIR=%~dp0\source

IF NOT EXIST "%TEST_DIR%" goto NO_DIR

IF EXIST "%DIST_DIR%" goto CREATED
mkdir "%DIST_DIR%"

:CREATED

cd /d %DIST_DIR%
mkdir binaries
mkdir "binaries\Linux"
mkdir "binaries\Mac OS X"
mkdir "binaries\Windows"
mkdir "binaries\Windows\R interface"
mkdir "binaries\Windows\python"

mkdir source

mkdir "source\accepted packages"
mkdir "source\beta packages"
mkdir "source\R interface"


copy /y %THIS_DIR%\README.md .

copy /y %THIS_DIR%\binaries\README.md %DIST_DIR%\binaries\.
copy /y %THIS_DIR%\binaries\linux\README.md "%DIST_DIR%\binaries\Linux\."
copy /y %THIS_DIR%\binaries\mac_osx\README.md "%DIST_DIR%\binaries\Mac OS X\."
copy /y %THIS_DIR%\binaries\windows\README.md "%DIST_DIR%\binaries\Windows\."
copy /y %THIS_DIR%\binaries\windows\R_interface\README.md "%DIST_DIR%\binaries\Windows\R interface\."
copy /y %THIS_DIR%\binaries\windows\python\README.md "%DIST_DIR%\binaries\Windows\python\."


copy /y %THIS_DIR%\source\README.md %DIST_DIR%\source\.
copy /y %THIS_DIR%\source\accepted-packages\README.md "%DIST_DIR%\source\accepted packages\."
copy /y %THIS_DIR%\source\beta-packages\README.md "%DIST_DIR%\source\beta packages\."
copy /y %THIS_DIR%\source\R_interface\README.md "%DIST_DIR%\source\R interface\."

goto DONE

:NO_DIR
echo. 
echo The directory %TEST_DIR% could not be found. 
echo We are in the wrong starting directory.
echo.
goto COMPLETE

:DONE
cd /d %THIS_DIR%

:COMPLETE
REM UNSET VARIABLES
SET THIS_DIR=
set VERSION=
SET DIST_DIR=
SET TEST_DIR=


