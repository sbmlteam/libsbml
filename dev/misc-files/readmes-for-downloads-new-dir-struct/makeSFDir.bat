@ echo off
REM 
REM This script creates the directory structure for SF
REM 

SET THIS_DIR=%~dp0
set VERSION=5.17.0
SET DIST_DIR=%~dp0\%VERSION%
SET TEST_DIR=%~dp0\stable

IF NOT EXIST "%TEST_DIR%" goto NO_DIR

IF EXIST "%DIST_DIR%" goto CREATED
mkdir "%DIST_DIR%"

:CREATED

cd /d %DIST_DIR%
mkdir experimental
mkdir experimental\binaries
mkdir "experimental\binaries\Linux"
mkdir "experimental\binaries\Mac OS X"
mkdir "experimental\binaries\Windows"
mkdir "experimental\binaries\Windows\R interface"
mkdir "experimental\binaries\Windows\python"

mkdir experimental\source
mkdir "experimental\source\R interface"

mkdir stable
mkdir "stable\Linux"
mkdir "stable\Linux\32-bit"
mkdir "stable\Linux\64-bit"
mkdir "stable\Mac OS X"
mkdir "stable\Windows"
mkdir "stable\Windows\32-bit"
mkdir "stable\Windows\32-bit\R interface"
mkdir "stable\Windows\32-bit\python"
mkdir "stable\Windows\64-bit"
mkdir "stable\Windows\64-bit\R interface"
mkdir "stable\Windows\64-bit\python"
mkdir "stable\Windows\R interface"
mkdir "stable\MATLAB interface"
mkdir "stable\R interface"



copy /y %THIS_DIR%\README.md .
copy /y %THIS_DIR%\experimental\README.md %DIST_DIR%\experimental\.
copy /y %THIS_DIR%\experimental\binaries\README.md %DIST_DIR%\experimental\binaries\.
copy /y %THIS_DIR%\experimental\binaries\linux\README.md "%DIST_DIR%\experimental\binaries\Linux\."
copy /y %THIS_DIR%\experimental\binaries\macos\README.md "%DIST_DIR%\experimental\binaries\Mac OS X\."
copy /y %THIS_DIR%\experimental\binaries\windows\README.md "%DIST_DIR%\experimental\binaries\Windows\."
copy /y %THIS_DIR%\experimental\binaries\windows\R_interface\README.md "%DIST_DIR%\experimental\binaries\Windows\R interface\."
copy /y %THIS_DIR%\experimental\binaries\windows\python\README.md "%DIST_DIR%\experimental\binaries\Windows\python\."

copy /y %THIS_DIR%\experimental\source\README.md %DIST_DIR%\experimental\source\.
copy /y %THIS_DIR%\experimental\source\R_interface\README.md "%DIST_DIR%\experimental\source\R interface\."

copy /y %THIS_DIR%\stable\README.md %DIST_DIR%\stable\.
copy /y %THIS_DIR%\stable\linux\README.md "%DIST_DIR%\stable\Linux\."
copy /y %THIS_DIR%\stable\linux\32-bit\README.md "%DIST_DIR%\stable\Linux\32-bit\."
copy /y %THIS_DIR%\stable\linux\64-bit\README.md "%DIST_DIR%\stable\Linux\64-bit\."
copy /y %THIS_DIR%\stable\macos\README.md "%DIST_DIR%\stable\Mac OS X\."
copy /y %THIS_DIR%\stable\windows\README.md "%DIST_DIR%\stable\Windows\."
copy /y %THIS_DIR%\stable\windows\32-bit\README.md "%DIST_DIR%\stable\Windows\32-bit\."
copy /y %THIS_DIR%\stable\windows\32-bit\R_interface\README.md "%DIST_DIR%\stable\Windows\32-bit\R interface\."
copy /y %THIS_DIR%\stable\windows\32-bit\python\README.md "%DIST_DIR%\stable\Windows\32-bit\python\."
copy /y %THIS_DIR%\stable\windows\64-bit\README.md "%DIST_DIR%\stable\Windows\64-bit\."
copy /y %THIS_DIR%\stable\windows\64-bit\R_interface\README.md "%DIST_DIR%\stable\Windows\64-bit\R interface\."
copy /y %THIS_DIR%\stable\windows\64-bit\python\README.md "%DIST_DIR%\stable\Windows\64-bit\python\."
copy /y %THIS_DIR%\stable\windows\64-bit\R_interface\README.md "%DIST_DIR%\stable\Windows\R interface\."
copy /y %THIS_DIR%\stable\MATLAB_interface\README.md "%DIST_DIR%\stable\MATLAB interface\."
copy /y %THIS_DIR%\stable\R_interface\README.md "%DIST_DIR%\stable\R interface\."

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


