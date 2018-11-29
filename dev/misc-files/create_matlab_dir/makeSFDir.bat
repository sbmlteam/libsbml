@ echo off
REM 
REM This script creates the files to place  in matlab binding archive
REM 

SET THIS_DIR=%~dp0
SET SRC_DIR=%~dp0\..\..\..
SET MATLAB_DIR=%~dp0\..\..\..\src\bindings\matlab
SET DIST_DIR=%~dp0\matlab

IF NOT EXIST "%SRC_DIR%" goto NO_DIR

IF EXIST "%DIST_DIR%" goto CREATED
mkdir "%DIST_DIR%"

:CREATED

cd /d %DIST_DIR%
copy /y %SRC_DIR%\VERSION.txt %DIST_DIR%\.
copy /y %MATLAB_DIR%\*.m %DIST_DIR%\.
copy /y %MATLAB_DIR%\*.xml %DIST_DIR%\.
copy /y %MATLAB_DIR%\AUTHORS.txt %DIST_DIR%\.
copy /y %MATLAB_DIR%\README.txt %DIST_DIR%\.

goto DONE

:NO_DIR
echo. 
echo The directory %SRC_DIR% could not be found. 
echo We are in the wrong starting directory.
echo.
goto COMPLETE

:DONE
cd /d %THIS_DIR%

:COMPLETE
REM UNSET VARIABLES
SET THIS_DIR=
set SRC_DIR=
SET DIST_DIR=
SET MATLAB_DIR=


