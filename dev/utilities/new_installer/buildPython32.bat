@echo off 

REM This file will build the python source dist 32 bit
REM
REM 2011/02/26 Frank Bergmann
REM

REM ensure visual studio is in the path
if "%INCLUDE%"=="" call vs10.bat

REM set up directory variables
SET BASE_DIR=%~dp0

REM check argument
SET PYTHON_INTERP=%1
if "%PYTHON_INTERP%" == "" goto MISSING_INTERP

REM goto path
cd  /d %BASE_DIR%

REM create build dir
mkdir b32
cd b32
cmake -G "NMake Makefiles"  -DPYTHON_EXECUTABLE="%PYTHON_INTERP%" ..
nmake

goto ALL_DONE

:MISSING_INTERP
echo.
echo Please provide the python interpreter to use as first argument.
echo. 
goto ALL_DONE

:ALL_DONE
cd  /d %BASE_DIR%
