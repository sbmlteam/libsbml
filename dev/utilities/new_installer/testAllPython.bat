@echo off 

REM This file tests all python builds
REM 
REM 2011/02/26 Frank Bergmann
REM

REM ensure visual studio is in the path
if "%INCLUDE%"=="" call vs10.bat

REM set up directory variables
SET BASE_DIR=%~dp0
cd  /d %BASE_DIR%

SET LIBSBML_ROOT=%BASE_DIR%\..\..\..\
SET INSTALL_DIR_32=c:\32bit
set SOURCE_32=%INSTALL_DIR_32%\bindings\python\src

REM this assumes that libsbml was previously build
SET PYTHON_25_32=C:\Python25\python.exe
SET PYTHON_25_64=C:\Python25_64\python.exe
SET PYTHON_26_32=C:\Python26_32\python.exe
SET PYTHON_26_64=C:\Python26\python.exe
SET PYTHON_27_32=C:\Python27\python.exe
SET PYTHON_27_64=C:\Python27_64\python.exe

call "%BASE_DIR%\testPython.bat" %PYTHON_25_32% %SOURCE_32%\build\lib.win32-2.5\ %LIBSBML_ROOT%\src\bindings\python
call "%BASE_DIR%\testPython.bat" %PYTHON_26_32% %SOURCE_32%\build\lib.win32-2.6\ %LIBSBML_ROOT%\src\bindings\python
call "%BASE_DIR%\testPython.bat" %PYTHON_27_32% %SOURCE_32%\build\lib.win32-2.7\ %LIBSBML_ROOT%\src\bindings\python
call "%BASE_DIR%\testPython.bat" %PYTHON_25_64% %SOURCE_32%\build\lib.win-amd64-2.5\ %LIBSBML_ROOT%\src\bindings\python
call "%BASE_DIR%\testPython.bat" %PYTHON_26_64% %SOURCE_32%\build\lib.win-amd64-2.6\ %LIBSBML_ROOT%\src\bindings\python
call "%BASE_DIR%\testPython.bat" %PYTHON_27_64% %SOURCE_32%\build\lib.win-amd64-2.7\ %LIBSBML_ROOT%\src\bindings\python


:ALL_DONE
cd  /d %BASE_DIR%
