@echo off 

REM This file will build the python source dist and the 
REM python installers for (32 and 64 bit)
REM
REM 2011/02/26 Frank Bergmann
REM

REM ensure visual studio is in the path
if "%INCLUDE%"=="" call vs10.bat

REM set up directory variables
SET WIN_INST_DIR=%~dp0
SET LIBSBML_ROOT=%WIN_INST_DIR%\..\..\..\

REM this assumes that libsbml was previously build
SET PYTHON_25_32=C:\Python25\python.exe
SET PYTHON_25_64=C:\Python25_64\python.exe
SET PYTHON_26_32=C:\Python26_32\python.exe
SET PYTHON_26_64=C:\Python26\python.exe
SET PYTHON_27_32=C:\Python27\python.exe
SET PYTHON_27_64=C:\Python27_64\python.exe


cd  /d %WIN_INST_DIR%

SET BUILD_DIR_32_BIT=%LIBSBML_ROOT%\build_32

SET INSTALL_DIR_32=c:\32bit
SET INSTALL_DIR_64=c:\64bit
set SOURCE_32=%INSTALL_DIR_32%\bindings\python\src


IF NOT EXIST "%BUILD_DIR_32_BIT%" goto MISSING_DEPENDENCIES
IF NOT EXIST "%INSTALL_DIR_32%" goto MISSING_DEPENDENCIES
IF NOT EXIST "%INSTALL_DIR_64%" goto MISSING_DEPENDENCIES

REM Allow to bypass things
if "%1" == "--skip-copy" goto COMPILE_BINDINGS
if "%1" == "--test-only" goto DONE_BUILDING

REM prepare install dir 

mkdir "%INSTALL_DIR_32%\win32"
mkdir "%INSTALL_DIR_32%\win32\bin"
mkdir "%INSTALL_DIR_32%\win32\include"
mkdir "%INSTALL_DIR_32%\win32\lib"

xcopy /E /Y "%INSTALL_DIR_32%\bin" "%INSTALL_DIR_32%\win32\bin"
xcopy /E /Y "%INSTALL_DIR_32%\include" "%INSTALL_DIR_32%\win32\include"
xcopy /E /Y "%INSTALL_DIR_32%\lib" "%INSTALL_DIR_32%\win32\lib"

mkdir "%INSTALL_DIR_32%\win64"
mkdir "%INSTALL_DIR_32%\win64\bin"
mkdir "%INSTALL_DIR_32%\win64\include"
mkdir "%INSTALL_DIR_32%\win64\lib"

xcopy /E /Y "%INSTALL_DIR_64%\bin" "%INSTALL_DIR_32%\win64\bin"
xcopy /E /Y "%INSTALL_DIR_64%\include" "%INSTALL_DIR_32%\win64\include"
xcopy /E /Y "%INSTALL_DIR_64%\lib" "%INSTALL_DIR_32%\win64\lib"


REM create source dir
:CREATE_SOURCE_32
mkdir "%SOURCE_32%"
mkdir "%SOURCE_32%\..\swig"
mkdir "%SOURCE_32%\libsbml"
cd /d "%SOURCE_32%"
copy /Y "%WIN_INST_DIR%\graphics\libsbml-python-installer-graphic.bmp" %SOURCE_32%\

copy /Y "%SOURCE_32%\..\libsbml.pth" %SOURCE_32%\
copy /Y "%LIBSBML_ROOT%\src\bindings\swig\*.h" "%SOURCE_32%\..\swig"
copy /Y "%LIBSBML_ROOT%\src\bindings\swig\libsbml.h" %SOURCE_32%\
copy /Y "%LIBSBML_ROOT%\src\bindings\swig\ListWrapper.h" %SOURCE_32%\
copy /Y "%LIBSBML_ROOT%\src\bindings\swig\OStream.h" %SOURCE_32%\
copy /Y "%LIBSBML_ROOT%\src\bindings\swig\OStream.cpp" %SOURCE_32%\

copy /Y "%LIBSBML_ROOT%\src\bindings\python\*.cpp" %SOURCE_32%\
copy /Y "%BUILD_DIR_32_BIT%\src\bindings\python\libsbml_wrap.*" %SOURCE_32%\
copy /Y "%BUILD_DIR_32_BIT%\src\bindings\python\libsbml.py" libsbml\__init__.py

copy /Y "%WIN_INST_DIR%\python\CMakeLists.txt" %SOURCE_32%

copy /Y "%WIN_INST_DIR%\vs10.bat" %SOURCE_32%
copy /Y "%WIN_INST_DIR%\buildPython32.bat" %SOURCE_32%
copy /Y "%WIN_INST_DIR%\buildPython64.bat" %SOURCE_32%

copy /Y "%WIN_INST_DIR%\python\setup32.py" %SOURCE_32%
copy /Y "%WIN_INST_DIR%\python\setup64.py" %SOURCE_32%


:COMPILE_BINDINGS

REM remove old results
rd /s /q "%SOURCE_32%\build"
rd /s /q "%SOURCE_32%\dist"
mkdir "%SOURCE_32%\build"
mkdir "%SOURCE_32%\dist"

REM build python 2.5
rd /s /q "%SOURCE_32%\b32
call  "%SOURCE_32%\buildPython32.bat" "%PYTHON_25_32%"
mkdir "%SOURCE_32%\build\lib.win32-2.5"
mkdir "%SOURCE_32%\build\lib.win32-2.5\libsbml"
copy  "%SOURCE_32%\libsbml\__init__.py" "%SOURCE_32%\build\lib.win32-2.5\libsbml"
move  "%SOURCE_32%\b32\_libsbml.pyd" "%SOURCE_32%\build\lib.win32-2.5\libsbml"

rd /s /q "%SOURCE_32%\b64
call  "%SOURCE_32%\buildPython64.bat" "%PYTHON_25_64%"
mkdir "%SOURCE_32%\build\lib.win-amd64-2.5"
mkdir "%SOURCE_32%\build\lib.win-amd64-2.5\libsbml"
copy "%SOURCE_32%\libsbml\__init__.py" "%SOURCE_32%\build\lib.win-amd64-2.5\libsbml"
move  "%SOURCE_32%\b64\RelWithDebInfo\_libsbml.pyd" "%SOURCE_32%\build\lib.win-amd64-2.5\libsbml"


REM build python 2.6
rd /s /q "%SOURCE_32%\b32
call  "%SOURCE_32%\buildPython32.bat" "%PYTHON_26_32%"
mkdir "%SOURCE_32%\build\lib.win32-2.6"
mkdir "%SOURCE_32%\build\lib.win32-2.6\libsbml"
copy  "%SOURCE_32%\libsbml\__init__.py" "%SOURCE_32%\build\lib.win32-2.6\libsbml"
move  "%SOURCE_32%\b32\_libsbml.pyd" "%SOURCE_32%\build\lib.win32-2.6\libsbml"

rd /s /q "%SOURCE_32%\b64
call  "%SOURCE_32%\buildPython64.bat" "%PYTHON_26_64%"
mkdir "%SOURCE_32%\build\lib.win-amd64-2.6"
mkdir "%SOURCE_32%\build\lib.win-amd64-2.6\libsbml"
copy "%SOURCE_32%\libsbml\__init__.py" "%SOURCE_32%\build\lib.win-amd64-2.6\libsbml"
move  "%SOURCE_32%\b64\RelWithDebInfo\_libsbml.pyd" "%SOURCE_32%\build\lib.win-amd64-2.6\libsbml"

REM build python 2.7
rd /s /q "%SOURCE_32%\b32
call "%SOURCE_32%\buildPython32.bat" "%PYTHON_27_32%"
mkdir "%SOURCE_32%\build\lib.win32-2.7"
mkdir "%SOURCE_32%\build\lib.win32-2.7\libsbml"
copy  "%SOURCE_32%\libsbml\__init__.py" "%SOURCE_32%\build\lib.win32-2.7\libsbml"
move  "%SOURCE_32%\b32\_libsbml.pyd" "%SOURCE_32%\build\lib.win32-2.7\libsbml"


rd /s /q "%SOURCE_32%\b64
call "%SOURCE_32%\buildPython64.bat" "%PYTHON_27_64%"
mkdir "%SOURCE_32%\build\lib.win-amd64-2.7"
mkdir "%SOURCE_32%\build\lib.win-amd64-2.7\libsbml"
copy "%SOURCE_32%\libsbml\__init__.py" "%SOURCE_32%\build\lib.win-amd64-2.7\libsbml"
move  "%SOURCE_32%\b64\RelWithDebInfo\_libsbml.pyd" "%SOURCE_32%\build\lib.win-amd64-2.7\libsbml"


"%PYTHON_25_32%" setup32.py bdist_wininst --skip-build  --target-version="2.5" --bitmap libsbml-python-installer-graphic.bmp
"%PYTHON_26_32%" setup32.py bdist_wininst --skip-build  --target-version="2.6" --bitmap libsbml-python-installer-graphic.bmp
"%PYTHON_27_32%" setup32.py bdist_wininst --skip-build  --target-version="2.7" --bitmap libsbml-python-installer-graphic.bmp

REM dist utils fails to create the python 25 64bit installer
REM "%PYTHON_25_64%" setup64.py bdist_wininst --skip-build --target-version="2.5"
"%PYTHON_26_64%" setup64.py bdist_wininst --skip-build --target-version="2.6" --bitmap libsbml-python-installer-graphic.bmp
"%PYTHON_27_64%" setup64.py bdist_wininst --skip-build --target-version="2.7" --bitmap libsbml-python-installer-graphic.bmp

REM here we can test the new bindindgs
cd  /d %WIN_INST_DIR%
call "%WIN_INST_DIR%\testPython.bat" %PYTHON_25_32% %SOURCE_32%\build\lib.win32-2.5\ %LIBSBML_ROOT%\src\bindings\python
call "%WIN_INST_DIR%\testPython.bat" %PYTHON_26_32% %SOURCE_32%\build\lib.win32-2.6\ %LIBSBML_ROOT%\src\bindings\python
call "%WIN_INST_DIR%\testPython.bat" %PYTHON_27_32% %SOURCE_32%\build\lib.win32-2.7\ %LIBSBML_ROOT%\src\bindings\python
call "%WIN_INST_DIR%\testPython.bat" %PYTHON_25_64% %SOURCE_32%\build\lib.win-amd64-2.5\ %LIBSBML_ROOT%\src\bindings\python
call "%WIN_INST_DIR%\testPython.bat" %PYTHON_26_64% %SOURCE_32%\build\lib.win-amd64-2.6\ %LIBSBML_ROOT%\src\bindings\python
call "%WIN_INST_DIR%\testPython.bat" %PYTHON_27_64% %SOURCE_32%\build\lib.win-amd64-2.7\ %LIBSBML_ROOT%\src\bindings\python


goto ALL_DONE

:MISSING_DEPENDENCIES
echo.
echo Please ensure that libSBML has been build prior to running
echo this script.  It is assumed that the following conditions are
echo met: 
echo   build dir exists: %BUILD_DIR_32_BIT%
echo   32bit install dir exists: %INSTALL_DIR_32%
echo   64bit install dir exists: %INSTALL_DIR_64%
echo. 
echo Please run the buildAll.bat file again. 
echo. 
goto ALL_DONE

:ALL_DONE
cd  /d %WIN_INST_DIR%
