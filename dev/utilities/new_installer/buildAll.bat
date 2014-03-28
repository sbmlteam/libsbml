@echo off 

REM This file will build libsbml for windows (32 and 64 bit)
REM
REM 2011/02/26 Frank Bergmann
REM

REM ensure visual studio is in the path
if "%INCLUDE%"=="" call vs10.bat

REM set up directory variables
SET BASE_DIR=%~dp0
SET LIBSBML_ROOT=%BASE_DIR%\..\..\..\

REM if set to a directory, zip files will be produced
REM and copied into it. To disable simply comment out 
REM the next line.
REM SET ZIP_DIR=C:\Users\fbergmann\Documents\My Dropbox\libsbml-dist\cmake-static-release
SET ZIP_DIR=C:\Development\libsbml-dist


cd  /d %BASE_DIR%

REM this version won't build the dependency libraries 
REM (though it could). Instead we just assume the 
REM 32bit (static, static CRT) and 64bit (static, static CRT) 
REM dependencies are present

SET DEPENDENCIES_32_BIT=%LIBSBML_ROOT%\dependencies_32_static
SET DEPENDENCIES_64_BIT=%LIBSBML_ROOT%\dependencies_64_static
SET DEPENDENCIES_CURRENT=%LIBSBML_ROOT%\dependencies

IF EXIST "%DEPENDENCIES_CURRENT%" goto DEPENDENCIES_PRESENT
IF NOT EXIST "%DEPENDENCIES_32_BIT%" goto MISSING_DEPENDENCIES
IF NOT EXIST "%DEPENDENCIES_64_BIT%" goto MISSING_DEPENDENCIES

SET BUILD_DIR_32_BIT=%LIBSBML_ROOT%\build_32
SET BUILD_DIR_64_BIT=%LIBSBML_ROOT%\build_64

REM IF EXIST "%BUILD_DIR_32_BIT%" goto BUILD_DIR_EXISTS
REM IF EXIST "%BUILD_DIR_64_BIT%" goto BUILD_DIR_EXISTS

:BUILD_32
REM BUILD 32bit libsbml
MOVE "%DEPENDENCIES_32_BIT%" "%DEPENDENCIES_CURRENT%"
mkdir "%BUILD_DIR_32_BIT%"
cd /d "%BUILD_DIR_32_BIT%"
copy "%BASE_DIR%\CMakeCache_32.txt" CMakeCache.txt
cmake -G "NMake Makefiles" -DCMAKE_INSTALL_PREFIX=c:/32bit/ -DCMAKE_SIZEOF_VOID_P=4 .. 
cmake -G "NMake Makefiles" -DCMAKE_INSTALL_PREFIX=c:/32bit/ -DCMAKE_SIZEOF_VOID_P=4 .. 
nmake
nmake install

if "%ZIP_DIR%" == "" goto DONE_32
cpack -G ZIP
move /Y *.zip "%ZIP_DIR%"

:DONE_32
MOVE "%DEPENDENCIES_CURRENT%" "%DEPENDENCIES_32_BIT%"

REM REM BUILD 64bit libsbml
MOVE "%DEPENDENCIES_64_BIT%" "%DEPENDENCIES_CURRENT%"
mkdir "%BUILD_DIR_64_BIT%"
cd /d "%BUILD_DIR_64_BIT%"
copy "%BASE_DIR%\CMakeCache_64.txt" CMakeCache.txt
cmake -G "Visual Studio 10 Win64" -DCMAKE_INSTALL_PREFIX=c:/64bit/ -DCMAKE_SIZEOF_VOID_P=8 .. 
cmake -G "Visual Studio 10 Win64" -DCMAKE_INSTALL_PREFIX=c:/64bit/ -DCMAKE_SIZEOF_VOID_P=8 .. 
devenv libsbml.sln /build RelWithDebInfo
devenv libsbml.sln /build RelWithDebInfo /project INSTALL

if "%ZIP_DIR%" == "" goto DONE_64
cpack -G ZIP -C RelWithDebInfo
move /Y *.zip "%ZIP_DIR%"

:DONE_64
MOVE "%DEPENDENCIES_CURRENT%" "%DEPENDENCIES_64_BIT%"

goto ALL_DONE
:BUILD_DIR_EXISTS
echo.
echo The build directory already exists, instead of overwriting
echo it please remove it first. The build directories are: 
echo. 
echo 32bit: %BUILD_DIR_32_BIT%
echo 64bit: %BUILD_DIR_64_BIT%
echo.
echo Please remove them before continuing.
echo.
goto ALL_DONE
:DEPENDENCIES_PRESENT
echo.
echo The dependencies directory already exists. This usually
echo is the case if a previous run was not finished:
echo. 
echo    %DEPENDENCIES_CURRENT%
echo. 
echo Please remove it first. 
echo. 
goto ALL_DONE
:MISSING_DEPENDENCIES
echo.
echo An error occured while trying to build libSBML for windows
echo please ensure that the dependencies are present. They are 
echo assumed to be in:
echo.
echo 32bit: %DEPENDENCIES_32_BIT%
echo 64bit: %DEPENDENCIES_64_BIT%
echo. 
echo Other than that, Visual Studio needs to be installed, and
echo CMake needs to be in the path. 
echo. 
goto ALL_DONE

:ALL_DONE
cd  /d %BASE_DIR%
