@echo off
SET BASE_DIR=%~dp0
Setlocal EnableDelayedExpansion

:: swig win exectuable
SET SWIG=C:\swigwin-3.0.12\swig.exe

:: specify which VS you want to use
SET VS_VERSION=14

:: set the matching build command for the generator
::SET BUILD_COMMAND=nmake
SET BUILD_COMMAND=ninja

:: set the generator to be used this could be VS, Nmake, Ninja 
::SET GENERATOR="NMake Makefiles"
SET GENERATOR=Ninja -DCMAKE_CXX_COMPILER=cl -DCMAKE_C_COMPILER=cl 


:: libsbml depenendency dir (has to match architecture and vs version)
REM SET DEPENDENCY_DIR_32=\Development\libSBML-dependencies\install_vs%VS_VERSION%_release_x86_static
SET DEPENDENCY_DIR_32=\Development\dependencies\x86\release-static
REM SET DEPENDENCY_DIR_64=\Development\libSBML-dependencies\install_vs%VS_VERSION%_release_x64_static
SET DEPENDENCY_DIR_64=\Development\dependencies\x64\release-static
::SET DEPENDENCY_DIR=\Development\libSBML-dependencies\install_vs12_release_x64

:: be sure to update to the correct python path
::SET PYTHON_32=c:\python27_32\python.exe
::SET PYTHON_64=c:\python27_64\python.exe
SET PYTHON_32=c:\python32\python27\python.exe
SET PYTHON_64=c:\python64\python27\python.exe

:: set up the packages to be used 
SET CMAKE_OPTIONS=%CMAKE_OPTIONS% -DENABLE_COMP=ON -DENABLE_RENDER=ON -DENABLE_FBC=ON -DENABLE_LAYOUT=ON -DENABLE_QUAL=ON -DENABLE_GROUPS=ON -DENABLE_MULTI=ON 
:: outcomment the next line if you want to use the new math layer
SET CMAKE_OPTIONS=%CMAKE_OPTIONS% -DENABLE_L3V2EXTENDEDMATH=ON
REM -DLIBSBML_USE_LEGACY_MATH=ON 


:: shouldn't need to touch those
SET CMAKE_OPTIONS=%CMAKE_OPTIONS% -DWITH_STATIC_RUNTIME=ON
SET CMAKE_OPTIONS=%CMAKE_OPTIONS% -DLIBSBML_SKIP_SHARED_LIBRARY=ON
SET CMAKE_OPTIONS=%CMAKE_OPTIONS% -DSWIG_EXECUTABLE=%SWIG%

SET BUILD_DIR_32=%BASE_DIR%\build_32
SET BUILD_DIR_64=%BASE_DIR%\build_64

SET INSTALL_DIR_32=%BASE_DIR%\install_32
SET INSTALL_DIR_64=%BASE_DIR%\install_64

SET VSVARS="%ProgramFiles(x86)%\Microsoft Visual Studio %VS_VERSION%.0\VC\vcvarsall.bat"

if not exist %BUILD_DIR_32% mkdir %BUILD_DIR_32%

pushd %BUILD_DIR_32%
SET ARCH=x86
call %VSVARS%  %ARCH%
cmake -G %GENERATOR% %CMAKE_OPTIONS% -DLIBSBML_DEPENDENCY_DIR=%DEPENDENCY_DIR_32% -DWITH_PYTHON=ON -DPYTHON_EXECUTABLE=%PYTHON_32% -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=%INSTALL_DIR_32% ../../../../
%BUILD_COMMAND%
%BUILD_COMMAND% install
popd


if not exist %BUILD_DIR_64% mkdir %BUILD_DIR_64%
pushd %BUILD_DIR_64%
SET ARCH=amd64
call %VSVARS%  %ARCH%
cmake -G %GENERATOR% %CMAKE_OPTIONS% -DLIBSBML_DEPENDENCY_DIR=%DEPENDENCY_DIR_64% -DWITH_PYTHON=ON -DPYTHON_EXECUTABLE=%PYTHON_64% -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=%INSTALL_DIR_64% ../../../../
%BUILD_COMMAND%
%BUILD_COMMAND% install
popd
