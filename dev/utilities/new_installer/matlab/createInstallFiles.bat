@echo off
REM 
REM This file copies the matlab binaries into the ./libsbml-matlab
REM directory. This batch file accepts three arguments: 
REM 
REM createInstallFiles <architecture> <binary dir> -silent
REM 
REM where <architecture> is 32 or 64, <binary dir> a directory that
REM contains the mexw<architecture> files in bindings\matlab. if the 
REM third argument '-silent' is given the batch file will not ask whether
REM to delete an existing libsbml-matlab directory and hide most output.
REM
REM If no output is specified this is the same as invoking the file as: 
REM 
REM createInstallFiles 32 ..\..\..\friday-25\libSBML-4.3.0-win32\libSBML-4.3.0-win32\
REM 

SET MATLAB_INST_DIR=%~dp0
cd /d %MATLAB_INST_DIR%
SET ARCH=%1

if "%ARCH%" == "" SET ARCH=32

REM this is the directory that contains the static matlab bindings
REM in its bindings\matlab directory.
 
SET MATLAB_BIN=%2
if "%MATLAB_BIN%" == "" SET MATLAB_BIN=..\..\..\friday-25\libSBML-4.3.0-win32\libSBML-4.3.0-win32\

echo. 
echo createInstallFiles %ARCH% %MATLAB_BIN%
echo.


if "%3" == "-silent" goto SILENT_DELETE
echo on
rmdir /S libsbml-matlab
goto CREATE

:SILENT_DELETE
@echo off
rd /s /q libsbml-matlab

:CREATE

mkdir libsbml-matlab
cd libsbml-matlab

rem copy files

copy ..\..\..\..\..\AUTHORS.txt AUTHORS.txt
copy ..\..\..\..\..\COPYING.txt COPYING.txt
copy ..\..\..\..\..\FUNDING.txt FUNDING.txt
copy ..\..\..\..\..\VERSION.txt VERSION.txt
copy ..\..\..\..\..\COPYING.html COPYING.html
copy ..\Uninstall_libSBML_matlab.bat Uninstall_libSBML_matlab.bat

rem make new directories

mkdir matlab
mkdir install

rem matlab directory

cd matlab
copy ..\..\..\..\..\..\src\bindings\matlab\TranslateSBML.m TranslateSBML.m
copy ..\..\..\..\..\..\src\bindings\matlab\TranslateSBML.c TranslateSBML.c
copy ..\..\..\..\..\..\src\bindings\matlab\OutputSBML.m OutputSBML.m
copy ..\..\..\..\..\..\src\bindings\matlab\OutputSBML.c OutputSBML.c
copy ..\..\..\..\..\..\src\bindings\matlab\addLevelVersion.m addLevelVersion.m
copy ..\..\..\..\..\..\src\bindings\matlab\applyUserValidation.m applyUserValidation.m
copy ..\..\..\..\..\..\src\bindings\matlab\CheckAndConvert.m  CheckAndConvert.m
copy ..\..\..\..\..\..\src\bindings\matlab\Contents.m Contents.m
copy ..\..\..\..\..\..\src\bindings\matlab\ConvertFormulaToMathML.m  ConvertFormulaToMathML.m
copy ..\..\..\..\..\..\src\bindings\matlab\getDefaultValues.m  getDefaultValues.m
copy ..\..\..\..\..\..\src\bindings\matlab\getStructureFieldnames.m  getStructureFieldnames.m
copy ..\..\..\..\..\..\src\bindings\matlab\getValueType.m  getValueType.m
copy ..\..\..\..\..\..\src\bindings\matlab\installSBML.m  installSBML.m
copy ..\..\..\..\..\..\src\bindings\matlab\isEnabled.m  isEnabled.m
copy ..\..\..\..\..\..\src\bindings\matlab\isFbcEnabled.m  isFbcEnabled.m
copy ..\..\..\..\..\..\src\bindings\matlab\isoctave.m  isoctave.m
copy ..\..\..\..\..\..\src\bindings\matlab\isSBML_Model.m  isSBML_Model.m
copy ..\..\..\..\..\..\src\bindings\matlab\test.xml test.xml

REM these need to find the static matlab builds

copy %MATLAB_BIN%\bindings\matlab\TranslateSBML.mexw%ARCH% TranslateSBML.mexw%ARCH%
copy %MATLAB_BIN%\bindings\matlab\OutputSBML.mexw%ARCH% OutputSBML.mexw%ARCH%

cd ..

cd install
copy ..\..\install\install.bat install.bat
cd ..


cd /d %MATLAB_INST_DIR%

