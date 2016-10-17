@echo off
REM 
REM Python Build script that builds a particular level / version of python bindings
REM
REM Frank T. Bergmann 9/10/2014
REM 
REM 
Setlocal EnableDelayedExpansion
SET MAJOR=%1
SET MINOR=%2
SET CURRENT_DIR=%~dp0
SET SOURCE_32=.
::SET PTYHON_32=c:\Python%MAJOR%%MINOR%_32\python.exe
::SET PTYHON_64=c:\Python%MAJOR%%MINOR%_64\python.exe
SET PTYHON_32=c:\Python32\Python%MAJOR%%MINOR%\python.exe
SET PTYHON_64=c:\Python64\Python%MAJOR%%MINOR%\python.exe
SET BUILD_32=%SOURCE_32%\b32_%MAJOR%%MINOR%
SET BUILD_64=%SOURCE_32%\b64_%MAJOR%%MINOR%

:: supported modes BUILD | INSTALLER | WHEEL | ALL 
:: where: BUILD     - only the 32 bit and 64bit bindings will be built
::        TEST      - only test the binaries (assumes a build was run before)
::        INSTALLER - only the installers will be built (assumes a build was run before)
::        WHEEL     - only the wheels will be created (assumes a build was run before)
::        ALL (anything else) - the files will be built, an installer created and the wheels
SET MODE=%3
if "%MODE%" == "" set MODE=ALL

if "%MODE%" == "INSTALLER" GOTO INSTALLER
if "%MODE%" == "WHEEL" GOTO WHEEL
if "%MODE%" == "TEST" GOTO TEST


:BUILD
:: 32 bit
if exist %BUILD_32% rmdir /q /s %BUILD_32%
call .\buildPython32.bat "%PTYHON_32%" ../install_32  %MAJOR% %MINOR%
if not exist "%SOURCE_32%\build\lib.win32-%MAJOR%.%MINOR%" mkdir "%SOURCE_32%\build\lib.win32-%MAJOR%.%MINOR%"
if not exist "%SOURCE_32%\build\lib.win32-%MAJOR%.%MINOR%\libsbml" mkdir "%SOURCE_32%\build\lib.win32-%MAJOR%.%MINOR%\libsbml"
copy  "%SOURCE_32%\libsbml\__init__.py" "%SOURCE_32%\build\lib.win32-%MAJOR%.%MINOR%\libsbml"
move  "%BUILD_32%\_libsbml.pyd" "%SOURCE_32%\build\lib.win32-%MAJOR%.%MINOR%\libsbml"

:: 64 bit
if exist %BUILD_64% rmdir /q /s %BUILD_64%
call "%SOURCE_32%\buildPython64.bat" "%PTYHON_64%" ../install_64 %MAJOR% %MINOR%
if not exist "%SOURCE_32%\build\lib.win-amd64-%MAJOR%.%MINOR%" mkdir "%SOURCE_32%\build\lib.win-amd64-%MAJOR%.%MINOR%"
if not exist "%SOURCE_32%\build\lib.win-amd64-%MAJOR%.%MINOR%\libsbml" mkdir "%SOURCE_32%\build\lib.win-amd64-%MAJOR%.%MINOR%\libsbml"
copy /y "%SOURCE_32%\libsbml\__init__.py" "%SOURCE_32%\build\lib.win-amd64-%MAJOR%.%MINOR%\libsbml"
move  "%BUILD_64%\_libsbml.pyd" "%SOURCE_32%\build\lib.win-amd64-%MAJOR%.%MINOR%\libsbml"

:TEST
SET OLD_PATH=%PATH%
SET PATH=%PATH%;C:\Python32\Python%MAJOR%%MINOR%\DLLs
call .\testPython.bat "%PTYHON_32%" %CURRENT_DIR%\build\lib.win32-%MAJOR%.%MINOR% ..\..\..\..\src\bindings\python\

SET PATH=%OLD_PATH%;C:\Python64\Python%MAJOR%%MINOR%\DLLs
call .\testPython.bat "%PTYHON_64%" %CURRENT_DIR%\build\lib.win-amd64-%MAJOR%.%MINOR% ..\..\..\..\src\bindings\python\

if "%MODE%" == "BUILD" GOTO DONE
if "%MODE%" == "TEST" GOTO DONE

:INSTALLER
:: 32 bit
"%PTYHON_32%" setup32.py bdist_wininst --skip-build  --target-version="%MAJOR%.%MINOR%" --bitmap libsbml-python-installer-graphic.bmp

:: 64 bit
"%PTYHON_64%" setup64.py bdist_wininst --skip-build  --target-version="%MAJOR%.%MINOR%" --bitmap libsbml-python-installer-graphic.bmp

goto DONE

:WHEEL
"%PTYHON_32%" setup.py bdist_wheel --skip-build
"%PTYHON_64%" setup.py bdist_wheel --skip-build
goto DONE

:DONE

if "%4"=="EXIT_WHEN_DONE" exit
