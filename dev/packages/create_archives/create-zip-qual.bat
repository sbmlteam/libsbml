@ echo off
REM 
REM This script creates a source archive for the Qual package
REM 

SET THIS_DIR=%~dp0
SET PACKAGE_NAME=qual
set VERSION=libSBML-5.8.0-%PACKAGE_NAME%-src
SET DIST_DIR=%~dp0\%VERSION%
SET PACKAGE_DIR=%~dp0\..\..\..\

SET ZIP=%THIS_DIR%\bin\zip --out "..\%VERSION%.zip" -9 -r 

IF NOT EXIST "%PACKAGE_DIR%" goto NO_DIR

REM IF EXIST "%THIS_DIR%\temp" goto TEMP_CREATED
REM mkdir "%THIS_DIR%\temp"
REM :TEMP_CREATED

IF EXIST "%DIST_DIR%" goto CREATED
mkdir "%DIST_DIR%"
:CREATED

cd /d %DIST_DIR%
mkdir src
mkdir src\sbml
mkdir src\sbml\packages
mkdir src\sbml\packages\%PACKAGE_NAME%
mkdir src\sbml\packages\%PACKAGE_NAME%\common
mkdir src\sbml\packages\%PACKAGE_NAME%\extension
mkdir src\sbml\packages\%PACKAGE_NAME%\extension\test
mkdir src\sbml\packages\%PACKAGE_NAME%\extension\test\test-data
mkdir src\sbml\packages\%PACKAGE_NAME%\sbml
mkdir src\sbml\packages\%PACKAGE_NAME%\sbml\test
mkdir src\sbml\packages\%PACKAGE_NAME%\validator
mkdir src\sbml\packages\%PACKAGE_NAME%\validator\constraints
mkdir src\sbml\packages\%PACKAGE_NAME%\validator\test
mkdir src\sbml\packages\%PACKAGE_NAME%\validator\test\test-data
mkdir src\sbml\packages\%PACKAGE_NAME%\validator\test\test-data\general-constraints
mkdir src\sbml\packages\%PACKAGE_NAME%\validator\test\test-data\identifier-constraints
mkdir src\sbml\packages\%PACKAGE_NAME%\validator\test\test-data\math-constraints

copy /y %PACKAGE_DIR%\%PACKAGE_NAME%-package.cmake .
copy /y %PACKAGE_DIR%\src\%PACKAGE_NAME%-package.cmake src
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%-register*      src\sbml\packages

copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\common\*.h      src\sbml\packages\%PACKAGE_NAME%\common\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\extension\*.h   src\sbml\packages\%PACKAGE_NAME%\extension\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\extension\*.cpp src\sbml\packages\%PACKAGE_NAME%\extension\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\sbml\*.h        src\sbml\packages\%PACKAGE_NAME%\sbml\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\sbml\*.cpp      src\sbml\packages\%PACKAGE_NAME%\sbml\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\validator\*.h        src\sbml\packages\%PACKAGE_NAME%\validator\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\validator\*.cpp      src\sbml\packages\%PACKAGE_NAME%\validator\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\validator\constraints\*.h        src\sbml\packages\%PACKAGE_NAME%\validator\constraints
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\validator\constraints\*.cpp      src\sbml\packages\%PACKAGE_NAME%\validator\constraints

copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\extension\test\*.h             src\sbml\packages\%PACKAGE_NAME%\extension\test\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\extension\test\*.c             src\sbml\packages\%PACKAGE_NAME%\extension\test\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\extension\test\*.cpp           src\sbml\packages\%PACKAGE_NAME%\extension\test\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\extension\test\*.txt           src\sbml\packages\%PACKAGE_NAME%\extension\test\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\extension\test\test-data\*.xml src\sbml\packages\%PACKAGE_NAME%\extension\test\test-data

copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\sbml\test\*.h             src\sbml\packages\%PACKAGE_NAME%\sbml\test\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\sbml\test\*.c             src\sbml\packages\%PACKAGE_NAME%\sbml\test\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\sbml\test\*.cpp           src\sbml\packages\%PACKAGE_NAME%\sbml\test\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\sbml\test\*.txt           src\sbml\packages\%PACKAGE_NAME%\sbml\test\

copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\validator\test\*.h             src\sbml\packages\%PACKAGE_NAME%\validator\test\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\validator\test\*.c             src\sbml\packages\%PACKAGE_NAME%\validator\test\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\validator\test\*.cpp           src\sbml\packages\%PACKAGE_NAME%\validator\test\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\validator\test\*.txt           src\sbml\packages\%PACKAGE_NAME%\validator\test\
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\validator\test\test-data\general-constraints\*.xml src\sbml\packages\%PACKAGE_NAME%\validator\test\test-data\general-constraints
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\validator\test\test-data\identifier-constraints\*.xml src\sbml\packages\%PACKAGE_NAME%\validator\test\test-data\identifier-constraints
copy /y %PACKAGE_DIR%\src\sbml\packages\%PACKAGE_NAME%\validator\test\test-data\math-constraints\*.xml src\sbml\packages\%PACKAGE_NAME%\validator\test\test-data\math-constraints

mkdir src\bindings
mkdir src\bindings\csharp
mkdir src\bindings\java
mkdir src\bindings\perl
mkdir src\bindings\python
mkdir src\bindings\r
mkdir src\bindings\ruby
mkdir src\bindings\swig

copy /y %PACKAGE_DIR%\src\bindings\csharp\*-%PACKAGE_NAME%.i    src\bindings\csharp
copy /y %PACKAGE_DIR%\src\bindings\java\*-%PACKAGE_NAME%.i      src\bindings\java
copy /y %PACKAGE_DIR%\src\bindings\perl\*-%PACKAGE_NAME%.cpp    src\bindings\perl
copy /y %PACKAGE_DIR%\src\bindings\perl\*-%PACKAGE_NAME%.i      src\bindings\perl
copy /y %PACKAGE_DIR%\src\bindings\python\*-%PACKAGE_NAME%.cpp  src\bindings\python
copy /y %PACKAGE_DIR%\src\bindings\python\*-%PACKAGE_NAME%.i    src\bindings\python
copy /y %PACKAGE_DIR%\src\bindings\r\*-%PACKAGE_NAME%.cpp       src\bindings\r
copy /y %PACKAGE_DIR%\src\bindings\r\*-%PACKAGE_NAME%.i         src\bindings\r
copy /y %PACKAGE_DIR%\src\bindings\ruby\*-%PACKAGE_NAME%.cpp    src\bindings\ruby
copy /y %PACKAGE_DIR%\src\bindings\ruby\*-%PACKAGE_NAME%.i      src\bindings\ruby
copy /y %PACKAGE_DIR%\src\bindings\swig\%PACKAGE_NAME%-*.i      src\bindings\swig
copy /y %PACKAGE_DIR%\src\bindings\swig\%PACKAGE_NAME%-*.h      src\bindings\swig

:COPY_COMPLETE

cd /d %DIST_DIR%
echo. 
echo creating archive with: %ZIP% *.cmake src
echo.
%ZIP% *.cmake src

goto DONE

:NO_DIR
echo. 
echo The package directory %PACKAGE_DIR% could not be found. 
echo.

:DONE
cd /d %THIS_DIR%
rd /s /q %DIST_DIR%

REM UNSET VARIABLES
SET THIS_DIR=
SET PACKAGE_NAME=
set VERSION=
SET DIST_DIR=
SET PACKAGE_DIR=
SET ZIP=
