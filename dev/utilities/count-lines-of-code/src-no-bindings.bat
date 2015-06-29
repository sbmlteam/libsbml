@ echo off
REM 
REM This script counts lines of code.
REM 

SET THIS_DIR=%~dp0
SET CLOC=%THIS_DIR%\bin\cloc-1.62 --autoconf
SET LIBSBML_DIR=%~dp0\..\..\..
SET EXCLUDE_DIRS=packages

REM additional things to do
REM make .t files counted as Perl (these are Perl test files)
SET FORCED="Perl",t

REM need to set these as appropriate for the count wanted
SET OUTPUT_FILE=%THIS_DIR%\src-no-bindings-no-packages.csv
SET INCLUDE_FILE=%THIS_DIR%\included.txt

REM run the count
%CLOC% %LIBSBML_DIR%  --list-file=%INCLUDE_FILE%  --exclude-dir=%EXCLUDE_DIRS% --report-file=%OUTPUT_FILE% --csv --force-lang=%FORCED%

REM unset variables
SET THIS_DIR=
SET CLOC=
SET LIBSBML_DIR=
SET EXCLUDE_DIRS=
SET FORCED=
SET OUTPUT_FILE=
SET INCLUDE_FILE=