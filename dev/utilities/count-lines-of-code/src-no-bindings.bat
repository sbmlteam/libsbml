@ echo off
REM 
REM This script counts lines of code.
REM 

SET THIS_DIR=%~dp0
SET CLOC=%THIS_DIR%\bin\cloc-1.62 --autoconf
SET LIBSBML_DIR=%~dp0\..\..\..
SET EXCLUDE_DIRS=dev,docs,examples,macosx

REM additional things to do
REM make .t files counted as Perl (these are Perl test files)
SET FORCED="Perl",t

REM need to set these as appropriate for the count wanted
SET ADDITIONAL_EXCLUDES=bindings
SET OUTPUT_FILE=%THIS_DIR%\no-bindings.csv

REM run the count
%CLOC% %LIBSBML_DIR% --exclude-dir=%EXCLUDE_DIRS%,%ADDITIONAL_EXCLUDES% --report-file=%OUTPUT_FILE% --csv --force-lang=%FORCED%

REM unset variables
SET THIS_DIR=
SET CLOC=
SET LIBSBML_DIR=
SET EXCLUDE_DIRS=
SET FORCED=
SET ADDITIONAL_EXCLUDES=
SET OUTPUT_FILE=