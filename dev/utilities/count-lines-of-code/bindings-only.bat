@ echo off
REM 
REM This script counts lines of code.
REM 

SET THIS_DIR=%~dp0
SET CLOC=%THIS_DIR%\bin\cloc-1.62 --autoconf
SET LIBSBML_DIR=%~dp0\..\..\..

REM additional things to do
REM make .t files counted as Perl (these are Perl test files)
SET FORCED="Perl",t

REM need to set these as appropriate for the count wanted
SET OUTPUT_FILE=%THIS_DIR%\bindings.csv
SET REMOVE_MATCHES="comp|fbc|layout|qual"
SET SWIG_DEF=SWIG_def.txt

REM run the count
%CLOC% %LIBSBML_DIR%\src\bindings --read-lang-def=%SWIG_DEF% --not-match-f=%REMOVE_MATCHES% --report-file=%OUTPUT_FILE% --csv --force-lang=%FORCED%

REM unset variables
SET THIS_DIR=
SET CLOC=
SET LIBSBML_DIR=
SET FORCED=
SET OUTPUT_FILE=
SET REMOVE_MATCHES=
SET SWIG_DEF=