@ echo off
REM 
REM This script counts lines of code.
REM 

SET THIS_DIR=%~dp0
SET CLOC=%THIS_DIR%\bin\cloc-1.62 --autoconf
SET LIBSBML_DIR=%~dp0\..\..\..

REM include a definition for SWIG interface files
SET SWIG_DEF=SWIG_def.txt

REM exclude python\doc-converter directory as it has .i files that are not SWIG
SET EXCLUDE_DIRS=doc-converter

REM exclude relevant files
SET EXCLUDE_EXT=xsl,xsd,html,css

REM record files with no extension as Bourne Shell
SET FORCE_NO="Bourne Shell"

REM additional things to do
REM want to group particular things to be counted together
REM note have to use the cloc group and will rename later
SET FORCE_BASH="Bourne Shell",bash
SET FORCE_BAT="Bourne Shell",bat

REM do not count configure
SET REMOVE_MATCHES=configure$

REM for some reason it is not picking up the R examples
REM but does if you tell it they have lowercase r (they dont but it works)
SET FORCE_R="R",r

REM output file from CLOC
SET OUTPUT_FILE=%THIS_DIR%\results_cloc.xml


REM run the count
%CLOC% %LIBSBML_DIR%  --exclude-dir=%EXCLUDE_DIRS% --exclude-ext=%EXCLUDE_EXT% --not-match-f=%REMOVE_MATCHES% --read-lang-def=%SWIG_DEF% --force-lang=%FORCE_BASH% --force-lang=%FORCE_BAT% --force-lang=%FORCE_R% --lang-no-ext=%FORCE_NO% --report-file=%OUTPUT_FILE% --xml

REM unset variables
SET THIS_DIR=
SET CLOC=
SET LIBSBML_DIR=
SET EXCLUDE_DIRS=
SET EXCLUDE_EXT=
SET FORCE_BASH=
SET FORCE_BAT=
SET FORCE_NO=
SET FORCE_AC=
SET FORCE_M=
SET FORCE_CSS=
SET FORCE_R=
SET OUTPUT_FILE=
SET SWIG_DEF=
SET REMOVE_MATCHES=