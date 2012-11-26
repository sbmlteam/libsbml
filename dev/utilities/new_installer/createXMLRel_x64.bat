@echo off

REM this file is based on sarah's file that creates the 
REM directory structure for the inno setup script. However
REM it should only need adjusting of the following constants.

SET INST_BASE_DIR=%~dp0
SET LIBSBML_ROOT=%INST_BASE_DIR%\..\..\..\
SET DEPENDENCIES_32_BIT=%LIBSBML_ROOT%\dependencies_32_static
SET MATLAB_INSTALLER_DIR=%INST_BASE_DIR%\matlab\Output
SET VERSION=5.3.0

REM the installation directory as created by a cmake installation
SET INSTALL_DIR=c:\64bit
REM this is where the python build script copies the python installers
SET INSTALL_DIR_32=c:\32bit

SET DEST_DIR=%INST_BASE_DIR%\libsbml\

REM from here i copy the old libsbml dependecies (lib and dll)
SET OLD_DEPENDENCIES_DIR=C:\Program Files\SBML\libSBML-4.2.0-libxml2-x64\win64

SET LIBXML_INCLUDE_DIR=%OLD_DEPENDENCIES_DIR%\include\libxml2
SET BZIP2_INCLUDE_DIR=%OLD_DEPENDENCIES_DIR%\include\bzip2
SET ZIP_INCLUDE_DIR=%OLD_DEPENDENCIES_DIR%\include\zlib
SET ICONV_INCLUDE_DIR=%OLD_DEPENDENCIES_DIR%\include\iconv


REM if set to a directory, the files will be produced
REM and copied into it. To disable simply comment out 
REM the next line.
REM SET DROP_DIR=C:\Users\fbergmann\Documents\My Dropbox\libsbml-dist
SET DROP_DIR=C:\Development\libsbml-dist



REM the remaining relies only on these variables 

if not exist libsbml goto NO_DELETE
rmdir /S /Q libsbml
:NO_DELETE

mkdir libsbml
cd libsbml

rem copy files

copy %LIBSBML_ROOT%\AUTHORS.txt AUTHORS.txt
copy %LIBSBML_ROOT%\COPYING.txt COPYING.txt
copy %LIBSBML_ROOT%\FUNDING.txt FUNDING.txt
copy %LIBSBML_ROOT%\NEWS.txt NEWS.txt
copy %LIBSBML_ROOT%\OLD_NEWS.txt OLD_NEWS.txt
copy %LIBSBML_ROOT%\README.txt README.txt
copy %LIBSBML_ROOT%\VERSION.txt VERSION.txt
copy %LIBSBML_ROOT%\COPYING.html COPYING.html
copy ..\Uninstall_libSBML.bat Uninstall_libSBML.bat

rem make new directories

mkdir win64
mkdir docs
mkdir bindings
mkdir examples

rem docs directory

cd docs
copy "%LIBSBML_ROOT%\docs\README.txt" README.txt
cd ..

rem examples directory

cd examples
mkdir c
mkdir c++
mkdir java
mkdir csharp
mkdir perl
mkdir sample-models
mkdir layout
mkdir python


copy "%LIBSBML_ROOT%\examples\README.txt" README.txt

rem examples/c directory

cd c
copy %LIBSBML_ROOT%\examples\c\addCVTerms.c addCVTerms.c
copy %LIBSBML_ROOT%\examples\c\addModelHistory.c addModelHistory.c
copy %LIBSBML_ROOT%\examples\c\addingEvidenceCodes_1.c addingEvidenceCodes_1.c
copy %LIBSBML_ROOT%\examples\c\addingEvidenceCodes_2.c addingEvidenceCodes_2.c
copy %LIBSBML_ROOT%\examples\c\appendAnnotation.c appendAnnotation.c
copy %LIBSBML_ROOT%\examples\c\convertSBML.c convertSBML.c
copy %LIBSBML_ROOT%\examples\c\createExampleSBML.c createExampleSBML.c
copy %LIBSBML_ROOT%\examples\c\drawMath.c drawMath.c
copy %LIBSBML_ROOT%\examples\c\echoSBML.c echoSBML.c
copy %LIBSBML_ROOT%\examples\c\evaluateMath.c evaluateMath.c
copy %LIBSBML_ROOT%\examples\c\FormulaGraphvizFormatter.h FormulaGraphvizFormatter.h
copy %LIBSBML_ROOT%\examples\c\printAnnotation.c printAnnotation.c 
copy %LIBSBML_ROOT%\examples\c\printMath.c printMath.c
copy %LIBSBML_ROOT%\examples\c\printNotes.c printNotes.c
copy %LIBSBML_ROOT%\examples\c\printRegisteredPackages.c printRegisteredPackages.c
copy %LIBSBML_ROOT%\examples\c\printSBML.c printSBML.c
copy %LIBSBML_ROOT%\examples\c\printUnits.c printUnits.c
copy %LIBSBML_ROOT%\examples\c\printSupported.c printSupported.c
copy %LIBSBML_ROOT%\examples\c\readSBML.c readSBML.c
copy %LIBSBML_ROOT%\examples\c\translateMath.c translateMath.c
copy %LIBSBML_ROOT%\examples\c\translateL3Math.c translateL3Math.c
copy %LIBSBML_ROOT%\examples\c\unsetAnnotation.c unsetAnnotation.c
copy %LIBSBML_ROOT%\examples\c\unsetNotes.c unsetNotes.c
copy %LIBSBML_ROOT%\examples\c\util.c util.c
copy %LIBSBML_ROOT%\examples\c\util.h util.h
copy %LIBSBML_ROOT%\examples\c\validateSBML.c validateSBML.c
cd ..

rem examples/c++ directory

cd c++
copy "%LIBSBML_ROOT%\examples\c++\addCustomValidator.cpp" addCustomValidator.cpp
copy "%LIBSBML_ROOT%\examples\c++\addCVTerms.cpp" addCVTerms.cpp
copy "%LIBSBML_ROOT%\examples\c++\addingEvidenceCodes_1.cpp" addingEvidenceCodes_1.cpp
copy "%LIBSBML_ROOT%\examples\c++\addingEvidenceCodes_2.cpp" addingEvidenceCodes_2.cpp
copy "%LIBSBML_ROOT%\examples\c++\addModelHistory.cpp" addModelHistory.cpp
copy "%LIBSBML_ROOT%\examples\c++\appendAnnotation.cpp" appendAnnotation.cpp
copy "%LIBSBML_ROOT%\examples\c++\callExternalValidator.cpp" callExternalValidator.cpp
copy "%LIBSBML_ROOT%\examples\c++\convertSBML.cpp" convertSBML.cpp
copy "%LIBSBML_ROOT%\examples\c++\createExampleSBML.cpp" createExampleSBML.cpp
copy "%LIBSBML_ROOT%\examples\c++\echoSBML.cpp" echoSBML.cpp
copy "%LIBSBML_ROOT%\examples\c++\printAnnotation.cpp" printAnnotation.cpp
copy "%LIBSBML_ROOT%\examples\c++\printMath.cpp" printMath.cpp
copy "%LIBSBML_ROOT%\examples\c++\printNotes.cpp" printNotes.cpp
copy "%LIBSBML_ROOT%\examples\c++\printRegisteredPackages.cpp" printRegisteredPackages.cpp
copy "%LIBSBML_ROOT%\examples\c++\printSBML.cpp" printSBML.cpp
copy "%LIBSBML_ROOT%\examples\c++\printSupported.cpp" printSupported.cpp
copy "%LIBSBML_ROOT%\examples\c++\printUnits.cpp" printUnits.cpp
copy "%LIBSBML_ROOT%\examples\c++\readSBML.cpp" readSBML.cpp
copy "%LIBSBML_ROOT%\examples\c++\translateMath.cpp" translateMath.cpp
copy "%LIBSBML_ROOT%\examples\c++\translateL3Math.cpp" translateL3Math.cpp
copy "%LIBSBML_ROOT%\examples\c++\unsetAnnotation.cpp" unsetAnnotation.cpp
copy "%LIBSBML_ROOT%\examples\c++\unsetNotes.cpp" unsetNotes.cpp
copy "%LIBSBML_ROOT%\examples\c++\util.h" util.h
copy "%LIBSBML_ROOT%\examples\c++\util.c" util.c
copy "%LIBSBML_ROOT%\examples\c++\validateSBML.cpp" validateSBML.cpp
cd ..

rem examples/java directory

cd java
copy %LIBSBML_ROOT%\examples\java\addCustomValidator.java addCustomValidator.java
copy %LIBSBML_ROOT%\examples\java\addCVTerms.java addCVTerms.java
copy %LIBSBML_ROOT%\examples\java\addingEvidenceCodes_1.java addingEvidenceCodes_1.java
copy %LIBSBML_ROOT%\examples\java\addingEvidenceCodes_2.java addingEvidenceCodes_2.java
copy %LIBSBML_ROOT%\examples\java\addModelHistory.java addModelHistory.java
copy %LIBSBML_ROOT%\examples\java\appendAnnotation.java appendAnnotation.java
copy %LIBSBML_ROOT%\examples\java\callExternalValidator.java callExternalValidator.java
copy %LIBSBML_ROOT%\examples\java\convertSBML.java convertSBML.java
copy %LIBSBML_ROOT%\examples\java\createExampleSBML.java createExampleSBML.java
copy %LIBSBML_ROOT%\examples\java\echoSBML.java echoSBML.java
copy %LIBSBML_ROOT%\examples\java\evaluateMath.java evaluateMath.java
copy %LIBSBML_ROOT%\examples\java\printAnnotation.java printAnnotation.java
copy %LIBSBML_ROOT%\examples\java\printMath.java printMath.java
copy %LIBSBML_ROOT%\examples\java\printNotes.java printNotes.java
copy %LIBSBML_ROOT%\examples\java\printRegisteredPackages.java printRegisteredPackages.java
copy %LIBSBML_ROOT%\examples\java\printSBML.java printSBML.java
copy %LIBSBML_ROOT%\examples\java\printsupported.java printsupported.java
copy %LIBSBML_ROOT%\examples\java\printUnits.java printUnits.java
copy %LIBSBML_ROOT%\examples\java\readSBML.java readSBML.java
copy %LIBSBML_ROOT%\examples\java\translateMath.java translateMath.java
copy %LIBSBML_ROOT%\examples\java\unsetAnnotation.java unsetAnnotation.java
copy %LIBSBML_ROOT%\examples\java\unsetNotes.java unsetNotes.java
copy %LIBSBML_ROOT%\examples\java\validateSBML.java validateSBML.java
copy %LIBSBML_ROOT%\examples\java\README.txt README.txt
cd ..

rem examples/perl directory

cd perl
copy %LIBSBML_ROOT%\examples\perl\addCustomValidator.pl addCustomValidator.pl
copy %LIBSBML_ROOT%\examples\perl\addCVTerms.pl addCVTerms.pl
copy %LIBSBML_ROOT%\examples\perl\addingEvidenceCodes_1.pl addingEvidenceCodes_1.pl
copy %LIBSBML_ROOT%\examples\perl\addingEvidenceCodes_2.pl addingEvidenceCodes_2.pl
copy %LIBSBML_ROOT%\examples\perl\addModelHistory.pl addModelHistory.pl
copy %LIBSBML_ROOT%\examples\perl\appendAnnotation.pl appendAnnotation.pl
copy %LIBSBML_ROOT%\examples\perl\convertSBML.pl convertSBML.pl
copy %LIBSBML_ROOT%\examples\perl\echoSBML.pl echoSBML.pl
copy %LIBSBML_ROOT%\examples\perl\evaluateMath.pl evaluateMath.pl
copy %LIBSBML_ROOT%\examples\perl\extractReactionInfo.pl extractReactionInfo.pl
copy %LIBSBML_ROOT%\examples\perl\extractReactions.pl extractReactions.pl
copy %LIBSBML_ROOT%\examples\perl\printAnnotation.pl printAnnotation.pl
copy %LIBSBML_ROOT%\examples\perl\printMath.pl printMath.pl
copy %LIBSBML_ROOT%\examples\perl\printNotes.pl printNotes.pl
copy %LIBSBML_ROOT%\examples\perl\printRegisteredPackages.pl printRegisteredPackages.pl
copy %LIBSBML_ROOT%\examples\perl\printSBML.pl printSBML.pl
copy %LIBSBML_ROOT%\examples\perl\printsupported.pl printsupported.pl
copy %LIBSBML_ROOT%\examples\perl\printUnits.pl printUnits.pl
copy %LIBSBML_ROOT%\examples\perl\readSBML.pl readSBML.pl
copy %LIBSBML_ROOT%\examples\perl\translateMath.pl translateMath.pl
copy %LIBSBML_ROOT%\examples\perl\unsetAnnotation.pl unsetAnnotation.pl
copy %LIBSBML_ROOT%\examples\perl\unsetNotes.pl unsetNotes.pl
copy %LIBSBML_ROOT%\examples\perl\validateSBML.pl validateSBML.pl
cd ..

rem examples/layout directory

cd layout
copy %LIBSBML_ROOT%\examples\layout\addLayout.cpp addLayout.cpp
copy %LIBSBML_ROOT%\examples\layout\example1-L3.cpp example1-L3.cpp
copy %LIBSBML_ROOT%\examples\layout\example1-L3.cs example1-L3.cs
copy %LIBSBML_ROOT%\examples\layout\example1-L3.pl example1-L3.pl
copy %LIBSBML_ROOT%\examples\layout\example1-L3.py example1-L3.py
copy %LIBSBML_ROOT%\examples\layout\example1-L3.rb example1-L3.rb
copy %LIBSBML_ROOT%\examples\layout\example1.cpp example1.cpp
copy %LIBSBML_ROOT%\examples\layout\example1.java example1.java
copy %LIBSBML_ROOT%\examples\layout\example1.py example1.py
copy %LIBSBML_ROOT%\examples\layout\example1_L3.java example1_L3.java
copy %LIBSBML_ROOT%\examples\layout\example2-L3.cpp example2-L3.cpp
copy %LIBSBML_ROOT%\examples\layout\example2.cpp example2.cpp
copy %LIBSBML_ROOT%\examples\layout\example3-L3.cpp example3-L3.cpp
copy %LIBSBML_ROOT%\examples\layout\example3.cpp example3.cpp
copy %LIBSBML_ROOT%\examples\layout\layout2svg.xsl layout2svg.xsl
cd ..

rem examples/csharp directory

cd csharp
copy %LIBSBML_ROOT%\examples\csharp\addCustomValidator.cs addCustomValidator.cs
copy %LIBSBML_ROOT%\examples\csharp\addCVTerms.cs addCVTerms.cs
copy %LIBSBML_ROOT%\examples\csharp\addingEvidenceCodes_1.cs addingEvidenceCodes_1.cs
copy %LIBSBML_ROOT%\examples\csharp\addingEvidenceCodes_2.cs addingEvidenceCodes_2.cs
copy %LIBSBML_ROOT%\examples\csharp\addModelHistory.cs addModelHistory.cs
copy %LIBSBML_ROOT%\examples\csharp\appendAnnotation.cs appendAnnotation.cs
copy %LIBSBML_ROOT%\examples\csharp\callExternalValidator.cs callExternalValidator.cs
copy %LIBSBML_ROOT%\examples\csharp\convertSBML.cs convertSBML.cs
copy %LIBSBML_ROOT%\examples\csharp\createExampleSBML.cs createExampleSBML.cs
copy %LIBSBML_ROOT%\examples\csharp\echoSBML.cs echoSBML.cs
copy %LIBSBML_ROOT%\examples\csharp\printAnnotation.cs printAnnotation.cs
copy %LIBSBML_ROOT%\examples\csharp\printMath.cs printMath.cs
copy %LIBSBML_ROOT%\examples\csharp\printNotes.cs printNotes.cs
copy %LIBSBML_ROOT%\examples\csharp\printRegisteredPackages.cs printRegisteredPackages.cs
copy %LIBSBML_ROOT%\examples\csharp\printSBML.cs printSBML.cs
copy %LIBSBML_ROOT%\examples\csharp\printsupported.cs printsupported.cs
copy %LIBSBML_ROOT%\examples\csharp\printUnits.cs printUnits.cs
copy %LIBSBML_ROOT%\examples\csharp\readSBML.cs readSBML.cs
copy %LIBSBML_ROOT%\examples\csharp\translateMath.cs translateMath.cs
copy %LIBSBML_ROOT%\examples\csharp\unsetAnnotation.cs unsetAnnotation.cs
copy %LIBSBML_ROOT%\examples\csharp\unsetNotes.cs unsetNotes.cs
copy %LIBSBML_ROOT%\examples\csharp\validateSBML.cs validateSBML.cs
cd ..

rem examples/python directory

cd python
copy %LIBSBML_ROOT%\examples\python\addCustomValidator.py addCustomValidator.py
copy %LIBSBML_ROOT%\examples\python\addCVTerms.py addCVTerms.py
copy %LIBSBML_ROOT%\examples\python\addingEvidenceCodes_1.py addingEvidenceCodes_1.py
copy %LIBSBML_ROOT%\examples\python\addingEvidenceCodes_2.py addingEvidenceCodes_2.py
copy %LIBSBML_ROOT%\examples\python\addModelHistory.py addModelHistory.py
copy %LIBSBML_ROOT%\examples\python\appendAnnotation.py appendAnnotation.py
copy %LIBSBML_ROOT%\examples\python\callExternalValidator.py callExternalValidator.py
copy %LIBSBML_ROOT%\examples\python\convertSBML.py convertSBML.py
copy %LIBSBML_ROOT%\examples\python\createExampleSBML.py createExampleSBML.py
copy %LIBSBML_ROOT%\examples\python\echoSBML.py echoSBML.py
copy %LIBSBML_ROOT%\examples\python\printAnnotation.py printAnnotation.py
copy %LIBSBML_ROOT%\examples\python\printMath.py printMath.py
copy %LIBSBML_ROOT%\examples\python\printNotes.py printNotes.py
copy %LIBSBML_ROOT%\examples\python\printRegisteredPackages.py printRegisteredPackages.py
copy %LIBSBML_ROOT%\examples\python\printSBML.py printSBML.py
copy %LIBSBML_ROOT%\examples\python\printsupported.py printsupported.py
copy %LIBSBML_ROOT%\examples\python\printUnits.py printUnits.py
copy %LIBSBML_ROOT%\examples\python\translateMath.py translateMath.py
copy %LIBSBML_ROOT%\examples\python\unsetAnnotation.py unsetAnnotation.py
copy %LIBSBML_ROOT%\examples\python\unsetNotes.py unsetNotes.py
copy %LIBSBML_ROOT%\examples\python\validateSBML.py validateSBML.py
cd ..

rem examples/sample-models directory

cd sample-models

mkdir from-spec-level2

cd from-spec-level2
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\algebraicrules.xml algebraicrules.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\assignmentrules.xml assignmentrules.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\boundarycondition.xml boundarycondition.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\delay.xml delay.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\dimerization.xml dimerization.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\enzymekinetics.xml enzymekinetics.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\events.xml events.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\functiondef.xml functiondef.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\multicomp.xml multicomp.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\overdetermined.xml overdetermined.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\twodimensional.xml twodimensional.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\units.xml units.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-2\README.txt README.txt
cd ..


mkdir from-spec-level3

cd from-spec-level3
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\algebraicrules.xml algebraicrules.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\assignmentrules.xml assignmentrules.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\boundarycondition.xml boundarycondition.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\conversionfactor1.xml conversionfactor1.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\conversionfactor2.xml conversionfactor2.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\delay.xml delay.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\dimerization.xml dimerization.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\enzymekinetics.xml enzymekinetics.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\events.xml events.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\fullydeterminedevent.xml fullydeterminedevent.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\functiondef.xml functiondef.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\membrane.xml membrane.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\multicomp.xml multicomp.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\overdetermined.xml overdetermined.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\twodimensional.xml twodimensional.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\twoalgebraicrules.xml twoalgebraicrules.xml
copy %LIBSBML_ROOT%\examples\sample-models\from-spec\level-3\README.txt README.txt
cd ..
cd ..
cd ..

rem bindings directory

cd bindings

mkdir java
mkdir matlab
mkdir python
mkdir csharp
mkdir perl

rem bindings/java directory

cd java
copy %INSTALL_DIR%\bindings\java\libsbmlj.jar libsbmlj.jar
copy %INSTALL_DIR%\bindings\java\sbmlj.dll sbmlj.dll
cd ..

rem bindings/matlab directory

cd matlab
copy "%MATLAB_INSTALLER_DIR%\*-x64.exe" .

REM copy %INSTALL_DIR%\bindings\matlab\TranslateSBML.mexw64 TranslateSBML.mexw64
REM copy %INSTALL_DIR%\bindings\matlab\OutputSBML.mexw64 OutputSBML.mexw64
REM copy %LIBSBML_ROOT%\src\bindings\matlab\TranslateSBML.m TranslateSBML.m
REM copy %LIBSBML_ROOT%\src\bindings\matlab\TranslateSBML.c TranslateSBML.c
REM copy %LIBSBML_ROOT%\src\bindings\matlab\OutputSBML.m OutputSBML.m
REM copy %LIBSBML_ROOT%\src\bindings\matlab\OutputSBML.c OutputSBML.c
REM copy %LIBSBML_ROOT%\src\bindings\matlab\Contents.m Contents.m
REM copy %LIBSBML_ROOT%\src\bindings\matlab\CheckAndConvert.m  CheckAndConvert.m
REM copy %LIBSBML_ROOT%\src\bindings\matlab\isoctave.m  isoctave.m
REM copy %LIBSBML_ROOT%\src\bindings\matlab\ConvertFormulaToMathML.m  ConvertFormulaToMathML.m
REM copy %LIBSBML_ROOT%\src\bindings\matlab\isSBML_Model.m  isSBML_Model.m
REM copy %LIBSBML_ROOT%\src\bindings\matlab\buildSBML.m  buildSBML.m
REM copy %LIBSBML_ROOT%\src\bindings\matlab\installSBML.m  installSBML.m
REM copy %LIBSBML_ROOT%\src\bindings\matlab\test.xml test.xml
cd ..

rem bindings/csharp directory
cd csharp
copy "%INSTALL_DIR%\bindings\csharp\libsbmlcs.dll" libsbmlcs.dll
copy "%INSTALL_DIR%\bindings\csharp\libsbmlcsP.dll" libsbmlcsP.dll
cd ..


rem bindings/python directory

cd python

copy /y "%INSTALL_DIR_32%\bindings\python\src\dist\libsbml-%VERSION%.win-amd64-py2.6.exe" libSBML-%VERSION%-win-py2.6-amd64.exe
copy /y "%INSTALL_DIR_32%\bindings\python\src\dist\libsbml-%VERSION%.win-amd64-py2.7.exe" libSBML-%VERSION%-win-py2.7-amd64.exe


if "%DROP_DIR%" == "" goto DONE_COPYING
copy /Y *.exe "%DROP_DIR%"

:DONE_COPYING

cd ..

rem bindings/perl directory

cd perl
copy "%INSTALL_DIR%\bindings\perl\Libsbml.pm" Libsbml.pm
copy "%INSTALL_DIR%\bindings\perl\Libsbml.pod" Libsbml.pod
copy "%INSTALL_DIR%\bindings\perl\Libsbml.dll" Libsbml.dll
cd ..
cd ..

rem win64 directory

cd win64
mkdir bin
mkdir lib
mkdir include

rem win64/bin directory

cd bin
copy "%INSTALL_DIR%\bin\convertSBML.exe" convertSBML.exe
copy "%INSTALL_DIR%\bin\echoSBML.exe" echoSBML.exe
copy "%INSTALL_DIR%\bin\validateSBML.exe" validateSBML.exe
copy "%INSTALL_DIR%\bin\libsbml.dll" libsbml.dll
copy "%OLD_DEPENDENCIES_DIR%\bin\zlib1.dll" zlib1.dll
copy "%OLD_DEPENDENCIES_DIR%\bin\libbz2.dll" libbz2.dll
copy "%OLD_DEPENDENCIES_DIR%\bin\libiconv.dll" libiconv.dll
copy "%OLD_DEPENDENCIES_DIR%\bin\libxml2.dll" libxml2.dll
cd ..

cd lib
copy "%INSTALL_DIR%\lib\libsbml.lib" libsbml.lib
copy "%INSTALL_DIR%\lib\libsbml-static.lib" libsbml.lib
copy "%OLD_DEPENDENCIES_DIR%\lib\libxml2.lib" libxml2.lib
copy "%OLD_DEPENDENCIES_DIR%\lib\zdll.lib" zdll.lib
copy "%OLD_DEPENDENCIES_DIR%\lib\libbz2.lib" libbz2.lib
copy "%OLD_DEPENDENCIES_DIR%\lib\libiconv.lib" libiconv.lib
cd ..

cd include
mkdir sbml
mkdir libxml2
mkdir zlib
mkdir bzip2
mkdir iconv

rem win64/include/sbml directory
xcopy "%INSTALL_DIR%\include\sbml" .\sbml /S /Y

rem ****************************
rem The following are hardcoded directory structures


rem win32/include/libxml2

cd /d %LIBXML_INCLUDE_DIR%
echo a | xcopy *.h %INST_BASE_DIR%\libsbml\win64\include\libxml2
 
rem win32/include/bzip2

cd /d %BZIP2_INCLUDE_DIR%
echo a | xcopy *.h %INST_BASE_DIR%\libsbml\win64\include\bzip2

rem win32/include/zlib

cd /d %ZIP_INCLUDE_DIR%
echo a | xcopy *.h %INST_BASE_DIR%\libsbml\win64\include\zlib

rem win32/include/iconv
cd /d %ICONV_INCLUDE_DIR%

echo a | xcopy *.h %INST_BASE_DIR%\libsbml\win64\include\iconv

cd /D %INST_BASE_DIR%
