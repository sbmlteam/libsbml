mkdir libsbml_3_expat
cd libsbml_3_expat

rem copy files

copy ..\..\..\AUTHORS.txt AUTHORS.txt
copy ..\..\..\COPYING.txt COPYING.txt
copy ..\..\..\FUNDING.txt FUNDING.txt
copy ..\..\..\NEWS.txt NEWS.txt
copy ..\..\..\README.txt README.txt
copy ..\..\..\VERSION.txt VERSION.txt
copy ..\..\..\COPYING.html COPYING.html
copy ..\..\..\win32\installer\Uninstall_libSBML.bat Uninstall_libSBML.bat

rem make new directories

mkdir win32
mkdir docs
mkdir bindings
mkdir examples

rem bindings directory

cd bindings

mkdir java
mkdir matlab
mkdir python
mkdir csharp
mkdir octave

rem bindings/java directory

cd java

mkdir classes

cd classes
copy ..\..\..\..\..\MSVC7\bin\java\sbmlj.jar sbmlj.jar

cd ..
copy ..\..\..\..\MSVC7\bin\java\sbmlj.dll sbmlj.dll
copy ..\..\..\..\MSVC7\bin\java\sbmlj.lib sbmlj.lib
cd ..

rem bindings/matlab directory

cd matlab
copy ..\..\..\..\..\src\bindings\matlab\TranslateSBML.mexw32 TranslateSBML.mexw32
copy ..\..\..\..\..\src\bindings\matlab\TranslateSBML.m TranslateSBML.m
copy ..\..\..\..\..\src\bindings\matlab\Contents.m Contents.m
copy ..\..\..\..\..\src\bindings\matlab\CheckAndConvert.m  CheckAndConvert.m
copy ..\..\..\..\..\src\bindings\matlab\install_for_Win32installers.m install_Win32.m
copy ..\..\..\..\..\src\bindings\matlab\make.bat make.bat
copy ..\..\..\..\..\src\bindings\matlab\README.txt README.txt
copy ..\..\..\..\..\src\bindings\matlab\test.xml test.xml
cd ..

rem bindings/octave directory
cd octave
copy ..\..\..\..\..\src\bindings\octave\TranslateSBML.mex TranslateSBML.mex
copy ..\..\..\..\..\src\bindings\matlab\CheckAndConvert.m  CheckAndConvert.m
copy ..\..\..\..\..\src\bindings\matlab\test.xml test.xml
cd ..

rem bindings/csharp directory
cd csharp
copy ..\..\..\..\MSVC7\bin\csharp\libsbmlcs.dll libsbmlcs.dll
copy ..\..\..\..\MSVC7\bin\csharp\libsbmlcs.lib libsbmlcs.lib
copy ..\..\..\..\MSVC7\bin\csharp\libsbmlcsP.dll libsbmlcsP.dll
cd ..


rem bindings/python directory

cd python

mkdir python23
mkdir python24
mkdir python25

cd python23
copy ..\..\..\..\..\MSVC7\bin\python\python23\_libsbml.dll _libsbml.dll
copy ..\..\..\..\..\MSVC7\bin\python\python23\_libsbml.lib _libsbml.lib
copy ..\..\..\..\..\MSVC7\bin\python\python23\libsbml.py libsbml.py
copy ..\..\..\..\..\MSVC7\bin\python\python23\setup.py setup.py
cd ..

cd python24
copy ..\..\..\..\..\MSVC7\bin\python\python24\_libsbml.dll _libsbml.dll
copy ..\..\..\..\..\MSVC7\bin\python\python24\_libsbml.lib _libsbml.lib
copy ..\..\..\..\..\MSVC7\bin\python\python24\libsbml.py libsbml.py
copy ..\..\..\..\..\MSVC7\bin\python\python24\setup.py setup.py
cd ..

cd python25
copy ..\..\..\..\..\MSVC7\bin\python\python25\_libsbml.pyd _libsbml.pyd
copy ..\..\..\..\..\MSVC7\bin\python\python25\_libsbml.lib _libsbml.lib
copy ..\..\..\..\..\MSVC7\bin\python\python25\libsbml.py libsbml.py
copy ..\..\..\..\..\MSVC7\bin\python\python25\setup.py setup.py
cd ..
cd ..
cd ..

rem docs directory

cd docs
copy ..\..\..\..\docs\README-for-Windows.txt README.txt
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


copy ..\..\..\..\examples\README.txt README.txt

rem examples/c directory

cd c
copy ..\..\..\..\..\examples\c\convertSBML.c convertSBML.c
copy ..\..\..\..\..\examples\c\drawMath.c drawMath.c
copy ..\..\..\..\..\examples\c\evaluateMath.c evaluateMath.c
copy ..\..\..\..\..\examples\c\FormulaGraphvizFormatter.h FormulaGraphvizFormatter.h
copy ..\..\..\..\..\examples\c\printMath.c printMath.c
copy ..\..\..\..\..\examples\c\printSBML.c printSBML.c
copy ..\..\..\..\..\examples\c\readSBML.c readSBML.c
copy ..\..\..\..\..\examples\c\translateMath.c translateMath.c
copy ..\..\..\..\..\examples\c\util.c util.c
copy ..\..\..\..\..\examples\c\validateSBML.c validateSBML.c
copy ..\..\..\..\..\examples\c\util.h util.h
copy ..\..\..\..\..\examples\c\echoSBML.c echoSBML.c
cd ..

rem examples/c++ directory

cd c++
copy "..\..\..\..\..\examples\c++\printMath.cpp" printMath.cpp
copy "..\..\..\..\..\examples\c++\printSBML.cpp" printSBML.cpp
copy "..\..\..\..\..\examples\c++\readSBML.cpp" readSBML.cpp
copy "..\..\..\..\..\examples\c++\translateMath.cpp" translateMath.cpp
copy "..\..\..\..\..\examples\c++\util.c" util.c
copy "..\..\..\..\..\examples\c++\validateSBML.cpp" validateSBML.cpp
copy "..\..\..\..\..\examples\c++\util.h" util.h
copy "..\..\..\..\..\examples\c++\addCVTerms.cpp" addCVTerms.cpp
copy "..\..\..\..\..\examples\c++\addModelHistory.cpp" addModelHistory.cpp
copy "..\..\..\..\..\examples\c++\echoSBML.cpp" echoSBML.cpp
copy "..\..\..\..\..\examples\c++\printUnits.cpp" printUnits.cpp
copy "..\..\..\..\..\examples\c++\appendAnnotation.cpp" appendAnnotation.cpp
copy "..\..\..\..\..\examples\c++\convertSBML.cpp" convertSBML.cpp
copy "..\..\..\..\..\examples\c++\printAnnotation.cpp" printAnnotation.cpp
copy "..\..\..\..\..\examples\c++\printNotes.cpp" printNotes.cpp
copy "..\..\..\..\..\examples\c++\unsetAnnotation.cpp" unsetAnnotation.cpp
copy "..\..\..\..\..\examples\c++\unsetNotes.cpp" unsetNotes.cpp
copy "..\..\..\..\..\examples\c++\createExampleSBML.cpp" createExampleSBML.cpp
cd ..

rem examples/java directory

cd java
copy ..\..\..\..\..\examples\java\convertSBML.java convertSBML.java
copy ..\..\..\..\..\examples\java\evaluateMath.java evaluateMath.java
copy ..\..\..\..\..\examples\java\printMath.java printMath.java
copy ..\..\..\..\..\examples\java\printSBML.java printSBML.java
copy ..\..\..\..\..\examples\java\readSBML.java readSBML.java
copy ..\..\..\..\..\examples\java\translateMath.java translateMath.java
copy ..\..\..\..\..\examples\java\validateSBML.java validateSBML.java
copy ..\..\..\..\..\examples\java\README.txt README.txt
cd ..

rem examples/perl directory

cd perl
copy ..\..\..\..\..\examples\perl\convertSBML.pl convertSBML.pl
copy ..\..\..\..\..\examples\perl\evaluateMath.pl evaluateMath.pl
copy ..\..\..\..\..\examples\perl\printMath.pl printMath.pl
copy ..\..\..\..\..\examples\perl\printSBML.pl printSBML.pl
copy ..\..\..\..\..\examples\perl\readSBML.pl readSBML.pl
copy ..\..\..\..\..\examples\perl\translateMath.pl translateMath.pl
copy ..\..\..\..\..\examples\perl\validateSBML.pl validateSBML.pl
cd ..

rem examples/layout directory

cd layout
copy ..\..\..\..\..\examples\layout\example1.py example1.py
copy ..\..\..\..\..\examples\layout\example1.cpp example1.cpp
copy ..\..\..\..\..\examples\layout\example1.java example1.java
copy ..\..\..\..\..\examples\layout\example2.cpp example2.cpp
copy ..\..\..\..\..\examples\layout\example3.cpp example3.cpp
copy ..\..\..\..\..\examples\layout\layout2svg.xsl layout2svg.xsl
cd ..

rem examples/csharp directory

cd csharp
copy ..\..\..\..\..\examples\csharp\echoSBML.cs echoSBML.cs
copy ..\..\..\..\..\examples\csharp\validateSBML.cs validateSBML.cs
cd ..

rem examples/python directory

cd python
copy ..\..\..\..\..\examples\python\echoSBML.py echoSBML.py
copy ..\..\..\..\..\examples\python\validateSBML.py validateSBML.py
cd ..

rem examples/sample-models directory

cd sample-models

mkdir from-spec-level2

cd from-spec-level2
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\algebraicrules.xml algebraicrules.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\assignmentrules.xml assignmentrules.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\boundarycondition.xml boundarycondition.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\delay.xml delay.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\dimerization.xml dimerization.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\enzymekinetics.xml enzymekinetics.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\events.xml events.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\functiondef.xml functiondef.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\multicomp.xml multicomp.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\overdetermined.xml overdetermined.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\twodimensional.xml twodimensional.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\units.xml units.xml
copy ..\..\..\..\..\..\examples\sample-models\from-spec\level-2\README.txt README.txt
cd ..
cd ..
cd ..

rem win32 directory

cd win32
mkdir bin
mkdir lib
mkdir include

rem win32/bin directory

cd bin
copy ..\..\..\..\MSVC7\bin\convertSBML.exe convertSBML.exe
copy ..\..\..\..\MSVC7\bin\echoSBML.exe echoSBML.exe
copy ..\..\..\..\MSVC7\bin\validateSBML.exe validateSBML.exe
copy ..\..\..\..\MSVC7\bin\libsbml.dll libsbml.dll
copy ..\..\..\..\bin\zlib1.dll zlib1.dll
copy ..\..\..\..\bin\libexpat.dll libexpat.dll
copy ..\..\..\..\bin\bzip2.dll bzip2.dll
cd ..

cd lib
copy ..\..\..\..\MSVC7\bin\libsbml.lib libsbml.lib
copy ..\..\..\..\bin\libexpat.lib libexpat.lib
copy ..\..\..\..\bin\zdll.lib zdll.lib
copy ..\..\..\..\bin\bzip2.lib bzip2.lib
cd ..

cd include
mkdir sbml
mkdir expat
mkdir zlib
mkdir bzip2

rem win32/include/sbml directory

cd sbml
mkdir common
mkdir compress
mkdir math
mkdir util
mkdir validator
mkdir xml
mkdir units
mkdir annotation
mkdir layout

cd ..\..\..\..\..\..\src\sbml
echo a | xcopy *.h ..\..\win32\installer\libsbml_3_expat\win32\include\sbml
cd layout
echo a | xcopy *.h ..\..\..\win32\installer\libsbml_3_expat\win32\include\sbml\layout
cd ..
cd ..\math
echo a | xcopy *.h ..\..\win32\installer\libsbml_3_expat\win32\include\sbml\math
cd ..\common
echo a | xcopy *.h ..\..\win32\installer\libsbml_3_expat\win32\include\sbml\common
cd ..\compress
echo a | xcopy *.h ..\..\win32\installer\libsbml_3_expat\win32\include\sbml\compress
cd ..\units
echo a | xcopy *.h ..\..\win32\installer\libsbml_3_expat\win32\include\sbml\units
cd ..\util
echo a | xcopy *.h ..\..\win32\installer\libsbml_3_expat\win32\include\sbml\util
cd ..\validator
echo a | xcopy *.h ..\..\win32\installer\libsbml_3_expat\win32\include\sbml\validator
cd ..\xml
echo a | xcopy *.h ..\..\win32\installer\libsbml_3_expat\win32\include\sbml\xml
cd ..\annotation
echo a | xcopy *.h ..\..\win32\installer\libsbml_3_expat\win32\include\sbml\annotation

rem ****************************
rem The following are hardcoded directory structures


rem win32/include/expat

cd C:\libSBML_Dependencies\Expat-2.0.1\Source\lib
echo a | xcopy *.h C:\libsbml\win32\installer\libsbml_3_expat\win32\include\expat
 
rem win32/include/bzip2

cd C:\libSBML_Dependencies\bzip2\include
echo a | xcopy *.h C:\libsbml\win32\installer\libsbml_3_expat\win32\include\bzip2

rem win32/include/zlib

cd C:\libSBML_Dependencies\zlib1.2.3\include
echo a | xcopy *.h C:\libsbml\win32\installer\libsbml_3_expat\win32\include\zlib

cd C:\libsbml\win32\installer












