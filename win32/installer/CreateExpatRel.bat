mkdir libsbml_3_expat
cd libsbml_3_expat

rem copy files

copy C:\libsbml_3\AUTHORS.txt AUTHORS.txt
copy C:\libsbml_3\COPYING.txt COPYING.txt
copy C:\libsbml_3\FUNDING.txt FUNDING.txt
copy C:\libsbml_3\NEWS.txt NEWS.txt
copy C:\libsbml_3\README.txt README.txt
copy C:\libsbml_3\VERSION.txt VERSION.txt
copy C:\libsbml_3\COPYING.html COPYING.html

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

rem bindings/java directory

cd java

mkdir classes

cd classes
copy C:\libsbml_3\src\bindings\java\java-files\sbmlj.jar sbmlj.jar

cd ..
copy C:\libsbml_3\src\bindings\java\README.txt README.txt
copy C:\libsbml_3\src\bindings\java\Test.java Test.java
copy C:\libsbml_3\src\bindings\java\libs\sbmlj.dll sbmlj.dll
copy C:\libsbml_3\src\bindings\java\libs\sbmljD.dll sbmljD.dll
copy C:\libsbml_3\src\bindings\java\libs\sbmlj.lib sbmlj.lib
copy C:\libsbml_3\src\bindings\java\libs\sbmljD.lib sbmljD.lib
cd ..

rem bindings/matlab directory

cd matlab
copy C:\libsbml_3\src\bindings\matlab\TranslateSBML.mexw32 TranslateSBML.mexw32
copy C:\libsbml_3\src\bindings\matlab\CheckAndConvert.m  CheckAndConvert.m
copy C:\libsbml_3\src\bindings\matlab\install_for_Win32installers.m install_Win32.m
copy C:\libsbml_3\src\bindings\matlab\make.bat make.bat
copy C:\libsbml_3\src\bindings\matlab\README.txt README.txt
copy C:\libsbml_3\src\bindings\matlab\test.xml test.xml
cd ..

rem bindings/python directory

cd python

mkdir python23
mkdir python24
mkdir python25

cd python23
copy C:\libsbml_3\src\bindings\python\libs\_libsbml.dll _libsbml.dll
copy C:\libsbml_3\src\bindings\python\libs\_libsbml.lib _libsbml.lib
copy C:\libsbml_3\src\bindings\python\libsbml.py libsbml.py
copy C:\libsbml_3\src\bindings\python\setup.py setup.py
copy C:\libsbml_3\src\bindings\python\accept.py accept.py
cd ..

cd python24
copy C:\libsbml_3\src\bindings\Python24\libs\_libsbml.dll _libsbml.dll
copy C:\libsbml_3\src\bindings\Python24\libs\_libsbml.lib _libsbml.lib
copy C:\libsbml_3\src\bindings\python\libsbml.py libsbml.py
copy C:\libsbml_3\src\bindings\python\setup.py setup.py
copy C:\libsbml_3\src\bindings\python\accept.py accept.py
cd ..

cd python25
copy C:\libsbml_3\src\bindings\Python25\libs\_libsbml.pyd _libsbml.pyd
copy C:\libsbml_3\src\bindings\Python25\libs\_libsbml.lib _libsbml.lib
copy C:\libsbml_3\src\bindings\python\libsbml.py libsbml.py
copy C:\libsbml_3\src\bindings\python\setup.py setup.py
copy C:\libsbml_3\src\bindings\python\accept.py accept.py
cd ..
cd ..
cd ..

rem docs directory

cd docs
copy C:\libsbml_3\docs\README-for-Windows.txt README.txt
cd ..

rem examples directory

cd examples
mkdir c
mkdir c++
mkdir java
mkdir perl
mkdir sample-models
mkdir layout


copy C:\libsbml_3\examples\README.txt README.txt

rem examples/c directory

cd c
copy C:\libsbml_3\examples\c\convertSBML.c convertSBML.c
copy C:\libsbml_3\examples\c\drawMath.c drawMath.c
copy C:\libsbml_3\examples\c\evaluateMath.c evaluateMath.c
copy C:\libsbml_3\examples\c\FormulaGraphvizFormatter.h FormulaGraphvizFormatter.h
copy C:\libsbml_3\examples\c\printMath.c printMath.c
copy C:\libsbml_3\examples\c\printSBML.c printSBML.c
copy C:\libsbml_3\examples\c\readSBML.c readSBML.c
copy C:\libsbml_3\examples\c\translateMath.c translateMath.c
copy C:\libsbml_3\examples\c\util.c util.c
copy C:\libsbml_3\examples\c\validateSBML.c validateSBML.c
copy C:\libsbml_3\examples\c\util.h util.h
copy C:\libsbml_3\examples\c\echoSBML.c echoSBML.c
cd ..

rem examples/c++ directory

cd c++
copy "C:\libsbml_3\examples\c++\printMath.cpp" printMath.cpp
copy "C:\libsbml_3\examples\c++\printSBML.cpp" printSBML.cpp
copy "C:\libsbml_3\examples\c++\readSBML.cpp" readSBML.cpp
copy "C:\libsbml_3\examples\c++\translateMath.cpp" translateMath.cpp
copy "C:\libsbml_3\examples\c++\util.c" util.c
copy "C:\libsbml_3\examples\c++\validateSBML.cpp" validateSBML.cpp
copy "C:\libsbml_3\examples\c++\util.h" util.h
copy "C:\libsbml_3\examples\c++\addCVTerms.cpp" addCVTerms.cpp
copy "C:\libsbml_3\examples\c++\addModelHistory.cpp" addModelHistory.cpp
copy "C:\libsbml_3\examples\c++\echoSBML.cpp" echoSBML.cpp
copy "C:\libsbml_3\examples\c++\printUnits.cpp" printUnits.cpp
copy "C:\libsbml_3\examples\c++\appendAnnotation.cpp" appendAnnotation.cpp
copy "C:\libsbml_3\examples\c++\convertSBML.cpp" convertSBML.cpp
copy "C:\libsbml_3\examples\c++\printAnnotation.cpp" printAnnotation.cpp
copy "C:\libsbml_3\examples\c++\printNotes.cpp" printNotes.cpp
copy "C:\libsbml_3\examples\c++\unsetAnnotation.cpp" unsetAnnotation.cpp
copy "C:\libsbml_3\examples\c++\unsetNotes.cpp" unsetNotes.cpp
cd ..

rem examples/java directory

cd java
copy C:\libsbml_3\examples\java\convertSBML.java convertSBML.java
copy C:\libsbml_3\examples\java\evaluateMath.java evaluateMath.java
copy C:\libsbml_3\examples\java\printMath.java printMath.java
copy C:\libsbml_3\examples\java\printSBML.java printSBML.java
copy C:\libsbml_3\examples\java\readSBML.java readSBML.java
copy C:\libsbml_3\examples\java\translateMath.java translateMath.java
copy C:\libsbml_3\examples\java\validateSBML.java validateSBML.java
copy C:\libsbml_3\examples\java\README.txt README.txt
cd ..

rem examples/perl directory

cd perl
copy C:\libsbml_3\examples\perl\convertSBML.pl convertSBML.pl
copy C:\libsbml_3\examples\perl\evaluateMath.pl evaluateMath.pl
copy C:\libsbml_3\examples\perl\printMath.pl printMath.pl
copy C:\libsbml_3\examples\perl\printSBML.pl printSBML.pl
copy C:\libsbml_3\examples\perl\readSBML.pl readSBML.pl
copy C:\libsbml_3\examples\perl\translateMath.pl translateMath.pl
copy C:\libsbml_3\examples\perl\validateSBML.pl validateSBML.pl
cd ..

rem examples/layout directory

cd layout
copy C:\libsbml_3\examples\layout\example1.py example1.py
copy C:\libsbml_3\examples\layout\example1.cpp example1.cpp
copy C:\libsbml_3\examples\layout\example1.java example1.java
copy C:\libsbml_3\examples\layout\example2.cpp example2.cpp
copy C:\libsbml_3\examples\layout\example3.cpp example3.cpp
copy C:\libsbml_3\examples\layout\layout2svg.xsl layout2svg.xsl
cd ..

rem examples/sample-models directory

cd sample-models

mkdir from-spec-level2

cd from-spec-level2
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\algebraicrules.xml algebraicrules.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\assignmentrules.xml assignmentrules.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\boundarycondition.xml boundarycondition.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\delay.xml delay.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\dimerization.xml dimerization.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\enzymekinetics.xml enzymekinetics.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\events.xml events.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\functiondef.xml functiondef.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\multicomp.xml multicomp.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\overdetermined.xml overdetermined.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\twodimensional.xml twodimensional.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\units.xml units.xml
copy C:\libsbml_3\examples\sample-models\from-spec\level-2\README.txt README.txt
cd ..
cd ..
cd ..

rem win32 directory

cd win32
mkdir bin
mkdir include

rem win32/bin directory

cd bin
copy C:\libsbml_3\win32\bin\convertSBML.exe convertSBML.exe
copy C:\libsbml_3\win32\bin\convertSBMLD.exe convertSBMLD.exe
copy C:\libsbml_3\win32\bin\echoSBML.exe echoSBML.exe
copy C:\libsbml_3\win32\bin\echoSBMLD.exe echoSBMLD.exe
copy C:\libsbml_3\win32\bin\validateSBML.exe validateSBML.exe
copy C:\libsbml_3\win32\bin\validateSBMLD.exe validateSBMLD.exe
copy C:\libsbml_3\src\bindings\java\libs\sbmlj.dll sbmlj.dll
copy C:\libsbml_3\src\bindings\java\libs\sbmljD.dll sbmljD.dll
copy C:\libsbml_3\src\bindings\java\libs\sbmlj.lib sbmlj.lib
copy C:\libsbml_3\src\bindings\java\libs\sbmljD.lib sbmljD.lib
copy C:\libsbml_3\win32\bin\libsbml.dll libsbml.dll
copy C:\libsbml_3\win32\bin\libsbmlD.dll libsbmlD.dll
copy C:\libsbml_3\win32\bin\libsbml.lib libsbml.lib
copy C:\libsbml_3\win32\bin\libsbmlD.lib libsbmlD.lib
copy C:\Expat-2.0.0\Libs\libexpat.lib libexpat.lib
copy C:\Expat-2.0.0\Libs\libexpat.dll libexpat.dll
cd ..


cd include
mkdir sbml
mkdir expat

rem win32/include/sbml directory

cd sbml
mkdir common
mkdir math
mkdir util
mkdir validator
mkdir xml
mkdir units
mkdir annotation
mkdir layout

cd C:\libsbml_3\include\sbml
copy * C:\libsbml_3\win32\installer\libsbml_3_expat\win32\include\sbml
cd common
copy * C:\libsbml_3\win32\installer\libsbml_3_expat\win32\include\sbml\common
cd ..
cd math
copy * C:\libsbml_3\win32\installer\libsbml_3_expat\win32\include\sbml\math
cd ..
cd util
copy * C:\libsbml_3\win32\installer\libsbml_3_expat\win32\include\sbml\util
cd ..
cd validator
copy * C:\libsbml_3\win32\installer\libsbml_3_expat\win32\include\sbml\validator
cd ..
cd xml
copy * C:\libsbml_3\win32\installer\libsbml_3_expat\win32\include\sbml\xml
cd ..
cd units
copy * C:\libsbml_3\win32\installer\libsbml_3_expat\win32\include\sbml\units
cd ..
cd annotation
copy * C:\libsbml_3\win32\installer\libsbml_3_expat\win32\include\sbml\annotation
cd ..
cd layout
copy * C:\libsbml_3\win32\installer\libsbml_3_expat\win32\include\sbml\layout
cd ..
cd C:\libsbml_3\win32\installer\libsbml_3_expat
























