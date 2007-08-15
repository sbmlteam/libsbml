cd..
cd..
cd ..
mkdir include
cd include
mkdir sbml
cd sbml
mkdir annotation
mkdir common
mkdir layout
mkdir math
mkdir units
mkdir util
mkdir validator
mkdir xml
cd ..\..\src\sbml
echo a | xcopy *.h ..\..\include\sbml
cd layout
echo a | xcopy *.h ..\..\..\include\sbml\layout
cd ..
cd ..\math
echo a | xcopy *.h ..\..\include\sbml\math
cd ..\common
echo a | xcopy *.h ..\..\include\sbml\common
cd ..\units
echo a | xcopy *.h ..\..\include\sbml\units
cd ..\util
echo a | xcopy *.h ..\..\include\sbml\util
cd ..\validator
echo a | xcopy *.h ..\..\include\sbml\validator
cd ..\xml
echo a | xcopy *.h ..\..\include\sbml\xml
cd ..\annotation
echo a | xcopy *.h ..\..\include\sbml\annotation
cd ..
cd ..
cd win32

