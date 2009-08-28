cd ..
cd ..
mkdir include
cd include
mkdir sbml
cd sbml
mkdir annotation
mkdir common
mkdir compress
mkdir layout
mkdir math
mkdir units
mkdir util
mkdir validator
mkdir xml
cd ..\..\src\sbml
echo a | xcopy /D *.h ..\..\include\sbml
cd layout
echo a | xcopy /D *.h ..\..\..\include\sbml\layout
cd ..
cd ..\math
echo a | xcopy /D *.h ..\..\include\sbml\math
cd ..\common
echo a | xcopy /D *.h ..\..\include\sbml\common
cd ..\compress
echo a | xcopy /D *.h ..\..\include\sbml\compress
cd ..\units
echo a | xcopy /D *.h ..\..\include\sbml\units
cd ..\util
echo a | xcopy /D *.h ..\..\include\sbml\util
cd ..\validator
echo a | xcopy /D *.h ..\..\include\sbml\validator
cd ..\xml
echo a | xcopy /D *.h ..\..\include\sbml\xml
cd ..\annotation
echo a | xcopy /D *.h ..\..\include\sbml\annotation
cd ..
cd ..
cd win

