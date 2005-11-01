cd ..
cd ..
mkdir include
cd include
mkdir sbml
cd sbml
mkdir common
mkdir math
mkdir util
mkdir validator
mkdir xml
cd ..\..\src\sbml
echo a | xcopy *.h ..\..\include\sbml
cd ..\math
echo a | xcopy *.h ..\..\include\sbml\math
cd ..\common
echo a | xcopy *.h ..\..\include\sbml\common
cd ..\util
echo a | xcopy *.h ..\..\include\sbml\util
cd ..\validator
echo a | xcopy *.h ..\..\include\sbml\validator
cd ..\xml
echo a | xcopy *.h ..\..\include\sbml\xml
cd ..
cd ..
cd win32

