cd ..
cd ..
cd bin
cd csharp
copy libsbmlcs.dll ..\libsbmlcs.dll
copy libsbmlcsP.dll ..\libsbmlcsP.dll
copy libsbmlcs.lib ..\libsbmlcs.lib
cd ..
LibSBMLCSTestRunner.exe ..\..\src\bindings\csharp\test ..\..\src\sbml\test\test-data libsbmlcsP.dll
cd ..
cd msvc8
cd test
