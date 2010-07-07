cd ..
cd ..
cd bin
cd csharp
copy libsbmlcs.dll ..\libsbmlcs.dll
copy libsbmlcsP.dll ..\libsbmlcsP.dll
copy libsbmlcs.lib ..\libsbmlcs.lib
cd ..
TestRW.exe ..\..\examples\sample-models\from-spec\level-2
TestRW.exe ..\..\examples\sample-models\from-spec\level-3
cd ..
cd msvc8
cd test
