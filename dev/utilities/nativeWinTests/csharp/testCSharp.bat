echo off
cd ..
cd ..
cd ..
cd ..
cd win
cd bin
cd csharp
copy libsbmlcs.dll ..\libsbmlcs.dll
copy libsbmlcsP.dll ..\libsbmlcsP.dll
copy libsbmlcs.lib ..\libsbmlcs.lib
cd ..
cd ..
cd ..
cd src
cd bindings
cd csharp
..\..\..\win\bin\LibSBMLCSTestRunner.exe test ..\..\sbml\test\test-data ..\..\..\win\bin\libsbmlcsP.dll
..\..\..\win\bin\TestRW.exe ..\..\..\examples\sample-models\from-spec\level-2
..\..\..\win\bin\TestRW.exe ..\..\..\examples\sample-models\from-spec\level-3
cd ..
cd ..
cd ..
cd dev
cd utilities
cd nativeWinTests
cd csharp