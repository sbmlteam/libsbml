rem
rem  Please change LIBPATH for your environment, 
rem  otherwise put the required DLLs in the current directory.
rem  (LIBPATH should contain paths in which required DLLs (for libsbmlcs.dll) located.)
rem
rem  TestRW.exe needs to be built and located in the current directory
rem  (or change the path in this file) to run this batch file.
rem

set LIBPATH=.;..\..\..\win32\bin
set PATH=%LIBPATH%;%PATH%;

TestRW.exe ..\..\..\examples\sample-models\from-spec\level-2

pause
