rem
rem  Please change LIBPATH for your environment, 
rem  otherwise put the required DLLs in the current directory.
rem  (LIBPATH should contain paths in which required DLLs (for libsbmlcs.dll) located.)
rem 
rem  LibSBMLCSTestRunner.exe needs to be built and located in bin\Release directory
rem  (or change the path in this file) to run this batch file.
rem 
rem

set LIBPATH=.;..\..\..\win32\bin
set PATH=%LIBPATH%;%PATH%

copy bin\Release\LibSBMLCSTestRunner.exe .
LibSBMLCSTestRunner.exe test ..\..\sbml\test\test-data libsbmlcsP.dll

pause
