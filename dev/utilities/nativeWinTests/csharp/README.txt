These are batch files that copy native windows libraries from a build to the 
relevant directories and run the test files.

They are specific for each language binding and for the main windows tests.

CSharp Bindings
=============

1. Build the csharp binding.

2. Build the testCheck and textReadWrite projects in the win/msvc8/test dir.

3. testCSharp.bat does the following:
   
   i) copies libsbmlcs.dll to the win/bin dir
  ii) copies libsbmlcsP.dll to the win/bin dir
 iii) copies libsbmlcs.lib to the win/bin dir
  
NOTE: The win/bin directory contains all dlls and lib files needed.

  iV) runs the tests from the csharp/test directory
   v) runs the read/write test on files in examples\sample-models\from-spec\level-2
  vi) runs the read/write test on files in examples\sample-models\from-spec\level-3


