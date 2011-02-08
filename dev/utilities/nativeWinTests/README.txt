These are batch files that copy native windows libraries from a build to the 
relevant directories and run the test files.

They are specific for each language binding and for the main windows tests.

libsbml test
============

1. Build libsbml

2. build the testValidateSBML and testEchSBML projects in the win/msvc8/test dir.

3. testLibsbml.bat does the following:

   i) runs echoSBML on all models in the Biomodels database
  ii) runs validateSBML on all models in teh Biomodels database

NOTE: Essentially what it checks is that there are no crashes.



