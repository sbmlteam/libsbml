These are batch files that copy native windows libraries from a build to the 
relevant directories and run the test files.

They are specific for each language binding and for the main windows tests.

Perl Bindings
=============

1. Build the perl binding.

2. testPerl.bat does the following:
   
   i) copies LibSBML.pm to the Perl\lib dir
  ii) copies Libsbml.dll to the Perl\lib\auto\libsbml dir
 iii) runs the tests in the perl/t directory
  
NOTE: This does not do a summary in the way make check would; 
it is therefore best to pipe the output to a text file and check
for fails.

Reported failures indicate that the tests did not build or run correctly. 
