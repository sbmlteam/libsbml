These are batch files that copy native windows libraries from a build to the 
relevant directories and run the test files.

They are specific for each language binding and for the main windows tests.

Java Bindings
=============

1. Build the java binding.

2. testJava.bat does the following:
   
   i) copies libsbmlj.lib to the bindings/java dir
  ii) copies libsbmlj.dll to the bindings/java dir
 iii) builds the AutoTestRunner in the bindings/java dir
  iv) runs the AutoTestRunner


