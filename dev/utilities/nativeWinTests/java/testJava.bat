echo off

cd ..
cd ..
cd ..
cd ..
cd win
cd bin
cd java
copy sbmlj.lib C:\libsbml_trunk\src\bindings\java
copy sbmlj.dll C:\libsbml_trunk\src\bindings\java
cd ..
cd ..
cd ..
cd src
cd bindings
cd java



set CLASSPATH=.;C:\libsbml_trunk\src\bindings\java\java-files\sbmlj.jar;C:\libsbml_trunk\win\bin;C:\libsbml_trunk\win\bin\java;C:\libsbml_trunk\src\bindings\java\test;

javac -g AutoTestRunner.java test/org/sbml/libsbml/test/annotation/*.java test/org/sbml/libsbml/test/math/*.java test/org/sbml/libsbml/test/sbml/*.java test/org/sbml/libsbml/test/xml/*.java

java AutoTestRunner

cd ..
cd ..
cd ..
cd dev
cd utilities
cd nativeWinTests
cd java