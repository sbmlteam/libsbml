REM run swig for java
cd ..
cd ..
cd src
cd bindings
cd swig
swigdoc.py java -I../../../include -D../../../docs/src ../swig/libsbml.i ../java/javadoc.i
cd ..
cd java
mkdir java-files
cd java-files
mkdir org
cd org
mkdir sbml
cd sbml
mkdir libsbml
cd ..
cd ..
cd ..
C:/swigwin-1.3.39/swig.exe -I../swig -I../../../include -c++ -DUSE_LAYOUT -java -package org.sbml.libsbml -outdir java-files/org/sbml/libsbml -o libsbml_java_wrap.cpp libsbml.i
cd java-files
C:/Sun/SDK/jdk/bin/javac org/sbml/libsbml/*.java
C:/Sun/SDK/jdk/bin/jar cf sbmlj.jar org/sbml/libsbml/*.class