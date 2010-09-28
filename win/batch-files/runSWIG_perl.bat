REM run swig for perl
cd ..
cd ..
cd src
cd bindings
cd perl
C:/libsbml_trunk/src/bindings/swig/swigdoc.py perl -I../.. -D../../../docs/src ../swig/libsbml.i LibSBML.pod
C:/swigwin-2.0.0/swig.exe -I../swig  -c++ -DUSE_LAYOUT -perl5 -proxy -I../../../include -o libsbml_perl_wrap.cpp Libsbml.i
