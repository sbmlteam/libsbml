REM run swig for python
cd ..
cd ..
cd src
cd bindings
cd swig
swigdoc.py python -I../../../include -D../../../docs/src ../swig/libsbml.i ../python/pydoc.i
cd ..
cd python
C:/swigwin-1.3.39/swig.exe -I../swig -I../../../include -c++ -DUSE_LAYOUT -python -o libsbml_python_wrap.cpp libsbml.i
