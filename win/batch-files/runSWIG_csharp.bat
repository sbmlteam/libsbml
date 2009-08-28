REM run swig for csharp
cd ..
cd ..
cd src
cd bindings
cd csharp
mkdir csharp-files-win
C:/swigwin-1.3.39/swig.exe -I../swig -I../../../include -c++ -DSWIGWIN -DSWIG_CSHARP_NO_WSTRING_HELPER -DUSE_LAYOUT -csharp -namespace libsbml -dllimport libsbmlcs -outdir csharp-files-win -o libsbml_wrap-win.cpp libsbml.i
