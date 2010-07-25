echo off
set PYTHONPATH=C:\libsbml_trunk\src\bindings\python\test\annotation;C:\libsbml_trunk\src\bindings\python\test\math;C:\libsbml_trunk\src\bindings\python\test\sbml;C:\libsbml_trunk\src\bindings\python\test\xml
cd ..
cd ..
cd ..
cd ..
cd win
cd bin
cd python
cd python25
copy _libsbml.pyd C:\Python\Python25\Lib\site-packages\_libsbml.pyd
copy _libsbml.lib C:\Python\Python25\Lib\site-packages\_libsbml.lib
cd ..
cd ..
cd ..
cd ..
cd src
cd bindings
cd python
copy libsbml.py C:\Python\Python25\Lib\site-packages\libsbml.py
copy setup.py C:\Python\Python25\Lib\site-packages\setup.py
C:\Python\Python25\python.exe test.py
cd ..
cd ..
cd ..
cd dev
cd utilities
cd nativeWinTests
cd python

