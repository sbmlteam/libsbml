These are batch files that copy native windows libraries from a build to the 
relevant directories and run the test files.

They are specific for each language binding and for the main windows tests.

Python Bindings
=============

1. Build the python binding.

2. testPython.bat does the following:
   
   i) copies _libsbml.pyd to the python site-packages dir
  ii) copies libsbml.py to the python site-packages dir
 iii) runs the test.py from the bindings/python directory


