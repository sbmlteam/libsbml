To update the hardcoded SBO tree in libsbml.

1) Download the flat obo file from SBO and name the file 'sbo-obo-flat.txt'.

2) Run the python script sbo2cpp.py.

3) This creates a file 'output.txt'.

4) Replace the body of function SBO::populateSBOTree() 
in the file libsbml/src/sbml/SBO.cpp
with the code from output.txt.


Alternatively this has been integrated into cmake, where you simply execute the update_sbo target for example using gnumake this would be done using: 

make update_sbo

