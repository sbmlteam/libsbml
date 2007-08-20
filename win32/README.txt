
                            l i b S B M L

                   building from source using MSVC


         For more information about SBML or libSBML, contact:

                            The SBML Team
                         http://www.sbml.org/
                     mailto:sbml-team@caltech.edu


       Please join the libsbml-discuss mailing list by visiting
	    http://www.sbml.org/forums/index.php?t=pre_reg


-------------------
Include directories
-------------------

To build libsbml from source the directory ..\libsbml\include needs to be
to be created and added to the include directories searched by the 
Microsoft Development environment.

To create the directory ..\libsbml\include 
either 

run createInclude.bat from the ..\libsbml\win32 directory

or 

build libSBML from MSVC which includes a prebuild step 
that creates the directory


-----
MSVC7
-----

This directory contains solution files:

  libsbml
  libsbml_expat
  libsbml_xml

which load all the relevant libsbml projects and some example projects.

Project files:

  libsbml        -  which builds libsbml with xerces
  libsbml_expat  -  which builds libsbml with expat
  libsbml_xml    -  which builds libsbml with libxml2

It also includes project files:

  addCVTerms
  addModelHistory
  convertSBML
  echoSBML
  printMath
  printSBML
  printUnits
  readSBML
  translateMath
  validateSBML

which use the code given in the ..\libsbml\examples\c++ directory

-------------------------------------------
File author: S. Keating
Last Modified: $Date$
Last Modified By: $Author$
$Source$
-------------------------------------------

# The following is for [X]Emacs users.  Please leave in place.
# Local Variables:
# fill-column: 70
# End:
