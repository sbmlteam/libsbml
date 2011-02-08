
                            l i b S B M L

                   building from source using MSVC


         For more information about SBML or libSBML, contact:

                            The SBML Team
                         http://www.sbml.org/
                     mailto:libsbml-team@caltech.edu


       Please join the libsbml-development mailing list by visiting
	    http://www.sbml.org/forums/index.php?t=pre_reg


-------------------
Include directories
-------------------

To build libsbml from source the directory ..\libsbml\include needs to be
to be created and added to the include directories searched by the 
Microsoft Development environment.

To create the directory ..\libsbml\include 
either 

run createInclude.bat from the ..\libsbml\win\batch-files directory

or 

build libSBML from MSVC which includes a prebuild step 
that creates the directory


-------------
win directory
-------------

The directory contains the following subdirectories:

1) batch-files

which contains batch files for performing pre and
post build events from within msvc

NOTE: These include locations of third-party software
such as SWIG that will be specific to the computer. 

2) msvc7

3) msvc8

4) msvc9

which each contain project files for the specific version of
the Microsoft Visual Studio.

--------------------
Each msvcX directory
--------------------

These directories each contain further sub-directories:

1) common

which contains project files that are not parser specific

2) expat

which contains project files for libsbml and the csharp,
java and python bindings specific to the expat parser.

3) libxml2

which contains project files for libsbml and the csharp,
java and python bindings specific to the libxml2 parser.

4) xerces

which contains project files for libsbml and the csharp,
java and python bindings specific to the xerces parser.


Project files in each parser specific directory:

  libsbml         -  which builds libsbml
  libsbml_csharp  -  which builds libsbmlcs for csharp
  libsbml_java    -  which builds sbmlj for java
  libsbml_python  -  which builds _libsbml for python

The common\examples directory includes project files:

  addCVTerms
  addModelHistory
  convertSBML
  createExampleSBML
  drawMath
  echoSBML
  evidenceCode1
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
$HeadURL$
-------------------------------------------

# The following is for [X]Emacs users.  Please leave in place.
# Local Variables:
# fill-column: 70
# End:
