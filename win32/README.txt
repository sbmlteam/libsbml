
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

To build libsbml from source the directory ..\libsbml\src needs to be
added to the include directories searched by the Microsoft Development
environment.

To build any of the example projects the release include directory
structure needs to be created and this directory also added to the
include list.

To create the directory ..\libsbml\include 
either 

run createInclude.bat from the ..\libsbml\win32 directory

or 

build libSBML from MSVC which includes a postbuild step 
that creates the directory

-----
MSVC6
-----

This directory contains workspace files:

  libsbml
  libsbml_expat

which load all the relevant projects.

Project files:

  libsbml        -  which builds libsbml with xerces
  libsbml_expat  -  which libsbml with expat

It also includes project files:

  convertSBML
  printMath
  printSBML
  readSBML
  translateSBML
  validateSBML

which use the code given in the ..\libsbml\examples\c++ directory
and:

  sbmlj

which builds the java binding dll.


NOTE: It has occasionally been reported that these files cannot be
opened due to unix-endings. This appears to be due to the zip/unzip
process and varies depending on the tools used.  If you experience
such problems and have cygwin installed you can use the unix2dos
facility on these files to remedy the problem.  Alternatively email
the SBMLTeam and we can send you the unzipped files.


-----
MSVC7
-----

This directory contains solution files:

  libsbml
  libsbml_expat

which load all the relevant projects.

Project files:

  libsbml        -  which builds libsbml with xerces
  libsbml_expat  -  which libsbml with expat

It also includes project files:

  convertSBML
  printMath
  printSBML
  readSBML
  translateSBML
  validateSBML

which use the code given in the ..\libsbml\examples\c++ directory

  drawMath

which uses code given in the ..\libsbml\example\c directory

and:

  sbmlj

which builds the java binding dll.


-------------------------------------------
File author: S. Keating
Last Modified: $Date$
-------------------------------------------




# The following is for [X]Emacs users.  Please leave in place.
# Local Variables:
# fill-column: 70
# End:
