
                            l i b S B M L

                           Example Programs

                            Ben Bornstein

        The Systems Biology Markup Language Development Group
              JST ERATO Kitano Symbiotic Systems Project
               Control and Dynamical Systems, MC 107-81
                  California Institute of Technology
                       Pasadena, CA, 91125, USA

                         http://www.sbml.org/
                    mailto:sysbio-team@caltech.edu


--------------
0. Quick Start
--------------

The Makefile in this directory is kept simple for illustrative
purposes.  For this reason, it is not machine generated (Automake) and
as such you may need to modify the variables LDFLAGS and CPPFLAGS (in
the Makefile) to point to the library and include directories,
respectively, where libsbml was installed.  If libsbml was installed
in /usr/local (the default) you will not need to change anything.

At the Unix command prompt, type:


  % make


Run the examples on some SBML files.  A number of files are readily
available in ../src/test-data.  For example:


  % printSBML     ../src/test-data/l1v1-branch.xml
  % readSBML      ../src/test-data/l2v1-delay.xml
  % convertSBML   ../src/test-data/l1v1-rules l2v1-rules.xml
  % validateSBML  ../src/test-data/l2v1-branch.xml
  % validateSBML  ../src/test-data/l1v1-branch.xml
  % validateSBML  ../src/test-data/l1v1-branch-schema-error.xml


NOTE:

For validateSBML, the three XML Schema files (sbml-l1v1.xsd,
sbml-l1v2.xsd and sbml-l2v1.xsd) are hard-coded with their basenames
only.  If Schema filenames are not absolute paths (as in this case)
the Xerces-C Schema validation engine only looks for them in the same
directory as the XML file being read.  This is why validateSBML works
above, but will not work if you try it on other SBML files without the
Schema filename in the same directory.


-------------------------------------------
File author: B. Bornstein
Last Modified: $Date$
-------------------------------------------
