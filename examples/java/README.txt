
                            l i b S B M L

                        Java Example Programs

                          Nicolas Rodriguez



-----------
0. Building
-----------

The Makefile in this directory is kept simple for illustrative
purposes.  For this reason, it is not machine generated (Automake).

At the Unix command prompt, type:


  % make


NOTE:

The evaluateMath.java example uses the following functions which were
not introduced until JDK 1.5: Math.cosh(), Math.log10(), Math.sinh(),
and Math.tanh().


----------
1. Running
----------

To run the example programs, you must have libsbmlj.jar in your Java
CLASSPATH and:

     (On Linux)      libsbmlj.so     in your  LD_LIBRARY_PATH
     (On Mac OS X)   libsbml.jnilib  in your  DYLD_LIBRARY_PATH
     (On Windows)    sbmlj.dll       in your  PATH

To run the examples on some SBML files, a number of files are readily
available in ../../src/sbml/test/test-data/.  For example:


  % java printSBML     ../../src/sbml/test/test-data/l1v1-branch.xml
  % java readSBML      ../../src/sbml/test/test-data/l2v1-delay.xml

  % java convertSBML   ../../src/sbml/test/test-data/l1v1-rules.xml
                       l2v1-rules.xml

  % java validateSBML  ../../src/sbml/test/test-data/l2v1-branch.xml
  % java validateSBML  ../../src/sbml/test/test-data/test-data/l1v1-branch.xml
  % java validateSBML  ../../src/sbml/test/test-data/l1v1-branch-schema-error.xml


NOTE:

For validateSBML, the three XML Schema files (sbml-l1v1.xsd,
sbml-l1v2.xsd and sbml-l2v1.xsd) are hard-coded with their basenames
only.  If Schema filenames are not absolute paths (as in this case)
the Xerces Schema validation engine only looks for them in the same
directory as the XML file being read.  This is why validateSBML works
above, but will not work if you try it on other SBML files without the
Schema filename in the same directory.


-------------------------------------------
File author: B. Bornstein
Last Modified: $Date$
-------------------------------------------
