
                            l i b S B M L

                        Java Example Programs

		  Nicolas Rodriguez, Ben Bornstein,
		     Michael Hucka, Akiya Jouraku



-----------
0. Building
-----------

A file called "Makefile" should have been generated automatically when
you ran the 'configure' program for libSBML.  If it is not present,
you must re-run 'configure' and give it the --with-java flag on the
command line.  Please see the file ../../README.txt or the libSBML
manual for more information.

With a Makefile in the directory, you can compile the Java examples by
executing the following command in a Unix command shell:

  % make


NOTE:

The evaluateMath.java example uses the following functions which were
not introduced until JDK 1.5: Math.cosh(), Math.log10(), Math.sinh(),
and Math.tanh().  You will therefore need at least version 1.5 of Java
to compile that example.


----------
1. Running
----------

To run the example programs, you must have libsbmlj.jar in your Java
CLASSPATH *and* you must also have set up your object library search
path to find the libSBML libray file.  The exact method to use depends
on the particular operating system you are running:

     (On Linux)      libsbmlj.so     in your  LD_LIBRARY_PATH
     (On Mac OS X)   libsbml.jnilib  in your  DYLD_LIBRARY_PATH
     (On Windows)    sbmlj.dll       in your  PATH

To run the examples on some SBML files, a number of files are readily
available in ../sample-models/.  For example:

  % java printSBML     ../sample-models/from-spec/level-2/enzymekinetics.xml
  % java readSBML      ../sample-models/from-spec/level-2/units.xml



-------------------------------------------
File authors: B. Bornstein, Mike Hucka
Last Modified: $Date$
Last Modified By: $Author$
$Source$
-------------------------------------------

# The following is for [X]Emacs users.  Please leave in place.
# Local Variables:
# fill-column: 70
# End:
