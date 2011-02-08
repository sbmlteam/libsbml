	 MATLAB application programming interface for libSBML

			    Sarah Keating

			    The SBML Team
			 http://www.sbml.org/
		     mailto:sbml-team@caltech.edu


		  Date of last update to this file:
	$Date$


---------------
1.  Quick Start
---------------

The libSBML MATLAB binding provides a MATLAB function called
TranslateSBML.  This function can be used to import an SBML model 
into the MATLAB environment as a MATLAB data structure.

The Windows distribution of libSBML includes a precompiled copy of
TranslateSBML.  The source distribution of libSBML for Linux, MacOS X
and other Unix-like environments does not come with precompiled
executables and you will need to build TranslateSBML as described
below.


-------------------------------------------
2.  Configuration and Installation from src
-------------------------------------------

Windows
-------

Build libSBML as described in the libsbml instructions.

Start MATLAB.

Change to the directory holding the file you
are reading right now, i.e., 'libsbml-VERSION\src\binding\matlab'
where "VERSION" is the version of libSBML you have obtained.

Run the buildWin script follwed by the install_Win32 or 
install_Win64 script as appropriate for your MATLAB installation.

NOTE: You may need to edit the install_Win32/64 script to include copying
other dependent libraries to the matlab path.




Linux and MacOS X
-----------------

The first step is to run the top-level libSBML 'configure' script with
the --with-matlab option.  (See the top-level README.txt file in the
libSBML source distribution.)  You will probably have to supply the
pathname of the directory tree where MATLAB is installed on your
system.  For example, on MacOS X with release R14sp3 of MATLAB, run
the following shell command in the top-level libsbml directory:

    ./configure --with-matlab=/Applications/MATLAB704

The libSBML 'configure' script will construct a Makefile in this
directory (i.e., the directory containing the file you are reading
right now), and executing 'make' from the top level will run the
MATLAB mex compiler to create an object file for use with MATLAB.  The
'make install' step will then copy this object file to the library
installation directory chosen by the user.  (This directory is
/usr/local/lib by default.)

The second step is to configure MATLAB to look for the TranslateSBML
function in the libSBML library installation directory.  You can do
this by using a MATLAB command such as

    addpath('/usr/local/lib');

at the MATLAB prompt.  You may wish to add this command to your MATLAB
startup script in ${HOME}/matlab/startup.m.


------------------------------------------
3.  Licensing, Copyrights and Distribution
------------------------------------------

The terms of redistribution for this software are stated in the files
LICENSE.txt and COPYING.txt at the top level of the libSBML
distribution.





-----------------------------------------------------------------------------
File author: B. Bornstein, S. Keating, M. Hucka
Last Modified: $Date$
Last Modified By: $Author$
$HeadURL$
-----------------------------------------------------------------------------

# The following is for [X]Emacs users.  Please leave in place.
# Local Variables:
# fill-column: 70
# End:
