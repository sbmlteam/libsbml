	  MATLAB application programming interface for libSBML

			     Sarah Keating

		 Science and Technology Research Centre
		      University of Hertfordshire
			   Hatfield, AL10 9AB
			     United Kingdom

			     The SBML Team
			  http://www.sbml.org/
		     mailto:sbml-team@caltech.edu


---------------
1.  Quick Start
---------------

The binding provides a MATLAB function (TranslateSBML) that imports an
SBML model into the MATLAB environment as a Matlab structure.  After
building and installing libSBML with 


----------------------------------
2.  Configuration and Installation
----------------------------------


Windows
-------

At the command prompt change to directory '..libsbml-   \binding\matlab'

Run make.bat

This will start MATLAB and run a script that 

  1) adds the directory to the MATLAB search path

  2) checks whether the necessary libraries are on the PATH and if not
     copies the files to the matlabroot\bin\win32 directory as this must be
     on the PATH if MATLAB is installed.

  3) Prompts user to close MATLAB

The TranslateSBML executable is provided with the download and it is not
necessary to build it in order to use it. However the script
BuildTranslate_Win32 can be used within MATLAB to build TranslateSBML.dll
provided the C compiler that MATLAB uses is compatible (Working on this!)

(Use mex -setup in MATLAB to chose a default compiler)


Linux and MacOS X
-----------------

The first step is to run the top-level 'configure' script with the
--with-matlab option.  (See the top-level README.txt file.)  You will
probably have to supply the pathname of the directory tree where MATLAB is
installed on your system.  For example, on MacOS X with release R13sp1 of
MATLAB, run the following shell command in the top-level libsbml directory:

    ./configure --with-matlab=/Applications/MATLAB6p5p1/

The 'configure' script will construct a Makefile in this directory, and a
'make' from the top level will run the MATLAB mex compiler to create an
object file for use with MATLAB.  The 'make install' step will then copy
this object file to the library installation directory chosen by the user.
(This directory is /usr/local/lib by default.)

To use the MATLAB functionality, you need to configure MATLAB to look
for extension programs in the library installation directory.  Do this by
using a command such as

    addpath('/usr/local/lib');

at the MATLAB prompt.  You may wish to add this command to your MATLAB
startup script in ${HOME}/matlab/startup.m.


------------------------------------------
3.  Licensing, Copyrights and Distribution
------------------------------------------

The terms of redistribution for this software are stated in the file
COPYING.txt at the top level of the libSBML distribution.




-----------------------------------------------------------------------------
Revision: $Id$
Source:   $Source$
-----------------------------------------------------------------------------
