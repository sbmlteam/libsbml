	  MATLAB application programming interface for libSBML

			     Sarah Keating
		 Science and Technology Research Centre
		      University of Hertfordshire
			   Hatfield, AL10 9AB
			     United Kingdom


----------------
1.  Introduction
----------------

The binding provides a MATLAB function (TranslateSBML) that imports an
SBML model into the MATLAB environment as a Matlab structure.


----------------
2.  Installation
----------------


Windows
-------

At the command prompt change to directory '..libsbml-   \binding\matlab'

Run make.bat

This will start Matlab and run a script that 

  1) adds the directory to the matlab search path

  2) checks whether the necessary libraries are on the PATH and if not
     copies the files to the matlabroot\bin\win32 directory as this must be
     on the PATH if matlab is installed.

  3) Prompts user to close Matlab

The TranslateSBML executable is provided with the download and it is not
necessary to build it in order to use it. However the script
BuildTranslate_Win32 can be used within matlab to build TranslateSBML.dll
provided the C compiler that Matlab uses is compatible (Working on this!)

(Use mex -setup in MATLAB to chose a default compiler)



Linux
-----

To build:

  1.  Change to the directory 'bindings\matlab'.

  2.  Ensure that Matlab's mex compiler is in your PATH.

      You can verify this by typing 'mex' or 'which mex' at the
      command-prompt (The mex executable is located in Matlab's bin
      directory).

  3.  Ensure the CFLAGS and LDFLAGS point to the directories
      containing the libsbml header and library files.  For example,
      if you installed libsbml in /usr/local:

        In sh or Bash:

          export CFLAGS=-I/usr/local/include
          export LDLAGS=-L/usr/local/lib

        In csh or tcsh:

          setenv CFLAGS -I/usr/local/include
          setenv LDLAGS -L/usr/local/lib

  4.  Type 'make'

      This should build TranslateSBML.mexglx.


To run:

  Ensure the directory containing TranslateSBML.mexglx is in your Matlab
  path.  For example, at the Matlab prompt:

    >> addpath('bindings/matlab');

  You may wish to add this commands to your Matlab startup script in
  ${HOME}/matlab/startup.m


If you have any questions or problems with the above install
procedure, please email Ben Bornstein (bornstei@caltech.edu).  In a
future release of libsbml, this procedure will be integrated with the
top-level build ('configure') script.


------------
3.  Contents
------------

BuildTranslate_Win32 
Builds the TranslateSBML executable

TranslateSBML('filename') 
Translates a SBML file into a Matlab structure


------------------------------------------
4.  Licensing, Copyrights and Distribution
------------------------------------------

The terms of redistribution for this software are stated in the file
COPYING.txt.




-----------------------------------------------------------------------------
Revision: $Id$
Source:   $Source$
-----------------------------------------------------------------------------
