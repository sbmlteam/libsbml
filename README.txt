
                            l i b S B M L

                            Ben Bornstein
                       with contributions from
         Ben Kovitz, Stefan Hoops, Sarah Keating, Mike Hucka,
                and many others in the SBML community.

         For more information about SBML or libSBML, contact:

                            The SBML Team
                         http://www.sbml.org/
                     mailto:sbml-team@caltech.edu


       Please join the libsbml-discuss mailing list by visiting
    https://utils.its.caltech.edu/mailman/listinfo/libsbml-discuss


--------------
0. Quick Start
--------------

At the Unix command prompt, untar the distribution, cd into it (e.g.,
libsbml-2.1/), and first type:

  ./configure

If you do not have the Xerces-C++ XML library on your system, the
configure step will fail.  In that case, you can try using the Expat
library instead:

  ./configure --with-expat

If you want to build the Java, Python and/or MATLAB libraries, add the
flags --with-java, --with-python, and/or --with-matlab to the
configure command.

Once you've successfully configured libSBML, type:

  make
  make install

To compile C or C++ programs that use libSBML with GCC, use a command
such as:

  gcc -o myapp.c myapp.c -lsbml


---------------
1. Introduction
---------------

This README file describes libSBML, a library for reading, writing and
manipulating files and data streams containing the Systems Biology
Markup Language (SBML).  The library supports both SBML Level 1 (version
1 and 2) and SBML Level 2.

The library is written in ISO C and C++ and provides an API for the
languages C, C++, Java, Python and MATLAB.  LibSBML is known to run on
Linux, Windows, and MacOS X, but is portable and support for other
platforms should be straightforward to implement.

LibSBML is entirely open-source and all specifications and source code
are freely and publicly available.  For more information about SBML,
please see the references section or visit http://sbml.org/.

Feature Highlights:
-------------------

  - Complete user manual

      Documentation is available in the "docs" subdirectory in both
      pre-formatted and source form.  Documents are available in PDF,
      PostScript and HTML formats.

  - Small memory footprint and fast runtime

      The parser is event-based (SAX2) and loads SBML data into C++
      structures that mirror the SBML specification.

      The Gepasi generated 100 Yeast file (2Mb; 2000 reactions
      http://www.gepasi.org/gep3sbml.html) loads in 1.18s on a 1 GHz
      AMD Athlon XP and uses 1.4Mb of memory.

  - Fully supports <notes> and <annotation> elements, including XML
    namespaces

  - Portable, pure ISO C and C++

      The build system uses GNU tools (Autoconf, GNU Make) to build
      shared and static libraries.

  - Support for both the Expat and Apache Xerces-C++ XML Libraries.
    
      The Apache Xerces-C++ XML library supports:
        - SAX 1 and 2
        - DOM 1, 2, and 3
        - Full DTD and Schema validation
        - XML Namespaces
        - Unicode

      SBML Documents are parsed and manipulated in the Unicode codepage
      for efficiency (this is Xerces-C++ native format); however,
      strings are transcoded to the local code page for SBML structures.

  - Well tested: 737 unit tests, 3440 individual assertions.

      The entire library was written using the test-first approach
      popularized by Kent Beck and eXtreme Programming, where it's one
      of the 12 principles.

      Five test cases are responsible for reading entire SBML files
      (three are examples from the L1 document) into memory and
      verifying every field of the resulting structures.

  - Memory tests: 7536 allocations and frees, 0 leaks.

      For use by developers, a custom memory trace facility tracks all
      memory allocated and freed in both the library and all test
      suites.  This facility must be enabled at build time with
      ./configure --enable-memory-tracing.  For performance reasons
      memory tracing should be turned off in production environments.


---------------
2. Installation
---------------

LibSBML depends on Apache's Xerces-C++ XML library for low-level XML
tokenizing and Unicode support.  Xerces is supported on both Unix
(Linux), Windows and MacOS X.  Many popular Linux systems provide the
Xerces library either as part of their standard distribution or as an
optional RPM or Debian package.  For more information, see:


  http://xml.apache.org/xerces-c/


A good way to determine whether or not Xerces-C is installed is to run
the build script (see below); it will halt if it cannot find the
Xerces-C library.

LibSBML is designed to be extremely portable.  It is written in 100%
pure ISO C and C++ and the build system uses GNU tools (Autoconf, GNU
Make).  In most cases, building should be as easy unpacking the
sources and running:


  % ./configure
  % make
  % make check    (optional)
  % make install


However, if Xerces-C is installed in a non-standard place (e.g., your
home directory), configure will not be able to detect it.  In this
case, configure needs to be told explicitly where to find the library.
For example:


  % ./configure --with-xerces=/home/bornstei/software/xerces-c/2.2.0


'make install' copies header files to /usr/local/include/sbml and
(shared and static) library files to /usr/local/lib.  To specify a
different install location use:


  % ./configure --prefix=/my/favorite/path


'make check' is optional and will build and run an extensive suite of
unit tests to verify all facets of the library.  These tests are meant
primarily for developers of libSBML and running them is not required
for the library to function properly.

To run the unit tests a second library is required, libcheck.  Check
is a very lightweight C unit test framework based on the xUnit
framework popularized by Kent Beck and eXtrememe Programming (all of
libSBML was written using the test first approach).  Check is quite
small, once installed, it's only two files: libcheck.a and check.h.
To download Check, visit:


  http://check.sf.net/


Debian users can find Check as a standard add-on package (.deb).

All tests should pass with no failures or errors.  If for some reason
this is not the case on your system, please let me know!


Developer's Note:
-----------------

In addition to the unit tests, a custom memory tracing facility is
available:


  % ./configure --enable-memory-tracing
  % make
  % make check


With memory tracing turned on, every piece of memory in both the
library and all test suites is tracked.  At the end of the run
statistics are printed on total memory allocations, deallocations and
leaks.

The memory statistics should report zero leaks.  Again, please let me
know if you experience otherwise.

For performance reasons, memory tracing should be turned off in
production environments:


  % ./configure --disable-memory-tracing




--------------------------------------
3. Building on Windows with Visual C++
--------------------------------------

To build libsbml on Windows:

   1. Install Xerces-C if it's not already installed.

   2. Download the libsbml tar archive from sf.net/projects/sbml and
   extract using WinZip.  Do not extract the tar archive using the "tar"
   that comes with CygWin; that "tar" does not translate line
   terminators so that they'll work with Visual C++.

   3. Using CygWin, cd to the (libsbml) directory and run:

      ./configure

   4. In Visual Studio 7, open (libsbml)/win32/libsbml.vcproj.
      In Visual Studio 6, open (libsbml)/win32/libsbml.dsw.

   5. Select Tools->Options, and select Projects/Directories (VS7) or
   click the Directories tab (VS6).

   The screen for indicating which directories contain project-relevant
   files appears.

   6. At "Show Directories For:", select "Include Files".  Add the
   following if they're not already shown:

      The include directory from Xerces-C.
      (libsbml)/src
      (libsbml)/win32/include

   7. At "Show Directories For:", select "Library Files".  Add the
   following it's not already shown:

      The lib directory from Xerces-C.


-----------------------------------
4. The libsbml-discuss Mailing List
-----------------------------------

Please join the libsbml-discuss mailing list by visiting the URL

  https://utils.its.caltech.edu/mailman/listinfo/libsbml-discuss

Being a member of libsbml-discuss will enable you to keep in touch with
the latest developments in LibSBML as well as to ask questions and share
your experiences with fellow developers and users of libSBML.

The libsbml-discuss archives are available at http://sbml.org/forums/.



-------------------------------------------
File author: B. Bornstein
Last Modified: $Date$
-------------------------------------------

