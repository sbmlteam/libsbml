
                            l i b S B M L

                   Ben Bornstein and Sarah Keating
                       with contributions from
         Mike Hucka, Akiya Jouraku, Ben Kovitz, Stefan Hoops, 
     Ralph Gauges, Martin Ginkel, Christoph Flamm, Rainer Machne,
                and many others in the SBML community.

         For more information about SBML or libSBML, contact:

                            The SBML Team
                         http://www.sbml.org/
                     mailto:sbml-team@caltech.edu


       Please join the libsbml-discuss mailing list by visiting
            http://www.sbml.org/forums/index.php?t=pre_reg

       Please report problems with libSBML using the tracker at
            http://sbml.org/software/libsbml/issue-tracker

   ,----------------------------------------------------------------.
  | Table of contents                                               |
  | 1. Quick start                                                  |
  | 2. Introduction                                                 |
  | 3. Detailed instructions for configuring and installing LibSBML |
  | 4. Building on Windows with Visual C++                          |
  | 5. The libsbml-discuss mailing list                             |
  | 6. The sbml-cvs-libsbml mailing list                            |
  | 7. Reporting bugs and other problems                            |
   `----------------------------------------------------------------'
    Date of last update to this file: $Date$


--------------
1. QUICK START
--------------

If you are using the Xerces XML library, beware there is a bug in
Xerces 2.6.0 that cannot be worked around at this time and causes
errors in software using it.  Xerces versions 2.2 - 2.5 and 2.7.0
are known to work properly.  We recommend Xerces 2.7.

1.1 Linux, MacOS X and Solaris
------------------------------

At the Unix command prompt, untar the distribution, cd into it (e.g.,
libsbml-2.4/), and first type:

  ./configure

If you do not have the Xerces-C++ XML library on your system, the
configure step will fail.  In that case, you can try using the Expat
library instead:

  ./configure --with-expat

By default, libSBML only builds the C and C++ API library.  If you
want to configure libSBML to build the Java, Python, Perl, Lisp and/or
MATLAB API libraries as well, add the flags --with-java,
--with-python, --with-perl, --with-lisp and/or --with-matlab to the
configure command.  You can combine options as you need.

Depending on your system, you may need to tell the configure program
where to find some of these extra components by adding a directory
path after the option.  For example, if you wish to use a copy of Java
whose components are in /usr/local (with files in /usr/local/bin and
/usr/local/lib), use

  ./configure --with-expat --with-python --with-java=/usr/local

Once you've successfully configured libSBML, run the following two
commands to build and install it:

  make
  make install

To compile C or C++ programs that use libSBML with GCC, use a command
such as the following, where -lsbml tells the compiler to link with
the installed libSBML library:

  gcc -o myapp.c myapp.c -lsbml

If the compiler cannot find the library, refer to Section 3.2 below
for information about ldconfig, LD_LIBRARY_PATH, and related matters.


1.2 Windows
-----------

We should point out at the outset that the main developers have been
using only Windows XP, and do not have direct experience building
libSBML under other versions of Windows.  Users do report some success
on other versions of Windows, but your mileage may vary.

Download and run the self-extracting windows installer for libSBML.
There are debug (libsbmld) and release (libsbml) versions of libSBML,
with .dll and .lib files for both versions in the 'win32/bin'
subdirectory of the libSBML distribution.  Header files are located in
the subdirectory 'win32/include/sbml'.

Users of Visual C++ should make their Visual C++ projects link with
the files libsbml.lib or libsbmld.lib and generate code for the
Multithreaded DLL or Debug Multithreaded DLL version of the VC++
runtime, respectively.


---------------
2. INTRODUCTION
---------------

This README file describes libSBML, a library for reading, writing and
manipulating files and data streams containing the Systems Biology
Markup Language (SBML).  The library supports both SBML Level 1 and
SBML Level 2.

The library is written in ISO C and C++ and currently provides an API
for the languages C, C++, Java, Lisp, Perl, Python and MATLAB.
LibSBML is known to run on Linux, Windows, and MacOS X, but is
portable and support for other platforms should be straightforward to
implement.

LibSBML is entirely open-source and all specifications and source code
are freely and publicly available.  For more information about SBML,
please visit the website http://sbml.org/.

Feature Highlights:
-------------------

  - Complete user manual

      Documentation is available in the "docs" subdirectory in both
      pre-formatted and source form.  Full documentation, including
      copies of the API references, are also available online from
      http://sbml.org/software/libsbml/.

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

  - Support for the Apache Xerces-C++, libXML, and Expat XML Libraries.
    
      The Apache Xerces-C++ XML library supports:
        - SAX 1 and 2
        - DOM 1, 2, and 3
        - Full DTD and Schema validation
        - XML Namespaces
        - Unicode

      SBML Documents are parsed and manipulated in the Unicode codepage
      for efficiency (this is Xerces-C++ native format); however,
      strings are transcoded to the local code page for SBML structures.

  - Well tested: over 1280 unit tests, over 5500 individual assertions.

      The entire library was written using the test-first approach
      popularized by Kent Beck and eXtreme Programming, where it's one
      of the 12 principles.  Extensive unit tests are provided and
      can be invoked by linking with the Check library (an option
      provided in the 'configure' program).


---------------------------------------------------------------
3. DETAILED INSTRUCTIONS FOR CONFIGURING AND INSTALLING LIBSBML
---------------------------------------------------------------

LibSBML requires a separate XML library for low-level XML tokenizing
and Unicode support.  As of libSBML 3.0, it supports the Xerces-C++,
Expat, and libXML XML libraries on Linux, Windows, MacOS X and Solaris.
Many Linux systems provide one or more of these libraries either as
part of their standard distribution or as an optional RPM, Debian,
Mandrake or other package.  For more information, see
http://xml.apache.org/xerces-c/ for Xerces and http://expat.sf.net for
Expat.

3.1 Linux, MacOS X and Solaris
------------------------------

If you have obtained the libSBML source code distribution, then at
your Linux, MacOS X or Solaris command prompt, unpack the
distribution, cd into the directory created (e.g., libsbml-2.4.0), and
type the following command in a terminal shell window to configure
libSBML for your system:

  ./configure

To specify Expat explicitly rather than the libSBML default of Xerces,
use a command such as the following instead (and make sure to read
about the limitations surrounding the use of Expat explained below):

  ./configure --with-expat

If either Expat or Xerces is installed in a non-standard location on
your computer system (e.g., a private home directory), configure will
not be able to detect it.  In this case, configure needs to be told
explicitly where to find the libraries.  Use the following forms:

  ./configure --with-expat="DIR"

or

  ./configure --with-xerces="DIR"

where DIR is the parent directory of where the 'include' and 'lib'
directories of Xerces or Expat (whichever one you are trying to use)
is located.  For example, on MacOS X, if you used Fink to install
Expat in Fink's default software tree, you would configure libSBML
using the following command:

  ./configure --with-xerces="/sw"

During the installation phase (i.e., during make install, discussed
below), the default libSBML installation commands will copy header
files to /usr/local/include/sbml, the (shared and static) library
files to /usr/local/lib, and documentation files in various formats to
/usr/local/share/doc/libsbml-VERSION, by default.  To specify a
different installation location, use the --prefix argument to
configure.  For example,

  ./configure --prefix="/my/favorite/path"

Of course, you can combine the flags to configure, giving both
--prefix and --with-expat or --with-xerces to set both options.  


3.2 Building and Installing LibSBML
-----------------------------------

Once configured, building should be very easy.  Simply execute the
following commands at your Linux, MacOS X or Solaris command prompt:

  make
  make install

Note that you will probably have to perform the make install command
as the user 'root' on your system if you used the default installation
directory (/usr/local) or you set --prefix to a system directory that
only root is permitted to write into.

Finally, on most platforms, you will also need to either run the
system program 'ldconfig' as user 'root' (consult the man page for
ldconfig if this is unfamiliar), or else set the environment variable
LD_LIBRARY_PATH in your terminal shell window.  (On MacOS X, the
variable is named DYLD_LIBRARY_PATH.)  If you do not do this,
attempting to link other programs with the libSBML library will fail
with errors about being unable to find it.

If all went as it should, libSBML should end up compiled and installed
on your system, in either the default location ('/usr/local/') or in
the location you indicated during the configuration step as explained
above.


3.3 Additional Options for Configuring LibSBML
----------------------------------------------

In addition to the --prefix, --with-expat and --with-xerces options
already described, the libSBML configuration command supports the
options described below.


3.3.1 Interfaces to Java, Lisp, Perl, Python and MATLAB
.......................................................

libSBML includes language bindings for Java, Perl, Python, Common Lisp
and MATLAB, enabling you to write Java, Perl, Python, Lisp and MATLAB
programs that call libSBML methods, and work with libSBML through
Python's and MATLAB's interactive modes.  Short tutorials for how to
use these facilities are available in the libSBML documentation.

To enable the library extensions for Java, Python, Lisp and MATLAB,
you need to supply additional options to configure.  These options are
--with-java, --with-perl, --with-python, --with-lisp, and
--with-matlab.  As with other configure options, these three take an
optional prefix argument; for example,

  ./configure --with-java="DIR"

If you want to build multiple language bindings for libSBML, combine
multiple flags together as in the following example:

  ./configure --with-java --with-python

The libSBML distribution ships with certain interface files provided,
so that you do not need to have the software necessary to recreate
them.  However, if you obtained the libSBML distribution from CVS or
want to recreate the files deliberately, you will need SWIG, the
Simplified Wrapper and Interface Generator.  More information about
SWIG is available from http://www.swig.org.  At this time, libSBML is
known to work only with the latest stable version of SWIG (1.3.24);
earlier versions are known not to work.  To tell configure to enable
the use of SWIG to regenerate the interface files, use the --with-swig
option to configure.  If your copy of SWIG is installed in a
non-standard location, you can specify it on the configure command
line like this:

  ./configure --with-swig="DIR"

As with Expat and Xerces, the directory '/sw' is what you would
specify if you were running on MacOS X and you used Fink to install
SWIG, Python, etc.


3.3.2 Unit Testing
..................

libSBML provides built-in facilities for testing itself.  To run the
unit tests, a second library is required, libcheck (version 0.9.2 or
higher).  Check is a very lightweight C unit test framework based on
the xUnit framework popularized by Kent Beck and eXtreme Programming.
Check is quite small and once installed, it consists of only two
files: libcheck.a and check.h.  To download Check, visit:

  http://check.sf.net/

(Note: Debian users can find Check as a standard add-on package (.deb).
As of 11 Feb. 2007, MacOS X users cannot use Fink's version of check
because it is an old version, 0.8.x.  LibSBML needs version 0.9.x.)

To enable the unit testing facilities in libSBML, add the --with-check
flag to the configure command:

  ./configure --with-check

Following this, you must build libSBML and then you can run the tests:

  make
  make check

The make check step is optional and will build and run an extensive
suite of unit tests to verify all facets of the library.  These tests
are meant primarily for developers of libSBML and running them is not
required for the library to function properly.  All tests should pass
with no failures or errors.  If for some reason this is not the case
on your system, please submit a bug report using the facilities at
http://sbml.org/software/libsbml/.


--------------------------------------
4. BUILDING ON WINDOWS WITH VISUAL C++
--------------------------------------

The Windows distributions of libSBML come in the form of both
precompiled binaries (with a self-extracting installer), and source
code.  The precompiled binaries come ready-to-use and only need to be
installed.  The source distribution is a .zip file containing the
libSBML code that must be extracted in a directory on your system,
compiled and installed.

As mentioned above, libSBML requires that the Xerces-C or Expat
XML parsing libraries be available on your computer prior to
attempting to compile libSBML.  The DLL for xerces or expat must be
placed in the win32/bin subdirectory of the libSBML directory.

To build libsbml on Windows:
   1. You need to create the include directory structure.
      In (libsbml)/win32 run the createInclude batch file.


   2. In Visual Studio 7, open (libsbml)/win32/MSVC7/libsbml.sln.
      In Visual Studio 6, open (libsbml)/win32/MSVC6/libsbml.dsw.

   3. Select Tools->Options, and select Projects/Directories (VS7) or
   click the Directories tab (VS6).

   The screen for indicating which directories contain project-relevant
   files appears.

   4. At "Show Directories For:", select "Include Files".  Add the
   following if they're not already shown:

      (libsbml)/src

   5. At "Show Directories For:", select "Library Files".  Add the
   following it's not already shown:

      (libsbml)/win32/bin

   6. Select Build->Rebuild All from the Visual Studio main menu.


-----------------------------------
5. THE libsbml-discuss MAILING LIST
-----------------------------------

Please join the libsbml-discuss mailing list by visiting the URL

  http://www.sbml.org/forums/index.php?t=pre_reg

Being a member of libsbml-discuss will enable you to keep in touch
with the latest developments in LibSBML as well as to ask questions
and share your experiences with fellow developers and users of
libSBML.  The mailing list is vigilantly moderated by humans to remove
spam and off-topic postings.

A web-based interface is available at http://sbml.org/forums/ for
those who prefer to interact with the list via a web interface.

The libsbml-discuss archives are available at http://sbml.org/forums/.


------------------------------------
6. THE sbml-cvs-libsbml MAILING LIST
------------------------------------

If you are obtaining your libSBML files from CVS, you may wish to
subscribe to the mailing list sbml-cvs-libsbml, to be apprised of
changes to the CVS repository as soon as they are committed.  You can
join the list by visiting the following URL:

  https://lists.sourceforge.net/lists/listinfo/sbml-cvs-libsbml

Thank you for your interest in SBML and libSBML.


------------------------------------
7. REPORTING BUGS AND OTHER PROBLEMS
------------------------------------

We invite you to report bugs and other problems using the issue
tracker for libSBML on SourceForge.  The following URL will take you
there directly:

  http://sbml.org/software/libsbml/issue-tracker

You can also report problems on libsbml-discuss@caltech.edu.  This may
even have advantages, such as that other people may also have
experienced the issue and offer a workaround more quickly than the
libSBML developers can respond.




-------------------------------------------
File author: B. Bornstein, M. Hucka
Last Modified: $Date$
Last Modified By: $Author$
$Source$
-------------------------------------------

# The following is for [X]Emacs users.  Please leave in place.
# Local Variables:
# fill-column: 70
# End:
