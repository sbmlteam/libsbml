
                            l i b S B M L

                   Ben Bornstein and Sarah Keating
           with contributions from (in alphabetical order)
      Bill Denney, Christoph Flamm, Akira Funahashi, Mike Hucka,
     Ralph Gauges, Martin Ginkel, Alex Gutteridge, Stefan Hoops,
     Akiya Jouraku, Ben Kovitz, Rainer Machne, Nicolas Rodriguez,
                           and many others.

        More information about libSBML is available online at
                   http://sbml.org/Software/libSBML

       Please report problems with libSBML using the tracker at
	    http://sbml.org/software/libsbml/issue-tracker

    Online forums for discussing libSBML and SBML are available at
                        http://sbml.org/Forums

   ,----------------------------------------------------------------.
  | Table of contents                                               |
  | 1. Quick start                                                  |
  | 2. Introduction                                                 |
  | 3. Detailed instructions for configuring and installing LibSBML |
  | 4. Reporting bugs and other problems                            |
  | 5. Mailing lists                                                |
  | 6. Licensing and distribution                                   |
  | 7. Acknowledgments                                              |
   `----------------------------------------------------------------'
    Date of last update to this file: $Date$


--------------
1. QUICK START
--------------

1.1 Linux, MacOS X and Solaris
------------------------------

At the Unix command prompt, untar the distribution, cd into it (e.g.,
libsbml-2.4/), and first type:

  ./configure

LibSBML will try to find and use libxml2 as its XML parser library.
If you do not have libxml2 version 2.6.16 or later on your system, the
configure step will fail.  In that case, you can try using the Expat
or Xerces libraries instead.  For Expat, use

  ./configure --with-expat

and for Xerces, use

  ./configure --with-xerces

IMPORTANT: If you are using the Xerces XML library, beware there is a
bug in Xerces 2.6.0 that cannot be worked around at this time and
causes errors in software using it.  Xerces versions 2.2 -
2.5 and 2.7.0 are known to work properly.

By default, libSBML only builds the C and C++ API library.  If you
want to configure libSBML to build the Java, Python, Perl, Lisp,
MATLAB, Ruby and/or Octave API libraries as well, add the flags
--with-java, --with-python, --with-perl, --with-lisp, --with-matlab,
--with-ruby, and/or --with-octave to the configure command.  You can
combine options as you need.

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
for information about ldconfig, LD_LIBRARY_PATH/DYLD_LIBRARY_PATH, and
related matters.

Documentation for libSBML is available as a separate download from the
same locations as the libSBML distribution (namely, the SBML project
SourceForge and the http://sbml.org/software/libsbml web page).  You
may also regenerate the documentation from the source code
distribution if you have Doxygen version 1.5.4 installed and have
configured libSBML with the --with-doxygen flag.  Then you can execute
the following to generate and install the libSBML documentation files:

  make install-docs


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

The library is written in ISO standard C and C++ and currently
provides an API for the languages C, C++, Java, Lisp, Perl, Python,
Ruby, MATLAB and Octave.  LibSBML is known to run on Linux, Windows,
and MacOS X, but is portable and support for other platforms should be
straightforward to implement.

LibSBML is entirely open-source and all specifications and source code
are freely and publicly available.  For more information about SBML,
please visit the website http://sbml.org/.

Feature Highlights:
-------------------

* Parser abstraction layer.  LibSBML relies on third-party XML parser
  libraries, but thanks to its implementation of an abstraction layer,
  libSBML can use any of three different popular XML parser libraries:
  Expat, Apache Xerces-C++, and Libxml2.  LibSBML provides identical
  functionality and checking of XML syntax is now available no matter
  which one is used.  SBML Documents are parsed and manipulated in the
  Unicode codepage for efficiency; however, strings are transcoded to
  the local code page for SBML structures.

* Small memory footprint and fast runtime.  The parser is event-based
  (SAX2) and loads SBML data into C++ structures that mirror the SBML
  specification.

* Full SBML Support.  All constructs in SBML Level 1 (Versions 1 and
  2) and SBML Level 2 are supported.  These exceptions will be removed
  in the near future.  LibSBML handles such SBML differences as the
  alternate spellings of species and annotation between the SBML
  specifications.  For compatibility with some technically incorrect
  but popular Level 1 documents, the parser recognizes and stores
  notes and annotations defined for the top-level <sbml> element
  (logging a warning).

* Dimensional analysis and unit checking.  LibSBML implements a
  thorough system for dimensional analysis and checking units of
  quantities in a model.  The validation rules for units that are
  specified in SBML Level 2 Version 2 and Version 3 are fully
  implemented, including checking units in mathematical formulas.

* Unified SBML Level 2 and Level 1 object models.  All objects have
  .getSBMLDocument(), .getModel(), .getLevel(), and .getVersion(),
  methods among other things.  Also, the interface to SBML's Rules
  abstracts away some of the individual rule type (assignment, rate,
  algebraic) differences.

* Access to SBML annotations and notes as XML objects.  Annotations
  and notes in libSBML 3.x are read and manipulated as XML structures;
  a text-string interface is available for backward compatibility with
  the libSBML 2.x series.  Further, in order to facilitate the support
  of MIRIAM compatible annotations, there are new object classes
  ModelHistory and CVTerm.  These classes facilitate the creation and
  addition of RDF annotations inside <annotation> elements by
  providing parsing and manipulation functions that treat the
  annotations in terms of XMLNode objects implemented by the new XML
  layer.  Both ModelHistory and CVTerm follow the general libSBML
  format of providing getters and setters for each variable stored
  within the class.
      
* Interfaces for C, C++, Java, Lisp, Python, Perl, MATLAB, Ruby and
  Octave.  C and C++ interfaces are implemented natively; the Java,
  Perl, Python, and Ruby interfaces are implemented using SWIG, the
  Simplified Wrapper Interface Generator; and the rest are implemented
  using custom hand-written interface code.

* Well tested: version 3.0.0 has over 1280 unit tests and 5800
  individual assertions.  The entire library was written using the
  test-first approach popularized by Kent Beck and eXtreme
  Programming, where it's one of the 12 principles.
    
* Written in portable, pure ISO C and C++. The build system uses GNU
  tools (Autoconf, GNU Make) to build shared and static libraries.

* Full XML and SBML Validation.  All XML and Schema warning, error and
  fatal error messages are logged with line and column number
  information and may be retrieved and manipulated programmatically.

* Complete user manual.  The manual is generated from the source code
  and closely reflects the actual API.


---------------------------------------------------------------
3. DETAILED INSTRUCTIONS FOR CONFIGURING AND INSTALLING LIBSBML
---------------------------------------------------------------

Detailed instructions for building and configuring libSBML are now
included as part of the full documentation available for libSBML.
You may find the documentation online at 

  http://sbml.org/software/libsbml

You may also download a compressed archive of the formatted
documentation from the same place you found this libSBML source
distribution (e.g., from http://sourceforge.net/projects/sbml/).

Lastly you can format the documentation from the sources, which are
provided as part of this libSBML source distribution in the "docs"
subdirectory.  (To do that, however, you will need certain additional
software tools such as Doxygen and a full latex distribution.)


------------------------------------
4. REPORTING BUGS AND OTHER PROBLEMS
------------------------------------

We invite you to report bugs and other problems using the issue
tracker for libSBML on SourceForge.  The following URL will take you
there directly:

  http://sbml.org/software/libsbml/issue-tracker

You can also ask questions on the 'sbml-interoperability' mailing
list, either over email or using the web forum interface (see
http://sbml.org/Forums/).  This may even have advantages, such as that
other people may also have experienced the issue and offer a
workaround more quickly than the libSBML developers can respond.


----------------
5. MAILING LISTS
----------------

There are two kinds of mailing lists available: normal discussion
lists for humans, and a SVN change notification list.

Discussion lists
----------------

All discussion lists, their web interfaces and their RSS feeds are at

		       http://sbml.org/Forums/

If you use SBML, you are strongly encouraged to sign up for the SBML
announcements mailing list.  It is a low-volume, broadcast-only list.

If you use libSBML, you are also encouraged to subscribe to or monitor
via RSS the 'sbml-interoperability' list, where people discuss the
development, use, and interoperability of software that supports SBML,
including libSBML.

If you are interested in helping to modify libSBML, or just want to
know about deeper issues and technical topics, you are welcome to
subscribe to the 'libsbml-development' mailing list.  Being a member
of libsbml-development will enable you to keep in touch with the
latest developments in libSBML as well as to ask questions and share
your experiences with fellow developers and users of libSBML.

SVN notification
----------------

If you are obtaining your libSBML files from SVN, you may wish to
subscribe to the mailing list sbml-svn, to be apprised of changes to
the SVN repository as soon as they are committed.  You can join the
list by visiting the following URL:

  https://lists.sourceforge.net/lists/listinfo/sbml-svn


-----------------------------
6. LICENSING AND DISTRIBUTION
-----------------------------

Copyright 2005-2007 California Institute of Technology.
Copyright 2002-2005 California Institute of Technology and the
                    Japan Science and Technology Agency.

LibSBML is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation; either version 2.1 of the License, or
any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
documentation provided hereunder is on an "as is" basis, and the
California Institute of Technology and the Japan Science and
Technology Agency have no obligations to provide maintenance, support,
updates, enhancements or modifications.  In no event shall the
California Institute of Technology or the Japan Science and Technology
Agency be liable to any party for direct, indirect, special,
incidental or consequential damages, including lost profits, arising
out of the use of this software and its documentation, even if the
California Institute of Technology and/or the Japan Science and
Technology Agency have been advised of the possibility of such damage.
See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library in the file named "LICENSE.txt"
included with the software distribution.  A copy is also available
online at the Internet address
http://sbml.org/software/libsbml/license.html for your convenience.
You may also write to obtain a copy from the Free Software Foundation,
Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.


------------------
7. ACKNOWLEDGMENTS
------------------

This and other projects of the SBML Team have been supported by the
following organizations: the National Institutes of Health (USA) under
grants R01 GM070923 and R01 GM077671; the International Joint Research
Program of NEDO (Japan); the JST ERATO-SORST Program (Japan); the
Japanese Ministry of Agriculture; the Japanese Ministry of Education,
Culture, Sports, Science and Technology; the BBSRC e-Science
Initiative (UK); the DARPA IPTO Bio-Computation Program (USA); the
Army Research Office's Institute for Collaborative Biotechnologies
(USA); the Air Force Office of Scientific Research (USA); the
California Institute of Technology (USA); the University of
Hertfordshire (UK); the Molecular Sciences Institute (USA); the
Systems Biology Institute (Japan); and Keio University (Japan).



-----------------------------------------------
File author: M. Hucka, B. Bornstein, S. Keating
Last Modified: $Date$
Last Modified By: $Author$
$Source$
-----------------------------------------------

# The following is for [X]Emacs users.  Please leave in place.
# Local Variables:
# fill-column: 70
# End:
