/**
 * \mainpage LibSBML API Reference Manual
 *
 * \author Ben Bornstein, <em>with contributions from</em> Ben Kovitz,
 * Stefan Hoops, Sarah Keating, Mike Hucka, and many others in the
 * SBML community.
 *
 * This manual describes the application programming interface (API) of
 * libSBML, a library for writing and manipulating the Systems Biology
 * Markup Language (SBML).  Currently, the library supports all of SBML
 * Level&nbsp;1 Version&nbsp;1 and Version&nbsp;2, and nearly all of SBML
 * Level&nbsp;2 Version&nbsp;1.  For more information about SBML, please
 * see the references or visit http://www.sbml.org/ on the Internet.
 * LibSBML is entirely open-source under the terms of the GNU LGPL, and all
 * source code and other materials are freely and publicly available.
 *
 * Some of the features of libSBML include:
 * 
 * \li <em>Complete user manual</em>. Documentation is available in
 *   the "docs" subdirectory in both pre-formatted and source form.  Documents
 *   are available in PDF, PostScript and HTML formats.
 *   
 * \li <em>Small memory footprint and fast runtime</em>. The parser
 *   is event-based (SAX2) and loads SBML data into C++ structures that mirror
 *   the SBML specification.  As an example of libSBML's performance, the
 *   Gepasi generated 100 Yeast file (2Mb, 2000 reactions; see
 *   http://www.gepasi.org/gep3sbml.html) loads in 1.18s on a 1 GHz AMD
 *   Athlon XP and uses 1.4Mb of memory.
 *       
 * \li <em>Well tested: 737 unit tests, 3440 individual
 *       assertions</em>.  The entire library was written using the test-first
 *   approach popularized by Kent Beck and eXtreme Programming, where it's one
 *   of the 12 principles.  In libSBML, five test cases are responsible for
 *   reading entire SBML files (three are examples from the Level 1
 *   specification document) into memory and verifying every field of the
 *   resulting structures.
 *       
 * \li <em>Memory tests: 7536 allocations and frees, 0 leaks</em>.
 *   For use by developers, a custom memory trace facility tracks all memory
 *   allocated and freed in both the library and all test suites.  This
 *   facility must be enabled at libSBML configuration time with
 *   <tt>./configure --enable-memory-tracing</tt>.  (For performance reasons
 *   memory tracing should be turned off in production environments.)
 *   
 * \li <em>Interfaces for C, C++, Java, Python and MATLAB</em>.  C and
 *   C++ interfaces are implemented natively; the Java and Python interfaces
 *   are implemented using SWIG, the Simplified Wrapper Interface Generator.
 * 
 * \li <em>Full SBML Support</em>.  All constructs in SBML Level 1
 *   (Versions 1 and 2) and SBML Level 2 are supported, with the exceptions
 *   noted above (i.e., RDF, and three rarely-used MathML constructs).  The
 *   exceptions will be removed in the near future.  libSBML handles such
 *   SBML differences as the alternate spellings of <em>species</em> and
 *   <em>annotation</em> between the SBML specifications.
 *   The full-text of <tt>&lt;notes&gt;</tt> and <tt>&lt;annotation&gt;</tt> elements (the
 *   latter including namespace declarations) may be retrieved from any
 *   SBML object.  For compatibility with some technically incorrect but
 *   popular Level 1 documents, the parser recognizes and stores notes
 *   and annotations defined for the top-level <tt>&lt;sbml&gt;</tt> element
 *   (logging a warning).
 *     
 * \li <em>Written in portable, pure ISO C and C++</em>. The build
 *   system uses GNU tools (Autoconf, GNU Make) to build shared and static
 *   libraries.
 *   
 * \li <em>Support for both the Expat and Apache Xerces-C++ XML
 *       Libraries</em>.  SBML Documents are parsed and manipulated in the
 *   Unicode codepage for efficiency (this is Xerces-C++ native format);
 *   however, strings are transcoded to the local code page for SBML
 *   structures.
 * 
 * \li <em>Full XML Schema Validation</em>. The library can use the
 *   Apache Xerces-C++ XML library, which supports full XML Schema validation.
 *   All XML and Schema warning, error and fatal error messages are logged
 *   with line and column number information and may be retrieved and
 *   manipulated programmatically.  The XML Schema file used by the parser for
 *   validation is configurable.
 **/
