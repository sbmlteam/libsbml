%feature("docstring") getLibSBMLVersion "
 Returns the version number of this copy of libSBML as an integer.

 Returns the libSBML version as an integer; version 1.2.3 becomes
 10203.
";


%feature("docstring") getLibSBMLDottedVersion "
 Returns the version number of this copy of libSBML as a string.

 Returns the libSBML version as a string; version 1.2.3 becomes
 '1.2.3'.

 See also getLibSBMLVersionString().
";


%feature("docstring") getLibSBMLVersionString "
 Returns the version number of this copy of libSBML as a string
 without  periods.

 Returns the libSBML version as a string: version 1.2.3 becomes
 '10203'.

 See also getLibSBMLDottedVersion().
";


%feature("docstring") SBMLReader "
 Methods for reading SBML from files and text strings.

 This class of objects is defined by libSBML only and has no direct
 equivalent in terms of SBML components. This class is not prescribed
 by  the SBML specifications, although it is used to implement
 features  defined in SBML.

 The SBMLReader class provides the main interface for reading SBML
 content from files and strings.  The methods for reading SBML all
 return  an SBMLDocument object representing the results.

 In the case of failures (such as if the SBML contains errors or a
 file  cannot be read), the errors will be recorded with the
 SBMLErrorLog  object kept in the SBMLDocument returned by SBMLReader.
 Consequently,  immediately after calling a method on SBMLReader,
 callers should always  check for errors and warnings using the
 methods for this purpose  provided by SBMLDocument.

 For convenience as well as easy access from other languages besides
 C++,  this file also defines two global functions,
 libsbml.readSBML()  and libsbml.readSBMLFromString().  They are
 equivalent to creating an SBMLReader  object and then calling the
 SBMLReader.readSBML() or  SBMLReader.readSBMLFromString()  methods,
 respectively.

 Support for reading compressed files
 ======================================================================

 LibSBML provides support for reading (as well as writing) compressed
 SBML files.  The process is transparent to the calling  application
 -- the application does not need to do anything  deliberate to invoke
 the functionality.  If a given SBML filename ends  with an extension
 for the gzip, zip or bzip2 compression  formats (respectively, .gz,
 .zip, or .bz2), then the methods  SBMLReader.readSBML() and
 SBMLWriter.writeSBML()  will automatically decompress and compress
 the file while writing and  reading it.  If the filename has no such
 extension, it  will be read and written uncompressed as normal.

 The compression feature requires that the zlib (for gzip and zip
 formats) and/or bzip2 (for bzip2 format) be available on the  system
 running libSBML, and that libSBML was configured with their  support
 compiled-in.  Please see the libSBML installation instructions  for
 more information about this.  The methods  hasZlib() and  hasBzip2()
 can be used by an application to query at run-time whether support
 for the compression libraries is available in the present copy of
 libSBML.

 Support for compression is not mandated by the SBML standard, but
 applications may find it helpful, particularly when large SBML models
 are being communicated across data links of limited bandwidth.
";

%feature("docstring") SBMLReader::SBMLReader "
 Creates a new SBMLReader and returns it.

 The libSBML SBMLReader objects offer methods for reading SBML in  XML
 form from files and text strings.
";
