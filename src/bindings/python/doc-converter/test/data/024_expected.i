%feature("docstring") SBMLWriter::writeSBML "
 This method has multiple variants that differ in the arguments  they
 accept.  Each is described separately below.

 ______________________________________________________________________
 Method variant with the following signature:

    writeSBML(SBMLDocument d, string filename)

 Writes the given SBML document to filename.

 If the given filename ends with the suffix \".gz\" (for example,
 \"myfile.xml.gz\"),  libSBML assumes the caller wants the file to be
 written compressed in  gzip format. Similarly, if the given filename
 ends with \".zip\" or \".bz2\",  libSBML assumes the caller wants the
 file to be compressed in zip or  bzip2 format (respectively). Files
 whose names lack these suffixes will  be written uncompressed.
 Special considerations for the zip format: If  the given filename
 ends with \".zip\", the file placed in the zip archive  will have the
 suffix \".xml\" or \".sbml\". For example, the file in the zip
 archive will be named \"test.xml\" if the given filename is
 \"test.xml.zip\"  or \"test.zip\". Similarly, the filename in the
 archive will be \"test.sbml\"  if the given filename is
 \"test.sbml.zip\".

 Parameter 'd' is the SBML document to be written

 Parameter 'filename' is the name or full pathname of the file where
 the SBML  is to be written.

 Returns True on success and False if the filename could not be
 opened for writing.

 Note:

 To write a gzip/zip file, libSBML needs to be configured and linked
 with  the zlib library at compile time. It also needs to be linked
 with the  bzip2 library to write files in bzip2 format. (Both of
 these are the  default configurations for libSBML.) Errors about
 unreadable files will  be logged and this method will return false if
 a compressed filename is  given and libSBML was not linked with the
 corresponding required  library.

 See also setProgramVersion(), setProgramName(), setProgramVersion(),
 setProgramName().
";


%feature("docstring") SBMLWriter::writeToString "
 Internal implementation method.
";


%feature("docstring") SBMLWriter::writeSBMLToFile "
 Writes the given SBML document to filename.

 If the given filename ends with the suffix \".gz\" (for example,
 \"myfile.xml.gz\"),  libSBML assumes the caller wants the file to be
 written compressed in  gzip format. Similarly, if the given filename
 ends with \".zip\" or \".bz2\",  libSBML assumes the caller wants the
 file to be compressed in zip or  bzip2 format (respectively). Files
 whose names lack these suffixes will  be written uncompressed.
 Special considerations for the zip format: If  the given filename
 ends with \".zip\", the file placed in the zip archive  will have the
 suffix \".xml\" or \".sbml\". For example, the file in the zip
 archive will be named \"test.xml\" if the given filename is
 \"test.xml.zip\"  or \"test.zip\". Similarly, the filename in the
 archive will be \"test.sbml\"  if the given filename is
 \"test.sbml.zip\".

 Parameter 'd' is the SBML document to be written

 Parameter 'filename' is the name or full pathname of the file where
 the SBML  is to be written.

 Returns True on success and False if the filename could not be
 opened for writing.

 Note:

 To write a gzip/zip file, libSBML needs to be configured and linked
 with  the zlib library at compile time. It also needs to be linked
 with the  bzip2 library to write files in bzip2 format. (Both of
 these are the  default configurations for libSBML.) Errors about
 unreadable files will  be logged and this method will return false if
 a compressed filename is  given and libSBML was not linked with the
 corresponding required  library.

 See also setProgramVersion(), setProgramName().
";


%feature("docstring") SBMLWriter::writeSBMLToString "
 Writes the given SBML document to an in-memory string and returns a
 pointer to it.

 The string is owned by the caller and should be freed (with free())
 when no longer needed.

 Parameter 'd' is the SBML document to be written

 Returns the string on success and 0 if one of the underlying parser
 components fail.

 See also setProgramVersion(), setProgramName().
";


%feature("docstring") SBMLWriter::hasZlib "
 Predicate returning True if this copy of libSBML has been linked
 with the zlib library.

 LibSBML supports reading and writing files compressed with either
 bzip2 or zip/gzip compression.  The facility depends on libSBML
 having  been compiled with the necessary support libraries.  This
 method  allows a calling program to inquire whether that is the case
 for the  copy of libSBML it is using.

 Note:

 Because this is a static method, the  non-C++ language interfaces for
 libSBML will contain two variants.  One  will be a static method on
 the class (i.e., SBMLWriter), and the other  will be a standalone
 top-level function with the name  SBMLWriter_hasZlib(). They are
 functionally identical.

 Returns True if libSBML is linked with zlib, False otherwise.

 See also SBMLWriter.hasBzip2().
";


%feature("docstring") SBMLWriter::hasBzip2 "
 Predicate returning True if this copy of libSBML has been linked
 with the bzip2 library.

 LibSBML supports reading and writing files compressed with either
 bzip2 or zip/gzip compression.  The facility depends on libSBML
 having  been compiled with the necessary support libraries.  This
 method  allows a calling program to inquire whether that is the case
 for the  copy of libSBML it is using.

 Note:

 Because this is a static method, the  non-C++ language interfaces for
 libSBML will contain two variants.  One  will be a static method on
 the class (i.e., SBMLWriter), and the other  will be a standalone
 top-level function with the name  SBMLWriter_hasZlib(). They are
 functionally identical.

 Returns True if libSBML is linked with bzip2, False otherwise.

 See also SBMLWriter.hasZlib().
";
