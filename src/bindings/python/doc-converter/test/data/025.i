%feature("docstring") getLibSBMLVersion "
 Returns the version number of this copy of libSBML as an integer.

 @return the libSBML version as an integer; version 1.2.3 becomes 10203.
 
";


%feature("docstring") getLibSBMLDottedVersion "
 Returns the version number of this copy of libSBML as a string.

 @return the libSBML version as a string; version 1.2.3 becomes
 \'1.2.3\'.

 @see getLibSBMLVersionString()
 
";


%feature("docstring") getLibSBMLVersionString "
 Returns the version number of this copy of libSBML as a string without
 periods.

 @return the libSBML version as a string: version 1.2.3 becomes \'10203\'.

 @see getLibSBMLDottedVersion()
 
";


%feature("docstring") SBMLReader "
 Methods for reading SBML from files and text strings.

 @htmlinclude not-sbml-warning.html

 The SBMLReader class provides the main interface for reading SBML
 content from files and strings.  The methods for reading SBML all return
 an SBMLDocument object representing the results.

 In the case of failures (such as if the SBML contains errors or a file
 cannot be read), the errors will be recorded with the SBMLErrorLog
 object kept in the SBMLDocument returned by SBMLReader.  Consequently,
 immediately after calling a method on SBMLReader, callers should always
 check for errors and warnings using the methods for this purpose
 provided by SBMLDocument.

 For convenience as well as easy access from other languages besides C++,
 this file also defines two global functions,
 libsbml.readSBML()
 and libsbml.readSBMLFromString().
 They are equivalent to creating an SBMLReader
 object and then calling the
 SBMLReader.readSBML() or
 SBMLReader.readSBMLFromString()
 methods, respectively.

 @section compression Support for reading compressed files

 LibSBML provides support for reading (as well as writing) compressed
 SBML files.  The process is transparent to the calling
 application&mdash;the application does not need to do anything
 deliberate to invoke the functionality.  If a given SBML filename ends
 with an extension for the @em gzip, @em zip or @em bzip2 compression
 formats (respectively, @c .gz, @c .zip, or @c .bz2), then the methods
 SBMLReader.readSBML() and
 SBMLWriter.writeSBML()
 will automatically decompress and compress the file while writing and
 reading it.  If the filename has no such extension, it
 will be read and written uncompressed as normal.

 The compression feature requires that the @em zlib (for @em gzip and @em
 zip formats) and/or @em bzip2 (for @em bzip2 format) be available on the
 system running libSBML, and that libSBML was configured with their
 support compiled-in.  Please see the libSBML @if clike <a href=\'libsbml-installation.html\'>installation instructions</a> @endif@if python <a href=\'libsbml-installation.html\'>installation instructions</a> @endif@if java  <a href=\'../../../libsbml-installation.html\'>installation instructions</a> @endif for more information about this.  The methods
 @if java SBMLReader.hasZlib()@else hasZlib()@endif and
 @if java SBMLReader.hasBzip2()@else hasBzip2()@endif
 can be used by an application to query at run-time whether support
 for the compression libraries is available in the present copy of
 libSBML.

 Support for compression is not mandated by the SBML standard, but
 applications may find it helpful, particularly when large SBML models
 are being communicated across data links of limited bandwidth.
";

%feature("docstring") SBMLReader::SBMLReader "
 Creates a new SBMLReader and returns it. 

 The libSBML SBMLReader objects offer methods for reading SBML in
 XML form from files and text strings.
   
";
