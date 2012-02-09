%feature("docstring") SBMLWriter::writeSBML "
 This method has multiple variants that differ in the arguments
 they accept.  Each is described separately below.

 <hr>
 Method variant with the following signature:
 <pre class='signature'>writeSBML(SBMLDocument d, string filename)</pre>

 Writes the given SBML document to filename.

 @htmlinclude assuming-compressed-file.html

 @param d the SBML document to be written

 @param filename the name or full pathname of the file where the SBML
 is to be written. 

 @return @c True on success and @c False if the filename could not be
 opened for writing.

 @note @htmlinclude note-writing-zipped-files.html
 
 @see setProgramVersion()
 @see setProgramName()
   

 <hr>
 Method variant with the following signature:
 <pre class='signature'>writeSBML(SBMLDocument d, std::ostream& stream)</pre>

 Writes the given SBML document to the output stream.

 @param d the SBML document to be written

 @param stream the stream object where the SBML is to be written.

 @return @c True on success and @c False if one of the underlying
 parser components fail (rare).
 
 @see setProgramVersion()
 @see setProgramName()
   
";


%feature("docstring") SBMLWriter::writeToString "
  @internal
 Writes the given SBML document to an in-memory string and returns a
 pointer to it.

 The string is owned by the caller and should be freed (with @c free())
 when no longer needed.

 @param d the SBML document to be written

 @return the string on success and @c 0 if one of the underlying parser
 components fail.
 
 @see setProgramVersion()
 @see setProgramName()
   
";


%feature("docstring") SBMLWriter::writeSBMLToFile "
 Writes the given SBML document to filename.

 @htmlinclude assuming-compressed-file.html

 @param d the SBML document to be written

 @param filename the name or full pathname of the file where the SBML
 is to be written. 

 @return @c True on success and @c False if the filename could not be
 opened for writing.

 @note @htmlinclude note-writing-zipped-files.html
 
 @see setProgramVersion()
 @see setProgramName()
   
";


%feature("docstring") SBMLWriter::writeSBMLToString "
 Writes the given SBML document to an in-memory string and returns a
 pointer to it.

 The string is owned by the caller and should be freed (with @c free())
 when no longer needed.

 @param d the SBML document to be written

 @return the string on success and @c 0 if one of the underlying parser
 components fail.
 
 @see setProgramVersion()
 @see setProgramName()
   
";


%feature("docstring") SBMLWriter::hasZlib "
 Predicate returning @c True if this copy of libSBML has been linked
 with the <em>zlib</em> library.

 LibSBML supports reading and writing files compressed with either
 bzip2 or zip/gzip compression.  The facility depends on libSBML having
 been compiled with the necessary support libraries.  This method
 allows a calling program to inquire whether that is the case for the
 copy of libSBML it is using.

 @if notclike @note Because this is a @em static method, the
 non-C++ language interfaces for libSBML will contain two variants.  One
 will be a static method on the class (i.e., SBMLWriter), and the other
 will be a standalone top-level function with the name
 SBMLWriter_hasZlib(). They are functionally identical. @endif

 @return @c True if libSBML is linked with zlib, @c False otherwise.

 @see @if clike hasBzip2() @else SBMLWriter.hasBzip2() @endif
   
";


%feature("docstring") SBMLWriter::hasBzip2 "
 Predicate returning @c True if this copy of libSBML has been linked
 with the <em>bzip2</em> library.

 LibSBML supports reading and writing files compressed with either
 bzip2 or zip/gzip compression.  The facility depends on libSBML having
 been compiled with the necessary support libraries.  This method
 allows a calling program to inquire whether that is the case for the
 copy of libSBML it is using.

 @if notclike @note Because this is a @em static method, the
 non-C++ language interfaces for libSBML will contain two variants.  One
 will be a static method on the class (i.e., SBMLWriter), and the other
 will be a standalone top-level function with the name
 SBMLWriter_hasZlib(). They are functionally identical. @endif

 @return @c True if libSBML is linked with bzip2, @c False otherwise.

 @see @if clike hasZlib() @else SBMLWriter.hasZlib() @endif
   
";
