%feature("docstring") readSBMLFromString "
 Reads an SBML document from a string assumed to be in XML format.

 If the string does not begin with XML declaration,

     <?xml version=\'1.0\' encoding=\'UTF-8\'?>

  an XML declaration string will be prepended.

 This method will report an error if the given string 'xml' is not
 SBML.  The error will be logged in the error log of the SBMLDocument
 object  returned by this method.  Calling programs can inspect this
 error log to  determine the nature of the problem.  Please refer to
 the definition of  SBMLDocument for more information about the error
 reporting mechanism.

 Returns a pointer to the SBMLDocument read.
";


%feature("docstring") OFStream::is_open "
 Returns true if this stream object is currently  associated with a
 file.

 Returns true if the stream object is currently  associated with a
 file, false otherwise
";


%feature("docstring") OStringStream::OStringStream "
 Creates a new OStringStream object
";


%feature("docstring") OStringStream::str "
 This method has multiple variants that differ in the arguments  they
 accept.  Each is described separately below.

 ______________________________________________________________________
 Method variant with the following signature:

    str()

 Returns the copy of the string object currently assosiated   with
 this ostringstream buffer.

 Returns a copy of the string object for this stream

 ______________________________________________________________________
 Method variant with the following signature:

    str(string s)

 Sets string 's' to the string object currently assosiated with  this
 stream buffer.

 Parameter 's' is the string to write to this stream
";


