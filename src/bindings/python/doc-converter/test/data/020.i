%feature("docstring") readSBMLFromString "
 Reads an SBML document from a string assumed to be in XML format.

 If the string does not begin with XML declaration,
  @verbatim
  <?xml version=\'1.0\' encoding=\'UTF-8\'?>
  @endverbatim

 an XML declaration string will be prepended.

 This method will report an error if the given string @p xml is not SBML.
 The error will be logged in the error log of the SBMLDocument object
 returned by this method.  Calling programs can inspect this error log to
 determine the nature of the problem.  Please refer to the definition of
 SBMLDocument for more information about the error reporting mechanism.

 @return a pointer to the SBMLDocument read.
 
";


%feature("docstring") OFStream::is_open "
 Returns <code>true</code> if this stream object is currently
 associated with a file.
 <p>
 @return <code>true</code> if the stream object is currently
 associated with a file, <code>false</code> otherwise
     
";


%feature("docstring") OStringStream::OStringStream "
 Creates a new OStringStream object
     
";


%feature("docstring") OStringStream::str "
 This method has multiple variants that differ in the arguments
 they accept.  Each is described separately below.

 <hr>
 Method variant with the following signature:
 <pre class='signature'>str()</pre>

 Returns the copy of the string object currently assosiated 
 with this <code>ostringstream</code> buffer.
 <p>
 @return a copy of the string object for this stream
     

 <hr>
 Method variant with the following signature:
 <pre class='signature'>str(string s)</pre>

 Sets string @p s to the string object currently assosiated with
 this stream buffer.
 <p>
 @param s the string to write to this stream
     
";


