%feature("docstring") SBMLReader::readSBMLFromString "
 Reads an SBML document from the given XML string.

 This method is flexible with respect to the presence of an XML
 declaration at the beginning of the string.  In particular, if the
 string in @p xml does not begin with the XML declaration
 <code>&lt;?xml version=\'1.0\' encoding=\'UTF-8\'?&gt;</code>, then this
 method will automatically prepend the declaration to @p xml.

 This method will log a fatal error if the content given in the
 parameter @p xml is not SBML.  See the method documentation for
 SBMLReader.readSBML()
 for an example of code for testing the returned error code.

 @param xml a string containing a full SBML model

 @return a pointer to the SBMLDocument created from the SBML content.

 @see SBMLReader.readSBML()
   
";
