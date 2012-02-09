%feature("docstring") SBMLReader::readSBMLFromString "
 Reads an SBML document from the given XML string.

 This method is flexible with respect to the presence of an XML
 declaration at the beginning of the string.  In particular, if the
 string in 'xml' does not begin with the XML declaration  <?xml
 version='1.0' encoding='UTF-8'?>, then this  method will
 automatically prepend the declaration to 'xml'.

 This method will log a fatal error if the content given in the
 parameter 'xml' is not SBML.  See the method documentation for
 SBMLReader.readSBML()  for an example of code for testing the
 returned error code.

 Parameter 'xml' is a string containing a full SBML model

 Returns a pointer to the SBMLDocument created from the SBML content.

 See also SBMLReader.readSBML().
";
