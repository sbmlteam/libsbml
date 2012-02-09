%feature("docstring") SBMLWriter::writeSBMLToString "
 Writes the given SBML document to an in-memory string and returns a
 pointer to it.

 @param d the SBML document to be written

 @return the string on success and 0 if one of the underlying parser
 components fail.
 
 @see setProgramVersion()
 @see setProgramName()
   
";
