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
