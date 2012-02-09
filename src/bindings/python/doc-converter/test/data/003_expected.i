%feature("docstring") SBMLWriter::writeSBMLToString "
 Writes the given SBML document to an in-memory string and returns a
 pointer to it.

 Parameter 'd' is the SBML document to be written

 Returns the string on success and 0 if one of the underlying parser
 components fail.

 See also setProgramVersion(), setProgramName().
";
