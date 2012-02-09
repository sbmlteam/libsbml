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


