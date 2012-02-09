%feature("docstring") getLibSBMLVersion "
 Returns the version number of this copy of libSBML as an integer.

 Returns the libSBML version as an integer; version 1.2.3 becomes
 10203.
";


%feature("docstring") getLibSBMLDottedVersion "
 Returns the version number of this copy of libSBML as a string.

 Returns the libSBML version as a string; version 1.2.3 becomes
 '1.2.3'.

 See also getLibSBMLVersionString().
";


%feature("docstring") getLibSBMLVersionString "
 Returns the version number of this copy of libSBML as a string
 without  periods.

 Returns the libSBML version as a string: version 1.2.3 becomes
 '10203'.

 See also getLibSBMLDottedVersion().
";


