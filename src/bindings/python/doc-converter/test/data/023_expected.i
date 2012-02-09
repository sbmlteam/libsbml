%feature("docstring") SBMLWriter::setProgramName "
 Sets the name of this program, i.e., the program that is about to
 write out the SBMLDocument.

 If the program name and version are set (see
 SBMLWriter.setProgramVersion()), the  following XML comment, intended
 for human consumption, will be written  at the beginning of the XML
 document:

     <!-- Created by <program name> version <program version>
     on yyyy-MM-dd HH:mm with libSBML version <libsbml version>. -->

  If the program name and version are not set at some point before
 calling the writeSBML() methods, no such comment is written out.

 Parameter 'name' is the name of this program (where 'this program'
 refers to  program in which libSBML is embedded, not libSBML itself!)

 Returns integer value indicating success/failure of the  function.
 The possible values  returned by this function are:

   * libsbml.LIBSBML_OPERATION_SUCCESS

 See also setProgramVersion().
";


%feature("docstring") SBMLWriter::setProgramVersion "
 Sets the version of this program, i.e., the program that is about to
 write out the SBMLDocument.

 If the program version and name are set (see
 SBMLWriter.setProgramName()), the  following XML comment, intended
 for human consumption, will be written  at the beginning of the
 document:

     <!-- Created by <program name> version <program version>
     on yyyy-MM-dd HH:mm with libSBML version <libsbml version>. -->

  If the program version and name are not set at some point before
 calling the writeSBML() methods, no such comment is written out.

 Parameter 'version' is the version of this program (where 'this
 program'  refers to program in which libSBML is embedded, not libSBML
 itself!)

 Returns integer value indicating success/failure of the  function.
 The possible values  returned by this function are:

   * libsbml.LIBSBML_OPERATION_SUCCESS

 See also setProgramName().
";
