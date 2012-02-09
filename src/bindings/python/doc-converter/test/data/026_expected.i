%feature("docstring") SBase::appendNotes "
 This method has multiple variants that differ in the arguments  they
 accept.  Each is described separately below.

 ______________________________________________________________________
 Method variant with the following signature:

    appendNotes(string notes)

 Appends the given 'notes' to the 'notes' subelement of this object.

 The content of the parameter 'notes' is copied.

 The optional SBML element named 'notes', present on every major SBML
 component type, is intended as a place for storing optional
 information intended to be seen by humans.  An example use of the
 'notes' element would be to contain formatted user comments about the
 model element in which the 'notes' element is enclosed.  Every object
 derived directly or indirectly from type SBase can have a separate
 value for 'notes', allowing users considerable freedom when adding
 comments to their models.

 The format of 'notes' elements must be XHTML 1.0.  To help  verify
 the formatting of 'notes' content, libSBML provides the static
 utility method SyntaxChecker.hasExpectedXHTMLSyntax(); however,
 readers are urged to consult the appropriate SBML specification
 document for the Level and Version of their model for more  in-depth
 explanations.  The SBML Level 2 and  3  specifications have
 considerable detail about how 'notes' element  content must be
 structured.

 Parameter 'notes' is an XML string that is to appended to the content
 of  the 'notes' subelement of this object

 Returns integer value indicating success/failure of the  function.
 The possible values returned by this function are:

   * libsbml.LIBSBML_OPERATION_SUCCESS

   * libsbml.LIBSBML_INVALID_OBJECT

   * libsbml.LIBSBML_OPERATION_FAILED

 See also getNotesString(), isSetNotes(), setNotes(), setNotes(),
 appendNotes(), unsetNotes(), SyntaxChecker.hasExpectedXHTMLSyntax(),
 getNotesString(), isSetNotes(), setNotes(), setNotes(),
 appendNotes(), unsetNotes(), SyntaxChecker.hasExpectedXHTMLSyntax().
";


%feature("docstring") SBase::setModelHistory "
 Sets the ModelHistory of this object.

 The content of 'history' is copied, and this object's existing model
 history content is deleted.

 Parameter 'history' is ModelHistory of this object.

 Returns integer value indicating success/failure of the  function.
 The possible values returned by this function are:

   * libsbml.LIBSBML_OPERATION_SUCCESS

   * libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE

   * libsbml.LIBSBML_INVALID_OBJECT

 Note:

 In SBML Level 2, model history annotations were only  permitted on
 the Model element.  In SBML Level 3, they are  permitted on all SBML
 components derived from SBase.
";

