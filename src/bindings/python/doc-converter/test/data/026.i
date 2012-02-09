%feature("docstring") SBase::appendNotes "
 This method has multiple variants that differ in the arguments
 they accept.  Each is described separately below.

 <hr>
 Method variant with the following signature:
 <pre class='signature'>appendNotes(string notes)</pre>

 Appends the given @p notes to the \'notes\' subelement of this object.

 The content of the parameter @p notes is copied.

 The optional SBML element named \'notes\', present on every major SBML
 component type, is intended as a place for storing optional
 information intended to be seen by humans.  An example use of the
 \'notes\' element would be to contain formatted user comments about the
 model element in which the \'notes\' element is enclosed.  Every object
 derived directly or indirectly from type SBase can have a separate
 value for \'notes\', allowing users considerable freedom when adding
 comments to their models.

 The format of \'notes\' elements must be <a target=\'_blank\'
 href=\'http://www.w3.org/TR/xhtml1/\'>XHTML&nbsp;1.0</a>.  To help
 verify the formatting of \'notes\' content, libSBML provides the static
 utility method SyntaxChecker.hasExpectedXHTMLSyntax(); however,
 readers are urged to consult the appropriate <a target=\'_blank\'
 href=\'http://sbml.org/Documents/Specifications\'>SBML specification
 document</a> for the Level and Version of their model for more
 in-depth explanations.  The SBML Level&nbsp;2 and &nbsp;3
 specifications have considerable detail about how \'notes\' element
 content must be structured.

 @param notes an XML string that is to appended to the content of
 the \'notes\' subelement of this object

 @return integer value indicating success/failure of the
 function.  The possible values returned by this function are:
 @li @link libsbml.LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
 @li @link libsbml.LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
 @li @link libsbml.LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink

 @see getNotesString()
 @see isSetNotes()
 @see setNotes()
 @see setNotes()
 @see appendNotes()
 @see unsetNotes()
 @see SyntaxChecker.hasExpectedXHTMLSyntax()
   

 <hr>
 Method variant with the following signature:
 <pre class='signature'>appendNotes(XMLNode notes)</pre>

 Appends the given @p notes to the \'notes\' subelement of this object.

 The content of @p notes is copied.

 The optional SBML element named \'notes\', present on every major SBML
 component type, is intended as a place for storing optional
 information intended to be seen by humans.  An example use of the
 \'notes\' element would be to contain formatted user comments about the
 model element in which the \'notes\' element is enclosed.  Every object
 derived directly or indirectly from type SBase can have a separate
 value for \'notes\', allowing users considerable freedom when adding
 comments to their models.

 The format of \'notes\' elements must be <a target=\'_blank\'
 href=\'http://www.w3.org/TR/xhtml1/\'>XHTML&nbsp;1.0</a>.  To help
 verify the formatting of \'notes\' content, libSBML provides the static
 utility method SyntaxChecker.hasExpectedXHTMLSyntax(); however,
 readers are urged to consult the appropriate <a target=\'_blank\'
 href=\'http://sbml.org/Documents/Specifications\'>SBML specification
 document</a> for the Level and Version of their model for more
 in-depth explanations.  The SBML Level&nbsp;2 and &nbsp;3
 specifications have considerable detail about how \'notes\' element
 content must be structured.
 
 @param notes an XML node structure that is to appended to the content
 of the \'notes\' subelement of this object

 @return integer value indicating success/failure of the
 function.  The possible values returned by this function are:
 @li @link libsbml.LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
 @li @link libsbml.LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
 @li @link libsbml.LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink

 @see getNotesString()
 @see isSetNotes()
 @see setNotes()
 @see setNotes()
 @see appendNotes()
 @see unsetNotes()
 @see SyntaxChecker.hasExpectedXHTMLSyntax()
   
";


%feature("docstring") SBase::setModelHistory "
 Sets the ModelHistory of this object.

 The content of @p history is copied, and this object\'s existing model
 history content is deleted.

 @param history ModelHistory of this object.

 @return integer value indicating success/failure of the
 function.  The possible values returned by this function are:
 @li @link libsbml.LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
 @li @link libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
 @li @link libsbml.LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
 
 @note In SBML Level&nbsp;2, model history annotations were only
 permitted on the Model element.  In SBML Level&nbsp;3, they are
 permitted on all SBML components derived from SBase.
   
";

