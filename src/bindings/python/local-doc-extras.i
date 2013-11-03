%feature("docstring") Model::renameIDs "
";


%feature("docstring") SBasePlugin::getListOfAllElements "
Returns an SBaseList of all child SBase objects, including those
nested to an arbitrary depth.

@return an SBaseList
";



%feature("docstring") SBase::getListOfAllElements "
Returns an SBaseList of all child SBase objects, including those
nested to an arbitrary depth.

@return an SBaseList
";


%feature("docstring") SBase::getListOfAllElementsFromPlugins "
Returns an SBaseList of all child SBase objects contained in SBML package
plugins.

This method walks down the list of all packages used by the model and
returns all objects contained in them.

@return an SBaseList of all children objects from plugins.
";


%feature("docstring") ASTNode::getListOfNodes "
Returns an ASTNodeList of all ASTNode objects.

Unlike the equivalent method in the libSBML C/C++ interface, this method does
not offer the ability to pass a predicate as an argument.  The method always
returns the list of all ASTNode objects.

@return the ASTNodeList of nodes for which the predicate returned @c true
(non-zero).

@warning The list returned is owned by the caller and should be deleted after
the caller is done using it.  The ASTNode objects in the list; however, are
<strong>not</strong> owned by the caller (as they still belong to the tree
itself), and therefore should not be deleted.
";


%feature("docstring") RDFAnnotationParser::parseRDFAnnotation "
Parses an annotation (given as an XMLNode tree) into a list of
CVTerm objects.

The purpose of this method is to take an annotation that has been read into
an SBML model, identify the RDF elements within it, and create a list of
corresponding CVTerm (controlled vocabulary term) objects.

@param annotation XMLNode containing the annotation.
@param CVTerms a CVTermList of CVTerm objects to be created.

@note Because this is a @em static method, the non-C++ language interfaces
for libSBML will contain two variants.  One will be a static method on the
class (i.e., RDFAnnotationParser), and the other will be a standalone
top-level function with the name
RDFAnnotationParser_parseRDFAnnotation(). They are functionally identical.

@see RDFAnnotationParser::parseRDFAnnotation(const XMLNode *annotation)
";
