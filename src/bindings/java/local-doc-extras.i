%javamethodmodifiers getListOfAllElements "
  /**
   * Returns an {@link SBaseList} of all child {@link SBase} objects,
   * including those nested to an arbitrary depth.
   *
   * @return a pointer to an {@link SBaseList} of pointers to all children objects.
   */
 public";


%javamethodmodifiers getListOfAllElementsFromPlugins "
  /**
   * Returns an {@link SBaseList} of all child {@link SBase} objects
   * contained in SBML package plugins.
   *
   * This method walks down the list of all packages used by the model and
   * returns all objects contained in them.
   *
   * @return a pointer to a {@link SBaseList} of pointers to all children
   * objects from plugins.
   */
 public";


%javamethodmodifiers RDFAnnotationParser::parseRDFAnnotation(const XMLNode *annotation, ListWrapper<CVTerm> *CVTerms) "
  /**
   * Parses an annotation (given as an {@link XMLNode} tree) into a
   * list of {@link CVTerm} objects.
   * <p>
   * This is used to take an annotation that has been read into an SBML
   * model, identify the RDF elements within it, and create a list of
   * corresponding {@link CVTerm} (controlled vocabulary term) objects.
   * <p>
   * @param annotation {@link XMLNode} containing the annotation.
   * <p>
   * @param CVTerms a {@link CVTermList} of {@link CVTerm} objects to be created.
   * <p>
   * @see #parseRDFAnnotation(XMLNode annotation)
   */
 public";


%javamethodmodifiers ASTNode::getListOfNodes "
  /**
   * Performs a depth-first search of the tree rooted at this {@link ASTNode}
   * object, and returns an {@link ASTNodeList} of all nodes.
   *
   * @return the list of {@link ASTNode} objects.
   */
 public";
