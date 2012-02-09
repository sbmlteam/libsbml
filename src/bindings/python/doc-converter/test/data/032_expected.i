%feature("docstring") ASTNode::fillListOfNodes "
 Performs a depth-first search of the tree rooted at this ASTNode
 object, and adds to the list 'lst' the nodes where the given function
 predicate(node) returns True (non-zero).

 This method is identical to getListOfNodes(ASTNodePredicate
 predicate) const,   except that instead of creating a new List
 object, it uses the one passed  in as argument 'lst'.

 For portability between different programming languages, the
 predicate  is passed in as a pointer to a function.  The function
 definition must  have the type ASTNode.h::ASTNodePredicate, which is
 defined as

     int (*ASTNodePredicate) (const ASTNode_t *node);
  where a return value of non-zero represents True and zero
 represents False.

 Parameter 'predicate' is the predicate to use.

 Parameter 'lst' is the List to which ASTNode objects should be added.

 See also getListOfNodes().
";
