%feature("docstring") ASTNode::fillListOfNodes "
 Performs a depth-first search of the tree rooted at this ASTNode
 object, and adds to the list @p lst the nodes where the given function
 <code>predicate(node)</code> returns @c True (non-zero).

 This method is identical to getListOfNodes(ASTNodePredicate predicate) const, 
 except that instead of creating a new List object, it uses the one passed
 in as argument @p lst. 

 For portability between different programming languages, the predicate
 is passed in as a pointer to a function.  The function definition must
 have the type @link ASTNode.h::ASTNodePredicate ASTNodePredicate
 @endlink, which is defined as
 @code
 int (*ASTNodePredicate) (const ASTNode_t *node);
 @endcode
 where a return value of non-zero represents @c True and zero
 represents @c False.

 @param predicate the predicate to use.

 @param lst the List to which ASTNode objects should be added.

 @see getListOfNodes() const
   
";
