%feature("docstring") ASTNode::isNumber "
 Predicate returning @c True (non-zero) if this node contains a number,
 @c False (zero) otherwise.  This is functionally equivalent to the
 following code:
 @code
   isInteger() || isReal()
 @endcode
 
 @return @c True if this ASTNode is a number, @c False otherwise.
   
";
