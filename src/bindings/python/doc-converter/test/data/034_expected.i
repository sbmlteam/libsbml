%feature("docstring") ASTNode::isNumber "
 Predicate returning True (non-zero) if this node contains a number,
 False (zero) otherwise.  This is functionally equivalent to the
 following code:

     isInteger() || isReal()

  Returns True if this ASTNode is a number, False otherwise.
";
