/**
 * casting to most specific ASTBasePlugin object
 */

#ifdef USE_ARRAYS
%pragma(java) modulecode =
%{
  if (pkgName.equals("arrays"))
  {
    return new ArraysASTPlugin(cPtr, owner);
  }
%}
#endif /* USE_ARRAYS */

