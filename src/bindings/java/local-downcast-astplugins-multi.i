/**
 * casting to most specific ASTBasePlugin object
 */

#ifdef USE_MULTI
%pragma(java) modulecode =
%{
  if (pkgName.equals("multi"))
  {
    return new MultiASTPlugin(cPtr, owner);
  }
%}
#endif /* USE_MULTI */

