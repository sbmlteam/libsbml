/**
 * casting to most specific ASTBasePlugin object
 */

#ifdef USE_DISTRIB
%pragma(java) modulecode =
%{
  if (pkgName.equals("distrib"))
  {
    return new DistribASTPlugin(cPtr, owner);
  }
%}
#endif /* USE_DISTRIB */

