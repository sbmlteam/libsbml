/**
 * casting to most specific ASTBasePlugin object
 */

#ifdef USE_DISTRIB
%pragma(csharp) modulecode =
%{
  if (pkgName == "distrib")
  {
    return new DistribASTPlugin(cPtr, owner);
  }
%}
#endif /* USE_DISTRIB */

