/**
 * casting to most specific ASTBasePlugin object
 */

#ifdef USE_MULTI
%pragma(csharp) modulecode =
%{
  if (pkgName == "multi")
  {
    return new MultiASTPlugin(cPtr, owner);
  }
%}
#endif /* USE_MULTI */

