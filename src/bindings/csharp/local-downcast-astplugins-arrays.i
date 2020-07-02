/**
 * casting to most specific ASTBasePlugin object
 */

#ifdef USE_ARRAYS
%pragma(csharp) modulecode =
%{
  if (pkgName == "arrays")
  {
    return new ArraysASTPlugin(cPtr, owner);
  }
%}
#endif /* USE_ARRAYS */

