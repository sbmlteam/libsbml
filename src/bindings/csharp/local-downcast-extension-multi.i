/**
 * casting to most specific SBMLExtension object
 */

#ifdef USE_MULTI
%pragma(csharp) modulecode =
%{
  if (pkgName == "multi")
  {
    return new MultiExtension(cPtr, owner);
  }
%}
#endif /* USE_MULTI */

