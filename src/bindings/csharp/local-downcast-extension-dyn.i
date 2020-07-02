/**
 * casting to most specific SBMLExtension object
 */

#ifdef USE_DYN
%pragma(csharp) modulecode =
%{
  if (pkgName == "dyn")
  {
    return new DynExtension(cPtr, owner);
  }
%}
#endif /* USE_DYN */

