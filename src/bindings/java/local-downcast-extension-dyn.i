/**
 * casting to most specific SBMLExtension object
 */

#ifdef USE_DYN
%pragma(java) modulecode =
%{
  if (pkgName.equals("dyn"))
  {
    return new DynExtension(cPtr, owner);
  }
%}
#endif // USE_DYN

