/**
 * casting to most specific SBMLExtension object
 */

#ifdef USE_MULTI
%pragma(java) modulecode =
%{
  if (pkgName.equals("multi"))
  {
    return new MultiExtension(cPtr, owner);
  }
%}
#endif // USE_MULTI

