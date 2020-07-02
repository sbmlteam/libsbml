/**
 * casting to most specific SBMLExtension object
 */

#ifdef USE_REQUIREDELEMENTS
%pragma(java) modulecode =
%{
  if (pkgName.equals("req"))
  {
    return new ReqExtension(cPtr, owner);
  }
%}
#endif // USE_REQUIREDELEMENTS

