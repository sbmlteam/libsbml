/**
 * casting to most specific SBMLNamespaces object
 */

#ifdef USE_DYN
%pragma(java) modulecode =
%{
  if (ns.hasURI(DynExtension.getXmlnsL3V1V1()))
  {
    return new DynPkgNamespaces(cPtr, owner);
  }
%}
#endif /* USE_DYN */

