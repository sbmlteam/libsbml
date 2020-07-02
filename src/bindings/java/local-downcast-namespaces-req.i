/**
 * casting to most specific SBMLNamespaces object
 */


#ifdef USE_REQUIREDELEMENTS
%pragma(java) modulecode =
%{
  if (ns.hasURI(ReqExtension.getXmlnsL3V1V1()))
  {
    return new ReqPkgNamespaces(cPtr, owner);
  }
%}
#endif /* USE_REQUIREDELEMENTS */

