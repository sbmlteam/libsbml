/**
 * casting to most specific SBMLNamespaces object
 */


#ifdef USE_REQUIREDELEMENTS
%pragma(csharp) modulecode =
%{
  if (ns.hasURI(ReqExtension.getXmlnsL3V1V1()))
  {
    return new ReqPkgNamespaces(cPtr, owner);
  }
%}
#endif /* USE_REQUIREDELEMENTS */
