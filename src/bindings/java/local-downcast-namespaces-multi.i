/**
 * casting to most specific SBMLNamespaces object
 */

#ifdef USE_MULTI
%pragma(java) modulecode =
%{
  if (ns.hasURI(MultiExtension.getXmlnsL3V1V1()))
  {
    return new MultiPkgNamespaces(cPtr, owner);
  }
%}
#endif /* USE_MULTI */

