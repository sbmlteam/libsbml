/**
 * casting to most specific SBMLNamespaces object
 */


#ifdef USE_RENDER
%pragma(csharp) modulecode =
%{
	
	if (ns.hasURI(RenderExtension.getXmlnsL3V1V1()) || ns.hasURI(RenderExtension.getXmlnsL2()))
	{
		return new RenderPkgNamespaces(cPtr, owner);
	}
	%}
#endif /* USE_RENDER*/
