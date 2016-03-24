/**
 * casting to most specific SBMLNamespaces object
 */


#ifdef USE_GROUPS
%pragma(java) modulecode =
%{
	
	if (ns.hasURI(GroupsExtension.getXmlnsL3V1V1()))
	{
		return new GroupsPkgNamespaces(cPtr, owner);
	}
	%}
#endif /* USE_GROUPS*/
