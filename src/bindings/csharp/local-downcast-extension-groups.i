/**
 * casting to most specific SBMLExtension object
 */
 
#ifdef USE_GROUPS
%pragma(csharp) modulecode =
%{
		if (pkgName == "groups")
			return new GroupsExtension(cPtr, owner);
%}
#endif /* USE_GROUPS*/
