/**
 * casting to most specific SBMLExtension object
 */
 

#ifdef USE_GROUPS
%pragma(java) modulecode =
%{
    if (pkgName.equals("groups"))
		return new GroupsExtension(cPtr, owner);
	%}
#endif // USE_GROUPS		

