/**
 * casting to most specific SBMLExtension object
 */
 
#ifdef USE_RENDER
%pragma(csharp) modulecode =
%{
		if (pkgName == "render")
			return new RenderExtension(cPtr, owner);
%}
#endif /* USE_RENDER*/
