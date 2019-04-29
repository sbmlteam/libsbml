/**
 * casting to most specific SBMLExtension object
 */
 

#ifdef USE_RENDER
%pragma(java) modulecode =
%{
    if (pkgName.equals("render"))
      return new RenderExtension(cPtr, owner);
%}
#endif // USE_RENDER

