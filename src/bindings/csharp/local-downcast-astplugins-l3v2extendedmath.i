/**
 * casting to most specific ASTBasePlugin object
 */

#ifdef USE_L3V2EXTENDEDMATH
%pragma(csharp) modulecode =
%{
  if (pkgName == "l3v2extendedmath")
  {
    return new L3v2extendedmathASTPlugin(cPtr, owner);
  }
%}
#endif /* USE_L3V2EXTENDEDMATH */

