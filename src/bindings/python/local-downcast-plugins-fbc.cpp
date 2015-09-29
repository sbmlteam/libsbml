#ifdef USE_FBC
if (pkgName == "fbc")
{
  if (sb->getTypeCode() == SBML_SPECIES)
  {
    return SWIGTYPE_p_FbcSpeciesPlugin;
  }
  else if (sb->getTypeCode() == SBML_MODEL)
  {
    return SWIGTYPE_p_FbcModelPlugin;
  }
  else if (sb->getTypeCode() == SBML_REACTION)
  {
    return SWIGTYPE_p_FbcReactionPlugin;
  }
}

#endif // USE_FBC 

