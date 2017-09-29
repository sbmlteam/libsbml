#ifdef USE_FBC
if (pkgName == "fbc")
{
  if (sb->getTypeCode() == SBML_SPECIES)
  {
    return SWIGTYPE_p_FbcSpeciesPlugin;
  }
  else if (sb->getTypeCode() == SBML_MODEL || dynamic_cast<Model*>(sb) != NULL)
  {
    return SWIGTYPE_p_FbcModelPlugin;
  }
  else if (sb->getTypeCode() == SBML_REACTION)
  {
    return SWIGTYPE_p_FbcReactionPlugin;
  }
  else if(sb->getTypeCode() == SBML_DOCUMENT)
  {
    return SWIGTYPE_p_FbcSBMLDocumentPlugin;
  }
}

#endif // USE_FBC 

