#ifdef USE_DYN
if (pkgName == "dyn")
{
  if (sb->getTypeCode() == SBML_COMPARTMENT)
  {
    return SWIGTYPE_p_DynCompartmentPlugin;
  }
  else if (sb->getTypeCode() == SBML_EVENT)
  {
    return SWIGTYPE_p_DynEventPlugin;
  }
  else if (sb->getTypeCode() == SBML_GENERIC_SBASE)
  {
    return SWIGTYPE_p_DynSBasePlugin;
  }
}

#endif // USE_DYN 

