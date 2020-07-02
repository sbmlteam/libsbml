#ifdef USE_DYN
else if (pkgName == "dyn")
{
  switch (sb->getTypeCode() )
  {
    case SBML_LIST_OF:
      name = sb->getElementName();
      if (name == "listOfDynElements")
      {
        return SWIGTYPE_p_ListOfDynElements;
      }
      else if (name == "listOfSpatialComponents")
      {
        return SWIGTYPE_p_ListOfSpatialComponents;
      }

      return SWIGTYPE_p_ListOf;

    case SBML_DYN_ELEMENT:
      return SWIGTYPE_p_DynElement;

    case SBML_DYN_SPATIALCOMPONENT:
      return SWIGTYPE_p_SpatialComponent;

    default:
      return SWIGTYPE_p_SBase;
  }
}

#endif // USE_DYN 

