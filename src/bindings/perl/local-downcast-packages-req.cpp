#ifdef USE_REQUIREDELEMENTS
else if (pkgName == "req")
{
  switch (sb->getTypeCode() )
  {
    case SBML_LIST_OF:
      name = sb->getElementName();
      if (name == "listOfChangedMaths")
      {
        return SWIGTYPE_p_ListOfChangedMaths;
      }

      return SWIGTYPE_p_ListOf;

    case SBML_REQ_CHANGED_MATH:
      return SWIGTYPE_p_ChangedMath;

    default:
      return SWIGTYPE_p_SBase;
  }
}

#endif // USE_REQUIREDELEMENTS 

