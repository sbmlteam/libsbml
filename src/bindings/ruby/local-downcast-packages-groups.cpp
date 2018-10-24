
#ifdef USE_GROUPS
else if (pkgName == "groups")
{
  switch ( sb->getTypeCode() )
  {
    case SBML_LIST_OF:
      name = sb->getElementName();
      if (name == "listOfGroups")
      {
        return SWIGTYPE_p_ListOfGroups;
      }
      else if (name == "listOfMembers")
      {
        return SWIGTYPE_p_ListOfMembers;
      }

      return SWIGTYPE_p_ListOf;

    case SBML_GROUPS_GROUP:
      return SWIGTYPE_p_Group;

    case SBML_GROUPS_MEMBER:
      return SWIGTYPE_p_Member;

    default:
      return SWIGTYPE_p_SBase;
    }
}
#endif // USE_GROUPS				  

