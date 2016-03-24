
#ifdef USE_GROUPS
if (pkgName == "groups")
{
	if (sb->getTypeCode() == SBML_MODEL)
	{
		return SWIGTYPE_p_GroupsModelPlugin;
	}
}
#endif // USE_GROUPS

