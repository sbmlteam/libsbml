
#ifdef USE_QUAL
if (pkgName == "qual")
{
	if (sb->getTypeCode() == SBML_MODEL || dynamic_cast<Model*>(sb) != NULL)
	{
		return SWIGTYPE_p_QualModelPlugin;
	}
}
#endif // USE_QUAL

