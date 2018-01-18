
#ifdef USE_RENDER
if (pkgName == "render")
{
	if (sb->getTypeCode() == SBML_LIST_OF)
	{
	    std::string name = sb->getElementName();
		if(name == "listOfLayouts"){
			return SWIGTYPE_p_RenderListOfLayoutsPlugin;
		}
	}
	else if (sb->getTypeCode() == SBML_LAYOUT_LAYOUT)
	{
		return SWIGTYPE_p_RenderLayoutPlugin;
	}
	else if (sb->getTypeCode() == SBML_LAYOUT_GRAPHICALOBJECT)
	{
		return SWIGTYPE_p_RenderGraphicalObjectPlugin;
	}
}
#endif // USE_RENDER

