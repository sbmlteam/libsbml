
#ifdef USE_L3V2EXTENDEDMATH
else if (pkgName == "l3v2extendedmath")
{
	switch (sb->getTypeCode())
	{
		case SBML_LIST_OF:
			return SWIGTYPE_p_ListOf;				  
						
		default:
			return SWIGTYPE_p_SBase;
				
	}
}
#endif // USE_COMP				  

