
#ifdef USE_RENDER
else if (pkgName == "render")
{
	switch (sb->getTypeCode())
	{
		case SBML_LIST_OF:
			{
			name = sb->getElementName();
			int itemType = ((ListOf*)sb)->getItemTypeCode();
		    if(name =="listOfColorDefinitions")
			{
				return SWIGTYPE_p_ListOfColorDefinitions;
            }
		    else if(name =="listOfGlobalRenderInformation")
			{
		       return SWIGTYPE_p_ListOfGlobalRenderInformation;
            }
			else if(name =="listOfStyles")
			{
			   if (itemType == SBML_RENDER_LOCALSTYLE)
				  return SWIGTYPE_p_ListOfGlobalStyles;
				else 
		         return SWIGTYPE_p_ListOfLocalStyles;
            }
			else if(name =="listOfGradientDefinitions")
			{
		       return SWIGTYPE_p_ListOfGradientDefinitions;
            }
			else if(name =="listOfLineEndings")
			{
		       return SWIGTYPE_p_ListOfLineEndings;
            }
			else if(name =="listOfElements")
			{
		       return SWIGTYPE_p_ListOfCurveElements;
            }
			else if(name =="listOfRenderInformation")
			{
		       return SWIGTYPE_p_ListOfLocalRenderInformation;
            }
			else if(name =="listOfDrawables")
			{
		       return SWIGTYPE_p_ListOfDrawables;
            }
			else if(name =="listOfGradientStops")
			{
		       return SWIGTYPE_p_ListOfGradientStops;
            }	
			return SWIGTYPE_p_ListOf;				  
			}
		case SBML_RENDER_COLORDEFINITION:
			return SWIGTYPE_p_ColorDefinition;
			
		case SBML_RENDER_ELLIPSE:
			return SWIGTYPE_p_Ellipse;

		case SBML_RENDER_GLOBALRENDERINFORMATION:
			return SWIGTYPE_p_GlobalRenderInformation;
			
		case SBML_RENDER_GLOBALSTYLE:
			return SWIGTYPE_p_GlobalStyle;
		
		case SBML_RENDER_GROUP:
			return SWIGTYPE_p_RenderGroup;
		
		case SBML_RENDER_IMAGE:
			return SWIGTYPE_p_Image;
		
		case SBML_RENDER_LINEENDING:
			return SWIGTYPE_p_LineEnding;
		
		case SBML_RENDER_LINEARGRADIENT:
			return SWIGTYPE_p_LinearGradient;
		
		case SBML_RENDER_LOCALRENDERINFORMATION:
			return SWIGTYPE_p_LocalRenderInformation;
		
		case SBML_RENDER_LOCALSTYLE:
			return SWIGTYPE_p_LocalStyle;
		
		case SBML_RENDER_POLYGON:
			return SWIGTYPE_p_Polygon;
		
		case SBML_RENDER_RADIALGRADIENT:
			return SWIGTYPE_p_RadialGradient;
		
		case SBML_RENDER_RECTANGLE:
			return SWIGTYPE_p_Rectangle;
		
		case SBML_RENDER_CUBICBEZIER:
			return SWIGTYPE_p_RenderCubicBezier;
		
		case SBML_RENDER_CURVE:
			return SWIGTYPE_p_RenderCurve;
		
		case SBML_RENDER_POINT:
			return SWIGTYPE_p_RenderPoint;
		
		case SBML_RENDER_TEXT:
			return SWIGTYPE_p_Text;
			
		default:
			return SWIGTYPE_p_SBase;
	}
}
#endif // USE_RENDER				  

