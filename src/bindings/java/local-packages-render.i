#ifdef USE_RENDER

/**
 * Adds DownCastBase(long cPtr, boolean owner) method for the layout package extension
 */
%typemap(javacode) RenderExtension
%{
  public SBasePlugin DowncastSBasePlugin(long cPtr, boolean owner)
  {
    if (cPtr == 0) return null;
    
    SBasePlugin sbp = new SBasePlugin(cPtr, false);
    SBase sb = sbp.getParentSBMLObject();
    
    switch( sb.getTypeCode() )
    {
      case (int) libsbml.SBML_LIST_OF:
           String name = sb.getElementName();
           if(name.equals("listOfLayouts"))
           {
             return new RenderListOfLayoutsPlugin(cPtr, owner);
           }
        return new SBasePlugin(cPtr,owner);
      case (int) libsbml.SBML_DOCUMENT:
        return new SBMLDocumentPlugin(cPtr, owner);
      case (int) libsbml.SBML_LAYOUT_LAYOUT:
        return new RenderLayoutPlugin(cPtr, owner);
      case (int) libsbml.SBML_LAYOUT_GRAPHICALOBJECT:
        return new RenderGraphicalObjectPlugin(cPtr, owner);
      default:
        return new SBasePlugin(cPtr,owner);
    }
  }
  
  public SBase DowncastSBase(long cPtr, boolean owner)
  {
    if (cPtr == 0) return null;
    
    SBase sb = new SBase(cPtr, false);
    switch( sb.getTypeCode() )
    {
      case (int) libsbml.SBML_LIST_OF:
           String name = sb.getElementName();
           int itemType = ((ListOf)sb).getItemTypeCode();
           if(name.equals("listOfColorDefinitions"))
           {
             return new ListOfColorDefinitions(cPtr, owner);
           }
           else if(name.equals("listOfGlobalRenderInformation"))
           {
             return new ListOfGlobalRenderInformation(cPtr, owner);
           }
           else if(name.equals("listOfStyles"))
           {
            if (itemType == libsbml.SBML_RENDER_LOCALSTYLE)
              return new ListOfLocalStyles(cPtr, owner);
            else 
              return new ListOfGlobalStyles(cPtr, owner);
           }
           else if(name.equals("listOfGradientDefinitions"))
           {
             return new ListOfGradientDefinitions(cPtr, owner);
           }
           else if(name.equals("listOfLineEndings"))
           {
                return new ListOfLineEndings(cPtr, owner);
           }
           else if(name.equals("listOfElements"))
           {
                return new ListOfCurveElements(cPtr, owner);
           }
           else if(name.equals("listOfRenderInformation"))
           {
                return new ListOfLocalRenderInformation(cPtr, owner);
           }
           else if(name.equals("listOfDrawables"))
           {
                return new ListOfDrawables(cPtr, owner);
           }
           else if(name.equals("listOfGradientStops"))
           {
                return new ListOfGradientStops(cPtr, owner);
           }
           return new ListOf(cPtr, owner);
        
      case (int) libsbml.SBML_RENDER_COLORDEFINITION:
        return new ColorDefinition(cPtr, owner);
        
      case (int) libsbml.SBML_RENDER_ELLIPSE:
        return new Ellipse(cPtr, owner);

      case (int) libsbml.SBML_RENDER_GLOBALRENDERINFORMATION:
        return new GlobalRenderInformation(cPtr, owner);
        
      case (int) libsbml.SBML_RENDER_GLOBALSTYLE:
        return new GlobalStyle(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_GROUP:
        return new RenderGroup(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_IMAGE:
        return new Image(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_LINEENDING:
        return new LineEnding(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_LINEARGRADIENT:
        return new LinearGradient(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_LOCALRENDERINFORMATION:
        return new LocalRenderInformation(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_LOCALSTYLE:
        return new LocalStyle(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_POLYGON:
        return new Polygon(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_RADIALGRADIENT:
        return new RadialGradient(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_RECTANGLE:
        return new Rectangle(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_CUBICBEZIER:
        return new RenderCubicBezier(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_CURVE:
        return new RenderCurve(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_POINT:
        return new RenderPoint(cPtr, owner);
      
      case (int) libsbml.SBML_RENDER_TEXT:
        return new Text(cPtr, owner);
      
      default:
        return new SBase(cPtr, owner);
    }
  }
  
  %}

COVARIANT_RTYPE_CLONE(ListOfGlobalRenderInformation)
COVARIANT_RTYPE_CLONE(ListOfLocalStyles)
COVARIANT_RTYPE_CLONE(ListOfGlobalStyles)
COVARIANT_RTYPE_CLONE(ListOfGradientDefinitions)
COVARIANT_RTYPE_CLONE(ListOfColorDefinitions)
COVARIANT_RTYPE_CLONE(ListOfLineEndings)
COVARIANT_RTYPE_CLONE(ListOfCurveElements)
COVARIANT_RTYPE_CLONE(ListOfLocalRenderInformation)
COVARIANT_RTYPE_CLONE(ListOfDrawables)
COVARIANT_RTYPE_CLONE(ListOfGradientStops)

COVARIANT_RTYPE_CLONE(RenderExtension)
COVARIANT_RTYPE_CLONE(ColorDefinition)
COVARIANT_RTYPE_CLONE(Ellipse)
COVARIANT_RTYPE_CLONE(GlobalRenderInformation)
COVARIANT_RTYPE_CLONE(GlobalStyle)
COVARIANT_RTYPE_CLONE(GradientDefinition)
COVARIANT_RTYPE_CLONE(Group)
COVARIANT_RTYPE_CLONE(Image)
COVARIANT_RTYPE_CLONE(LineEnding)
COVARIANT_RTYPE_CLONE(LinearGradient)
COVARIANT_RTYPE_CLONE(LocalRenderInformation)
COVARIANT_RTYPE_CLONE(LocalStyle)
COVARIANT_RTYPE_CLONE(Polygon)
COVARIANT_RTYPE_CLONE(RadialGradient)
COVARIANT_RTYPE_CLONE(Rectangle)
COVARIANT_RTYPE_CLONE(RenderCubicBezier)
COVARIANT_RTYPE_CLONE(RenderCurve)
COVARIANT_RTYPE_CLONE(RenderPoint)
COVARIANT_RTYPE_CLONE(Text)

COVARIANT_RTYPE_LISTOF_GET_REMOVE(ColorDefinition)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(Ellipse)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(GlobalRenderInformation)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(GlobalStyle)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(GradientDefinition)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(Group)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(Image)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(LineEnding)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(LinearGradient)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(LocalRenderInformation)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(LocalStyle)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(Polygon)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(RadialGradient)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(Rectangle)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(RenderCubicBezier)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(RenderCurve)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(RenderPoint)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(Text)


SBMLCONSTRUCTOR_EXCEPTION(ColorDefinition)
SBMLCONSTRUCTOR_EXCEPTION(Ellipse)
SBMLCONSTRUCTOR_EXCEPTION(GlobalRenderInformation)
SBMLCONSTRUCTOR_EXCEPTION(GlobalStyle)
SBMLCONSTRUCTOR_EXCEPTION(Group)
SBMLCONSTRUCTOR_EXCEPTION(RenderGroup)
SBMLCONSTRUCTOR_EXCEPTION(GradientDefinition)
SBMLCONSTRUCTOR_EXCEPTION(GradientStop)
SBMLCONSTRUCTOR_EXCEPTION(Image)
SBMLCONSTRUCTOR_EXCEPTION(LineEnding)
SBMLCONSTRUCTOR_EXCEPTION(LinearGradient)
SBMLCONSTRUCTOR_EXCEPTION(LocalRenderInformation)
SBMLCONSTRUCTOR_EXCEPTION(LocalStyle)
SBMLCONSTRUCTOR_EXCEPTION(Polygon)
SBMLCONSTRUCTOR_EXCEPTION(RadialGradient)
SBMLCONSTRUCTOR_EXCEPTION(Rectangle)
SBMLCONSTRUCTOR_EXCEPTION(RenderCubicBezier)
SBMLCONSTRUCTOR_EXCEPTION(RenderCurve)
SBMLCONSTRUCTOR_EXCEPTION(RenderPoint)
SBMLCONSTRUCTOR_EXCEPTION(Text)
SBMLCONSTRUCTOR_EXCEPTION(RenderPkgNamespaces)

SBMLCONSTRUCTOR_EXCEPTION(ListOfGlobalRenderInformation)
SBMLCONSTRUCTOR_EXCEPTION(ListOfLocalStyles)
SBMLCONSTRUCTOR_EXCEPTION(ListOfGlobalStyles)
SBMLCONSTRUCTOR_EXCEPTION(ListOfGradientDefinitions)
SBMLCONSTRUCTOR_EXCEPTION(ListOfColorDefinitions)
SBMLCONSTRUCTOR_EXCEPTION(ListOfLineEndings)
SBMLCONSTRUCTOR_EXCEPTION(ListOfCurveElements)
SBMLCONSTRUCTOR_EXCEPTION(ListOfLocalRenderInformation)
SBMLCONSTRUCTOR_EXCEPTION(ListOfDrawables)
SBMLCONSTRUCTOR_EXCEPTION(ListOfGradientStops)
SBMLCONSTRUCTOR_EXCEPTION(Polygon)
SBMLCONSTRUCTOR_EXCEPTION(RadialGradient)

//
// Convert Render objects into the most specific object possible.
//
%typemap("javaout") GradientBase*, GraphicalPrimitive1D*, GraphicalPrimitive2D*, RenderInformationBase*, Style*, RenderPoint*, Transformation*, Transformation2D*
{
  return ($javaclassname) libsbml.DowncastSBase($jnicall, $owner);
}


#endif  /* USE_RENDER */
