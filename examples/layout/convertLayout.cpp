/**
 * @file    convertLayout.cpp
 * @brief   Converts SBML Layout and Render Information from L2 to L3 and vice versa
 * @author  Frank Bergmann
 *
 */


#include <iostream>
#include <string>
#include <vector>
#include <sstream>

#include <stdlib.h>


#define CONVERT_RENDER


#include "sbml/SBMLTypes.h"
#include "sbml/extension/SBMLDocumentPlugin.h"
#include "sbml/conversion/ConversionProperties.h"
#include "sbml/packages/layout/common/LayoutExtensionTypes.h"

#ifdef CONVERT_RENDER
#include "sbml/packages/render/common/RenderExtensionTypes.h"
#endif

using namespace std;
LIBSBML_CPP_NAMESPACE_USE


class LayoutConverter
{
public:
  
  LayoutConverter(SBMLDocument *doc) {
    _doc = doc;
  }
  
  LayoutConverter(const char* filename) {
    _doc = readSBMLFromFile(filename);
  }
  
  ~LayoutConverter(){
    delete _doc;
  }

  SBMLDocument* getDocument() const { return _doc; }

  void convertLayout()
  {
    if (_doc == NULL || _doc->getModel() == NULL)
      return;

    // if layout is not used bail
    if (_doc->getPlugin("layout") == NULL) 
      return;

    if (_doc->getLevel() == 3)
      convertLayoutToL2();
    else
      convertLayoutToL3();
  }

  void convertLayoutToL2()
  {    
    if (_doc == NULL || _doc->getModel() == NULL)
      return;

    layoutNsUri = "http://projects.eml.org/bcb/sbml/level2";
    layoutNs = new LayoutPkgNamespaces(2, 1);

#ifdef CONVERT_RENDER
    renderNsUri = "http://projects.eml.org/bcb/sbml/render/level2";
    renderNs = new RenderPkgNamespaces(2, 1);
#endif

    LayoutModelPlugin* plugin = (LayoutModelPlugin*)_doc->getModel()->getPlugin("layout");
    if (plugin == NULL) 
      return;


     ConversionProperties prop(new SBMLNamespaces(2, 4));
     prop.addOption("strict", false);
     prop.addOption("setLevelAndVersion", true);
     prop.addOption("ignorePackages", true);

     _doc->convert(prop);


    plugin->setElementNamespace(layoutNsUri);
    
    SBMLDocumentPlugin *docPlugin = (SBMLDocumentPlugin*)_doc->getPlugin("layout");
    if (docPlugin != NULL)
      docPlugin->setElementNamespace(layoutNsUri);
    
    
    updateNs(plugin->getListOfLayouts());
    
    _doc->getSBMLNamespaces()->removePackageNamespace(3, 1, "layout", 1);        

#ifdef CONVERT_RENDER
    SBMLDocumentPlugin *rdocPlugin = (SBMLDocumentPlugin*)_doc->getPlugin("render");
    if (rdocPlugin!= NULL)
      rdocPlugin->setElementNamespace(renderNsUri);
    _doc->getSBMLNamespaces()->removePackageNamespace(3, 1, "render", 1);        
#endif
  }

  void convertLayoutToL3()
  {
    if (_doc == NULL || _doc->getModel() == NULL)
      return;


    layoutNsUri = "http://www.sbml.org/sbml/level3/version1/layout/version1";
    layoutNs = new LayoutPkgNamespaces(3, 1, 1);

#ifdef CONVERT_RENDER
    renderNsUri = "http://www.sbml.org/sbml/level3/version1/render/version1";
    renderNs = new RenderPkgNamespaces(3, 1, 1);
#endif

    LayoutModelPlugin* plugin = (LayoutModelPlugin*)_doc->getModel()->getPlugin("layout");
    if (plugin == NULL) 
      return;


    ConversionProperties prop(new SBMLNamespaces(3, 1));
    prop.addOption("strict", false);
    prop.addOption("setLevelAndVersion", true);
    prop.addOption("ignorePackages", true);

    _doc->convert(prop);

    plugin->setElementNamespace(layoutNsUri);

    SBMLDocumentPlugin *docPlugin = (SBMLDocumentPlugin*)_doc->getPlugin("layout");
    if (docPlugin != NULL)
      docPlugin->setElementNamespace(layoutNsUri);

    updateNs(plugin->getListOfLayouts());
    
    _doc->getSBMLNamespaces()->addPackageNamespace("layout", 1);
    _doc->setPackageRequired("layout", false);

#ifdef CONVERT_RENDER
    SBMLDocumentPlugin *rdocPlugin = (SBMLDocumentPlugin*)_doc->getPlugin("render");
    if (rdocPlugin != NULL)
      rdocPlugin->setElementNamespace(renderNsUri);
    _doc->getSBMLNamespaces()->addPackageNamespace("render", 1);
    _doc->setPackageRequired("render", false);
#endif

  }

  void updateNs(ListOfLayouts *list)
  {
    list->setSBMLNamespaces(layoutNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((Layout*)(list->get(i)));

#ifdef CONVERT_RENDER

    RenderListOfLayoutsPlugin* lolPlugin = (RenderListOfLayoutsPlugin*)list->getPlugin("render");
    if (lolPlugin != NULL)
    {
      updateNs(lolPlugin->getListOfGlobalRenderInformation());
      lolPlugin->setElementNamespace(renderNsUri);
    }

#endif

  }

#ifdef CONVERT_RENDER
  
  void updateNs(ListOfGlobalRenderInformation *list)
  {
    list->setSBMLNamespaces(renderNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((GlobalRenderInformation*)(list->get(i)));
  }
  
  void updateNs(ListOfLocalRenderInformation *list)
  {
    list->setSBMLNamespaces(renderNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((LocalRenderInformation*)(list->get(i)));
  }

  void updateNs(GlobalRenderInformation *info)
  {
    info->setSBMLNamespaces(renderNs);

    updateNs(info->getListOfColorDefinitions());
    updateNs(info->getListOfGradientDefinitions());
    updateNs(info->getListOfLineEndings());
    updateNs(info->getListOfStyles());

  }

  
  void updateNs(LocalRenderInformation *info)
  {
    info->setSBMLNamespaces(renderNs);

    updateNs(info->getListOfColorDefinitions());
    updateNs(info->getListOfGradientDefinitions());
    updateNs(info->getListOfLineEndings());
    updateNs(info->getListOfStyles());

  }

  void updateNs(ListOfColorDefinitions *list)
  {
    list->setSBMLNamespaces(renderNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((ColorDefinition*)(list->get(i)));
  }
  
  void updateNs(ColorDefinition *color)
  {
    color->setSBMLNamespaces(renderNs);
  }

  void updateNs(ListOfGradientDefinitions *list)
  {
    list->setSBMLNamespaces(renderNs);

    for (unsigned int i = 0; i < list->size(); i++)
    {
      GradientBase* current = list->get(i);
      if (current->getTypeCode() == SBML_RENDER_LINEARGRADIENT)
        updateNs((LinearGradient*)(current));
      else
        updateNs((RadialGradient*)(current));
    }
  }

  void updateNs(GradientBase *gradient)
  {
    gradient->setSBMLNamespaces(renderNs);
    updateNs(gradient->getListOfGradientStops());
  }
  
  void updateNs(ListOfGradientStops *list)
  {
    list->setSBMLNamespaces(renderNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((GradientStop*)(list->get(i)));
  }
  
  void updateNs(GradientStop *stop)
  {
    stop->setSBMLNamespaces(renderNs);
  }

  void updateNs(ListOfLineEndings *list)
  {
    list->setSBMLNamespaces(renderNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((LineEnding*)(list->get(i)));
  }
    
  void updateNs(LineEnding *line)
  {
    line->setSBMLNamespaces(renderNs);
    updateNs(line->getBoundingBox());
    updateNs(line->getGroup());
  }

  void updateNs(ListOfGlobalStyles *list)
  {
    list->setSBMLNamespaces(renderNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((GlobalStyle*)(list->get(i)));
  }
  
  void updateNs(GlobalStyle *style)
  {
    style->setSBMLNamespaces(renderNs);
    updateNs(style->getGroup());
  }

  void updateNs(LocalStyle *style)
  {
    style->setSBMLNamespaces(renderNs);
    updateNs(style->getGroup());
  }

  void updateNs(RenderGroup *group)
  {
    group->setSBMLNamespaces(renderNs);
    updateNs(group->getListOfElements());
  }

  void updateNs(ListOfDrawables *list)
  {
    list->setSBMLNamespaces(renderNs);

    for (unsigned int i = 0; i < list->size(); i++)
    {
      Transformation2D* current = list->get(i);
      switch(current->getTypeCode())
      {
      case SBML_RENDER_CURVE:
        updateNs((RenderCurve*)(current));
        break;
      case SBML_RENDER_ELLIPSE:
        updateNs((Ellipse*)(current));
        break;
      case SBML_RENDER_GROUP:
        updateNs((RenderGroup*)(current));
        break;
      case SBML_RENDER_IMAGE:
        updateNs((Image*)(current));
        break;
      case SBML_RENDER_POLYGON:
        updateNs((Polygon*)(current));
        break;
      case SBML_RENDER_RECTANGLE:
        updateNs((Rectangle*)(current));
        break;
      case SBML_RENDER_TEXT:
        updateNs((Text*)(current));
        break;
      }      
    }
  }
  
  void updateNs(RenderCurve *curve)
  {
    curve->setSBMLNamespaces(renderNs);
    updateNs(curve->getListOfElements());
  }

  void updateNs(Transformation2D *element)
  {
    element->setSBMLNamespaces(renderNs);    
  }
  
  void updateNs(Polygon *element)
  {
    element->setSBMLNamespaces(renderNs);    
    updateNs(element->getListOfElements());
  }

  void updateNs(ListOfCurveElements *list)
  {
    list->setSBMLNamespaces(renderNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((RenderPoint*)(list->get(i)));
  }
    
  void updateNs(RenderPoint *element)
  {
    element->setSBMLNamespaces(renderNs);    
  }

  void updateNs(ListOfLocalStyles *list)
  {
    list->setSBMLNamespaces(renderNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((LocalStyle*)(list->get(i)));
  }

#endif

  void updateNs(ListOfGraphicalObjects *list)
  {
    list->setSBMLNamespaces(layoutNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((GraphicalObject*)(list->get(i)));
  }

  void updateNs(ListOfSpeciesGlyphs*list)
  {
    list->setSBMLNamespaces(layoutNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((SpeciesGlyph*)(list->get(i)));
  }

  void updateNs(ListOfCompartmentGlyphs*list)
  {
    list->setSBMLNamespaces(layoutNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((CompartmentGlyph*)(list->get(i)));
  }

  void updateNs(ListOfLineSegments *list)
  {
    list->setSBMLNamespaces(layoutNs);

    for (unsigned int i = 0; i < list->size(); i++)
    {
      LineSegment* current = (LineSegment*)(list->get(i));
      if (current->getTypeCode() == SBML_LAYOUT_CUBICBEZIER)
        updateNs((CubicBezier*)current);
      else
        updateNs(current);
    }
  }

  void updateNs(ListOfReactionGlyphs*list)
  {
    list->setSBMLNamespaces(layoutNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((ReactionGlyph*)(list->get(i)));
  }

  void updateNs(ListOfSpeciesReferenceGlyphs *list)
  {
    list->setSBMLNamespaces(layoutNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((SpeciesReferenceGlyph*)(list->get(i)));
  }

  void updateNs(ListOfTextGlyphs* list)
  {
    list->setSBMLNamespaces(layoutNs);

    for (unsigned int i = 0; i < list->size(); i++)
      updateNs((TextGlyph*)(list->get(i)));
  }

  void updateNs(CubicBezier *cubic)
  {
    cubic->setSBMLNamespaces(layoutNs);
    updateNs((LineSegment*)(cubic));
    updateNs(cubic->getBasePoint1());
    updateNs(cubic->getBasePoint2());
  }


  void updateNs(LineSegment *segment)
  {
    segment->setSBMLNamespaces(layoutNs);
    updateNs(segment->getStart());
    updateNs(segment->getEnd());
  }

  void updateNs(SpeciesGlyph *glyph)
  {
    glyph->setSBMLNamespaces(layoutNs);
    updateNs((GraphicalObject*) glyph);
  }

  void updateNs(SpeciesReferenceGlyph *glyph)
  {
    glyph->setSBMLNamespaces(layoutNs);
    updateNs((Curve*)glyph->getCurve());
    updateNs((GraphicalObject*) glyph);
  }


  void updateNs(TextGlyph *glyph)
  {
    glyph->setSBMLNamespaces(layoutNs);
    updateNs((GraphicalObject*) glyph);
  }

  void updateNs(CompartmentGlyph *glyph)
  {
    glyph->setSBMLNamespaces(layoutNs);
    updateNs((GraphicalObject*) glyph);
  }

  void updateNs(ReactionGlyph *glyph)
  {
    glyph->setSBMLNamespaces(layoutNs);
    updateNs((GraphicalObject*) glyph);

    updateNs(glyph->getListOfSpeciesReferenceGlyphs());
    updateNs(glyph->getCurve());

  }

  void updateNs(Curve *curve)
  {
    curve->setSBMLNamespaces(layoutNs);        
    updateNs(curve->getListOfCurveSegments());
  }

  void updateNs(GraphicalObject *glyph)
  {
    glyph->setSBMLNamespaces(layoutNs);    
    updateNs(glyph->getBoundingBox());
  }

  void updateNs(BoundingBox *box)
  {
    box->setSBMLNamespaces(layoutNs);        
    updateNs(box->getDimensions());
    updateNs(box->getPosition());
  }

  void updateNs(Dimensions *dim)
  {
    dim->setSBMLNamespaces(layoutNs);        
  }

  void updateNs(Point *point)
  {
    point->setSBMLNamespaces(layoutNs);        
  }

  void updateNs(Layout *layout)
  {
    layout->setSBMLNamespaces(layoutNs);

    updateNs(layout->getDimensions());
    updateNs(layout->getListOfAdditionalGraphicalObjects());
    updateNs(layout->getListOfCompartmentGlyphs());
    updateNs(layout->getListOfReactionGlyphs());
    updateNs(layout->getListOfSpeciesGlyphs());
    updateNs(layout->getListOfTextGlyphs());    

#ifdef CONVERT_RENDER

    RenderLayoutPlugin* layoutPlugin = (RenderLayoutPlugin*)layout->getPlugin("render");
    if (layoutPlugin != NULL)
    {
      updateNs(layoutPlugin->getListOfLocalRenderInformation());
      layoutPlugin->setElementNamespace(renderNsUri);

    }

#endif
  }


protected:

  SBMLDocument* _doc;

#ifdef CONVERT_RENDER
  string renderNsUri;
  SBMLNamespaces *renderNs;
#endif

  string layoutNsUri; 
  SBMLNamespaces *layoutNs;

};

int main(int argc,char** argv)
{

  if (argc != 3)
  {
    cerr << "usage convertLayout <input> <output>";
    exit(1);
  }

  // read document
  LayoutConverter converter(argv[1]);  
  
  // convert from L3 -> L2 or L2 -> L3
  converter.convertLayout();
  

  char* sbml = writeSBMLToString(converter.getDocument());

  // write document
  writeSBML(converter.getDocument(),argv[2]);

}

