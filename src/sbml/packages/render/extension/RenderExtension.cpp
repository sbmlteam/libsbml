/**
 * @file    RenderExtension.cpp
 * @brief   Implementation of RenderExtension, the core module of the render package.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/extension/SBasePluginCreator.h>

#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/layout/extension/LayoutExtension.h>
#include <sbml/packages/layout/extension/LayoutModelPlugin.h>
#include <sbml/packages/render/extension/RenderListOfLayoutsPlugin.h>
#include <sbml/packages/render/validator/RenderSBMLErrorTable.h>
#include <sbml/packages/render/extension/RenderGraphicalObjectPlugin.h>
#include <sbml/packages/render/extension/RenderSBMLDocumentPlugin.h>
#include <sbml/SBMLDocument.h>
#include <sbml/packages/render/extension/RenderLayoutPlugin.h>
#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/packages/render/util/RenderLayoutConverter.h>

#ifdef __cplusplus

#include <iostream>
#include <string>

LIBSBML_CPP_NAMESPACE_BEGIN

// -------------------------------------------------------------------------
//
// This block is global initialization code which should be automatically 
// executed before invoking main() block.
//
// -------------------------------------------------------------------------

//------------- (START) -----------------------------------

// The name of this package

const std::string& RenderExtension::getPackageName ()
{
  static const std::string pkgName = "render";
  return pkgName;
}

//
// Default SBML level, version, and package version
//
unsigned int RenderExtension::getDefaultLevel()
{
  return 3;
}  

unsigned int RenderExtension::getDefaultVersion()
{
  return 1; 
}

unsigned int RenderExtension::getDefaultPackageVersion()
{
  return 1;
} 

//
// XML namespaces of (1) package versions of groups extension, and 
// (2) another XML namespace(XMLSchema-instance) required in the groups 
//  extension.
//

const std::string& RenderExtension::getXmlnsL3V1V1 ()
{
  static const std::string xmlns = "http://www.sbml.org/sbml/level3/version1/render/version1";
  return xmlns;
}

const std::string& RenderExtension::getXmlnsL2 ()
{
  static const std::string xmlns = "http://projects.eml.org/bcb/sbml/render/level2";
  return xmlns;
}


//
// Adds this RenderExtension object to the SBMLExtensionRegistry class.
// RenderExtension::init() function is automatically invoked when this
// object is instantiated.
//
static SBMLExtensionRegister<RenderExtension> renderExtensionRegister;

static
const char* SBML_RENDER_TYPECODE_STRINGS[] =
{
    "ColorDefinition"
  , "Ellipse"
  , "GlobalRenderInformation"
  , "GlobalStyle"
  , "GradientDefinition"
  , "GradientStop"
  , "Group"
  , "Image"
  , "LineEnding"
  , "LinearGradient"
  , "LineSegment"
  , "ListOfGlobalStyles"
  , "ListOfLocalStyles"
  , "LocalRenderInformation"
  , "LocalStyle"
  , "Polygon"
  , "RadialGradient"
  , "Rectangle"
  , "RelAbsVector"
  , "CubicBezier"
  , "Curve"
  , "Point"
  , "Text"
  , "Transformation2D"
  , "DefaultValues"

};

//------------- (END) -----------------------------------

// --------------------------------------------------------
//
// Instantiate SBMLExtensionNamespaces<RenderExtension>
// (RenderPkgNamespaces) for DLL.
//
// --------------------------------------------------------

template class LIBSBML_EXTERN SBMLExtensionNamespaces<RenderExtension>;



RenderExtension::RenderExtension ()
{
}


/*
 * Copy constructor.
 */
RenderExtension::RenderExtension(const RenderExtension& orig)
: SBMLExtension(orig)
{
}


/*
 * Destroy this object.
 */
RenderExtension::~RenderExtension ()
{
}


/*
 * Assignment operator for RenderExtension.
 */
RenderExtension& 
RenderExtension::operator=(const RenderExtension& orig)
{
  SBMLExtension::operator=(orig);
  return *this;
}


/*
 * Creates and returns a deep copy of this RenderExtension object.
 * 
 * @return a (deep) copy of this RenderExtension object
 */
RenderExtension* 
RenderExtension::clone () const
{
  return new RenderExtension(*this);  
}


const std::string&
RenderExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns the URI (namespace) of the package corresponding to the combination of the given sbml level,
 * sbml version, and package version.
 * Empty string will be returned if no corresponding URI exists.
 *
 * @return a string of the package URI
 */
const std::string& 
RenderExtension::getURI(unsigned int sbmlLevel, unsigned int sbmlVersion, unsigned int pkgVersion) const
{
  if (sbmlLevel == 3)
  {
    if (sbmlVersion == 1 || sbmlVersion == 2)
    {
      if (pkgVersion == 1)
      {
        return getXmlnsL3V1V1();
      }
    }
  }
  else if (sbmlLevel == 2)
  {
    return getXmlnsL2();
  }

  static std::string empty = "";

  return empty;
}


/*
 * Returns the SBML level with the given URI of this package.
 *
 *  (NOTICE) Package developers MUST OVERRIDE this pure virtual function in their derived class.
 *
 */
unsigned int 
RenderExtension::getLevel(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 3;
  }
  else if (uri == getXmlnsL2())
  {
    return 2;
  }
  
  return 0;
}


/*
 * Returns the SBML version with the given URI of this package.
 *
 *  (NOTICE) Package developers MUST OVERRIDE this pure virtual function in their derived class.
 *
 */
unsigned int 
RenderExtension::getVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }
  else if (uri == getXmlnsL2())
  {
    //
    // (NOTE) This may cause unexpected behaviour.
    //
    /* which indeed it does */
    return 1;
  }

  return 0;
}


/*
 * Returns the package version with the given URI of this package.
 *
 *  (NOTICE) Package developers MUST OVERRIDE this pure virtual function in their derived class.
 *
 */
unsigned int
RenderExtension::getPackageVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }
  else if (uri == getXmlnsL2())
  {
    //  
    // (NOTE) This should be harmless but may cause some problem.
    //
    return 1;
  }

  return 0;
}


/*
 * Returns an SBMLExtensionNamespaces<class SBMLExtensionType> object 
 * (e.g. SBMLExtensionNamespaces<RenderExtension> whose alias type is 
 * RenderPkgNamespaces) corresponding to the given uri.
 * NULL will be returned if the given uri is not defined in the corresponding package.
 *
 *  (NOTICE) Package developers MUST OVERRIDE this pure virtual function in their derived class.
 *
 */
SBMLNamespaces*
RenderExtension::getSBMLExtensionNamespaces(const std::string& uri) const
{
  RenderPkgNamespaces* pkgns = NULL;

  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new RenderPkgNamespaces(3, 1, 1);
  }
  else if ( uri == getXmlnsL2())
  {
    //  
    // (NOTE) This should be harmless but may cause some problem.
    //
    pkgns = new RenderPkgNamespaces(2);
  }  
  return pkgns;
}


/*
 * This method takes a type code of groups package and returns a string representing
 * the code.
 */
const char* 
RenderExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_RENDER_COLORDEFINITION;
  int max = SBML_RENDER_DEFAULTS;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Render Type)";  
  }

  return SBML_RENDER_TYPECODE_STRINGS[typeCode - min];
}



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the entry in the error table at this index.
 */
packageErrorTableEntry
RenderExtension::getErrorTable(unsigned int index) const
{
  return renderErrorTable[index];
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Return the index in the error table with the given errorId.
 */
unsigned int
RenderExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize =
    sizeof(renderErrorTable)/sizeof(renderErrorTable[0]);
  unsigned int index = 0;

  for (unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == renderErrorTable[i].code)
    {
      index = i;
      break;
    }
  }

  return index;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the offset for the errorId range for the "render" package.
 */
unsigned int
RenderExtension::getErrorIdOffset() const
{
  return 1300000;
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 *
 * Initialization function of groups extension module which is automatically invoked 
 * by SBMLExtensionRegister class before main() function invoked.
 *
 */
void 
RenderExtension::init()
{
  //-------------------------------------------------------------------------
  //
  // 1. Checks if the groups pacakge has already been registered.
  //
  //-------------------------------------------------------------------------

  if (SBMLExtensionRegistry::getInstance().isRegistered(getPackageName()))
  {
    // do nothing;
    return;
  }

  //-------------------------------------------------------------------------
  //
  // 2. Creates an SBMLExtension derived object.
  //
  //-------------------------------------------------------------------------

  RenderExtension renderExtension;

  //-------------------------------------------------------------------------------------
  //
  // 3. Creates SBasePluginCreatorBase derived objects required for this 
  //    extension. The derived classes can be instantiated by using the following 
  //     template class.
  //
  //    temaplate<class SBasePluginType> class SBasePluginCreator
  //
  //    The constructor of the creator class has two arguments:
  //
  //        (1) SBaseExtensionPoint : extension point to which the plugin object connected
  //        (2) std::vector<std::string> : a std::vector object that contains a list of URI
  //                                       (package versions) supported by the plugin object.
  //
  //    For example, two plugin objects (plugged in SBMLDocument and Model elements) are 
  //    required for the groups extension.
  //
  //    Since only 'required' attribute is used in SBMLDocument by the groups package, existing
  //    SBMLDocumentPluginNotRequired class can be used as-is for the plugin.
  //
  //    Since the lists of supported package versions (currently only L3V1-groups-V1 supported )
  //    are equal in the both plugin objects, the same vector object is given to each 
  //    constructor.
  //
  //---------------------------------------------------------------------------------------

  std::vector<std::string> packageURIs;
  packageURIs.push_back(getXmlnsL3V1V1());
  packageURIs.push_back(getXmlnsL2());  

  // 
  // LayoutSpeciesReferencePlugin is used only for SBML Level 2
  //
  std::vector<std::string> L2packageURI;
  L2packageURI.push_back(getXmlnsL2());  

  SBaseExtensionPoint sbmldocExtPoint("core",SBML_DOCUMENT);
  SBaseExtensionPoint layoutExtPoint("layout",SBML_LAYOUT_LAYOUT);
  SBaseExtensionPoint layoutGOExtPoint("layout",SBML_LAYOUT_GRAPHICALOBJECT);
  SBaseExtensionPoint clayoutExtPoint("core",SBML_LAYOUT_LAYOUT);
  SBaseExtensionPoint listOfLayoutsExtPoint("layout", SBML_LIST_OF);

  SBasePluginCreator<RenderSBMLDocumentPlugin, RenderExtension> sbmldocPluginCreator(sbmldocExtPoint,packageURIs);
  SBasePluginCreator<RenderLayoutPlugin,   RenderExtension> layoutPluginCreator(layoutExtPoint,packageURIs);
  SBasePluginCreator<RenderLayoutPlugin,   RenderExtension> clayoutPluginCreator(clayoutExtPoint,packageURIs);
  SBasePluginCreator<RenderListOfLayoutsPlugin,   RenderExtension> lolPluginCreator(listOfLayoutsExtPoint,packageURIs);
  SBasePluginCreator<RenderGraphicalObjectPlugin,   RenderExtension> goPluginCreator(layoutGOExtPoint,packageURIs);

  //--------------------------------------------------------------------------------------
  //
  // 3. Adds the above SBasePluginCreatorBase derived objects to the SBMLExtension derived object.
  //
  //--------------------------------------------------------------------------------------

  renderExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  renderExtension.addSBasePluginCreator(&layoutPluginCreator);
  renderExtension.addSBasePluginCreator(&clayoutPluginCreator);
  renderExtension.addSBasePluginCreator(&lolPluginCreator);
  renderExtension.addSBasePluginCreator(&goPluginCreator);
  
  //-------------------------------------------------------------------------
  //
  // 4. Registers the SBMLExtension derived object to SBMLExtensionRegistry
  //
  //-------------------------------------------------------------------------

  int result = SBMLExtensionRegistry::getInstance().addExtension(&renderExtension);

  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    std::cerr << "[Error] RenderExtension::init() failed." << std::endl;
  }

  // add converter to registry;
  RenderLayoutConverter rlc;
  SBMLConverterRegistry::getInstance().addConverter(&rlc);
}
/** @endcond */


/*
 * Removes the L2 Namespace from a document. 
 *
 * This method should be overridden by all extensions that want to serialize
 * to an L2 annotation.
 */
void RenderExtension::removeL2Namespaces(XMLNamespaces* xmlns)  const
{
  for (int n = 0; n < xmlns->getNumNamespaces(); n++)
  {
    if (xmlns->getURI(n) == RenderExtension::getXmlnsL2())
    {
      xmlns->remove(n);
    }
  }
}

/*
 * adds the L2 Namespace 
 *
 * This method should be overridden by all extensions that want to serialize
 * to an L2 annotation.
 */
void RenderExtension::addL2Namespaces(XMLNamespaces* xmlns)  const
{
  if (!xmlns->containsUri( RenderExtension::getXmlnsL2()))
    xmlns->add(RenderExtension::getXmlnsL2(), "render");
}

/*
 * Adds the L2 Namespace to the document and enables the extension.
 *
 * If the extension supports serialization to SBML L2 Annotations, this 
 * method should be overrridden, so it will be activated.
 */
void RenderExtension::enableL2NamespaceForDocument(SBMLDocument* doc)  const
{
  if (doc->getLevel() == 2)
  {
    doc->enablePackage(RenderExtension::getXmlnsL2(),"render", true);
  }
}

bool 
RenderExtension::isInUse(SBMLDocument *doc) const
{  

  if (doc == NULL || doc->getModel() == NULL) return false;
  LayoutModelPlugin* plugin = (LayoutModelPlugin*)doc->getModel()->getPlugin("layout");
  if (plugin == NULL || plugin->getNumLayouts() == 0) return false;

  RenderListOfLayoutsPlugin* lolPlugin = (RenderListOfLayoutsPlugin*) plugin->getListOfLayouts()->getPlugin("render");
  if (lolPlugin != NULL && lolPlugin->getNumGlobalRenderInformationObjects() > 0) return true;

  for(int i = 0; i < plugin->getNumLayouts(); i++)
  {
    Layout* layout = plugin->getLayout(i);
    RenderLayoutPlugin* rPlugin = (RenderLayoutPlugin*)layout->getPlugin("render");
    if (rPlugin != NULL && rPlugin->getNumLocalRenderInformationObjects() > 0) return true;
  }

  return false;
}


#endif  /* __cplusplus */


//static
//const char* SBML_STYLE_TYPE_STRINGS[] =
//{
//  "COMPARTMENTGLYPH"
//, "SPECIESGLYPH"
//, "REACTIONGLYPH"
//, "SPECIESREFERENCEGLYPH"
//, "TEXTGLYPH"
//, "GENERALGLYPH"
//, "GRAPHICALOBJECT"
//, "ANY"
//, "invalid StyleType"
//};
//
//
///*
// * Returns the string version of the provided #StyleType_t enumeration.
// */
//LIBSBML_EXTERN
//const char*
//StyleType_toString(StyleType_t st)
//{
//  int min = STYLE_TYPE_COMPARTMENTGLYPH;
//  int max = STYLE_TYPE_INVALID;
//
//  if (st < min || st > max)
//  {
//    return "(Unknown StyleType value)";
//  }
//
//  return SBML_STYLE_TYPE_STRINGS[st - min];
//}
//
//
///*
// * Returns the #StyleType_t enumeration corresponding to the given string or
// * @sbmlconstant{STYLE_TYPE_INVALID, StyleType_t} if there is no such match.
// */
//LIBSBML_EXTERN
//StyleType_t
//StyleType_fromString(const char* code)
//{
//  static int size =
//    sizeof(SBML_STYLE_TYPE_STRINGS)/sizeof(SBML_STYLE_TYPE_STRINGS[0]);
//  std::string type(code);
//
//  for (int i = 0; i < size; i++)
//  {
//    if (type == SBML_STYLE_TYPE_STRINGS[i])
//    {
//      return (StyleType_t)(i);
//    }
//  }
//
//  return STYLE_TYPE_INVALID;
//}
//
//
///*
// * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
// * given #StyleType_t is valid.
// */
//LIBSBML_EXTERN
//int
//StyleType_isValid(StyleType_t st)
//{
//  int min = STYLE_TYPE_COMPARTMENTGLYPH;
//  int max = STYLE_TYPE_INVALID;
//
//  if (st < min || st >= max)
//  {
//    return 0;
//  }
//  else
//  {
//    return 1;
//  }
//}
//
//
///*
// * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
// * given string is a valid #StyleType_t.
// */
//LIBSBML_EXTERN
//int
//StyleType_isValidString(const char* code)
//{
//  return StyleType_isValid(StyleType_fromString(code));
//}
//
//
static
const char* SBML_GRADIENT_SPREAD_METHOD_STRINGS[] =
{
  "pad"
, "reflect"
, "repeat"
, "invalid"
};


/*
 * Returns the string version of the provided #GradientSpreadMethod_t
 * enumeration.
 */
LIBSBML_EXTERN
const char*
GradientSpreadMethod_toString(GradientSpreadMethod_t gsm)
{
  int min = GRADIENT_SPREADMETHOD_PAD;
  int max = GRADIENT_SPREAD_METHOD_INVALID;

  if (gsm < min || gsm > max)
  {
    return "(Unknown GradientSpreadMethod value)";
  }

  return SBML_GRADIENT_SPREAD_METHOD_STRINGS[gsm - min];
}


/*
 * Returns the #GradientSpreadMethod_t enumeration corresponding to the given
 * string or @sbmlconstant{GRADIENT_SPREAD_METHOD_INVALID,
 * GradientSpreadMethod_t} if there is no such match.
 */
LIBSBML_EXTERN
GradientSpreadMethod_t
GradientSpreadMethod_fromString(const char* code)
{
  static int size =
    sizeof(SBML_GRADIENT_SPREAD_METHOD_STRINGS)/sizeof(SBML_GRADIENT_SPREAD_METHOD_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_GRADIENT_SPREAD_METHOD_STRINGS[i])
    {
      return (GradientSpreadMethod_t)(i);
    }
  }

  return GRADIENT_SPREAD_METHOD_INVALID;
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #GradientSpreadMethod_t is valid.
 */
LIBSBML_EXTERN
int
GradientSpreadMethod_isValid(GradientSpreadMethod_t gsm)
{
  int min = GRADIENT_SPREADMETHOD_PAD;
  int max = GRADIENT_SPREAD_METHOD_INVALID;

  if (gsm < min || gsm >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #GradientSpreadMethod_t.
 */
LIBSBML_EXTERN
int
GradientSpreadMethod_isValidString(const char* code)
{
  return GradientSpreadMethod_isValid(GradientSpreadMethod_fromString(code));
}


static
const char* SBML_FILL_RULE_STRINGS[] =
{
  "unset"
, "nonzero"
, "evenodd"
, "inherit"
, "invalid FillRule"
};


/*
 * Returns the string version of the provided #FillRule_t enumeration.
 */
LIBSBML_EXTERN
const char*
FillRule_toString(FillRule_t fr)
{
  int min = FILL_RULE_UNSET;
  int max = FILL_RULE_INVALID;

  if (fr < min || fr > max)
  {
    return "(Unknown FillRule value)";
  }

  return SBML_FILL_RULE_STRINGS[fr - min];
}


/*
 * Returns the #FillRule_t enumeration corresponding to the given string or
 * @sbmlconstant{FILL_RULE_INVALID, FillRule_t} if there is no such match.
 */
LIBSBML_EXTERN
FillRule_t
FillRule_fromString(const char* code)
{
  static int size =
    sizeof(SBML_FILL_RULE_STRINGS)/sizeof(SBML_FILL_RULE_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_FILL_RULE_STRINGS[i])
    {
      return (FillRule_t)(i);
    }
  }

  return FILL_RULE_INVALID;
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #FillRule_t is valid.
 */
LIBSBML_EXTERN
int
FillRule_isValid(FillRule_t fr)
{
  int min = FILL_RULE_NONZERO;
  int max = FILL_RULE_INVALID;

  if (fr < min || fr >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #FillRule_t.
 */
LIBSBML_EXTERN
int
FillRule_isValidString(const char* code)
{
  return FillRule_isValid(FillRule_fromString(code));
}


static
const char* SBML_FONT_FAMILY_STRINGS[] =
{
  "serif"
, "sans-serif"
, "monospace"
, "invalid FontFamily"
};


/*
 * Returns the string version of the provided #FontFamily_t enumeration.
 */
LIBSBML_EXTERN
const char*
FontFamily_toString(FontFamily_t ff)
{
  int min = FONT_FAMILY_SERIF;
  int max = FONT_FAMILY_INVALID;

  if (ff < min || ff > max)
  {
    return "(Unknown FontFamily value)";
  }

  return SBML_FONT_FAMILY_STRINGS[ff - min];
}


/*
 * Returns the #FontFamily_t enumeration corresponding to the given string or
 * @sbmlconstant{FONT_FAMILY_INVALID, FontFamily_t} if there is no such match.
 */
LIBSBML_EXTERN
FontFamily_t
FontFamily_fromString(const char* code)
{
  static int size =
    sizeof(SBML_FONT_FAMILY_STRINGS)/sizeof(SBML_FONT_FAMILY_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_FONT_FAMILY_STRINGS[i])
    {
      return (FontFamily_t)(i);
    }
  }

  return FONT_FAMILY_INVALID;
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #FontFamily_t is valid.
 */
LIBSBML_EXTERN
int
FontFamily_isValid(FontFamily_t ff)
{
  int min = FONT_FAMILY_SERIF;
  int max = FONT_FAMILY_INVALID;

  if (ff < min || ff >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #FontFamily_t.
 */
LIBSBML_EXTERN
int
FontFamily_isValidString(const char* code)
{
  return FontFamily_isValid(FontFamily_fromString(code));
}


static
const char* SBML_FONT_WEIGHT_STRINGS[] =
{
  "unset"
, "normal"
, "bold"
, "invalid FontWeight"
};


/*
 * Returns the string version of the provided #FontWeight_t enumeration.
 */
LIBSBML_EXTERN
const char*
FontWeight_toString(FontWeight_t fw)
{
  int min = FONT_WEIGHT_UNSET;
  int max = FONT_WEIGHT_INVALID;

  if (fw < min || fw > max)
  {
    return "(Unknown FontWeight value)";
  }

  return SBML_FONT_WEIGHT_STRINGS[fw - min];
}


/*
 * Returns the #FontWeight_t enumeration corresponding to the given string or
 * @sbmlconstant{FONT_WEIGHT_INVALID, FontWeight_t} if there is no such match.
 */
LIBSBML_EXTERN
FontWeight_t
FontWeight_fromString(const char* code)
{
  static int size =
    sizeof(SBML_FONT_WEIGHT_STRINGS)/sizeof(SBML_FONT_WEIGHT_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_FONT_WEIGHT_STRINGS[i])
    {
      return (FontWeight_t)(i);
    }
  }

  return FONT_WEIGHT_INVALID;
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #FontWeight_t is valid.
 */
LIBSBML_EXTERN
int
FontWeight_isValid(FontWeight_t fw)
{
  int min = FONT_WEIGHT_NORMAL;
  int max = FONT_WEIGHT_INVALID;

  if (fw < min || fw >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #FontWeight_t.
 */
LIBSBML_EXTERN
int
FontWeight_isValidString(const char* code)
{
  return FontWeight_isValid(FontWeight_fromString(code));
}


static
const char* SBML_FONT_STYLE_STRINGS[] =
{
  "unset"
, "normal"
,  "italic"
, "invalid FontStyle"
};


/*
 * Returns the string version of the provided #FontStyle_t enumeration.
 */
LIBSBML_EXTERN
const char*
FontStyle_toString(FontStyle_t fs)
{
  int min = FONT_STYLE_UNSET;
  int max = FONT_STYLE_INVALID;

  if (fs < min || fs > max)
  {
    return "(Unknown FontStyle value)";
  }

  return SBML_FONT_STYLE_STRINGS[fs - min];
}


/*
 * Returns the #FontStyle_t enumeration corresponding to the given string or
 * @sbmlconstant{FONT_STYLE_INVALID, FontStyle_t} if there is no such match.
 */
LIBSBML_EXTERN
FontStyle_t
FontStyle_fromString(const char* code)
{
  static int size =
    sizeof(SBML_FONT_STYLE_STRINGS)/sizeof(SBML_FONT_STYLE_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_FONT_STYLE_STRINGS[i])
    {
      return (FontStyle_t)(i);
    }
  }

  return FONT_STYLE_INVALID;
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #FontStyle_t is valid.
 */
LIBSBML_EXTERN
int
FontStyle_isValid(FontStyle_t fs)
{
  int min = FONT_STYLE_NORMAL;
  int max = FONT_STYLE_INVALID;

  if (fs < min || fs >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #FontStyle_t.
 */
LIBSBML_EXTERN
int
FontStyle_isValidString(const char* code)
{
  return FontStyle_isValid(FontStyle_fromString(code));
}


static
const char* SBML_V_TEXT_ANCHOR_STRINGS[] =
{
  "unset"
, "top"
, "middle"
, "bottom"
, "baseline"
, "invalid VTextAnchor"
};


/*
 * Returns the string version of the provided #VTextAnchor_t enumeration.
 */
LIBSBML_EXTERN
const char*
VTextAnchor_toString(VTextAnchor_t vta)
{
  int min = V_TEXTANCHOR_UNSET;
  int max = V_TEXTANCHOR_INVALID;

  if (vta < min || vta > max)
  {
    return "(Unknown VTextAnchor value)";
  }

  return SBML_V_TEXT_ANCHOR_STRINGS[vta - min];
}


/*
 * Returns the #VTextAnchor_t enumeration corresponding to the given string or
 * @sbmlconstant{V_TEXTANCHOR_INVALID, VTextAnchor_t} if there is no
 * such match.
 */
LIBSBML_EXTERN
VTextAnchor_t
VTextAnchor_fromString(const char* code)
{
  static int size =
    sizeof(SBML_V_TEXT_ANCHOR_STRINGS)/sizeof(SBML_V_TEXT_ANCHOR_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_V_TEXT_ANCHOR_STRINGS[i])
    {
      return (VTextAnchor_t)(i);
    }
  }

  return V_TEXTANCHOR_INVALID;
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #VTextAnchor_t is valid.
 */
LIBSBML_EXTERN
int
VTextAnchor_isValid(VTextAnchor_t vta)
{
  int min = V_TEXTANCHOR_TOP;
  int max = V_TEXTANCHOR_INVALID;

  if (vta < min || vta >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #VTextAnchor_t.
 */
LIBSBML_EXTERN
int
VTextAnchor_isValidString(const char* code)
{
  return VTextAnchor_isValid(VTextAnchor_fromString(code));
}


static
const char* SBML_H_TEXT_ANCHOR_STRINGS[] =
{
  "unset"
, "start"
, "middle"
, "end"
, "invalid HTextAnchor"
};


/*
 * Returns the string version of the provided #HTextAnchor_t enumeration.
 */
LIBSBML_EXTERN
const char*
HTextAnchor_toString(HTextAnchor_t hta)
{
  int min = H_TEXTANCHOR_UNSET;
  int max = H_TEXTANCHOR_INVALID;

  if (hta < min || hta > max)
  {
    return "(Unknown HTextAnchor value)";
  }

  return SBML_H_TEXT_ANCHOR_STRINGS[hta - min];
}


/*
 * Returns the #HTextAnchor_t enumeration corresponding to the given string or
 * @sbmlconstant{H_TEXTANCHOR_INVALID, HTextAnchor_t} if there is no such
 * match.
 */
LIBSBML_EXTERN
HTextAnchor_t
HTextAnchor_fromString(const char* code)
{
  static int size =
    sizeof(SBML_H_TEXT_ANCHOR_STRINGS)/sizeof(SBML_H_TEXT_ANCHOR_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_H_TEXT_ANCHOR_STRINGS[i])
    {
      return (HTextAnchor_t)(i);
    }
  }

  return H_TEXTANCHOR_INVALID;
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #HTextAnchor_t is valid.
 */
LIBSBML_EXTERN
int
HTextAnchor_isValid(HTextAnchor_t hta)
{
  int min = H_TEXTANCHOR_START;
  int max = H_TEXTANCHOR_INVALID;

  if (hta < min || hta >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #HTextAnchor_t.
 */
LIBSBML_EXTERN
int
HTextAnchor_isValidString(const char* code)
{
  return HTextAnchor_isValid(HTextAnchor_fromString(code));
}




LIBSBML_CPP_NAMESPACE_END
