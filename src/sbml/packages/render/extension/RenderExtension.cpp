/**
 * @file    RenderExtension.cpp
 * @brief   Implementation of RenderExtension, the core module of the render package.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
RenderExtension::getLevel(const std::string &uri) const
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
RenderExtension::getVersion(const std::string &uri) const
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
RenderExtension::getPackageVersion(const std::string &uri) const
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
RenderExtension::getSBMLExtensionNamespaces(const std::string &uri) const
{
  LayoutPkgNamespaces* pkgns = NULL;
  if ( uri == getXmlnsL3V1V1())
  {
    pkgns = new LayoutPkgNamespaces(3,1,1);    
  }  
  else if ( uri == getXmlnsL2())
  {
    //  
    // (NOTE) This should be harmless but may cause some problem.
    //
    pkgns = new LayoutPkgNamespaces(2);
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


LIBSBML_CPP_NAMESPACE_END

