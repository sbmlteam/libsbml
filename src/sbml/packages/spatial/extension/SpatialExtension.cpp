/**
 * @file    SpatialExtension.cpp
 * @brief   Implementation of SpatialExtension, the core module of spatial package.
 * @author  
 *
 * $Id: SpatialExtension.cpp 10667 2010-01-16 10:20:44Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/extension/SpatialExtension.cpp $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
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
#include <sbml/extension/SBMLDocumentPlugin.h>

#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/extension/SpatialModelPlugin.h>
#include <sbml/packages/spatial/extension/SpatialParameterPlugin.h>
#include <sbml/packages/spatial/extension/SpatialSpeciesRxnPlugin.h>
#include <sbml/packages/spatial/extension/SpatialCompartmentPlugin.h>

#ifdef __cplusplus

#include <iostream>

LIBSBML_CPP_NAMESPACE_BEGIN

// -------------------------------------------------------------------------
//
// This block is global initialization code which should be automatically 
// executed before invoking main() block.
//
// -------------------------------------------------------------------------

//------------- (START) -----------------------------------

// The name of this package

const std::string& SpatialExtension::getPackageName ()
{
	static const std::string pkgName = "spatial";
	return pkgName;
}

//
// Default SBML level, version, and package version
//
unsigned int SpatialExtension::getDefaultLevel()
{
	return 3;
}  

unsigned int SpatialExtension::getDefaultVersion()
{
	return 1; 
}

unsigned int SpatialExtension::getDefaultPackageVersion()
{
	return 1;
} 

//
// XML namespaces of (1) package versions of spatial extension, and 
// (2) another XML namespace(XMLSchema-instance) required in the spatial 
//  extension.
//

const std::string& SpatialExtension::getXmlnsL3V1V1 ()
{
	static const std::string xmlns = "http://www.sbml.org/sbml/level3/version1/spatial/version1";
	return xmlns;
}

//
// Adds this SpatialExtension object to the SBMLExtensionRegistry class.
// SpatialExtension::init() function is automatically invoked when this
// object is instantiated.
//
static SBMLExtensionRegister<SpatialExtension> spatialExtensionRegistry;

static
const char* SBML_SPATIAL_TYPECODE_STRINGS[] =
{

    "DomainType"
  , "Domain"
  , "InteriorPoint"
  , "CoordinateComponent"
  , "Boundary"
  , "BoundaryMin"
  , "BoundaryMax"
  , "CompartmentMapping"
  , "AdjacentDomains"
  , "GeometryDefinition"
  , "SampledFieldGeometry"
  , "SampledField"
  , "ImageData"
  , "SampledVolume"
  , "AnalyticGeometry"
  , "AnalyticVolume"
  , "ParametricGeometry" 
  , "ParametricObject" 
  , "PolygonObject" 
  , "SpatialPoint"
  , "CSGGeometry"
  , "CSGObject"
  , "CSGNode"
  , "CSGTransformation"
  , "CSGTranslation"
  , "CSGRotation"
  , "CSGScale"
  , "CSGHomogeneousTransformation"
  , "TransformationComponents"
  , "CSGPrimitive"
  , "CSGPseudoPrimitive"
  , "CSGSetOperator"
  , "SpatialSymbolReference"
  , "DiffusionCoefficient"
  , "AdvectionCoefficient"
  , "BoundaryCondition"
  , "Geometry"

  /*  "Geometry"
  , "CoordinateComponent"
  , "Boundary"
  , "DomainType"
  , "CompartmentMapping"
  , "Domain"
  , "InteriorPoint"
  , "SpatialPoint"
  , "AdjacentDomains"
  , "GeometryDefinition"
  , "AnalyticGeometry"
  , "AnalyticVolume"
  , "SampledFieldGeometry"
  , "SampledVolume"
  , "SampledField"
  , "ImageData"
  , "SpatialSymbolReference"
  , "DiffusionCoefficient"
  , "AdvectionCoefficient"
  , "BoundaryCondition"
  */
};

//------------- (END) -----------------------------------

// --------------------------------------------------------
//
// Instantiate SBMLExtensionNamespaces<SpatialExtension>
// (SpatialPkgNamespaces) for DLL.
//
// --------------------------------------------------------

template class LIBSBML_EXTERN SBMLExtensionNamespaces<SpatialExtension>;



SpatialExtension::SpatialExtension ()
{
}


/*
 * Copy constructor.
 */
SpatialExtension::SpatialExtension(const SpatialExtension& orig)
: SBMLExtension(orig)
{
}


/*
 * Destroy this object.
 */
SpatialExtension::~SpatialExtension ()
{
}


/*
 * Assignment operator for SpatialExtension.
 */
SpatialExtension& 
SpatialExtension::operator=(const SpatialExtension& orig)
{
  SBMLExtension::operator=(orig);
  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialExtension object.
 * 
 * @return a (deep) copy of this SBase object
 */
SpatialExtension* 
SpatialExtension::clone () const
{
  return new SpatialExtension(*this);  
}


const std::string&
SpatialExtension::getName() const
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
SpatialExtension::getURI(unsigned int sbmlLevel, unsigned int sbmlVersion, unsigned int pkgVersion) const
{
  if (sbmlLevel == 3)
  {
    if (sbmlVersion == 1)
    {
      if (pkgVersion == 1)
      {
        return getXmlnsL3V1V1();
      }
    }
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
SpatialExtension::getLevel(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 3;
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
SpatialExtension::getVersion(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
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
SpatialExtension::getPackageVersion(const std::string &uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns an SBMLExtensionNamespaces<class SBMLExtensionType> object 
 * (e.g. SBMLExtensionNamespaces<SpatialExtension> whose alias type is 
 * SpatialPkgNamespaces) corresponding to the given uri.
 * NULL will be returned if the given uri is not defined in the corresponding package.
 *
 *  (NOTICE) Package developers MUST OVERRIDE this pure virtual function in their derived class.
 *
 */
SBMLNamespaces*
SpatialExtension::getSBMLExtensionNamespaces(const std::string &uri) const
{
  SpatialPkgNamespaces* pkgns = 0;
  if ( uri == getXmlnsL3V1V1())
  {
    pkgns = new SpatialPkgNamespaces(3,1,1);    
  }  
  return pkgns;
}


/*
 * This method takes a type code of spatial package and returns a string representing
 * the code.
 */
const char* 
SpatialExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_SPATIAL_DOMAINTYPE;
  int max = SBML_SPATIAL_GEOMETRY;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Spatial Type)";  
  }

  return SBML_SPATIAL_TYPECODE_STRINGS[typeCode - min];
}


/*
 *
 * Initialization function of spatial extension module which is automatically invoked 
 * by SBMLExtensionRegister class before main() function invoked.
 *
 */
void 
SpatialExtension::init()
{
  //-------------------------------------------------------------------------
  //
  // 1. Checks if the spatial pacakge has already been registered.
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

  SpatialExtension spatialExtension;

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
  //    required for the spatial extension.
  //
  //    Since only 'required' attribute is used in SBMLDocument by the spatial package, existing
  //    SBMLDocumentPlugin class can be used as-is for the plugin.
  //
  //    Since the lists of supported package versions (currently only L3V1-spatial-V1 supported )
  //    are equal in the both plugin objects, the same vector object is given to each 
  //    constructor.
  //
  //---------------------------------------------------------------------------------------

  std::vector<std::string> packageURIs;
  packageURIs.push_back(getXmlnsL3V1V1());

  SBaseExtensionPoint sbmldocExtPoint("core",SBML_DOCUMENT);
  SBaseExtensionPoint modelExtPoint("core",SBML_MODEL);
  SBaseExtensionPoint compartmentExtPoint("core",SBML_COMPARTMENT);
  SBaseExtensionPoint paramExtPoint("core",SBML_PARAMETER);
  SBaseExtensionPoint speciesExtPoint("core",SBML_SPECIES);
  SBaseExtensionPoint reactionExtPoint("core",SBML_REACTION);

  SBasePluginCreator<SBMLDocumentPlugin, SpatialExtension> sbmldocPluginCreator(sbmldocExtPoint,packageURIs);
  SBasePluginCreator<SpatialModelPlugin, SpatialExtension> modelPluginCreator(modelExtPoint,packageURIs);
  SBasePluginCreator<SpatialCompartmentPlugin, SpatialExtension> compPluginCreator(compartmentExtPoint,packageURIs);
  SBasePluginCreator<SpatialParameterPlugin, SpatialExtension> paramPluginCreator(paramExtPoint,packageURIs);
  SBasePluginCreator<SpatialSpeciesRxnPlugin, SpatialExtension> spPluginCreator(speciesExtPoint,packageURIs);
  SBasePluginCreator<SpatialSpeciesRxnPlugin, SpatialExtension> rxnPluginCreator(reactionExtPoint,packageURIs);

  //--------------------------------------------------------------------------------------
  //
  // 4. Adds the above SBasePluginCreatorBase derived objects to the SBMLExtension derived object.
  //
  //--------------------------------------------------------------------------------------

  spatialExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  spatialExtension.addSBasePluginCreator(&modelPluginCreator);
  spatialExtension.addSBasePluginCreator(&compPluginCreator);
  spatialExtension.addSBasePluginCreator(&paramPluginCreator);
  spatialExtension.addSBasePluginCreator(&spPluginCreator);
  spatialExtension.addSBasePluginCreator(&rxnPluginCreator);

  //-------------------------------------------------------------------------
  //
  // 5. Registers the SBMLExtension derived object to SBMLExtensionRegistry
  //
  //-------------------------------------------------------------------------

  int result = SBMLExtensionRegistry::getInstance().addExtension(&spatialExtension);

  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    std::cerr << "[Error] SpatialExtension::init() failed." << std::endl;
  }
}

#endif  /* __cplusplus */
LIBSBML_CPP_NAMESPACE_END

