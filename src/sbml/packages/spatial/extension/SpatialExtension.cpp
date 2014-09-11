/**
 * @file:   SpatialExtension.cpp
 * @brief:  Implementation of the SpatialExtension class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/extension/SBasePluginCreator.h>
#include <sbml/extension/SBMLDocumentPlugin.h>


#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/extension/SpatialModelPlugin.h>
#include <sbml/packages/spatial/extension/SpatialCompartmentPlugin.h>
#include <sbml/packages/spatial/extension/SpatialSpeciesPlugin.h>
#include <sbml/packages/spatial/extension/SpatialParameterPlugin.h>
#include <sbml/packages/spatial/extension/SpatialReactionPlugin.h>
#include <sbml/packages/spatial/extension/SpatialSBMLDocumentPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLErrorTable.h>


#ifdef __cplusplus


#include <iostream>


LIBSBML_CPP_NAMESPACE_BEGIN


/*---------------------------------------------------------------
 *
 * This block is global initialization code which should be automatically
 * executed before invoking main() block.
 *
 */

/*------------------ (START) ----------------------------------*/

/*
 * Returns the package name of this extension.
 */
const std::string&
SpatialExtension::getPackageName ()
{
  static const std::string pkgName = "spatial";
  return pkgName;
}


/*
 * Returns the default SBML Level this extension.
 */
unsigned int
SpatialExtension::getDefaultLevel ()
{
  return 3;
}


/*
 * Returns the default SBML Version this extension.
 */
unsigned int
SpatialExtension::getDefaultVersion ()
{
  return 1;
}


/*
 * Returns the default SBML version this extension.
 */
unsigned int
SpatialExtension::getDefaultPackageVersion ()
{
  return 1;
}


/*
 * XML namespaces of package.
 */
const std::string&
SpatialExtension::getXmlnsL3V1V1 ()
{
  static const std::string xmlns = "http://www.sbml.org/sbml/level3/version1/spatial/version1";
  return xmlns;
}


/*
 * Adds this SpatialExtension object to the SBMLExtensionRegistry class.
 * SpatialExtension::init function is automatically invoked when this
 * object is instantiated
 */
static SBMLExtensionRegister<SpatialExtension> spatialExtensionRegistry;


static
const char * SBML_SPATIAL_TYPECODE_STRINGS[] = 
{
    "DomainType"
  , "Domain"
  , "InteriorPoint"
  , "Boundary"
  , "AdjacentDomains"
  , "GeometryDefinition"
  , "CompartmentMapping"
  , "CoordinateComponent"
  , "SampledFieldGeometry"
  , "SampledField"
  , "SampledVolume"
  , "AnalyticGeometry"
  , "AnalyticVolume"
  , "ParametricGeometry"
  , "ParametricObject"
  , "PolygonObject"
  , "SpatialPoint"
  , "CSGeometry"
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
  , "CoordinateReference"
  , "MixedGeometry"
  , "OrdinalMapping"
};


/*
 * Instantiate SBMLExtensionNamespaces<SpatialExtension>
 * (SpatialPkgNamespaces) for DLL.
 */
template class LIBSBML_EXTERN  SBMLExtensionNamespaces<SpatialExtension>;


/*------------------ (END) ----------------------------------*/

/*
 * Constructor
 */
SpatialExtension::SpatialExtension()
{
}


/*
 * Copy constructor
 */
SpatialExtension::SpatialExtension(const SpatialExtension& orig) :
   SBMLExtension(orig)
{
}


/*
 * Assignment operator
 */
SpatialExtension&
SpatialExtension::operator=(const SpatialExtension& rhs)
 {
  if (&rhs != this)
  {
    SBMLExtension::operator=(rhs);
  }
  return *this;
}


/*
 * Clone
 */
SpatialExtension*
SpatialExtension::clone () const
 {
  return new SpatialExtension(*this);
}


/*
 * Destructor
 */
SpatialExtension::~SpatialExtension()
 {
}


/*
 * Returns the name of this package
 */
const std::string&
SpatialExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns the URI (namespace) of the package
 */
const std::string&
SpatialExtension::getURI(unsigned int sbmlLevel,
                                  unsigned int sbmlVersion,
                                  unsigned int pkgVersion) const
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
 * Returns an SBMLExtensionNamespaces<SpatialExtension> object 
 */
SBMLNamespaces*
SpatialExtension::getSBMLExtensionNamespaces(const std::string &uri) const
{
  SpatialPkgNamespaces* pkgns = NULL;
  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new SpatialPkgNamespaces(3, 1, 1);
  }

  return pkgns;
}


/*
 * This method takes a type code from the Spatial package and returns a string representing 
 */
const char*
SpatialExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_SPATIAL_DOMAINTYPE;
  int max = SBML_SPATIAL_ORDINALMAPPING;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Spatial Type)";
  }

  return SBML_SPATIAL_TYPECODE_STRINGS[typeCode - min];
}


static
const char * SBML_BOUNDARYCONDITIONKIND_STRINGS[] = 
{
   "Unknown BoundaryConditionKind"
 , "Robin_valueCoefficient"
 , "Robin_inwardNormalGradientCoefficient"
 , "Robin_sum"
 , "Neumann"
 , "Dirichlet"
};


/*
 * This method takes a type code from the BoundaryConditionKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
BoundaryConditionKind_toString(BoundaryConditionKind_t typeCode)
{
  int min = BOUNDARYCONDITIONKIND_UNKNOWN;
  int max = SPATIAL_BOUNDARYKIND_DIRICHLET;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown BoundaryConditionKind value)";
  }

  return SBML_BOUNDARYCONDITIONKIND_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a BoundaryConditionKind code to match it
 */
LIBSBML_EXTERN
BoundaryConditionKind_t
BoundaryConditionKind_parse(const char* code)
{
  static const int size = sizeof(SBML_BOUNDARYCONDITIONKIND_STRINGS) / sizeof(SBML_BOUNDARYCONDITIONKIND_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_BOUNDARYCONDITIONKIND_STRINGS[i])
      return (BoundaryConditionKind_t)i;
  }
  return BOUNDARYCONDITIONKIND_UNKNOWN;
}


static
const char * SBML_COORDINATEKIND_STRINGS[] = 
{
   "Unknown CoordinateKind"
 , "cartesianX"
 , "cartesianY"
 , "cartesianZ"
};


/*
 * This method takes a type code from the CoordinateKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
CoordinateKind_toString(CoordinateKind_t typeCode)
{
  int min = COORDINATEKIND_UNKNOWN;
  int max = SPATIAL_COORDINATEKIND_CARTESIAN_Z;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown CoordinateKind value)";
  }

  return SBML_COORDINATEKIND_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a CoordinateKind code to match it
 */
LIBSBML_EXTERN
CoordinateKind_t
CoordinateKind_parse(const char* code)
{
  static const int size = sizeof(SBML_COORDINATEKIND_STRINGS) / sizeof(SBML_COORDINATEKIND_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_COORDINATEKIND_STRINGS[i])
      return (CoordinateKind_t)i;
  }
  return COORDINATEKIND_UNKNOWN;
}


static
const char * SBML_DIFFUSIONKIND_STRINGS[] = 
{
   "Unknown DiffusionKind"
 , "isotropic"
 , "anisotropic"
 , "tensor"
};


/*
 * This method takes a type code from the DiffusionKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
DiffusionKind_toString(DiffusionKind_t typeCode)
{
  int min = DIFFUSIONKIND_UNKNOWN;
  int max = SPATIAL_DIFFUSIONKIND_TENSOR;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown DiffusionKind value)";
  }

  return SBML_DIFFUSIONKIND_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a DiffusionKind code to match it
 */
LIBSBML_EXTERN
DiffusionKind_t
DiffusionKind_parse(const char* code)
{
  static const int size = sizeof(SBML_DIFFUSIONKIND_STRINGS) / sizeof(SBML_DIFFUSIONKIND_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_DIFFUSIONKIND_STRINGS[i])
      return (DiffusionKind_t)i;
  }
  return DIFFUSIONKIND_UNKNOWN;
}


static
const char * SBML_FUNCTIONKIND_STRINGS[] = 
{
   "Unknown FunctionKind"
 , "layered"
};


/*
 * This method takes a type code from the FunctionKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
FunctionKind_toString(FunctionKind_t typeCode)
{
  int min = FUNCTIONKIND_UNKNOWN;
  int max = SPATIAL_FUNCTIONKIND_LAYERED;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown FunctionKind value)";
  }

  return SBML_FUNCTIONKIND_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a FunctionKind code to match it
 */
LIBSBML_EXTERN
FunctionKind_t
FunctionKind_parse(const char* code)
{
  static const int size = sizeof(SBML_FUNCTIONKIND_STRINGS) / sizeof(SBML_FUNCTIONKIND_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_FUNCTIONKIND_STRINGS[i])
      return (FunctionKind_t)i;
  }
  return FUNCTIONKIND_UNKNOWN;
}


static
const char * SBML_GEOMETRYKIND_STRINGS[] = 
{
   "Unknown GeometryKind"
 , "cartesian"
};


/*
 * This method takes a type code from the GeometryKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
GeometryKind_toString(GeometryKind_t typeCode)
{
  int min = GEOMETRYKIND_UNKNOWN;
  int max = SPATIAL_GEOMETRYKIND_CARTESIAN;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown GeometryKind value)";
  }

  return SBML_GEOMETRYKIND_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a GeometryKind code to match it
 */
LIBSBML_EXTERN
GeometryKind_t
GeometryKind_parse(const char* code)
{
  static const int size = sizeof(SBML_GEOMETRYKIND_STRINGS) / sizeof(SBML_GEOMETRYKIND_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_GEOMETRYKIND_STRINGS[i])
      return (GeometryKind_t)i;
  }
  return GEOMETRYKIND_UNKNOWN;
}


static
const char * SBML_SETOPERATION_STRINGS[] = 
{
   "Unknown SetOperation"
 , "union"
 , "intersection"
 , "relativeComplement"
};


/*
 * This method takes a type code from the SetOperation enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
SetOperation_toString(SetOperation_t typeCode)
{
  int min = SETOPERATION_UNKNOWN;
  int max = SPATIAL_SETOPERATION_RELATIVECOMPLEMENT;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown SetOperation value)";
  }

  return SBML_SETOPERATION_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a SetOperation code to match it
 */
LIBSBML_EXTERN
SetOperation_t
SetOperation_parse(const char* code)
{
  static const int size = sizeof(SBML_SETOPERATION_STRINGS) / sizeof(SBML_SETOPERATION_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_SETOPERATION_STRINGS[i])
      return (SetOperation_t)i;
  }
  return SETOPERATION_UNKNOWN;
}


static
const char * SBML_INTERPOLATIONKIND_STRINGS[] = 
{
   "Unknown InterpolationKind"
 , "nearestNeighbor"
 , "linear"
};


/*
 * This method takes a type code from the InterpolationKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
InterpolationKind_toString(InterpolationKind_t typeCode)
{
  int min = INTERPOLATIONKIND_UNKNOWN;
  int max = SPATIAL_INTERPOLATIONKIND_LINEAR;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown InterpolationKind value)";
  }

  return SBML_INTERPOLATIONKIND_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a InterpolationKind code to match it
 */
LIBSBML_EXTERN
InterpolationKind_t
InterpolationKind_parse(const char* code)
{
  static const int size = sizeof(SBML_INTERPOLATIONKIND_STRINGS) / sizeof(SBML_INTERPOLATIONKIND_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_INTERPOLATIONKIND_STRINGS[i])
      return (InterpolationKind_t)i;
  }
  return INTERPOLATIONKIND_UNKNOWN;
}


static
const char * SBML_POLYGONKIND_STRINGS[] = 
{
   "Unknown PolygonKind"
 , "triangle"
 , "quadrilateral"
};


/*
 * This method takes a type code from the PolygonKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
PolygonKind_toString(PolygonKind_t typeCode)
{
  int min = POLYGONKIND_UNKNOWN;
  int max = SPATIAL_POLYGONKIND_QUADRILATERAL;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown PolygonKind value)";
  }

  return SBML_POLYGONKIND_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a PolygonKind code to match it
 */
LIBSBML_EXTERN
PolygonKind_t
PolygonKind_parse(const char* code)
{
  static const int size = sizeof(SBML_POLYGONKIND_STRINGS) / sizeof(SBML_POLYGONKIND_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_POLYGONKIND_STRINGS[i])
      return (PolygonKind_t)i;
  }
  return POLYGONKIND_UNKNOWN;
}


static
const char * SBML_PRIMITIVEKIND_STRINGS[] = 
{
   "Unknown PrimitiveKind"
 , "sphere"
 , "cube"
 , "cylinder"
 , "cone"
 , "circle"
 , "square"
 , "rightTriangle"
};


/*
 * This method takes a type code from the PrimitiveKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
PrimitiveKind_toString(PrimitiveKind_t typeCode)
{
  int min = PRIMITIVEKIND_UNKNOWN;
  int max = SPATIAL_PRIMITIVEKIND_RIGHTTRIANGLE;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown PrimitiveKind value)";
  }

  return SBML_PRIMITIVEKIND_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a PrimitiveKind code to match it
 */
LIBSBML_EXTERN
PrimitiveKind_t
PrimitiveKind_parse(const char* code)
{
  static const int size = sizeof(SBML_PRIMITIVEKIND_STRINGS) / sizeof(SBML_PRIMITIVEKIND_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_PRIMITIVEKIND_STRINGS[i])
      return (PrimitiveKind_t)i;
  }
  return PRIMITIVEKIND_UNKNOWN;
}


static
const char * SBML_DATAKIND_STRINGS[] = 
{
   "Unknown DataKind"
 , "double"
 , "float"
 , "uint8"
 , "uint16"
 , "uint32"
};


/*
 * This method takes a type code from the DataKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
DataKind_toString(DataKind_t typeCode)
{
  int min = DATAKIND_UNKNOWN;
  int max = SPATIAL_DATAKIND_UINT32;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown DataKind value)";
  }

  return SBML_DATAKIND_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a DataKind code to match it
 */
LIBSBML_EXTERN
DataKind_t
DataKind_parse(const char* code)
{
  static const int size = sizeof(SBML_DATAKIND_STRINGS) / sizeof(SBML_DATAKIND_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_DATAKIND_STRINGS[i])
      return (DataKind_t)i;
  }
  return DATAKIND_UNKNOWN;
}


static
const char * SBML_COMPRESSIONKIND_STRINGS[] = 
{
   "Unknown CompressionKind"
 , "uncompressed"
 , "deflated"
};


/*
 * This method takes a type code from the CompressionKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
CompressionKind_toString(CompressionKind_t typeCode)
{
  int min = COMPRESSIONKIND_UNKNOWN;
  int max = SPATIAL_COMPRESSIONKIND_DEFLATED;

  if ( typeCode < min || typeCode > max)
  {
    return "(Unknown CompressionKind value)";
  }

  return SBML_COMPRESSIONKIND_STRINGS[typeCode - min];
}


/*
 * This method takes a string and tries to find a CompressionKind code to match it
 */
LIBSBML_EXTERN
CompressionKind_t
CompressionKind_parse(const char* code)
{
  static const int size = sizeof(SBML_COMPRESSIONKIND_STRINGS) / sizeof(SBML_COMPRESSIONKIND_STRINGS[0]);
  unsigned int i;
  std::string type(code);
  for (i = 0; i < size; ++i)
  {
    if (type == SBML_COMPRESSIONKIND_STRINGS[i])
      return (CompressionKind_t)i;
  }
  return COMPRESSIONKIND_UNKNOWN;
}


/*
 * Initialization function of spatial extension module which is automatically invoked
 * by SBMLExtensionRegister class before main() function invoked. 
 */
void
SpatialExtension::init()
{
  //----------------------------------------------------------------
  //
  // 1. Check if the spatial package has already been registered
  //
  //----------------------------------------------------------------

  if (SBMLExtensionRegistry::getInstance().isRegistered(getPackageName()))
  {
    // do nothing
     return;
  }

  //----------------------------------------------------------------
  //
  // 2. Creates an SBMLExtension derived object
  //
  //----------------------------------------------------------------

  SpatialExtension spatialExtension;

  //----------------------------------------------------------------
  //
  // 3. Creates the SBasePlugins required by this package
  //
  //----------------------------------------------------------------

  std::vector<std::string> packageURIs;
  packageURIs.push_back(getXmlnsL3V1V1());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint modelExtPoint("core", SBML_MODEL);
  SBaseExtensionPoint compartmentExtPoint("core", SBML_COMPARTMENT);
  SBaseExtensionPoint speciesExtPoint("core", SBML_SPECIES);
  SBaseExtensionPoint parameterExtPoint("core", SBML_PARAMETER);
  SBaseExtensionPoint reactionExtPoint("core", SBML_REACTION);

  SBasePluginCreator<SpatialSBMLDocumentPlugin, SpatialExtension> sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<SpatialModelPlugin, SpatialExtension> modelPluginCreator(modelExtPoint, packageURIs);
  SBasePluginCreator<SpatialCompartmentPlugin, SpatialExtension> compartmentPluginCreator(compartmentExtPoint, packageURIs);
  SBasePluginCreator<SpatialSpeciesPlugin, SpatialExtension> speciesPluginCreator(speciesExtPoint, packageURIs);
  SBasePluginCreator<SpatialParameterPlugin, SpatialExtension> parameterPluginCreator(parameterExtPoint, packageURIs);
  SBasePluginCreator<SpatialReactionPlugin, SpatialExtension> reactionPluginCreator(reactionExtPoint, packageURIs);

  //----------------------------------------------------------------
  //
  // 4. Adds the creator objects to the extension
  //
  //----------------------------------------------------------------

  spatialExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  spatialExtension.addSBasePluginCreator(&modelPluginCreator);
  spatialExtension.addSBasePluginCreator(&compartmentPluginCreator);
  spatialExtension.addSBasePluginCreator(&speciesPluginCreator);
  spatialExtension.addSBasePluginCreator(&parameterPluginCreator);
  spatialExtension.addSBasePluginCreator(&reactionPluginCreator);

  //----------------------------------------------------------------
  //
  // 5. Register the object with the registry
  //
  //----------------------------------------------------------------

  int result = SBMLExtensionRegistry::getInstance().addExtension(&spatialExtension);

  if (result != LIBSBML_OPERATION_SUCCESS)
  {
    std::cerr << "[Error] SpatialExtension::init() failed." << std::endl;
  }
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error table entry. 
 */
packageErrorTableEntry
SpatialExtension::getErrorTable(unsigned int index) const
{
  return spatialErrorTable[index];
}

  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error table index for this id. 
 */
unsigned int
SpatialExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize = sizeof(spatialErrorTable)/sizeof(spatialErrorTable[0]);
  unsigned int index = 0;

  for(unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == spatialErrorTable[i].code)
    {
      index = i;
      break;
    }

  }

  return index;
}

  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Return error offset. 
 */
unsigned int
SpatialExtension::getErrorIdOffset() const
{
  return 1200000;
}

  /** @endcond doxygenLibsbmlInternal */




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


