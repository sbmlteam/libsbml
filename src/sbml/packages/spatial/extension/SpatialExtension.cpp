/**
 * @file SpatialExtension.cpp
 * @brief Implementation of SpatialExtension.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/extension/SBasePluginCreator.h>
#include <sbml/extension/SBMLDocumentPlugin.h>

#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/extension/SpatialSBMLDocumentPlugin.h>
#include <sbml/packages/spatial/validator/SpatialSBMLErrorTable.h>
#include <sbml/packages/spatial/extension/SpatialModelPlugin.h>
#include <sbml/packages/spatial/extension/SpatialCompartmentPlugin.h>
#include <sbml/packages/spatial/extension/SpatialSpeciesPlugin.h>
#include <sbml/packages/spatial/extension/SpatialParameterPlugin.h>
#include <sbml/packages/spatial/extension/SpatialReactionPlugin.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Returns the nickname of the SBML Level&nbsp;3 package implemented by this
 * libSBML extension.
 */
const std::string&
SpatialExtension::getPackageName()
{
  static const std::string pkgName = "spatial";
  return pkgName;
}


/*
 * Returns the default SBML Level implemented by this libSBML extension.
 */
unsigned int
SpatialExtension::getDefaultLevel()
{
  return 3;
}


/*
 * Returns the default SBML Version implemented by this libSBML extension.
 */
unsigned int
SpatialExtension::getDefaultVersion()
{
  return 1;
}


/*
 * Returns the default version of the SBML Level&nbsp;3 package implemented by
 * this libSBML extension.
 */
unsigned int
SpatialExtension::getDefaultPackageVersion()
{
  return 1;
}


/*
 * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
 * by this libSBML extension.
 */
const std::string&
SpatialExtension::getXmlnsL3V1V1()
{
  static const std::string xmlns =
    "http://www.sbml.org/sbml/level3/version1/spatial/version1";
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
  , "CSGeometry"
  , "CSGObject"
  , "CSGNode"
  , "CSGTransformation"
  , "CSGTranslation"
  , "CSGRotation"
  , "CSGScale"
  , "CSGHomogeneousTransformation"
  , "TransformationComponent"
  , "CSGPrimitive"
  , "CSGSetOperator"
  , "SpatialSymbolReference"
  , "DiffusionCoefficient"
  , "AdvectionCoefficient"
  , "BoundaryCondition"
  , "Geometry"
  , "MixedGeometry"
  , "OrdinalMapping"
  , "SpatialPoints"
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
 * Creates and returns a deep copy of this SpatialExtension object.
 */
SpatialExtension*
SpatialExtension::clone () const
 {
  return new SpatialExtension(*this);
}


/*
 * Destructor for SpatialExtension.
 */
SpatialExtension::~SpatialExtension()
 {
}


/*
 * Returns the name of this SBML Level&nbsp;3 package ("spatial").
 */
const std::string&
SpatialExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns a string representing the SBML XML namespace of this SBML
 * Level&nbsp;3 package.
 */
const std::string&
SpatialExtension::getURI(unsigned int sbmlLevel,
                         unsigned int sbmlVersion,
                         unsigned int pkgVersion) const
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

  static std::string empty = "";

  return empty;
}


/*
 * Returns the SBML Level for the given URI of this package.
 */
unsigned int
SpatialExtension::getLevel(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 3;
  }

  return 0;
}


/*
 * Returns the Version within the SBML Level for the given URI of this package.
 */
unsigned int
SpatialExtension::getVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns the SBML Level&nbsp;3 package version for the given URI of this
 * package.
 */
unsigned int
SpatialExtension::getPackageVersion(const std::string& uri) const
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
 * Takes a type code of the &ldquo;spatial&rdquo; package and returns a string
 * describing the code.
 */
const char*
SpatialExtension::getStringFromTypeCode(int typeCode) const
{
  int min = SBML_SPATIAL_DOMAINTYPE;
  int max = SBML_SPATIAL_SPATIALPOINTS;

  if (typeCode < min || typeCode > max)
  {
    return "(Unknown SBML Spatial Type)";
  }

  return SBML_SPATIAL_TYPECODE_STRINGS[typeCode - min];
}



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the entry in the error table at this index.
 */
packageErrorTableEntry
SpatialExtension::getErrorTable(unsigned int index) const
{
  return spatialErrorTable[index];
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Return the index in the error table with the given errorId.
 */
unsigned int
SpatialExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize =
    sizeof(spatialErrorTable)/sizeof(spatialErrorTable[0]);
  unsigned int index = 0;

  for (unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == spatialErrorTable[i].code)
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
 * Returns the offset for the errorId range for the "spatial" package.
 */
unsigned int
SpatialExtension::getErrorIdOffset() const
{
  return 1200000;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Initializes spatial extension by creating an object of this class with the
 * required SBasePlugin derived objects and registering the object to the
 * SBMLExtensionRegistry class
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






#endif /* __cplusplus */


static
const char * SBML_BOUNDARYCONDITIONKIND_STRINGS[] = 
{
  "Robin_valueCoefficient"
, "Robin_inwardNormalGradientCoefficient"
, "Robin_sum"
, "Neumann"
, "Dirichlet"
, "invalid BoundaryConditionKind"
};


/*
 * This method takes a type code from the BoundaryConditionKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
BoundaryConditionKind_toString(BoundaryConditionKind_t typeCode)
{
  int min = SPATIAL_BOUNDARYKIND_ROBIN_VALUE_COEFFICIENT;
  int max = SPATIAL_BOUNDARYKIND_INVALID;

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
  return SPATIAL_BOUNDARYKIND_INVALID;
}


LIBSBML_EXTERN
BoundaryConditionKind_t
BoundaryConditionKind_fromString(const char* code)
{
  return BoundaryConditionKind_parse(code);
}


LIBSBML_EXTERN
int
BoundaryConditionKind_isValid(BoundaryConditionKind_t bck)
{
  int min = SPATIAL_BOUNDARYKIND_ROBIN_VALUE_COEFFICIENT;
  int max = SPATIAL_BOUNDARYKIND_INVALID;

  if (bck < min || bck >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 */
LIBSBML_EXTERN
int
BoundaryConditionKind_isValidString(const char* code)
{
  return BoundaryConditionKind_isValid(BoundaryConditionKind_fromString(code));
}
static
const char * SBML_COORDINATEKIND_STRINGS[] = 
{
  "cartesianX"
, "cartesianY"
, "cartesianZ"
, "invalid CoordinateKind"
};


/*
 * This method takes a type code from the CoordinateKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
CoordinateKind_toString(CoordinateKind_t typeCode)
{
  int min = SPATIAL_COORDINATEKIND_CARTESIAN_X;
  int max = SPATIAL_COORDINATEKIND_INVALID;

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
  return SPATIAL_COORDINATEKIND_INVALID;
}


LIBSBML_EXTERN
CoordinateKind_t
CoordinateKind_fromString(const char* code)
{
  return CoordinateKind_parse(code);
}


/*
 */
LIBSBML_EXTERN
int
CoordinateKind_isValid(CoordinateKind_t ck)
{
  int min = SPATIAL_COORDINATEKIND_CARTESIAN_X;
  int max = SPATIAL_COORDINATEKIND_INVALID;

  if (ck < min || ck >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 */
LIBSBML_EXTERN
int
CoordinateKind_isValidString(const char* code)
{
  return CoordinateKind_isValid(CoordinateKind_fromString(code));
}


static
const char * SBML_DIFFUSIONKIND_STRINGS[] = 
{
  "isotropic"
, "anisotropic"
, "tensor"
, "invalid DiffusionKind"
};


/*
 * This method takes a type code from the DiffusionKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
DiffusionKind_toString(DiffusionKind_t typeCode)
{
  int min = SPATIAL_DIFFUSIONKIND_ISOTROPIC;
  int max = SPATIAL_DIFFUSIONKIND_INVALID;

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
  return SPATIAL_DIFFUSIONKIND_INVALID;
}


LIBSBML_EXTERN
DiffusionKind_t
DiffusionKind_fromString(const char* code)
{
  return DiffusionKind_parse(code);
}


/*
 */
LIBSBML_EXTERN
int
DiffusionKind_isValid(DiffusionKind_t dk)
{
  int min = SPATIAL_DIFFUSIONKIND_ISOTROPIC;
  int max = SPATIAL_DIFFUSIONKIND_INVALID;

  if (dk < min || dk >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 */
LIBSBML_EXTERN
int
DiffusionKind_isValidString(const char* code)
{
  return DiffusionKind_isValid(DiffusionKind_fromString(code));
}


static
const char * SBML_FUNCTIONKIND_STRINGS[] = 
{
  "layered"
, "invalid FunctionKind"
};


/*
 * This method takes a type code from the FunctionKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
FunctionKind_toString(FunctionKind_t typeCode)
{
  int min = SPATIAL_FUNCTIONKIND_LAYERED;
  int max = SPATIAL_FUNCTIONKIND_INVALID;

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
  return SPATIAL_FUNCTIONKIND_INVALID;
}


LIBSBML_EXTERN
FunctionKind_t
FunctionKind_fromString(const char* code)
{
  return FunctionKind_parse(code);
}


/*
 */
LIBSBML_EXTERN
int
FunctionKind_isValid(FunctionKind_t fk)
{
  int min = SPATIAL_FUNCTIONKIND_LAYERED;
  int max = SPATIAL_FUNCTIONKIND_INVALID;

  if (fk < min || fk >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 */
LIBSBML_EXTERN
int
FunctionKind_isValidString(const char* code)
{
  return FunctionKind_isValid(FunctionKind_fromString(code));
}
static
const char * SBML_GEOMETRYKIND_STRINGS[] = 
{
  "cartesian"
, "invalid GeometryKind"
};


/*
 * This method takes a type code from the GeometryKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
GeometryKind_toString(GeometryKind_t typeCode)
{
  int min = SPATIAL_GEOMETRYKIND_CARTESIAN;
  int max = SPATIAL_GEOMETRYKIND_INVALID;

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
  return SPATIAL_GEOMETRYKIND_INVALID;
}


LIBSBML_EXTERN
GeometryKind_t
GeometryKind_fromString(const char* code)
{
  return GeometryKind_parse(code);
}


LIBSBML_EXTERN
int
GeometryKind_isValid(GeometryKind_t gk)
{
  int min = SPATIAL_GEOMETRYKIND_CARTESIAN;
  int max = SPATIAL_GEOMETRYKIND_INVALID;

  if (gk < min || gk >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 */
LIBSBML_EXTERN
int
GeometryKind_isValidString(const char* code)
{
  return GeometryKind_isValid(GeometryKind_fromString(code));
}


static
const char * SBML_SETOPERATION_STRINGS[] = 
{
  "union"
, "intersection"
, "relativeComplement"
, "invalid SetOperation"
};


/*
 * This method takes a type code from the SetOperation enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
SetOperation_toString(SetOperation_t typeCode)
{
  int min = SPATIAL_SETOPERATION_UNION;
  int max = SPATIAL_SETOPERATION_INVALID;

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
  return SPATIAL_SETOPERATION_INVALID;
}


LIBSBML_EXTERN
SetOperation_t
SetOperation_fromString(const char* code)
{
  return SetOperation_parse(code);
}


/*
 */
LIBSBML_EXTERN
int
SetOperation_isValid(SetOperation_t so)
{
  int min = SPATIAL_SETOPERATION_UNION;
  int max = SPATIAL_SETOPERATION_INVALID;

  if (so < min || so >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 */
LIBSBML_EXTERN
int
SetOperation_isValidString(const char* code)
{
  return SetOperation_isValid(SetOperation_fromString(code));
}


static
const char * SBML_INTERPOLATIONKIND_STRINGS[] = 
{
  "nearestNeighbor"
, "linear"
, "invalid InterpolationKind"
};


/*
 * This method takes a type code from the InterpolationKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
InterpolationKind_toString(InterpolationKind_t typeCode)
{
  int min = SPATIAL_INTERPOLATIONKIND_NEARESTNEIGHBOR;
  int max = SPATIAL_INTERPOLATIONKIND_INVALID;

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
  return SPATIAL_INTERPOLATIONKIND_INVALID;
}


LIBSBML_EXTERN
InterpolationKind_t
InterpolationKind_fromString(const char* code)
{
  return InterpolationKind_parse(code);
}


/*
 */
LIBSBML_EXTERN
int
InterpolationKind_isValid(InterpolationKind_t ik)
{
  int min = SPATIAL_INTERPOLATIONKIND_NEARESTNEIGHBOR;
  int max = SPATIAL_INTERPOLATIONKIND_INVALID;

  if (ik < min || ik >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 */
LIBSBML_EXTERN
int
InterpolationKind_isValidString(const char* code)
{
  return InterpolationKind_isValid(InterpolationKind_fromString(code));
}


static
const char * SBML_POLYGONKIND_STRINGS[] = 
{
  "triangle"
, "quadrilateral"
, "invalid PolygonKind"
};


/*
 * This method takes a type code from the PolygonKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
PolygonKind_toString(PolygonKind_t typeCode)
{
  int min = SPATIAL_POLYGONKIND_TRIANGLE;
  int max = SPATIAL_POLYGONKIND_INVALID;

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
  return SPATIAL_POLYGONKIND_INVALID;
}


LIBSBML_EXTERN
PolygonKind_t
PolygonKind_fromString(const char* code)
{
  return PolygonKind_parse(code);
}


/*
 */
LIBSBML_EXTERN
int
PolygonKind_isValid(PolygonKind_t pk)
{
  int min = SPATIAL_POLYGONKIND_TRIANGLE;
  int max = SPATIAL_POLYGONKIND_INVALID;

  if (pk < min || pk >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 */
LIBSBML_EXTERN
int
PolygonKind_isValidString(const char* code)
{
  return PolygonKind_isValid(PolygonKind_fromString(code));
}


static
const char * SBML_PRIMITIVEKIND_STRINGS[] = 
{
  "sphere"
, "cube"
, "cylinder"
, "cone"
, "circle"
, "square"
, "rightTriangle"
, "invalid PrimitiveKind"
};


/*
 * Returns the string version of the provided #PrimitiveKind_t enumeration.
 */
LIBSBML_EXTERN
const char *
PrimitiveKind_toString(PrimitiveKind_t typeCode)
{
  int min = SPATIAL_PRIMITIVEKIND_SPHERE;
  int max = SPATIAL_PRIMITIVEKIND_INVALID;

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
  return SPATIAL_PRIMITIVEKIND_INVALID;
}


LIBSBML_EXTERN
PrimitiveKind_t
PrimitiveKind_fromString(const char* code)
{
  return PrimitiveKind_parse(code);
}


/*
 */
LIBSBML_EXTERN
int
PrimitiveKind_isValid(PrimitiveKind_t pk)
{
  int min = SPATIAL_PRIMITIVEKIND_SPHERE;
  int max = SPATIAL_PRIMITIVEKIND_INVALID;

  if (pk < min || pk >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 */
LIBSBML_EXTERN
int
PrimitiveKind_isValidString(const char* code)
{
  return PrimitiveKind_isValid(PrimitiveKind_fromString(code));
}


static
const char * SBML_DATAKIND_STRINGS[] = 
{
  "double"
, "float"
, "uint8"
, "uint16"
, "uint32"
, "invalid DataKind"
};


/*
 * This method takes a type code from the DataKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
DataKind_toString(DataKind_t typeCode)
{
  int min = SPATIAL_DATAKIND_DOUBLE;
  int max = SPATIAL_DATAKIND_INVALID;

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
  return SPATIAL_DATAKIND_INVALID;
}


LIBSBML_EXTERN
DataKind_t
DataKind_fromString(const char* code)
{
  return DataKind_parse(code);
}


/*
 */
LIBSBML_EXTERN
int
DataKind_isValid(DataKind_t dk)
{
  int min = SPATIAL_DATAKIND_DOUBLE;
  int max = SPATIAL_DATAKIND_INVALID;

  if (dk < min || dk >= max)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}


/*
 */
LIBSBML_EXTERN
int
DataKind_isValidString(const char* code)
{
  return DataKind_isValid(DataKind_fromString(code));
}


static
const char * SBML_COMPRESSIONKIND_STRINGS[] = 
{
  "uncompressed"
, "deflated"
, "invalid CompressionKind"
};


/*
 * This method takes a type code from the CompressionKind enum and returns a string representing 
 */
LIBSBML_EXTERN
const char *
CompressionKind_toString(CompressionKind_t typeCode)
{
  int min = SPATIAL_COMPRESSIONKIND_UNCOMPRESSED;
  int max = SPATIAL_COMPRESSIONKIND_INVALID;

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
  return SPATIAL_COMPRESSIONKIND_INVALID;
}


LIBSBML_EXTERN
CompressionKind_t
CompressionKind_fromString(const char* code)
{
  return CompressionKind_parse(code);
}


/*
 */
LIBSBML_EXTERN
int
CompressionKind_isValid(CompressionKind_t ck)
{
  int min = SPATIAL_COMPRESSIONKIND_UNCOMPRESSED;
  int max = SPATIAL_COMPRESSIONKIND_INVALID;

  if (ck < min || ck >= max)
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
 * given string is a valid #CompressionKind_t.
 */
LIBSBML_EXTERN
int
CompressionKind_isValidString(const char* code)
{
  return CompressionKind_isValid(CompressionKind_fromString(code));
}




LIBSBML_CPP_NAMESPACE_END


