/**
 * @file SpatialExtension.cpp
 * @brief Implementation of SpatialExtension.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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


/**
 *
 * Adds this SpatialExtension to the SBMLExtensionRegistry class
 *
 */
static SBMLExtensionRegister<SpatialExtension> spatialExtensionRegistry;

static
const char* SBML_SPATIAL_TYPECODE_STRINGS[] =
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


/**
 *
 * Instantiate SBMLExtensionNamespaces<SpatialExtension> for DLL
 *
 */
template class LIBSBML_EXTERN SBMLExtensionNamespaces<SpatialExtension>;

/*
 * Creates a new SpatialExtension instance.
 */
SpatialExtension::SpatialExtension()
{
}


/*
 * Copy constructor for SpatialExtension.
 */
SpatialExtension::SpatialExtension(const SpatialExtension& orig)
  : SBMLExtension( orig )
{
}


/*
 * Assignment operator for SpatialExtension.
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
SpatialExtension::clone() const
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
 * Returns a SpatialPkgNamespaces object.
 */
SBMLNamespaces*
SpatialExtension::getSBMLExtensionNamespaces(const std::string& uri) const
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
  if (SBMLExtensionRegistry::getInstance().isRegistered(getPackageName()))
  {
    return;
  }

  SpatialExtension spatialExtension;


  std::vector<std::string> packageURIs;

  packageURIs.push_back(getXmlnsL3V1V1());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint modelExtPoint("core", SBML_MODEL);
  SBaseExtensionPoint compartmentExtPoint("core", SBML_COMPARTMENT);
  SBaseExtensionPoint speciesExtPoint("core", SBML_SPECIES);
  SBaseExtensionPoint parameterExtPoint("core", SBML_PARAMETER);
  SBaseExtensionPoint reactionExtPoint("core", SBML_REACTION);

  SBasePluginCreator<SpatialSBMLDocumentPlugin, SpatialExtension>
    sbmldocPluginCreator(sbmldocExtPoint, packageURIs);
  SBasePluginCreator<SpatialModelPlugin, SpatialExtension>
    modelPluginCreator(modelExtPoint, packageURIs);
  SBasePluginCreator<SpatialCompartmentPlugin, SpatialExtension>
    compartmentPluginCreator(compartmentExtPoint, packageURIs);
  SBasePluginCreator<SpatialSpeciesPlugin, SpatialExtension>
    speciesPluginCreator(speciesExtPoint, packageURIs);
  SBasePluginCreator<SpatialParameterPlugin, SpatialExtension>
    parameterPluginCreator(parameterExtPoint, packageURIs);
  SBasePluginCreator<SpatialReactionPlugin, SpatialExtension>
    reactionPluginCreator(reactionExtPoint, packageURIs);

  spatialExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  spatialExtension.addSBasePluginCreator(&modelPluginCreator);
  spatialExtension.addSBasePluginCreator(&compartmentPluginCreator);
  spatialExtension.addSBasePluginCreator(&speciesPluginCreator);
  spatialExtension.addSBasePluginCreator(&parameterPluginCreator);
  spatialExtension.addSBasePluginCreator(&reactionPluginCreator);

  SBMLExtensionRegistry::getInstance().addExtension(&spatialExtension);
}

/** @endcond */




#endif /* __cplusplus */


static
const char* SBML_BOUNDARY_KIND_STRINGS[] =
{
  "Neumann"
, "Dirichlet"
, "invalid BoundaryKind value"
};


/*
 * Returns the string version of the provided #BoundaryKind_t enumeration.
 */
LIBSBML_EXTERN
const char*
BoundaryKind_toString(BoundaryKind_t bk)
{
  int min = SPATIAL_BOUNDARYKIND_NEUMANN;
  int max = SPATIAL_BOUNDARYKIND_INVALID;

  if (bk < min || bk > max)
  {
    return "(Unknown BoundaryKind value)";
  }

  return SBML_BOUNDARY_KIND_STRINGS[bk - min];
}


/*
 * Returns the #BoundaryKind_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_BOUNDARYKIND_INVALID, BoundaryKind_t} if there is no
 * such match.
 */
LIBSBML_EXTERN
BoundaryKind_t
BoundaryKind_parse(const char* code)
{
  static int size = sizeof(SBML_BOUNDARY_KIND_STRINGS)/sizeof(SBML_BOUNDARY_KIND_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_BOUNDARY_KIND_STRINGS[i])
    {
      return (BoundaryKind_t)(i);
    }
  }

  return SPATIAL_BOUNDARYKIND_INVALID;
}


LIBSBML_EXTERN
BoundaryKind_t
BoundaryKind_fromString(const char* code)
{
  return BoundaryKind_parse(code);
}


/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #BoundaryKind_t is valid.
 */
LIBSBML_EXTERN
int
BoundaryKind_isValid(BoundaryKind_t bk)
{
  int min = SPATIAL_BOUNDARYKIND_NEUMANN;
  int max = SPATIAL_BOUNDARYKIND_INVALID;

  if (bk < min || bk >= max)
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
 * given string is a valid #BoundaryKind_t.
 */
LIBSBML_EXTERN
int
BoundaryKind_isValidString(const char* code)
{
  return BoundaryKind_isValid(BoundaryKind_fromString(code));
}


static
const char* SBML_COORDINATE_KIND_STRINGS[] =
{
  "cartesianX"
, "cartesianY"
, "cartesianZ"
, "invalid CoordinateKind value"
};


/*
 * Returns the string version of the provided #CoordinateKind_t enumeration.
 */
LIBSBML_EXTERN
const char*
CoordinateKind_toString(CoordinateKind_t ck)
{
  int min = SPATIAL_COORDINATEKIND_CARTESIAN_X;
  int max = SPATIAL_COORDINATEKIND_INVALID;

  if (ck < min || ck > max)
  {
    return "(Unknown CoordinateKind value)";
  }

  return SBML_COORDINATE_KIND_STRINGS[ck - min];
}


/*
 * Returns the #CoordinateKind_t enumeration corresponding to the given string
 * or @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t} if there
 * is no such match.
 */
LIBSBML_EXTERN
CoordinateKind_t
CoordinateKind_parse(const char* code)
{
  static int size = sizeof(SBML_COORDINATE_KIND_STRINGS)/sizeof(SBML_COORDINATE_KIND_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_COORDINATE_KIND_STRINGS[i])
    {
      return (CoordinateKind_t)(i);
    }
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #CoordinateKind_t is valid.
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #CoordinateKind_t.
 */
LIBSBML_EXTERN
int
CoordinateKind_isValidString(const char* code)
{
  return CoordinateKind_isValid(CoordinateKind_fromString(code));
}


static
const char* SBML_DIFFUSION_KIND_STRINGS[] =
{
  "isotropic"
, "anisotropic"
, "tensor"
, "invalid DiffusionKind value"
};


/*
 * Returns the string version of the provided #DiffusionKind_t enumeration.
 */
LIBSBML_EXTERN
const char*
DiffusionKind_toString(DiffusionKind_t dk)
{
  int min = SPATIAL_DIFFUSIONKIND_ISOTROPIC;
  int max = SPATIAL_DIFFUSIONKIND_INVALID;

  if (dk < min || dk > max)
  {
    return "(Unknown DiffusionKind value)";
  }

  return SBML_DIFFUSION_KIND_STRINGS[dk - min];
}


/*
 * Returns the #DiffusionKind_t enumeration corresponding to the given string
 * or @sbmlconstant{SPATIAL_DIFFUSIONKIND_INVALID, DiffusionKind_t} if there is
 * no such match.
 */
LIBSBML_EXTERN
DiffusionKind_t
DiffusionKind_parse(const char* code)
{
  static int size = sizeof(SBML_DIFFUSION_KIND_STRINGS)/sizeof(SBML_DIFFUSION_KIND_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_DIFFUSION_KIND_STRINGS[i])
    {
      return (DiffusionKind_t)(i);
    }
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #DiffusionKind_t is valid.
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #DiffusionKind_t.
 */
LIBSBML_EXTERN
int
DiffusionKind_isValidString(const char* code)
{
  return DiffusionKind_isValid(DiffusionKind_fromString(code));
}


static
const char* SBML_FUNCTION_KIND_STRINGS[] =
{
  "layered"
, "invalid FunctionKind value"
};


/*
 * Returns the string version of the provided #FunctionKind_t enumeration.
 */
LIBSBML_EXTERN
const char*
FunctionKind_toString(FunctionKind_t fk)
{
  int min = SPATIAL_FUNCTIONKIND_LAYERED;
  int max = SPATIAL_FUNCTIONKIND_INVALID;

  if (fk < min || fk > max)
  {
    return "(Unknown FunctionKind value)";
  }

  return SBML_FUNCTION_KIND_STRINGS[fk - min];
}


/*
 * Returns the #FunctionKind_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_FUNCTIONKIND_INVALID, FunctionKind_t} if there is no
 * such match.
 */
LIBSBML_EXTERN
FunctionKind_t
FunctionKind_parse(const char* code)
{
  static int size = sizeof(SBML_FUNCTION_KIND_STRINGS)/sizeof(SBML_FUNCTION_KIND_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_FUNCTION_KIND_STRINGS[i])
    {
      return (FunctionKind_t)(i);
    }
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #FunctionKind_t is valid.
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #FunctionKind_t.
 */
LIBSBML_EXTERN
int
FunctionKind_isValidString(const char* code)
{
  return FunctionKind_isValid(FunctionKind_fromString(code));
}


static
const char* SBML_GEOMETRY_KIND_STRINGS[] =
{
  "cartesian"
, "invalid GeometryKind value"
};


/*
 * Returns the string version of the provided #GeometryKind_t enumeration.
 */
LIBSBML_EXTERN
const char*
GeometryKind_toString(GeometryKind_t gk)
{
  int min = SPATIAL_GEOMETRYKIND_CARTESIAN;
  int max = SPATIAL_GEOMETRYKIND_INVALID;

  if (gk < min || gk > max)
  {
    return "(Unknown GeometryKind value)";
  }

  return SBML_GEOMETRY_KIND_STRINGS[gk - min];
}


/*
 * Returns the #GeometryKind_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_GEOMETRYKIND_INVALID, GeometryKind_t} if there is no
 * such match.
 */
LIBSBML_EXTERN
GeometryKind_t
GeometryKind_parse(const char* code)
{
  static int size = sizeof(SBML_GEOMETRY_KIND_STRINGS)/sizeof(SBML_GEOMETRY_KIND_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_GEOMETRY_KIND_STRINGS[i])
    {
      return (GeometryKind_t)(i);
    }
  }

  return SPATIAL_GEOMETRYKIND_INVALID;
}


LIBSBML_EXTERN
GeometryKind_t
GeometryKind_fromString(const char* code)
{
  return GeometryKind_parse(code);
}

/*
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #GeometryKind_t is valid.
 */
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #GeometryKind_t.
 */
LIBSBML_EXTERN
int
GeometryKind_isValidString(const char* code)
{
  return GeometryKind_isValid(GeometryKind_fromString(code));
}


static
const char* SBML_SET_OPERATION_STRINGS[] =
{
  "union"
, "intersection"
, "difference"
, "invalid SetOperation value"
};


/*
 * Returns the string version of the provided #SetOperation_t enumeration.
 */
LIBSBML_EXTERN
const char*
SetOperation_toString(SetOperation_t so)
{
  int min = SPATIAL_SETOPERATION_UNION;
  int max = SPATIAL_SETOPERATION_INVALID;

  if (so < min || so > max)
  {
    return "(Unknown SetOperation value)";
  }

  return SBML_SET_OPERATION_STRINGS[so - min];
}


/*
 * Returns the #SetOperation_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_SETOPERATION_INVALID, SetOperation_t} if there is no
 * such match.
 */
LIBSBML_EXTERN
SetOperation_t
SetOperation_parse(const char* code)
{
  static int size = sizeof(SBML_SET_OPERATION_STRINGS)/sizeof(SBML_SET_OPERATION_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_SET_OPERATION_STRINGS[i])
    {
      return (SetOperation_t)(i);
    }
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #SetOperation_t is valid.
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #SetOperation_t.
 */
LIBSBML_EXTERN
int
SetOperation_isValidString(const char* code)
{
  return SetOperation_isValid(SetOperation_fromString(code));
}


static
const char* SBML_INTERPOLATION_KIND_STRINGS[] =
{
  "nearestNeighbor"
, "linear"
, "invalid InterpolationKind value"
};


/*
 * Returns the string version of the provided #InterpolationKind_t enumeration.
 */
LIBSBML_EXTERN
const char*
InterpolationKind_toString(InterpolationKind_t ik)
{
  int min = SPATIAL_INTERPOLATIONKIND_NEARESTNEIGHBOR;
  int max = SPATIAL_INTERPOLATIONKIND_INVALID;

  if (ik < min || ik > max)
  {
    return "(Unknown InterpolationKind value)";
  }

  return SBML_INTERPOLATION_KIND_STRINGS[ik - min];
}


/*
 * Returns the #InterpolationKind_t enumeration corresponding to the given
 * string or @sbmlconstant{SPATIAL_INTERPOLATIONKIND_INVALID,
 * InterpolationKind_t} if there is no such match.
 */
LIBSBML_EXTERN
InterpolationKind_t
InterpolationKind_parse(const char* code)
{
  static int size = sizeof(SBML_INTERPOLATION_KIND_STRINGS)/sizeof(SBML_INTERPOLATION_KIND_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_INTERPOLATION_KIND_STRINGS[i])
    {
      return (InterpolationKind_t)(i);
    }
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #InterpolationKind_t is valid.
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #InterpolationKind_t.
 */
LIBSBML_EXTERN
int
InterpolationKind_isValidString(const char* code)
{
  return InterpolationKind_isValid(InterpolationKind_fromString(code));
}


static
const char* SBML_POLYGON_KIND_STRINGS[] =
{
  "triangle"
, "invalid PolygonKind value"
};


/*
 * Returns the string version of the provided #PolygonKind_t enumeration.
 */
LIBSBML_EXTERN
const char*
PolygonKind_toString(PolygonKind_t pk)
{
  int min = SPATIAL_POLYGONKIND_TRIANGLE;
  int max = SPATIAL_POLYGONKIND_INVALID;

  if (pk < min || pk > max)
  {
    return "(Unknown PolygonKind value)";
  }

  return SBML_POLYGON_KIND_STRINGS[pk - min];
}


/*
 * Returns the #PolygonKind_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_POLYGONKIND_INVALID, PolygonKind_t} if there is no
 * such match.
 */
LIBSBML_EXTERN
PolygonKind_t
PolygonKind_parse(const char* code)
{
  static int size =
    sizeof(SBML_POLYGON_KIND_STRINGS)/sizeof(SBML_POLYGON_KIND_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_POLYGON_KIND_STRINGS[i])
    {
      return (PolygonKind_t)(i);
    }
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #PolygonKind_t is valid.
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #PolygonKind_t.
 */
LIBSBML_EXTERN
int
PolygonKind_isValidString(const char* code)
{
  return PolygonKind_isValid(PolygonKind_fromString(code));
}


static
const char* SBML_PRIMITIVE_KIND_STRINGS[] =
{
  "sphere"
, "cube"
, "cylinder"
, "cone"
, "circle"
, "square"
, "invalid PrimitiveKind value"
};


/*
 * Returns the string version of the provided #PrimitiveKind_t enumeration.
 */
LIBSBML_EXTERN
const char*
PrimitiveKind_toString(PrimitiveKind_t pk)
{
  int min = SPATIAL_PRIMITIVEKIND_SPHERE;
  int max = SPATIAL_PRIMITIVEKIND_INVALID;

  if (pk < min || pk > max)
  {
    return "(Unknown PrimitiveKind value)";
  }

  return SBML_PRIMITIVE_KIND_STRINGS[pk - min];
}


/*
 * Returns the #PrimitiveKind_t enumeration corresponding to the given string
 * or @sbmlconstant{SPATIAL_PRIMITIVEKIND_INVALID, PrimitiveKind_t} if there is
 * no such match.
 */
LIBSBML_EXTERN
PrimitiveKind_t
PrimitiveKind_parse(const char* code)
{
  static int size = sizeof(SBML_PRIMITIVE_KIND_STRINGS)/sizeof(SBML_PRIMITIVE_KIND_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_PRIMITIVE_KIND_STRINGS[i])
    {
      return (PrimitiveKind_t)(i);
    }
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #PrimitiveKind_t is valid.
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #PrimitiveKind_t.
 */
LIBSBML_EXTERN
int
PrimitiveKind_isValidString(const char* code)
{
  return PrimitiveKind_isValid(PrimitiveKind_fromString(code));
}


static
const char* SBML_DATA_KIND_STRINGS[] =
{
  "double"
, "float"
, "uint8"
, "uint16"
, "uint32"
, "uint"
, "int"
, "invalid DataKind value"
};


/*
 * Returns the string version of the provided #DataKind_t enumeration.
 */
LIBSBML_EXTERN
const char*
DataKind_toString(DataKind_t dk)
{
  int min = SPATIAL_DATAKIND_DOUBLE;
  int max = SPATIAL_DATAKIND_INVALID;

  if (dk < min || dk > max)
  {
    return "(Unknown DataKind value)";
  }

  return SBML_DATA_KIND_STRINGS[dk - min];
}


/*
 * Returns the #DataKind_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_DATAKIND_INVALID, DataKind_t} if there is no such
 * match.
 */
LIBSBML_EXTERN
DataKind_t
DataKind_parse(const char* code)
{
  static int size =
    sizeof(SBML_DATA_KIND_STRINGS)/sizeof(SBML_DATA_KIND_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_DATA_KIND_STRINGS[i])
    {
      return (DataKind_t)(i);
    }
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #DataKind_t is valid.
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #DataKind_t.
 */
LIBSBML_EXTERN
int
DataKind_isValidString(const char* code)
{
  return DataKind_isValid(DataKind_fromString(code));
}


static
const char* SBML_COMPRESSION_KIND_STRINGS[] =
{
  "uncompressed"
, "deflated"
, "invalid CompressionKind value"
};


/*
 * Returns the string version of the provided #CompressionKind_t enumeration.
 */
LIBSBML_EXTERN
const char*
CompressionKind_toString(CompressionKind_t ck)
{
  int min = SPATIAL_COMPRESSIONKIND_UNCOMPRESSED;
  int max = SPATIAL_COMPRESSIONKIND_INVALID;

  if (ck < min || ck > max)
  {
    return "(Unknown CompressionKind value)";
  }

  return SBML_COMPRESSION_KIND_STRINGS[ck - min];
}


/*
 * Returns the #CompressionKind_t enumeration corresponding to the given string
 * or @sbmlconstant{SPATIAL_COMPRESSIONKIND_INVALID, CompressionKind_t} if
 * there is no such match.
 */
LIBSBML_EXTERN
CompressionKind_t
CompressionKind_parse(const char* code)
{
  static int size = sizeof(SBML_COMPRESSION_KIND_STRINGS)/sizeof(SBML_COMPRESSION_KIND_STRINGS[0]);
  std::string type(code);

  for (int i = 0; i < size; i++)
  {
    if (type == SBML_COMPRESSION_KIND_STRINGS[i])
    {
      return (CompressionKind_t)(i);
    }
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
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #CompressionKind_t is valid.
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


