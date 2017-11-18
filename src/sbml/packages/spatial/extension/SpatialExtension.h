/**
 * @file SpatialExtension.h
 * @brief Definition of SpatialExtension.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2017 jointly by the following organizations:
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
 *
 * @class SpatialExtension
 * @sbmlbrief{spatial} Base extension class.
 *
 * @class SpatialPkgNamespaces
 * @sbmlbrief{spatial} SBMLNamespaces extension.
 */


#ifndef SpatialExtension_H__
#define SpatialExtension_H__


#include <sbml/common/extern.h>
#include <sbml/SBMLTypeCodes.h>


#ifdef __cplusplus


#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionNamespaces.h>
#include <sbml/extension/SBMLExtensionRegister.h>

#ifndef SPATIAL_CREATE_NS
#define SPATIAL_CREATE_NS(variable, sbmlns)\
EXTENSION_CREATE_NS(SpatialPkgNamespaces, variable, sbmlns);
#endif

#include <vector>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpatialExtension : public SBMLExtension
{
public:

  /**
   * Returns the nickname of the SBML Level&nbsp;3 package implemented by this
   * libSBML extension.
   *
   * @return the package nickname, as a string.
   *
   * @copydetails doc_note_static_methods
   */
  static const std::string& getPackageName();


  /**
   * Returns the default SBML Level implemented by this libSBML extension.
   *
   * @return the SBML Level, as an unsigned integer.
   *
   * @copydetails doc_note_static_methods
   */
  static unsigned int getDefaultLevel();


  /**
   * Returns the default SBML Version implemented by this libSBML extension.
   *
   * @return the Version within the default SBML Level, as an unsigned integer.
   *
   * @copydetails doc_note_static_methods
   */
  static unsigned int getDefaultVersion();


  /**
   * Returns the default version of the SBML Level&nbsp;3 package implemented
   * by this libSBML extension.
   *
   * @return the default version number of the SBML Level&nbsp;3 package
   * definition, as an unsigned integer.
   *
   * @copydetails doc_note_static_methods
   */
  static unsigned int getDefaultPackageVersion();


  /**
   * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
   * by this libSBML extension.
   *
   * @return the XML namespace, as a string.
   *
   * @copydetails doc_note_static_methods
   */
  static const std::string& getXmlnsL3V1V1();


  /**
   * Creates a new SpatialExtension instance.
   */
  SpatialExtension();


  /**
   * Copy constructor for SpatialExtension.
   *
   * @param orig the SpatialExtension instance to copy.
   */
  SpatialExtension(const SpatialExtension& orig);


  /**
   * Assignment operator for SpatialExtension.
   *
   * @param rhs the SpatialExtension object whose values are to be used as the
   * basis of the assignment.
   */
  SpatialExtension& operator=(const SpatialExtension& rhs);


  /**
   * Creates and returns a deep copy of this SpatialExtension object.
   *
   * @return a (deep) copy of this SpatialExtension object.
   */
  virtual SpatialExtension* clone() const;


  /**
   * Destructor for SpatialExtension.
   */
  virtual ~SpatialExtension();


  /**
   * Returns the name of this SBML Level&nbsp;3 package ("spatial").
   *
   * @return a string representing the name of this package ("spatial").
   */
  virtual const std::string& getName() const;


  /**
   * Returns a string representing the SBML XML namespace of this SBML
   * Level&nbsp;3 package.
   *
   * @param sbmlLevel the level of SBML.
   *
   * @param sbmlVersion the version of SBML.
   *
   * @param pkgVersion the version of this package.
   *
   * @return a string representing the name of this package ("spatial").
   *
   * The namespace URI constructed by this method corresponds to the
   * combination of the Level and Version of SBML, and the Version of the SBML
   * Level&nbsp;3 package. (At the time of this writing, the only SBML Level
   * that supports packages is Level&nbsp;3, so the value of @p sbmlLevel is
   * necessarily always <code>3</code>.)
   */
  virtual const std::string& getURI(unsigned int sbmlLevel,
                                    unsigned int sbmlVersion,
                                    unsigned int pkgVersion) const;


  /**
   * Returns the SBML Level for the given URI of this package.
   *
   * @param uri the string of the URI that represents one of the versions of
   * the "spatial" package.
   *
   * @return the SBML Level for the given URI of this package, or @c 0 if the
   * given URI is invalid.
   */
  virtual unsigned int getLevel(const std::string& uri) const;


  /**
   * Returns the Version within the SBML Level for the given URI of this
   * package.
   *
   * @param uri the string of the URI that represents one of the versions of
   * the "spatial" package.
   *
   * @return the SBML Version within the SBML Level for the given URI of this
   * package, or @c 0 if the given URI is invalid.
   */
  virtual unsigned int getVersion(const std::string& uri) const;


  /**
   * Returns the SBML Level&nbsp;3 package version for the given URI of this
   * package.
   *
   * @param uri the string of the URI that represents one of the versions of
   * the "spatial" package.
   *
   * @return the version of the SBML Level&nbsp;3 package for the given URI of
   * this package, or @c 0 if the given URI is invalid.
   */
  virtual unsigned int getPackageVersion(const std::string& uri) const;


  /**
   * Returns a SpatialPkgNamespaces object.
   *
   * @param uri the string of the URI that represents one of the versions of
   * the "spatial" package.
   *
   * @return SpatialPkgNamespaces object corresponding to the given URI of this
   * package, or @c NULL if the given URI is not defined in the "spatial"
   * package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string& uri)
    const;


  /**
   * Takes a type code of the &ldquo;spatial&rdquo; package and returns a
   * string describing the code.
   *
   * @param typeCode a libSBML type code defined by the libSBML extension
   * implementing support for the SBML Level&nbsp;3 &ldquo;spatial&rdquo;
   * package.
   *
   * @return a text string representing the type code given by @p typeCode. If
   * the type code is unrecognized for this implementation of the libSBML
   * &ldquo;spatial&rdquo; package, the string returned will be <code>"(Unknown
   * SBML Spatial Type)"</code>.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the entry in the error table at this index.
   *
   * @param index an unsigned integer representing the index of the error.
   *
   * @return packageErrorTableEntry object in the SpatialSBMLErrorTable.
   */
  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the index in the error table with the given errorId.
   *
   * @param errorId an unsigned integer representing the errorId of the error.
   *
   * @return unsigned int representing the index in the SpatialSBMLErrorTable
   * corresponding to the errorId given.
   */
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the offset for the errorId range for the "spatial" package.
   *
   * @return unsigned int representing the offset for errors in the
   * SpatialSBMLErrorTable.
   */
  virtual unsigned int getErrorIdOffset() const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Initializes spatial extension by creating an object of this class with the
   * required SBasePlugin derived objects and registering the object to the
   * SBMLExtensionRegistry class
   *
   * This function is automatically invoked when creatingthe following global
   * object in SpatialExtension.cpp
   *
   * static SBMLExtensionRegister<SpatialExtension> spatialExtensionRegistry;
   */
  static void init();

  /** @endcond */


};

/**
 *
 * Required typedef definitions
 *
 * SpatialPkgNamespace is derived from SBMLNamespaces class and used when
 * creating an object of SBase derived classes defined in the spatial package
 *
 * SBMLExtensionNamespaces<SpatialExtension> must be instantiated in
 * SpatialExtension.cpp for DLL
 *
 */
typedef SBMLExtensionNamespaces<SpatialExtension> SpatialPkgNamespaces;


LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_BEGIN


/**
 * @enum SBMLSpatialTypeCode_t
 * @brief SBMLSpatialTypeCode_t Enumeration of possible types in the libSBML
 * &ldquo;spatial&rdquo; package implementation.
 *
 * @copydetails doc_what_are_typecodes
 *
 * @copydetails doc_additional_typecode_details
 */
typedef enum
{
  SBML_SPATIAL_DOMAINTYPE                       =   300  /*!<DomainType */
, SBML_SPATIAL_DOMAIN                           =   301  /*!<Domain */
, SBML_SPATIAL_INTERIORPOINT                    =   302  /*!<InteriorPoint */
, SBML_SPATIAL_BOUNDARY                         =   303  /*!<Boundary */
, SBML_SPATIAL_ADJACENTDOMAINS                  =   304  /*!<AdjacentDomains */
, SBML_SPATIAL_GEOMETRYDEFINITION               =   305  /*!<GeometryDefinition */
, SBML_SPATIAL_COMPARTMENTMAPPING               =   306  /*!<CompartmentMapping */
, SBML_SPATIAL_COORDINATECOMPONENT              =   307  /*!<CoordinateComponent */
, SBML_SPATIAL_SAMPLEDFIELDGEOMETRY             =   308  /*!<SampledFieldGeometry */
, SBML_SPATIAL_SAMPLEDFIELD                     =   309  /*!<SampledField */
, SBML_SPATIAL_SAMPLEDVOLUME                    =   310  /*!<SampledVolume */
, SBML_SPATIAL_ANALYTICGEOMETRY                 =   311  /*!<AnalyticGeometry */
, SBML_SPATIAL_ANALYTICVOLUME                   =   312  /*!<AnalyticVolume */
, SBML_SPATIAL_PARAMETRICGEOMETRY               =   313  /*!<ParametricGeometry */
, SBML_SPATIAL_PARAMETRICOBJECT                 =   314  /*!<ParametricObject */
, SBML_SPATIAL_CSGEOMETRY                       =   315  /*!<CSGeometry */
, SBML_SPATIAL_CSGOBJECT                        =   316  /*!<CSGObject */
, SBML_SPATIAL_CSGNODE                          =   317  /*!<CSGNode */
, SBML_SPATIAL_CSGTRANSFORMATION                =   318  /*!<CSGTransformation */
, SBML_SPATIAL_CSGTRANSLATION                   =   319  /*!<CSGTranslation */
, SBML_SPATIAL_CSGROTATION                      =   320  /*!<CSGRotation */
, SBML_SPATIAL_CSGSCALE                         =   321  /*!<CSGScale */
, SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION     =   322  /*!<CSGHomogeneousTransformation */
, SBML_SPATIAL_TRANSFORMATIONCOMPONENT          =   323  /*!<TransformationComponent */
, SBML_SPATIAL_CSGPRIMITIVE                     =   324  /*!<CSGPrimitive */
, SBML_SPATIAL_CSGSETOPERATOR                   =   325  /*!<CSGSetOperator */
, SBML_SPATIAL_SPATIALSYMBOLREFERENCE           =   326  /*!<SpatialSymbolReference */
, SBML_SPATIAL_DIFFUSIONCOEFFICIENT             =   327  /*!<DiffusionCoefficient */
, SBML_SPATIAL_ADVECTIONCOEFFICIENT             =   328  /*!<AdvectionCoefficient */
, SBML_SPATIAL_BOUNDARYCONDITION                =   329  /*!<BoundaryCondition */
, SBML_SPATIAL_GEOMETRY                         =   330  /*!<Geometry */
, SBML_SPATIAL_MIXEDGEOMETRY                    =   331  /*!<MixedGeometry */
, SBML_SPATIAL_ORDINALMAPPING                   =   332  /*!<OrdinalMapping */
, SBML_SPATIAL_SPATIALPOINTS                    =   333  /*!<SpatialPoints */
} SBMLSpatialTypeCode_t;


typedef enum
{
    BOUNDARYCONDITIONKIND_UNKNOWN  /*!< Unknown BoundaryConditionKind */
  , SPATIAL_BOUNDARYKIND_ROBIN_VALUE_COEFFICIENT /*!< Robin_valueCoefficient */
  , SPATIAL_BOUNDARYKIND_ROBIN_INWARD_NORMAL_GRADIENT_COEFFICIENT /*!< Robin_inwardNormalGradientCoefficient */
  , SPATIAL_BOUNDARYKIND_ROBIN_SUM /*!< Robin_sum */
  , SPATIAL_BOUNDARYKIND_NEUMANN /*!< Neumann */
  , SPATIAL_BOUNDARYKIND_DIRICHLET /*!< Dirichlet */
} BoundaryConditionKind_t;


/**
 */
LIBSBML_EXTERN
const char*
BoundaryConditionKind_toString(BoundaryConditionKind_t bck);


/**
 */
LIBSBML_EXTERN
BoundaryConditionKind_t
BoundaryConditionKind_parse(const char* code);


LIBSBML_EXTERN
BoundaryConditionKind_t
BoundaryConditionKind_fromString(const char* code);


/**
 */
LIBSBML_EXTERN
int
BoundaryConditionKind_isValid(BoundaryConditionKind_t bck);


/**
 */
LIBSBML_EXTERN
int
BoundaryConditionKind_isValidString(const char* code);


typedef enum
{
    COORDINATEKIND_UNKNOWN  /*!< Unknown CoordinateKind */
  , SPATIAL_COORDINATEKIND_CARTESIAN_X /*!< cartesianX */
  , SPATIAL_COORDINATEKIND_CARTESIAN_Y /*!< cartesianY */
  , SPATIAL_COORDINATEKIND_CARTESIAN_Z /*!< cartesianZ */
} CoordinateKind_t;


/**
 */
LIBSBML_EXTERN
const char*
CoordinateKind_toString(CoordinateKind_t ck);


/**
 */
LIBSBML_EXTERN
CoordinateKind_t
CoordinateKind_parse(const char* code);


LIBSBML_EXTERN
CoordinateKind_t
CoordinateKind_fromString(const char* code);


/**
 */
LIBSBML_EXTERN
int
CoordinateKind_isValid(CoordinateKind_t ck);


/**
 */
LIBSBML_EXTERN
int
CoordinateKind_isValidString(const char* code);


typedef enum
{
    DIFFUSIONKIND_UNKNOWN  /*!< Unknown DiffusionKind */
  , SPATIAL_DIFFUSIONKIND_ISOTROPIC /*!< isotropic */
  , SPATIAL_DIFFUSIONKIND_ANISOTROPIC /*!< anisotropic */
  , SPATIAL_DIFFUSIONKIND_TENSOR /*!< tensor */
} DiffusionKind_t;


/**
 */
LIBSBML_EXTERN
const char*
DiffusionKind_toString(DiffusionKind_t dk);


/**
 */
LIBSBML_EXTERN
DiffusionKind_t
DiffusionKind_parse(const char* code);

LIBSBML_EXTERN
DiffusionKind_t
DiffusionKind_fromString(const char* code);


/**
 */
LIBSBML_EXTERN
int
DiffusionKind_isValid(DiffusionKind_t dk);


/**
 */
LIBSBML_EXTERN
int
DiffusionKind_isValidString(const char* code);


typedef enum
{
    FUNCTIONKIND_UNKNOWN  /*!< Unknown FunctionKind */
  , SPATIAL_FUNCTIONKIND_LAYERED /*!< layered */
} FunctionKind_t;


/**
 */
LIBSBML_EXTERN
const char*
FunctionKind_toString(FunctionKind_t fk);


/**
 */
LIBSBML_EXTERN
FunctionKind_t
FunctionKind_fromString(const char* code);


LIBSBML_EXTERN
FunctionKind_t
FunctionKind_parse(const char* code);

/**
 */
LIBSBML_EXTERN
int
FunctionKind_isValid(FunctionKind_t fk);


/**
 */
LIBSBML_EXTERN
int
FunctionKind_isValidString(const char* code);


typedef enum
{
    GEOMETRYKIND_UNKNOWN  /*!< Unknown GeometryKind */
  , SPATIAL_GEOMETRYKIND_CARTESIAN /*!< cartesian */
} GeometryKind_t;


/**
 */
LIBSBML_EXTERN
const char*
GeometryKind_toString(GeometryKind_t gk);


/**
 */
LIBSBML_EXTERN
GeometryKind_t
GeometryKind_parse(const char* code);


LIBSBML_EXTERN
GeometryKind_t
GeometryKind_fromString(const char* code);


/**
 */
LIBSBML_EXTERN
int
GeometryKind_isValid(GeometryKind_t gk);


/**
 */
LIBSBML_EXTERN
int
GeometryKind_isValidString(const char* code);


typedef enum
{
    SETOPERATION_UNKNOWN  /*!< Unknown SetOperation */
  , SPATIAL_SETOPERATION_UNION /*!< union */
  , SPATIAL_SETOPERATION_INTERSECTION /*!< intersection */
  , SPATIAL_SETOPERATION_DIFFERENCE /*!< difference */
} SetOperation_t;


/**
 */
LIBSBML_EXTERN
const char*
SetOperation_toString(SetOperation_t so);


/**
 */
LIBSBML_EXTERN
SetOperation_t
SetOperation_fromString(const char* code);


LIBSBML_EXTERN
SetOperation_t
SetOperation_parse(const char* code);


/**
 */
LIBSBML_EXTERN
int
SetOperation_isValid(SetOperation_t so);


/**
 */
LIBSBML_EXTERN
int
SetOperation_isValidString(const char* code);


typedef enum
{
    INTERPOLATIONKIND_UNKNOWN  /*!< Unknown InterpolationKind */
  , SPATIAL_INTERPOLATIONKIND_NEARESTNEIGHBOR /*!< nearestNeighbor */
  , SPATIAL_INTERPOLATIONKIND_LINEAR /*!< linear */
} InterpolationKind_t;


/**
 */
LIBSBML_EXTERN
const char*
InterpolationKind_toString(InterpolationKind_t ik);


/**
 */
LIBSBML_EXTERN
InterpolationKind_t
InterpolationKind_fromString(const char* code);


LIBSBML_EXTERN
InterpolationKind_t
InterpolationKind_parse(const char* code);


/**
 */
LIBSBML_EXTERN
int
InterpolationKind_isValid(InterpolationKind_t ik);


/**
 */
LIBSBML_EXTERN
int
InterpolationKind_isValidString(const char* code);


typedef enum
{
    POLYGONKIND_UNKNOWN  /*!< Unknown PolygonKind */
  , SPATIAL_POLYGONKIND_TRIANGLE /*!< triangle */
  , SPATIAL_POLYGONKIND_QUADRILATERAL /*!< quadrilateral */
} PolygonKind_t;


/**
 */
LIBSBML_EXTERN
const char*
PolygonKind_toString(PolygonKind_t pk);


/**
 */
LIBSBML_EXTERN
PolygonKind_t
PolygonKind_parse(const char* code);


LIBSBML_EXTERN
PolygonKind_t
PolygonKind_fromString(const char* code);


/**
 */
LIBSBML_EXTERN
int
PolygonKind_isValid(PolygonKind_t pk);


/**
 */
LIBSBML_EXTERN
int
PolygonKind_isValidString(const char* code);


typedef enum
{
    PRIMITIVEKIND_UNKNOWN  /*!< Unknown PrimitiveKind */
  , SPATIAL_PRIMITIVEKIND_SPHERE /*!< sphere */
  , SPATIAL_PRIMITIVEKIND_CUBE /*!< cube */
  , SPATIAL_PRIMITIVEKIND_CYLINDER /*!< cylinder */
  , SPATIAL_PRIMITIVEKIND_CONE /*!< cone */
  , SPATIAL_PRIMITIVEKIND_CIRCLE /*!< circle */
  , SPATIAL_PRIMITIVEKIND_SQUARE /*!< square */
  , SPATIAL_PRIMITIVEKIND_RIGHTTRIANGLE /*!< rightTriangle */
} PrimitiveKind_t;


/**
 */
LIBSBML_EXTERN
const char*
PrimitiveKind_toString(PrimitiveKind_t pk);


/**
 */
LIBSBML_EXTERN
PrimitiveKind_t
PrimitiveKind_parse(const char* code);


LIBSBML_EXTERN
PrimitiveKind_t
PrimitiveKind_fromString(const char* code);


/**
 */
LIBSBML_EXTERN
int
PrimitiveKind_isValid(PrimitiveKind_t pk);


/**
 */
LIBSBML_EXTERN
int
PrimitiveKind_isValidString(const char* code);


typedef enum
{
    DATAKIND_UNKNOWN  /*!< Unknown DataKind */
  , SPATIAL_DATAKIND_DOUBLE /*!< double */
  , SPATIAL_DATAKIND_FLOAT /*!< float */
  , SPATIAL_DATAKIND_UINT8 /*!< uint8 */
  , SPATIAL_DATAKIND_UINT16 /*!< uint16 */
  , SPATIAL_DATAKIND_UINT32 /*!< uint32 */
} DataKind_t;


/**
 */
LIBSBML_EXTERN
const char*
DataKind_toString(DataKind_t dk);


/**
 */
LIBSBML_EXTERN
DataKind_t
DataKind_fromString(const char* code);


/**
 */
LIBSBML_EXTERN
int
DataKind_isValid(DataKind_t dk);


LIBSBML_EXTERN
int
DataKind_isValidString(const char* code);

/**
 */
LIBSBML_EXTERN
DataKind_t
DataKind_parse(const char* code);


typedef enum
{
    COMPRESSIONKIND_UNKNOWN  /*!< Unknown CompressionKind */
  , SPATIAL_COMPRESSIONKIND_UNCOMPRESSED /*!< uncompressed */
  , SPATIAL_COMPRESSIONKIND_DEFLATED /*!< deflated */
} CompressionKind_t;


/**
 */
LIBSBML_EXTERN
const char*
CompressionKind_toString(CompressionKind_t ck);


/**
 */
LIBSBML_EXTERN
CompressionKind_t
CompressionKind_parse(const char* code);


LIBSBML_EXTERN
CompressionKind_t
CompressionKind_fromString(const char* code);


/**
 */
LIBSBML_EXTERN
int
CompressionKind_isValid(CompressionKind_t ck);


/**
 */
LIBSBML_EXTERN
int
CompressionKind_isValidString(const char* code);




LIBSBML_CPP_NAMESPACE_END




#endif /* SpatialExtension_H__ */


