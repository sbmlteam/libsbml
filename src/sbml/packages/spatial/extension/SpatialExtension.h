/**
 * @file:   SpatialExtension.h
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

  //---------------------------------------------------------------
  //
  // Required class methods
  //
  //---------------------------------------------------------------

  /**
   * Returns the package name of this extension.
   */
  static const std::string& getPackageName ();


  /**
   * Returns the default SBML Level this extension.
   */
  static unsigned int getDefaultLevel();


  /**
   * Returns the default SBML Version this extension.
   */
  static unsigned int getDefaultVersion();


  /**
   * Returns the default SBML version this extension.
   */
  static unsigned int getDefaultPackageVersion();


  /**
   * Returns URI of supported versions of this package.
   */
  static const std::string&  getXmlnsL3V1V1();


  //
  // Other URI needed in this package (if any)
  //
  //---------------------------------------------------------------


  /**
   * Creates a new SpatialExtension   */
  SpatialExtension();


  /**
   * Copy constructor for SpatialExtension.
   *
   * @param orig; the SpatialExtension instance to copy.
   */
  SpatialExtension(const SpatialExtension& orig);


   /**
   * Assignment operator for SpatialExtension.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  SpatialExtension& operator=(const SpatialExtension& rhs);


   /**
   * Creates and returns a deep copy of this SpatialExtension object.
   *
   * @return a (deep) copy of this SpatialExtension object.
   */
  virtual SpatialExtension* clone () const;


   /**
   * Destructor for SpatialExtension.
   */
  virtual ~SpatialExtension();


   /**
   * Returns the name of this package ("spatial")
   *
   * @return a string representing the name of this package ("spatial")
   */
  virtual const std::string& getName() const;


  /**
   * Returns the URI (namespace) of the package corresponding to the combination of 
   * the given sbml level, sbml version, and package version.
   * Empty string will be returned if no corresponding URI exists.
   *
   * @param sbmlLevel the level of SBML
   * @param sbmlVersion the version of SBML
   * @param pkgVersion the version of package
   *
   * @return a string of the package URI
   */
  virtual const std::string& getURI(unsigned int sbmlLevel,
                                    unsigned int sbmlVersion,
                                    unsigned int pkgVersion) const;


  /**
   * Returns the SBML level with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of spatial package
   *
   * @return the SBML level with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getLevel(const std::string &uri) const;


  /**
   * Returns the SBML version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of spatial package
   *
   * @return the SBML version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getVersion(const std::string &uri) const;


  /**
   * Returns the package version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of spatial package
   *
   * @return the package version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getPackageVersion(const std::string &uri) const;


  /**
   * Returns an SBMLExtensionNamespaces<SpatialExtension> object whose alias type is 
   * SpatialPkgNamespace.
   * Null will be returned if the given uri is not defined in the spatial package.
   *
   * @param uri the string of URI that represents one of versions of spatial package
   *
   * @return an SpatialPkgNamespace object corresponding to the given uri. NULL will
   * be returned if the given URI is not defined in spatial package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string &uri) const;


  /**
   * This method takes a type code from the Spatial package and returns a string representing 
   * the code.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Initializes spatial extension by creating an object of this class with 
   * required SBasePlugin derived objects and registering the object 
   * to the SBMLExtensionRegistry class.
   *
   * (NOTE) This function is automatically invoked when creating the following
   *        global object in SpatialExtension.cpp
   *
   *        static SBMLExtensionRegister<SpatialExtension> spatialExtensionRegistry;
   *
   */
  static void init();


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the entry in the error table at this index. 
   *
   * @param index an unsigned intgere representing the index of the error in the SpatialSBMLErrorTable
   *
   * @return packageErrorTableEntry object in the SpatialSBMLErrorTable corresponding to the index given.
   */
  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the index in the error table with the given errorId. 
   *
   * @param errorId an unsigned intgere representing the errorId of the error in the SpatialSBMLErrorTable
   *
   * @return unsigned integer representing the index in the SpatialSBMLErrorTable corresponding to the errorId given.
   */
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the offset for the errorId range for the spatial L3 package. 
   *
   * @return unsigned intege representing the  offset for errors SpatialSBMLErrorTable.
   */
  virtual unsigned int getErrorIdOffset() const;


  /** @endcond doxygenLibsbmlInternal */


};


// --------------------------------------------------------------------
//
// Required typedef definitions
//
// SpatialPkgNamespaces is derived from the SBMLNamespaces class and
// used when creating an object of SBase derived classes defined in
// spatial package.
//
// --------------------------------------------------------------------
//
// (NOTE)
//
// SBMLExtensionNamespaces<SpatialExtension> must be instantiated
// in SpatialExtension.cpp for DLL.
//
typedef SBMLExtensionNamespaces<SpatialExtension> SpatialPkgNamespaces;

typedef enum
{
    SBML_SPATIAL_DOMAINTYPE  = 300
  , SBML_SPATIAL_DOMAIN            = 301
  , SBML_SPATIAL_INTERIORPOINT     = 302
  , SBML_SPATIAL_BOUNDARY          = 303
  , SBML_SPATIAL_ADJACENTDOMAINS   = 304
  , SBML_SPATIAL_GEOMETRYDEFINITION = 305
  , SBML_SPATIAL_COMPARTMENTMAPPING = 306
  , SBML_SPATIAL_COORDINATECOMPONENT = 307
  , SBML_SPATIAL_SAMPLEDFIELDGEOMETRY = 308
  , SBML_SPATIAL_SAMPLEDFIELD      = 309
  , SBML_SPATIAL_SAMPLEDVOLUME     = 310
  , SBML_SPATIAL_ANALYTICGEOMETRY  = 311
  , SBML_SPATIAL_ANALYTICVOLUME    = 312
  , SBML_SPATIAL_PARAMETRICGEOMETRY = 313
  , SBML_SPATIAL_PARAMETRICOBJECT  = 314
  , SBML_SPATIAL_POLYGONOBJECT     = 315
  , SBML_SPATIAL_SPATIALPOINT      = 316
  , SBML_SPATIAL_CSGEOMETRY        = 317
  , SBML_SPATIAL_CSGOBJECT         = 318
  , SBML_SPATIAL_CSGNODE           = 319
  , SBML_SPATIAL_CSGTRANSFORMATION = 320
  , SBML_SPATIAL_CSGTRANSLATION    = 321
  , SBML_SPATIAL_CSGROTATION       = 322
  , SBML_SPATIAL_CSGSCALE          = 323
  , SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION = 324
  , SBML_SPATIAL_TRANSFORMATIONCOMPONENTS = 325
  , SBML_SPATIAL_CSGPRIMITIVE      = 326
  , SBML_SPATIAL_CSGPSEUDOPRIMITIVE = 327
  , SBML_SPATIAL_CSGSETOPERATOR    = 328
  , SBML_SPATIAL_SPATIALSYMBOLREFERENCE = 329
  , SBML_SPATIAL_DIFFUSIONCOEFFICIENT = 330
  , SBML_SPATIAL_ADVECTIONCOEFFICIENT = 331
  , SBML_SPATIAL_BOUNDARYCONDITION = 332
  , SBML_SPATIAL_GEOMETRY          = 333
  , SBML_SPATIAL_COORDINATEREFERENCE = 334
  , SBML_SPATIAL_MIXEDGEOMETRY     = 335
  , SBML_SPATIAL_ORDINALMAPPING    = 336
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


LIBSBML_EXTERN
const char *
BoundaryConditionKind_toString(BoundaryConditionKind_t code);


LIBSBML_EXTERN
BoundaryConditionKind_t
BoundaryConditionKind_parse(const char* code);


typedef enum
{
    COORDINATEKIND_UNKNOWN  /*!< Unknown CoordinateKind */
  , SPATIAL_COORDINATEKIND_CARTESIAN_X /*!< cartesianX */
  , SPATIAL_COORDINATEKIND_CARTESIAN_Y /*!< cartesianY */
  , SPATIAL_COORDINATEKIND_CARTESIAN_Z /*!< cartesianZ */
} CoordinateKind_t;


LIBSBML_EXTERN
const char *
CoordinateKind_toString(CoordinateKind_t code);


LIBSBML_EXTERN
CoordinateKind_t
CoordinateKind_parse(const char* code);


typedef enum
{
    DIFFUSIONKIND_UNKNOWN  /*!< Unknown DiffusionKind */
  , SPATIAL_DIFFUSIONKIND_ISOTROPIC /*!< isotropic */
  , SPATIAL_DIFFUSIONKIND_ANISOTROPIC /*!< anisotropic */
  , SPATIAL_DIFFUSIONKIND_TENSOR /*!< tensor */
} DiffusionKind_t;


LIBSBML_EXTERN
const char *
DiffusionKind_toString(DiffusionKind_t code);


LIBSBML_EXTERN
DiffusionKind_t
DiffusionKind_parse(const char* code);


typedef enum
{
    FUNCTIONKIND_UNKNOWN  /*!< Unknown FunctionKind */
  , SPATIAL_FUNCTIONKIND_LAYERED /*!< layered */
} FunctionKind_t;


LIBSBML_EXTERN
const char *
FunctionKind_toString(FunctionKind_t code);


LIBSBML_EXTERN
FunctionKind_t
FunctionKind_parse(const char* code);


typedef enum
{
    GEOMETRYKIND_UNKNOWN  /*!< Unknown GeometryKind */
  , SPATIAL_GEOMETRYKIND_CARTESIAN /*!< cartesian */
} GeometryKind_t;


LIBSBML_EXTERN
const char *
GeometryKind_toString(GeometryKind_t code);


LIBSBML_EXTERN
GeometryKind_t
GeometryKind_parse(const char* code);


typedef enum
{
    SETOPERATION_UNKNOWN  /*!< Unknown SetOperation */
  , SPATIAL_SETOPERATION_UNION /*!< union */
  , SPATIAL_SETOPERATION_INTERSECTION /*!< intersection */
  , SPATIAL_SETOPERATION_RELATIVECOMPLEMENT /*!< relativeComplement */
} SetOperation_t;


LIBSBML_EXTERN
const char *
SetOperation_toString(SetOperation_t code);


LIBSBML_EXTERN
SetOperation_t
SetOperation_parse(const char* code);


typedef enum
{
    INTERPOLATIONKIND_UNKNOWN  /*!< Unknown InterpolationKind */
  , SPATIAL_INTERPOLATIONKIND_NEARESTNEIGHBOR /*!< nearestNeighbor */
  , SPATIAL_INTERPOLATIONKIND_LINEAR /*!< linear */
} InterpolationKind_t;


LIBSBML_EXTERN
const char *
InterpolationKind_toString(InterpolationKind_t code);


LIBSBML_EXTERN
InterpolationKind_t
InterpolationKind_parse(const char* code);


typedef enum
{
    POLYGONKIND_UNKNOWN  /*!< Unknown PolygonKind */
  , SPATIAL_POLYGONKIND_TRIANGLE /*!< triangle */
  , SPATIAL_POLYGONKIND_QUADRILATERAL /*!< quadrilateral */
} PolygonKind_t;


LIBSBML_EXTERN
const char *
PolygonKind_toString(PolygonKind_t code);


LIBSBML_EXTERN
PolygonKind_t
PolygonKind_parse(const char* code);


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


LIBSBML_EXTERN
const char *
PrimitiveKind_toString(PrimitiveKind_t code);


LIBSBML_EXTERN
PrimitiveKind_t
PrimitiveKind_parse(const char* code);


typedef enum
{
    DATAKIND_UNKNOWN  /*!< Unknown DataKind */
  , SPATIAL_DATAKIND_DOUBLE /*!< double */
  , SPATIAL_DATAKIND_FLOAT /*!< float */
  , SPATIAL_DATAKIND_UINT8 /*!< uint8 */
  , SPATIAL_DATAKIND_UINT16 /*!< uint16 */
  , SPATIAL_DATAKIND_UINT32 /*!< uint32 */
} DataKind_t;


LIBSBML_EXTERN
const char *
DataKind_toString(DataKind_t code);


LIBSBML_EXTERN
DataKind_t
DataKind_parse(const char* code);


typedef enum
{
    COMPRESSIONKIND_UNKNOWN  /*!< Unknown CompressionKind */
  , SPATIAL_COMPRESSIONKIND_UNCOMPRESSED /*!< uncompressed */
  , SPATIAL_COMPRESSIONKIND_DEFLATED /*!< deflated */
} CompressionKind_t;


LIBSBML_EXTERN
const char *
CompressionKind_toString(CompressionKind_t code);


LIBSBML_EXTERN
CompressionKind_t
CompressionKind_parse(const char* code);




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* SpatialExtension_H__ */


