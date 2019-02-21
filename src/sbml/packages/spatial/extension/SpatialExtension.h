/**
 * @file SpatialExtension.h
 * @brief Definition of SpatialExtension.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 *
 * @class SpatialExtension
 * @sbmlbrief{spatial} Base extension class for the package.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * This is the Spatial package extension of the SBMLExtension class that is
 * used to facilitate libSBML plug-ins in the implementation of an
 * SBMLLevel&nbsp;3 package.
 *
 * @class SpatialPkgNamespaces
 * @sbmlbrief{spatial} SBMLNamespaces extension.
 *
 * @htmlinclude not-sbml-warning.html
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
   * given URI is invalid, or for a different package.
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
   * package, or @c 0 if the given URI is invalid, or for a different package.
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
   * this package, or @c 0 if the given URI is invalid, or for a different
   * package.
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


/**
 * @enum BoundaryKind_t
 * @brief Enumeration of values permitted as the value of the "boundarykind"
 * attribute on Spatial objects.
 *
 * @if conly
 * @see Spatial_getBoundarykind()
 * @see Spatial_setBoundarykind()
 * @elseif java
 * @see Spatial::getBoundarykind()
 * @see Spatial::setBoundarykind(long)
 * @else
 * @see Spatial::getBoundarykind()
 * @see Spatial::setBoundarykind()
 * @endif
 */
typedef enum
{
  SPATIAL_BOUNDARYKIND_ROBIN_VALUE_COEFFICIENT                        /*!< The spatial boundarykind is @c "Robin_valueCoefficient". */
, SPATIAL_BOUNDARYKIND_ROBIN_INWARD_NORMAL_GRADIENT_COEFFICIENT       /*!< The spatial boundarykind is @c "Robin_inwardNormalGradientCoefficient". */
, SPATIAL_BOUNDARYKIND_ROBIN_SUM                                      /*!< The spatial boundarykind is @c "Robin_sum". */
, SPATIAL_BOUNDARYKIND_NEUMANN                                        /*!< The spatial boundarykind is @c "Neumann". */
, SPATIAL_BOUNDARYKIND_DIRICHLET                                      /*!< The spatial boundarykind is @c "Dirichlet". */
, SPATIAL_BOUNDARYKIND_INVALID                                        /*!< Invalid BoundaryKind value. */
} BoundaryKind_t;


/**
 * Returns the string version of the provided #BoundaryKind_t enumeration.
 *
 * @param bk the #BoundaryKind_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "Robin_valueCoefficient",
 * "Robin_inwardNormalGradientCoefficient",
 * "Robin_sum",
 * "Neumann",
 * "Dirichlet",
 * or @c NULL if the value is @sbmlconstant{SPATIAL_BOUNDARYKIND_INVALID,
 * BoundaryKind_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
const char*
BoundaryKind_toString(BoundaryKind_t bk);


/**
 */
LIBSBML_EXTERN
BoundaryKind_t
BoundaryKind_parse(const char* code);

/**
 * Returns the #BoundaryKind_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_BOUNDARYKIND_INVALID, BoundaryKind_t} if there is no
 * such match.
 *
 * @param code the string to convert to a #BoundaryKind_t.
 *
 * @return the corresponding #BoundaryKind_t or
 * @sbmlconstant{SPATIAL_BOUNDARYKIND_INVALID, BoundaryKind_t} if no match is
 * found.
 *
 * @note The matching is case-sensitive: "Robin_valueCoefficient" will return
 * @sbmlconstant{SPATIAL_BOUNDARYKIND_ROBIN_VALUE_COEFFICIENT, BoundaryKind_t},
 * but "Robin_valueCoefficient" will return
 * @sbmlconstant{SPATIAL_BOUNDARYKIND_INVALID, BoundaryKind_t}.
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
BoundaryKind_t
BoundaryKind_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #BoundaryKind_t is valid.
 *
 * @param bk the #BoundaryKind_t enumeration to query.
 *
 * @return @c 1 (true) if the #BoundaryKind_t is
 * @sbmlconstant{SPATIAL_BOUNDARYKIND_ROBIN_VALUE_COEFFICIENT, BoundaryKind_t},
 * @sbmlconstant{SPATIAL_BOUNDARYKIND_ROBIN_INWARD_NORMAL_GRADIENT_COEFFICIENT,
 * BoundaryKind_t},
 * @sbmlconstant{SPATIAL_BOUNDARYKIND_ROBIN_SUM, BoundaryKind_t},
 * @sbmlconstant{SPATIAL_BOUNDARYKIND_NEUMANN, BoundaryKind_t}, or
 * @sbmlconstant{SPATIAL_BOUNDARYKIND_DIRICHLET, BoundaryKind_t};
 * @c 0 (false) otherwise (including
 * @sbmlconstant{SPATIAL_BOUNDARYKIND_INVALID, BoundaryKind_t}).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
BoundaryKind_isValid(BoundaryKind_t bk);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #BoundaryKind_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "Robin_valueCoefficient",
 * "Robin_inwardNormalGradientCoefficient",
 * "Robin_sum",
 * "Neumann",
 * "Dirichlet", or
 * "invalid BoundaryKind value";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "Robin_valueCoefficient" will return
 * @c 1 (true), but "Robin_valueCoefficient" will return @c 0 (false).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
BoundaryKind_isValidString(const char* code);


/**
 * @enum CoordinateKind_t
 * @brief Enumeration of values permitted as the value of the "coordinatekind"
 * attribute on Spatial objects.
 *
 * @if conly
 * @see Spatial_getCoordinatekind()
 * @see Spatial_setCoordinatekind()
 * @elseif java
 * @see Spatial::getCoordinatekind()
 * @see Spatial::setCoordinatekind(long)
 * @else
 * @see Spatial::getCoordinatekind()
 * @see Spatial::setCoordinatekind()
 * @endif
 */
typedef enum
{
  SPATIAL_COORDINATEKIND_CARTESIAN_X       /*!< The spatial coordinatekind is @c "cartesianX". */
, SPATIAL_COORDINATEKIND_CARTESIAN_Y       /*!< The spatial coordinatekind is @c "cartesianY". */
, SPATIAL_COORDINATEKIND_CARTESIAN_Z       /*!< The spatial coordinatekind is @c "cartesianZ". */
, SPATIAL_COORDINATEKIND_INVALID           /*!< Invalid CoordinateKind value. */
} CoordinateKind_t;


/**
 * Returns the string version of the provided #CoordinateKind_t enumeration.
 *
 * @param ck the #CoordinateKind_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "cartesianX",
 * "cartesianY",
 * "cartesianZ",
 * "invalid CoordinateKind value",
 * or @c NULL if the value is @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID,
 * CoordinateKind_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
const char*
CoordinateKind_toString(CoordinateKind_t ck);


/**
 */
LIBSBML_EXTERN
CoordinateKind_t
CoordinateKind_parse(const char* code);


/**
 * Returns the #CoordinateKind_t enumeration corresponding to the given string
 * or @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t} if there
 * is no such match.
 *
 * @param code the string to convert to a #CoordinateKind_t.
 *
 * @return the corresponding #CoordinateKind_t or
 * @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t} if no match
 * is found.
 *
 * @note The matching is case-sensitive: "cartesianX" will return
 * @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_X, CoordinateKind_t}, but
 * "CartesianX" will return @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID,
 * CoordinateKind_t}.
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
CoordinateKind_t
CoordinateKind_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #CoordinateKind_t is valid.
 *
 * @param ck the #CoordinateKind_t enumeration to query.
 *
 * @return @c 1 (true) if the #CoordinateKind_t is
 * @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_X, CoordinateKind_t},
 * @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Y, CoordinateKind_t}, or
 * @sbmlconstant{SPATIAL_COORDINATEKIND_CARTESIAN_Z, CoordinateKind_t};
 * @c 0 (false) otherwise (including
 * @sbmlconstant{SPATIAL_COORDINATEKIND_INVALID, CoordinateKind_t}).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
CoordinateKind_isValid(CoordinateKind_t ck);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #CoordinateKind_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "cartesianX",
 * "cartesianY", or
 * "cartesianZ";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "cartesianX" will return @c 1 (true),
 * but "CartesianX" will return @c 0 (false).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
CoordinateKind_isValidString(const char* code);


/**
 * @enum DiffusionKind_t
 * @brief Enumeration of values permitted as the value of the "diffusionkind"
 * attribute on Spatial objects.
 *
 * @if conly
 * @see Spatial_getDiffusionkind()
 * @see Spatial_setDiffusionkind()
 * @elseif java
 * @see Spatial::getDiffusionkind()
 * @see Spatial::setDiffusionkind(long)
 * @else
 * @see Spatial::getDiffusionkind()
 * @see Spatial::setDiffusionkind()
 * @endif
 */
typedef enum
{
  SPATIAL_DIFFUSIONKIND_ISOTROPIC         /*!< The spatial diffusionkind is @c "isotropic". */
, SPATIAL_DIFFUSIONKIND_ANISOTROPIC       /*!< The spatial diffusionkind is @c "anisotropic". */
, SPATIAL_DIFFUSIONKIND_TENSOR            /*!< The spatial diffusionkind is @c "tensor". */
, SPATIAL_DIFFUSIONKIND_INVALID           /*!< Invalid DiffusionKind value. */
} DiffusionKind_t;


/**
 * Returns the string version of the provided #DiffusionKind_t enumeration.
 *
 * @param dk the #DiffusionKind_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "isotropic",
 * "anisotropic",
 * "tensor",
 * or @c NULL if the value is @sbmlconstant{SPATIAL_DIFFUSIONKIND_INVALID,
 * DiffusionKind_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
const char*
DiffusionKind_toString(DiffusionKind_t dk);


/**
 */
LIBSBML_EXTERN
DiffusionKind_t
DiffusionKind_parse(const char* code);

/**
 * Returns the #DiffusionKind_t enumeration corresponding to the given string
 * or @sbmlconstant{SPATIAL_DIFFUSIONKIND_INVALID, DiffusionKind_t} if there is
 * no such match.
 *
 * @param code the string to convert to a #DiffusionKind_t.
 *
 * @return the corresponding #DiffusionKind_t or
 * @sbmlconstant{SPATIAL_DIFFUSIONKIND_INVALID, DiffusionKind_t} if no match is
 * found.
 *
 * @note The matching is case-sensitive: "isotropic" will return
 * @sbmlconstant{SPATIAL_DIFFUSIONKIND_ISOTROPIC, DiffusionKind_t}, but
 * "Isotropic" will return @sbmlconstant{SPATIAL_DIFFUSIONKIND_INVALID,
 * DiffusionKind_t}.
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
DiffusionKind_t
DiffusionKind_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #DiffusionKind_t is valid.
 *
 * @param dk the #DiffusionKind_t enumeration to query.
 *
 * @return @c 1 (true) if the #DiffusionKind_t is
 * @sbmlconstant{SPATIAL_DIFFUSIONKIND_ISOTROPIC, DiffusionKind_t},
 * @sbmlconstant{SPATIAL_DIFFUSIONKIND_ANISOTROPIC, DiffusionKind_t}, or
 * @sbmlconstant{SPATIAL_DIFFUSIONKIND_TENSOR, DiffusionKind_t};
 * @c 0 (false) otherwise (including
 * @sbmlconstant{SPATIAL_DIFFUSIONKIND_INVALID, DiffusionKind_t}).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
DiffusionKind_isValid(DiffusionKind_t dk);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #DiffusionKind_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "isotropic",
 * "anisotropic", or
 * "tensor";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "isotropic" will return @c 1 (true),
 * but "Isotropic" will return @c 0 (false).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
DiffusionKind_isValidString(const char* code);


/**
 * @enum FunctionKind_t
 * @brief Enumeration of values permitted as the value of the "functionkind"
 * attribute on Spatial objects.
 *
 * @if conly
 * @see Spatial_getFunctionkind()
 * @see Spatial_setFunctionkind()
 * @elseif java
 * @see Spatial::getFunctionkind()
 * @see Spatial::setFunctionkind(long)
 * @else
 * @see Spatial::getFunctionkind()
 * @see Spatial::setFunctionkind()
 * @endif
 */
typedef enum
{
  SPATIAL_FUNCTIONKIND_LAYERED       /*!< The spatial functionkind is @c "layered". */
, SPATIAL_FUNCTIONKIND_INVALID       /*!< Invalid FunctionKind value. */
} FunctionKind_t;


/**
 * Returns the string version of the provided #FunctionKind_t enumeration.
 *
 * @param fk the #FunctionKind_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "layered",
 * or @c NULL if the value is @sbmlconstant{SPATIAL_FUNCTIONKIND_INVALID,
 * FunctionKind_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
const char*
FunctionKind_toString(FunctionKind_t fk);


/**
 * Returns the #FunctionKind_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_FUNCTIONKIND_INVALID, FunctionKind_t} if there is no
 * such match.
 *
 * @param code the string to convert to a #FunctionKind_t.
 *
 * @return the corresponding #FunctionKind_t or
 * @sbmlconstant{SPATIAL_FUNCTIONKIND_INVALID, FunctionKind_t} if no match is
 * found.
 *
 * @note The matching is case-sensitive: "layered" will return
 * @sbmlconstant{SPATIAL_FUNCTIONKIND_LAYERED, FunctionKind_t}, but "Layered"
 * will return @sbmlconstant{SPATIAL_FUNCTIONKIND_INVALID, FunctionKind_t}.
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
FunctionKind_t
FunctionKind_fromString(const char* code);


LIBSBML_EXTERN
FunctionKind_t
FunctionKind_parse(const char* code);

/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #FunctionKind_t is valid.
 *
 * @param fk the #FunctionKind_t enumeration to query.
 *
 * @return @c 1 (true) if the #FunctionKind_t is
 * @sbmlconstant{SPATIAL_FUNCTIONKIND_LAYERED, FunctionKind_t};
 * @c 0 (false) otherwise (including
 * @sbmlconstant{SPATIAL_FUNCTIONKIND_INVALID, FunctionKind_t}).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
FunctionKind_isValid(FunctionKind_t fk);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #FunctionKind_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "layered";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "layered" will return @c 1 (true), but
 * "Layered" will return @c 0 (false).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
FunctionKind_isValidString(const char* code);


/**
 * @enum GeometryKind_t
 * @brief Enumeration of values permitted as the value of the "geometrykind"
 * attribute on Spatial objects.
 *
 * @if conly
 * @see Spatial_getGeometrykind()
 * @see Spatial_setGeometrykind()
 * @elseif java
 * @see Spatial::getGeometrykind()
 * @see Spatial::setGeometrykind(long)
 * @else
 * @see Spatial::getGeometrykind()
 * @see Spatial::setGeometrykind()
 * @endif
 */
typedef enum
{
  SPATIAL_GEOMETRYKIND_CARTESIAN       /*!< The spatial geometrykind is @c "cartesian". */
, SPATIAL_GEOMETRYKIND_INVALID         /*!< Invalid GeometryKind value. */
} GeometryKind_t;


/**
 * Returns the string version of the provided #GeometryKind_t enumeration.
 *
 * @param gk the #GeometryKind_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "cartesian",
 * or @c NULL if the value is @sbmlconstant{SPATIAL_GEOMETRYKIND_INVALID,
 * GeometryKind_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
const char*
GeometryKind_toString(GeometryKind_t gk);


/**
 */
LIBSBML_EXTERN
GeometryKind_t
GeometryKind_parse(const char* code);


/**
 * Returns the #GeometryKind_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_GEOMETRYKIND_INVALID, GeometryKind_t} if there is no
 * such match.
 *
 * @param code the string to convert to a #GeometryKind_t.
 *
 * @return the corresponding #GeometryKind_t or
 * @sbmlconstant{SPATIAL_GEOMETRYKIND_INVALID, GeometryKind_t} if no match is
 * found.
 *
 * @note The matching is case-sensitive: "cartesian" will return
 * @sbmlconstant{SPATIAL_GEOMETRYKIND_CARTESIAN, GeometryKind_t}, but
 * "Cartesian" will return @sbmlconstant{SPATIAL_GEOMETRYKIND_INVALID,
 * GeometryKind_t}.
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
GeometryKind_t
GeometryKind_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #GeometryKind_t is valid.
 *
 * @param gk the #GeometryKind_t enumeration to query.
 *
 * @return @c 1 (true) if the #GeometryKind_t is
 * @sbmlconstant{SPATIAL_GEOMETRYKIND_CARTESIAN, GeometryKind_t};
 * @c 0 (false) otherwise (including
 * @sbmlconstant{SPATIAL_GEOMETRYKIND_INVALID, GeometryKind_t}).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
GeometryKind_isValid(GeometryKind_t gk);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #GeometryKind_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "cartesian";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "cartesian" will return @c 1 (true),
 * but "Cartesian" will return @c 0 (false).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
GeometryKind_isValidString(const char* code);


/**
 * @enum SetOperation_t
 * @brief Enumeration of values permitted as the value of the "setoperation"
 * attribute on Spatial objects.
 *
 * @if conly
 * @see Spatial_getSetoperation()
 * @see Spatial_setSetoperation()
 * @elseif java
 * @see Spatial::getSetoperation()
 * @see Spatial::setSetoperation(long)
 * @else
 * @see Spatial::getSetoperation()
 * @see Spatial::setSetoperation()
 * @endif
 */
typedef enum
{
  SPATIAL_SETOPERATION_UNION              /*!< The spatial setoperation is @c "union". */
, SPATIAL_SETOPERATION_INTERSECTION       /*!< The spatial setoperation is @c "intersection". */
, SPATIAL_SETOPERATION_DIFFERENCE         /*!< The spatial setoperation is @c "difference". */
, SPATIAL_SETOPERATION_INVALID            /*!< Invalid SetOperation value. */
} SetOperation_t;


/**
 * Returns the string version of the provided #SetOperation_t enumeration.
 *
 * @param so the #SetOperation_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "union",
 * "intersection",
 * "difference",
 * or @c NULL if the value is @sbmlconstant{SPATIAL_SETOPERATION_INVALID,
 * SetOperation_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
const char*
SetOperation_toString(SetOperation_t so);


/**
 * Returns the #SetOperation_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_SETOPERATION_INVALID, SetOperation_t} if there is no
 * such match.
 *
 * @param code the string to convert to a #SetOperation_t.
 *
 * @return the corresponding #SetOperation_t or
 * @sbmlconstant{SPATIAL_SETOPERATION_INVALID, SetOperation_t} if no match is
 * found.
 *
 * @note The matching is case-sensitive: "union" will return
 * @sbmlconstant{SPATIAL_SETOPERATION_UNION, SetOperation_t}, but "Union" will
 * return @sbmlconstant{SPATIAL_SETOPERATION_INVALID, SetOperation_t}.
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
SetOperation_t
SetOperation_fromString(const char* code);


LIBSBML_EXTERN
SetOperation_t
SetOperation_parse(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #SetOperation_t is valid.
 *
 * @param so the #SetOperation_t enumeration to query.
 *
 * @return @c 1 (true) if the #SetOperation_t is
 * @sbmlconstant{SPATIAL_SETOPERATION_UNION, SetOperation_t},
 * @sbmlconstant{SPATIAL_SETOPERATION_INTERSECTION, SetOperation_t}, or
 * @sbmlconstant{SPATIAL_SETOPERATION_DIFFERENCE, SetOperation_t};
 * @c 0 (false) otherwise (including
 * @sbmlconstant{SPATIAL_SETOPERATION_INVALID, SetOperation_t}).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
SetOperation_isValid(SetOperation_t so);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #SetOperation_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "union",
 * "intersection",
 * "difference", or
 * "invalid SetOperation value";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "union" will return @c 1 (true), but
 * "Union" will return @c 0 (false).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
SetOperation_isValidString(const char* code);


/**
 * @enum InterpolationKind_t
 * @brief Enumeration of values permitted as the value of the
 * "interpolationkind" attribute on Spatial objects.
 *
 * @if conly
 * @see Spatial_getInterpolationkind()
 * @see Spatial_setInterpolationkind()
 * @elseif java
 * @see Spatial::getInterpolationkind()
 * @see Spatial::setInterpolationkind(long)
 * @else
 * @see Spatial::getInterpolationkind()
 * @see Spatial::setInterpolationkind()
 * @endif
 */
typedef enum
{
  SPATIAL_INTERPOLATIONKIND_NEARESTNEIGHBOR       /*!< The spatial interpolationkind is @c "nearestNeighbor". */
, SPATIAL_INTERPOLATIONKIND_LINEAR                /*!< The spatial interpolationkind is @c "linear". */
, SPATIAL_INTERPOLATIONKIND_INVALID               /*!< Invalid InterpolationKind value. */
} InterpolationKind_t;


/**
 * Returns the string version of the provided #InterpolationKind_t enumeration.
 *
 * @param ik the #InterpolationKind_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "nearestNeighbor",
 * "linear",
 * or @c NULL if the value is @sbmlconstant{SPATIAL_INTERPOLATIONKIND_INVALID,
 * InterpolationKind_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
const char*
InterpolationKind_toString(InterpolationKind_t ik);


/**
 * Returns the #InterpolationKind_t enumeration corresponding to the given
 * string or @sbmlconstant{SPATIAL_INTERPOLATIONKIND_INVALID,
 * InterpolationKind_t} if there is no such match.
 *
 * @param code the string to convert to a #InterpolationKind_t.
 *
 * @return the corresponding #InterpolationKind_t or
 * @sbmlconstant{SPATIAL_INTERPOLATIONKIND_INVALID, InterpolationKind_t} if no
 * match is found.
 *
 * @note The matching is case-sensitive: "nearestNeighbor" will return
 * @sbmlconstant{SPATIAL_INTERPOLATIONKIND_NEARESTNEIGHBOR,
 * InterpolationKind_t}, but "NearestNeighbor" will return
 * @sbmlconstant{SPATIAL_INTERPOLATIONKIND_INVALID, InterpolationKind_t}.
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
InterpolationKind_t
InterpolationKind_fromString(const char* code);


LIBSBML_EXTERN
InterpolationKind_t
InterpolationKind_parse(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #InterpolationKind_t is valid.
 *
 * @param ik the #InterpolationKind_t enumeration to query.
 *
 * @return @c 1 (true) if the #InterpolationKind_t is
 * @sbmlconstant{SPATIAL_INTERPOLATIONKIND_NEARESTNEIGHBOR,
 * InterpolationKind_t}, or
 * @sbmlconstant{SPATIAL_INTERPOLATIONKIND_LINEAR, InterpolationKind_t};
 * @c 0 (false) otherwise (including
 * @sbmlconstant{SPATIAL_INTERPOLATIONKIND_INVALID, InterpolationKind_t}).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
InterpolationKind_isValid(InterpolationKind_t ik);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #InterpolationKind_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "nearestNeighbor", or
 * "linear";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "nearestNeighbor" will return @c 1
 * (true), but "NearestNeighbor" will return @c 0 (false).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
InterpolationKind_isValidString(const char* code);


/**
 * @enum PolygonKind_t
 * @brief Enumeration of values permitted as the value of the "polygonkind"
 * attribute on Spatial objects.
 *
 * @if conly
 * @see Spatial_getPolygonkind()
 * @see Spatial_setPolygonkind()
 * @elseif java
 * @see Spatial::getPolygonkind()
 * @see Spatial::setPolygonkind(long)
 * @else
 * @see Spatial::getPolygonkind()
 * @see Spatial::setPolygonkind()
 * @endif
 */
typedef enum
{
  SPATIAL_POLYGONKIND_TRIANGLE            /*!< The spatial polygonkind is @c "triangle". */
, SPATIAL_POLYGONKIND_QUADRILATERAL       /*!< The spatial polygonkind is @c "quadrilateral". */
, SPATIAL_POLYGONKIND_INVALID             /*!< Invalid PolygonKind value. */
} PolygonKind_t;


/**
 * Returns the string version of the provided #PolygonKind_t enumeration.
 *
 * @param pk the #PolygonKind_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "triangle",
 * "quadrilateral",
 * or @c NULL if the value is @sbmlconstant{SPATIAL_POLYGONKIND_INVALID,
 * PolygonKind_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
const char*
PolygonKind_toString(PolygonKind_t pk);


/**
 */
LIBSBML_EXTERN
PolygonKind_t
PolygonKind_parse(const char* code);


/**
 * Returns the #PolygonKind_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_POLYGONKIND_INVALID, PolygonKind_t} if there is no
 * such match.
 *
 * @param code the string to convert to a #PolygonKind_t.
 *
 * @return the corresponding #PolygonKind_t or
 * @sbmlconstant{SPATIAL_POLYGONKIND_INVALID, PolygonKind_t} if no match is
 * found.
 *
 * @note The matching is case-sensitive: "triangle" will return
 * @sbmlconstant{SPATIAL_POLYGONKIND_TRIANGLE, PolygonKind_t}, but "Triangle"
 * will return @sbmlconstant{SPATIAL_POLYGONKIND_INVALID, PolygonKind_t}.
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
PolygonKind_t
PolygonKind_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #PolygonKind_t is valid.
 *
 * @param pk the #PolygonKind_t enumeration to query.
 *
 * @return @c 1 (true) if the #PolygonKind_t is
 * @sbmlconstant{SPATIAL_POLYGONKIND_TRIANGLE, PolygonKind_t}, or
 * @sbmlconstant{SPATIAL_POLYGONKIND_QUADRILATERAL, PolygonKind_t};
 * @c 0 (false) otherwise (including @sbmlconstant{SPATIAL_POLYGONKIND_INVALID,
 * PolygonKind_t}).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
PolygonKind_isValid(PolygonKind_t pk);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #PolygonKind_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "triangle", or
 * "quadrilateral";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "triangle" will return @c 1 (true),
 * but "Triangle" will return @c 0 (false).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
PolygonKind_isValidString(const char* code);


/**
 * @enum PrimitiveKind_t
 * @brief Enumeration of values permitted as the value of the "primitivekind"
 * attribute on Spatial objects.
 *
 * @if conly
 * @see Spatial_getPrimitivekind()
 * @see Spatial_setPrimitivekind()
 * @elseif java
 * @see Spatial::getPrimitivekind()
 * @see Spatial::setPrimitivekind(long)
 * @else
 * @see Spatial::getPrimitivekind()
 * @see Spatial::setPrimitivekind()
 * @endif
 */
typedef enum
{
  SPATIAL_PRIMITIVEKIND_SPHERE         /*!< The spatial primitivekind is @c "sphere". */
, SPATIAL_PRIMITIVEKIND_CUBE           /*!< The spatial primitivekind is @c "cube". */
, SPATIAL_PRIMITIVEKIND_CYLINDER       /*!< The spatial primitivekind is @c "cylinder". */
, SPATIAL_PRIMITIVEKIND_CONE           /*!< The spatial primitivekind is @c "cone". */
, SPATIAL_PRIMITIVEKIND_CIRCLE         /*!< The spatial primitivekind is @c "circle". */
, SPATIAL_PRIMITIVEKIND_SQUARE         /*!< The spatial primitivekind is @c "square". */
, SPATIAL_PRIMITIVEKIND_INVALID        /*!< Invalid PrimitiveKind value. */
} PrimitiveKind_t;


/**
 * Returns the string version of the provided #PrimitiveKind_t enumeration.
 *
 * @param pk the #PrimitiveKind_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "sphere",
 * "cube",
 * "cylinder",
 * "cone",
 * "circle",
 * "square",
 * or @c NULL if the value is @sbmlconstant{SPATIAL_PRIMITIVEKIND_INVALID,
 * PrimitiveKind_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
const char*
PrimitiveKind_toString(PrimitiveKind_t pk);


/**
 */
LIBSBML_EXTERN
PrimitiveKind_t
PrimitiveKind_parse(const char* code);


/**
 * Returns the #PrimitiveKind_t enumeration corresponding to the given string
 * or @sbmlconstant{SPATIAL_PRIMITIVEKIND_INVALID, PrimitiveKind_t} if there is
 * no such match.
 *
 * @param code the string to convert to a #PrimitiveKind_t.
 *
 * @return the corresponding #PrimitiveKind_t or
 * @sbmlconstant{SPATIAL_PRIMITIVEKIND_INVALID, PrimitiveKind_t} if no match is
 * found.
 *
 * @note The matching is case-sensitive: "sphere" will return
 * @sbmlconstant{SPATIAL_PRIMITIVEKIND_SPHERE, PrimitiveKind_t}, but "Sphere"
 * will return @sbmlconstant{SPATIAL_PRIMITIVEKIND_INVALID, PrimitiveKind_t}.
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
PrimitiveKind_t
PrimitiveKind_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #PrimitiveKind_t is valid.
 *
 * @param pk the #PrimitiveKind_t enumeration to query.
 *
 * @return @c 1 (true) if the #PrimitiveKind_t is
 * @sbmlconstant{SPATIAL_PRIMITIVEKIND_SPHERE, PrimitiveKind_t},
 * @sbmlconstant{SPATIAL_PRIMITIVEKIND_CUBE, PrimitiveKind_t},
 * @sbmlconstant{SPATIAL_PRIMITIVEKIND_CYLINDER, PrimitiveKind_t},
 * @sbmlconstant{SPATIAL_PRIMITIVEKIND_CONE, PrimitiveKind_t},
 * @sbmlconstant{SPATIAL_PRIMITIVEKIND_CIRCLE, PrimitiveKind_t},
 * @sbmlconstant{SPATIAL_PRIMITIVEKIND_SQUARE, PrimitiveKind_t}, or
 * @c 0 (false) otherwise (including
 * @sbmlconstant{SPATIAL_PRIMITIVEKIND_INVALID, PrimitiveKind_t}).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
PrimitiveKind_isValid(PrimitiveKind_t pk);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #PrimitiveKind_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "sphere",
 * "cube",
 * "cylinder",
 * "cone",
 * "circle",
 * "square", or
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "sphere" will return @c 1 (true), but
 * "Sphere" will return @c 0 (false).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
PrimitiveKind_isValidString(const char* code);


/**
 * @enum DataKind_t
 * @brief Enumeration of values permitted as the value of the "datakind"
 * attribute on Spatial objects.
 *
 * @if conly
 * @see Spatial_getDatakind()
 * @see Spatial_setDatakind()
 * @elseif java
 * @see Spatial::getDatakind()
 * @see Spatial::setDatakind(long)
 * @else
 * @see Spatial::getDatakind()
 * @see Spatial::setDatakind()
 * @endif
 */
typedef enum
{
  SPATIAL_DATAKIND_DOUBLE        /*!< The spatial datakind is @c "double". */
, SPATIAL_DATAKIND_FLOAT         /*!< The spatial datakind is @c "float". */
, SPATIAL_DATAKIND_UINT8         /*!< The spatial datakind is @c "uint8". */
, SPATIAL_DATAKIND_UINT16        /*!< The spatial datakind is @c "uint16". */
, SPATIAL_DATAKIND_UINT32        /*!< The spatial datakind is @c "uint32". */
, SPATIAL_DATAKIND_INVALID       /*!< Invalid DataKind value. */
} DataKind_t;


/**
 * Returns the string version of the provided #DataKind_t enumeration.
 *
 * @param dk the #DataKind_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "double",
 * "float",
 * "uint8",
 * "uint16",
 * "uint32",
 * or @c NULL if the value is @sbmlconstant{SPATIAL_DATAKIND_INVALID,
 * DataKind_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
const char*
DataKind_toString(DataKind_t dk);


/**
 * Returns the #DataKind_t enumeration corresponding to the given string or
 * @sbmlconstant{SPATIAL_DATAKIND_INVALID, DataKind_t} if there is no such
 * match.
 *
 * @param code the string to convert to a #DataKind_t.
 *
 * @return the corresponding #DataKind_t or
 * @sbmlconstant{SPATIAL_DATAKIND_INVALID, DataKind_t} if no match is found.
 *
 * @note The matching is case-sensitive: "double" will return
 * @sbmlconstant{SPATIAL_DATAKIND_DOUBLE, DataKind_t}, but "Double" will return
 * @sbmlconstant{SPATIAL_DATAKIND_INVALID, DataKind_t}.
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
DataKind_t
DataKind_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #DataKind_t is valid.
 *
 * @param dk the #DataKind_t enumeration to query.
 *
 * @return @c 1 (true) if the #DataKind_t is
 * @sbmlconstant{SPATIAL_DATAKIND_DOUBLE, DataKind_t},
 * @sbmlconstant{SPATIAL_DATAKIND_FLOAT, DataKind_t},
 * @sbmlconstant{SPATIAL_DATAKIND_UINT8, DataKind_t},
 * @sbmlconstant{SPATIAL_DATAKIND_UINT16, DataKind_t}, or
 * @sbmlconstant{SPATIAL_DATAKIND_UINT32, DataKind_t};
 * @c 0 (false) otherwise (including @sbmlconstant{SPATIAL_DATAKIND_INVALID,
 * DataKind_t}).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
DataKind_isValid(DataKind_t dk);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #DataKind_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "double",
 * "float",
 * "uint8",
 * "uint16", or
 * "uint32";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "double" will return @c 1 (true), but
 * "Double" will return @c 0 (false).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
DataKind_isValidString(const char* code);

/**
 */
LIBSBML_EXTERN
DataKind_t
DataKind_parse(const char* code);



/**
 * @enum CompressionKind_t
 * @brief Enumeration of values permitted as the value of the "compressionkind"
 * attribute on Spatial objects.
 *
 * @if conly
 * @see Spatial_getCompressionkind()
 * @see Spatial_setCompressionkind()
 * @elseif java
 * @see Spatial::getCompressionkind()
 * @see Spatial::setCompressionkind(long)
 * @else
 * @see Spatial::getCompressionkind()
 * @see Spatial::setCompressionkind()
 * @endif
 */
typedef enum
{
  SPATIAL_COMPRESSIONKIND_UNCOMPRESSED       /*!< The spatial compressionkind is @c "uncompressed". */
, SPATIAL_COMPRESSIONKIND_DEFLATED           /*!< The spatial compressionkind is @c "deflated". */
, SPATIAL_COMPRESSIONKIND_INVALID            /*!< Invalid CompressionKind value. */
} CompressionKind_t;


/**
 * Returns the string version of the provided #CompressionKind_t enumeration.
 *
 * @param ck the #CompressionKind_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "uncompressed",
 * "deflated",
 * or @c NULL if the value is @sbmlconstant{SPATIAL_COMPRESSIONKIND_INVALID,
 * CompressionKind_t} or another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
const char*
CompressionKind_toString(CompressionKind_t ck);


/**
 */
LIBSBML_EXTERN
CompressionKind_t
CompressionKind_parse(const char* code);


/**
 * Returns the #CompressionKind_t enumeration corresponding to the given string
 * or @sbmlconstant{SPATIAL_COMPRESSIONKIND_INVALID, CompressionKind_t} if
 * there is no such match.
 *
 * @param code the string to convert to a #CompressionKind_t.
 *
 * @return the corresponding #CompressionKind_t or
 * @sbmlconstant{SPATIAL_COMPRESSIONKIND_INVALID, CompressionKind_t} if no
 * match is found.
 *
 * @note The matching is case-sensitive: "uncompressed" will return
 * @sbmlconstant{SPATIAL_COMPRESSIONKIND_UNCOMPRESSED, CompressionKind_t}, but
 * "Uncompressed" will return @sbmlconstant{SPATIAL_COMPRESSIONKIND_INVALID,
 * CompressionKind_t}.
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
CompressionKind_t
CompressionKind_fromString(const char* code);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given #CompressionKind_t is valid.
 *
 * @param ck the #CompressionKind_t enumeration to query.
 *
 * @return @c 1 (true) if the #CompressionKind_t is
 * @sbmlconstant{SPATIAL_COMPRESSIONKIND_UNCOMPRESSED, CompressionKind_t}, or
 * @sbmlconstant{SPATIAL_COMPRESSIONKIND_DEFLATED, CompressionKind_t};
 * @c 0 (false) otherwise (including
 * @sbmlconstant{SPATIAL_COMPRESSIONKIND_INVALID, CompressionKind_t}).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
CompressionKind_isValid(CompressionKind_t ck);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid #CompressionKind_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "uncompressed", or
 * "deflated";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "uncompressed" will return @c 1
 * (true), but "Uncompressed" will return @c 0 (false).
 *
 * @if conly
 * @memberof Spatial_t
 * @endif
 */
LIBSBML_EXTERN
int
CompressionKind_isValidString(const char* code);




LIBSBML_CPP_NAMESPACE_END




#endif /* !SpatialExtension_H__ */


