/**
 * @file    SpatialExtension.h
 * @brief   Definition of SpatialExtension, the core module of spatial package. 
 * @author  
 *
 * $Id: SpatialExtension.h 10667 2010-01-16 10:20:44Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/groups/extension/SpatialExtension.h $
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

#ifndef SpatialExtension_h
#define SpatialExtension_h

#include <sbml/common/extern.h>
#include <sbml/SBMLTypeCodes.h>

#ifdef __cplusplus

#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionNamespaces.h>
#include <sbml/extension/SBMLExtensionRegister.h>


#ifndef SPATIAL_CREATE_NS
#define SPATIAL_CREATE_NS(variable,sbmlns)\
  EXTENSION_CREATE_NS(SpatialPkgNamespaces,variable,sbmlns);
#endif

#include <vector>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN SpatialExtension : public SBMLExtension
{
public:

  //---------------------------------------------------------------
  //
  // Required class variables
  //
  //---------------------------------------------------------------

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
   * Constructor
   */
  SpatialExtension ();


  /**
   * Copy constructor.
   */
  SpatialExtension(const SpatialExtension&);


  /**
   * Destroy this object.
   */
  virtual ~SpatialExtension ();


  /**
   * Assignment operator for SpatialExtension.
   */
  SpatialExtension& operator=(const SpatialExtension&);


  /**
   * Creates and returns a deep copy of this SpatialExtension object.
   * 
   * @return a (deep) copy of this SBase object
   */
  virtual SpatialExtension* clone () const;


  /**
   * Returns the name of this package ("spatial")
   *
   * @pram the name of this package ("spatial")
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
  virtual const std::string& getURI(unsigned int sbmlLevel, unsigned int sbmlVersion, 
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
   */
  virtual unsigned int getVersion(const std::string &uri) const;


  /**
   * Returns the package version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of spatial package
   *
   * @return the package version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
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
   * This method takes a type code of spatial package and returns a string representing 
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
    SBML_SPATIAL_DOMAINTYPE				      = 300
  , SBML_SPATIAL_DOMAIN					      = 301
  , SBML_SPATIAL_INTERIORPOINT			      = 302
  , SBML_SPATIAL_COORDINATECOMPONENT	      = 303
  , SBML_SPATIAL_BOUNDARY				      = 304
  , SBML_SPATIAL_BOUNDARYMIN			      = 305
  , SBML_SPATIAL_BOUNDARYMAX			      = 306
  , SBML_SPATIAL_COMPARTMENTMAPPING		      = 307
  , SBML_SPATIAL_ADJACENTDOMAINS		      = 308
  , SBML_SPATIAL_GEOMETRYDEFINITION		      = 309
  , SBML_SPATIAL_SAMPLEDFIELDGEOMETRY	      = 310
  , SBML_SPATIAL_SAMPLEDFIELD			      = 311
  , SBML_SPATIAL_IMAGEDATA				      = 312
  , SBML_SPATIAL_SAMPLEDVOLUME			      = 313
  , SBML_SPATIAL_ANALYTICGEOMETRY		      = 314
  , SBML_SPATIAL_ANALYTICVOLUME			      = 315
  , SBML_SPATIAL_PARAMETRICGEOMETRY	          = 316
  , SBML_SPATIAL_PARAMETRICOBJECT   	      = 317
  , SBML_SPATIAL_POLYGONOBJECT			      = 318
  , SBML_SPATIAL_SPATIALPOINT			      = 319
  , SBML_SPATIAL_CSGGEOMETRY			      = 320
  , SBML_SPATIAL_CSGOBJECT				      = 321
  , SBML_SPATIAL_CSGNODE				      = 322
  , SBML_SPATIAL_CSGTRANSFORMATION		      = 323
  , SBML_SPATIAL_CSGTRANSLATION 		      = 324
  , SBML_SPATIAL_CSGROTATION			      = 325
  , SBML_SPATIAL_CSGSCALE					  = 326
  , SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION = 327
  , SBML_SPATIAL_TRANSFORMATIONCOMPONENTS     = 328
  , SBML_SPATIAL_CSGPRIMITIVE			      = 329
  , SBML_SPATIAL_CSGPSEUDOPRIMITIVE		      = 330
  , SBML_SPATIAL_CSGSETOPERATOR			      = 331
  , SBML_SPATIAL_SPATIALSYMBOLREFERENCE	      = 332
  , SBML_SPATIAL_DIFFUSIONCOEFFICIENT	      = 333
  , SBML_SPATIAL_ADVECTIONCOEFFICIENT	      = 334
  , SBML_SPATIAL_BOUNDARYCONDITION		      = 335
  , SBML_SPATIAL_GEOMETRY				      = 336

} SBMLSpatialTypeCode_t;


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* SpatialExtension_h */
