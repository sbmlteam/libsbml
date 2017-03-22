/**
 * @file:   MultiExtension.h
 * @brief:  Implementation of the MultiExtension class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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


#ifndef MultiExtension_H__
#define MultiExtension_H__


#include <sbml/common/extern.h>
#include <sbml/SBMLTypeCodes.h>


#ifdef __cplusplus


#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionNamespaces.h>
#include <sbml/extension/SBMLExtensionRegister.h>


#ifndef MULTI_CREATE_NS
  #define MULTI_CREATE_NS(variable, sbmlns)\
    EXTENSION_CREATE_NS(MultiPkgNamespaces, variable, sbmlns);
#endif


#include <vector>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN MultiExtension : public SBMLExtension
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
   * Creates a new MultiExtension   */
  MultiExtension();


  /**
   * Copy constructor for MultiExtension.
   *
   * @param orig; the MultiExtension instance to copy.
   */
  MultiExtension(const MultiExtension& orig);


   /**
   * Assignment operator for MultiExtension.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  MultiExtension& operator=(const MultiExtension& rhs);


   /**
   * Creates and returns a deep copy of this MultiExtension object.
   *
   * @return a (deep) copy of this MultiExtension object.
   */
  virtual MultiExtension* clone () const;


   /**
   * Destructor for MultiExtension.
   */
  virtual ~MultiExtension();


   /**
   * Returns the name of this package ("multi")
   *
   * @return a string representing the name of this package ("multi")
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
   * @param uri the string of URI that represents one of versions of multi package
   *
   * @return the SBML level with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getLevel(const std::string &uri) const;


  /**
   * Returns the SBML version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of multi package
   *
   * @return the SBML version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getVersion(const std::string &uri) const;


  /**
   * Returns the package version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of multi package
   *
   * @return the package version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getPackageVersion(const std::string &uri) const;


  /**
   * Returns an SBMLExtensionNamespaces<MultiExtension> object whose alias type is 
   * MultiPkgNamespace.
   * Null will be returned if the given uri is not defined in the multi package.
   *
   * @param uri the string of URI that represents one of versions of multi package
   *
   * @return an MultiPkgNamespace object corresponding to the given uri. NULL will
   * be returned if the given URI is not defined in multi package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string &uri) const;


  /**
   * This method takes a type code from the Multi package and returns a string representing 
   * the code.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Initializes multi extension by creating an object of this class with 
   * required SBasePlugin derived objects and registering the object 
   * to the SBMLExtensionRegistry class.
   *
   * (NOTE) This function is automatically invoked when creating the following
   *        global object in MultiExtension.cpp
   *
   *        static SBMLExtensionRegister<MultiExtension> multiExtensionRegistry;
   *
   */
  static void init();


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the entry in the error table at this index. 
   *
   * @param index an unsigned intgere representing the index of the error in the MultiSBMLErrorTable
   *
   * @return packageErrorTableEntry object in the MultiSBMLErrorTable corresponding to the index given.
   */
  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the index in the error table with the given errorId. 
   *
   * @param errorId an unsigned intgere representing the errorId of the error in the MultiSBMLErrorTable
   *
   * @return unsigned integer representing the index in the MultiSBMLErrorTable corresponding to the errorId given.
   */
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the offset for the errorId range for the multi L3 package. 
   *
   * @return unsigned intege representing the  offset for errors MultiSBMLErrorTable.
   */
  virtual unsigned int getErrorIdOffset() const;


  /** @endcond doxygenLibsbmlInternal */


};


// --------------------------------------------------------------------
//
// Required typedef definitions
//
// MultiPkgNamespaces is derived from the SBMLNamespaces class and
// used when creating an object of SBase derived classes defined in
// multi package.
//
// --------------------------------------------------------------------
//
// (NOTE)
//
// SBMLExtensionNamespaces<MultiExtension> must be instantiated
// in MultiExtension.cpp for DLL.
//
typedef SBMLExtensionNamespaces<MultiExtension> MultiPkgNamespaces;

typedef enum
{
    SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE  = 1400
  , SBML_MULTI_SPECIES_FEATURE_VALUE = 1401
  , SBML_MULTI_COMPARTMENT_REFERENCE = 1402
  , SBML_MULTI_SPECIES_TYPE_INSTANCE = 1403
  , SBML_MULTI_IN_SPECIES_TYPE_BOND = 1404
  , SBML_MULTI_OUTWARD_BINDING_SITE = 1405
  , SBML_MULTI_SPECIES_FEATURE_TYPE = 1406
  , SBML_MULTI_SPECIES_TYPE_COMPONENT_INDEX = 1407
  , SBML_MULTI_SPECIES_FEATURE     = 1408
  , SBML_MULTI_SPECIES_TYPE_COMPONENT_MAP_IN_PRODUCT = 1409
  , SBML_MULTI_SPECIES_TYPE        = 1410
  , SBML_MULTI_BINDING_SITE_SPECIES_TYPE        = 1411
  , SBML_MULTI_INTRA_SPECIES_REACTION = 1412
  , SBML_MULTI_SUBLIST_OF_SPECIES_FEATURES = 1413
} SBMLMultiTypeCode_t;




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* MultiExtension_H__ */


