/**
 * @file    FbcExtension.h
 * @brief   Definition of FbcExtension, the core module of fbc package. 
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009-2013 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class FbcExtension
 * @sbmlbrief{fbc} The core module of the 'fbc' package extension.
 *
 * @class FbcPkgNamespaces
 * @sbmlbrief{fbc} Extension of SBMLNamespaces for the SBML Level&nbsp;3
 * 'fbc' package.
 */

#ifndef FbcExtension_h
#define FbcExtension_h

#include <sbml/common/extern.h>

#ifdef __cplusplus

#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionNamespaces.h>
#include <sbml/extension/SBMLExtensionRegister.h>


#ifndef FBC_CREATE_NS
#define FBC_CREATE_NS(variable,sbmlns)\
  EXTENSION_CREATE_NS(FbcPkgNamespaces,variable,sbmlns);
#endif


#include <vector>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN FbcExtension : public SBMLExtension
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
   * Constructor
   */
  FbcExtension ();


  /**
   * Copy constructor.
   */
  FbcExtension(const FbcExtension&);


  /**
   * Destroy this object.
   */
  virtual ~FbcExtension ();


  /**
   * Assignment operator for FbcExtension.
   */
  FbcExtension& operator=(const FbcExtension&);


  /**
   * Creates and returns a deep copy of this FbcExtension object.
   * 
   * @return a (deep) copy of this FbcExtension object
   */
  virtual FbcExtension* clone () const;


  /**
   * Returns the name of this package ("fbc")
   *
   * @return the name of this package ("fbc")
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
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return the SBML level with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getLevel(const std::string &uri) const;


  /**
   * Returns the SBML version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return the SBML version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   */
  virtual unsigned int getVersion(const std::string &uri) const;


  /**
   * Returns the package version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return the package version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   */
  virtual unsigned int getPackageVersion(const std::string &uri) const;


  /**
   * Returns an SBMLExtensionNamespaces<FbcExtension> object whose alias type is 
   * LayoutPkgNamespace.
   * Null will be returned if the given uri is not defined in the layout package.
   *
   * @param uri the string of URI that represents one of versions of layout package
   *
   * @return an LayoutPkgNamespace object corresponding to the given uri. NULL will
   * be returned if the given URI is not defined in layout package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string &uri) const;


  /**
   * This method takes a type code of the fbc package and returns a string representing 
   * the code.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Initializes layout extension by creating an object of this class with 
   * required SBasePlugin derived objects and registering the object 
   * to the SBMLExtensionRegistry class.
   *
   * (NOTE) This function is automatically invoked when creating the following
   *        global object in FbcExtension.cpp
   *
   *        static SBMLExtensionRegister<FbcExtension> fbcExtensionRegister;
   *
   */
  static void init();
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  virtual unsigned int getErrorIdOffset() const;
  /** @endcond */

};



//
// (NOTE) 
//
// SBMLExtensionNamespaces<FbcExtension> must be instantiated
// in FbcExtension.cpp for DLL.
//
typedef SBMLExtensionNamespaces<FbcExtension> FbcPkgNamespaces; 

/**
 * @enum  SBMLFbcTypeCode_t
 * @brief SBMLFbcTypeCode_t is the enumeration of possible types from the 'fbc' package.
 *
 * @copydetails doc_what_are_typecodes
 *
 * @copydetails doc_additional_typecode_details
 */
typedef enum
{
    SBML_FBC_ASSOCIATION      = 800 /*!< Association */
   ,SBML_FBC_FLUXBOUND        = 801 /*!< FluxBound */
   ,SBML_FBC_FLUXOBJECTIVE    = 802 /*!< FluxObjective */
   ,SBML_FBC_GENEASSOCIATION  = 803 /*!< GeneAssociation */
   ,SBML_FBC_OBJECTIVE        = 804 /*!< Objective */
} SBMLFbcTypeCode_t;


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* FbcExtension_h */
