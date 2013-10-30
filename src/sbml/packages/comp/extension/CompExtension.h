/**
 * @file    CompExtension.h
 * @brief   Definition of CompExtension, the core module of comp package. 
 * @author  Lucian Smith
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2011 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class CompExtension
 * @ingroup comp
 * @brief @htmlinclude pkg-marker-comp.html
 * The core module of the &ldquo;comp&rdquo; package extension.
 *
 * @class CompPkgNamespaces
 * @ingroup comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Extension of SBMLNamespaces for the SBML Level&nbsp;3 'comp' package.
 */

#ifndef CompExtension_h
#define CompExtension_h

#include <sbml/common/extern.h>
#include <sbml/SBMLTypeCodes.h>

#ifdef __cplusplus

#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionNamespaces.h>
#include <sbml/extension/SBMLExtensionRegister.h>


#ifndef COMP_CREATE_NS
#define COMP_CREATE_NS(variable,sbmlns)\
  EXTENSION_CREATE_NS(CompPkgNamespaces,variable,sbmlns);
#endif

#include <vector>

#endif  /* __cplusplus */

LIBSBML_CPP_NAMESPACE_BEGIN

#ifdef __cplusplus

class LIBSBML_EXTERN CompExtension : public SBMLExtension
{
public:

  //---------------------------------------------------------------
  //
  // Required class variables
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
  CompExtension ();


  /**
   * Copy constructor.
   */
  CompExtension(const CompExtension&);


  /**
   * Destroy this object.
   */
  virtual ~CompExtension ();


  /**
   * Assignment operator for CompExtension.
   */
  CompExtension& operator=(const CompExtension&);


  /**
   * Creates and returns a deep copy of this CompExtension object.
   * 
   * @return a (deep) copy of this CompExtension object
   */
  virtual CompExtension* clone () const;


  /**
   * Returns the name of this package as a short-form label
   * (&quot;<code>comp</code>&quot;).
   *
   * @return the name of this package.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the URI (namespace) of the package corresponding to the
   * combination of the given SBML Level, SBML Version, and Level&nbsp;3
   * package version.
   *
   * @param sbmlLevel the level of SBML
   * @param sbmlVersion the version of SBML
   * @param pkgVersion the version of package
   *
   * @return a string of the package URI.  An empty string will be returned
   * if no corresponding URI exists.
   */
  virtual const std::string& getURI(unsigned int sbmlLevel,
                                    unsigned int sbmlVersion, 
                                    unsigned int pkgVersion) const;


  /**
   * Returns the SBML Level with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of the comp
   * package.
   *
   * @return the SBML level with the given URI of this package. @c 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getLevel(const std::string &uri) const;


  /**
   * Returns the SBML version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of comp package.
   *
   * @return the SBML version with the given URI of this package. @c 0 will
   * be returned if the given URI is invalid.
   */
  virtual unsigned int getVersion(const std::string &uri) const;


  /**
   * Returns the package version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of comp package.
   *
   * @return the package version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   */
  virtual unsigned int getPackageVersion(const std::string &uri) const;


  /**
   * Returns an SBMLExtensionNamespaces&lt;CompExtension&gt; object whose alias
   * type is CompPkgNamespace.
   *
   * @param uri the string of URI that represents one of versions of comp package.
   *
   * @return an CompPkgNamespace object corresponding to the given URI. @c
   * NULL will be returned if the given URI is not defined in comp package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string &uri) const;


  /**
   * This method takes a type code of comp package and returns a string
   * representing the code.
   *
   * @param typeCode the libSBML typecode in question.
   *
   * @return a string representing the libSBML type code.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Initializes comp extension by creating an object of this class with 
   * required SBasePlugin derived objects and registering the object 
   * to the SBMLExtensionRegistry class.
   *
   * (NOTE) This function is automatically invoked when creating the following
   *        global object in CompExtension.cpp
   *
   *        static SBMLExtensionRegister<CompExtension> compExtensionRegistry;
   *
   */

  static void init();

  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;
  
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;

  virtual unsigned int getErrorIdOffset() const;

  /** @endcond */

};


// --------------------------------------------------------------------
//
// Required typedef definitions 
//
// CompPkgNamespaces is derived from the SBMLNamespaces class and
// used when creating an object of SBase derived classes defined in
// comp package.
//
// --------------------------------------------------------------------

//
// (NOTE) 
//
// SBMLExtensionNamespaces<CompExtension> must be instantiated
// in CompExtension.cpp for DLL.
//
typedef SBMLExtensionNamespaces<CompExtension> CompPkgNamespaces; 

#endif  /* __cplusplus */

BEGIN_C_DECLS

/**
 * @enum  SBMLCompTypeCode_t
 * @brief SBMLCompTypeCode_t is the enumeration of possible types from the 'comp' package.
 *
 * An enumeration of SBML comp types to help identify SBML objects at runtime.
 * Abstract types sometimes do not have a typecode since they cannot be instantiated.
 *
 * @copydetails SBML_type_codes
 */
typedef enum
  {
    SBML_COMP_SUBMODEL                = 250 /*!< Submodel */
  , SBML_COMP_MODELDEFINITION         = 251 /*!< ModelDefinition */
  , SBML_COMP_EXTERNALMODELDEFINITION = 252 /*!< ExternalModelDefinition */
  , SBML_COMP_SBASEREF                = 253 /*!< SBaseRef */
  , SBML_COMP_DELETION                = 254 /*!< Deletion */
  , SBML_COMP_REPLACEDELEMENT         = 255 /*!< ReplacedElement */
  , SBML_COMP_REPLACEDBY              = 256 /*!< ReplacedBy */
  , SBML_COMP_PORT                    = 257 /*!< Port */

  } SBMLCompTypeCode_t;

END_C_DECLS

LIBSBML_CPP_NAMESPACE_END

#endif  /* CompExtension_h */
