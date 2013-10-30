/**
 * @file    QualExtension.h
 * @brief   Definition of QualExtension, the core module of qual package. 
 * @author  Akiya Jouraku
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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
 *
 * @class QualExtension
 * @ingroup qual
 * @brief @htmlinclude pkg-marker-qual.html
 * The core module of the 'qual' package extension.
 *
 * @class QualPkgNamespaces
 * @ingroup qual
 * @brief @htmlinclude pkg-marker-qual.html
 * Extension of SBMLNamespaces for the SBML Level&nbsp;3 'qual' package.
 */

#ifndef QualExtension_h
#define QualExtension_h

#include <sbml/common/extern.h>
#include <sbml/SBMLTypeCodes.h>

#ifdef __cplusplus

#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionNamespaces.h>
#include <sbml/extension/SBMLExtensionRegister.h>


#ifndef QUAL_CREATE_NS
#define QUAL_CREATE_NS(variable,sbmlns)\
  EXTENSION_CREATE_NS(QualPkgNamespaces,variable,sbmlns);
#endif

#include <vector>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN QualExtension : public SBMLExtension
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
  QualExtension ();


  /**
   * Copy constructor.
   */
  QualExtension(const QualExtension& orig);


  /**
   * Destroy this object.
   */
  virtual ~QualExtension ();


  /**
   * Assignment operator for QualExtension.
   */
  QualExtension& operator=(const QualExtension& rhs);


  /**
   * Creates and returns a deep copy of this QualExtension object.
   * 
   * @return a (deep) copy of this SBase object
   */
  virtual QualExtension* clone () const;


  /**
   * Returns the name of this package ("qual")
   *
   * @return a string representing the name of this package ("qual")
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
   * @param uri the string of URI that represents one of versions of qual package
   *
   * @return the SBML level with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getLevel(const std::string &uri) const;


  /**
   * Returns the SBML version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of qual package
   *
   * @return the SBML version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   */
  virtual unsigned int getVersion(const std::string &uri) const;


  /**
   * Returns the package version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of qual package
   *
   * @return the package version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   */
  virtual unsigned int getPackageVersion(const std::string &uri) const;


  /**
   * Returns an SBMLExtensionNamespaces<QualExtension> object whose alias type is 
   * QualPkgNamespace.
   * Null will be returned if the given uri is not defined in the qual package.
   *
   * @param uri the string of URI that represents one of versions of qual package
   *
   * @return an QualPkgNamespace object corresponding to the given uri. NULL will
   * be returned if the given URI is not defined in qual package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string &uri) const;


  /**
   * This method takes a type code of qual package and returns a string representing 
   * the code.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;


  /** @cond doxygenLibsbmlInternal */
  
  /**
   * Initializes qual extension by creating an object of this class with 
   * required SBasePlugin derived objects and registering the object 
   * to the SBMLExtensionRegistry class.
   *
   * (NOTE) This function is automatically invoked when creating the following
   *        global object in QualExtension.cpp
   *
   *        static SBMLExtensionRegister<QualExtension> qualExtensionRegistry;
   *
   */

  static void init();

  /** @endcond */
  
  
  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the entry in the error table at this index. 
   *
   * @param index an unsigned intgere representing the index of the error in the QualSBMLErrorTable
   *
   * @return packageErrorTableEntry object in the QualSBMLErrorTable corresponding to the index given.
   */
  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the index in the error table with the given errorId. 
   *
   * @param errorId an unsigned intgere representing the errorId of the error in the QualSBMLErrorTable
   *
   * @return unsigned integer representing the index in the QualSBMLErrorTable corresponding to the errorId given.
   */
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the offset for the errorId range for the qual L3 package. 
   *
   * @return unsigned intege representing the  offset for errors QualSBMLErrorTable.
   */
  virtual unsigned int getErrorIdOffset() const;


  /** @endcond */

};


// --------------------------------------------------------------------
//
// Required typedef definitions 
//
// QualPkgNamespaces is derived from the SBMLNamespaces class and
// used when creating an object of SBase derived classes defined in
// qual package.
//
// --------------------------------------------------------------------

//
// (NOTE) 
//
// SBMLExtensionNamespaces<QualExtension> must be instantiated
// in QualExtension.cpp for DLL.
//
typedef SBMLExtensionNamespaces<QualExtension> QualPkgNamespaces; 

/**
 * @enum  SBMLQualTypeCode_t
 * @brief SBMLQualTypeCode_t is the enumeration of possible types from the 'qual' package.
 *
 * An enumeration of SBML qual types to help identify SBML objects at runtime.
 * Abstract types sometimes do not have a typecode since they cannot be instantiated.
 *
 * @copydetails SBML_type_codes
 */
typedef enum
{
   SBML_QUAL_QUALITATIVE_SPECIES  = 1100 /*!< QualitativeSpecies */
 , SBML_QUAL_TRANSITION           = 1101 /*!< Transition */
 , SBML_QUAL_INPUT                = 1102 /*!< Input */
 , SBML_QUAL_OUTPUT               = 1103 /*!< Output */
 , SBML_QUAL_FUNCTION_TERM        = 1104 /*!< FunctionTerm */
 , SBML_QUAL_DEFAULT_TERM         = 1105 /*!< DefaultTerm */
} SBMLQualTypeCode_t;


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* QualExtension_h */
