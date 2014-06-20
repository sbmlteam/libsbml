/**
 * @file:   ArraysExtension.h
 * @brief:  Implementation of the ArraysExtension class
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


#ifndef ArraysExtension_H__
#define ArraysExtension_H__


#include <sbml/common/extern.h>
#include <sbml/SBMLTypeCodes.h>


#ifdef __cplusplus


#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionNamespaces.h>
#include <sbml/extension/SBMLExtensionRegister.h>


#ifndef ARRAYS_CREATE_NS
  #define ARRAYS_CREATE_NS(variable, sbmlns)\
    EXTENSION_CREATE_NS(ArraysPkgNamespaces, variable, sbmlns);
#endif


#include <vector>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ArraysExtension : public SBMLExtension
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
   * Creates a new ArraysExtension   */
  ArraysExtension();


  /**
   * Copy constructor for ArraysExtension.
   *
   * @param orig; the ArraysExtension instance to copy.
   */
  ArraysExtension(const ArraysExtension& orig);


   /**
   * Assignment operator for ArraysExtension.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  ArraysExtension& operator=(const ArraysExtension& rhs);


   /**
   * Creates and returns a deep copy of this ArraysExtension object.
   *
   * @return a (deep) copy of this ArraysExtension object.
   */
  virtual ArraysExtension* clone () const;


   /**
   * Destructor for ArraysExtension.
   */
  virtual ~ArraysExtension();


   /**
   * Returns the name of this package ("arrays")
   *
   * @return a string representing the name of this package ("arrays")
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
   * @param uri the string of URI that represents one of versions of arrays package
   *
   * @return the SBML level with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getLevel(const std::string &uri) const;


  /**
   * Returns the SBML version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of arrays package
   *
   * @return the SBML version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getVersion(const std::string &uri) const;


  /**
   * Returns the package version with the given URI of this package.
   *
   * @param uri the string of URI that represents one of versions of arrays package
   *
   * @return the package version with the given URI of this package. 0 will be returned
   * if the given URI is invalid.
   *
   */
  virtual unsigned int getPackageVersion(const std::string &uri) const;


  /**
   * Returns an SBMLExtensionNamespaces<ArraysExtension> object whose alias type is 
   * ArraysPkgNamespace.
   * Null will be returned if the given uri is not defined in the arrays package.
   *
   * @param uri the string of URI that represents one of versions of arrays package
   *
   * @return an ArraysPkgNamespace object corresponding to the given uri. NULL will
   * be returned if the given URI is not defined in arrays package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string &uri) const;


  /**
   * This method takes a type code from the Arrays package and returns a string representing 
   * the code.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Initializes arrays extension by creating an object of this class with 
   * required SBasePlugin derived objects and registering the object 
   * to the SBMLExtensionRegistry class.
   *
   * (NOTE) This function is automatically invoked when creating the following
   *        global object in ArraysExtension.cpp
   *
   *        static SBMLExtensionRegister<ArraysExtension> arraysExtensionRegistry;
   *
   */
  static void init();


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the entry in the error table at this index. 
   *
   * @param index an unsigned intgere representing the index of the error in the ArraysSBMLErrorTable
   *
   * @return packageErrorTableEntry object in the ArraysSBMLErrorTable corresponding to the index given.
   */
  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the index in the error table with the given errorId. 
   *
   * @param errorId an unsigned intgere representing the errorId of the error in the ArraysSBMLErrorTable
   *
   * @return unsigned integer representing the index in the ArraysSBMLErrorTable corresponding to the errorId given.
   */
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the offset for the errorId range for the arrays L3 package. 
   *
   * @return unsigned intege representing the  offset for errors ArraysSBMLErrorTable.
   */
  virtual unsigned int getErrorIdOffset() const;


  /** @endcond doxygenLibsbmlInternal */


};


// --------------------------------------------------------------------
//
// Required typedef definitions
//
// ArraysPkgNamespaces is derived from the SBMLNamespaces class and
// used when creating an object of SBase derived classes defined in
// arrays package.
//
// --------------------------------------------------------------------
//
// (NOTE)
//
// SBMLExtensionNamespaces<ArraysExtension> must be instantiated
// in ArraysExtension.cpp for DLL.
//
typedef SBMLExtensionNamespaces<ArraysExtension> ArraysPkgNamespaces;

typedef enum
{
    SBML_ARRAYS_INDEX  = 1200
  , SBML_ARRAYS_DIMENSION          = 1201
} SBMLArraysTypeCode_t;

#if (0)
typedef enum
{
    AST_LINEAR_ALGEBRA_DETERMINANT = 1000
  , AST_LINEAR_ALGEBRA_SELECTOR   
  , AST_LINEAR_ALGEBRA_TRANSPOSE
  , AST_LINEAR_ALGEBRA_VECTOR_PRODUCT
  , AST_LINEAR_ALGEBRA_SCALAR_PRODUCT
  , AST_LINEAR_ALGEBRA_OUTER_PRODUCT
  , AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR
  , AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR
  , AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR
  , AST_ARRAYS_UNKNOWN
} ArraysASTNodeType_t;
#endif

typedef enum
{
    AST_QUALIFIER_CONDITION = 1000
  , AST_LOGICAL_EXISTS
  , AST_LOGICAL_FORALL
  , AST_QUALIFIER_LOWLIMIT
  , AST_STATISTICS_MEAN
  , AST_STATISTICS_MEDIAN
  , AST_STATISTICS_MODE
  , AST_STATISTICS_MOMENT
  , AST_QUALIFIER_MOMENTABOUT
  , AST_SERIES_PRODUCT
  , AST_STATISTICS_SDEV
  , AST_LINEAR_ALGEBRA_SELECTOR
  , AST_SERIES_SUM
  , AST_QUALIFIER_UPLIMIT
  , AST_STATISTICS_VARIANCE
  , AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR
  , AST_ARRAYS_UNKNOWN
} ArraysASTNodeType_t;



LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* ArraysExtension_H__ */


