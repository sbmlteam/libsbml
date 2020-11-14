/**
 * @file ArraysExtension.h
 * @brief Definition of ArraysExtension.
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
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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
 * @class ArraysExtension
 * @sbmlbrief{arrays} Base extension class.
 *
 * @class ArraysPkgNamespaces
 * @sbmlbrief{arrays} SBMLNamespaces extension.
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
   * Creates a new ArraysExtension instance.
   */
  ArraysExtension();


  /**
   * Copy constructor for ArraysExtension.
   *
   * @param orig the ArraysExtension instance to copy.
   */
  ArraysExtension(const ArraysExtension& orig);


  /**
   * Assignment operator for ArraysExtension.
   *
   * @param rhs the ArraysExtension object whose values are to be used as the
   * basis of the assignment.
   */
  ArraysExtension& operator=(const ArraysExtension& rhs);


  /**
   * Creates and returns a deep copy of this ArraysExtension object.
   *
   * @return a (deep) copy of this ArraysExtension object.
   */
  virtual ArraysExtension* clone() const;


  /**
   * Destructor for ArraysExtension.
   */
  virtual ~ArraysExtension();


  /**
   * Returns the name of this SBML Level&nbsp;3 package ("arrays").
   *
   * @return a string representing the name of this package ("arrays").
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
   * @return a string representing the name of this package ("arrays").
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
   * the "arrays" package.
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
   * the "arrays" package.
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
   * the "arrays" package.
   *
   * @return the version of the SBML Level&nbsp;3 package for the given URI of
   * this package, or @c 0 if the given URI is invalid.
   */
  virtual unsigned int getPackageVersion(const std::string& uri) const;


  /**
   * Returns a ArraysPkgNamespaces object.
   *
   * @param uri the string of the URI that represents one of the versions of
   * the "arrays" package.
   *
   * @return ArraysPkgNamespaces object corresponding to the given URI of this
   * package, or @c NULL if the given URI is not defined in the "arrays"
   * package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string& uri)
    const;


  /**
   * Takes a type code of the &ldquo;arrays&rdquo; package and returns a string
   * describing the code.
   *
   * @param typeCode a libSBML type code defined by the libSBML extension
   * implementing support for the SBML Level&nbsp;3 &ldquo;arrays&rdquo;
   * package.
   *
   * @return a text string representing the type code given by @p typeCode. If
   * the type code is unrecognized for this implementation of the libSBML
   * &ldquo;arrays&rdquo; package, the string returned will be <code>"(Unknown
   * SBML Arrays Type)"</code>.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the entry in the error table at this index.
   *
   * @param index an unsigned integer representing the index of the error.
   *
   * @return packageErrorTableEntry object in the ArraysSBMLErrorTable.
   */
  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the index in the error table with the given errorId.
   *
   * @param errorId an unsigned integer representing the errorId of the error.
   *
   * @return unsigned int representing the index in the ArraysSBMLErrorTable
   * corresponding to the errorId given.
   */
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the offset for the errorId range for the "arrays" package.
   *
   * @return unsigned int representing the offset for errors in the
   * ArraysSBMLErrorTable.
   */
  virtual unsigned int getErrorIdOffset() const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Initializes arrays extension by creating an object of this class with the
   * required SBasePlugin derived objects and registering the object to the
   * SBMLExtensionRegistry class
   *
   * This function is automatically invoked when creatingthe following global
   * object in ArraysExtension.cpp
   *
   * static SBMLExtensionRegister<ArraysExtension> arraysExtensionRegistry;
   */
  static void init();

  /** @endcond */


};

/**
 *
 * Required typedef definitions
 *
 * ArraysPkgNamespace is derived from SBMLNamespaces class and used when
 * creating an object of SBase derived classes defined in the arrays package
 *
 * SBMLExtensionNamespaces<ArraysExtension> must be instantiated in
 * ArraysExtension.cpp for DLL
 *
 */
typedef SBMLExtensionNamespaces<ArraysExtension> ArraysPkgNamespaces;


LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_BEGIN


/**
 * @enum SBMLArraysTypeCode_t
 * @brief SBMLArraysTypeCode_t Enumeration of possible types in the libSBML
 * &ldquo;arrays&rdquo; package implementation.
 *
 * @copydetails doc_what_are_typecodes
 *
 * @copydetails doc_additional_typecode_details
 */
typedef enum
{
  SBML_ARRAYS_INDEX         =  1200  /*!<Index */
, SBML_ARRAYS_DIMENSION     =  1201  /*!<Dimension */
} SBMLArraysTypeCode_t;

LIBSBML_CPP_NAMESPACE_END




#endif /* !ArraysExtension_H__ */


