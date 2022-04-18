/**
 * @file:   MultiExtension.h
 * @brief:  Implementation of the MultiExtension class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
 *
 * @class MultiExtension
 * @sbmlbrief{multi}  Base extension class for the package.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * This is the Multistate, Multicomponent and Multicompartment Species
 * package extension of the SBMLExtension class.  This is a class that every
 * libSBML plug-in must implement in order to implement an SBML Level&nbsp;3
 * package.
 *
 * @class MultiPkgNamespaces
 * @sbmlbrief{multi} SBMLNamespaces extension for the "multi" package.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * There is currently exactly one namespace defined for the Multistate,
 * Multicomponent and Multicompartment Species package:
 * @c "http://www.sbml.org/sbml/level3/version1/multi/version1".  Despite
 * referencing SBML Level&nbsp;3 Version&nbsp;1 explicitly, this package (and
 * all such packages) can be used without change in SBML Level&nbsp;3
 * Version&nbsp;2 documents.  The only caveat is that features of the SBML
 * Level&nbsp;3 Version&nbsp;2 specification that were not present in
 * Level&nbsp;1 may not be used by constructs from Level&nbsp;1 packages.
 * However, this restriction should not affect the 'multi' package.
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
   * Returns the nickname of the SBML Level&nbsp;3 package implemented by
   * this libSBML extension.
   *
   * @return the package nickname, as a string.
   *
   * @copydetails doc_note_static_methods
   */
  static const std::string& getPackageName ();


  /**
   * Returns the default SBML Level used by this libSBML package extension.
   *
   * @return the SBML Level.
   *
   * @copydetails doc_note_static_methods
   */
  static unsigned int getDefaultLevel();


  /**
   * Returns the default SBML Version used by this libSBML package extension.
   *
   * @return the Version within the default SBML Level.
   *
   * @copydetails doc_note_static_methods
   */
  static unsigned int getDefaultVersion();


  /**
   * Returns the default version of the SBML Level&nbsp;3 package implemented
   * by this libSBML extension.
   *
   * @return the default version number of the SBML Level&nbsp;3 package
   * definition.
   *
   * @copydetails doc_note_static_methods
   */
  static unsigned int getDefaultPackageVersion();


  /**
   * Returns the XML namespace URI of the SBML Level&nbsp;3 package
   * implemented by this libSBML extension.
   *
   * @return the XML namespace as a string.
   *
   * @copydetails doc_note_static_methods
   */
  static const std::string&  getXmlnsL3V1V1();


  //
  // Other URI needed in this package (if any)
  //
  //---------------------------------------------------------------


  /**
   * Creates a new MultiExtension instance.
   */
  MultiExtension();


  /**
   * Copy constructor for MultiExtension.
   *
   * @param orig the instance to copy.
   */
  MultiExtension(const MultiExtension& orig);


  /**
   * Assignment operator for MultiExtension.
   *
   * @param rhs the object whose values are used as the basis
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
   * Returns a string representing the SBML XML namespace of this SBML
   * Level&nbsp;3 package.
   *
   * The namespace URI constructed by this method corresponds to the
   * combination of the Level and Version of SBML, and the Version of the SBML
   * Level&nbsp;3 package. (At the time of this writing, the only SBML Level
   * that supports packages is Level&nbsp;3, so the value of @p sbmlLevel must
   * necessarily always be <code>3</code>.)
   *
   * @param sbmlLevel the level of SBML.
   * @param sbmlVersion the version of SBML.
   * @param pkgVersion the version of the package.
   *
   * @return a string of the package URI, or an empty string if no
   * corresponding URI exists.
   */
  virtual const std::string& getURI(unsigned int sbmlLevel,
                                    unsigned int sbmlVersion,
                                    unsigned int pkgVersion) const;


  /**
   * Returns the SBML Level for the given URI of this package.
   *
   * @param uri a URI that represents a version of this package.
   *
   * @return the SBML Level for the given URI of this package, or @c 0 if the
   * given URI is invalid, or for a different package.
   */
  virtual unsigned int getLevel(const std::string& uri) const;


  /**
   * Returns the Version within the SBML Level for the given URI of this
   * package.
   *
   * @param uri a URI that represents a version of this package.
   *
   * @return the SBML Version within the SBML Level for the given URI of this
   * package, or @c 0 if the given URI is invalid, or for a different package.
   */
  virtual unsigned int getVersion(const std::string& uri) const;


  /**
   * Returns the SBML Level&nbsp;3 package version for the given URI of this
   * package.
   *
   * @param uri a URI that represents one of the valid versions of this
   * package.
   *
   * @return the version of the SBML Level&nbsp;3 package with the given URI,
   * or @c 0 if the given URI is invalid, or for a different package.
   */
  virtual unsigned int getPackageVersion(const std::string& uri) const;


  /**
   * Returns a MultiPkgNamespaces object.
   *
   * @param uri a URI that represents one of the valid versions of the
   * "multi" package.
   *
   * @return a MultiPkgNamespaces object corresponding to the given @p uri,
   * or @c NULL if the URI is not defined in the Multi package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string& uri) const;


  /**
   * Takes a type code of the "multi" package and returns a string
   * describing the code.
   *
   * @param typeCode a libSBML type code defined by the libSBML extension.
   * implementing support for the SBML Level&nbsp;3 "multi" package.
   *
   * @return a text string representing the type code given by @p typeCode.
   * If the type code is unrecognized for this implementation of the libSBML
   * "multi" package, the string returned will be
   * <code>"(Unknown SBML Multi Type)"</code>.
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
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Return the entry in the error table at this index. 
   *
   * @param index an unsigned intgere representing the index of the error in
   * the MultiSBMLErrorTable
   *
   * @return packageErrorTableEntry object in the MultiSBMLErrorTable
   * corresponding to the index given.
   */
  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Return the index in the error table with the given errorId. 
   *
   * @param errorId an unsigned intgere representing the errorId of the error
   * in the MultiSBMLErrorTable
   *
   * @return unsigned integer representing the index in the
   * MultiSBMLErrorTable corresponding to the errorId given.
   */
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Return the offset for the errorId range for the multi L3 package. 
   *
   * @return unsigned intege representing the  offset for errors MultiSBMLErrorTable.
   */
  virtual unsigned int getErrorIdOffset() const;
  /** @endcond */


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

/**
 * @enum  SBMLMultiTypeCode_t
 * @brief SBMLMultiTypeCode_t Enumeration of possible types in the libSBML
 * &ldquo;multi&rdquo; package implementation.
 *
 * @copydetails doc_what_are_typecodes
 *
 * @copydetails doc_additional_typecode_details
 */
typedef enum
{
  SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE          = 1400  /*!< PossibleSpeciesFeatureValue */
  , SBML_MULTI_SPECIES_FEATURE_VALUE                 = 1401  /*!< SpeciesFeatureValue */
  , SBML_MULTI_COMPARTMENT_REFERENCE                 = 1402  /*!< CompartmentReference */
  , SBML_MULTI_SPECIES_TYPE_INSTANCE                 = 1403  /*!< SpeciesTypeInstance */
  , SBML_MULTI_IN_SPECIES_TYPE_BOND                  = 1404  /*!< InSpeciesTypeBond */
  , SBML_MULTI_OUTWARD_BINDING_SITE                  = 1405  /*!< OutwardBindingSite */
  , SBML_MULTI_SPECIES_FEATURE_TYPE                  = 1406  /*!< SpeciesFeatureType */
  , SBML_MULTI_SPECIES_TYPE_COMPONENT_INDEX          = 1407  /*!< SpeciesTypeComponentIndex */
  , SBML_MULTI_SPECIES_FEATURE                       = 1408  /*!< SpeciesFeature */
  , SBML_MULTI_SPECIES_TYPE_COMPONENT_MAP_IN_PRODUCT = 1409  /*!< SpeciesTypeComponentMapInProduct */
  , SBML_MULTI_SPECIES_TYPE                          = 1410  /*!< MultiSpeciesType */
  , SBML_MULTI_BINDING_SITE_SPECIES_TYPE             = 1411  /*!< BindingSiteSpeciesType */
  , SBML_MULTI_INTRA_SPECIES_REACTION                = 1412  /*!< IntraSpeciesReaction */
  , SBML_MULTI_SUBLIST_OF_SPECIES_FEATURES           = 1413  /*!< SubListOfSpeciesFeatures */
} SBMLMultiTypeCode_t;




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* MultiExtension_H__ */


