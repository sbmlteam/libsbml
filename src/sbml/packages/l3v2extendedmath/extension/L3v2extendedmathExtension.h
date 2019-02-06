/**
 * @file L3v2extendedmathExtension.h
 * @brief Definition of L3v2extendedmathExtension.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * @class L3v2extendedmathExtension
 * @sbmlbrief{l3v2extendedmath} Base extension class for the package.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * This is the L3v2extendedmath package extension of the SBMLExtension class
 * that is used to facilitate libSBML plug-ins in the implementation of an
 * SBMLLevel&nbsp;3 package.
 *
 * @class L3v2extendedmathPkgNamespaces
 * @sbmlbrief{l3v2extendedmath} SBMLNamespaces extension.
 *
 * @htmlinclude not-sbml-warning.html
 */


#ifndef L3v2extendedmathExtension_H__
#define L3v2extendedmathExtension_H__


#include <sbml/common/extern.h>
#include <sbml/SBMLTypeCodes.h>


#ifdef __cplusplus


#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionNamespaces.h>
#include <sbml/extension/SBMLExtensionRegister.h>

#ifndef L3V2EXTENDEDMATH_CREATE_NS
#define L3V2EXTENDEDMATH_CREATE_NS(variable, sbmlns)\
EXTENSION_CREATE_NS(L3v2extendedmathPkgNamespaces, variable, sbmlns);
#endif

#include <vector>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN L3v2extendedmathExtension : public SBMLExtension
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
  * Returns the XML namespace URI of the SBML Level&nbsp;3 package automatically
  * included in L3V2.
  *
  * @return the XML namespace, as a string.
  *
  * @copydetails doc_note_static_methods
  */
  static const std::string& getXmlnsL3V2();


  /**
   * Creates a new L3v2extendedmathExtension instance.
   */
  L3v2extendedmathExtension();


  /**
   * Copy constructor for L3v2extendedmathExtension.
   *
   * @param orig the L3v2extendedmathExtension instance to copy.
   */
  L3v2extendedmathExtension(const L3v2extendedmathExtension& orig);


  /**
   * Assignment operator for L3v2extendedmathExtension.
   *
   * @param rhs the L3v2extendedmathExtension object whose values are to be
   * used as the basis of the assignment.
   */
  L3v2extendedmathExtension& operator=(const L3v2extendedmathExtension& rhs);


  /**
   * Creates and returns a deep copy of this L3v2extendedmathExtension object.
   *
   * @return a (deep) copy of this L3v2extendedmathExtension object.
   */
  virtual L3v2extendedmathExtension* clone() const;


  /**
   * Destructor for L3v2extendedmathExtension.
   */
  virtual ~L3v2extendedmathExtension();


  /**
   * Returns the name of this SBML Level&nbsp;3 package ("l3v2extendedmath").
   *
   * @return a string representing the name of this package
   * ("l3v2extendedmath").
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
   * @return a string representing the name of this package
   * ("l3v2extendedmath").
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
   * the "l3v2extendedmath" package.
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
   * the "l3v2extendedmath" package.
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
   * the "l3v2extendedmath" package.
   *
   * @return the version of the SBML Level&nbsp;3 package for the given URI of
   * this package, or @c 0 if the given URI is invalid, or for a different
   * package.
   */
  virtual unsigned int getPackageVersion(const std::string& uri) const;


  /**
   * Returns a L3v2extendedmathPkgNamespaces object.
   *
   * @param uri the string of the URI that represents one of the versions of
   * the "l3v2extendedmath" package.
   *
   * @return L3v2extendedmathPkgNamespaces object corresponding to the given
   * URI of this package, or @c NULL if the given URI is not defined in the
   * "l3v2extendedmath" package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string& uri)
    const;


  /**
   * Takes a type code of the &ldquo;l3v2extendedmath&rdquo; package and
   * returns a string describing the code.
   *
   * @param typeCode a libSBML type code defined by the libSBML extension
   * implementing support for the SBML Level&nbsp;3
   * &ldquo;l3v2extendedmath&rdquo; package.
   *
   * @return a text string representing the type code given by @p typeCode. If
   * the type code is unrecognized for this implementation of the libSBML
   * &ldquo;l3v2extendedmath&rdquo; package, the string returned will be
   * <code>"(Unknown SBML L3v2extendedmath Type)"</code>.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the entry in the error table at this index.
   *
   * @param index an unsigned integer representing the index of the error.
   *
   * @return packageErrorTableEntry object in the
   * L3v2extendedmathSBMLErrorTable.
   */
  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the index in the error table with the given errorId.
   *
   * @param errorId an unsigned integer representing the errorId of the error.
   *
   * @return unsigned int representing the index in the
   * L3v2extendedmathSBMLErrorTable corresponding to the errorId given.
   */
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the offset for the errorId range for the "l3v2extendedmath"
   * package.
   *
   * @return unsigned int representing the offset for errors in the
   * L3v2extendedmathSBMLErrorTable.
   */
  virtual unsigned int getErrorIdOffset() const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Initializes l3v2extendedmath extension by creating an object of this class
   * with the required SBasePlugin derived objects and registering the object
   * to the SBMLExtensionRegistry class
   *
   * This function is automatically invoked when creatingthe following global
   * object in L3v2extendedmathExtension.cpp
   *
   * static SBMLExtensionRegister<L3v2extendedmathExtension>
   * l3v2extendedmathExtensionRegistry;
   */
  static void init();

  /** @endcond */


};

/**
 *
 * Required typedef definitions
 *
 * L3v2extendedmathPkgNamespace is derived from SBMLNamespaces class and used
 * when creating an object of SBase derived classes defined in the
 * l3v2extendedmath package
 *
 * SBMLExtensionNamespaces<L3v2extendedmathExtension> must be instantiated in
 * L3v2extendedmathExtension.cpp for DLL
 *
 */
typedef SBMLExtensionNamespaces<L3v2extendedmathExtension>
  L3v2extendedmathPkgNamespaces;


LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_BEGIN


/**
 * @enum SBMLL3v2extendedmathTypeCode_t
 * @brief SBMLL3v2extendedmathTypeCode_t Enumeration of possible types in the
 * libSBML &ldquo;l3v2extendedmath&rdquo; package implementation.
 *
 * @copydetails doc_what_are_typecodes
 *
 * @copydetails doc_additional_typecode_details
 */
typedef enum
{
} SBMLL3v2extendedmathTypeCode_t;


///**
// * @enum AST_L3V2_TYPES_t
// * @brief Enumeration of values permitted as the value of the "st" attribute on
// * A objects.
// *
// * @if conly
// * @see A_getSt()
// * @see A_setSt()
// * @elseif java
// * @see A::getSt()
// * @see A::setSt(long)
// * @else
// * @see A::getSt()
// * @see A::setSt()
// * @endif
// */
//typedef enum
//{
//  A_ST_L3V2_TYPES_FUNCTION_MAX            /*!< The a st is @c "max". */
//, A_ST_L3V2_TYPES_FUNCTION_MIN            /*!< The a st is @c "min". */
//, A_ST_L3V2_TYPES_FUNCTION_QUOTIENT       /*!< The a st is @c "quotient". */
//, FUNCTION_RATE_OF                        /*!< The function rate is @c "rateOf". */
//, A_ST_L3V2_TYPES_FUNCTION_REM            /*!< The a st is @c "rem". */
//, A_ST_L3V2_TYPES_LOGICAL_IMPLIES         /*!< The a st is @c "implies". */
//, FUNCTION_RATE_INVALID                   /*!< Invalid AST_L3V2_TYPES value. */
//} AST_L3V2_TYPES_t;
//
//
///**
// * Returns the string version of the provided #AST_L3V2_TYPES_t enumeration.
// *
// * @param astlvtypes the #AST_L3V2_TYPES_t enumeration value to convert.
// *
// * @return A string corresponding to the given type:
// * "max",
// * "min",
// * "quotient",
// * "rateOf",
// * "rem",
// * "implies",
// * or @c NULL if the value is @sbmlconstant{FUNCTION_RATE_INVALID,
// * AST_L3V2_TYPES_t} or another invalid enumeration value.
// *
// * @copydetails doc_returned_unowned_char
// *
// * @if conly
// * @memberof A_t
// * @endif
// */
//LIBSBML_EXTERN
//const char*
//AST_L3V2_TYPES_toString(AST_L3V2_TYPES_t astlvtypes);
//
//
///**
// * Returns the #AST_L3V2_TYPES_t enumeration corresponding to the given string
// * or @sbmlconstant{FUNCTION_RATE_INVALID, AST_L3V2_TYPES_t} if there is no
// * such match.
// *
// * @param code the string to convert to a #AST_L3V2_TYPES_t.
// *
// * @return the corresponding #AST_L3V2_TYPES_t or
// * @sbmlconstant{FUNCTION_RATE_INVALID, AST_L3V2_TYPES_t} if no match is found.
// *
// * @note The matching is case-sensitive: "max" will return
// * @sbmlconstant{A_ST_L3V2_TYPES_FUNCTION_MAX, AST_L3V2_TYPES_t}, but "Max"
// * will return @sbmlconstant{FUNCTION_RATE_INVALID, AST_L3V2_TYPES_t}.
// *
// * @if conly
// * @memberof A_t
// * @endif
// */
//LIBSBML_EXTERN
//AST_L3V2_TYPES_t
//AST_L3V2_TYPES_fromString(const char* code);
//
//
///**
// * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
// * given #AST_L3V2_TYPES_t is valid.
// *
// * @param astlvtypes the #AST_L3V2_TYPES_t enumeration to query.
// *
// * @return @c 1 (true) if the #AST_L3V2_TYPES_t is
// * @sbmlconstant{A_ST_L3V2_TYPES_FUNCTION_MAX, AST_L3V2_TYPES_t},
// * @sbmlconstant{A_ST_L3V2_TYPES_FUNCTION_MIN, AST_L3V2_TYPES_t},
// * @sbmlconstant{A_ST_L3V2_TYPES_FUNCTION_QUOTIENT, AST_L3V2_TYPES_t},
// * @sbmlconstant{FUNCTION_RATE_OF, AST_L3V2_TYPES_t},
// * @sbmlconstant{A_ST_L3V2_TYPES_FUNCTION_REM, AST_L3V2_TYPES_t}, or
// * @sbmlconstant{A_ST_L3V2_TYPES_LOGICAL_IMPLIES, AST_L3V2_TYPES_t};
// * @c 0 (false) otherwise (including @sbmlconstant{FUNCTION_RATE_INVALID,
// * AST_L3V2_TYPES_t}).
// *
// * @if conly
// * @memberof A_t
// * @endif
// */
//LIBSBML_EXTERN
//int
//AST_L3V2_TYPES_isValid(AST_L3V2_TYPES_t astlvtypes);
//
//
///**
// * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
// * given string is a valid #AST_L3V2_TYPES_t.
// *
// * @param code the string to query.
// *
// * @return @c 1 (true) if the string is
// * "max",
// * "min",
// * "quotient",
// * "rateOf",
// * "rem", or
// * "implies";
// * @c 0 (false) otherwise.
// *
// * @note The matching is case-sensitive: "max" will return @c 1 (true), but
// * "Max" will return @c 0 (false).
// *
// * @if conly
// * @memberof A_t
// * @endif
// */
//LIBSBML_EXTERN
//int
//AST_L3V2_TYPES_isValidString(const char* code);
//
//
//

LIBSBML_CPP_NAMESPACE_END




#endif /* !L3v2extendedmathExtension_H__ */


