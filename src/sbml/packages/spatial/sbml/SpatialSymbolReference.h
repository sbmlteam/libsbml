/**
 * @file:   SpatialSymbolReference.h
 * @brief:  Implementation of the SpatialSymbolReference class
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


#ifndef SpatialSymbolReference_H__
#define SpatialSymbolReference_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN SpatialSymbolReference : public SBase
{

protected:

  std::string   mSpatialRef;


public:

  /**
   * Creates a new SpatialSymbolReference with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this SpatialSymbolReference
   *
   * @param version an unsigned int, the SBML Version to assign to this SpatialSymbolReference
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this SpatialSymbolReference
   */
  SpatialSymbolReference(unsigned int level      = SpatialExtension::getDefaultLevel(),
                         unsigned int version    = SpatialExtension::getDefaultVersion(),
                         unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpatialSymbolReference with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  SpatialSymbolReference(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for SpatialSymbolReference.
   *
   * @param orig; the SpatialSymbolReference instance to copy.
   */
  SpatialSymbolReference(const SpatialSymbolReference& orig);


   /**
   * Assignment operator for SpatialSymbolReference.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  SpatialSymbolReference& operator=(const SpatialSymbolReference& rhs);


   /**
   * Creates and returns a deep copy of this SpatialSymbolReference object.
   *
   * @return a (deep) copy of this SpatialSymbolReference object.
   */
  virtual SpatialSymbolReference* clone () const;


   /**
   * Destructor for SpatialSymbolReference.
   */
  virtual ~SpatialSymbolReference();


   /**
   * Returns the value of the "spatialRef" attribute of this SpatialSymbolReference.
   *
   * @return the value of the "spatialRef" attribute of this SpatialSymbolReference as a string.
   */
  virtual const std::string& getSpatialRef() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialSymbolReference's "spatialRef" attribute has been set.
   *
   * @return @c true if this SpatialSymbolReference's "spatialRef" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialRef() const;


  /**
   * Sets the value of the "spatialRef" attribute of this SpatialSymbolReference.
   *
   * @param spatialRef; const std::string& value of the "spatialRef" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setSpatialRef(const std::string& spatialRef);


  /**
   * Unsets the value of the "spatialRef" attribute of this SpatialSymbolReference.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSpatialRef();


  /**
   * Renames all the @c SIdRef attributes on this element, including any
   * found in MathML content (if such exists).
   *
   * This method works by looking at all attributes and (if appropriate)
   * mathematical formulas, comparing the identifiers to the value of @p
   * oldid.  If any matches are found, the matching identifiers are replaced
   * with @p newid.  The method does @em not descend into child elements.
   *
   * @param oldid the old identifier
   * @param newid the new identifier
   */
   virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);


  /**
   * Returns the XML element name of this object, which for SpatialSymbolReference, is
   * always @c "spatialSymbolReference".
   *
   * @return the name of this element, i.e. @c "spatialSymbolReference".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;


  /**
   * Predicate returning @c true if all the required attributes
   * for this SpatialSymbolReference object have been set.
   *
   * @note The required attributes for a SpatialSymbolReference object are:
   * @li "spatialRef"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);


  /** @endcond doxygenLibsbmlInternal */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new SpatialSymbolReference_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * SpatialSymbolReference_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * SpatialSymbolReference_t structure.
 *
 * @returns the newly-created SpatialSymbolReference_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof SpatialSymbolReference_t
 */
LIBSBML_EXTERN
SpatialSymbolReference_t *
SpatialSymbolReference_create(unsigned int level, unsigned int version,
                              unsigned int pkgVersion);


/**
 * Frees the given SpatialSymbolReference_t structure.
 * 
 * @param ssr the SpatialSymbolReference_t structure to be freed.
 *
 * @memberof SpatialSymbolReference_t
 */
LIBSBML_EXTERN
void
SpatialSymbolReference_free(SpatialSymbolReference_t * ssr);


/**
 * Creates a deep copy of the given SpatialSymbolReference_t structure.
 * 
 * @param ssr the SpatialSymbolReference_t structure to be copied.
 *
 * @returns a (deep) copy of the given SpatialSymbolReference_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof SpatialSymbolReference_t
 */
LIBSBML_EXTERN
SpatialSymbolReference_t *
SpatialSymbolReference_clone(SpatialSymbolReference_t * ssr);


/**
 * Returns the value of the "spatialRef" attribute of the given SpatialSymbolReference_t
 * structure.
 *
 * @param ssr the SpatialSymbolReference_t structure.
 *
 * @return the spatialRef of this structure.
 *
 * @member of SpatialSymbolReference_t
 */
LIBSBML_EXTERN
const char *
SpatialSymbolReference_getSpatialRef(const SpatialSymbolReference_t * ssr);


/**
 * Predicate returning @c 1 if the given SpatialSymbolReference_t structure's "spatialRef"
 * is set.
 *
 * @param ssr the SpatialSymbolReference_t structure.
 *
 * @return @c 1 if the "spatialRef" of this SpatialSymbolReference_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SpatialSymbolReference_t
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_isSetSpatialRef(const SpatialSymbolReference_t * ssr);


/**
 * Sets the "spatialRef" attribute of the given SpatialSymbolReference_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs SpatialSymbolReference_unsetSpatialRef() instead.
 *
 * @param ssr the SpatialSymbolReference_t structure.
 *
 * @param spatialRef the string to which the structures "spatialRef" attribute should be
 * set.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of SpatialSymbolReference_t
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_setSpatialRef(SpatialSymbolReference_t * ssr, const char * spatialRef);


/**
 * Unsets the value of the "spatialRef" attribute of the given 
 *SpatialSymbolReference_t structure.
 *
 * @param ssr the SpatialSymbolReference_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SpatialSymbolReference_t
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_unsetSpatialRef(SpatialSymbolReference_t * ssr);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given SpatialSymbolReference_t structure have been set.
 *
 * @param ssr the SpatialSymbolReference_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of SpatialSymbolReference_t
 */
LIBSBML_EXTERN
int
SpatialSymbolReference_hasRequiredAttributes(const SpatialSymbolReference_t * ssr);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpatialSymbolReference_H__  */

