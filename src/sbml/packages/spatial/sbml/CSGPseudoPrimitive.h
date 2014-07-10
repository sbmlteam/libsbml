/**
 * @file:   CSGPseudoPrimitive.h
 * @brief:  Implementation of the CSGPseudoPrimitive class
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


#ifndef CSGPseudoPrimitive_H__
#define CSGPseudoPrimitive_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN CSGPseudoPrimitive : public CSGNode
{

protected:

  std::string   mCsgObjectRef;


public:

  /**
   * Creates a new CSGPseudoPrimitive with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGPseudoPrimitive
   *
   * @param version an unsigned int, the SBML Version to assign to this CSGPseudoPrimitive
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CSGPseudoPrimitive
   */
  CSGPseudoPrimitive(unsigned int level      = SpatialExtension::getDefaultLevel(),
                     unsigned int version    = SpatialExtension::getDefaultVersion(),
                     unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGPseudoPrimitive with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CSGPseudoPrimitive(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CSGPseudoPrimitive.
   *
   * @param orig; the CSGPseudoPrimitive instance to copy.
   */
  CSGPseudoPrimitive(const CSGPseudoPrimitive& orig);


   /**
   * Assignment operator for CSGPseudoPrimitive.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CSGPseudoPrimitive& operator=(const CSGPseudoPrimitive& rhs);


   /**
   * Creates and returns a deep copy of this CSGPseudoPrimitive object.
   *
   * @return a (deep) copy of this CSGPseudoPrimitive object.
   */
  virtual CSGPseudoPrimitive* clone () const;


   /**
   * Destructor for CSGPseudoPrimitive.
   */
  virtual ~CSGPseudoPrimitive();


   /**
   * Returns the value of the "csgObjectRef" attribute of this CSGPseudoPrimitive.
   *
   * @return the value of the "csgObjectRef" attribute of this CSGPseudoPrimitive as a string.
   */
  virtual const std::string& getCsgObjectRef() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGPseudoPrimitive's "csgObjectRef" attribute has been set.
   *
   * @return @c true if this CSGPseudoPrimitive's "csgObjectRef" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCsgObjectRef() const;


  /**
   * Sets the value of the "csgObjectRef" attribute of this CSGPseudoPrimitive.
   *
   * @param csgObjectRef; const std::string& value of the "csgObjectRef" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCsgObjectRef(const std::string& csgObjectRef);


  /**
   * Unsets the value of the "csgObjectRef" attribute of this CSGPseudoPrimitive.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCsgObjectRef();


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
   * Returns the XML element name of this object, which for CSGPseudoPrimitive, is
   * always @c "cSGPseudoPrimitive".
   *
   * @return the name of this element, i.e. @c "cSGPseudoPrimitive".
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
   * for this CSGPseudoPrimitive object have been set.
   *
   * @note The required attributes for a CSGPseudoPrimitive object are:
   * @li "csgObjectRef"
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
   * return the SBML object corresponding to next XMLToken.
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


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
 * Creates a new CSGPseudoPrimitive_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CSGPseudoPrimitive_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CSGPseudoPrimitive_t structure.
 *
 * @returns the newly-created CSGPseudoPrimitive_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGPseudoPrimitive_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion);


/**
 * Frees the given CSGPseudoPrimitive_t structure.
 * 
 * @param csgpp the CSGPseudoPrimitive_t structure to be freed.
 *
 * @memberof CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
void
CSGPseudoPrimitive_free(CSGPseudoPrimitive_t * csgpp);


/**
 * Creates a deep copy of the given CSGPseudoPrimitive_t structure.
 * 
 * @param csgpp the CSGPseudoPrimitive_t structure to be copied.
 *
 * @returns a (deep) copy of the given CSGPseudoPrimitive_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
CSGPseudoPrimitive_t *
CSGPseudoPrimitive_clone(CSGPseudoPrimitive_t * csgpp);


/**
 * Returns the value of the "csgObjectRef" attribute of the given CSGPseudoPrimitive_t
 * structure.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure.
 *
 * @return the csgObjectRef of this structure.
 *
 * @member of CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
const char *
CSGPseudoPrimitive_getCsgObjectRef(const CSGPseudoPrimitive_t * csgpp);


/**
 * Predicate returning @c 1 if the given CSGPseudoPrimitive_t structure's "csgObjectRef"
 * is set.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure.
 *
 * @return @c 1 if the "csgObjectRef" of this CSGPseudoPrimitive_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_isSetCsgObjectRef(const CSGPseudoPrimitive_t * csgpp);


/**
 * Sets the "csgObjectRef" attribute of the given CSGPseudoPrimitive_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs CSGPseudoPrimitive_unsetCsgObjectRef() instead.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure.
 *
 * @param csgObjectRef the string to which the structures "csgObjectRef" attribute should be
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
 * @member of CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_setCsgObjectRef(CSGPseudoPrimitive_t * csgpp, const char * csgObjectRef);


/**
 * Unsets the value of the "csgObjectRef" attribute of the given 
 *CSGPseudoPrimitive_t structure.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_unsetCsgObjectRef(CSGPseudoPrimitive_t * csgpp);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CSGPseudoPrimitive_t structure have been set.
 *
 * @param csgpp the CSGPseudoPrimitive_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGPseudoPrimitive_t
 */
LIBSBML_EXTERN
int
CSGPseudoPrimitive_hasRequiredAttributes(const CSGPseudoPrimitive_t * csgpp);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CSGPseudoPrimitive_H__  */

