/**
 * @file:   GeneProductRef.h
 * @brief:  Implementation of the GeneProductRef class
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
 *
 * @class GeneProductRef
 * @sbmlbrief{fbc} A GeneProductRef is an FbcAssociation that relates the parent 
 * Reaction to a GeneProduct.
 *
 * A GeneProductRef is a child of a Reaction, and points to a GeneProduct 
 * involved in that Reaction.
 *
 * @copydetails fbcv2_annotation_replacement
 */


#ifndef GeneProductRef_H__
#define GeneProductRef_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/sbml/FbcAssociation.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN GeneProductRef : public FbcAssociation
{

protected:

  /** @cond doxygenLibsbmlInternal */
  std::string   mId;
  std::string   mGeneProduct;
  std::string   mName;
  /** @endcond */


public:

  /**
   * Creates a new GeneProductRef with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this GeneProductRef
   *
   * @param version an unsigned int, the SBML Version to assign to this GeneProductRef
   *
   * @param pkgVersion an unsigned int, the SBML Fbc Version to assign to this GeneProductRef
   */
  GeneProductRef(unsigned int level      = FbcExtension::getDefaultLevel(),
                 unsigned int version    = FbcExtension::getDefaultVersion(),
                 unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new GeneProductRef with the given FbcPkgNamespaces object.
   *
   * @param fbcns the FbcPkgNamespaces object
   */
  GeneProductRef(FbcPkgNamespaces* fbcns);


   /**
   * Copy constructor for GeneProductRef.
   *
   * @param orig; the GeneProductRef instance to copy.
   */
  GeneProductRef(const GeneProductRef& orig);


   /**
   * Assignment operator for GeneProductRef.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  GeneProductRef& operator=(const GeneProductRef& rhs);


   /**
   * Creates and returns a deep copy of this GeneProductRef object.
   *
   * @return a (deep) copy of this GeneProductRef object.
   */
  virtual GeneProductRef* clone () const;


   /**
   * Destructor for GeneProductRef.
   */
  virtual ~GeneProductRef();


   /**
   * Returns the value of the "id" attribute of this GeneProductRef.
   *
   * @return the value of the "id" attribute of this GeneProductRef as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "geneProduct" attribute of this GeneProductRef.
   *
   * @return the value of the "geneProduct" attribute of this GeneProductRef as a string.
   */
  virtual const std::string& getGeneProduct() const;

  /**
  * Converts this FbcAssociation object into an infix string representation.
  *
  * @return the association as infix string.
  */
  virtual std::string toInfix() const;

  /**
   * Returns the value of the "name" attribute of this GeneProductRef.
   *
   * @return the value of the "name" attribute of this GeneProductRef as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeneProductRef's "id" attribute has been set.
   *
   * @return @c true if this GeneProductRef's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeneProductRef's "geneProduct" attribute has been set.
   *
   * @return @c true if this GeneProductRef's "geneProduct" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetGeneProduct() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeneProductRef's "name" attribute has been set.
   *
   * @return @c true if this GeneProductRef's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this GeneProductRef.
   *
   * @param id; const std::string& value of the "id" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setId(const std::string& id);


  /**
   * Sets the value of the "geneProduct" attribute of this GeneProductRef.
   *
   * @param geneProduct; const std::string& value of the "geneProduct" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setGeneProduct(const std::string& geneProduct);


  /**
   * Sets the value of the "name" attribute of this GeneProductRef.
   *
   * @param name; const std::string& value of the "name" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "id" attribute of this GeneProductRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "geneProduct" attribute of this GeneProductRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetGeneProduct();


  /**
   * Unsets the value of the "name" attribute of this GeneProductRef.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetName();


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
   * Returns the XML element name of this object, which for GeneProductRef, is
   * always @c "geneProductRef".
   *
   * @return the name of this element, i.e. @c "geneProductRef".
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
   * for this GeneProductRef object have been set.
   *
   * @note The required attributes for a GeneProductRef object are:
   * @li "geneProduct"
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
 * Creates a new GeneProductRef_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * GeneProductRef_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * GeneProductRef_t structure.
 *
 * @returns the newly-created GeneProductRef_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof GeneProductRef_t
 */
LIBSBML_EXTERN
GeneProductRef_t *
GeneProductRef_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion);


/**
 * Frees the given GeneProductRef_t structure.
 * 
 * @param gpr the GeneProductRef_t structure to be freed.
 *
 * @memberof GeneProductRef_t
 */
LIBSBML_EXTERN
void
GeneProductRef_free(GeneProductRef_t * gpr);


/**
 * Creates a deep copy of the given GeneProductRef_t structure.
 * 
 * @param gpr the GeneProductRef_t structure to be copied.
 *
 * @returns a (deep) copy of the given GeneProductRef_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof GeneProductRef_t
 */
LIBSBML_EXTERN
GeneProductRef_t *
GeneProductRef_clone(GeneProductRef_t * gpr);


/**
 * Returns the value of the "id" attribute of the given GeneProductRef_t
 * structure.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @return the id of this structure.
 *
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
const char *
GeneProductRef_getId(const GeneProductRef_t * gpr);


/**
 * Returns the value of the "geneProduct" attribute of the given GeneProductRef_t
 * structure.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @return the geneProduct of this structure.
 *
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
const char *
GeneProductRef_getGeneProduct(const GeneProductRef_t * gpr);


/**
 * Returns the value of the "name" attribute of the given GeneProductRef_t
 * structure.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @return the name of this structure.
 *
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
const char *
GeneProductRef_getName(const GeneProductRef_t * gpr);


/**
 * Predicate returning @c 1 if the given GeneProductRef_t structure's "id"
 * is set.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @return @c 1 if the "id" of this GeneProductRef_t structure is
 * set, @c 0 otherwise.
 *
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
int
GeneProductRef_isSetId(const GeneProductRef_t * gpr);


/**
 * Predicate returning @c 1 if the given GeneProductRef_t structure's "geneProduct"
 * is set.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @return @c 1 if the "geneProduct" of this GeneProductRef_t structure is
 * set, @c 0 otherwise.
 *
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
int
GeneProductRef_isSetGeneProduct(const GeneProductRef_t * gpr);


/**
 * Predicate returning @c 1 if the given GeneProductRef_t structure's "name"
 * is set.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @return @c 1 if the "name" of this GeneProductRef_t structure is
 * set, @c 0 otherwise.
 *
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
int
GeneProductRef_isSetName(const GeneProductRef_t * gpr);


/**
 * Sets the "id" attribute of the given GeneProductRef_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs GeneProductRef_unsetId() instead.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @param id the string to which the structures "id" attribute should be
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
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
int
GeneProductRef_setId(GeneProductRef_t * gpr, const char * id);


/**
 * Sets the "geneProduct" attribute of the given GeneProductRef_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs GeneProductRef_unsetGeneProduct() instead.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @param geneProduct the string to which the structures "geneProduct" attribute should be
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
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
int
GeneProductRef_setGeneProduct(GeneProductRef_t * gpr, const char * geneProduct);


/**
 * Sets the "name" attribute of the given GeneProductRef_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs GeneProductRef_unsetName() instead.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @param name the string to which the structures "name" attribute should be
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
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
int
GeneProductRef_setName(GeneProductRef_t * gpr, const char * name);


/**
 * Unsets the value of the "id" attribute of the given 
 * GeneProductRef_t structure.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
int
GeneProductRef_unsetId(GeneProductRef_t * gpr);


/**
 * Unsets the value of the "geneProduct" attribute of the given 
 * GeneProductRef_t structure.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
int
GeneProductRef_unsetGeneProduct(GeneProductRef_t * gpr);


/**
 * Unsets the value of the "name" attribute of the given 
 * GeneProductRef_t structure.
 *
 * @param gpr the GeneProductRef_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
int
GeneProductRef_unsetName(GeneProductRef_t * gpr);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given GeneProductRef_t structure have been set.
 *
 * @param gpr the GeneProductRef_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of GeneProductRef_t
 */
LIBSBML_EXTERN
int
GeneProductRef_hasRequiredAttributes(const GeneProductRef_t * gpr);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  GeneProductRef_H__  */

