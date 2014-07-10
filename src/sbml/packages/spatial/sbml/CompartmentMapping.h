/**
 * @file:   CompartmentMapping.h
 * @brief:  Implementation of the CompartmentMapping class
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


#ifndef CompartmentMapping_H__
#define CompartmentMapping_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN CompartmentMapping : public SBase
{

protected:

  std::string   mId;
  std::string   mDomainType;
  double        mUnitSize;
  bool          mIsSetUnitSize;


public:

  /**
   * Creates a new CompartmentMapping with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CompartmentMapping
   *
   * @param version an unsigned int, the SBML Version to assign to this CompartmentMapping
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CompartmentMapping
   */
  CompartmentMapping(unsigned int level      = SpatialExtension::getDefaultLevel(),
                     unsigned int version    = SpatialExtension::getDefaultVersion(),
                     unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CompartmentMapping with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CompartmentMapping(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CompartmentMapping.
   *
   * @param orig; the CompartmentMapping instance to copy.
   */
  CompartmentMapping(const CompartmentMapping& orig);


   /**
   * Assignment operator for CompartmentMapping.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CompartmentMapping& operator=(const CompartmentMapping& rhs);


   /**
   * Creates and returns a deep copy of this CompartmentMapping object.
   *
   * @return a (deep) copy of this CompartmentMapping object.
   */
  virtual CompartmentMapping* clone () const;


   /**
   * Destructor for CompartmentMapping.
   */
  virtual ~CompartmentMapping();


   /**
   * Returns the value of the "id" attribute of this CompartmentMapping.
   *
   * @return the value of the "id" attribute of this CompartmentMapping as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "domainType" attribute of this CompartmentMapping.
   *
   * @return the value of the "domainType" attribute of this CompartmentMapping as a string.
   */
  virtual const std::string& getDomainType() const;


  /**
   * Returns the value of the "unitSize" attribute of this CompartmentMapping.
   *
   * @return the value of the "unitSize" attribute of this CompartmentMapping as a double.
   */
  virtual double getUnitSize() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CompartmentMapping's "id" attribute has been set.
   *
   * @return @c true if this CompartmentMapping's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CompartmentMapping's "domainType" attribute has been set.
   *
   * @return @c true if this CompartmentMapping's "domainType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetDomainType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CompartmentMapping's "unitSize" attribute has been set.
   *
   * @return @c true if this CompartmentMapping's "unitSize" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetUnitSize() const;


  /**
   * Sets the value of the "id" attribute of this CompartmentMapping.
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
   * Sets the value of the "domainType" attribute of this CompartmentMapping.
   *
   * @param domainType; const std::string& value of the "domainType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setDomainType(const std::string& domainType);


  /**
   * Sets the value of the "unitSize" attribute of this CompartmentMapping.
   *
   * @param unitSize; double value of the "unitSize" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setUnitSize(double unitSize);


  /**
   * Unsets the value of the "id" attribute of this CompartmentMapping.
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
   * Unsets the value of the "domainType" attribute of this CompartmentMapping.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDomainType();


  /**
   * Unsets the value of the "unitSize" attribute of this CompartmentMapping.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetUnitSize();


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
   * Returns the XML element name of this object, which for CompartmentMapping, is
   * always @c "compartmentMapping".
   *
   * @return the name of this element, i.e. @c "compartmentMapping".
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
   * for this CompartmentMapping object have been set.
   *
   * @note The required attributes for a CompartmentMapping object are:
   * @li "id"
   * @li "domainType"
   * @li "unitSize"
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
 * Creates a new CompartmentMapping_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CompartmentMapping_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CompartmentMapping_t structure.
 *
 * @returns the newly-created CompartmentMapping_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
CompartmentMapping_t *
CompartmentMapping_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion);


/**
 * Frees the given CompartmentMapping_t structure.
 * 
 * @param cm the CompartmentMapping_t structure to be freed.
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
void
CompartmentMapping_free(CompartmentMapping_t * cm);


/**
 * Creates a deep copy of the given CompartmentMapping_t structure.
 * 
 * @param cm the CompartmentMapping_t structure to be copied.
 *
 * @returns a (deep) copy of the given CompartmentMapping_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CompartmentMapping_t
 */
LIBSBML_EXTERN
CompartmentMapping_t *
CompartmentMapping_clone(CompartmentMapping_t * cm);


/**
 * Returns the value of the "id" attribute of the given CompartmentMapping_t
 * structure.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return the id of this structure.
 *
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
const char *
CompartmentMapping_getId(const CompartmentMapping_t * cm);


/**
 * Returns the value of the "domainType" attribute of the given CompartmentMapping_t
 * structure.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return the domainType of this structure.
 *
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
const char *
CompartmentMapping_getDomainType(const CompartmentMapping_t * cm);


/**
 * Returns the value of the "unitSize" attribute of the given CompartmentMapping_t
 * structure.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return the unitSize of this structure.
 *
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
double
CompartmentMapping_getUnitSize(const CompartmentMapping_t * cm);


/**
 * Predicate returning @c 1 if the given CompartmentMapping_t structure's "id"
 * is set.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return @c 1 if the "id" of this CompartmentMapping_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_isSetId(const CompartmentMapping_t * cm);


/**
 * Predicate returning @c 1 if the given CompartmentMapping_t structure's "domainType"
 * is set.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return @c 1 if the "domainType" of this CompartmentMapping_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_isSetDomainType(const CompartmentMapping_t * cm);


/**
 * Predicate returning @c 1 if the given CompartmentMapping_t structure's "unitSize"
 * is set.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return @c 1 if the "unitSize" of this CompartmentMapping_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_isSetUnitSize(const CompartmentMapping_t * cm);


/**
 * Sets the "id" attribute of the given CompartmentMapping_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs CompartmentMapping_unsetId() instead.
 *
 * @param cm the CompartmentMapping_t structure.
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
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_setId(CompartmentMapping_t * cm, const char * id);


/**
 * Sets the "domainType" attribute of the given CompartmentMapping_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs CompartmentMapping_unsetDomainType() instead.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @param domainType the string to which the structures "domainType" attribute should be
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
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_setDomainType(CompartmentMapping_t * cm, const char * domainType);


/**
 * Sets the "unitSize" attribute of the given CompartmentMapping_t structure.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @param unitSize the string to which the structures "unitSize" attribute should be
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
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_setUnitSize(CompartmentMapping_t * cm, double unitSize);


/**
 * Unsets the value of the "id" attribute of the given 
 *CompartmentMapping_t structure.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_unsetId(CompartmentMapping_t * cm);


/**
 * Unsets the value of the "domainType" attribute of the given 
 *CompartmentMapping_t structure.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_unsetDomainType(CompartmentMapping_t * cm);


/**
 * Unsets the value of the "unitSize" attribute of the given 
 *CompartmentMapping_t structure.
 *
 * @param cm the CompartmentMapping_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_unsetUnitSize(CompartmentMapping_t * cm);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CompartmentMapping_t structure have been set.
 *
 * @param cm the CompartmentMapping_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CompartmentMapping_t
 */
LIBSBML_EXTERN
int
CompartmentMapping_hasRequiredAttributes(const CompartmentMapping_t * cm);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CompartmentMapping_H__  */

