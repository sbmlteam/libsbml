/**
 * @file:   BoundaryCondition.h
 * @brief:  Implementation of the BoundaryCondition class
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


#ifndef BoundaryCondition_H__
#define BoundaryCondition_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN BoundaryCondition : public SBase
{

protected:

  std::string   mVariable;
  BoundaryConditionKind_t   mType;
  std::string   mCoordinateBoundary;
  std::string   mBoundaryDomainType;


public:

  /**
   * Creates a new BoundaryCondition with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this BoundaryCondition
   *
   * @param version an unsigned int, the SBML Version to assign to this BoundaryCondition
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this BoundaryCondition
   */
  BoundaryCondition(unsigned int level      = SpatialExtension::getDefaultLevel(),
                    unsigned int version    = SpatialExtension::getDefaultVersion(),
                    unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new BoundaryCondition with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  BoundaryCondition(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for BoundaryCondition.
   *
   * @param orig; the BoundaryCondition instance to copy.
   */
  BoundaryCondition(const BoundaryCondition& orig);


   /**
   * Assignment operator for BoundaryCondition.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  BoundaryCondition& operator=(const BoundaryCondition& rhs);


   /**
   * Creates and returns a deep copy of this BoundaryCondition object.
   *
   * @return a (deep) copy of this BoundaryCondition object.
   */
  virtual BoundaryCondition* clone () const;


   /**
   * Destructor for BoundaryCondition.
   */
  virtual ~BoundaryCondition();


   /**
   * Returns the value of the "variable" attribute of this BoundaryCondition.
   *
   * @return the value of the "variable" attribute of this BoundaryCondition as a string.
   */
  virtual const std::string& getVariable() const;


  /**
   * Returns the value of the "type" attribute of this BoundaryCondition.
   *
   * @return the value of the "type" attribute of this BoundaryCondition as a BoundaryConditionKind_t.
   */
  virtual BoundaryConditionKind_t getType() const;


  /**
   * Returns the value of the "coordinateBoundary" attribute of this BoundaryCondition.
   *
   * @return the value of the "coordinateBoundary" attribute of this BoundaryCondition as a string.
   */
  virtual const std::string& getCoordinateBoundary() const;


  /**
   * Returns the value of the "boundaryDomainType" attribute of this BoundaryCondition.
   *
   * @return the value of the "boundaryDomainType" attribute of this BoundaryCondition as a string.
   */
  virtual const std::string& getBoundaryDomainType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * BoundaryCondition's "variable" attribute has been set.
   *
   * @return @c true if this BoundaryCondition's "variable" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetVariable() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * BoundaryCondition's "type" attribute has been set.
   *
   * @return @c true if this BoundaryCondition's "type" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * BoundaryCondition's "coordinateBoundary" attribute has been set.
   *
   * @return @c true if this BoundaryCondition's "coordinateBoundary" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCoordinateBoundary() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * BoundaryCondition's "boundaryDomainType" attribute has been set.
   *
   * @return @c true if this BoundaryCondition's "boundaryDomainType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetBoundaryDomainType() const;


  /**
   * Sets the value of the "variable" attribute of this BoundaryCondition.
   *
   * @param variable; const std::string& value of the "variable" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setVariable(const std::string& variable);


  /**
   * Sets the value of the "type" attribute of this BoundaryCondition.
   *
   * @param type; BoundaryConditionKind_t value of the "type" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setType(BoundaryConditionKind_t type);


  /**
   * Sets the value of the "type" attribute of this BoundaryCondition.
   *
   * @param type; string value of the "type" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setType(const std::string& type);


  /**
   * Sets the value of the "coordinateBoundary" attribute of this BoundaryCondition.
   *
   * @param coordinateBoundary; const std::string& value of the "coordinateBoundary" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoordinateBoundary(const std::string& coordinateBoundary);


  /**
   * Sets the value of the "boundaryDomainType" attribute of this BoundaryCondition.
   *
   * @param boundaryDomainType; const std::string& value of the "boundaryDomainType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setBoundaryDomainType(const std::string& boundaryDomainType);


  /**
   * Unsets the value of the "variable" attribute of this BoundaryCondition.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetVariable();


  /**
   * Unsets the value of the "type" attribute of this BoundaryCondition.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetType();


  /**
   * Unsets the value of the "coordinateBoundary" attribute of this BoundaryCondition.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoordinateBoundary();


  /**
   * Unsets the value of the "boundaryDomainType" attribute of this BoundaryCondition.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetBoundaryDomainType();


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
   * Returns the XML element name of this object, which for BoundaryCondition, is
   * always @c "boundaryCondition".
   *
   * @return the name of this element, i.e. @c "boundaryCondition".
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
   * for this BoundaryCondition object have been set.
   *
   * @note The required attributes for a BoundaryCondition object are:
   * @li "variable"
   * @li "type"
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
 * Creates a new BoundaryCondition_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * BoundaryCondition_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * BoundaryCondition_t structure.
 *
 * @returns the newly-created BoundaryCondition_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
BoundaryCondition_t *
BoundaryCondition_create(unsigned int level, unsigned int version,
                         unsigned int pkgVersion);


/**
 * Frees the given BoundaryCondition_t structure.
 * 
 * @param bc the BoundaryCondition_t structure to be freed.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
void
BoundaryCondition_free(BoundaryCondition_t * bc);


/**
 * Creates a deep copy of the given BoundaryCondition_t structure.
 * 
 * @param bc the BoundaryCondition_t structure to be copied.
 *
 * @returns a (deep) copy of the given BoundaryCondition_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof BoundaryCondition_t
 */
LIBSBML_EXTERN
BoundaryCondition_t *
BoundaryCondition_clone(BoundaryCondition_t * bc);


/**
 * Returns the value of the "variable" attribute of the given BoundaryCondition_t
 * structure.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return the variable of this structure.
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
const char *
BoundaryCondition_getVariable(const BoundaryCondition_t * bc);


/**
 * Returns the value of the "type" attribute of the given BoundaryCondition_t
 * structure.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return the type of this structure.
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
BoundaryConditionKind_t
BoundaryCondition_getType(const BoundaryCondition_t * bc);


/**
 * Returns the value of the "coordinateBoundary" attribute of the given BoundaryCondition_t
 * structure.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return the coordinateBoundary of this structure.
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
const char *
BoundaryCondition_getCoordinateBoundary(const BoundaryCondition_t * bc);


/**
 * Returns the value of the "boundaryDomainType" attribute of the given BoundaryCondition_t
 * structure.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return the boundaryDomainType of this structure.
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
const char *
BoundaryCondition_getBoundaryDomainType(const BoundaryCondition_t * bc);


/**
 * Predicate returning @c 1 if the given BoundaryCondition_t structure's "variable"
 * is set.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return @c 1 if the "variable" of this BoundaryCondition_t structure is
 * set, @c 0 otherwise.
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetVariable(const BoundaryCondition_t * bc);


/**
 * Predicate returning @c 1 if the given BoundaryCondition_t structure's "type"
 * is set.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return @c 1 if the "type" of this BoundaryCondition_t structure is
 * set, @c 0 otherwise.
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetType(const BoundaryCondition_t * bc);


/**
 * Predicate returning @c 1 if the given BoundaryCondition_t structure's "coordinateBoundary"
 * is set.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return @c 1 if the "coordinateBoundary" of this BoundaryCondition_t structure is
 * set, @c 0 otherwise.
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetCoordinateBoundary(const BoundaryCondition_t * bc);


/**
 * Predicate returning @c 1 if the given BoundaryCondition_t structure's "boundaryDomainType"
 * is set.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return @c 1 if the "boundaryDomainType" of this BoundaryCondition_t structure is
 * set, @c 0 otherwise.
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_isSetBoundaryDomainType(const BoundaryCondition_t * bc);


/**
 * Sets the "variable" attribute of the given BoundaryCondition_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs BoundaryCondition_unsetVariable() instead.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @param variable the string to which the structures "variable" attribute should be
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
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_setVariable(BoundaryCondition_t * bc, const char * variable);


/**
 * Sets the "type" attribute of the given BoundaryCondition_t structure.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @param type the string to which the structures "type" attribute should be
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
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_setType(BoundaryCondition_t * bc, BoundaryConditionKind_t type);


/**
 * Sets the "coordinateBoundary" attribute of the given BoundaryCondition_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs BoundaryCondition_unsetCoordinateBoundary() instead.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @param coordinateBoundary the string to which the structures "coordinateBoundary" attribute should be
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
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_setCoordinateBoundary(BoundaryCondition_t * bc, const char * coordinateBoundary);


/**
 * Sets the "boundaryDomainType" attribute of the given BoundaryCondition_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs BoundaryCondition_unsetBoundaryDomainType() instead.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @param boundaryDomainType the string to which the structures "boundaryDomainType" attribute should be
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
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_setBoundaryDomainType(BoundaryCondition_t * bc, const char * boundaryDomainType);


/**
 * Unsets the value of the "variable" attribute of the given 
 *BoundaryCondition_t structure.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetVariable(BoundaryCondition_t * bc);


/**
 * Unsets the value of the "type" attribute of the given 
 *BoundaryCondition_t structure.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetType(BoundaryCondition_t * bc);


/**
 * Unsets the value of the "coordinateBoundary" attribute of the given 
 *BoundaryCondition_t structure.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetCoordinateBoundary(BoundaryCondition_t * bc);


/**
 * Unsets the value of the "boundaryDomainType" attribute of the given 
 *BoundaryCondition_t structure.
 *
 * @param bc the BoundaryCondition_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_unsetBoundaryDomainType(BoundaryCondition_t * bc);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given BoundaryCondition_t structure have been set.
 *
 * @param bc the BoundaryCondition_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of BoundaryCondition_t
 */
LIBSBML_EXTERN
int
BoundaryCondition_hasRequiredAttributes(const BoundaryCondition_t * bc);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  BoundaryCondition_H__  */

