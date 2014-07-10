/**
 * @file:   CSGTranslation.h
 * @brief:  Implementation of the CSGTranslation class
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


#ifndef CSGTranslation_H__
#define CSGTranslation_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGTransformation.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN CSGTranslation : public CSGTransformation
{

protected:

  double        mTranslateX;
  bool          mIsSetTranslateX;
  double        mTranslateY;
  bool          mIsSetTranslateY;
  double        mTranslateZ;
  bool          mIsSetTranslateZ;


public:

  /**
   * Creates a new CSGTranslation with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGTranslation
   *
   * @param version an unsigned int, the SBML Version to assign to this CSGTranslation
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CSGTranslation
   */
  CSGTranslation(unsigned int level      = SpatialExtension::getDefaultLevel(),
                 unsigned int version    = SpatialExtension::getDefaultVersion(),
                 unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGTranslation with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CSGTranslation(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CSGTranslation.
   *
   * @param orig; the CSGTranslation instance to copy.
   */
  CSGTranslation(const CSGTranslation& orig);


   /**
   * Assignment operator for CSGTranslation.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CSGTranslation& operator=(const CSGTranslation& rhs);


   /**
   * Creates and returns a deep copy of this CSGTranslation object.
   *
   * @return a (deep) copy of this CSGTranslation object.
   */
  virtual CSGTranslation* clone () const;


   /**
   * Destructor for CSGTranslation.
   */
  virtual ~CSGTranslation();


   /**
   * Returns the value of the "translateX" attribute of this CSGTranslation.
   *
   * @return the value of the "translateX" attribute of this CSGTranslation as a double.
   */
  virtual double getTranslateX() const;


  /**
   * Returns the value of the "translateY" attribute of this CSGTranslation.
   *
   * @return the value of the "translateY" attribute of this CSGTranslation as a double.
   */
  virtual double getTranslateY() const;


  /**
   * Returns the value of the "translateZ" attribute of this CSGTranslation.
   *
   * @return the value of the "translateZ" attribute of this CSGTranslation as a double.
   */
  virtual double getTranslateZ() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTranslation's "translateX" attribute has been set.
   *
   * @return @c true if this CSGTranslation's "translateX" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetTranslateX() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTranslation's "translateY" attribute has been set.
   *
   * @return @c true if this CSGTranslation's "translateY" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetTranslateY() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGTranslation's "translateZ" attribute has been set.
   *
   * @return @c true if this CSGTranslation's "translateZ" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetTranslateZ() const;


  /**
   * Sets the value of the "translateX" attribute of this CSGTranslation.
   *
   * @param translateX; double value of the "translateX" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setTranslateX(double translateX);


  /**
   * Sets the value of the "translateY" attribute of this CSGTranslation.
   *
   * @param translateY; double value of the "translateY" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setTranslateY(double translateY);


  /**
   * Sets the value of the "translateZ" attribute of this CSGTranslation.
   *
   * @param translateZ; double value of the "translateZ" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setTranslateZ(double translateZ);


  /**
   * Unsets the value of the "translateX" attribute of this CSGTranslation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetTranslateX();


  /**
   * Unsets the value of the "translateY" attribute of this CSGTranslation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetTranslateY();


  /**
   * Unsets the value of the "translateZ" attribute of this CSGTranslation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetTranslateZ();


  /**
   * Returns the XML element name of this object, which for CSGTranslation, is
   * always @c "cSGTranslation".
   *
   * @return the name of this element, i.e. @c "cSGTranslation".
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
   * for this CSGTranslation object have been set.
   *
   * @note The required attributes for a CSGTranslation object are:
   * @li "translateX"
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
 * Creates a new CSGTranslation_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CSGTranslation_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CSGTranslation_t structure.
 *
 * @returns the newly-created CSGTranslation_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
CSGTranslation_t *
CSGTranslation_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion);


/**
 * Frees the given CSGTranslation_t structure.
 * 
 * @param csgt the CSGTranslation_t structure to be freed.
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
void
CSGTranslation_free(CSGTranslation_t * csgt);


/**
 * Creates a deep copy of the given CSGTranslation_t structure.
 * 
 * @param csgt the CSGTranslation_t structure to be copied.
 *
 * @returns a (deep) copy of the given CSGTranslation_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CSGTranslation_t
 */
LIBSBML_EXTERN
CSGTranslation_t *
CSGTranslation_clone(CSGTranslation_t * csgt);


/**
 * Returns the value of the "translateX" attribute of the given CSGTranslation_t
 * structure.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return the translateX of this structure.
 *
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
double
CSGTranslation_getTranslateX(const CSGTranslation_t * csgt);


/**
 * Returns the value of the "translateY" attribute of the given CSGTranslation_t
 * structure.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return the translateY of this structure.
 *
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
double
CSGTranslation_getTranslateY(const CSGTranslation_t * csgt);


/**
 * Returns the value of the "translateZ" attribute of the given CSGTranslation_t
 * structure.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return the translateZ of this structure.
 *
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
double
CSGTranslation_getTranslateZ(const CSGTranslation_t * csgt);


/**
 * Predicate returning @c 1 if the given CSGTranslation_t structure's "translateX"
 * is set.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return @c 1 if the "translateX" of this CSGTranslation_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateX(const CSGTranslation_t * csgt);


/**
 * Predicate returning @c 1 if the given CSGTranslation_t structure's "translateY"
 * is set.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return @c 1 if the "translateY" of this CSGTranslation_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateY(const CSGTranslation_t * csgt);


/**
 * Predicate returning @c 1 if the given CSGTranslation_t structure's "translateZ"
 * is set.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return @c 1 if the "translateZ" of this CSGTranslation_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateZ(const CSGTranslation_t * csgt);


/**
 * Sets the "translateX" attribute of the given CSGTranslation_t structure.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @param translateX the string to which the structures "translateX" attribute should be
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
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_setTranslateX(CSGTranslation_t * csgt, double translateX);


/**
 * Sets the "translateY" attribute of the given CSGTranslation_t structure.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @param translateY the string to which the structures "translateY" attribute should be
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
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_setTranslateY(CSGTranslation_t * csgt, double translateY);


/**
 * Sets the "translateZ" attribute of the given CSGTranslation_t structure.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @param translateZ the string to which the structures "translateZ" attribute should be
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
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_setTranslateZ(CSGTranslation_t * csgt, double translateZ);


/**
 * Unsets the value of the "translateX" attribute of the given 
 *CSGTranslation_t structure.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateX(CSGTranslation_t * csgt);


/**
 * Unsets the value of the "translateY" attribute of the given 
 *CSGTranslation_t structure.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateY(CSGTranslation_t * csgt);


/**
 * Unsets the value of the "translateZ" attribute of the given 
 *CSGTranslation_t structure.
 *
 * @param csgt the CSGTranslation_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateZ(CSGTranslation_t * csgt);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CSGTranslation_t structure have been set.
 *
 * @param csgt the CSGTranslation_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGTranslation_t
 */
LIBSBML_EXTERN
int
CSGTranslation_hasRequiredAttributes(const CSGTranslation_t * csgt);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CSGTranslation_H__  */

