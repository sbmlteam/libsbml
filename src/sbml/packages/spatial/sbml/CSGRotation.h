/**
 * @file:   CSGRotation.h
 * @brief:  Implementation of the CSGRotation class
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


#ifndef CSGRotation_H__
#define CSGRotation_H__


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



class LIBSBML_EXTERN CSGRotation : public CSGTransformation
{

protected:

  double        mRotateAxisX;
  bool          mIsSetRotateAxisX;
  double        mRotateAxisY;
  bool          mIsSetRotateAxisY;
  double        mRotateAxisZ;
  bool          mIsSetRotateAxisZ;
  double        mRotateAngleInRadians;
  bool          mIsSetRotateAngleInRadians;


public:

  /**
   * Creates a new CSGRotation with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGRotation
   *
   * @param version an unsigned int, the SBML Version to assign to this CSGRotation
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CSGRotation
   */
  CSGRotation(unsigned int level      = SpatialExtension::getDefaultLevel(),
              unsigned int version    = SpatialExtension::getDefaultVersion(),
              unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGRotation with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CSGRotation(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CSGRotation.
   *
   * @param orig; the CSGRotation instance to copy.
   */
  CSGRotation(const CSGRotation& orig);


   /**
   * Assignment operator for CSGRotation.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CSGRotation& operator=(const CSGRotation& rhs);


   /**
   * Creates and returns a deep copy of this CSGRotation object.
   *
   * @return a (deep) copy of this CSGRotation object.
   */
  virtual CSGRotation* clone () const;


   /**
   * Destructor for CSGRotation.
   */
  virtual ~CSGRotation();


   /**
   * Returns the value of the "rotateAxisX" attribute of this CSGRotation.
   *
   * @return the value of the "rotateAxisX" attribute of this CSGRotation as a double.
   */
  virtual double getRotateAxisX() const;


  /**
   * Returns the value of the "rotateAxisY" attribute of this CSGRotation.
   *
   * @return the value of the "rotateAxisY" attribute of this CSGRotation as a double.
   */
  virtual double getRotateAxisY() const;


  /**
   * Returns the value of the "rotateAxisZ" attribute of this CSGRotation.
   *
   * @return the value of the "rotateAxisZ" attribute of this CSGRotation as a double.
   */
  virtual double getRotateAxisZ() const;


  /**
   * Returns the value of the "rotateAngleInRadians" attribute of this CSGRotation.
   *
   * @return the value of the "rotateAngleInRadians" attribute of this CSGRotation as a double.
   */
  virtual double getRotateAngleInRadians() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGRotation's "rotateAxisX" attribute has been set.
   *
   * @return @c true if this CSGRotation's "rotateAxisX" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetRotateAxisX() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGRotation's "rotateAxisY" attribute has been set.
   *
   * @return @c true if this CSGRotation's "rotateAxisY" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetRotateAxisY() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGRotation's "rotateAxisZ" attribute has been set.
   *
   * @return @c true if this CSGRotation's "rotateAxisZ" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetRotateAxisZ() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGRotation's "rotateAngleInRadians" attribute has been set.
   *
   * @return @c true if this CSGRotation's "rotateAngleInRadians" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetRotateAngleInRadians() const;


  /**
   * Sets the value of the "rotateAxisX" attribute of this CSGRotation.
   *
   * @param rotateAxisX; double value of the "rotateAxisX" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setRotateAxisX(double rotateAxisX);


  /**
   * Sets the value of the "rotateAxisY" attribute of this CSGRotation.
   *
   * @param rotateAxisY; double value of the "rotateAxisY" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setRotateAxisY(double rotateAxisY);


  /**
   * Sets the value of the "rotateAxisZ" attribute of this CSGRotation.
   *
   * @param rotateAxisZ; double value of the "rotateAxisZ" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setRotateAxisZ(double rotateAxisZ);


  /**
   * Sets the value of the "rotateAngleInRadians" attribute of this CSGRotation.
   *
   * @param rotateAngleInRadians; double value of the "rotateAngleInRadians" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setRotateAngleInRadians(double rotateAngleInRadians);


  /**
   * Unsets the value of the "rotateAxisX" attribute of this CSGRotation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetRotateAxisX();


  /**
   * Unsets the value of the "rotateAxisY" attribute of this CSGRotation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetRotateAxisY();


  /**
   * Unsets the value of the "rotateAxisZ" attribute of this CSGRotation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetRotateAxisZ();


  /**
   * Unsets the value of the "rotateAngleInRadians" attribute of this CSGRotation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetRotateAngleInRadians();


  /**
   * Returns the XML element name of this object, which for CSGRotation, is
   * always @c "cSGRotation".
   *
   * @return the name of this element, i.e. @c "cSGRotation".
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
   * for this CSGRotation object have been set.
   *
   * @note The required attributes for a CSGRotation object are:
   * @li "rotateAxisX"
   * @li "rotateAngleInRadians"
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
 * Creates a new CSGRotation_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CSGRotation_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CSGRotation_t structure.
 *
 * @returns the newly-created CSGRotation_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
CSGRotation_t *
CSGRotation_create(unsigned int level, unsigned int version,
                   unsigned int pkgVersion);


/**
 * Frees the given CSGRotation_t structure.
 * 
 * @param csgr the CSGRotation_t structure to be freed.
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
void
CSGRotation_free(CSGRotation_t * csgr);


/**
 * Creates a deep copy of the given CSGRotation_t structure.
 * 
 * @param csgr the CSGRotation_t structure to be copied.
 *
 * @returns a (deep) copy of the given CSGRotation_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CSGRotation_t
 */
LIBSBML_EXTERN
CSGRotation_t *
CSGRotation_clone(CSGRotation_t * csgr);


/**
 * Returns the value of the "rotateAxisX" attribute of the given CSGRotation_t
 * structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return the rotateAxisX of this structure.
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateAxisX(const CSGRotation_t * csgr);


/**
 * Returns the value of the "rotateAxisY" attribute of the given CSGRotation_t
 * structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return the rotateAxisY of this structure.
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateAxisY(const CSGRotation_t * csgr);


/**
 * Returns the value of the "rotateAxisZ" attribute of the given CSGRotation_t
 * structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return the rotateAxisZ of this structure.
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateAxisZ(const CSGRotation_t * csgr);


/**
 * Returns the value of the "rotateAngleInRadians" attribute of the given CSGRotation_t
 * structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return the rotateAngleInRadians of this structure.
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateAngleInRadians(const CSGRotation_t * csgr);


/**
 * Predicate returning @c 1 if the given CSGRotation_t structure's "rotateAxisX"
 * is set.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return @c 1 if the "rotateAxisX" of this CSGRotation_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateAxisX(const CSGRotation_t * csgr);


/**
 * Predicate returning @c 1 if the given CSGRotation_t structure's "rotateAxisY"
 * is set.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return @c 1 if the "rotateAxisY" of this CSGRotation_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateAxisY(const CSGRotation_t * csgr);


/**
 * Predicate returning @c 1 if the given CSGRotation_t structure's "rotateAxisZ"
 * is set.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return @c 1 if the "rotateAxisZ" of this CSGRotation_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateAxisZ(const CSGRotation_t * csgr);


/**
 * Predicate returning @c 1 if the given CSGRotation_t structure's "rotateAngleInRadians"
 * is set.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return @c 1 if the "rotateAngleInRadians" of this CSGRotation_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateAngleInRadians(const CSGRotation_t * csgr);


/**
 * Sets the "rotateAxisX" attribute of the given CSGRotation_t structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @param rotateAxisX the string to which the structures "rotateAxisX" attribute should be
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
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateAxisX(CSGRotation_t * csgr, double rotateAxisX);


/**
 * Sets the "rotateAxisY" attribute of the given CSGRotation_t structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @param rotateAxisY the string to which the structures "rotateAxisY" attribute should be
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
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateAxisY(CSGRotation_t * csgr, double rotateAxisY);


/**
 * Sets the "rotateAxisZ" attribute of the given CSGRotation_t structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @param rotateAxisZ the string to which the structures "rotateAxisZ" attribute should be
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
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateAxisZ(CSGRotation_t * csgr, double rotateAxisZ);


/**
 * Sets the "rotateAngleInRadians" attribute of the given CSGRotation_t structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @param rotateAngleInRadians the string to which the structures "rotateAngleInRadians" attribute should be
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
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateAngleInRadians(CSGRotation_t * csgr, double rotateAngleInRadians);


/**
 * Unsets the value of the "rotateAxisX" attribute of the given 
 *CSGRotation_t structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateAxisX(CSGRotation_t * csgr);


/**
 * Unsets the value of the "rotateAxisY" attribute of the given 
 *CSGRotation_t structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateAxisY(CSGRotation_t * csgr);


/**
 * Unsets the value of the "rotateAxisZ" attribute of the given 
 *CSGRotation_t structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateAxisZ(CSGRotation_t * csgr);


/**
 * Unsets the value of the "rotateAngleInRadians" attribute of the given 
 *CSGRotation_t structure.
 *
 * @param csgr the CSGRotation_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateAngleInRadians(CSGRotation_t * csgr);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CSGRotation_t structure have been set.
 *
 * @param csgr the CSGRotation_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGRotation_t
 */
LIBSBML_EXTERN
int
CSGRotation_hasRequiredAttributes(const CSGRotation_t * csgr);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CSGRotation_H__  */

