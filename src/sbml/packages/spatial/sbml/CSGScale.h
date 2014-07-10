/**
 * @file:   CSGScale.h
 * @brief:  Implementation of the CSGScale class
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


#ifndef CSGScale_H__
#define CSGScale_H__


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



class LIBSBML_EXTERN CSGScale : public CSGTransformation
{

protected:

  double        mScaleX;
  bool          mIsSetScaleX;
  double        mScaleY;
  bool          mIsSetScaleY;
  double        mScaleZ;
  bool          mIsSetScaleZ;


public:

  /**
   * Creates a new CSGScale with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGScale
   *
   * @param version an unsigned int, the SBML Version to assign to this CSGScale
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this CSGScale
   */
  CSGScale(unsigned int level      = SpatialExtension::getDefaultLevel(),
           unsigned int version    = SpatialExtension::getDefaultVersion(),
           unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGScale with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  CSGScale(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for CSGScale.
   *
   * @param orig; the CSGScale instance to copy.
   */
  CSGScale(const CSGScale& orig);


   /**
   * Assignment operator for CSGScale.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  CSGScale& operator=(const CSGScale& rhs);


   /**
   * Creates and returns a deep copy of this CSGScale object.
   *
   * @return a (deep) copy of this CSGScale object.
   */
  virtual CSGScale* clone () const;


   /**
   * Destructor for CSGScale.
   */
  virtual ~CSGScale();


   /**
   * Returns the value of the "scaleX" attribute of this CSGScale.
   *
   * @return the value of the "scaleX" attribute of this CSGScale as a double.
   */
  virtual double getScaleX() const;


  /**
   * Returns the value of the "scaleY" attribute of this CSGScale.
   *
   * @return the value of the "scaleY" attribute of this CSGScale as a double.
   */
  virtual double getScaleY() const;


  /**
   * Returns the value of the "scaleZ" attribute of this CSGScale.
   *
   * @return the value of the "scaleZ" attribute of this CSGScale as a double.
   */
  virtual double getScaleZ() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGScale's "scaleX" attribute has been set.
   *
   * @return @c true if this CSGScale's "scaleX" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetScaleX() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGScale's "scaleY" attribute has been set.
   *
   * @return @c true if this CSGScale's "scaleY" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetScaleY() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CSGScale's "scaleZ" attribute has been set.
   *
   * @return @c true if this CSGScale's "scaleZ" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetScaleZ() const;


  /**
   * Sets the value of the "scaleX" attribute of this CSGScale.
   *
   * @param scaleX; double value of the "scaleX" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setScaleX(double scaleX);


  /**
   * Sets the value of the "scaleY" attribute of this CSGScale.
   *
   * @param scaleY; double value of the "scaleY" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setScaleY(double scaleY);


  /**
   * Sets the value of the "scaleZ" attribute of this CSGScale.
   *
   * @param scaleZ; double value of the "scaleZ" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setScaleZ(double scaleZ);


  /**
   * Unsets the value of the "scaleX" attribute of this CSGScale.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetScaleX();


  /**
   * Unsets the value of the "scaleY" attribute of this CSGScale.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetScaleY();


  /**
   * Unsets the value of the "scaleZ" attribute of this CSGScale.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetScaleZ();


  /**
   * Returns the XML element name of this object, which for CSGScale, is
   * always @c "cSGScale".
   *
   * @return the name of this element, i.e. @c "cSGScale".
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
   * for this CSGScale object have been set.
   *
   * @note The required attributes for a CSGScale object are:
   * @li "scaleX"
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
 * Creates a new CSGScale_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * CSGScale_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * CSGScale_t structure.
 *
 * @returns the newly-created CSGScale_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
CSGScale_t *
CSGScale_create(unsigned int level, unsigned int version,
                unsigned int pkgVersion);


/**
 * Frees the given CSGScale_t structure.
 * 
 * @param csgs the CSGScale_t structure to be freed.
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
void
CSGScale_free(CSGScale_t * csgs);


/**
 * Creates a deep copy of the given CSGScale_t structure.
 * 
 * @param csgs the CSGScale_t structure to be copied.
 *
 * @returns a (deep) copy of the given CSGScale_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof CSGScale_t
 */
LIBSBML_EXTERN
CSGScale_t *
CSGScale_clone(CSGScale_t * csgs);


/**
 * Returns the value of the "scaleX" attribute of the given CSGScale_t
 * structure.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return the scaleX of this structure.
 *
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
double
CSGScale_getScaleX(const CSGScale_t * csgs);


/**
 * Returns the value of the "scaleY" attribute of the given CSGScale_t
 * structure.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return the scaleY of this structure.
 *
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
double
CSGScale_getScaleY(const CSGScale_t * csgs);


/**
 * Returns the value of the "scaleZ" attribute of the given CSGScale_t
 * structure.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return the scaleZ of this structure.
 *
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
double
CSGScale_getScaleZ(const CSGScale_t * csgs);


/**
 * Predicate returning @c 1 if the given CSGScale_t structure's "scaleX"
 * is set.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return @c 1 if the "scaleX" of this CSGScale_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_isSetScaleX(const CSGScale_t * csgs);


/**
 * Predicate returning @c 1 if the given CSGScale_t structure's "scaleY"
 * is set.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return @c 1 if the "scaleY" of this CSGScale_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_isSetScaleY(const CSGScale_t * csgs);


/**
 * Predicate returning @c 1 if the given CSGScale_t structure's "scaleZ"
 * is set.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return @c 1 if the "scaleZ" of this CSGScale_t structure is
 * set, @c 0 otherwise.
 *
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_isSetScaleZ(const CSGScale_t * csgs);


/**
 * Sets the "scaleX" attribute of the given CSGScale_t structure.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @param scaleX the string to which the structures "scaleX" attribute should be
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
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_setScaleX(CSGScale_t * csgs, double scaleX);


/**
 * Sets the "scaleY" attribute of the given CSGScale_t structure.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @param scaleY the string to which the structures "scaleY" attribute should be
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
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_setScaleY(CSGScale_t * csgs, double scaleY);


/**
 * Sets the "scaleZ" attribute of the given CSGScale_t structure.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @param scaleZ the string to which the structures "scaleZ" attribute should be
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
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_setScaleZ(CSGScale_t * csgs, double scaleZ);


/**
 * Unsets the value of the "scaleX" attribute of the given 
 *CSGScale_t structure.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_unsetScaleX(CSGScale_t * csgs);


/**
 * Unsets the value of the "scaleY" attribute of the given 
 *CSGScale_t structure.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_unsetScaleY(CSGScale_t * csgs);


/**
 * Unsets the value of the "scaleZ" attribute of the given 
 *CSGScale_t structure.
 *
 * @param csgs the CSGScale_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_unsetScaleZ(CSGScale_t * csgs);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given CSGScale_t structure have been set.
 *
 * @param csgs the CSGScale_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of CSGScale_t
 */
LIBSBML_EXTERN
int
CSGScale_hasRequiredAttributes(const CSGScale_t * csgs);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  CSGScale_H__  */

