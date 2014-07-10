/**
 * @file:   PolygonObject.h
 * @brief:  Implementation of the PolygonObject class
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


#ifndef PolygonObject_H__
#define PolygonObject_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN PolygonObject : public SBase
{

protected:

  int*         mPointIndex;
  int           mPointIndexLength;
  bool          mIsSetPointIndexLength;


public:

  /**
   * Creates a new PolygonObject with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this PolygonObject
   *
   * @param version an unsigned int, the SBML Version to assign to this PolygonObject
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this PolygonObject
   */
  PolygonObject(unsigned int level      = SpatialExtension::getDefaultLevel(),
                unsigned int version    = SpatialExtension::getDefaultVersion(),
                unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new PolygonObject with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  PolygonObject(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for PolygonObject.
   *
   * @param orig; the PolygonObject instance to copy.
   */
  PolygonObject(const PolygonObject& orig);


   /**
   * Assignment operator for PolygonObject.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  PolygonObject& operator=(const PolygonObject& rhs);


   /**
   * Creates and returns a deep copy of this PolygonObject object.
   *
   * @return a (deep) copy of this PolygonObject object.
   */
  virtual PolygonObject* clone () const;


   /**
   * Destructor for PolygonObject.
   */
  virtual ~PolygonObject();


   /**
   * The "pointIndex" attribute of this PolygonObject is returned in an int* array (pointer)
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). The method itself has a return type void.
   *
   * NOTE: you have to pre-allocate the array with the correct length!   *
   * @return void.
   */
  void getPointIndex(int* outArray) const;


  /**
   * Returns the value of the "pointIndexLength" attribute of this PolygonObject.
   *
   * @return the value of the "pointIndexLength" attribute of this PolygonObject as a integer.
   */
  virtual int getPointIndexLength() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * PolygonObject's "pointIndex" attribute has been set.
   *
   * @return @c true if this PolygonObject's "pointIndex" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetPointIndex() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * PolygonObject's "pointIndexLength" attribute has been set.
   *
   * @return @c true if this PolygonObject's "pointIndexLength" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetPointIndexLength() const;


  /**
   * Sets the "pointIndex" element of this PolygonObject.
   *
   * @param inArray; int* array to be set (it will be copied).
   * @param arrayLength; the length of the array.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setPointIndex(int* inArray, int arrayLength);


  /**
   * Sets the value of the "pointIndexLength" attribute of this PolygonObject.
   *
   * @param pointIndexLength; int value of the "pointIndexLength" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setPointIndexLength(int pointIndexLength);


  /**
   * Unsets the value of the "pointIndex" attribute of this PolygonObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetPointIndex();


  /**
   * Unsets the value of the "pointIndexLength" attribute of this PolygonObject.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetPointIndexLength();


  /**
   * Returns the XML element name of this object, which for PolygonObject, is
   * always @c "polygonObject".
   *
   * @return the name of this element, i.e. @c "polygonObject".
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
   * for this PolygonObject object have been set.
   *
   * @note The required attributes for a PolygonObject object are:
   * @li "pointIndex"
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


  virtual void write(XMLOutputStream& stream) const;


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


  virtual void setElementText(const std::string &text);



};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new PolygonObject_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * PolygonObject_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * PolygonObject_t structure.
 *
 * @returns the newly-created PolygonObject_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof PolygonObject_t
 */
LIBSBML_EXTERN
PolygonObject_t *
PolygonObject_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion);


/**
 * Frees the given PolygonObject_t structure.
 * 
 * @param po the PolygonObject_t structure to be freed.
 *
 * @memberof PolygonObject_t
 */
LIBSBML_EXTERN
void
PolygonObject_free(PolygonObject_t * po);


/**
 * Creates a deep copy of the given PolygonObject_t structure.
 * 
 * @param po the PolygonObject_t structure to be copied.
 *
 * @returns a (deep) copy of the given PolygonObject_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof PolygonObject_t
 */
LIBSBML_EXTERN
PolygonObject_t *
PolygonObject_clone(PolygonObject_t * po);


/**
 * Returns the value of the "pointIndex" attribute of the given PolygonObject_t
 * structure.
 *
 * @param po the PolygonObject_t structure.
 *
 * @return the pointIndex of this structure.
 *
 * @member of PolygonObject_t
 */
LIBSBML_EXTERN
int*
PolygonObject_getPointIndex(const PolygonObject_t * po);


/**
 * Returns the value of the "pointIndexLength" attribute of the given PolygonObject_t
 * structure.
 *
 * @param po the PolygonObject_t structure.
 *
 * @return the pointIndexLength of this structure.
 *
 * @member of PolygonObject_t
 */
LIBSBML_EXTERN
int
PolygonObject_getPointIndexLength(const PolygonObject_t * po);


/**
 * Predicate returning @c 1 if the given PolygonObject_t structure's "pointIndex"
 * is set.
 *
 * @param po the PolygonObject_t structure.
 *
 * @return @c 1 if the "pointIndex" of this PolygonObject_t structure is
 * set, @c 0 otherwise.
 *
 * @member of PolygonObject_t
 */
LIBSBML_EXTERN
int
PolygonObject_isSetPointIndex(const PolygonObject_t * po);


/**
 * Predicate returning @c 1 if the given PolygonObject_t structure's "pointIndexLength"
 * is set.
 *
 * @param po the PolygonObject_t structure.
 *
 * @return @c 1 if the "pointIndexLength" of this PolygonObject_t structure is
 * set, @c 0 otherwise.
 *
 * @member of PolygonObject_t
 */
LIBSBML_EXTERN
int
PolygonObject_isSetPointIndexLength(const PolygonObject_t * po);


/**
 * Sets the "pointIndex" attribute of the given PolygonObject_t structure.
 *
 * @param po the PolygonObject_t structure.
 *
 * @param pointIndex the string to which the structures "pointIndex" attribute should be
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
 * @member of PolygonObject_t
 */
LIBSBML_EXTERN
int
PolygonObject_setPointIndex(PolygonObject_t * po, int* pointIndex);


/**
 * Sets the "pointIndexLength" attribute of the given PolygonObject_t structure.
 *
 * @param po the PolygonObject_t structure.
 *
 * @param pointIndexLength the string to which the structures "pointIndexLength" attribute should be
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
 * @member of PolygonObject_t
 */
LIBSBML_EXTERN
int
PolygonObject_setPointIndexLength(PolygonObject_t * po, int pointIndexLength);


/**
 * Unsets the value of the "pointIndex" attribute of the given 
 *PolygonObject_t structure.
 *
 * @param po the PolygonObject_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of PolygonObject_t
 */
LIBSBML_EXTERN
int
PolygonObject_unsetPointIndex(PolygonObject_t * po);


/**
 * Unsets the value of the "pointIndexLength" attribute of the given 
 *PolygonObject_t structure.
 *
 * @param po the PolygonObject_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of PolygonObject_t
 */
LIBSBML_EXTERN
int
PolygonObject_unsetPointIndexLength(PolygonObject_t * po);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given PolygonObject_t structure have been set.
 *
 * @param po the PolygonObject_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of PolygonObject_t
 */
LIBSBML_EXTERN
int
PolygonObject_hasRequiredAttributes(const PolygonObject_t * po);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  PolygonObject_H__  */

