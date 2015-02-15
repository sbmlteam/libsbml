/**
 * @file:   SpatialPoints.h
 * @brief:  Implementation of the SpatialPoints class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2015 jointly by the following organizations:
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


#ifndef SpatialPoints_H__
#define SpatialPoints_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN SpatialPoints : public SBase
{

protected:

  std::string   mId;
  CompressionKind_t   mCompression;
  double*         mArrayData;
  int           mArrayDataLength;
  bool          mIsSetArrayDataLength;
  DataKind_t   mDataType;


public:

  /**
   * Creates a new SpatialPoints with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this SpatialPoints
   *
   * @param version an unsigned int, the SBML Version to assign to this SpatialPoints
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this SpatialPoints
   */
  SpatialPoints(unsigned int level      = SpatialExtension::getDefaultLevel(),
                unsigned int version    = SpatialExtension::getDefaultVersion(),
                unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpatialPoints with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  SpatialPoints(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for SpatialPoints.
   *
   * @param orig; the SpatialPoints instance to copy.
   */
  SpatialPoints(const SpatialPoints& orig);


   /**
   * Assignment operator for SpatialPoints.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  SpatialPoints& operator=(const SpatialPoints& rhs);


   /**
   * Creates and returns a deep copy of this SpatialPoints object.
   *
   * @return a (deep) copy of this SpatialPoints object.
   */
  virtual SpatialPoints* clone () const;


   /**
   * Destructor for SpatialPoints.
   */
  virtual ~SpatialPoints();


   /**
   * Returns the value of the "id" attribute of this SpatialPoints.
   *
   * @return the value of the "id" attribute of this SpatialPoints as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "compression" attribute of this SpatialPoints.
   *
   * @return the value of the "compression" attribute of this SpatialPoints as a CompressionKind_t.
   */
  virtual CompressionKind_t getCompression() const;


  /**
   * The "arrayData" attribute of this SpatialPoints is returned in an double* array (pointer)
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). The method itself has a return type void.
   *
   * NOTE: you have to pre-allocate the array with the correct length!   *
   * @return void.
   */
  void getArrayData(double* outArray) const;


  /**
   * Returns the value of the "arrayDataLength" attribute of this SpatialPoints.
   *
   * @return the value of the "arrayDataLength" attribute of this SpatialPoints as a integer.
   */
  virtual int getArrayDataLength() const;


  /**
   * Returns the value of the "dataType" attribute of this SpatialPoints.
   *
   * @return the value of the "dataType" attribute of this SpatialPoints as a DataKind_t.
   */
  virtual DataKind_t getDataType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoints's "id" attribute has been set.
   *
   * @return @c true if this SpatialPoints's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoints's "compression" attribute has been set.
   *
   * @return @c true if this SpatialPoints's "compression" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCompression() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoints's "arrayData" attribute has been set.
   *
   * @return @c true if this SpatialPoints's "arrayData" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetArrayData() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoints's "arrayDataLength" attribute has been set.
   *
   * @return @c true if this SpatialPoints's "arrayDataLength" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetArrayDataLength() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpatialPoints's "dataType" attribute has been set.
   *
   * @return @c true if this SpatialPoints's "dataType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetDataType() const;


  /**
   * Sets the value of the "id" attribute of this SpatialPoints.
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
   * Sets the value of the "compression" attribute of this SpatialPoints.
   *
   * @param compression; CompressionKind_t value of the "compression" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCompression(CompressionKind_t compression);


  /**
   * Sets the value of the "compression" attribute of this SpatialPoints.
   *
   * @param compression; string value of the "compression" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCompression(const std::string& compression);


  /**
   * Sets the "arrayData" element of this SpatialPoints.
   *
   * @param inArray; double* array to be set (it will be copied).
   * @param arrayLength; the length of the array.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setArrayData(double* inArray, int arrayLength);


  /**
   * Sets the value of the "arrayDataLength" attribute of this SpatialPoints.
   *
   * @param arrayDataLength; int value of the "arrayDataLength" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setArrayDataLength(int arrayDataLength);


  /**
   * Sets the value of the "dataType" attribute of this SpatialPoints.
   *
   * @param dataType; DataKind_t value of the "dataType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setDataType(DataKind_t dataType);


  /**
   * Sets the value of the "dataType" attribute of this SpatialPoints.
   *
   * @param dataType; string value of the "dataType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setDataType(const std::string& dataType);


  /**
   * Unsets the value of the "id" attribute of this SpatialPoints.
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
   * Unsets the value of the "compression" attribute of this SpatialPoints.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCompression();


  /**
   * Unsets the value of the "arrayData" attribute of this SpatialPoints.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetArrayData();


  /**
   * Unsets the value of the "arrayDataLength" attribute of this SpatialPoints.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetArrayDataLength();


  /**
   * Unsets the value of the "dataType" attribute of this SpatialPoints.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetDataType();


  /**
   * Returns the XML element name of this object, which for SpatialPoints, is
   * always @c "spatialPoints".
   *
   * @return the name of this element, i.e. @c "spatialPoints".
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
   * for this SpatialPoints object have been set.
   *
   * @note The required attributes for a SpatialPoints object are:
   * @li "id"
   * @li "compression"
   * @li "arrayData"
   * @li "arrayDataLength"
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
 * Creates a new SpatialPoints_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * SpatialPoints_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * SpatialPoints_t structure.
 *
 * @returns the newly-created SpatialPoints_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof SpatialPoints_t
 */
LIBSBML_EXTERN
SpatialPoints_t *
SpatialPoints_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion);


/**
 * Frees the given SpatialPoints_t structure.
 * 
 * @param sp the SpatialPoints_t structure to be freed.
 *
 * @memberof SpatialPoints_t
 */
LIBSBML_EXTERN
void
SpatialPoints_free(SpatialPoints_t * sp);


/**
 * Creates a deep copy of the given SpatialPoints_t structure.
 * 
 * @param sp the SpatialPoints_t structure to be copied.
 *
 * @returns a (deep) copy of the given SpatialPoints_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof SpatialPoints_t
 */
LIBSBML_EXTERN
SpatialPoints_t *
SpatialPoints_clone(SpatialPoints_t * sp);


/**
 * Returns the value of the "id" attribute of the given SpatialPoints_t
 * structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return the id of this structure.
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
const char *
SpatialPoints_getId(const SpatialPoints_t * sp);


/**
 * Returns the value of the "compression" attribute of the given SpatialPoints_t
 * structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return the compression of this structure.
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
CompressionKind_t
SpatialPoints_getCompression(const SpatialPoints_t * sp);


/**
 * Returns the value of the "arrayData" attribute of the given SpatialPoints_t
 * structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return the arrayData of this structure.
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
double*
SpatialPoints_getArrayData(const SpatialPoints_t * sp);


/**
 * Returns the value of the "arrayDataLength" attribute of the given SpatialPoints_t
 * structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return the arrayDataLength of this structure.
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_getArrayDataLength(const SpatialPoints_t * sp);


/**
 * Returns the value of the "dataType" attribute of the given SpatialPoints_t
 * structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return the dataType of this structure.
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
DataKind_t
SpatialPoints_getDataType(const SpatialPoints_t * sp);


/**
 * Predicate returning @c 1 if the given SpatialPoints_t structure's "id"
 * is set.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return @c 1 if the "id" of this SpatialPoints_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_isSetId(const SpatialPoints_t * sp);


/**
 * Predicate returning @c 1 if the given SpatialPoints_t structure's "compression"
 * is set.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return @c 1 if the "compression" of this SpatialPoints_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_isSetCompression(const SpatialPoints_t * sp);


/**
 * Predicate returning @c 1 if the given SpatialPoints_t structure's "arrayData"
 * is set.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return @c 1 if the "arrayData" of this SpatialPoints_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_isSetArrayData(const SpatialPoints_t * sp);


/**
 * Predicate returning @c 1 if the given SpatialPoints_t structure's "arrayDataLength"
 * is set.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return @c 1 if the "arrayDataLength" of this SpatialPoints_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_isSetArrayDataLength(const SpatialPoints_t * sp);


/**
 * Predicate returning @c 1 if the given SpatialPoints_t structure's "dataType"
 * is set.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return @c 1 if the "dataType" of this SpatialPoints_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_isSetDataType(const SpatialPoints_t * sp);


/**
 * Sets the "id" attribute of the given SpatialPoints_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs SpatialPoints_unsetId() instead.
 *
 * @param sp the SpatialPoints_t structure.
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
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_setId(SpatialPoints_t * sp, const char * id);


/**
 * Sets the "compression" attribute of the given SpatialPoints_t structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @param compression the string to which the structures "compression" attribute should be
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
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_setCompression(SpatialPoints_t * sp, CompressionKind_t compression);


/**
 * Sets the "arrayData" attribute of the given SpatialPoints_t structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @param arrayData the string to which the structures "arrayData" attribute should be
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
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_setArrayData(SpatialPoints_t * sp, double* arrayData);


/**
 * Sets the "arrayDataLength" attribute of the given SpatialPoints_t structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @param arrayDataLength the string to which the structures "arrayDataLength" attribute should be
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
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_setArrayDataLength(SpatialPoints_t * sp, int arrayDataLength);


/**
 * Sets the "dataType" attribute of the given SpatialPoints_t structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @param dataType the string to which the structures "dataType" attribute should be
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
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_setDataType(SpatialPoints_t * sp, DataKind_t dataType);


/**
 * Unsets the value of the "id" attribute of the given 
 * SpatialPoints_t structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_unsetId(SpatialPoints_t * sp);


/**
 * Unsets the value of the "compression" attribute of the given 
 * SpatialPoints_t structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_unsetCompression(SpatialPoints_t * sp);


/**
 * Unsets the value of the "arrayData" attribute of the given 
 * SpatialPoints_t structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_unsetArrayData(SpatialPoints_t * sp);


/**
 * Unsets the value of the "arrayDataLength" attribute of the given 
 * SpatialPoints_t structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_unsetArrayDataLength(SpatialPoints_t * sp);


/**
 * Unsets the value of the "dataType" attribute of the given 
 * SpatialPoints_t structure.
 *
 * @param sp the SpatialPoints_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_unsetDataType(SpatialPoints_t * sp);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given SpatialPoints_t structure have been set.
 *
 * @param sp the SpatialPoints_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of SpatialPoints_t
 */
LIBSBML_EXTERN
int
SpatialPoints_hasRequiredAttributes(const SpatialPoints_t * sp);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpatialPoints_H__  */

