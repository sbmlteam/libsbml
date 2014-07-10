/**
 * @file:   SampledField.h
 * @brief:  Implementation of the SampledField class
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


#ifndef SampledField_H__
#define SampledField_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

#include <sbml/packages/spatial/sbml/ImageData.h>

LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN SampledField : public SBase
{

protected:

  std::string   mId;
  std::string   mDataType;
  int           mNumSamples1;
  bool          mIsSetNumSamples1;
  int           mNumSamples2;
  bool          mIsSetNumSamples2;
  int           mNumSamples3;
  bool          mIsSetNumSamples3;
  std::string   mInterpolationType;
  std::string   mEncoding;
  ImageData*      mImageData;


public:

  /**
   * Creates a new SampledField with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this SampledField
   *
   * @param version an unsigned int, the SBML Version to assign to this SampledField
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this SampledField
   */
  SampledField(unsigned int level      = SpatialExtension::getDefaultLevel(),
               unsigned int version    = SpatialExtension::getDefaultVersion(),
               unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SampledField with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  SampledField(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for SampledField.
   *
   * @param orig; the SampledField instance to copy.
   */
  SampledField(const SampledField& orig);


   /**
   * Assignment operator for SampledField.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  SampledField& operator=(const SampledField& rhs);


   /**
   * Creates and returns a deep copy of this SampledField object.
   *
   * @return a (deep) copy of this SampledField object.
   */
  virtual SampledField* clone () const;


   /**
   * Destructor for SampledField.
   */
  virtual ~SampledField();


   /**
   * Returns the value of the "id" attribute of this SampledField.
   *
   * @return the value of the "id" attribute of this SampledField as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "dataType" attribute of this SampledField.
   *
   * @return the value of the "dataType" attribute of this SampledField as a string.
   */
  virtual const std::string& getDataType() const;


  /**
   * Returns the value of the "numSamples1" attribute of this SampledField.
   *
   * @return the value of the "numSamples1" attribute of this SampledField as a integer.
   */
  virtual int getNumSamples1() const;


  /**
   * Returns the value of the "numSamples2" attribute of this SampledField.
   *
   * @return the value of the "numSamples2" attribute of this SampledField as a integer.
   */
  virtual int getNumSamples2() const;


  /**
   * Returns the value of the "numSamples3" attribute of this SampledField.
   *
   * @return the value of the "numSamples3" attribute of this SampledField as a integer.
   */
  virtual int getNumSamples3() const;


  /**
   * Returns the value of the "interpolationType" attribute of this SampledField.
   *
   * @return the value of the "interpolationType" attribute of this SampledField as a string.
   */
  virtual const std::string& getInterpolationType() const;


  /**
   * Returns the value of the "encoding" attribute of this SampledField.
   *
   * @return the value of the "encoding" attribute of this SampledField as a string.
   */
  virtual const std::string& getEncoding() const;


  /**
   * Returns the "imageData" element of this SampledField.
   *
   * @return the "imageData" element of this SampledField.
   */
  virtual const ImageData* getImageData() const;


  /**
   * Returns the "imageData" element of this SampledField.
   *
   * @return the "imageData" element of this SampledField.
   */
  virtual ImageData* getImageData();


  /**
   * Creates a new "ImageData" and sets it for this SampledField.
   *
   * @return the created "ImageData" element of this SampledField.
   */
  virtual ImageData* createImageData();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "id" attribute has been set.
   *
   * @return @c true if this SampledField's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "dataType" attribute has been set.
   *
   * @return @c true if this SampledField's "dataType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetDataType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "numSamples1" attribute has been set.
   *
   * @return @c true if this SampledField's "numSamples1" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetNumSamples1() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "numSamples2" attribute has been set.
   *
   * @return @c true if this SampledField's "numSamples2" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetNumSamples2() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "numSamples3" attribute has been set.
   *
   * @return @c true if this SampledField's "numSamples3" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetNumSamples3() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "interpolationType" attribute has been set.
   *
   * @return @c true if this SampledField's "interpolationType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetInterpolationType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "encoding" attribute has been set.
   *
   * @return @c true if this SampledField's "encoding" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetEncoding() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "imageData" element has been set.
   *
   * @return @c true if this SampledField's "imageData" element has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetImageData() const;


  /**
   * Sets the value of the "id" attribute of this SampledField.
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
   * Sets the value of the "dataType" attribute of this SampledField.
   *
   * @param dataType; const std::string& value of the "dataType" attribute to be set
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
   * Sets the value of the "numSamples1" attribute of this SampledField.
   *
   * @param numSamples1; int value of the "numSamples1" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setNumSamples1(int numSamples1);


  /**
   * Sets the value of the "numSamples2" attribute of this SampledField.
   *
   * @param numSamples2; int value of the "numSamples2" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setNumSamples2(int numSamples2);


  /**
   * Sets the value of the "numSamples3" attribute of this SampledField.
   *
   * @param numSamples3; int value of the "numSamples3" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setNumSamples3(int numSamples3);


  /**
   * Sets the value of the "interpolationType" attribute of this SampledField.
   *
   * @param interpolationType; const std::string& value of the "interpolationType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setInterpolationType(const std::string& interpolationType);


  /**
   * Sets the value of the "encoding" attribute of this SampledField.
   *
   * @param encoding; const std::string& value of the "encoding" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setEncoding(const std::string& encoding);


  /**
   * Sets the "imageData" element of this SampledField.
   *
   * @param imageData; ImageData* to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setImageData(ImageData* imageData);


  /**
   * Unsets the value of the "id" attribute of this SampledField.
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
   * Unsets the value of the "dataType" attribute of this SampledField.
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
   * Unsets the value of the "numSamples1" attribute of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetNumSamples1();


  /**
   * Unsets the value of the "numSamples2" attribute of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetNumSamples2();


  /**
   * Unsets the value of the "numSamples3" attribute of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetNumSamples3();


  /**
   * Unsets the value of the "interpolationType" attribute of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetInterpolationType();


  /**
   * Unsets the value of the "encoding" attribute of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetEncoding();


  /**
   * Unsets the "imageData" element of this SampledField.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetImageData();


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for SampledField, is
   * always @c "sampledField".
   *
   * @return the name of this element, i.e. @c "sampledField".
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
   * for this SampledField object have been set.
   *
   * @note The required attributes for a SampledField object are:
   * @li "id"
   * @li "numSamples1"
   * @li "encoding"
   * @li "imageData"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this SampledField object have been set.
   *
   * @note The required elements for a SampledField object are:
   * @li "imageData"
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const;


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
   * Connects to child elements.
   */
  virtual void connectToChild ();


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
 * Creates a new SampledField_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * SampledField_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * SampledField_t structure.
 *
 * @returns the newly-created SampledField_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
SampledField_t *
SampledField_create(unsigned int level, unsigned int version,
                    unsigned int pkgVersion);


/**
 * Frees the given SampledField_t structure.
 * 
 * @param sf the SampledField_t structure to be freed.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
void
SampledField_free(SampledField_t * sf);


/**
 * Creates a deep copy of the given SampledField_t structure.
 * 
 * @param sf the SampledField_t structure to be copied.
 *
 * @returns a (deep) copy of the given SampledField_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
SampledField_t *
SampledField_clone(SampledField_t * sf);


/**
 * Returns the value of the "id" attribute of the given SampledField_t
 * structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return the id of this structure.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
const char *
SampledField_getId(const SampledField_t * sf);


/**
 * Returns the value of the "dataType" attribute of the given SampledField_t
 * structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return the dataType of this structure.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
const char *
SampledField_getDataType(const SampledField_t * sf);


/**
 * Returns the value of the "numSamples1" attribute of the given SampledField_t
 * structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return the numSamples1 of this structure.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_getNumSamples1(const SampledField_t * sf);


/**
 * Returns the value of the "numSamples2" attribute of the given SampledField_t
 * structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return the numSamples2 of this structure.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_getNumSamples2(const SampledField_t * sf);


/**
 * Returns the value of the "numSamples3" attribute of the given SampledField_t
 * structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return the numSamples3 of this structure.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_getNumSamples3(const SampledField_t * sf);


/**
 * Returns the value of the "interpolationType" attribute of the given SampledField_t
 * structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return the interpolationType of this structure.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
const char *
SampledField_getInterpolationType(const SampledField_t * sf);


/**
 * Returns the value of the "encoding" attribute of the given SampledField_t
 * structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return the encoding of this structure.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
const char *
SampledField_getEncoding(const SampledField_t * sf);


LIBSBML_EXTERN
ImageData_t*
SampledField_getImageData(SampledField_t * sf);


LIBSBML_EXTERN
ImageData_t*
SampledField_createImageData(SampledField_t * sf);


/**
 * Predicate returning @c 1 if the given SampledField_t structure's "id"
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if the "id" of this SampledField_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetId(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if the given SampledField_t structure's "dataType"
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if the "dataType" of this SampledField_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetDataType(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if the given SampledField_t structure's "numSamples1"
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if the "numSamples1" of this SampledField_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetNumSamples1(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if the given SampledField_t structure's "numSamples2"
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if the "numSamples2" of this SampledField_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetNumSamples2(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if the given SampledField_t structure's "numSamples3"
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if the "numSamples3" of this SampledField_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetNumSamples3(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if the given SampledField_t structure's "interpolationType"
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if the "interpolationType" of this SampledField_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetInterpolationType(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if the given SampledField_t structure's "encoding"
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if the "encoding" of this SampledField_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetEncoding(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if the given SampledField_t structure's "imageData"
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if the "imageData" of this SampledField_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetImageData(const SampledField_t * sf);


/**
 * Sets the "id" attribute of the given SampledField_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs SampledField_unsetId() instead.
 *
 * @param sf the SampledField_t structure.
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
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setId(SampledField_t * sf, const char * id);


/**
 * Sets the "dataType" attribute of the given SampledField_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs SampledField_unsetDataType() instead.
 *
 * @param sf the SampledField_t structure.
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
 * @note Using this function with a null pointer for @p name is equivalent to
 * unsetting the value of the "name" attribute.
 * 
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setDataType(SampledField_t * sf, const char * dataType);


/**
 * Sets the "numSamples1" attribute of the given SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @param numSamples1 the string to which the structures "numSamples1" attribute should be
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
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setNumSamples1(SampledField_t * sf, int numSamples1);


/**
 * Sets the "numSamples2" attribute of the given SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @param numSamples2 the string to which the structures "numSamples2" attribute should be
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
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setNumSamples2(SampledField_t * sf, int numSamples2);


/**
 * Sets the "numSamples3" attribute of the given SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @param numSamples3 the string to which the structures "numSamples3" attribute should be
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
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setNumSamples3(SampledField_t * sf, int numSamples3);


/**
 * Sets the "interpolationType" attribute of the given SampledField_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs SampledField_unsetInterpolationType() instead.
 *
 * @param sf the SampledField_t structure.
 *
 * @param interpolationType the string to which the structures "interpolationType" attribute should be
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
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setInterpolationType(SampledField_t * sf, const char * interpolationType);


/**
 * Sets the "encoding" attribute of the given SampledField_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs SampledField_unsetEncoding() instead.
 *
 * @param sf the SampledField_t structure.
 *
 * @param encoding the string to which the structures "encoding" attribute should be
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
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setEncoding(SampledField_t * sf, const char * encoding);


LIBSBML_EXTERN
int
SampledField_setImageData(SampledField_t * sf, ImageData_t* imageData);


/**
 * Unsets the value of the "id" attribute of the given 
 *SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetId(SampledField_t * sf);


/**
 * Unsets the value of the "dataType" attribute of the given 
 *SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetDataType(SampledField_t * sf);


/**
 * Unsets the value of the "numSamples1" attribute of the given 
 *SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetNumSamples1(SampledField_t * sf);


/**
 * Unsets the value of the "numSamples2" attribute of the given 
 *SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetNumSamples2(SampledField_t * sf);


/**
 * Unsets the value of the "numSamples3" attribute of the given 
 *SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetNumSamples3(SampledField_t * sf);


/**
 * Unsets the value of the "interpolationType" attribute of the given 
 *SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetInterpolationType(SampledField_t * sf);


/**
 * Unsets the value of the "encoding" attribute of the given 
 *SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetEncoding(SampledField_t * sf);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given SampledField_t structure have been set.
 *
 * @param sf the SampledField_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_hasRequiredAttributes(const SampledField_t * sf);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * sub-elements of the given SampledField_t structure have been set.
 *
 * @param sf the SampledField_t structure to check.
 *
 * @return @c 1 if all the required sub-elements for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_hasRequiredElements(const SampledField_t * sf);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SampledField_H__  */

