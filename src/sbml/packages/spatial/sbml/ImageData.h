/**
 * @file:   ImageData.h
 * @brief:  Implementation of the ImageData class
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


#ifndef ImageData_H__
#define ImageData_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN ImageData : public SBase
{

protected:

  int*         mSamples;
  int           mSamplesLength;
  bool          mIsSetSamplesLength;
  std::string   mDataType;


public:

  /**
   * Creates a new ImageData with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ImageData
   *
   * @param version an unsigned int, the SBML Version to assign to this ImageData
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ImageData
   */
  ImageData(unsigned int level      = SpatialExtension::getDefaultLevel(),
            unsigned int version    = SpatialExtension::getDefaultVersion(),
            unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ImageData with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ImageData(SpatialPkgNamespaces* spatialns);


   /**
   * Copy constructor for ImageData.
   *
   * @param orig; the ImageData instance to copy.
   */
  ImageData(const ImageData& orig);


   /**
   * Assignment operator for ImageData.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  ImageData& operator=(const ImageData& rhs);


   /**
   * Creates and returns a deep copy of this ImageData object.
   *
   * @return a (deep) copy of this ImageData object.
   */
  virtual ImageData* clone () const;


   /**
   * Destructor for ImageData.
   */
  virtual ~ImageData();


   /**
   * The "samples" attribute of this ImageData is returned in an int* array (pointer)
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). The method itself has a return type void.
   *
   * NOTE: you have to pre-allocate the array with the correct length!   *
   * @return void.
   */
  void getSamples(int* outArray) const;


  /**
   * Returns the value of the "samplesLength" attribute of this ImageData.
   *
   * @return the value of the "samplesLength" attribute of this ImageData as a integer.
   */
  virtual int getSamplesLength() const;


  /**
   * Returns the value of the "dataType" attribute of this ImageData.
   *
   * @return the value of the "dataType" attribute of this ImageData as a string.
   */
  virtual const std::string& getDataType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ImageData's "samples" attribute has been set.
   *
   * @return @c true if this ImageData's "samples" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetSamples() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ImageData's "samplesLength" attribute has been set.
   *
   * @return @c true if this ImageData's "samplesLength" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetSamplesLength() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * ImageData's "dataType" attribute has been set.
   *
   * @return @c true if this ImageData's "dataType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetDataType() const;


  /**
   * Sets the "samples" element of this ImageData.
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
  virtual int setSamples(int* inArray, int arrayLength);


  /**
   * Sets the value of the "samplesLength" attribute of this ImageData.
   *
   * @param samplesLength; int value of the "samplesLength" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setSamplesLength(int samplesLength);


  /**
   * Sets the value of the "dataType" attribute of this ImageData.
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
   * Unsets the value of the "samples" attribute of this ImageData.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSamples();


  /**
   * Unsets the value of the "samplesLength" attribute of this ImageData.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSamplesLength();


  /**
   * Unsets the value of the "dataType" attribute of this ImageData.
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
   * Returns the XML element name of this object, which for ImageData, is
   * always @c "imageData".
   *
   * @return the name of this element, i.e. @c "imageData".
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
   * for this ImageData object have been set.
   *
   * @note The required attributes for a ImageData object are:
   * @li "samples"
   * @li "samplesLength"
   * @li "dataType"
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


protected:
  int* mUncompressedSamples;
  int mUncompressedLength;

  static void copySampleArrays(int* &target, int& targetLength, int* source, int sourceLength);
  static void uncompress_data(void *data, size_t length, int*& result, int& outLength);

public:

 /**
   * Returns the "samplesLength" attribute of this ImageData.
   *
   * @return the "samplesLength" attribute of this ImageData.
   */
  unsigned int getUncompressedLength();

 /**
   * The "samples" attribute of this ImageData is returned in an int array (pointer) 
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). This method returns the uncompressed sample field.
   *
   * @return void.
   */
  void getUncompressed(int* outputSamples);

  /** 
   * utility function freeing the uncompressed data. 
   */
  void freeUncompressed();
  
  /** 
   * utility function uncompressing samples
   */
  void uncompress();

  /**  
   *  Returns the data of this image as uncompressed array of integers
   * 
   * @param data the output array of integers (it will be allocated using
   *             malloc and will have to be freed using free)
   * @param length the output lenght of the array
   *
   */
  void getUncompressedData(int* &data, int& length);


};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Creates a new ImageData_t structure using the given SBML @p level and
 * @p version values.
 *
 * @param level an unsigned int, the SBML level to assign to this
 * ImageData_t structure.
 *
 * @param version an unsigned int, the SBML version to assign to this
 * ImageData_t structure.
 *
 * @returns the newly-created ImageData_t structure, or a null pointer if
 * an error occurred during construction.
 *
 * @copydetails doc_note_setting_lv
 *
 * @memberof ImageData_t
 */
LIBSBML_EXTERN
ImageData_t *
ImageData_create(unsigned int level, unsigned int version,
                 unsigned int pkgVersion);


/**
 * Frees the given ImageData_t structure.
 * 
 * @param id the ImageData_t structure to be freed.
 *
 * @memberof ImageData_t
 */
LIBSBML_EXTERN
void
ImageData_free(ImageData_t * id);


/**
 * Creates a deep copy of the given ImageData_t structure.
 * 
 * @param id the ImageData_t structure to be copied.
 *
 * @returns a (deep) copy of the given ImageData_t structure, or a null
 * pointer if a failure occurred.
 *
 * @memberof ImageData_t
 */
LIBSBML_EXTERN
ImageData_t *
ImageData_clone(ImageData_t * id);


/**
 * Returns the value of the "samples" attribute of the given ImageData_t
 * structure.
 *
 * @param id the ImageData_t structure.
 *
 * @return the samples of this structure.
 *
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int*
ImageData_getSamples(const ImageData_t * id);


/**
 * Returns the value of the "samplesLength" attribute of the given ImageData_t
 * structure.
 *
 * @param id the ImageData_t structure.
 *
 * @return the samplesLength of this structure.
 *
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int
ImageData_getSamplesLength(const ImageData_t * id);


/**
 * Returns the value of the "dataType" attribute of the given ImageData_t
 * structure.
 *
 * @param id the ImageData_t structure.
 *
 * @return the dataType of this structure.
 *
 * @member of ImageData_t
 */
LIBSBML_EXTERN
const char *
ImageData_getDataType(const ImageData_t * id);


/**
 * Predicate returning @c 1 if the given ImageData_t structure's "samples"
 * is set.
 *
 * @param id the ImageData_t structure.
 *
 * @return @c 1 if the "samples" of this ImageData_t structure is
 * set, @c 0 otherwise.
 *
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int
ImageData_isSetSamples(const ImageData_t * id);


/**
 * Predicate returning @c 1 if the given ImageData_t structure's "samplesLength"
 * is set.
 *
 * @param id the ImageData_t structure.
 *
 * @return @c 1 if the "samplesLength" of this ImageData_t structure is
 * set, @c 0 otherwise.
 *
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int
ImageData_isSetSamplesLength(const ImageData_t * id);


/**
 * Predicate returning @c 1 if the given ImageData_t structure's "dataType"
 * is set.
 *
 * @param id the ImageData_t structure.
 *
 * @return @c 1 if the "dataType" of this ImageData_t structure is
 * set, @c 0 otherwise.
 *
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int
ImageData_isSetDataType(const ImageData_t * id);


/**
 * Sets the "samples" attribute of the given ImageData_t structure.
 *
 * @param id the ImageData_t structure.
 *
 * @param samples the string to which the structures "samples" attribute should be
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
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int
ImageData_setSamples(ImageData_t * id, int* samples);


/**
 * Sets the "samplesLength" attribute of the given ImageData_t structure.
 *
 * @param id the ImageData_t structure.
 *
 * @param samplesLength the string to which the structures "samplesLength" attribute should be
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
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int
ImageData_setSamplesLength(ImageData_t * id, int samplesLength);


/**
 * Sets the "dataType" attribute of the given ImageData_t structure.
 *
 * This function copies the string given in @p string.  If the string is
 * a null pointer, this function performs ImageData_unsetDataType() instead.
 *
 * @param id the ImageData_t structure.
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
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int
ImageData_setDataType(ImageData_t * id, const char * dataType);


/**
 * Unsets the value of the "samples" attribute of the given 
 *ImageData_t structure.
 *
 * @param id the ImageData_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int
ImageData_unsetSamples(ImageData_t * id);


/**
 * Unsets the value of the "samplesLength" attribute of the given 
 *ImageData_t structure.
 *
 * @param id the ImageData_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int
ImageData_unsetSamplesLength(ImageData_t * id);


/**
 * Unsets the value of the "dataType" attribute of the given 
 *ImageData_t structure.
 *
 * @param id the ImageData_t structure.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif@~ The possible values
 * returned by this function are:
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
 * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED@endlink
 * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT@endlink
 *
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int
ImageData_unsetDataType(ImageData_t * id);


/**
 * Predicate returning @c 1 or *c 0 depending on whether all the required
 * attributes of the given ImageData_t structure have been set.
 *
 * @param id the ImageData_t structure to check.
 *
 * @return @c 1 if all the required attributes for this
 * structure have been defined, @c 0 otherwise.
 *
 * @member of ImageData_t
 */
LIBSBML_EXTERN
int
ImageData_hasRequiredAttributes(const ImageData_t * id);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  ImageData_H__  */

