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


LIBSBML_CPP_NAMESPACE_BEGIN



class LIBSBML_EXTERN SampledField : public SBase
{

protected:

  std::string   mId;
  DataKind_t   mDataType;
  int           mNumSamples1;
  bool          mIsSetNumSamples1;
  int           mNumSamples2;
  bool          mIsSetNumSamples2;
  int           mNumSamples3;
  bool          mIsSetNumSamples3;
  InterpolationKind_t   mInterpolationType;
  CompressionKind_t   mCompression;
  int*         mSamples;
  int           mSamplesLength;
  bool          mIsSetSamplesLength;


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
   * @return the value of the "dataType" attribute of this SampledField as a DataKind_t.
   */
  virtual DataKind_t getDataType() const;


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
   * @return the value of the "interpolationType" attribute of this SampledField as a InterpolationKind_t.
   */
  virtual InterpolationKind_t getInterpolationType() const;


  /**
   * Returns the value of the "compression" attribute of this SampledField.
   *
   * @return the value of the "compression" attribute of this SampledField as a CompressionKind_t.
   */
  virtual CompressionKind_t getCompression() const;


  /**
   * The "samples" attribute of this SampledField is returned in an int* array (pointer)
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). The method itself has a return type void.
   *
   * NOTE: you have to pre-allocate the array with the correct length!   *
   * @return void.
   */
  void getSamples(int* outArray) const;


  /**
   * Returns the value of the "samplesLength" attribute of this SampledField.
   *
   * @return the value of the "samplesLength" attribute of this SampledField as a integer.
   */
  virtual int getSamplesLength() const;


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
   * SampledField's "compression" attribute has been set.
   *
   * @return @c true if this SampledField's "compression" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCompression() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "samples" attribute has been set.
   *
   * @return @c true if this SampledField's "samples" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetSamples() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SampledField's "samplesLength" attribute has been set.
   *
   * @return @c true if this SampledField's "samplesLength" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetSamplesLength() const;


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
   * Sets the value of the "dataType" attribute of this SampledField.
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
   * @param interpolationType; InterpolationKind_t value of the "interpolationType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setInterpolationType(InterpolationKind_t interpolationType);


  /**
   * Sets the value of the "interpolationType" attribute of this SampledField.
   *
   * @param interpolationType; string value of the "interpolationType" attribute to be set
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
   * Sets the value of the "compression" attribute of this SampledField.
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
   * Sets the value of the "compression" attribute of this SampledField.
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
   * Sets the "samples" element of this SampledField.
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
  virtual int setSamples(int* inArray, size_t arrayLength);


  /**
   * Sets the value of the "samplesLength" attribute of this SampledField.
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
   * Unsets the value of the "compression" attribute of this SampledField.
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
   * Unsets the value of the "samples" attribute of this SampledField.
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
   * Unsets the value of the "samplesLength" attribute of this SampledField.
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
   * @li "dataType"
   * @li "numSamples1"
   * @li "compression"
   * @li "samples"
   * @li "samplesLength"
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


  virtual void write(XMLOutputStream& stream) const;


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


  virtual void setElementText(const std::string &text);


protected:
  int* mUncompressedSamples;
  int mUncompressedLength;

  static void copySampleArrays(int* &target, int& targetLength, int* source, int sourceLength);
  static void uncompress_data(void *data, size_t length, int*& result, int& outLength);

public:

 /**
   * Returns the "samplesLength" attribute of this SampledField.
   *
   * @return the "samplesLength" attribute of this SampledField.
   */
  unsigned int getUncompressedLength();

 /**
   * The "samples" attribute of this SampledField is returned in an int array (pointer) 
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

class LIBSBML_EXTERN ListOfSampledFields : public ListOf
{

public:

  /**
   * Creates a new ListOfSampledFields with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfSampledFields
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfSampledFields
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to this ListOfSampledFields
   */
  ListOfSampledFields(unsigned int level      = SpatialExtension::getDefaultLevel(),
                      unsigned int version    = SpatialExtension::getDefaultVersion(),
                      unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfSampledFields with the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object
   */
  ListOfSampledFields(SpatialPkgNamespaces* spatialns);


   /**
   * Creates and returns a deep copy of this ListOfSampledFields object.
   *
   * @return a (deep) copy of this ListOfSampledFields object.
   */
  virtual ListOfSampledFields* clone () const;


   /**
   * Get a SampledField from the ListOfSampledFields.
   *
   * @param n the index number of the SampledField to get.
   *
   * @return the nth SampledField in this ListOfSampledFields.
   *
   * @see size()
   */
	virtual SampledField* get(unsigned int n);


  /**
   * Get a SampledField from the ListOfSampledFields.
   *
   * @param n the index number of the SampledField to get.
   *
   * @return the nth SampledField in this ListOfSampledFields.
   *
   * @see size()
   */
	virtual const SampledField* get(unsigned int n) const;


  /**
   * Get a SampledField from the ListOfSampledFields
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SampledField to get.
   *
   * @return SampledField in this ListOfSampledFields
   * with the given id or NULL if no such
   * SampledField exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
	virtual SampledField* get(const std::string& sid);


  /**
   * Get a SampledField from the ListOfSampledFields
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SampledField to get.
   *
   * @return SampledField in this ListOfSampledFields
   * with the given id or NULL if no such
   * SampledField exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const SampledField* get(const std::string& sid) const;


	/**
	 * Adds a copy the given "SampledField" to this ListOfSampledFields.
	 *
	 * @param sf; the SampledField object to add
	 *
	 * @return integer value indicating success/failure of the
	 * function.  @if clike The value is drawn from the
	 * enumeration #OperationReturnValues_t. @endif The possible values
	 * returned by this function are:
	 * @li LIBSEDML_OPERATION_SUCCESS
	 * @li LIBSEDML_INVALID_ATTRIBUTE_VALUE
	 */
	int addSampledField(const SampledField* sf);


	/**
	 * Get the number of SampledField objects in this ListOfSampledFields.
	 *
	 * @return the number of SampledField objects in this ListOfSampledFields
	 */
	unsigned int getNumSampledFields() const;


	/**
	 * Creates a new SampledField object, adds it to the
	 * ListOfSampledFields and returns the SampledField object created. 
	 *
	 * @return a new SampledField object instance
	 *
	 * @see addSampledField(const SampledField* sf)
	 */
	SampledField* createSampledField();


  /**
   * Removes the nth SampledField from this ListOfSampledFields
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SampledField to remove.
   *
   * @see size()
   */
	virtual SampledField* remove(unsigned int n);


  /**
   * Removes the SampledField from this ListOfSampledFields with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SampledField to remove.
   *
   * @return the SampledField removed. As mentioned above, the caller owns the
   * returned item.
   */
	virtual SampledField* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfSampledFields, is
   * always @c "listOfSampledFields".
   *
   * @return the name of this element, i.e. @c "listOfSampledFields".
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
   * Returns the libSBML type code for the SBML objects
   * contained in this ListOf object
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
   * @return the SBML type code for the objects in this ListOf instance, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getItemTypeCode () const;


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new SampledField in this ListOfSampledFields
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Spatial package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


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
DataKind_t
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
InterpolationKind_t
SampledField_getInterpolationType(const SampledField_t * sf);


/**
 * Returns the value of the "compression" attribute of the given SampledField_t
 * structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return the compression of this structure.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
CompressionKind_t
SampledField_getCompression(const SampledField_t * sf);


/**
 * Returns the value of the "samples" attribute of the given SampledField_t
 * structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return the samples of this structure.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int*
SampledField_getSamples(const SampledField_t * sf);


/**
 * Returns the value of the "samplesLength" attribute of the given SampledField_t
 * structure.
 *
 * @param sf the SampledField_t structure.
 *
 * @return the samplesLength of this structure.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_getSamplesLength(const SampledField_t * sf);


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
 * Predicate returning @c 1 if the given SampledField_t structure's "compression"
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if the "compression" of this SampledField_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetCompression(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if the given SampledField_t structure's "samples"
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if the "samples" of this SampledField_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetSamples(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if the given SampledField_t structure's "samplesLength"
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if the "samplesLength" of this SampledField_t structure is
 * set, @c 0 otherwise.
 *
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetSamplesLength(const SampledField_t * sf);


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
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setDataType(SampledField_t * sf, DataKind_t dataType);


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
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setInterpolationType(SampledField_t * sf, InterpolationKind_t interpolationType);


/**
 * Sets the "compression" attribute of the given SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
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
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setCompression(SampledField_t * sf, CompressionKind_t compression);


/**
 * Sets the "samples" attribute of the given SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
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
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setSamples(SampledField_t * sf, int* samples);


/**
 * Sets the "samplesLength" attribute of the given SampledField_t structure.
 *
 * @param sf the SampledField_t structure.
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
 * @member of SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setSamplesLength(SampledField_t * sf, int samplesLength);


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
 * Unsets the value of the "compression" attribute of the given 
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
SampledField_unsetCompression(SampledField_t * sf);


/**
 * Unsets the value of the "samples" attribute of the given 
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
SampledField_unsetSamples(SampledField_t * sf);


/**
 * Unsets the value of the "samplesLength" attribute of the given 
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
SampledField_unsetSamplesLength(SampledField_t * sf);


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


LIBSBML_EXTERN
SampledField_t *
ListOfSampledFields_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
SampledField_t *
ListOfSampledFields_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SampledField_H__  */

