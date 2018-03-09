/**
 * @file SampledField.h
 * @brief Definition of the SampledField class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class SampledField
 * @sbmlbrief{spatial} TODO:Definition of the SampledField class.
 */


#ifndef SampledField_H__
#define SampledField_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SampledField : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mId;
  DataKind_t mDataType;
  int mNumSamples1;
  bool mIsSetNumSamples1;
  int mNumSamples2;
  bool mIsSetNumSamples2;
  int mNumSamples3;
  bool mIsSetNumSamples3;
  InterpolationKind_t mInterpolationType;
  CompressionKind_t mCompression;
  int* mSamples;
  int mSamplesLength;
  bool mIsSetSamplesLength;

  /** @endcond */

public:

  /**
   * Creates a new SampledField using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * SampledField.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * SampledField.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this SampledField.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  SampledField(unsigned int level = SpatialExtension::getDefaultLevel(),
               unsigned int version = SpatialExtension::getDefaultVersion(),
               unsigned int pkgVersion =
                 SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new SampledField using the given SpatialPkgNamespaces object.
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @throws SBMLConstructorException
   * Thrown if the given @p level and @p version combination, or this kind of
   * SBML object, are either invalid or mismatched with respect to the parent
   * SBMLDocument object.
   * @copydetails doc_note_setting_lv
   */
  SampledField(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for SampledField.
   *
   * @param orig the SampledField instance to copy.
   */
  SampledField(const SampledField& orig);


  /**
   * Assignment operator for SampledField.
   *
   * @param rhs the SampledField object whose values are to be used as the
   * basis of the assignment.
   */
  SampledField& operator=(const SampledField& rhs);


  /**
   * Creates and returns a deep copy of this SampledField object.
   *
   * @return a (deep) copy of this SampledField object.
   */
  virtual SampledField* clone() const;


  /**
   * Destructor for SampledField.
   */
  virtual ~SampledField();


  /**
   * Returns the value of the "id" attribute of this SampledField.
   *
   * @return the value of the "id" attribute of this SampledField as a string.
   */
  const std::string& getId() const;


  /**
   * Returns the value of the "dataType" attribute of this SampledField.
   *
   * @return the value of the "dataType" attribute of this SampledField as a
   * DataKind_t.
   */
  DataKind_t getDataType() const;


  /**
   * Returns the value of the "dataType" attribute of this SampledField.
   *
   * @return the value of the "dataType" attribute of this SampledField as a
   * string.
   */
  std::string getDataTypeAsString() const;
  //bgoli22


  /**
   * Returns the value of the "numSamples1" attribute of this SampledField.
   *
   * @return the value of the "numSamples1" attribute of this SampledField as a
   * integer.
   */
  int getNumSamples1() const;


  /**
   * Returns the value of the "numSamples2" attribute of this SampledField.
   *
   * @return the value of the "numSamples2" attribute of this SampledField as a
   * integer.
   */
  int getNumSamples2() const;


  /**
   * Returns the value of the "numSamples3" attribute of this SampledField.
   *
   * @return the value of the "numSamples3" attribute of this SampledField as a
   * integer.
   */
  int getNumSamples3() const;


  /**
   * Returns the value of the "interpolationType" attribute of this
   * SampledField.
   *
   * @return the value of the "interpolationType" attribute of this
   * SampledField as a InterpolationKind_t.
   */
  InterpolationKind_t getInterpolationType() const;


  /**
   * Returns the value of the "interpolationType" attribute of this
   * SampledField.
   *
   * @return the value of the "interpolationType" attribute of this
   * SampledField as a string.
   */
  std::string getInterpolationTypeAsString() const;
  //bgoli22


  /**
   * Returns the value of the "compression" attribute of this SampledField.
   *
   * @return the value of the "compression" attribute of this SampledField as a
   * CompressionKind_t.
   */
  CompressionKind_t getCompression() const;


  /**
   * Returns the value of the "compression" attribute of this SampledField.
   *
   * @return the value of the "compression" attribute of this SampledField as a
   * string.
   */
  std::string getCompressionAsString() const;
  //bgoli22
  

  /**
   * Returns the value of the "samples" attribute of this SampledField.
   *
   * @param outArray int* array that will be used to return the value of the
   * "samples" attribute of this SampledField.
   *
   * @note the value of the "samples" attribute of this SampledField is
   * returned in the argument array.
   */
  void getSamples(int* outArray) const;


  /**
   * Returns the value of the "samplesLength" attribute of this SampledField.
   *
   * @return the value of the "samplesLength" attribute of this SampledField as
   * a integer.
   */
  int getSamplesLength() const;


  /**
   * Predicate returning @c true if this SampledField's "id" attribute is set.
   *
   * @return @c true if this SampledField's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetId() const;


  /**
   * Predicate returning @c true if this SampledField's "dataType" attribute is
   * set.
   *
   * @return @c true if this SampledField's "dataType" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetDataType() const;


  /**
   * Predicate returning @c true if this SampledField's "numSamples1" attribute
   * is set.
   *
   * @return @c true if this SampledField's "numSamples1" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetNumSamples1() const;


  /**
   * Predicate returning @c true if this SampledField's "numSamples2" attribute
   * is set.
   *
   * @return @c true if this SampledField's "numSamples2" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetNumSamples2() const;


  /**
   * Predicate returning @c true if this SampledField's "numSamples3" attribute
   * is set.
   *
   * @return @c true if this SampledField's "numSamples3" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetNumSamples3() const;


  /**
   * Predicate returning @c true if this SampledField's "interpolationType"
   * attribute is set.
   *
   * @return @c true if this SampledField's "interpolationType" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetInterpolationType() const;


  /**
   * Predicate returning @c true if this SampledField's "compression" attribute
   * is set.
   *
   * @return @c true if this SampledField's "compression" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetCompression() const;


  /**
   * Predicate returning @c true if this SampledField's "samples" attribute is
   * set.
   *
   * @return @c true if this SampledField's "samples" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetSamples() const;


  /**
   * Predicate returning @c true if this SampledField's "samplesLength"
   * attribute is set.
   *
   * @return @c true if this SampledField's "samplesLength" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetSamplesLength() const;


  /**
   * Sets the value of the "id" attribute of this SampledField.
   *
   * @param id std::string& value of the "id" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setId(const std::string& id);


  /**
   * Sets the value of the "dataType" attribute of this SampledField.
   *
   * @param dataType DataKind_t value of the "dataType" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDataType(const DataKind_t dataType);


  /**
   * Sets the value of the "dataType" attribute of this SampledField.
   *
   * @param dataType std::string& of the "dataType" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDataType(const std::string& dataType);


  /**
   * Sets the value of the "numSamples1" attribute of this SampledField.
   *
   * @param numSamples1 int value of the "numSamples1" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setNumSamples1(int numSamples1);


  /**
   * Sets the value of the "numSamples2" attribute of this SampledField.
   *
   * @param numSamples2 int value of the "numSamples2" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setNumSamples2(int numSamples2);


  /**
   * Sets the value of the "numSamples3" attribute of this SampledField.
   *
   * @param numSamples3 int value of the "numSamples3" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setNumSamples3(int numSamples3);


  /**
   * Sets the value of the "interpolationType" attribute of this SampledField.
   *
   * @param interpolationType InterpolationKind_t value of the
   * "interpolationType" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setInterpolationType(const InterpolationKind_t interpolationType);


  /**
   * Sets the value of the "interpolationType" attribute of this SampledField.
   *
   * @param interpolationType std::string& of the "interpolationType" attribute
   * to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setInterpolationType(const std::string& interpolationType);


  /**
   * Sets the value of the "compression" attribute of this SampledField.
   *
   * @param compression CompressionKind_t value of the "compression" attribute
   * to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCompression(const CompressionKind_t compression);


  /**
   * Sets the value of the "compression" attribute of this SampledField.
   *
   * @param compression std::string& of the "compression" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCompression(const std::string& compression);


  /**
   * Sets the value of the "samples" attribute of this SampledField.
   *
   * @param inArray int* array value of the "samples" attribute to be set.
   *
   * @param arrayLength int value for the length of the "samples" attribute to
   * be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSamples(int* inArray, int arrayLength);


  /**
   * Sets the value of the "samplesLength" attribute of this SampledField.
   *
   * @param samplesLength int value of the "samplesLength" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSamplesLength(int samplesLength);


  /**
   * Unsets the value of the "id" attribute of this SampledField.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetId();


  /**
   * Unsets the value of the "dataType" attribute of this SampledField.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDataType();


  /**
   * Unsets the value of the "numSamples1" attribute of this SampledField.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetNumSamples1();


  /**
   * Unsets the value of the "numSamples2" attribute of this SampledField.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetNumSamples2();


  /**
   * Unsets the value of the "numSamples3" attribute of this SampledField.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetNumSamples3();


  /**
   * Unsets the value of the "interpolationType" attribute of this
   * SampledField.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetInterpolationType();


  /**
   * Unsets the value of the "compression" attribute of this SampledField.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCompression();


  /**
   * Unsets the value of the "samples" attribute of this SampledField.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSamples();


  /**
   * Unsets the value of the "samplesLength" attribute of this SampledField.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSamplesLength();


  /**
   * Returns the XML element name of this SampledField object.
   *
   * For SampledField, the XML element name is always @c "sampledField".
   *
   * @return the name of this element, i.e. @c "sampledField".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this SampledField object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   *
   * @sbmlconstant{SBML_SPATIAL_SAMPLEDFIELD, SBMLSpatialTypeCode_t}
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * SampledField object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * SampledField have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the SampledField object are:
   * @li "id"
   * @li "dataType"
   * @li "numSamples1"
   * @li "interpolationType"
   * @li "compression"
   * @li "samples"
   * @li "samplesLength"
   */
  virtual bool hasRequiredAttributes() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Write any contained elements
   */
  virtual void writeElements(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument
   */
  virtual void setSBMLDocument(SBMLDocument* d);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * used to write arrays
   */
  virtual void write(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this SampledField.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, bool& value)
    const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this SampledField.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this SampledField.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           double& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this SampledField.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           unsigned int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this SampledField.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           std::string& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Predicate returning @c true if this SampledField's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this SampledField's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this SampledField.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, bool value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this SampledField.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this SampledField.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, double value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this SampledField.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           unsigned int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this SampledField.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           const std::string& value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Unsets the value of the "attributeName" attribute of this SampledField.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */




  #endif /* !SWIG */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds the expected attributes for this element
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual void readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the array data as a text element
   */
  virtual void setElementText(const std::string& text);

  /** @endcond */


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



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new SampledField_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SampledField_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SampledField_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this SampledField_t.
 *
 * @throws SBMLConstructorException
 * Thrown if the given @p level and @p version combination, or this kind of
 * SBML object, are either invalid or mismatched with respect to the parent
 * SBMLDocument object.
 * @copydetails doc_note_setting_lv
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
SampledField_t *
SampledField_create(unsigned int level,
                    unsigned int version,
                    unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this SampledField_t object.
 *
 * @param sf the SampledField_t structure.
 *
 * @return a (deep) copy of this SampledField_t object.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
SampledField_t*
SampledField_clone(const SampledField_t* sf);


/**
 * Frees this SampledField_t object.
 *
 * @param sf the SampledField_t structure.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
void
SampledField_free(SampledField_t* sf);


/**
 * Returns the value of the "id" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this SampledField_t as a pointer
 * to a string.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
const char *
SampledField_getId(const SampledField_t * sf);


/**
 * Returns the value of the "dataType" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure whose dataType is sought.
 *
 * @return the value of the "dataType" attribute of this SampledField_t as a
 * DataKind_t.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
DataKind_t
SampledField_getDataType(const SampledField_t * sf);


/**
 * Returns the value of the "dataType" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure whose dataType is sought.
 *
 * @return the value of the "dataType" attribute of this SampledField_t as a
 * const char *.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
const char *
SampledField_getDataTypeAsString(const SampledField_t * sf);


/**
 * Returns the value of the "numSamples1" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure whose numSamples1 is sought.
 *
 * @return the value of the "numSamples1" attribute of this SampledField_t as a
 * integer.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_getNumSamples1(const SampledField_t * sf);


/**
 * Returns the value of the "numSamples2" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure whose numSamples2 is sought.
 *
 * @return the value of the "numSamples2" attribute of this SampledField_t as a
 * integer.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_getNumSamples2(const SampledField_t * sf);


/**
 * Returns the value of the "numSamples3" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure whose numSamples3 is sought.
 *
 * @return the value of the "numSamples3" attribute of this SampledField_t as a
 * integer.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_getNumSamples3(const SampledField_t * sf);


/**
 * Returns the value of the "interpolationType" attribute of this
 * SampledField_t.
 *
 * @param sf the SampledField_t structure whose interpolationType is sought.
 *
 * @return the value of the "interpolationType" attribute of this
 * SampledField_t as a InterpolationKind_t.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
InterpolationKind_t
SampledField_getInterpolationType(const SampledField_t * sf);


/**
 * Returns the value of the "interpolationType" attribute of this
 * SampledField_t.
 *
 * @param sf the SampledField_t structure whose interpolationType is sought.
 *
 * @return the value of the "interpolationType" attribute of this
 * SampledField_t as a const char *.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
const char *
SampledField_getInterpolationTypeAsString(const SampledField_t * sf);


/**
 * Returns the value of the "compression" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure whose compression is sought.
 *
 * @return the value of the "compression" attribute of this SampledField_t as a
 * CompressionKind_t.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
CompressionKind_t
SampledField_getCompression(const SampledField_t * sf);


/**
 * Returns the value of the "compression" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure whose compression is sought.
 *
 * @return the value of the "compression" attribute of this SampledField_t as a
 * const char *.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
const char *
SampledField_getCompressionAsString(const SampledField_t * sf);


/**
 * Returns the value of the "samplesLength" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure whose samplesLength is sought.
 *
 * @return the value of the "samplesLength" attribute of this SampledField_t as
 * a integer.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_getSamplesLength(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if this SampledField_t's "id" attribute is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if this SampledField_t's "id" attribute has been set, otherwise
 * @c 0 is returned.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetId(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if this SampledField_t's "dataType" attribute is
 * set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if this SampledField_t's "dataType" attribute has been set,
 * otherwise @c 0 is returned.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetDataType(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if this SampledField_t's "numSamples1" attribute is
 * set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if this SampledField_t's "numSamples1" attribute has been set,
 * otherwise @c 0 is returned.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetNumSamples1(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if this SampledField_t's "numSamples2" attribute is
 * set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if this SampledField_t's "numSamples2" attribute has been set,
 * otherwise @c 0 is returned.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetNumSamples2(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if this SampledField_t's "numSamples3" attribute is
 * set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if this SampledField_t's "numSamples3" attribute has been set,
 * otherwise @c 0 is returned.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetNumSamples3(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if this SampledField_t's "interpolationType"
 * attribute is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if this SampledField_t's "interpolationType" attribute has been
 * set, otherwise @c 0 is returned.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetInterpolationType(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if this SampledField_t's "compression" attribute is
 * set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if this SampledField_t's "compression" attribute has been set,
 * otherwise @c 0 is returned.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetCompression(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if this SampledField_t's "samples" attribute is
 * set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if this SampledField_t's "samples" attribute has been set,
 * otherwise @c 0 is returned.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetSamples(const SampledField_t * sf);


/**
 * Predicate returning @c 1 if this SampledField_t's "samplesLength" attribute
 * is set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 if this SampledField_t's "samplesLength" attribute has been
 * set, otherwise @c 0 is returned.
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_isSetSamplesLength(const SampledField_t * sf);


/**
 * Sets the value of the "id" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setId(SampledField_t * sf, const char * id);


/**
 * Sets the value of the "dataType" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param dataType DataKind_t value of the "dataType" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setDataType(SampledField_t * sf, DataKind_t dataType);


/**
 * Sets the value of the "dataType" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param dataType const char * of the "dataType" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setDataTypeAsString(SampledField_t * sf, const char * dataType);


/**
 * Sets the value of the "numSamples1" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param numSamples1 int value of the "numSamples1" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setNumSamples1(SampledField_t * sf, int numSamples1);


/**
 * Sets the value of the "numSamples2" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param numSamples2 int value of the "numSamples2" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setNumSamples2(SampledField_t * sf, int numSamples2);


/**
 * Sets the value of the "numSamples3" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param numSamples3 int value of the "numSamples3" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setNumSamples3(SampledField_t * sf, int numSamples3);


/**
 * Sets the value of the "interpolationType" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param interpolationType InterpolationKind_t value of the
 * "interpolationType" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setInterpolationType(SampledField_t * sf,
                                  InterpolationKind_t interpolationType);


/**
 * Sets the value of the "interpolationType" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param interpolationType const char * of the "interpolationType" attribute
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setInterpolationTypeAsString(SampledField_t * sf,
                                          const char * interpolationType);


/**
 * Sets the value of the "compression" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param compression CompressionKind_t value of the "compression" attribute to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setCompression(SampledField_t * sf,
                            CompressionKind_t compression);


/**
 * Sets the value of the "compression" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param compression const char * of the "compression" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setCompressionAsString(SampledField_t * sf,
                                    const char * compression);


/**
 * Sets the value of the "samples" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param samples pointer value of the "samples" attribute to be set.
 *
 * @param arrayLength int value for the length of the "samples" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setSamples(SampledField_t* sf, int* samples, int arrayLength);


/**
 * Sets the value of the "samplesLength" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @param samplesLength int value of the "samplesLength" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_setSamplesLength(SampledField_t * sf, int samplesLength);


/**
 * Unsets the value of the "id" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetId(SampledField_t * sf);


/**
 * Unsets the value of the "dataType" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetDataType(SampledField_t * sf);


/**
 * Unsets the value of the "numSamples1" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetNumSamples1(SampledField_t * sf);


/**
 * Unsets the value of the "numSamples2" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetNumSamples2(SampledField_t * sf);


/**
 * Unsets the value of the "numSamples3" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetNumSamples3(SampledField_t * sf);


/**
 * Unsets the value of the "interpolationType" attribute of this
 * SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetInterpolationType(SampledField_t * sf);


/**
 * Unsets the value of the "compression" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetCompression(SampledField_t * sf);


/**
 * Unsets the value of the "samples" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetSamples(SampledField_t * sf);


/**
 * Unsets the value of the "samplesLength" attribute of this SampledField_t.
 *
 * @param sf the SampledField_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_unsetSamplesLength(SampledField_t * sf);


/**
 * Predicate returning @c 1 if all the required attributes for this
 * SampledField_t object have been set.
 *
 * @param sf the SampledField_t structure.
 *
 * @return @c 1 to indicate that all the required attributes of this
 * SampledField_t have been set, otherwise @c 0 is returned.
 *
 *
 * @note The required attributes for the SampledField_t object are:
 * @li "id"
 * @li "dataType"
 * @li "numSamples1"
 * @li "interpolationType"
 * @li "compression"
 * @li "samples"
 * @li "samplesLength"
 *
 * @memberof SampledField_t
 */
LIBSBML_EXTERN
int
SampledField_hasRequiredAttributes(const SampledField_t * sf);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !SampledField_H__ */


