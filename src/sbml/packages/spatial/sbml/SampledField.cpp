/**
 * @file SampledField.cpp
 * @brief Implementation of the SampledField class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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
 */
#include <sbml/packages/spatial/sbml/SampledField.h>
#include <sbml/packages/spatial/sbml/ListOfSampledFields.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/packages/spatial/common/CompressionUtil.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus

/*
 * Creates a new SampledField using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
SampledField::SampledField(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
  : SBase(level, version)
  , mDataType(SPATIAL_DATAKIND_INVALID)
  , mNumSamples1(0)
  , mIsSetNumSamples1(false)
  , mNumSamples2(0)
  , mIsSetNumSamples2(false)
  , mNumSamples3(0)
  , mIsSetNumSamples3(false)
  , mInterpolationType(SPATIAL_INTERPOLATIONKIND_INVALID)
  , mCompression(SPATIAL_COMPRESSIONKIND_INVALID)
  , mSamples()
  , mSamplesLength(SBML_INT_MAX)
  , mIsSetSamplesLength(false)
  , mSamplesCompressed(NULL)
  , mSamplesUncompressed(NULL)
  , mSamplesUncompressedInt(NULL)
  , mSamplesCompressedLength(0)
  , mSamplesUncompressedLength(0)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new SampledField using the given SpatialPkgNamespaces object.
 */
SampledField::SampledField(SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mDataType(SPATIAL_DATAKIND_INVALID)
  , mNumSamples1(0)
  , mIsSetNumSamples1(false)
  , mNumSamples2(0)
  , mIsSetNumSamples2(false)
  , mNumSamples3(0)
  , mIsSetNumSamples3(false)
  , mInterpolationType(SPATIAL_INTERPOLATIONKIND_INVALID)
  , mCompression(SPATIAL_COMPRESSIONKIND_INVALID)
  , mSamples()
  , mSamplesLength(SBML_INT_MAX)
  , mIsSetSamplesLength(false)
  , mSamplesCompressed(NULL)
  , mSamplesUncompressed(NULL)
  , mSamplesUncompressedInt(NULL)
  , mSamplesCompressedLength(0)
  , mSamplesUncompressedLength(0)
{
  setElementNamespace(spatialns->getURI());
  // connect to child objects
  connectToChild();

  loadPlugins(spatialns);
}


/*
 * Copy constructor for SampledField.
 */
SampledField::SampledField(const SampledField& orig)
  : SBase(orig)
  , mDataType(orig.mDataType)
  , mNumSamples1(orig.mNumSamples1)
  , mIsSetNumSamples1(orig.mIsSetNumSamples1)
  , mNumSamples2(orig.mNumSamples2)
  , mIsSetNumSamples2(orig.mIsSetNumSamples2)
  , mNumSamples3(orig.mNumSamples3)
  , mIsSetNumSamples3(orig.mIsSetNumSamples3)
  , mInterpolationType(orig.mInterpolationType)
  , mCompression(orig.mCompression)
  , mSamples(orig.mSamples)
  , mSamplesLength(orig.mSamplesLength)
  , mIsSetSamplesLength(orig.mIsSetSamplesLength)
  , mSamplesCompressed(NULL)
  , mSamplesUncompressed(NULL)
  , mSamplesUncompressedInt(NULL)
  , mSamplesCompressedLength(0)
  , mSamplesUncompressedLength(0)
{
  // connect to child objects
  connectToChild();
}


/*
 * Assignment operator for SampledField.
 */
SampledField&
SampledField::operator=(const SampledField& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mDataType = rhs.mDataType;
    mNumSamples1 = rhs.mNumSamples1;
    mIsSetNumSamples1 = rhs.mIsSetNumSamples1;
    mNumSamples2 = rhs.mNumSamples2;
    mIsSetNumSamples2 = rhs.mIsSetNumSamples2;
    mNumSamples3 = rhs.mNumSamples3;
    mIsSetNumSamples3 = rhs.mIsSetNumSamples3;
    mInterpolationType = rhs.mInterpolationType;
    mCompression = rhs.mCompression;
    mSamples = rhs.mSamples;
    mSamplesLength = rhs.mSamplesLength;
    mIsSetSamplesLength = rhs.mIsSetSamplesLength;

    freeCompressed();
    freeUncompressed();
    // connect to child objects
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SampledField object.
 */
SampledField*
SampledField::clone() const
{
  return new SampledField(*this);
}


/*
 * Destructor for SampledField.
 */
SampledField::~SampledField()
{
  freeCompressed();
  freeUncompressed();
}


/*
 * Returns the value of the "id" attribute of this SampledField.
 */
const std::string&
SampledField::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this SampledField.
 */
const std::string&
SampledField::getName() const
{
  return mName;
}


/*
 * Returns the value of the "dataType" attribute of this SampledField.
 */
DataKind_t
SampledField::getDataType() const
{
  return mDataType;
}


/*
 * Returns the value of the "dataType" attribute of this SampledField.
 */
std::string
SampledField::getDataTypeAsString() const
{
  return DataKind_toString(mDataType);
}


/*
 * Returns the value of the "numSamples1" attribute of this SampledField.
 */
int
SampledField::getNumSamples1() const
{
  return mNumSamples1;
}


/*
 * Returns the value of the "numSamples2" attribute of this SampledField.
 */
int
SampledField::getNumSamples2() const
{
  return mNumSamples2;
}


/*
 * Returns the value of the "numSamples3" attribute of this SampledField.
 */
int
SampledField::getNumSamples3() const
{
  return mNumSamples3;
}


/*
 * Returns the value of the "interpolationType" attribute of this SampledField.
 */
InterpolationKind_t
SampledField::getInterpolationType() const
{
  return mInterpolationType;
}


/*
 * Returns the value of the "interpolationType" attribute of this SampledField.
 */
std::string
SampledField::getInterpolationTypeAsString() const
{
  return InterpolationKind_toString(mInterpolationType);
}


/*
 * Returns the value of the "compression" attribute of this SampledField.
 */
CompressionKind_t
SampledField::getCompression() const
{
  return mCompression;
}


/*
 * Returns the value of the "compression" attribute of this SampledField.
 */
std::string
SampledField::getCompressionAsString() const
{
  std::string code_str = CompressionKind_toString(mCompression);
  return code_str;
}


void SampledField::getSamples(std::vector<int>& outVector) const
{
  readSamplesFromString<int>(mSamples, outVector);
  if (outVector.size() != getActualSamplesLength()) {
    outVector.clear();
  }
}

void SampledField::getSamples(std::vector<float>& outVector) const
{
  store();
  string uncompressedString;
  uncompressInternal(uncompressedString, mSamplesUncompressedLength);
  readSamplesFromString<float>(uncompressedString, outVector);
}

void SampledField::getSamples(std::vector<double>& outVector) const
{
  store();
  string uncompressedString;
  uncompressInternal(uncompressedString, mSamplesUncompressedLength);
  readSamplesFromString<double>(uncompressedString, outVector);
}

std::string SampledField::getSamples() const
{
  return mSamples;
}

int
SampledField::getSamples(int* outArray) const
{
  if (outArray == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  store();
  if (mCompression == SPATIAL_COMPRESSIONKIND_DEFLATED)
  {
    if (mSamplesCompressed == NULL)
    {
      return LIBSBML_OPERATION_FAILED;
    }
    memcpy(outArray, mSamplesCompressed, sizeof(int)*mSamplesCompressedLength);
  }
  else
  {
    if (mSamplesUncompressedInt == NULL)
    {
      return LIBSBML_OPERATION_FAILED;
    }
    memcpy(outArray, mSamplesUncompressedInt, sizeof(int)*mSamplesUncompressedLength);
  }
  return LIBSBML_OPERATION_SUCCESS;
}

int
SampledField::getSamples(double* outArray) const
{
  if (outArray == NULL) 
  {
    return LIBSBML_OPERATION_FAILED;
  }
  //This function will uncompres and store the data if need be:
  size_t len = getUncompressedLength();

  if (mSamplesUncompressed == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }

  memcpy(outArray, mSamplesUncompressed, sizeof(double)*mSamplesUncompressedLength);
  return LIBSBML_OPERATION_SUCCESS;
}


int
SampledField::getSamples(float* outArray) const
{
  if (outArray == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }

  store();
  float* samples = NULL;
  size_t length;
  if (mCompression == SPATIAL_COMPRESSIONKIND_DEFLATED)
  {
    string uncompressedString;
    uncompressInternal(uncompressedString, length);
    samples = readSamplesFromString<float>(uncompressedString, length);
  }
  else {
    samples = readSamplesFromString<float>(mSamples, length);
  }

  if (samples == NULL || length==0)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  memcpy(outArray, samples, sizeof(float) * length);
  free(samples);
  return LIBSBML_OPERATION_SUCCESS;
}

int
SampledField::getSamplesLength() const
{
  return mSamplesLength;
}

size_t SampledField::getActualSamplesLength() const
{
  store();
  if (mCompression == SPATIAL_COMPRESSIONKIND_DEFLATED) {
    return mSamplesCompressedLength;
  }
  return mSamplesUncompressedLength;
}


/*
 * Predicate returning @c true if this SampledField's "id" attribute is set.
 */
bool
SampledField::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this SampledField's "name" attribute is set.
 */
bool
SampledField::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this SampledField's "dataType" attribute is
 * set.
 */
bool
SampledField::isSetDataType() const
{
  return (mDataType != SPATIAL_DATAKIND_INVALID);
}


/*
 * Predicate returning @c true if this SampledField's "numSamples1" attribute
 * is set.
 */
bool
SampledField::isSetNumSamples1() const
{
  return mIsSetNumSamples1;
}


/*
 * Predicate returning @c true if this SampledField's "numSamples2" attribute
 * is set.
 */
bool
SampledField::isSetNumSamples2() const
{
  return mIsSetNumSamples2;
}


/*
 * Predicate returning @c true if this SampledField's "numSamples3" attribute
 * is set.
 */
bool
SampledField::isSetNumSamples3() const
{
  return mIsSetNumSamples3;
}


/*
 * Predicate returning @c true if this SampledField's "interpolationType"
 * attribute is set.
 */
bool
SampledField::isSetInterpolationType() const
{
  return (mInterpolationType != SPATIAL_INTERPOLATIONKIND_INVALID);
}


/*
 * Predicate returning @c true if this SampledField's "compression" attribute
 * is set.
 */
bool
SampledField::isSetCompression() const
{
  return (mCompression != SPATIAL_COMPRESSIONKIND_INVALID);
}


/*
 * Predicate returning @c true if this SampledField's "samples" attribute is
 * set.
 */
bool
SampledField::isSetSamples() const
{
  return (!mSamples.empty());
}


/*
 * Predicate returning @c true if this SampledField's "samplesLength" attribute
 * is set.
 */
bool
SampledField::isSetSamplesLength() const
{
  return mIsSetSamplesLength;
}


/*
 * Sets the value of the "id" attribute of this SampledField.
 */
int
SampledField::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this SampledField.
 */
int
SampledField::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "dataType" attribute of this SampledField.
 */
int
SampledField::setDataType(const DataKind_t dataType)
{
  if (DataKind_isValid(dataType) == 0)
  {
    mDataType = SPATIAL_DATAKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDataType = dataType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "dataType" attribute of this SampledField.
 */
int
SampledField::setDataType(const std::string& dataType)
{
  mDataType = DataKind_fromString(dataType.c_str());

  if (mDataType == SPATIAL_DATAKIND_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "numSamples1" attribute of this SampledField.
 */
int
SampledField::setNumSamples1(int numSamples1)
{
  if (numSamples1 < 1) {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  mNumSamples1 = numSamples1;
  mIsSetNumSamples1 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "numSamples2" attribute of this SampledField.
 */
int
SampledField::setNumSamples2(int numSamples2)
{
  if (numSamples2 < 1) {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  mNumSamples2 = numSamples2;
  mIsSetNumSamples2 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "numSamples3" attribute of this SampledField.
 */
int
SampledField::setNumSamples3(int numSamples3)
{
  if (numSamples3 < 1) {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  mNumSamples3 = numSamples3;
  mIsSetNumSamples3 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "interpolationType" attribute of this SampledField.
 */
int
SampledField::setInterpolationType(const InterpolationKind_t interpolationType)
{
  if (InterpolationKind_isValid(interpolationType) == 0)
  {
    mInterpolationType = SPATIAL_INTERPOLATIONKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mInterpolationType = interpolationType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "interpolationType" attribute of this SampledField.
 */
int
SampledField::setInterpolationType(const std::string& interpolationType)
{
  mInterpolationType = InterpolationKind_fromString(interpolationType.c_str());

  if (mInterpolationType == SPATIAL_INTERPOLATIONKIND_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "compression" attribute of this SampledField.
 */
int
SampledField::setCompression(const CompressionKind_t compression)
{
  if (CompressionKind_isValid(compression) == 0)
  {
    mCompression = SPATIAL_COMPRESSIONKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCompression = compression;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "compression" attribute of this SampledField.
 */
int
SampledField::setCompression(const std::string& compression)
{
  if (CompressionKind_isValidString(compression.c_str()) == 0)
  {
    mCompression = SPATIAL_COMPRESSIONKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCompression = CompressionKind_fromString(compression.c_str());
    return LIBSBML_OPERATION_SUCCESS;
  }
  //bgoli22
  mCompression = CompressionKind_fromString(compression.c_str());
  if (mCompression == SPATIAL_COMPRESSIONKIND_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  return LIBSBML_OPERATION_SUCCESS;
}


int
SampledField::setSamples(double* inArray, size_t arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  setCompression(SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);

  freeCompressed();
  freeUncompressed();
  copySampleArrays(mSamplesUncompressed, mSamplesUncompressedLength, inArray, arrayLength);
  mSamples = arrayToString(inArray, arrayLength);
  return setSamplesLength(arrayLength);
}


int
SampledField::setSamples(int* inArray, size_t arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  freeCompressed();
  freeUncompressed();
  if (mCompression == SPATIAL_COMPRESSIONKIND_DEFLATED)
  {
    copySampleArrays(mSamplesCompressed, mSamplesCompressedLength, inArray, arrayLength);
    mSamples = arrayToString(inArray, arrayLength);
    setSamplesLength(arrayLength);
  }
  else {
    copySampleArrays(mSamplesUncompressedInt, mSamplesUncompressedLength, inArray, arrayLength);
    mSamples = arrayToString(inArray, arrayLength);
    setSamplesLength(arrayLength);
  }

  return LIBSBML_OPERATION_SUCCESS;
}


int SampledField::setSamples(unsigned int* inArray, size_t arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  mSamples = arrayToString(inArray, arrayLength);

  return setSamplesLength(arrayLength);
}

int SampledField::setSamples(unsigned char* inArray, size_t arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  mSamples = arrayToString(inArray, arrayLength);

  return setSamplesLength(arrayLength);
}


int
SampledField::setSamples(float* inArray, size_t arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  setCompression(SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);

  mSamples = arrayToString(inArray, arrayLength);

  return setSamplesLength(arrayLength);
}

int SampledField::setSamples(const std::string& samples)
{
  mSamples = samples;
  return LIBSBML_OPERATION_SUCCESS;
}

int SampledField::setSamples(const std::vector<double>& samples)
{
  mSamples = vectorToString(samples);
  setCompression(SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
  return setSamplesLength(samples.size());
}

int SampledField::setSamples(const std::vector<int>& samples)
{
  mSamples = vectorToString(samples);
  return setSamplesLength(samples.size());
}

int SampledField::setSamples(const std::vector<float>& samples)
{
  mSamples = vectorToString(samples);
  setCompression(SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
  return setSamplesLength(samples.size());
}

/*
 * Sets the value of the "samplesLength" attribute of this SampledField.
 */
int
SampledField::setSamplesLength(int samplesLength)
{
  mSamplesLength = samplesLength;
  mIsSetSamplesLength = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this SampledField.
 */
int
SampledField::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "name" attribute of this SampledField.
 */
int
SampledField::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "dataType" attribute of this SampledField.
 */
int
SampledField::unsetDataType()
{
  mDataType = SPATIAL_DATAKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "numSamples1" attribute of this SampledField.
 */
int
SampledField::unsetNumSamples1()
{
  mNumSamples1 = SBML_INT_MAX;
  mIsSetNumSamples1 = false;

  if (isSetNumSamples1() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "numSamples2" attribute of this SampledField.
 */
int
SampledField::unsetNumSamples2()
{
  mNumSamples2 = SBML_INT_MAX;
  mIsSetNumSamples2 = false;

  if (isSetNumSamples2() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "numSamples3" attribute of this SampledField.
 */
int
SampledField::unsetNumSamples3()
{
  mNumSamples3 = SBML_INT_MAX;
  mIsSetNumSamples3 = false;

  if (isSetNumSamples3() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "interpolationType" attribute of this SampledField.
 */
int
SampledField::unsetInterpolationType()
{
  mInterpolationType = SPATIAL_INTERPOLATIONKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "compression" attribute of this SampledField.
 */
int
SampledField::unsetCompression()
{
  mCompression = SPATIAL_COMPRESSIONKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "samples" attribute of this SampledField.
 */
int
SampledField::unsetSamples()
{
  mSamples.clear();
  freeCompressed();
  freeUncompressed();

  return unsetSamplesLength();
}


/*
 * Unsets the value of the "samplesLength" attribute of this SampledField.
 */
int
SampledField::unsetSamplesLength()
{
  mSamplesLength = SBML_INT_MAX;
  mIsSetSamplesLength = false;

  if (isSetSamplesLength() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the XML element name of this SampledField object.
 */
const std::string&
SampledField::getElementName() const
{
  static const string name = "sampledField";
  return name;
}


/*
 * Returns the libSBML type code for this SampledField object.
 */
int
SampledField::getTypeCode() const
{
  return SBML_SPATIAL_SAMPLEDFIELD;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * SampledField object have been set.
 */
bool
SampledField::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetId() == false)
  {
    allPresent = false;
  }

  if (isSetDataType() == false)
  {
    allPresent = false;
  }

  if (isSetNumSamples1() == false)
  {
    allPresent = false;
  }

  if (isSetInterpolationType() == false)
  {
    allPresent = false;
  }

  if (isSetCompression() == false)
  {
    allPresent = false;
  }

  if (isSetSamples() == false)
  {
    allPresent = false;
  }

  if (isSetSamplesLength() == false)
  {
    allPresent = false;
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
SampledField::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
SampledField::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
SampledField::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * used to write arrays
 */
void
SampledField::write(XMLOutputStream& stream) const
{
  stream.startElement(getElementName(), getPrefix());
  writeAttributes(stream);

  if (isSetSamples())
  {
    //for (int i = 0; i < mSamplesLength; ++i)
    //{
    //  stream << (long)mSamples[i] << " ";
    //}

    stream << mSamples;
  }

  stream.endElement(getElementName(), getPrefix());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
SampledField::enablePackageInternal(const std::string& pkgURI,
  const std::string& pkgPrefix,
  bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SampledField.
 */
int
SampledField::getAttribute(const std::string& attributeName,
  bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SampledField.
 */
int
SampledField::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "numSamples1")
  {
    value = getNumSamples1();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "numSamples2")
  {
    value = getNumSamples2();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "numSamples3")
  {
    value = getNumSamples3();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "samplesLength")
  {
    value = getSamplesLength();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SampledField.
 */
int
SampledField::getAttribute(const std::string& attributeName,
  double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SampledField.
 */
int
SampledField::getAttribute(const std::string& attributeName,
  unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SampledField.
 */
int
SampledField::getAttribute(const std::string& attributeName,
  std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "dataType")
  {
    value = getDataTypeAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "interpolationType")
  {
    value = getInterpolationTypeAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "compression")
  {
    value = getCompressionAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this SampledField's attribute "attributeName"
 * is set.
 */
bool
SampledField::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }
  else if (attributeName == "dataType")
  {
    value = isSetDataType();
  }
  else if (attributeName == "numSamples1")
  {
    value = isSetNumSamples1();
  }
  else if (attributeName == "numSamples2")
  {
    value = isSetNumSamples2();
  }
  else if (attributeName == "numSamples3")
  {
    value = isSetNumSamples3();
  }
  else if (attributeName == "interpolationType")
  {
    value = isSetInterpolationType();
  }
  else if (attributeName == "compression")
  {
    value = isSetCompression();
  }
  else if (attributeName == "samples")
  {
    value = isSetSamples();
  }
  else if (attributeName == "samplesLength")
  {
    value = isSetSamplesLength();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SampledField.
 */
int
SampledField::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SampledField.
 */
int
SampledField::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "numSamples1")
  {
    return_value = setNumSamples1(value);
  }
  else if (attributeName == "numSamples2")
  {
    return_value = setNumSamples2(value);
  }
  else if (attributeName == "numSamples3")
  {
    return_value = setNumSamples3(value);
  }
  else if (attributeName == "samplesLength")
  {
    return_value = setSamplesLength(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SampledField.
 */
int
SampledField::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SampledField.
 */
int
SampledField::setAttribute(const std::string& attributeName,
  unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SampledField.
 */
int
SampledField::setAttribute(const std::string& attributeName,
  const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }
  else if (attributeName == "dataType")
  {
    return_value = setDataType(value);
  }
  else if (attributeName == "interpolationType")
  {
    return_value = setInterpolationType(value);
  }
  else if (attributeName == "compression")
  {
    return_value = setCompression(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this SampledField.
 */
int
SampledField::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }
  else if (attributeName == "dataType")
  {
    value = unsetDataType();
  }
  else if (attributeName == "numSamples1")
  {
    value = unsetNumSamples1();
  }
  else if (attributeName == "numSamples2")
  {
    value = unsetNumSamples2();
  }
  else if (attributeName == "numSamples3")
  {
    value = unsetNumSamples3();
  }
  else if (attributeName == "interpolationType")
  {
    value = unsetInterpolationType();
  }
  else if (attributeName == "compression")
  {
    value = unsetCompression();
  }
  else if (attributeName == "samples")
  {
    value = unsetSamples();
  }
  else if (attributeName == "samplesLength")
  {
    value = unsetSamplesLength();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
SampledField::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("dataType");

  attributes.add("numSamples1");

  attributes.add("numSamples2");

  attributes.add("numSamples3");

  attributes.add("interpolationType");

  attributes.add("compression");

  attributes.add("samplesLength");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
SampledField::readAttributes(const XMLAttributes& attributes,
  const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfSampledFields*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs - 1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialSampledFieldAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialGeometryLOSampledFieldsAllowedCoreAttributes, pkgVersion, level,
          version, details);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs - 1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialSampledFieldAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialSampledFieldAllowedCoreAttributes, pkgVersion, level, version,
          details);
      }
    }
  }

  // 
  // id SId (use = "required" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true && log)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<sampledField>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("spatial", SpatialIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
        "which does not conform to the syntax.", getLine(), getColumn());
    }
  }
  else if (log)
  {
    std::string message = "Spatial attribute 'id' is missing from the "
      "<sampledField> element.";
    log->logPackageError("spatial", SpatialSampledFieldAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (assigned == true && log)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<sampledField>");
    }
  }

  // 
  // dataType enum (use = "required" )
  // 

  std::string dataType;
  assigned = attributes.readInto("dataType", dataType);

  if (assigned == true && log)
  {
    if (dataType.empty() == true)
    {
      logEmptyString(dataType, level, version, "<sampledField>");
    }
    else
    {
      mDataType = DataKind_fromString(dataType.c_str());

      if (DataKind_isValid(mDataType) == 0)
      {
        std::string msg = "The dataType on the <sampledField> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + dataType + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialSampledFieldDataTypeMustBeDataKindEnum, pkgVersion, level,
          version, msg);
      }
    }
  }
  else if (log)
  {
    std::string message = "Spatial attribute 'dataType' is missing.";
    log->logPackageError("spatial", SpatialSampledFieldAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // numSamples1 int (use = "required" )
  // 

  numErrs = log == NULL ? 0 : log->getNumErrors();
  mIsSetNumSamples1 = attributes.readInto("numSamples1", mNumSamples1);

  if (mIsSetNumSamples1 == false && log)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'numSamples1' from the "
        "<sampledField> element must be an integer.";
      log->logPackageError("spatial",
        SpatialSampledFieldNumSamples1MustBeInteger, pkgVersion, level, version,
        message);
    }
    else
    {
      std::string message = "Spatial attribute 'numSamples1' is missing from "
        "the <SampledField> element.";
      log->logPackageError("spatial", SpatialSampledFieldAllowedAttributes,
        pkgVersion, level, version, message);
    }
  }

  // 
  // numSamples2 int (use = "optional" )
  // 

  numErrs = log == NULL ? 0 : log->getNumErrors();
  mIsSetNumSamples2 = attributes.readInto("numSamples2", mNumSamples2);

  if (mIsSetNumSamples2 == false && log)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'numSamples2' from the "
        "<sampledField> element must be an integer.";
      log->logPackageError("spatial",
        SpatialSampledFieldNumSamples2MustBeInteger, pkgVersion, level, version,
        message);
    }
  }

  // 
  // numSamples3 int (use = "optional" )
  // 

  numErrs = log == NULL ? 0 : log->getNumErrors();
  mIsSetNumSamples3 = attributes.readInto("numSamples3", mNumSamples3);

  if (mIsSetNumSamples3 == false && log)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'numSamples3' from the "
        "<sampledField> element must be an integer.";
      log->logPackageError("spatial",
        SpatialSampledFieldNumSamples3MustBeInteger, pkgVersion, level, version,
        message);
    }
  }

  // 
  // interpolationType enum (use = "required" )
  // 

  std::string interpolationType;
  assigned = attributes.readInto("interpolationType", interpolationType);

  if (assigned == true && log)
  {
    if (interpolationType.empty() == true)
    {
      logEmptyString(interpolationType, level, version, "<sampledField>");
    }
    else
    {
      mInterpolationType =
        InterpolationKind_fromString(interpolationType.c_str());

      if (InterpolationKind_isValid(mInterpolationType) == 0)
      {
        std::string msg = "The interpolationType on the <SampledField> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + interpolationType + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialSampledFieldInterpolationTypeMustBeInterpolationKindEnum,
          pkgVersion, level, version, msg);
      }
    }
  }
  else if (log)
  {
    std::string message = "Spatial attribute 'interpolationType' is missing.";
    log->logPackageError("spatial", SpatialSampledFieldAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // compression enum (use = "required" )
  // 

  std::string compression;
  assigned = attributes.readInto("compression", compression);

  if (assigned == true && log)
  {
    if (compression.empty() == true)
    {
      logEmptyString(compression, level, version, "<sampledField>");
    }
    else
    {
      mCompression = CompressionKind_fromString(compression.c_str());

      if (CompressionKind_isValid(mCompression) == 0)
      {
        std::string msg = "The compression on the <SampledField> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + compression + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialSampledFieldCompressionMustBeCompressionKindEnum, pkgVersion,
          level, version, msg);
      }
    }
  }
  else if (log)
  {
    std::string message = "Spatial attribute 'compression' is missing.";
    log->logPackageError("spatial", SpatialSampledFieldAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // samplesLength int (use = "required" )
  // 

  numErrs = log == NULL ? 0 : log->getNumErrors();
  mIsSetSamplesLength = attributes.readInto("samplesLength", mSamplesLength);

  if (mIsSetSamplesLength == false && log)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'samplesLength' from the "
        "<sampledField> element must be an integer.";
      log->logPackageError("spatial",
        SpatialSampledFieldSamplesLengthMustBeInteger, pkgVersion, level,
        version, message);
    }
    else
    {
      std::string message = "Spatial attribute 'samplesLength' is missing from "
        "the <SampledField> element.";
      log->logPackageError("spatial", SpatialSampledFieldAllowedAttributes,
        pkgVersion, level, version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
SampledField::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  if (isSetDataType() == true)
  {
    stream.writeAttribute("dataType", getPrefix(),
      DataKind_toString(mDataType));
  }

  if (isSetNumSamples1() == true)
  {
    stream.writeAttribute("numSamples1", getPrefix(), mNumSamples1);
  }

  if (isSetNumSamples2() == true)
  {
    stream.writeAttribute("numSamples2", getPrefix(), mNumSamples2);
  }

  if (isSetNumSamples3() == true)
  {
    stream.writeAttribute("numSamples3", getPrefix(), mNumSamples3);
  }

  if (isSetInterpolationType() == true)
  {
    stream.writeAttribute("interpolationType", getPrefix(),
      InterpolationKind_toString(mInterpolationType));
  }

  if (isSetCompression() == true)
  {
    stream.writeAttribute("compression", getPrefix(),
      CompressionKind_toString(mCompression));
  }

  if (isSetSamplesLength() == true)
  {
    stream.writeAttribute("samplesLength", getPrefix(), mSamplesLength);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
* Writes the array data as a text element
*/
void
SampledField::setElementText(const std::string& text)
{
  mSamples = text;
  SBMLErrorLog* log = getErrorLog();
  if (log)
  {
    if (mCompression == SPATIAL_COMPRESSIONKIND_UNCOMPRESSED)
    {
      stringstream strStream(text);
      double val;

      while (strStream >> val)
      {
        if (strStream.peek() == ',') {
          strStream.get();
        }
        if (strStream.peek() == ';') {
          strStream.get();
        }
      }
      if (strStream.fail() && !strStream.eof())
      {
        stringstream ss_msg;
        ss_msg << "A <SampledField>";
        if (isSetId())
        {
          ss_msg << " with id '" << getId() << "'";
        }
        ss_msg << " has a compression type of 'uncompressed', but contains non-numeric elements.";

        log->logPackageError("spatial",
          SpatialSampledFieldSamplesMustBeNumeric,
          getPackageVersion(), getLevel(), getVersion(), ss_msg.str());
      }
    }
    else if (mCompression == SPATIAL_COMPRESSIONKIND_DEFLATED)
    {
      size_t doubleslen;
      double ival;
      double* doublesVector = readSamplesFromString<double>(mSamples, doubleslen);
      for (size_t i = 0; i < doubleslen; i++)
      {
        if (modf(doublesVector[i], &ival) != 0) //Maybe also check to make sure it's in the -128 to 127 range?
        {
          stringstream ss_msg;
          ss_msg << "A <SampledField>";
          if (isSetId())
          {
            ss_msg << " with id '" << getId() << "'";
          }
          ss_msg << " has a compression type of 'deflated', but has an entry with the value '" << doublesVector[i];
          ss_msg << "', which is not an integer.";

          log->logPackageError("spatial",
            SpatialSampledFieldCompressedSamplesMustBeInts,
            getPackageVersion(), getLevel(), getVersion(), ss_msg.str());
        }
      }
      free(doublesVector);
    }
  }
}

void SampledField::store() const
{
  if (mCompression == SPATIAL_COMPRESSIONKIND_DEFLATED) 
  {
    if (mSamplesCompressed == NULL) {
      mSamplesCompressed = readSamplesFromString<int>(mSamples, mSamplesCompressedLength);
    }
  }
  else
  {
    if (mSamplesUncompressed == NULL) {
      mSamplesUncompressed = readSamplesFromString<double>(mSamples, mSamplesUncompressedLength);
      size_t alt_length;
      free(mSamplesUncompressedInt);
      mSamplesUncompressedInt = readSamplesFromString<int>(mSamples, alt_length);
      if (alt_length != mSamplesUncompressedLength)
      {
        free(mSamplesUncompressedInt);
        mSamplesUncompressedInt = NULL;
      }
    }
  }
}

void SampledField::uncompressInternal(string& sampleString, size_t& length) const
{
  freeUncompressed();
  store();

  if (mCompression == SPATIAL_COMPRESSIONKIND_DEFLATED)
  {
    if (mSamplesCompressed == NULL)
    {
      sampleString = "";
      length = 0;
      return;
    }
    char* csamples = (char*)malloc(sizeof(char) * mSamplesCompressedLength);
    int* result;
    for (unsigned int i = 0; i < mSamplesCompressedLength; ++i)
    {
      csamples[i] = (char)mSamplesCompressed[i];
    }
    uncompress_data(csamples, mSamplesCompressedLength, result, length);
    free(csamples);

    if (result == NULL)
    {
      sampleString = "";
      length = 0;
      return;
    }

    sampleString = charIntsToString(result, length);
    free(result);
    return;
  }
  else
  {
    sampleString = mSamples;
    length = mSamplesUncompressedLength;
  }
}

/**
*  Returns the data of this image as uncompressed array of integers
*
* @param data the output array of integers (it will be allocated using
*             malloc and will have to be freed using free)
* @param length the output length of the array
*
*/
void
SampledField::getUncompressedData(double*& data, size_t& length)
{
  store();
  length = getUncompressedLength();
  if (length == 0)
  {
    return;
  }
  copySampleArrays(data, length, mSamplesUncompressed, mSamplesUncompressedLength);
  return;
}

int
SampledField::uncompress()
{
  if (mCompression == SPATIAL_COMPRESSIONKIND_DEFLATED)
  {
    uncompressInternal(mSamples, mSamplesUncompressedLength);
    mCompression = SPATIAL_COMPRESSIONKIND_UNCOMPRESSED;
    store();
    setSamplesLength(mSamplesUncompressedLength);
  }

  return setCompression(SPATIAL_COMPRESSIONKIND_UNCOMPRESSED);
}

int SampledField::compress(int level)
{
  freeCompressed();
  unsigned char* result; int length;
  int ret = compress_data(const_cast<char*>(mSamples.c_str()), mSamples.length(), level, result, length);

  if (ret == LIBSBML_OPERATION_SUCCESS)
  {
      mSamples = arrayToString(result, length);
      copySampleArrays(mSamplesCompressed, mSamplesCompressedLength, result, length);

      free(result);

      setSamplesLength(mSamplesCompressedLength);
      return setCompression(SPATIAL_COMPRESSIONKIND_DEFLATED);
  }
  return ret;
}

unsigned int
SampledField::getUncompressedLength() const
{
  store();
  if (mSamplesUncompressed == NULL) {
    string uncompressedString;
    uncompressInternal(uncompressedString, mSamplesUncompressedLength);
    mSamplesUncompressed = readSamplesFromString<double>(uncompressedString, mSamplesUncompressedLength);
  }
  return mSamplesUncompressedLength;
}

void
SampledField::getUncompressed(double* outputPoints) const
{
  store();
  if (outputPoints == NULL) return;
  if (mSamplesUncompressed == NULL) {
    string uncompressedString;
    uncompressInternal(uncompressedString, mSamplesUncompressedLength);
    mSamplesUncompressed = readSamplesFromString<double>(uncompressedString, mSamplesUncompressedLength);
  }
  if (mSamplesUncompressed == NULL) return;
  memcpy(outputPoints, mSamplesUncompressed, sizeof(double) * mSamplesUncompressedLength);
}

void
SampledField::freeUncompressed() const
{
  free(mSamplesUncompressedInt);
  mSamplesUncompressedInt = NULL;
  if (mSamplesUncompressed != NULL)
  {
    free(mSamplesUncompressed);
  }
  mSamplesUncompressed = NULL;
  mSamplesUncompressedLength = 0;
}

void
SampledField::freeCompressed() const
{
  if (mSamplesCompressed != NULL)
  {
    free(mSamplesCompressed);
  }
  mSamplesCompressed = NULL;
  mSamplesCompressedLength = 0;
}



#endif /* __cplusplus */


/*
 * Creates a new SampledField_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
SampledField_t*
SampledField_create(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion)
{
  return new SampledField(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this SampledField_t object.
 */
LIBSBML_EXTERN
SampledField_t*
SampledField_clone(const SampledField_t* sf)
{
  if (sf != NULL)
  {
    return static_cast<SampledField_t*>(sf->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this SampledField_t object.
 */
LIBSBML_EXTERN
void
SampledField_free(SampledField_t* sf)
{
  if (sf != NULL)
  {
    delete sf;
  }
}


/*
 * Returns the value of the "id" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
char*
SampledField_getId(const SampledField_t* sf)
{
  if (sf == NULL)
  {
    return NULL;
  }

  return sf->getId().empty() ? NULL : safe_strdup(sf->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
char*
SampledField_getName(const SampledField_t* sf)
{
  if (sf == NULL)
  {
    return NULL;
  }

  return sf->getName().empty() ? NULL : safe_strdup(sf->getName().c_str());
}


/*
 * Returns the value of the "dataType" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
DataKind_t
SampledField_getDataType(const SampledField_t* sf)
{
  if (sf == NULL)
  {
    return SPATIAL_DATAKIND_INVALID;
  }

  return sf->getDataType();
}


/*
 * Returns the value of the "dataType" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
char*
SampledField_getDataTypeAsString(const SampledField_t* sf)
{
  return (char*)(DataKind_toString(sf->getDataType()));
}


/*
 * Returns the value of the "numSamples1" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_getNumSamples1(const SampledField_t* sf)
{
  return (sf != NULL) ? sf->getNumSamples1() : SBML_INT_MAX;
}


/*
 * Returns the value of the "numSamples2" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_getNumSamples2(const SampledField_t* sf)
{
  return (sf != NULL) ? sf->getNumSamples2() : SBML_INT_MAX;
}


/*
 * Returns the value of the "numSamples3" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_getNumSamples3(const SampledField_t* sf)
{
  return (sf != NULL) ? sf->getNumSamples3() : SBML_INT_MAX;
}


/*
 * Returns the value of the "interpolationType" attribute of this
 * SampledField_t.
 */
LIBSBML_EXTERN
InterpolationKind_t
SampledField_getInterpolationType(const SampledField_t* sf)
{
  if (sf == NULL)
  {
    return SPATIAL_INTERPOLATIONKIND_INVALID;
  }

  return sf->getInterpolationType();
}


/*
 * Returns the value of the "interpolationType" attribute of this
 * SampledField_t.
 */
LIBSBML_EXTERN
char*
SampledField_getInterpolationTypeAsString(const SampledField_t* sf)
{
  return (char*)(InterpolationKind_toString(sf->getInterpolationType()));
}


/*
 * Returns the value of the "compression" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
CompressionKind_t
SampledField_getCompression(const SampledField_t* sf)
{
  if (sf == NULL)
  {
    return SPATIAL_COMPRESSIONKIND_INVALID;
  }

  return sf->getCompression();
}


/*
 * Returns the value of the "compression" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
char*
SampledField_getCompressionAsString(const SampledField_t* sf)
{
  return (char*)(CompressionKind_toString(sf->getCompression()));
}


/*
 * Returns the value of the "samplesLength" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_getSamplesLength(const SampledField_t* sf)
{
  return (sf != NULL) ? sf->getSamplesLength() : SBML_INT_MAX;
}


/*
 * Predicate returning @c 1 (true) if this SampledField_t's "id" attribute is
 * set.
 */
LIBSBML_EXTERN
int
SampledField_isSetId(const SampledField_t* sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledField_t's "name" attribute is
 * set.
 */
LIBSBML_EXTERN
int
SampledField_isSetName(const SampledField_t* sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledField_t's "dataType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SampledField_isSetDataType(const SampledField_t* sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetDataType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledField_t's "numSamples1"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SampledField_isSetNumSamples1(const SampledField_t* sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetNumSamples1()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledField_t's "numSamples2"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SampledField_isSetNumSamples2(const SampledField_t* sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetNumSamples2()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledField_t's "numSamples3"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SampledField_isSetNumSamples3(const SampledField_t* sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetNumSamples3()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledField_t's "interpolationType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SampledField_isSetInterpolationType(const SampledField_t* sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetInterpolationType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledField_t's "compression"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SampledField_isSetCompression(const SampledField_t* sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetCompression()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledField_t's "samples" attribute
 * is set.
 */
LIBSBML_EXTERN
int
SampledField_isSetSamples(const SampledField_t* sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetSamples()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledField_t's "samplesLength"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SampledField_isSetSamplesLength(const SampledField_t* sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetSamplesLength()) : 0;
}


/*
 * Sets the value of the "id" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setId(SampledField_t* sf, const char* id)
{
  return (sf != NULL) ? sf->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setName(SampledField_t* sf, const char* name)
{
  return (sf != NULL) ? sf->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "dataType" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setDataType(SampledField_t* sf, DataKind_t dataType)
{
  return (sf != NULL) ? sf->setDataType(dataType) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "dataType" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setDataTypeAsString(SampledField_t* sf, const char* dataType)
{
  return (sf != NULL) ? sf->setDataType(dataType) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "numSamples1" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setNumSamples1(SampledField_t* sf, int numSamples1)
{
  return (sf != NULL) ? sf->setNumSamples1(numSamples1) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "numSamples2" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setNumSamples2(SampledField_t* sf, int numSamples2)
{
  return (sf != NULL) ? sf->setNumSamples2(numSamples2) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "numSamples3" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setNumSamples3(SampledField_t* sf, int numSamples3)
{
  return (sf != NULL) ? sf->setNumSamples3(numSamples3) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "interpolationType" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setInterpolationType(SampledField_t* sf,
  InterpolationKind_t interpolationType)
{
  return (sf != NULL) ? sf->setInterpolationType(interpolationType) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "interpolationType" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setInterpolationTypeAsString(SampledField_t* sf,
  const char* interpolationType)
{
  return (sf != NULL) ? sf->setInterpolationType(interpolationType) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "compression" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setCompression(SampledField_t* sf,
  CompressionKind_t compression)
{
  return (sf != NULL) ? sf->setCompression(compression) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "compression" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setCompressionAsString(SampledField_t* sf,
  const char* compression)
{
  return (sf != NULL) ? sf->setCompression(compression) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "samples" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setSamples(SampledField_t* sf, int* samples, size_t arrayLength)
{
  return (sf != NULL) ? sf->setSamples(samples, arrayLength) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "samplesLength" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_setSamplesLength(SampledField_t* sf, int samplesLength)
{
  return (sf != NULL) ? sf->setSamplesLength(samplesLength) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_unsetId(SampledField_t* sf)
{
  return (sf != NULL) ? sf->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_unsetName(SampledField_t* sf)
{
  return (sf != NULL) ? sf->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "dataType" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_unsetDataType(SampledField_t* sf)
{
  return (sf != NULL) ? sf->unsetDataType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "numSamples1" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_unsetNumSamples1(SampledField_t* sf)
{
  return (sf != NULL) ? sf->unsetNumSamples1() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "numSamples2" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_unsetNumSamples2(SampledField_t* sf)
{
  return (sf != NULL) ? sf->unsetNumSamples2() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "numSamples3" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_unsetNumSamples3(SampledField_t* sf)
{
  return (sf != NULL) ? sf->unsetNumSamples3() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "interpolationType" attribute of this
 * SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_unsetInterpolationType(SampledField_t* sf)
{
  return (sf != NULL) ? sf->unsetInterpolationType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "compression" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_unsetCompression(SampledField_t* sf)
{
  return (sf != NULL) ? sf->unsetCompression() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "samples" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_unsetSamples(SampledField_t* sf)
{
  return (sf != NULL) ? sf->unsetSamples() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "samplesLength" attribute of this SampledField_t.
 */
LIBSBML_EXTERN
int
SampledField_unsetSamplesLength(SampledField_t* sf)
{
  return (sf != NULL) ? sf->unsetSamplesLength() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SampledField_t object have been set.
 */
LIBSBML_EXTERN
int
SampledField_hasRequiredAttributes(const SampledField_t* sf)
{
  return (sf != NULL) ? static_cast<int>(sf->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


