/**
 * @file SampledField.cpp
 * @brief Implementation of the SampledField class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


template<typename type> std::string vectorToString(const std::vector< type >& vec)
{
  std::stringstream str;

  typename std::vector< type >::const_iterator it = vec.begin();

  for (; it != vec.end(); ++it)
  {
    str << *it << " ";
  }

  return str.str();
}


std::string vectorToString(const std::vector<double>& vec)
{
  std::stringstream str;

  std::vector<double>::const_iterator it = vec.begin();

  for (; it != vec.end(); ++it)
  {
    str << setprecision(17) << *it << " ";
  }

  return str.str();
}

template<typename type> void readSamplesFromString(const std::string& str, std::vector<type>& valuesVector)
{
  valuesVector.clear();
  stringstream strStream(str);
  type val;

  while (strStream >> val)
  {
    valuesVector.push_back(val);
  }
}

template<typename type> type* readSamplesFromString(const std::string& str, unsigned int& length)
{
  stringstream strStream(str);
  type val;
  vector< type> valuesVector;

  while (strStream >> val)
  {
    valuesVector.push_back(val);
  }

  length = (unsigned int)valuesVector.size();

  if (length > 0)
  {
    type* data = new type[length];
    for (unsigned int i = 0; i < length; ++i)
    {
      data[i] = valuesVector.at(i);
    }
    return data;
  }

  return NULL;
}

template<typename type> std::string arrayToString(const type* array, unsigned int length)
{
  std::stringstream str;

  for (unsigned int i = 0; i < length; ++i)
  {
    str << (type)array[i] << " ";
  }

  return str.str();
}


std::string arrayToString(const unsigned char* array, unsigned int length)
{
  std::stringstream str;

  for (unsigned int i = 0; i < length; ++i)
  {
    str << (int)array[i] << " ";
  }

  return str.str();
}


std::string arrayToString(const double* array, unsigned int length)
{
  std::stringstream str;

  for (unsigned int i = 0; i < length; ++i)
  {
    str << std::setprecision(17) << (double)array[i] << " ";
  }

  return str.str();
}

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
  , mSamplesLength(0)
  , mIsSetSamplesLength(false)
  , mUncompressedSamples(NULL)
  , mUncompressedLength(0)
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
  , mSamplesLength(0)
  , mIsSetSamplesLength(false)
  , mUncompressedSamples(NULL)
  , mUncompressedLength(0)
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
  , mUncompressedSamples(NULL)
  , mUncompressedLength(0)
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
    mUncompressedSamples = NULL;
    mUncompressedLength = 0;
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
  std::string code_str = DataKind_toString(mDataType);
  return code_str;
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
  std::string code_str = InterpolationKind_toString(mInterpolationType);
  return code_str;
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


/*
 * Returns the value of the "samples" attribute of this SampledField.
 */
void
SampledField::getSamples(int* outArray) const
{
  if (outArray == NULL)
  {
    return;
  }

  unsigned int length;
  int* samples = readSamplesFromString<int>(mSamples, length);

  memcpy(outArray, samples, sizeof(int) * length);
}

void SampledField::getSamples(std::vector<int>& outVector) const
{
  readSamplesFromString<int>(mSamples, outVector);
}

void SampledField::getSamples(std::vector<float>& outVector) const
{
  readSamplesFromString<float>(mSamples, outVector);
}

void SampledField::getSamples(std::vector<double>& outVector) const
{
  readSamplesFromString<double>(mSamples, outVector);
}

const std::string& SampledField::getSamples() const
{
  return mSamples;
}

void
SampledField::getSamples(double* outArray) const
{
  if (outArray == NULL)
  {
    return;
  }

  unsigned int length;
  double* samples = readSamplesFromString<double>(mSamples, length);

  memcpy(outArray, samples, sizeof(double) * length);
}


void
SampledField::getSamples(float* outArray) const
{
  if (outArray == NULL)
  {
    return;
  }

  unsigned int length;
  float* samples = readSamplesFromString<float>(mSamples, length);

  memcpy(outArray, samples, sizeof(float) * length);
}
/*
 * Returns the value of the "samplesLength" attribute of this SampledField.
 */
int
SampledField::getSamplesLength() const
{
  return mSamplesLength;
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


/*
 * Sets the value of the "samples" attribute of this SampledField.
 */
int
SampledField::setSamples(int* inArray, int arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  mSamples = arrayToString(inArray, arrayLength);
  mIsSetSamplesLength = true;
  mSamplesLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}

int SampledField::setSamples(unsigned int* inArray, int arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  mSamples = arrayToString(inArray, arrayLength);
  mIsSetSamplesLength = true;
  mSamplesLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}

int SampledField::setSamples(unsigned char* inArray, int arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  mSamples = arrayToString(inArray, arrayLength);
  mIsSetSamplesLength = true;
  mSamplesLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}


int
SampledField::setSamples(double* inArray, int arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  mSamples = arrayToString(inArray, arrayLength);
  mIsSetSamplesLength = true;
  mSamplesLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}


int
SampledField::setSamples(float* inArray, int arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  mSamples = arrayToString(inArray, arrayLength);
  mIsSetSamplesLength = true;
  mSamplesLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}

int SampledField::setSamples(const std::string& samples)
{
  mSamples = samples;
  return LIBSBML_OPERATION_SUCCESS;
}

int SampledField::setSamples(const std::vector<double>& samples)
{
  mSamples = vectorToString(samples);
  mIsSetSamplesLength = true;
  mSamplesLength = samples.size();
  return LIBSBML_OPERATION_SUCCESS;
}

int SampledField::setSamples(const std::vector<int>& samples)
{
  mSamples = vectorToString(samples);
  mIsSetSamplesLength = true;
  mSamplesLength = samples.size();
  return LIBSBML_OPERATION_SUCCESS;
}

int SampledField::setSamples(const std::vector<float>& samples)
{
  mSamples = vectorToString(samples);
  mIsSetSamplesLength = true;
  mSamplesLength = samples.size();
  return LIBSBML_OPERATION_SUCCESS;
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
      logEmptyString(mId, level, version, "<SampledField>");
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
      "<SampledField> element.";
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
      logEmptyString(mName, level, version, "<SampledField>");
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
      logEmptyString(dataType, level, version, "<SampledField>");
    }
    else
    {
      mDataType = DataKind_fromString(dataType.c_str());

      if (DataKind_isValid(mDataType) == 0)
      {
        std::string msg = "The dataType on the <SampledField> ";

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
        "<SampledField> element must be an integer.";
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
        "<SampledField> element must be an integer.";
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
        "<SampledField> element must be an integer.";
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
      logEmptyString(interpolationType, level, version, "<SampledField>");
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
      logEmptyString(compression, level, version, "<SampledField>");
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
        "<SampledField> element must be an integer.";
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
  setSamples(text);
}

/** @endcond */


#include <sbml/compress/CompressCommon.h>

#ifdef USE_ZLIB
#include <zlib.h>
#endif

/**
 *  Returns the data of this image as uncompressed array of integers
 *
 * @param data the output array of integers (it will be allocated using
 *             malloc and will have to be freed using free)
 * @param length the output length of the array
 *
 */
void
SampledField::getUncompressedData(int*& data, int& length)
{
  if (mUncompressedSamples == NULL)
  {
    uncompressLegacy();
  }

  copySampleArrays(data, length, mUncompressedSamples, mUncompressedLength);
  return;

}

void
SampledField::uncompressLegacy()
{
  freeUncompressed();

  unsigned int length;
  int* samples = readSamplesFromString<int>(mSamples, length);
  if (samples == NULL) return;
  if (mCompression == SPATIAL_COMPRESSIONKIND_DEFLATED)
  {
    char* csamples = (char*)malloc(sizeof(char) * length);
    for (int i = 0; i < mSamplesLength; ++i)
      csamples[i] = (char)samples[i];
    SampledField::uncompress_data(csamples, length, mUncompressedSamples, mUncompressedLength);
    free(csamples);

    if (mUncompressedSamples == 0)
      copySampleArrays(mUncompressedSamples, mUncompressedLength, samples, length);
  }
  else
  {
    copySampleArrays(mUncompressedSamples, mUncompressedLength, samples, mSamplesLength);
  }

  delete[] samples;

}

int
SampledField::uncompress()
{
  if (mCompression != SPATIAL_COMPRESSIONKIND_DEFLATED)
    return LIBSBML_OPERATION_SUCCESS;

  unsigned int length;
  int* samples = readSamplesFromString<int>(mSamples, length);
  if (samples == NULL)
    return LIBSBML_OPERATION_SUCCESS;

  char* csamples = (char*)malloc(sizeof(char) * length);
  for (int i = 0; i < mSamplesLength; ++i)
    csamples[i] = (char)samples[i];

  mSamples = SampledField::uncompress_data(csamples, length);
  mCompression = SPATIAL_COMPRESSIONKIND_UNCOMPRESSED;
  free(csamples);
  delete[] samples;

  return LIBSBML_OPERATION_SUCCESS;

}

int SampledField::compress(int level)
{
  unsigned char* result; int length;
  compress_data(const_cast<char*>(mSamples.c_str()), mSamples.length(), level, result, length);

  setSamples(result, length);
  free(result);

  mCompression = SPATIAL_COMPRESSIONKIND_DEFLATED;
  return 0;
}

unsigned int
SampledField::getUncompressedLength()
{
  if (mUncompressedSamples == NULL)
    uncompressLegacy();
  return mUncompressedLength;
}

void
SampledField::getUncompressed(int* outputSamples)
{
  if (outputSamples == NULL) return;
  if (mUncompressedSamples == NULL)
    uncompressLegacy();
  if (mUncompressedSamples == NULL)
    return;
  memcpy(outputSamples, mUncompressedSamples, sizeof(int) * mUncompressedLength);
}

void
SampledField::freeUncompressed()
{
  if (mUncompressedSamples == NULL) return;
  mUncompressedLength = 0;
  free(mUncompressedSamples);
  mUncompressedSamples = NULL;
}


std::string
SampledField::uncompress_data(void* data, size_t length)
{
  std::stringstream str;
#ifndef USE_ZLIB
  // throwing an exception won't help our users, better set the result array and length to NULL. 
  // throw ZlibNotLinked();
  return "";
#else
  const size_t BUFSIZE = 128 * 1024;
  Bytef temp_buffer[BUFSIZE];

  z_stream strm;
  strm.zalloc = 0;
  strm.zfree = 0;
  strm.next_in = reinterpret_cast<Bytef*>(data);
  strm.avail_in = length;
  strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
  strm.avail_out = BUFSIZE;

  int res = inflateInit(&strm);

  while (strm.avail_in != 0)
  {
    res = inflate(&strm, Z_NO_FLUSH);
    if (res < 0)
    {
      return "";
    }
    if (strm.avail_out == 0)
    {
      str << std::string(temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
  }

  res = Z_OK;
  while (res == Z_OK)
  {
    if (strm.avail_out == 0)
    {
      str << std::string(temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
    res = inflate(&strm, Z_FINISH);
    if (res < 0)
    {
      return "";
    }
  }

  str << std::string(temp_buffer, temp_buffer + BUFSIZE - strm.avail_out);
  inflateEnd(&strm);

  return str.str();
#endif
}

void
SampledField::copySampleArrays(int*& target, int& targetLength, int* source, int sourceLength)
{
  targetLength = sourceLength;
  target = (int*)malloc(sizeof(int) * sourceLength);
  memset(target, 0, sizeof(int) * sourceLength);
  memcpy(target, source, sizeof(int) * sourceLength);
}

void SampledField::uncompress_data(void* data, size_t length, int*& result, int& outLength)
{
#ifndef USE_ZLIB
  // throwing an exception won't help our users, better set the result array and length to NULL. 
  // throw ZlibNotLinked();
  outLength = 0;
  result = NULL;
#else
  std::vector<char> buffer;

  const size_t BUFSIZE = 128 * 1024;
  Bytef temp_buffer[BUFSIZE];

  z_stream strm;
  strm.zalloc = 0;
  strm.zfree = 0;
  strm.next_in = reinterpret_cast<Bytef*>(data);
  strm.avail_in = length;
  strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
  strm.avail_out = BUFSIZE;

  int res = inflateInit(&strm);

  while (strm.avail_in != 0)
  {
    res = inflate(&strm, Z_NO_FLUSH);
    if (res < 0)
    {
      outLength = 0;
      result = NULL;
      break;
    }
    if (strm.avail_out == 0)
    {
      buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
  }

  res = Z_OK;
  while (res == Z_OK)
  {
    if (strm.avail_out == 0)
    {
      buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
    res = inflate(&strm, Z_FINISH);
    if (res < 0)
    {
      outLength = 0;
      result = NULL;
    }
  }

  buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE - strm.avail_out);
  inflateEnd(&strm);

  outLength = buffer.size();
  result = (int*)malloc(sizeof(int) * outLength);
  if (result == NULL)
    return;
  for (int i = 0; i < outLength; i++)
    result[i] = buffer[i];
#endif
}


void SampledField::compress_data(void* data, size_t length, int level, unsigned char*& result, int& outLength)
{
#ifndef USE_ZLIB
  // throwing an exception won't help our users, better set the result array and length to NULL. 
  // throw ZlibNotLinked();
  outLength = 0;
  result = NULL;
#else
  std::vector<char> buffer;

  const size_t BUFSIZE = 128 * 1024;
  Bytef temp_buffer[BUFSIZE];

  z_stream strm;
  strm.zalloc = 0;
  strm.zfree = 0;
  strm.next_in = reinterpret_cast<Bytef*>(data);
  strm.avail_in = length;
  strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
  strm.avail_out = BUFSIZE;

  int res = deflateInit(&strm, level);

  while (strm.avail_in != 0)
  {
    res = deflate(&strm, Z_NO_FLUSH);
    if (res < 0)
    {
      outLength = 0;
      result = NULL;
      break;
    }
    if (strm.avail_out == 0)
    {
      buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
  }

  res = Z_OK;
  while (res == Z_OK)
  {
    if (strm.avail_out == 0)
    {
      buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE);
      strm.next_out = reinterpret_cast<Bytef*>(temp_buffer);
      strm.avail_out = BUFSIZE;
    }
    res = deflate(&strm, Z_FINISH);
    if (res < 0)
    {
      outLength = 0;
      result = NULL;
    }
  }

  buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE - strm.avail_out);
  deflateEnd(&strm);

  outLength = buffer.size();
  result = (unsigned char*)malloc(sizeof(int) * outLength);
  if (result == NULL)
    return;
  for (int i = 0; i < outLength; i++)
    result[i] = (unsigned char)buffer[i];
#endif
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
SampledField_setSamples(SampledField_t* sf, int* samples, int arrayLength)
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


