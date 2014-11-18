/**
 * @file:   SampledField.cpp
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


#include <sbml/packages/spatial/sbml/SampledField.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SampledField with the given level, version, and package version.
 */
SampledField::SampledField (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mDataType (DATAKIND_UNKNOWN)
  , mNumSamples1 (SBML_INT_MAX)
  , mIsSetNumSamples1 (false)
  , mNumSamples2 (SBML_INT_MAX)
  , mIsSetNumSamples2 (false)
  , mNumSamples3 (SBML_INT_MAX)
  , mIsSetNumSamples3 (false)
  , mInterpolationType (INTERPOLATIONKIND_UNKNOWN)
  , mCompression (COMPRESSIONKIND_UNKNOWN)
  , mSamples (NULL)
  , mSamplesLength (SBML_INT_MAX)
  , mIsSetSamplesLength (false)
, mUncompressedSamples(NULL)
, mUncompressedLength(0)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new SampledField with the given SpatialPkgNamespaces object.
 */
SampledField::SampledField (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mDataType (DATAKIND_UNKNOWN)
  , mNumSamples1 (SBML_INT_MAX)
  , mIsSetNumSamples1 (false)
  , mNumSamples2 (SBML_INT_MAX)
  , mIsSetNumSamples2 (false)
  , mNumSamples3 (SBML_INT_MAX)
  , mIsSetNumSamples3 (false)
  , mInterpolationType (INTERPOLATIONKIND_UNKNOWN)
  , mCompression (COMPRESSIONKIND_UNKNOWN)
  , mSamples (NULL)
  , mSamplesLength (SBML_INT_MAX)
  , mIsSetSamplesLength (false)
, mUncompressedSamples(NULL)
, mUncompressedLength(0)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for SampledField.
 */
SampledField::SampledField (const SampledField& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mDataType  = orig.mDataType;
    mNumSamples1  = orig.mNumSamples1;
    mIsSetNumSamples1  = orig.mIsSetNumSamples1;
    mNumSamples2  = orig.mNumSamples2;
    mIsSetNumSamples2  = orig.mIsSetNumSamples2;
    mNumSamples3  = orig.mNumSamples3;
    mIsSetNumSamples3  = orig.mIsSetNumSamples3;
    mInterpolationType  = orig.mInterpolationType;
    mCompression  = orig.mCompression;
    mSamples  = NULL;
    setSamples(orig.mSamples, orig.mSamplesLength);
    mSamplesLength  = orig.mSamplesLength;
    mIsSetSamplesLength  = orig.mIsSetSamplesLength;
    mUncompressedSamples = NULL;
    mUncompressedLength = 0;
    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for SampledField.
 */
SampledField&
SampledField::operator=(const SampledField& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mDataType  = rhs.mDataType;
    mNumSamples1  = rhs.mNumSamples1;
    mIsSetNumSamples1  = rhs.mIsSetNumSamples1;
    mNumSamples2  = rhs.mNumSamples2;
    mIsSetNumSamples2  = rhs.mIsSetNumSamples2;
    mNumSamples3  = rhs.mNumSamples3;
    mIsSetNumSamples3  = rhs.mIsSetNumSamples3;
    mInterpolationType  = rhs.mInterpolationType;
    mCompression  = rhs.mCompression;
    mSamples  = NULL;
    setSamples(rhs.mSamples, rhs.mSamplesLength);
    mSamplesLength  = rhs.mSamplesLength;
    mIsSetSamplesLength  = rhs.mIsSetSamplesLength;
    mUncompressedSamples = NULL;
    mUncompressedLength = 0;
    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for SampledField.
 */
SampledField*
SampledField::clone () const
{
  return new SampledField(*this);
}


/*
 * Destructor for SampledField.
 */
SampledField::~SampledField ()
{
  if (mSamples != NULL)
    delete[] mSamples;
  mSamples = NULL;
  
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
 * Returns the value of the "dataType" attribute of this SampledField.
 */
DataKind_t
SampledField::getDataType() const
{
  return mDataType;
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
 * Returns the value of the "compression" attribute of this SampledField.
 */
CompressionKind_t
SampledField::getCompression() const
{
  return mCompression;
}


/*
 * The "samples" attribute of this SampledField is returned in an int* array (pointer)
 * that is passed as argument to the method (this is needed while using SWIG to
 * convert int[] from C++ to Java). The method itself has a return type void.
 *
 * NOTE: you have to pre-allocate the array with the correct length! *
 * @return void.
 */
void
SampledField::getSamples(int* outArray) const
{
   if (outArray == NULL || mSamples == NULL) return;

   memcpy(outArray , mSamples, sizeof(int)*mSamplesLength);
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
 * Returns true/false if id is set.
 */
bool
SampledField::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if dataType is set.
 */
bool
SampledField::isSetDataType() const
{
  return mDataType != DATAKIND_UNKNOWN;
}


/*
 * Returns true/false if numSamples1 is set.
 */
bool
SampledField::isSetNumSamples1() const
{
  return mIsSetNumSamples1;
}


/*
 * Returns true/false if numSamples2 is set.
 */
bool
SampledField::isSetNumSamples2() const
{
  return mIsSetNumSamples2;
}


/*
 * Returns true/false if numSamples3 is set.
 */
bool
SampledField::isSetNumSamples3() const
{
  return mIsSetNumSamples3;
}


/*
 * Returns true/false if interpolationType is set.
 */
bool
SampledField::isSetInterpolationType() const
{
  return mInterpolationType != INTERPOLATIONKIND_UNKNOWN;
}


/*
 * Returns true/false if compression is set.
 */
bool
SampledField::isSetCompression() const
{
  return mCompression != COMPRESSIONKIND_UNKNOWN;
}


/*
 * Returns true/false if samples is set.
 */
bool
SampledField::isSetSamples() const
{
  return (mSamples != NULL);
}


/*
 * Returns true/false if samplesLength is set.
 */
bool
SampledField::isSetSamplesLength() const
{
  return mIsSetSamplesLength;
}


/*
 * Sets id and returns value indicating success.
 */
int
SampledField::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets dataType and returns value indicating success.
 */
int
SampledField::setDataType(DataKind_t dataType)
{
  mDataType = dataType;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets dataType and returns value indicating success.
 */
int
SampledField::setDataType(const std::string& dataType)
{
  DataKind_t parsed = DataKind_parse(dataType.c_str());
  if (parsed == DATAKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mDataType = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets numSamples1 and returns value indicating success.
 */
int
SampledField::setNumSamples1(int numSamples1)
{
  mNumSamples1 = numSamples1;
  mIsSetNumSamples1 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets numSamples2 and returns value indicating success.
 */
int
SampledField::setNumSamples2(int numSamples2)
{
  mNumSamples2 = numSamples2;
  mIsSetNumSamples2 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets numSamples3 and returns value indicating success.
 */
int
SampledField::setNumSamples3(int numSamples3)
{
  mNumSamples3 = numSamples3;
  mIsSetNumSamples3 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets interpolationType and returns value indicating success.
 */
int
SampledField::setInterpolationType(InterpolationKind_t interpolationType)
{
  mInterpolationType = interpolationType;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets interpolationType and returns value indicating success.
 */
int
SampledField::setInterpolationType(const std::string& interpolationType)
{
  InterpolationKind_t parsed = InterpolationKind_parse(interpolationType.c_str());
  if (parsed == INTERPOLATIONKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mInterpolationType = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets compression and returns value indicating success.
 */
int
SampledField::setCompression(CompressionKind_t compression)
{
  mCompression = compression;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets compression and returns value indicating success.
 */
int
SampledField::setCompression(const std::string& compression)
{
  CompressionKind_t parsed = CompressionKind_parse(compression.c_str());
  if (parsed == COMPRESSIONKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mCompression = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
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
int
SampledField::setSamples(int* inArray, size_t arrayLength)
{
  if (inArray == NULL) return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  if (mSamples != NULL) delete[] mSamples;
  mSamples = new int[arrayLength];
  memcpy(mSamples, inArray, sizeof(int)*arrayLength);
  mIsSetSamplesLength = true;
  mSamplesLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}
/*
 * Sets samplesLength and returns value indicating success.
 */
int
SampledField::setSamplesLength(int samplesLength)
{
  mSamplesLength = samplesLength;
  mIsSetSamplesLength = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
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
 * Unsets dataType and returns value indicating success.
 */
int
SampledField::unsetDataType()
{
  mDataType = DATAKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets numSamples1 and returns value indicating success.
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
 * Unsets numSamples2 and returns value indicating success.
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
 * Unsets numSamples3 and returns value indicating success.
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
 * Unsets interpolationType and returns value indicating success.
 */
int
SampledField::unsetInterpolationType()
{
  mInterpolationType = INTERPOLATIONKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets compression and returns value indicating success.
 */
int
SampledField::unsetCompression()
{
  mCompression = COMPRESSIONKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets samples and returns value indicating success.
 */
int
SampledField::unsetSamples()
{
  if (mSamples != NULL)
   delete[] mSamples;
  mSamples = NULL;
  return unsetSamplesLength();
}


/*
 * Unsets samplesLength and returns value indicating success.
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


List*
SampledField::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
SampledField::getElementName () const
{
  static const string name = "sampledField";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SampledField::getTypeCode () const
{
  return SBML_SPATIAL_SAMPLEDFIELD;
}


/*
 * check if all the required attributes are set
 */
bool
SampledField::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetDataType() == false)
    allPresent = false;

  if (isSetNumSamples1() == false)
    allPresent = false;

  if (isSetCompression() == false)
    allPresent = false;

  if (isSetSamples() == false)
    allPresent = false;

  if (isSetSamplesLength() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
SampledField::hasRequiredElements () const
{
  bool allPresent = true;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
SampledField::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
SampledField::accept (SBMLVisitor& v) const
{
  v.visit(*this);

/* VISIT CHILDREN */

  v.leave(*this);

  return true;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
SampledField::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
SampledField::connectToChild()
{
  SBase::connectToChild();

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SampledField::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
SampledField::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  //const string& name = stream.peek().getName();

  connectToChild();


  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
SampledField::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("dataType");
  attributes.add("numSamples1");
  attributes.add("numSamples2");
  attributes.add("numSamples3");
  attributes.add("interpolationType");
  attributes.add("compression");
  attributes.add("samplesLength");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SampledField::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfSampledFields - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfSampledFields*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // id SId  ( use = "required" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<SampledField>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'sampledField' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // dataType enum  ( use = "required" )
  //
  mDataType = DATAKIND_UNKNOWN;
  std::string stringValue;
  assigned = attributes.readInto("dataType", stringValue);

  if (assigned == true)
  {
    // parse enum

    mDataType = DataKind_parse(stringValue.c_str());
    if(mDataType == DATAKIND_UNKNOWN)
    {
      std::string message = "Unknown value for spatial attribute 'dataType' in 'sampledField' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  else if(mDataType == DATAKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'dataType' is missing from 'sampledField' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // numSamples1 int   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetNumSamples1 = attributes.readInto("numSamples1", mNumSamples1);

  if (mIsSetNumSamples1 == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
      else
      {
        std::string message = "Spatial attribute 'numSamples1' is missing from 'sampledField' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
      }
    }
  }

  //
  // numSamples2 int   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetNumSamples2 = attributes.readInto("numSamples2", mNumSamples2);

  if (mIsSetNumSamples2 == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
    }
  }

  //
  // numSamples3 int   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetNumSamples3 = attributes.readInto("numSamples3", mNumSamples3);

  if (mIsSetNumSamples3 == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
    }
  }

  //
  // interpolationType enum  ( use = "optional" )
  //
  mInterpolationType = INTERPOLATIONKIND_UNKNOWN;
  assigned = attributes.readInto("interpolationType", stringValue);

  if (assigned == true)
  {
    // parse enum

    mInterpolationType = InterpolationKind_parse(stringValue.c_str());
    if(mInterpolationType == INTERPOLATIONKIND_UNKNOWN) {
      std::string message = "Unknown value for spatial attribute 'interpoloationType' in 'sampledField' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  //
  // compression enum  ( use = "required" )
  //
  mCompression = COMPRESSIONKIND_UNKNOWN;
  assigned = attributes.readInto("compression", stringValue);

  if (assigned == true)
  {
    // parse enum

    mCompression = CompressionKind_parse(stringValue.c_str());
    if(mCompression == COMPRESSIONKIND_UNKNOWN) {
      std::string message = "Unknown value for spatial attribute 'compression' in 'sampledField' object: " + stringValue;
      getErrorLog()->logPackageError("spatial", SpatialUnknownError,
        getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
    }
  }
  if(mCompression == COMPRESSIONKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'compression' is missing from 'sampledField' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // samplesLength int   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetSamplesLength = attributes.readInto("samplesLength", mSamplesLength);

  if (mIsSetSamplesLength == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
      else
      {
        std::string message = "Spatial attribute 'samplesLength' is missing from 'sampledField' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
      }
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
SampledField::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetDataType() == true)
    stream.writeAttribute("dataType", getPrefix(), DataKind_toString(mDataType));

  if (isSetNumSamples1() == true)
    stream.writeAttribute("numSamples1", getPrefix(), mNumSamples1);

  if (isSetNumSamples2() == true)
    stream.writeAttribute("numSamples2", getPrefix(), mNumSamples2);

  if (isSetNumSamples3() == true)
    stream.writeAttribute("numSamples3", getPrefix(), mNumSamples3);

  if (isSetInterpolationType() == true)
    stream.writeAttribute("interpolationType", getPrefix(), InterpolationKind_toString(mInterpolationType));

  if (isSetCompression() == true)
    stream.writeAttribute("compression", getPrefix(), CompressionKind_toString(mCompression));

  if (isSetSamplesLength() == true)
    stream.writeAttribute("samplesLength", getPrefix(), mSamplesLength);

}


  /** @endcond doxygenLibsbmlInternal */


void
SampledField::write(XMLOutputStream& stream) const
{
  stream.startElement(getElementName(), getPrefix());
  writeAttributes(stream);
  if(isSetSamples())
  {
    for (int i = 0; i < mSamplesLength; ++i)
    {
      stream << (long)mSamples[i] << " ";
    }
  }
  stream.endElement(getElementName(), getPrefix());
}


void
SampledField::setElementText(const std::string &text)
{
  stringstream strStream(text); // Insert the string into a stream
  int val;
  vector<int> valuesVector;
  while (strStream >> val)
  {
    valuesVector.push_back(val);
  }

  // convert the vector to an array
  size_t length = valuesVector.size();
  if (length > 0)
  {
    setSamples(&valuesVector[0], length);
  }
}
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
SampledField::getUncompressedData(int* &data, int& length) 
{
  if (mUncompressedSamples == NULL)
  {
    uncompress();
  }

  copySampleArrays(data, length, mUncompressedSamples, mUncompressedLength);
  return;

}

void 
SampledField::uncompress()
{
  freeUncompressed();
  if (mCompression == SPATIAL_COMPRESSIONKIND_DEFLATED)
  {
    char* csamples = (char*)malloc(sizeof(char)*mSamplesLength);
    for (int i = 0 ; i < mSamplesLength; ++i)
      csamples[i] = mSamples[i];
    SampledField::uncompress_data(csamples, mSamplesLength, mUncompressedSamples, mUncompressedLength);
    free(csamples);

    if (mUncompressedSamples == 0)
      copySampleArrays(mUncompressedSamples, mUncompressedLength, mSamples, mSamplesLength);
  }
  else
  {
    copySampleArrays(mUncompressedSamples, mUncompressedLength, mSamples, mSamplesLength);
  }

}

unsigned int 
SampledField::getUncompressedLength()
{
  if (mUncompressedSamples == NULL)
    uncompress();
  return mUncompressedLength;
}

void 
SampledField::getUncompressed(int* outputSamples)
{
  if (outputSamples == NULL) return;
  if (mUncompressedSamples == NULL)
    uncompress();
  memcpy(outputSamples , mUncompressedSamples, sizeof(int)*mUncompressedLength);   
}

void 
SampledField::freeUncompressed()
{
  if (mUncompressedSamples == NULL) return;
  mUncompressedLength = 0;
  free(mUncompressedSamples);
  mUncompressedSamples = NULL;
}


void 
SampledField::uncompress_data(void *data, size_t length, int*& result, int& outLength)
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
 strm.next_in = reinterpret_cast<Bytef *>(data);
 strm.avail_in = length;
 strm.next_out = reinterpret_cast<Bytef *>(temp_buffer);
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
   strm.next_out = reinterpret_cast<Bytef *>(temp_buffer);
   strm.avail_out = BUFSIZE;
  }
 }

 res = Z_OK;
 while (res == Z_OK)
 {
  if (strm.avail_out == 0)
  {
   buffer.insert(buffer.end(), temp_buffer, temp_buffer + BUFSIZE);
   strm.next_out = reinterpret_cast<Bytef *>(temp_buffer);
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
 result = (int*) malloc(sizeof(int)*outLength);
 for (int i = 0; i < outLength; i++)
   result[i] = buffer[i];
#endif
}

void 
SampledField::copySampleArrays(int* &target, int& targetLength, int* source, int sourceLength)
{
    targetLength = sourceLength;
    target = (int*)malloc(sizeof(int)*sourceLength);
    memset(target, 0, sizeof(int)*sourceLength);
    memcpy(target, source, sizeof(int)*sourceLength);
}

/*
 * Constructor 
 */
ListOfSampledFields::ListOfSampledFields(unsigned int level, 
                      unsigned int version, 
                      unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfSampledFields::ListOfSampledFields(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfSampledFields 
 */
ListOfSampledFields* 
ListOfSampledFields::clone () const
 {
  return new ListOfSampledFields(*this);
}


/*
 * Get a SampledField from the ListOfSampledFields by index.
*/
SampledField*
ListOfSampledFields::get(unsigned int n)
{
  return static_cast<SampledField*>(ListOf::get(n));
}


/*
 * Get a SampledField from the ListOfSampledFields by index.
 */
const SampledField*
ListOfSampledFields::get(unsigned int n) const
{
  return static_cast<const SampledField*>(ListOf::get(n));
}


/*
 * Get a SampledField from the ListOfSampledFields by id.
 */
SampledField*
ListOfSampledFields::get(const std::string& sid)
{
	return const_cast<SampledField*>(
    static_cast<const ListOfSampledFields&>(*this).get(sid));
}


/*
 * Get a SampledField from the ListOfSampledFields by id.
 */
const SampledField*
ListOfSampledFields::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SampledField>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SampledField*> (*result);
}


/**
 * Adds a copy the given "SampledField" to this ListOfSampledFields.
 *
 * @param sf; the SampledField object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfSampledFields::addSampledField(const SampledField* sf)
{
  if (sf == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (sf->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sf->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sf->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(sf)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(sf);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of SampledField objects in this ListOfSampledFields.
 *
 * @return the number of SampledField objects in this ListOfSampledFields
 */
unsigned int 
ListOfSampledFields::getNumSampledFields() const
{
	return size();
}

/**
 * Creates a new SampledField object, adds it to this ListOfSampledFields
 * SampledField and returns the SampledField object created. 
 *
 * @return a new SampledField object instance
 *
 * @see addSampledField(const SampledField* sf)
 */
SampledField* 
ListOfSampledFields::createSampledField()
{
  SampledField* sf = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    sf = new SampledField(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(sf != NULL)
  {
    appendAndOwn(sf);
  }

  return sf;
}

/*
 * Removes the nth SampledField from this ListOfSampledFields
 */
SampledField*
ListOfSampledFields::remove(unsigned int n)
{
  return static_cast<SampledField*>(ListOf::remove(n));
}


/*
 * Removes the SampledField from this ListOfSampledFields with the given identifier
 */
SampledField*
ListOfSampledFields::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SampledField>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <SampledField*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfSampledFields::getElementName () const
{
  static const string name = "listOfSampledFields";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfSampledFields::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfSampledFields::getItemTypeCode () const
{
  return SBML_SPATIAL_SAMPLEDFIELD;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SampledField in this ListOfSampledFields
 */
SBase*
ListOfSampledFields::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "sampledField")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new SampledField(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Spatial package.
 */
void
ListOfSampledFields::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(SpatialExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(SpatialExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
SampledField_t *
SampledField_create(unsigned int level, unsigned int version,
                    unsigned int pkgVersion)
{
  return new SampledField(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SampledField_free(SampledField_t * sf)
{
  if (sf != NULL)
    delete sf;
}


LIBSBML_EXTERN
SampledField_t *
SampledField_clone(SampledField_t * sf)
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


LIBSBML_EXTERN
const char *
SampledField_getId(const SampledField_t * sf)
{
	return (sf != NULL && sf->isSetId()) ? sf->getId().c_str() : NULL;
}


LIBSBML_EXTERN
DataKind_t
SampledField_getDataType(const SampledField_t * sf)
{
	return (sf != NULL) ? sf->getDataType() : DATAKIND_UNKNOWN;
}


LIBSBML_EXTERN
int
SampledField_getNumSamples1(const SampledField_t * sf)
{
	return (sf != NULL) ? sf->getNumSamples1() : SBML_INT_MAX;
}


LIBSBML_EXTERN
int
SampledField_getNumSamples2(const SampledField_t * sf)
{
	return (sf != NULL) ? sf->getNumSamples2() : SBML_INT_MAX;
}


LIBSBML_EXTERN
int
SampledField_getNumSamples3(const SampledField_t * sf)
{
	return (sf != NULL) ? sf->getNumSamples3() : SBML_INT_MAX;
}


LIBSBML_EXTERN
InterpolationKind_t
SampledField_getInterpolationType(const SampledField_t * sf)
{
	return (sf != NULL) ? sf->getInterpolationType() : INTERPOLATIONKIND_UNKNOWN;
}


LIBSBML_EXTERN
CompressionKind_t
SampledField_getCompression(const SampledField_t * sf)
{
	return (sf != NULL) ? sf->getCompression() : COMPRESSIONKIND_UNKNOWN;
}


LIBSBML_EXTERN
int
SampledField_getSamplesLength(const SampledField_t * sf)
{
	return (sf != NULL) ? sf->getSamplesLength() : SBML_INT_MAX;
}


LIBSBML_EXTERN
int
SampledField_isSetId(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetId()) : 0;
}


LIBSBML_EXTERN
int
SampledField_isSetDataType(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetDataType()) : 0;
}


LIBSBML_EXTERN
int
SampledField_isSetNumSamples1(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetNumSamples1()) : 0;
}


LIBSBML_EXTERN
int
SampledField_isSetNumSamples2(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetNumSamples2()) : 0;
}


LIBSBML_EXTERN
int
SampledField_isSetNumSamples3(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetNumSamples3()) : 0;
}


LIBSBML_EXTERN
int
SampledField_isSetInterpolationType(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetInterpolationType()) : 0;
}


LIBSBML_EXTERN
int
SampledField_isSetCompression(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetCompression()) : 0;
}


LIBSBML_EXTERN
int
SampledField_isSetSamples(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetSamples()) : 0;
}


LIBSBML_EXTERN
int
SampledField_isSetSamplesLength(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetSamplesLength()) : 0;
}


LIBSBML_EXTERN
int
SampledField_setId(SampledField_t * sf, const char * id)
{
  if (sf != NULL)
    return (id == NULL) ? sf->setId("") : sf->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_setDataType(SampledField_t * sf, DataKind_t dataType)
{
  if (sf != NULL)
    return sf->setDataType(dataType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_setNumSamples1(SampledField_t * sf, int numSamples1)
{
  if (sf != NULL)
    return sf->setNumSamples1(numSamples1);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_setNumSamples2(SampledField_t * sf, int numSamples2)
{
  if (sf != NULL)
    return sf->setNumSamples2(numSamples2);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_setNumSamples3(SampledField_t * sf, int numSamples3)
{
  if (sf != NULL)
    return sf->setNumSamples3(numSamples3);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_setInterpolationType(SampledField_t * sf, InterpolationKind_t interpolationType)
{
  if (sf != NULL)
    return sf->setInterpolationType(interpolationType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_setCompression(SampledField_t * sf, CompressionKind_t compression)
{
  if (sf != NULL)
    return sf->setCompression(compression);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_setSamplesLength(SampledField_t * sf, int samplesLength)
{
  if (sf != NULL)
    return sf->setSamplesLength(samplesLength);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_unsetId(SampledField_t * sf)
{
  return (sf != NULL) ? sf->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_unsetDataType(SampledField_t * sf)
{
  return (sf != NULL) ? sf->unsetDataType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_unsetNumSamples1(SampledField_t * sf)
{
  return (sf != NULL) ? sf->unsetNumSamples1() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_unsetNumSamples2(SampledField_t * sf)
{
  return (sf != NULL) ? sf->unsetNumSamples2() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_unsetNumSamples3(SampledField_t * sf)
{
  return (sf != NULL) ? sf->unsetNumSamples3() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_unsetInterpolationType(SampledField_t * sf)
{
  return (sf != NULL) ? sf->unsetInterpolationType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_unsetCompression(SampledField_t * sf)
{
  return (sf != NULL) ? sf->unsetCompression() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_unsetSamples(SampledField_t * sf)
{
  return (sf != NULL) ? sf->unsetSamples() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_unsetSamplesLength(SampledField_t * sf)
{
  return (sf != NULL) ? sf->unsetSamplesLength() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_hasRequiredAttributes(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
SampledField_hasRequiredElements(const SampledField_t * sf)
{
	return (sf != NULL) ? static_cast<int>(sf->hasRequiredElements()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
SampledField_t *
ListOfSampledFields_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSampledFields *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
SampledField_t *
ListOfSampledFields_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSampledFields *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


