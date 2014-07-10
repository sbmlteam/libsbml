/**
 * @file:   ImageData.cpp
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


#include <sbml/packages/spatial/sbml/ImageData.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new ImageData with the given level, version, and package version.
 */
ImageData::ImageData (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mSamples (NULL)
  , mSamplesLength (SBML_INT_MAX)
  , mIsSetSamplesLength (false)
  , mDataType ("")
, mUncompressedSamples(NULL)
, mUncompressedLength(0)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new ImageData with the given SpatialPkgNamespaces object.
 */
ImageData::ImageData (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mSamples (NULL)
  , mSamplesLength (SBML_INT_MAX)
  , mIsSetSamplesLength (false)
  , mDataType ("")
  , mUncompressedSamples(NULL)
  , mUncompressedLength(0)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for ImageData.
 */
ImageData::ImageData (const ImageData& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mSamples  = NULL;
    setSamples(orig.mSamples, orig.mSamplesLength);
    mSamplesLength  = orig.mSamplesLength;
    mIsSetSamplesLength  = orig.mIsSetSamplesLength;
    mDataType  = orig.mDataType;
    mUncompressedSamples = NULL;
    mUncompressedLength = 0;
  }
}


/*
 * Assignment for ImageData.
 */
ImageData&
ImageData::operator=(const ImageData& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mSamples  = NULL;
    setSamples(rhs.mSamples, rhs.mSamplesLength);
    mSamplesLength  = rhs.mSamplesLength;
    mIsSetSamplesLength  = rhs.mIsSetSamplesLength;
    mDataType  = rhs.mDataType;
    mUncompressedSamples = NULL;
    mUncompressedLength = 0;
  }
  return *this;
}


/*
 * Clone for ImageData.
 */
ImageData*
ImageData::clone () const
{
  return new ImageData(*this);
}


/*
 * Destructor for ImageData.
 */
ImageData::~ImageData ()
{
  if (mSamples != NULL)
    delete[] mSamples;
  mSamples = NULL;

  if (mUncompressedSamples != NULL)
    delete[] mUncompressedSamples;
  mUncompressedSamples = NULL;

}


/*
 * The "samples" attribute of this ImageData is returned in an int* array (pointer)
 * that is passed as argument to the method (this is needed while using SWIG to
 * convert int[] from C++ to Java). The method itself has a return type void.
 *
 * NOTE: you have to pre-allocate the array with the correct length! *
 * @return void.
 */
void
ImageData::getSamples(int* outArray) const
{
   if (outArray == NULL || mSamples == NULL) return;

   memcpy(outArray , mSamples, sizeof(int)*mSamplesLength);
}


/*
 * Returns the value of the "samplesLength" attribute of this ImageData.
 */
int
ImageData::getSamplesLength() const
{
  return mSamplesLength;
}


/*
 * Returns the value of the "dataType" attribute of this ImageData.
 */
const std::string&
ImageData::getDataType() const
{
  return mDataType;
}


/*
 * Returns true/false if samples is set.
 */
bool
ImageData::isSetSamples() const
{
  return (mSamples != NULL);
}


/*
 * Returns true/false if samplesLength is set.
 */
bool
ImageData::isSetSamplesLength() const
{
  return mIsSetSamplesLength;
}


/*
 * Returns true/false if dataType is set.
 */
bool
ImageData::isSetDataType() const
{
  return (mDataType.empty() == false);
}


/*
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
int
ImageData::setSamples(int* inArray, int arrayLength)
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
ImageData::setSamplesLength(int samplesLength)
{
  mSamplesLength = samplesLength;
  mIsSetSamplesLength = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets dataType and returns value indicating success.
 */
int
ImageData::setDataType(const std::string& dataType)
{
  if (&(dataType) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDataType = dataType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets samples and returns value indicating success.
 */
int
ImageData::unsetSamples()
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
ImageData::unsetSamplesLength()
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
 * Unsets dataType and returns value indicating success.
 */
int
ImageData::unsetDataType()
{
  mDataType.erase();

  if (mDataType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ImageData::getElementName () const
{
  static const string name = "imageData";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ImageData::getTypeCode () const
{
  return SBML_SPATIAL_IMAGEDATA;
}


/*
 * check if all the required attributes are set
 */
bool
ImageData::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetSamples() == false)
    allPresent = false;

  if (isSetSamplesLength() == false)
    allPresent = false;

  if (isSetDataType() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
ImageData::writeElements (XMLOutputStream& stream) const
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
ImageData::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
ImageData::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
ImageData::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
ImageData::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("samplesLength");
  attributes.add("dataType");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
ImageData::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

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
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
    }
  }

  bool assigned = false;

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
                     getPackageVersion(), sbmlLevel, sbmlVersion);
      }
      else
      {
        std::string message = "Spatial attribute 'samplesLength' is missing.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
      }
    }
  }

  //
  // dataType string   ( use = "required" )
  //
  assigned = attributes.readInto("dataType", mDataType);

  if (assigned == true)
  {
    // check string is not empty

    if (mDataType.empty() == true)
    {
      logEmptyString(mDataType, getLevel(), getVersion(), "<ImageData>");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'dataType' is missing.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message);
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
ImageData::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetSamplesLength() == true)
    stream.writeAttribute("samplesLength", getPrefix(), mSamplesLength);

  if (isSetDataType() == true)
    stream.writeAttribute("dataType", getPrefix(), mDataType);

}


  /** @endcond doxygenLibsbmlInternal */


void
ImageData::write(XMLOutputStream& stream) const
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
ImageData::setElementText(const std::string &text)
{
  stringstream strStream(text); // Insert the string into a stream
  int val;
  vector<int> valuesVector;
  while (strStream >> val)
  {
    valuesVector.push_back(val);
  }

  // convert the vector to an array
  unsigned int length = (unsigned int)valuesVector.size();
  if (length > 0)
  {

    int* data = new int[length];
    for (unsigned int i = 0; i < length; ++i)
    {
      data[i] = valuesVector.at(i);
    }

    setSamples(data, length);
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
 * @param length the output lenght of the array
 *
 */
void 
ImageData::getUncompressedData(int* &data, int& length) 
{
  if (mUncompressedSamples == NULL)
  {
    uncompress();
  }

  copySampleArrays(data, length, mUncompressedSamples, mUncompressedLength);
  return;

}

void 
ImageData::uncompress()
{
  freeUncompressed();
  if (mDataType.empty() || mDataType == "compressed")
  {
    char* csamples = (char*)malloc(sizeof(char)*mSamplesLength);
    for (unsigned int i = 0 ; i < mSamplesLength; ++i)
      csamples[i] = mSamples[i];
    ImageData::uncompress_data(csamples, mSamplesLength, mUncompressedSamples, mUncompressedLength);
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
ImageData::getUncompressedLength()
{
  if (mUncompressedSamples == NULL)
    uncompress();
  return mUncompressedLength;
}

void 
ImageData::getUncompressed(int* outputSamples)
{
  if (outputSamples == NULL) return;
  if (mUncompressedSamples == NULL)
    uncompress();
  memcpy(outputSamples , mUncompressedSamples, sizeof(int)*mUncompressedLength);   
}

void 
ImageData::freeUncompressed()
{
  if (mUncompressedSamples == NULL) return;
  mUncompressedLength = 0;
  free(mUncompressedSamples);
  mUncompressedSamples = NULL;
}


void 
ImageData::uncompress_data(void *data, size_t length, int*& result, int& outLength)
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
 deflateEnd(&strm);

 outLength = buffer.size();
 result = (int*) malloc(sizeof(int)*outLength);
 for (int i = 0; i < outLength; i++)
   result[i] = buffer[i];
#endif
}

void 
ImageData::copySampleArrays(int* &target, int& targetLength, int* source, int sourceLength)
{
    targetLength = sourceLength;
    target = (int*)malloc(sizeof(int)*sourceLength);
    memset(target, 0, sizeof(int)*sourceLength);
    memcpy(target, source, sizeof(int)*sourceLength);
}

LIBSBML_EXTERN
ImageData_t *
ImageData_create(unsigned int level, unsigned int version,
                 unsigned int pkgVersion)
{
  return new ImageData(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
ImageData_free(ImageData_t * id)
{
  if (id != NULL)
    delete id;
}


LIBSBML_EXTERN
ImageData_t *
ImageData_clone(ImageData_t * id)
{
  if (id != NULL)
  {
    return static_cast<ImageData_t*>(id->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
int
ImageData_getSamplesLength(const ImageData_t * id)
{
	return (id != NULL) ? id->getSamplesLength() : SBML_INT_MAX;
}


LIBSBML_EXTERN
const char *
ImageData_getDataType(const ImageData_t * id)
{
	return (id != NULL && id->isSetDataType()) ? id->getDataType().c_str() : NULL;
}


LIBSBML_EXTERN
int
ImageData_isSetSamples(const ImageData_t * id)
{
  return (id != NULL) ? static_cast<int>(id->isSetSamples()) : 0;
}


LIBSBML_EXTERN
int
ImageData_isSetSamplesLength(const ImageData_t * id)
{
  return (id != NULL) ? static_cast<int>(id->isSetSamplesLength()) : 0;
}


LIBSBML_EXTERN
int
ImageData_isSetDataType(const ImageData_t * id)
{
  return (id != NULL) ? static_cast<int>(id->isSetDataType()) : 0;
}


LIBSBML_EXTERN
int
ImageData_setSamplesLength(ImageData_t * id, int samplesLength)
{
  if (id != NULL)
    return id->setSamplesLength(samplesLength);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ImageData_setDataType(ImageData_t * id, const char * dataType)
{
  if (id != NULL)
    return (dataType == NULL) ? id->setDataType("") : id->setDataType(dataType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ImageData_unsetSamples(ImageData_t * id)
{
  return (id != NULL) ? id->unsetSamples() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ImageData_unsetSamplesLength(ImageData_t * id)
{
  return (id != NULL) ? id->unsetSamplesLength() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ImageData_unsetDataType(ImageData_t * id)
{
  return (id != NULL) ? id->unsetDataType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ImageData_hasRequiredAttributes(const ImageData_t * id)
{
  return (id != NULL) ? static_cast<int>(id->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


