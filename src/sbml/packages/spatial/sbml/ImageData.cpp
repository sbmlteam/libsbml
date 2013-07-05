/**
 * @file    ImageData.cpp
 * @brief   Implementation of ImageData, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: ImageData.cpp 10670 2010-01-16 12:10:06Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/ImageData.cpp $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <iostream>
#include <stdlib.h>
#include <vector>
#include <limits>
#include <cstring>

#include <sbml/SBMLVisitor.h>
#include <sbml/SBase.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/compress/CompressCommon.h>

#ifdef USE_ZLIB
#include <zlib.h>
#endif

#include <sbml/packages/spatial/sbml/ImageData.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/extension/SBMLExtensionException.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN


  /**
  * Accepts the given SBMLVisitor for this SBase object.
  *
  * @param v the SBMLVisitor instance to be used
  *
  * @return the result of calling <code>v.visit()</code>.
  */

bool 
ImageData::accept (SBMLVisitor& v) const
{ 
  return false; 
} 


  /**
   * Creates and returns a deep copy of this SBase object.
   * 
   * @return a (deep) copy of this SBase object.
   */
SBase* 
ImageData::clone () const
{
  return deepCopy(); 
}

/*
 * Creates a new ImageData with the given level, version, and package version.
 */
ImageData::ImageData (unsigned int level, unsigned int version, unsigned int pkgVersion) 
 : SBase(level, version)
 , mDataType("")
 , mSamples(NULL)
 , mSamplesLength(0)
 , mURI("")
 , mParentSBMLObject(NULL)
 , mSBMLNamespaces(NULL)
 , mIsSetSamples(false)
{
  // initialize 'dataType'
  mDataType = "";
  // initilaize samples array
  mSamplesLength = 1;
  mSamples = new int[1];
  mSamples[0] = 0;

  mIsSetSamples = false;
  mParentSBMLObject = NULL;

  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  // Taken from SBase.setSBMLNamepsacesAndOwn();
  // setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
  /*
  SBMLNamespaces* sbmlns = new SpatialPkgNamespaces(level,version,pkgVersion);
  delete mSBMLNamespaces;
  mSBMLNamespaces = sbmlns;

  if(sbmlns) 
	mURI = sbmlns->getURI();
  */
}

/*
 * Creates a new ImageData with the given SpatialPkgNamespaces object.
 */
ImageData::ImageData(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
 , mDataType("")
 , mSamples(NULL)
 , mSamplesLength(0)
 , mURI("")
 , mParentSBMLObject(NULL)
 , mSBMLNamespaces(NULL)
 , mIsSetSamples(false)
{
  //
  // set the element namespace of this object
  //
  // taken from (SBase->setElementNamespace())
  mURI = spatialns->getURI();
  mSBMLNamespaces = spatialns;

  // initialize 'dataType'
  mDataType = "";
  // initilaize samples array
  mSamplesLength = 1;
  mSamples = new int[mSamplesLength];
  mSamples[0] = 0;

  mIsSetSamples = false;
  mParentSBMLObject = NULL;


  // load package extensions bound with this object (if any) 
  // loadPlugins(spatialns);
}

/*
 * Copy constructor.
 */
ImageData::ImageData(const ImageData& source)
 : SBase(source)
 , mDataType(source.mDataType)
 , mSamples(NULL)
 , mSamplesLength(source.mSamplesLength)
 , mURI(source.mURI)
 , mParentSBMLObject(NULL)
 , mSBMLNamespaces(NULL)
 , mIsSetSamples(source.mIsSetSamples)
{
  if (source.mSamples != NULL)
  {
  this->mSamples = new int[mSamplesLength];
  memcpy(mSamples , source.mSamples, sizeof(int)*mSamplesLength);     
  }
}

/*
 * Assignment operator.
 */
ImageData& ImageData::operator=(const ImageData& source)
{
  if(&source!=this)
  {
	this->mDataType = source.mDataType;
	this->mSamplesLength = source.mSamplesLength;
  if (source.mSamples != NULL)
  {
  this->mSamples = new int[mSamplesLength];
  memcpy(mSamples , source.mSamples, sizeof(int)*mSamplesLength);
  }
  else
  {
    this->mSamples = NULL;
  }

	this->mIsSetSamples=source.mIsSetSamples;
	this->mParentSBMLObject=source.mParentSBMLObject;
  this->mURI=source.mURI;
  this->mSBMLNamespaces=source.mSBMLNamespaces;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
ImageData::~ImageData ()
{
	// destroy 'samples' array here --- ???
	delete[] mSamples;
	mSamplesLength = 0;
}

/*
  * Returns the value of the "dataType" attribute of this ImageData.
  */
const std::string& 
ImageData::getDataType () const
{
  return mDataType;
}

  /*
   * The "samples" attribute of this ImageData is returned in an int array (pointer) 
   * that is passed as argument to the method (this is needed while using SWIG to
   * convert int[] from C++ to Java). The method itself has a return type void.
   *
   * @return void.
  */
void 
ImageData::getSamples (int* outputSamples) const
{
   if (outputSamples == NULL) {
	 throw SBMLExtensionException("Cannot return null or 0-length array");
   }

   memcpy(outputSamples , mSamples, sizeof(int)*mSamplesLength);
   
  // return mSamples;
}

unsigned int
ImageData::getSamplesLength () const
{
	return mSamplesLength;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * ImageData's "dataType" attribute has been set.
  */
bool 
ImageData::isSetDataType () const
{
  return (mDataType.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * ImageData's "samples" attribute has been set.
  */
bool 
ImageData::isSetSamples () const
{
  return mIsSetSamples;
}

/*
  * Sets the value of the "dataType" attribute of this ImageData.
  */
int 
ImageData::setDataType (const std::string& dataType)
{
  // return SyntaxChecker::checkAndSetSId(dataType ,mDataType);
  mDataType     = dataType;
  return LIBSBML_OPERATION_SUCCESS;

}

/*
  * Sets the value of the "samples" attribute of this ImageData.
  */
int 
ImageData::setSamples (int* samples, int samplesLength)
{
  if (samples == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
	  mSamplesLength = samplesLength;
	  mSamples = new int[mSamplesLength];
    memcpy(mSamples, samples, sizeof(int)*samplesLength);
	  mIsSetSamples  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
  * Unsets the value of the "dataType" attribute of this ImageData.
  */
int 
ImageData::unsetDataType ()
{
  mDataType.erase();
  if (mDataType.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "samples" attribute of this ImageData.
  */
int 
ImageData::unsetSamples ()
{
  mIsSetSamples = false;
  
  if (!isSetSamples())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Clones an imageData object
 */
ImageData*
ImageData::deepCopy () const
{
  return new ImageData(*this);
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ImageData::getElementName () const
{
  static const std::string name = "imageData";
  return name;
}

/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
ImageData::getTypeCode () const
{
	return SBML_SPATIAL_IMAGEDATA;
}

/**
 * Reads the imageData 'samples' from the given XMLInputStream, 
 */
ImageData* ImageData::readImageData (XMLInputStream& stream)
 {
	ImageData* id = NULL;
	const string& name = stream.peek().getName();
	if (name != "imageData") 
    return id;
	
		XMLToken nextToken = stream.next();

		// create an ImageData object
    SPATIAL_CREATE_NS(spatialns, stream.getSBMLNamespaces());
		id = new ImageData(spatialns);

		XMLAttributes attributes = nextToken.getAttributes();

		for (int i = 0; i < attributes.getLength(); i++)
	    {
		  std::string nameAtt   = attributes.getName(i);
		  std::string uriAtt    = attributes.getURI(i);
		  std::string prefixAtt = attributes.getPrefix(i);
		  std::string valueAtt    = attributes.getValue(i);

		  if (nameAtt == "dataType") {
			  id->setDataType(valueAtt);
			 //std::cout << "ImageData dataType: " << id->getDataType() << std::endl;
		  }
		}

		while (!nextToken.isText()) {
			nextToken = stream.next();
		}
		const string& nextTokenChars = nextToken.getCharacters();

		// The imageData element content is obtained as a string. Now parse 
		// the string to get individual ints as strings
		stringstream strStream(nextTokenChars); // Insert the string into a stream
		string buffer;
		vector<int> intValuesVector;
		while (strStream >> buffer) 
		{
			// convert each string token (buf) to int & store in a vector<int>
			int val = atoi(buffer.c_str());
			intValuesVector.push_back(val);
		}

		// convert the vector<int> to an array of ints
		unsigned int samplesSize = (unsigned int)intValuesVector.size();
		int* samples = new int[samplesSize];
		for (unsigned int i = 0; i < samplesSize; i++) 
		{
			samples[i] = intValuesVector.at(i);
		}
		
		// set the samples on imageData.
		id->setSamples(samples, samplesSize);
	
	return id;
 }

/*
 * Writes imageData 'samples' to given XMLOutputStream
 */
void ImageData::writeImageData (ImageData* imageData, XMLOutputStream& stream)
{
  unsigned int length = imageData->getSamplesLength();
  int* samplesArray = new int[length];
  imageData->getSamples(samplesArray);
  string uri = imageData->getURI();
  string dataType = imageData->getDataType();

  // Write out the values in 'samplesArray' as content of ImageData element.
  if (length == 0) 
    return ; 
  
	  string prefix = "";
    if (imageData->getSBMLNamespaces() != NULL)
    {
	  XMLNamespaces* xmlNamespaces = imageData->getSBMLNamespaces()->getNamespaces();
	  if (xmlNamespaces->hasURI(uri)) 
	  {		
		prefix = xmlNamespaces->getPrefix(uri);
	  }
    }
	  // write out the imageData element with prefix, if present.
	  stream.startElement("imageData", prefix);
	  stream.writeAttribute("dataType", prefix, dataType);
	  for (unsigned int i = 0; i < length; i++) {
		stream << (long)samplesArray[i] << " ";
	  }
	  stream.endElement("imageData", prefix);
  

}

/** @cond doxygen-libsbml-internal */
void ImageData::setParentSBMLObject(SBase * sb)
{
  mParentSBMLObject = sb;
  connectToParent(sb);
}
/** @endcond doxygen-libsbml-internal */


SBase * ImageData::getParentSBMLObject() const
{
  return mParentSBMLObject;
}

/* gets the SBMLnamespaces - internal use only*/
SBMLNamespaces *
ImageData::getSBMLNamespaces() const
{
  if (mParentSBMLObject != NULL)
    return mParentSBMLObject->getSBMLNamespaces();

  if (mSBMLNamespaces != NULL)
    return mSBMLNamespaces;
  else
    return new SBMLNamespaces();
}

std::string
ImageData::getURI() const
{
	return mURI;
}

/**  
 *  Returns the data of this image as uncompressed array of integers
 * 
 * @param data the output array of integers (it will be allocated using
 *             malloc and will have to be freed using free)
 * @param length the output lenght of the array
 *
 */
void 
ImageData::getUncompressedData(int* &data, int& length) const
{
  if (mDataType.empty() || mDataType == "compressed")
  {
    char* csamples = (char*)malloc(sizeof(char)*mSamplesLength);
    for (unsigned int i = 0 ; i < mSamplesLength; ++i)
      csamples[i] = mSamples[i];
    ImageData::uncompress_data(csamples, mSamplesLength, data, length);
    free(csamples);
  }
  else
  {
    // lets just return a copy of the data we have as they ought to be uncompressed
    length = mSamplesLength;
    data = (int*)malloc(sizeof(int)*mSamplesLength);
    memcpy(mSamples, data, sizeof(int)*length);
  }
}

void 
ImageData::uncompress_data(void *data, size_t length, int*& result, int& outLength)
{
#ifndef USE_ZLIB
  throw ZlibNotLinked();
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

LIBSBML_CPP_NAMESPACE_END
