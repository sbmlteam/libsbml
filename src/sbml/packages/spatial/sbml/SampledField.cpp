/**
 * @file    SampledField.cpp
 * @brief   Implementation of SampledField, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: SampledField.cpp 10670 2010-01-16 12:10:06Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/SampledField.cpp $
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
#include <limits>

#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/packages/spatial/sbml/ImageData.h>

#include <sbml/packages/spatial/sbml/SampledField.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new SampledField with the given level, version, and package version.
 */
SampledField::SampledField (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mDataType("")
   , mInterpolationType("")
   , mEncoding("")
   , mImageData(0)
   , mNumSamples1 (0)
   , mNumSamples2 (0)
   , mNumSamples3 (0)
   , mIsSetNumSamples1 (false)
   , mIsSetNumSamples2 (false)
   , mIsSetNumSamples3 (false)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}

/*
 * Creates a new SampledField with the given SpatialPkgNamespaces object.
 */
SampledField::SampledField(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mDataType("")
  , mInterpolationType("")
  , mEncoding("")
  , mImageData(0)
  , mNumSamples1 (0)
  , mNumSamples2 (0)
  , mNumSamples3 (0)
  , mIsSetNumSamples1 (false)
  , mIsSetNumSamples2 (false)
  , mIsSetNumSamples3 (false)
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor.
 */
SampledField::SampledField(const SampledField& source) : SBase(source)
{
  this->mSpatialId=source.mSpatialId;
  this->mDataType=source.mDataType;
  this->mInterpolationType=source.mInterpolationType;
  this->mEncoding = source.mEncoding;
  this->mNumSamples1=source.mNumSamples1;
  this->mNumSamples2=source.mNumSamples2;
  this->mNumSamples3=source.mNumSamples3;
  this->mIsSetNumSamples1=source.mIsSetNumSamples1;
  this->mIsSetNumSamples2=source.mIsSetNumSamples2;
  this->mIsSetNumSamples3=source.mIsSetNumSamples3;

  if (source.mImageData) 
  {
    mImageData = source.mImageData->deepCopy();
    mImageData->setParentSBMLObject(this);
  }

}

/*
 * Assignment operator.
 */
SampledField& SampledField::operator=(const SampledField& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
	this->mSpatialId = source.mSpatialId;
    this->mDataType = source.mDataType;
	this->mInterpolationType = source.mInterpolationType;
	this->mEncoding = source.mEncoding;
    this->mNumSamples1 = source.mNumSamples1;
    this->mNumSamples2 = source.mNumSamples2;
    this->mNumSamples3 = source.mNumSamples3;
    this->mIsSetNumSamples1 = source.mIsSetNumSamples1;
    this->mIsSetNumSamples2 = source.mIsSetNumSamples2;
    this->mIsSetNumSamples3 = source.mIsSetNumSamples3;

  	delete mImageData;
    if (source.mImageData) 
    {
      mImageData = source.mImageData->deepCopy();
      mImageData->setParentSBMLObject(this);
    }
    else
    {
      mImageData = 0;
    }
  }

  return *this;
}

/*
 * Destructor.
 */ 
SampledField::~SampledField ()
{
	// destroy 'ImageData'
	if (mImageData) delete mImageData;
}

/*
  * Returns the value of the "spatialId" attribute of this SampledField.
  */
const std::string& 
SampledField::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Returns the value of the "dataType" attribute of this SampledField.
  */
const std::string& 
SampledField::getDataType () const
{
  return mDataType;
}

/*
  * Returns the value of the "interpolationType" attribute of this SampledField.
  */
const std::string& 
SampledField::getInterpolationType () const
{
  return mInterpolationType;
}

/*
  * Returns the value of the "encoding" attribute of this SampledField.
  */
const std::string& 
SampledField::getEncoding () const
{
  return mEncoding;
}

/*
  * Returns the value of the "numSamples1" attribute of this SampledField.
  */
long 
SampledField::getNumSamples1 () const
{
  return mNumSamples1;
}

/*
  * Returns the value of the "numSamples2" attribute of this SampledField.
  */
long 
SampledField::getNumSamples2 () const
{
  return mNumSamples2;
}

/*
  * Returns the value of the "numSamples3" attribute of this SampledField.
  */
long 
SampledField::getNumSamples3 () const
{
  return mNumSamples3;
}

/*
  * Returns the "imageData" of this SampledField.
  */
const ImageData* 
SampledField::getImageData () const
{
  return mImageData;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SampledField's "spatialId" attribute has been set.
  */
bool 
SampledField::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SampledField's "dataType" attribute has been set.
  */
bool 
SampledField::isSetDataType () const
{
  return (mDataType.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SampledField's "interpolationType" attribute has been set.
  */
bool 
SampledField::isSetInterpolationType () const
{
  return (mInterpolationType.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SampledField's "encoding" attribute has been set.
  */
bool 
SampledField::isSetEncoding () const
{
  return (mEncoding.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SampledField's "numSamples1" attribute has been set.
  */
bool 
SampledField::isSetNumSamples1 () const
{
  return mIsSetNumSamples1;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SampledField's "numSamples2" attribute has been set.
  */
bool 
SampledField::isSetNumSamples2 () const
{
  return mIsSetNumSamples2;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SampledField's "numSamples3" attribute has been set.
  */
bool 
SampledField::isSetNumSamples3 () const
{
  return mIsSetNumSamples3;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SampledField's "imageData" has been set.
  */
bool 
SampledField::isSetImageData () const
{
  return (mImageData != 0);
}

/*
  * Sets the value of the "spatialId" attribute of this SampledField.
  */
int 
SampledField::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
  * Sets the value of the "dataType" attribute of this SampledField.
  */
int 
SampledField::setDataType (const std::string& dataType)
{
  return SyntaxChecker::checkAndSetSId(dataType ,mDataType);
}

/*
  * Sets the value of the "interpolationType" attribute of this SampledField.
  */
int 
SampledField::setInterpolationType (const std::string& interpolationType)
{
  return SyntaxChecker::checkAndSetSId(interpolationType ,mInterpolationType);
}

/*
  * Sets the value of the "encoding" attribute of this SampledField.
  */
int 
SampledField::setEncoding (const std::string& encoding)
{
  return SyntaxChecker::checkAndSetSId(encoding ,mEncoding);
}

/*
  * Sets the value of the "numSamples1" attribute of this SampledField.
  */
int 
SampledField::setNumSamples1 (long numSamples1)
{
  if (numSamples1 < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mNumSamples1 = numSamples1;
    mIsSetNumSamples1  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
  * Sets the value of the "numSamples2" attribute of this SampledField.
  */
int 
SampledField::setNumSamples2 (long numSamples2)
{
  if (numSamples2 < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mNumSamples2 = numSamples2;
    mIsSetNumSamples2  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
  * Sets the value of the "numSamples3" attribute of this SampledField.
  */
int 
SampledField::setNumSamples3 (long numSamples3)
{
  if (numSamples3 < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mNumSamples3 = numSamples3;
    mIsSetNumSamples3  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
  * Sets the value of the "samples" attribute of this SampledField.
  */
int 
SampledField::setImageData (const ImageData* imageData)
{
  if (mImageData == imageData)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (imageData == NULL)
  {
	  delete mImageData;
	  mImageData = 0;
      return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mImageData;
    mImageData = (imageData != 0) ? imageData->deepCopy() : 0;
    if (mImageData) mImageData->setParentSBMLObject(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}

 /*
  * Unsets the value of the "spatialId" attribute of this SampledField.
  */
int 
SampledField::unsetSpatialId ()
{
  mSpatialId.erase();
  if (mSpatialId.empty())
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
SampledField::unsetDataType ()
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
  * Unsets the value of the "interpolationType" attribute of this SampledField.
  */
int 
SampledField::unsetInterpolationType ()
{
  mInterpolationType.erase();
  if (mInterpolationType.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

 /*
  * Unsets the value of the "encoding" attribute of this SampledField.
  */
int 
SampledField::unsetEncoding ()
{
  mEncoding.erase();
  if (mEncoding.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "numSamples1" attribute of this SampledField.
  */
int 
SampledField::unsetNumSamples1 ()
{
  mIsSetNumSamples1 = false;
  
  if (!isSetNumSamples1())
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
SampledField::unsetNumSamples2 ()
{
  mIsSetNumSamples2 = false;
  
  if (!isSetNumSamples2())
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
SampledField::unsetNumSamples3 ()
{
  mIsSetNumSamples3 = false;
  
  if (!isSetNumSamples3())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Creates a new ImageData for this SampledField and returns it.  If this
 * SampledField had a previous ImageData, it will be destroyed.
 */
ImageData*
SampledField::createImageData ()
{
  delete mImageData;
  mImageData = 0;

  try
  {
    SPATIAL_CREATE_NS(spatialNs, getSBMLNamespaces());
    mImageData = new ImageData(spatialNs);
  }
  catch (...)
  {
    /* here we do not create a default object 
     *
     * so do nothing
     */
  }

  if (mImageData)
  {
    mImageData->setParentSBMLObject(this);
  }

  return mImageData;
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
SampledField::getElementName () const
{
  static const std::string name = "sampledField";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SampledField::createObject (XMLInputStream& stream)
{
  // return 0;
  SBase*        object = 0;

  object=SBase::createObject(stream);
  
  return object;

}

/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
SampledField::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);
  
  attributes.add("spatialId");
  attributes.add("dataType");
  attributes.add("interpolationType");
  attributes.add("encoding");
  attributes.add("numSamples1");
  attributes.add("numSamples2");
  attributes.add("numSamples3");
}

bool 
SampledField::hasRequiredElements() const
{
  bool allPresent = true;

  /* required attributes for sampledField: imageData */

  if (!isSetImageData())
    allPresent = false;

  return allPresent;
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SampledField::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<SampledField>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("dataType", mDataType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mDataType.empty())
  {
    logEmptyString(mDataType, sbmlLevel, sbmlVersion, "<SampledField>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mDataType)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("interpolationType", mInterpolationType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mInterpolationType.empty())
  {
    logEmptyString(mInterpolationType, sbmlLevel, sbmlVersion, "<SampledField>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mInterpolationType)) logError(InvalidIdSyntax);
  
  assigned = attributes.readInto("encoding", mEncoding, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mEncoding.empty())
  {
    logEmptyString(mEncoding, sbmlLevel, sbmlVersion, "<SampledField>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mEncoding)) logError(InvalidIdSyntax);
  
  attributes.readInto("numSamples1", mNumSamples1, getErrorLog(), true, getLine(), getColumn());
  if (mNumSamples1 < 0)
  {
    std::string message = "The numSamples1 attribute on ";
    message += "a <SampledField> may only have values > 0.";
    getErrorLog()->logError(NotSchemaConformant, sbmlLevel, sbmlVersion, message);
  }

  attributes.readInto("numSamples2", mNumSamples2, getErrorLog(), true, getLine(), getColumn());
  if (mNumSamples2 < 0)
  {
    std::string message = "The numSamples2 attribute on ";
    message += "a <SampledField> may only have values > 0.";
    getErrorLog()->logError(NotSchemaConformant, sbmlLevel, sbmlVersion, message);
  }

  attributes.readInto("numSamples3", mNumSamples3, getErrorLog(), true, getLine(), getColumn());
  if (mNumSamples3 < 0)
  {
    std::string message = "The numSamples3 attribute on ";
    message += "a <SampledField> may only have values > 0.";
    getErrorLog()->logError(NotSchemaConformant, sbmlLevel, sbmlVersion, message);
  }
}

/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, ImageData. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
SampledField::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  if (name == "imageData")
  {
 
    delete mImageData;
	mImageData = ImageData::readImageData(stream);
    if (mImageData) mImageData->setParentSBMLObject(this);
    read  = true;
  }

  /* ------------------------------
   *
   *   (EXTENSION)
   *
   *------------------------------- */
  if ( SBase::readOtherXML(stream) )
    read = true;

  return read;
}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SampledField::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);
  stream.writeAttribute("dataType",   getPrefix(), mDataType);
  stream.writeAttribute("interpolationType",   getPrefix(), mInterpolationType);  
  stream.writeAttribute("encoding",   getPrefix(), mEncoding);  
  stream.writeAttribute("numSamples1", getPrefix(), mNumSamples1);
  stream.writeAttribute("numSamples2", getPrefix(), mNumSamples2);
  stream.writeAttribute("numSamples3", getPrefix(), mNumSamples3);


  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}


/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
SampledField::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (mImageData) ImageData::writeImageData(mImageData, stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
SampledField::getTypeCode () const
{
	return SBML_SPATIAL_SAMPLEDFIELD;
}

SampledField*
SampledField::clone() const
{
    return new SampledField(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
SampledField::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}

LIBSBML_CPP_NAMESPACE_END

