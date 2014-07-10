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
  , mDataType ("")
  , mNumSamples1 (SBML_INT_MAX)
  , mIsSetNumSamples1 (false)
  , mNumSamples2 (SBML_INT_MAX)
  , mIsSetNumSamples2 (false)
  , mNumSamples3 (SBML_INT_MAX)
  , mIsSetNumSamples3 (false)
  , mInterpolationType ("")
  , mEncoding ("")
  , mImageData (NULL)
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
  , mDataType ("")
  , mNumSamples1 (SBML_INT_MAX)
  , mIsSetNumSamples1 (false)
  , mNumSamples2 (SBML_INT_MAX)
  , mIsSetNumSamples2 (false)
  , mNumSamples3 (SBML_INT_MAX)
  , mIsSetNumSamples3 (false)
  , mInterpolationType ("")
  , mEncoding ("")
  , mImageData (NULL)
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
    mEncoding  = orig.mEncoding;
    if (orig.mImageData != NULL)
    {
      mImageData = orig.mImageData->clone();
    }
    else
    {
      mImageData = NULL;
    }

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
    mEncoding  = rhs.mEncoding;
    if (rhs.mImageData != NULL)
    {
      mImageData = rhs.mImageData->clone();
    }
    else
    {
      mImageData = NULL;
    }

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
  delete mImageData;
  mImageData = NULL;
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
const std::string&
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
const std::string&
SampledField::getInterpolationType() const
{
  return mInterpolationType;
}


/*
 * Returns the value of the "encoding" attribute of this SampledField.
 */
const std::string&
SampledField::getEncoding() const
{
  return mEncoding;
}


/*
 * Returns the value of the "imageData" attribute of this SampledField.
 */
const ImageData*
SampledField::getImageData() const
{
  return mImageData;
}


/*
 * Returns the value of the "imageData" attribute of this SampledField.
 */
ImageData*
SampledField::getImageData()
{
  return mImageData;
}


/*
 * Creates a new "imageData" element of this SampledField and returns it.
 */
ImageData*
SampledField::createImageData()
{
  if (mImageData != NULL) delete mImageData;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mImageData = new ImageData(spatialns);
  delete spatialns;
  connectToChild();
  return mImageData;
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
  return (mDataType.empty() == false);
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
  return (mInterpolationType.empty() == false);
}


/*
 * Returns true/false if encoding is set.
 */
bool
SampledField::isSetEncoding() const
{
  return (mEncoding.empty() == false);
}


/*
 * Returns true/false if imageData is set.
 */
bool
SampledField::isSetImageData() const
{
  return (mImageData != NULL);
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
SampledField::setDataType(const std::string& dataType)
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
SampledField::setInterpolationType(const std::string& interpolationType)
{
  if (&(interpolationType) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mInterpolationType = interpolationType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets encoding and returns value indicating success.
 */
int
SampledField::setEncoding(const std::string& encoding)
{
  if (&(encoding) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mEncoding = encoding;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets imageData and returns value indicating success.
 */
int
SampledField::setImageData(ImageData* imageData)
{
  if (mImageData == imageData)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (imageData == NULL)
  {
    delete mImageData;
    mImageData = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mImageData;
    mImageData = (imageData != NULL) ?
      static_cast<ImageData*>(imageData->clone()) : NULL;
    if (mImageData != NULL)
    {
      mImageData->connectToParent(this);
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
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
  mInterpolationType.erase();

  if (mInterpolationType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets encoding and returns value indicating success.
 */
int
SampledField::unsetEncoding()
{
  mEncoding.erase();

  if (mEncoding.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets imageData and returns value indicating success.
 */
int
SampledField::unsetImageData()
{
  delete mImageData;
  mImageData = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


List*
SampledField::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mImageData, filter);

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

  if (isSetNumSamples1() == false)
    allPresent = false;

  if (isSetEncoding() == false)
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

  if (isSetImageData() == false)
    allPresent = false;

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
  if (isSetImageData() == true)
  {
    mImageData->write(stream);
  }
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
  if ( mImageData != NULL)
    mImageData->setSBMLDocument(d);
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

  if (mImageData != NULL)
    mImageData->connectToParent(this);
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

  const string& name = stream.peek().getName();

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "imageData")
  {
    mImageData = new ImageData(spatialns);
    object = mImageData;
  }

  delete spatialns;

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
  attributes.add("encoding");
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
        "The syntax of the attribute id='" + mId + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message);
  }

  //
  // dataType string   ( use = "optional" )
  //
  assigned = attributes.readInto("dataType", mDataType);

  if (assigned == true)
  {
    // check string is not empty

    if (mDataType.empty() == true)
    {
      logEmptyString(mDataType, getLevel(), getVersion(), "<SampledField>");
    }
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
                     getPackageVersion(), sbmlLevel, sbmlVersion);
      }
      else
      {
        std::string message = "Spatial attribute 'numSamples1' is missing.";
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
                     getPackageVersion(), sbmlLevel, sbmlVersion);
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
                     getPackageVersion(), sbmlLevel, sbmlVersion);
      }
    }
  }

  //
  // interpolationType string   ( use = "optional" )
  //
  assigned = attributes.readInto("interpolationType", mInterpolationType);

  if (assigned == true)
  {
    // check string is not empty

    if (mInterpolationType.empty() == true)
    {
      logEmptyString(mInterpolationType, getLevel(), getVersion(), "<SampledField>");
    }
  }

  //
  // encoding string   ( use = "required" )
  //
  assigned = attributes.readInto("encoding", mEncoding);

  if (assigned == true)
  {
    // check string is not empty

    if (mEncoding.empty() == true)
    {
      logEmptyString(mEncoding, getLevel(), getVersion(), "<SampledField>");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'encoding' is missing.";
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
SampledField::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetDataType() == true)
    stream.writeAttribute("dataType", getPrefix(), mDataType);

  if (isSetNumSamples1() == true)
    stream.writeAttribute("numSamples1", getPrefix(), mNumSamples1);

  if (isSetNumSamples2() == true)
    stream.writeAttribute("numSamples2", getPrefix(), mNumSamples2);

  if (isSetNumSamples3() == true)
    stream.writeAttribute("numSamples3", getPrefix(), mNumSamples3);

  if (isSetInterpolationType() == true)
    stream.writeAttribute("interpolationType", getPrefix(), mInterpolationType);

  if (isSetEncoding() == true)
    stream.writeAttribute("encoding", getPrefix(), mEncoding);

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
const char *
SampledField_getDataType(const SampledField_t * sf)
{
	return (sf != NULL && sf->isSetDataType()) ? sf->getDataType().c_str() : NULL;
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
const char *
SampledField_getInterpolationType(const SampledField_t * sf)
{
	return (sf != NULL && sf->isSetInterpolationType()) ? sf->getInterpolationType().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
SampledField_getEncoding(const SampledField_t * sf)
{
	return (sf != NULL && sf->isSetEncoding()) ? sf->getEncoding().c_str() : NULL;
}


LIBSBML_EXTERN
ImageData_t*
SampledField_getImageData(SampledField_t * sf)
{
	if (sf == NULL)
		return NULL;

	return (ImageData_t*)sf->getImageData();
}


LIBSBML_EXTERN
ImageData_t*
SampledField_createImageData(SampledField_t * sf)
{
	if (sf == NULL)
		return NULL;

	return (ImageData_t*)sf->createImageData();
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
SampledField_isSetEncoding(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetEncoding()) : 0;
}


LIBSBML_EXTERN
int
SampledField_isSetImageData(const SampledField_t * sf)
{
  return (sf != NULL) ? static_cast<int>(sf->isSetImageData()) : 0;
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
SampledField_setDataType(SampledField_t * sf, const char * dataType)
{
  if (sf != NULL)
    return (dataType == NULL) ? sf->setDataType("") : sf->setDataType(dataType);
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
SampledField_setInterpolationType(SampledField_t * sf, const char * interpolationType)
{
  if (sf != NULL)
    return (interpolationType == NULL) ? sf->setInterpolationType("") : sf->setInterpolationType(interpolationType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_setEncoding(SampledField_t * sf, const char * encoding)
{
  if (sf != NULL)
    return (encoding == NULL) ? sf->setEncoding("") : sf->setEncoding(encoding);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledField_setImageData(SampledField_t * sf, ImageData_t* imageData)
{
	return (sf != NULL) ? sf->setImageData(imageData) : LIBSBML_INVALID_OBJECT;
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
SampledField_unsetEncoding(SampledField_t * sf)
{
  return (sf != NULL) ? sf->unsetEncoding() : LIBSBML_INVALID_OBJECT;
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




LIBSBML_CPP_NAMESPACE_END


