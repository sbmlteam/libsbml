/**
 * @file:   ParametricObject.cpp
 * @brief:  Implementation of the ParametricObject class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
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


#include <sbml/packages/spatial/sbml/ParametricObject.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new ParametricObject with the given level, version, and package version.
 */
ParametricObject::ParametricObject (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mPolygonType (POLYGONKIND_UNKNOWN)
  , mDomainType ("")
  , mPointIndex (NULL)
  , mPointIndexLength (SBML_INT_MAX)
  , mIsSetPointIndexLength (false)
  , mCompression (COMPRESSIONKIND_UNKNOWN)
  , mDataType (DATAKIND_UNKNOWN)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new ParametricObject with the given SpatialPkgNamespaces object.
 */
ParametricObject::ParametricObject (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mPolygonType (POLYGONKIND_UNKNOWN)
  , mDomainType ("")
  , mPointIndex (NULL)
  , mPointIndexLength (SBML_INT_MAX)
  , mIsSetPointIndexLength (false)
  , mCompression (COMPRESSIONKIND_UNKNOWN)
  , mDataType (DATAKIND_UNKNOWN)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for ParametricObject.
 */
ParametricObject::ParametricObject (const ParametricObject& orig)
  : SBase(orig)
  , mId  ( orig.mId)
  , mPolygonType  ( orig.mPolygonType)
  , mDomainType  ( orig.mDomainType)
  , mPointIndex  ( NULL)
  , mPointIndexLength  ( orig.mPointIndexLength)
  , mIsSetPointIndexLength  ( orig.mIsSetPointIndexLength)
  , mCompression  ( orig.mCompression)
  , mDataType  ( orig.mDataType)
{
  setPointIndex(orig.mPointIndex, orig.mPointIndexLength);
  // connect to child objects
  connectToChild();
}


/*
 * Assignment for ParametricObject.
 */
ParametricObject&
ParametricObject::operator=(const ParametricObject& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mPolygonType  = rhs.mPolygonType;
    mDomainType  = rhs.mDomainType;
    mPointIndex  = NULL;
    setPointIndex(rhs.mPointIndex, rhs.mPointIndexLength);
    mPointIndexLength  = rhs.mPointIndexLength;
    mIsSetPointIndexLength  = rhs.mIsSetPointIndexLength;
    mCompression  = rhs.mCompression;
    mDataType  = rhs.mDataType;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for ParametricObject.
 */
ParametricObject*
ParametricObject::clone () const
{
  return new ParametricObject(*this);
}


/*
 * Destructor for ParametricObject.
 */
ParametricObject::~ParametricObject ()
{
  if (mPointIndex != NULL)
    delete[] mPointIndex;
  mPointIndex = NULL;
}


/*
 * Returns the value of the "id" attribute of this ParametricObject.
 */
const std::string&
ParametricObject::getId() const
{
  return mId;
}


/*
 * Returns the value of the "polygonType" attribute of this ParametricObject.
 */
PolygonKind_t
ParametricObject::getPolygonType() const
{
  return mPolygonType;
}


/*
 * Returns the value of the "domainType" attribute of this ParametricObject.
 */
const std::string&
ParametricObject::getDomainType() const
{
  return mDomainType;
}


/*
 * The "pointIndex" attribute of this ParametricObject is returned in an int* array (pointer)
 * that is passed as argument to the method (this is needed while using SWIG to
 * convert int[] from C++ to Java). The method itself has a return type void.
 *
 * NOTE: you have to pre-allocate the array with the correct length! *
 * @return void.
 */
void
ParametricObject::getPointIndex(int* outArray) const
{
   if (outArray == NULL || mPointIndex == NULL) return;

   memcpy(outArray , mPointIndex, sizeof(int)*mPointIndexLength);
}


/*
 * Returns the value of the "pointIndexLength" attribute of this ParametricObject.
 */
int
ParametricObject::getPointIndexLength() const
{
  return mPointIndexLength;
}


/*
 * Returns the value of the "compression" attribute of this ParametricObject.
 */
CompressionKind_t
ParametricObject::getCompression() const
{
  return mCompression;
}


/*
 * Returns the value of the "dataType" attribute of this ParametricObject.
 */
DataKind_t
ParametricObject::getDataType() const
{
  return mDataType;
}


/*
 * Returns true/false if id is set.
 */
bool
ParametricObject::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if polygonType is set.
 */
bool
ParametricObject::isSetPolygonType() const
{
  return mPolygonType != POLYGONKIND_UNKNOWN;
}


/*
 * Returns true/false if domainType is set.
 */
bool
ParametricObject::isSetDomainType() const
{
  return (mDomainType.empty() == false);
}


/*
 * Returns true/false if pointIndex is set.
 */
bool
ParametricObject::isSetPointIndex() const
{
  return (mPointIndex != NULL);
}


/*
 * Returns true/false if pointIndexLength is set.
 */
bool
ParametricObject::isSetPointIndexLength() const
{
  return mIsSetPointIndexLength;
}


/*
 * Returns true/false if compression is set.
 */
bool
ParametricObject::isSetCompression() const
{
  return mCompression != COMPRESSIONKIND_UNKNOWN;
}


/*
 * Returns true/false if dataType is set.
 */
bool
ParametricObject::isSetDataType() const
{
  return mDataType != DATAKIND_UNKNOWN;
}


/*
 * Sets id and returns value indicating success.
 */
int
ParametricObject::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets polygonType and returns value indicating success.
 */
int
ParametricObject::setPolygonType(PolygonKind_t polygonType)
{
  mPolygonType = polygonType;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets polygonType and returns value indicating success.
 */
int
ParametricObject::setPolygonType(const std::string& polygonType)
{
  PolygonKind_t parsed = PolygonKind_parse(polygonType.c_str());
  if (parsed == POLYGONKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mPolygonType = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets domainType and returns value indicating success.
 */
int
ParametricObject::setDomainType(const std::string& domainType)
{
  if (!(SyntaxChecker::isValidInternalSId(domainType)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomainType = domainType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the "pointIndex" element of this ParametricObject.
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
ParametricObject::setPointIndex(int* inArray, int arrayLength)
{
  if (inArray == NULL) return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  if (mPointIndex != NULL) delete[] mPointIndex;
  mPointIndex = new int[arrayLength];
  memcpy(mPointIndex, inArray, sizeof(int)*arrayLength);
  mIsSetPointIndexLength = true;
  mPointIndexLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}
/*
 * Sets pointIndexLength and returns value indicating success.
 */
int
ParametricObject::setPointIndexLength(int pointIndexLength)
{
  mPointIndexLength = pointIndexLength;
  mIsSetPointIndexLength = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets compression and returns value indicating success.
 */
int
ParametricObject::setCompression(CompressionKind_t compression)
{
  mCompression = compression;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets compression and returns value indicating success.
 */
int
ParametricObject::setCompression(const std::string& compression)
{
  CompressionKind_t parsed = CompressionKind_parse(compression.c_str());
  if (parsed == COMPRESSIONKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mCompression = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets dataType and returns value indicating success.
 */
int
ParametricObject::setDataType(DataKind_t dataType)
{
  mDataType = dataType;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets dataType and returns value indicating success.
 */
int
ParametricObject::setDataType(const std::string& dataType)
{
  DataKind_t parsed = DataKind_parse(dataType.c_str());
  if (parsed == DATAKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mDataType = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
 */
int
ParametricObject::unsetId()
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
 * Unsets polygonType and returns value indicating success.
 */
int
ParametricObject::unsetPolygonType()
{
  mPolygonType = POLYGONKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets domainType and returns value indicating success.
 */
int
ParametricObject::unsetDomainType()
{
  mDomainType.erase();

  if (mDomainType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets pointIndex and returns value indicating success.
 */
int
ParametricObject::unsetPointIndex()
{
  if (mPointIndex != NULL)
   delete[] mPointIndex;
  mPointIndex = NULL;
  return unsetPointIndexLength();
}


/*
 * Unsets pointIndexLength and returns value indicating success.
 */
int
ParametricObject::unsetPointIndexLength()
{
  mPointIndexLength = SBML_INT_MAX;
  mIsSetPointIndexLength = false;

  if (isSetPointIndexLength() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets compression and returns value indicating success.
 */
int
ParametricObject::unsetCompression()
{
  mCompression = COMPRESSIONKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets dataType and returns value indicating success.
 */
int
ParametricObject::unsetDataType()
{
  mDataType = DATAKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
ParametricObject::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetDomainType() == true && mDomainType == oldid)
  {
    setDomainType(newid);
  }

}


List*
ParametricObject::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ParametricObject::getElementName () const
{
  static const string name = "parametricObject";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ParametricObject::getTypeCode () const
{
  return SBML_SPATIAL_PARAMETRICOBJECT;
}


/*
 * check if all the required attributes are set
 */
bool
ParametricObject::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetPolygonType() == false)
    allPresent = false;

  if (isSetDomainType() == false)
    allPresent = false;

  if (isSetPointIndex() == false)
    allPresent = false;

  if (isSetPointIndexLength() == false)
    allPresent = false;

  if (isSetCompression() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
ParametricObject::hasRequiredElements () const
{
  bool allPresent = true;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
ParametricObject::writeElements (XMLOutputStream& stream) const
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
ParametricObject::accept (SBMLVisitor& v) const
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
ParametricObject::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
ParametricObject::connectToChild()
{
  SBase::connectToChild();

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
ParametricObject::enablePackageInternal(const std::string& pkgURI,
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
ParametricObject::createObject(XMLInputStream& stream)
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
ParametricObject::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("polygonType");
  attributes.add("domainType");
  attributes.add("pointIndexLength");
  attributes.add("compression");
  attributes.add("dataType");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
ParametricObject::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfParametricObjects - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfParametricObjects*>(getParentSBMLObject())->size() < 2)
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
      logEmptyString(mId, getLevel(), getVersion(), "<ParametricObject>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'parametricObject' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // polygonType enum  ( use = "required" )
  //
  mPolygonType = POLYGONKIND_UNKNOWN;
  {
    std::string stringValue;
    assigned = attributes.readInto("polygonType", stringValue);

    if (assigned == true)
    {
      // parse enum

      mPolygonType = PolygonKind_parse(stringValue.c_str());
      if(mPolygonType == POLYGONKIND_UNKNOWN)
      {
        std::string message = "Unknown value for Spatial attribute 'polygonType' in 'parametricObject' object: " + stringValue;
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }
  if(mPolygonType == POLYGONKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'polygonType' is missing from 'parametricObject' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // domainType SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("domainType", mDomainType);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mDomainType.empty() == true)
    {
      logEmptyString(mDomainType, getLevel(), getVersion(), "<ParametricObject>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomainType) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute domainType='" + mDomainType + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domainType' is missing from 'parametricObject' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // pointIndexLength int   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetPointIndexLength = attributes.readInto("pointIndexLength", mPointIndexLength);

  if (mIsSetPointIndexLength == false)
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
        std::string message = "Spatial attribute 'pointIndexLength' is missing from 'parametricObject' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
      }
    }
  }

  //
  // compression enum  ( use = "required" )
  //
  mCompression = COMPRESSIONKIND_UNKNOWN;
  {
    std::string stringValue;
    assigned = attributes.readInto("compression", stringValue);

    if (assigned == true)
    {
      // parse enum

      mCompression = CompressionKind_parse(stringValue.c_str());
      if(mCompression == COMPRESSIONKIND_UNKNOWN)
      {
        std::string message = "Unknown value for Spatial attribute 'compression' in 'parametricObject' object: " + stringValue;
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }
  if(mCompression == COMPRESSIONKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'compression' is missing from 'parametricObject' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // dataType enum  ( use = "optional" )
  //
  mDataType = DATAKIND_UNKNOWN;
  {
    std::string stringValue;
    assigned = attributes.readInto("dataType", stringValue);

    if (assigned == true)
    {
      // parse enum

      mDataType = DataKind_parse(stringValue.c_str());
      if(mDataType == DATAKIND_UNKNOWN)
      {
        std::string message = "Unknown value for Spatial attribute 'dataType' in 'parametricObject' object: " + stringValue;
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
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
ParametricObject::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetPolygonType() == true)
    stream.writeAttribute("polygonType", getPrefix(), PolygonKind_toString(mPolygonType));

  if (isSetDomainType() == true)
    stream.writeAttribute("domainType", getPrefix(), mDomainType);

  if (isSetPointIndexLength() == true)
    stream.writeAttribute("pointIndexLength", getPrefix(), mPointIndexLength);

  if (isSetCompression() == true)
    stream.writeAttribute("compression", getPrefix(), CompressionKind_toString(mCompression));

  if (isSetDataType() == true)
    stream.writeAttribute("dataType", getPrefix(), DataKind_toString(mDataType));

}


  /** @endcond doxygenLibsbmlInternal */


void
ParametricObject::write(XMLOutputStream& stream) const
{
  stream.startElement(getElementName(), getPrefix());
  writeAttributes(stream);
  if(isSetPointIndex())
  {
    for (int i = 0; i < mPointIndexLength; ++i)
    {
      stream << (long)mPointIndex[i] << " ";
    }
  }
  stream.endElement(getElementName(), getPrefix());
}


void
ParametricObject::setElementText(const std::string &text)
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

    setPointIndex(data, length);
    delete[] data;
  }
}
/*
 * Constructor 
 */
ListOfParametricObjects::ListOfParametricObjects(unsigned int level, 
                          unsigned int version, 
                          unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfParametricObjects::ListOfParametricObjects(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfParametricObjects 
 */
ListOfParametricObjects* 
ListOfParametricObjects::clone () const
 {
  return new ListOfParametricObjects(*this);
}


/*
 * Get a ParametricObject from the ListOfParametricObjects by index.
*/
ParametricObject*
ListOfParametricObjects::get(unsigned int n)
{
  return static_cast<ParametricObject*>(ListOf::get(n));
}


/*
 * Get a ParametricObject from the ListOfParametricObjects by index.
 */
const ParametricObject*
ListOfParametricObjects::get(unsigned int n) const
{
  return static_cast<const ParametricObject*>(ListOf::get(n));
}


/*
 * Get a ParametricObject from the ListOfParametricObjects by id.
 */
ParametricObject*
ListOfParametricObjects::get(const std::string& sid)
{
	return const_cast<ParametricObject*>(
    static_cast<const ListOfParametricObjects&>(*this).get(sid));
}


/*
 * Get a ParametricObject from the ListOfParametricObjects by id.
 */
const ParametricObject*
ListOfParametricObjects::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<ParametricObject>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <ParametricObject*> (*result);
}


/**
 * Adds a copy the given "ParametricObject" to this ListOfParametricObjects.
 *
 * @param po; the ParametricObject object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfParametricObjects::addParametricObject(const ParametricObject* po)
{
  if (po == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (po->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != po->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != po->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(po)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(po);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of ParametricObject objects in this ListOfParametricObjects.
 *
 * @return the number of ParametricObject objects in this ListOfParametricObjects
 */
unsigned int 
ListOfParametricObjects::getNumParametricObjects() const
{
	return size();
}

/**
 * Creates a new ParametricObject object, adds it to this ListOfParametricObjects
 * ParametricObject and returns the ParametricObject object created. 
 *
 * @return a new ParametricObject object instance
 *
 * @see addParametricObject(const ParametricObject* po)
 */
ParametricObject* 
ListOfParametricObjects::createParametricObject()
{
  ParametricObject* po = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    po = new ParametricObject(spatialns);
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

  if(po != NULL)
  {
    appendAndOwn(po);
  }

  return po;
}

/*
 * Removes the nth ParametricObject from this ListOfParametricObjects
 */
ParametricObject*
ListOfParametricObjects::remove(unsigned int n)
{
  return static_cast<ParametricObject*>(ListOf::remove(n));
}


/*
 * Removes the ParametricObject from this ListOfParametricObjects with the given identifier
 */
ParametricObject*
ListOfParametricObjects::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<ParametricObject>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <ParametricObject*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfParametricObjects::getElementName () const
{
  static const string name = "listOfParametricObjects";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfParametricObjects::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfParametricObjects::getItemTypeCode () const
{
  return SBML_SPATIAL_PARAMETRICOBJECT;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new ParametricObject in this ListOfParametricObjects
 */
SBase*
ListOfParametricObjects::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "parametricObject")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new ParametricObject(spatialns);
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
ListOfParametricObjects::writeXMLNS(XMLOutputStream& stream) const
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
ParametricObject_t *
ParametricObject_create(unsigned int level, unsigned int version,
                        unsigned int pkgVersion)
{
  return new ParametricObject(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
ParametricObject_free(ParametricObject_t * po)
{
  if (po != NULL)
    delete po;
}


LIBSBML_EXTERN
ParametricObject_t *
ParametricObject_clone(ParametricObject_t * po)
{
  if (po != NULL)
  {
    return static_cast<ParametricObject_t*>(po->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
ParametricObject_getId(const ParametricObject_t * po)
{
	return (po != NULL && po->isSetId()) ? po->getId().c_str() : NULL;
}


LIBSBML_EXTERN
PolygonKind_t
ParametricObject_getPolygonType(const ParametricObject_t * po)
{
	return (po != NULL) ? po->getPolygonType() : POLYGONKIND_UNKNOWN;
}


LIBSBML_EXTERN
const char *
ParametricObject_getDomainType(const ParametricObject_t * po)
{
	return (po != NULL && po->isSetDomainType()) ? po->getDomainType().c_str() : NULL;
}


LIBSBML_EXTERN
int
ParametricObject_getPointIndexLength(const ParametricObject_t * po)
{
	return (po != NULL) ? po->getPointIndexLength() : SBML_INT_MAX;
}


LIBSBML_EXTERN
CompressionKind_t
ParametricObject_getCompression(const ParametricObject_t * po)
{
	return (po != NULL) ? po->getCompression() : COMPRESSIONKIND_UNKNOWN;
}


LIBSBML_EXTERN
DataKind_t
ParametricObject_getDataType(const ParametricObject_t * po)
{
	return (po != NULL) ? po->getDataType() : DATAKIND_UNKNOWN;
}


LIBSBML_EXTERN
int
ParametricObject_isSetId(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetId()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_isSetPolygonType(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetPolygonType()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_isSetDomainType(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetDomainType()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_isSetPointIndex(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetPointIndex()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_isSetPointIndexLength(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetPointIndexLength()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_isSetCompression(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetCompression()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_isSetDataType(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetDataType()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_setId(ParametricObject_t * po, const char * id)
{
  if (po != NULL)
    return (id == NULL) ? po->setId("") : po->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_setPolygonType(ParametricObject_t * po, PolygonKind_t polygonType)
{
  if (po != NULL)
    return po->setPolygonType(polygonType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_setDomainType(ParametricObject_t * po, const char * domainType)
{
  if (po != NULL)
    return (domainType == NULL) ? po->setDomainType("") : po->setDomainType(domainType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_setPointIndexLength(ParametricObject_t * po, int pointIndexLength)
{
  if (po != NULL)
    return po->setPointIndexLength(pointIndexLength);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_setCompression(ParametricObject_t * po, CompressionKind_t compression)
{
  if (po != NULL)
    return po->setCompression(compression);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_setDataType(ParametricObject_t * po, DataKind_t dataType)
{
  if (po != NULL)
    return po->setDataType(dataType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_unsetId(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_unsetPolygonType(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetPolygonType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_unsetDomainType(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetDomainType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_unsetPointIndex(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetPointIndex() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_unsetPointIndexLength(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetPointIndexLength() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_unsetCompression(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetCompression() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_unsetDataType(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetDataType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_hasRequiredAttributes(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_hasRequiredElements(const ParametricObject_t * po)
{
	return (po != NULL) ? static_cast<int>(po->hasRequiredElements()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
ParametricObject_t *
ListOfParametricObjects_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfParametricObjects *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
ParametricObject_t *
ListOfParametricObjects_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfParametricObjects *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


