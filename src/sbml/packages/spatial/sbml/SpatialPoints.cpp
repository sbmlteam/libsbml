/**
 * @file:   SpatialPoints.cpp
 * @brief:  Implementation of the SpatialPoints class
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


#include <sbml/packages/spatial/sbml/SpatialPoints.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpatialPoints with the given level, version, and package version.
 */
SpatialPoints::SpatialPoints (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mCompression (COMPRESSIONKIND_UNKNOWN)
  , mArrayData (NULL)
  , mArrayDataLength (SBML_INT_MAX)
  , mIsSetArrayDataLength (false)
  , mDataType (DATAKIND_UNKNOWN)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new SpatialPoints with the given SpatialPkgNamespaces object.
 */
SpatialPoints::SpatialPoints (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mCompression (COMPRESSIONKIND_UNKNOWN)
  , mArrayData (NULL)
  , mArrayDataLength (SBML_INT_MAX)
  , mIsSetArrayDataLength (false)
  , mDataType (DATAKIND_UNKNOWN)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for SpatialPoints.
 */
SpatialPoints::SpatialPoints (const SpatialPoints& orig)
  : SBase(orig)
  , mId  ( orig.mId)
  , mCompression  ( orig.mCompression)
  , mArrayData  ( NULL)
  , mArrayDataLength  ( orig.mArrayDataLength)
  , mIsSetArrayDataLength  ( orig.mIsSetArrayDataLength)
  , mDataType  ( orig.mDataType)
{
  setArrayData(orig.mArrayData, orig.mArrayDataLength);
}


/*
 * Assignment for SpatialPoints.
 */
SpatialPoints&
SpatialPoints::operator=(const SpatialPoints& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mCompression  = rhs.mCompression;
    mArrayData  = NULL;
    setArrayData(rhs.mArrayData, rhs.mArrayDataLength);
    mArrayDataLength  = rhs.mArrayDataLength;
    mIsSetArrayDataLength  = rhs.mIsSetArrayDataLength;
    mDataType  = rhs.mDataType;
  }
  return *this;
}


/*
 * Clone for SpatialPoints.
 */
SpatialPoints*
SpatialPoints::clone () const
{
  return new SpatialPoints(*this);
}


/*
 * Destructor for SpatialPoints.
 */
SpatialPoints::~SpatialPoints ()
{
  if (mArrayData != NULL)
    delete[] mArrayData;
  mArrayData = NULL;
}


/*
 * Returns the value of the "id" attribute of this SpatialPoints.
 */
const std::string&
SpatialPoints::getId() const
{
  return mId;
}


/*
 * Returns the value of the "compression" attribute of this SpatialPoints.
 */
CompressionKind_t
SpatialPoints::getCompression() const
{
  return mCompression;
}


/*
 * The "arrayData" attribute of this SpatialPoints is returned in an double* array (pointer)
 * that is passed as argument to the method (this is needed while using SWIG to
 * convert int[] from C++ to Java). The method itself has a return type void.
 *
 * NOTE: you have to pre-allocate the array with the correct length! *
 * @return void.
 */
void
SpatialPoints::getArrayData(double* outArray) const
{
   if (outArray == NULL || mArrayData == NULL) return;

   memcpy(outArray , mArrayData, sizeof(double)*mArrayDataLength);
}


/*
 * Returns the value of the "arrayDataLength" attribute of this SpatialPoints.
 */
int
SpatialPoints::getArrayDataLength() const
{
  return mArrayDataLength;
}


/*
 * Returns the value of the "dataType" attribute of this SpatialPoints.
 */
DataKind_t
SpatialPoints::getDataType() const
{
  return mDataType;
}


/*
 * Returns true/false if id is set.
 */
bool
SpatialPoints::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if compression is set.
 */
bool
SpatialPoints::isSetCompression() const
{
  return mCompression != COMPRESSIONKIND_UNKNOWN;
}


/*
 * Returns true/false if arrayData is set.
 */
bool
SpatialPoints::isSetArrayData() const
{
  return (mArrayData != NULL);
}


/*
 * Returns true/false if arrayDataLength is set.
 */
bool
SpatialPoints::isSetArrayDataLength() const
{
  return mIsSetArrayDataLength;
}


/*
 * Returns true/false if dataType is set.
 */
bool
SpatialPoints::isSetDataType() const
{
  return mDataType != DATAKIND_UNKNOWN;
}


/*
 * Sets id and returns value indicating success.
 */
int
SpatialPoints::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets compression and returns value indicating success.
 */
int
SpatialPoints::setCompression(CompressionKind_t compression)
{
  mCompression = compression;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets compression and returns value indicating success.
 */
int
SpatialPoints::setCompression(const std::string& compression)
{
  CompressionKind_t parsed = CompressionKind_parse(compression.c_str());
  if (parsed == COMPRESSIONKIND_UNKNOWN) return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  mCompression = parsed;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the "arrayData" element of this SpatialPoints.
 *
 * @param inArray; double* array to be set (it will be copied).
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
SpatialPoints::setArrayData(double* inArray, int arrayLength)
{
  if (inArray == NULL) return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  if (mArrayData != NULL) delete[] mArrayData;
  mArrayData = new double[arrayLength];
  memcpy(mArrayData, inArray, sizeof(double)*arrayLength);
  mIsSetArrayDataLength = true;
  mArrayDataLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}
/*
 * Sets arrayDataLength and returns value indicating success.
 */
int
SpatialPoints::setArrayDataLength(int arrayDataLength)
{
  mArrayDataLength = arrayDataLength;
  mIsSetArrayDataLength = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets dataType and returns value indicating success.
 */
int
SpatialPoints::setDataType(DataKind_t dataType)
{
  mDataType = dataType;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets dataType and returns value indicating success.
 */
int
SpatialPoints::setDataType(const std::string& dataType)
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
SpatialPoints::unsetId()
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
 * Unsets compression and returns value indicating success.
 */
int
SpatialPoints::unsetCompression()
{
  mCompression = COMPRESSIONKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets arrayData and returns value indicating success.
 */
int
SpatialPoints::unsetArrayData()
{
  if (mArrayData != NULL)
   delete[] mArrayData;
  mArrayData = NULL;
  return unsetArrayDataLength();
}


/*
 * Unsets arrayDataLength and returns value indicating success.
 */
int
SpatialPoints::unsetArrayDataLength()
{
  mArrayDataLength = SBML_INT_MAX;
  mIsSetArrayDataLength = false;

  if (isSetArrayDataLength() == false)
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
SpatialPoints::unsetDataType()
{
  mDataType = DATAKIND_UNKNOWN;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
SpatialPoints::getElementName () const
{
  static const string name = "spatialPoints";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SpatialPoints::getTypeCode () const
{
  return SBML_SPATIAL_SPATIALPOINTS;
}


/*
 * check if all the required attributes are set
 */
bool
SpatialPoints::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetCompression() == false)
    allPresent = false;

  if (isSetArrayData() == false)
    allPresent = false;

  if (isSetArrayDataLength() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
SpatialPoints::writeElements (XMLOutputStream& stream) const
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
SpatialPoints::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
SpatialPoints::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SpatialPoints::enablePackageInternal(const std::string& pkgURI,
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
SpatialPoints::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("compression");
  attributes.add("arrayDataLength");
  attributes.add("dataType");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpatialPoints::readAttributes (const XMLAttributes& attributes,
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
      logEmptyString(mId, getLevel(), getVersion(), "<SpatialPoints>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'spatialPoints' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
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
        std::string message = "Unknown value for Spatial attribute 'compression' in 'spatialPoints' object: " + stringValue;
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }
  if(mCompression == COMPRESSIONKIND_UNKNOWN)
  {
    std::string message = "Spatial attribute 'compression' is missing from 'spatialPoints' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // arrayDataLength int   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetArrayDataLength = attributes.readInto("arrayDataLength", mArrayDataLength);

  if (mIsSetArrayDataLength == false)
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
        std::string message = "Spatial attribute 'arrayDataLength' is missing from 'spatialPoints' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
      }
    }
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
        std::string message = "Unknown value for Spatial attribute 'dataType' in 'spatialPoints' object: " + stringValue;
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
SpatialPoints::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetCompression() == true)
    stream.writeAttribute("compression", getPrefix(), CompressionKind_toString(mCompression));

  if (isSetArrayDataLength() == true)
    stream.writeAttribute("arrayDataLength", getPrefix(), mArrayDataLength);

  if (isSetDataType() == true)
    stream.writeAttribute("dataType", getPrefix(), DataKind_toString(mDataType));

}


  /** @endcond doxygenLibsbmlInternal */


void
SpatialPoints::write(XMLOutputStream& stream) const
{
  stream.startElement(getElementName(), getPrefix());
  writeAttributes(stream);
  if(isSetArrayData())
  {
    for (int i = 0; i < mArrayDataLength; ++i)
    {
      stream << (double)mArrayData[i] << " ";
    }
  }
  stream.endElement(getElementName(), getPrefix());
}


void
SpatialPoints::setElementText(const std::string &text)
{
  stringstream strStream(text); // Insert the string into a stream
  double val;
  vector<double> valuesVector;
  while (strStream >> val)
  {
    valuesVector.push_back(val);
  }

  // convert the vector to an array
  unsigned int length = (unsigned int)valuesVector.size();
  if (length > 0)
  {

    double* data = new double[length];
    for (unsigned int i = 0; i < length; ++i)
    {
      data[i] = valuesVector.at(i);
    }

    setArrayData(data, length);
    delete[] data;
  }
}
LIBSBML_EXTERN
SpatialPoints_t *
SpatialPoints_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion)
{
  return new SpatialPoints(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SpatialPoints_free(SpatialPoints_t * sp)
{
  if (sp != NULL)
    delete sp;
}


LIBSBML_EXTERN
SpatialPoints_t *
SpatialPoints_clone(SpatialPoints_t * sp)
{
  if (sp != NULL)
  {
    return static_cast<SpatialPoints_t*>(sp->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
SpatialPoints_getId(const SpatialPoints_t * sp)
{
	return (sp != NULL && sp->isSetId()) ? sp->getId().c_str() : NULL;
}


LIBSBML_EXTERN
CompressionKind_t
SpatialPoints_getCompression(const SpatialPoints_t * sp)
{
	return (sp != NULL) ? sp->getCompression() : COMPRESSIONKIND_UNKNOWN;
}


LIBSBML_EXTERN
int
SpatialPoints_getArrayDataLength(const SpatialPoints_t * sp)
{
	return (sp != NULL) ? sp->getArrayDataLength() : SBML_INT_MAX;
}


LIBSBML_EXTERN
DataKind_t
SpatialPoints_getDataType(const SpatialPoints_t * sp)
{
	return (sp != NULL) ? sp->getDataType() : DATAKIND_UNKNOWN;
}


LIBSBML_EXTERN
int
SpatialPoints_isSetId(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetId()) : 0;
}


LIBSBML_EXTERN
int
SpatialPoints_isSetCompression(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetCompression()) : 0;
}


LIBSBML_EXTERN
int
SpatialPoints_isSetArrayData(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetArrayData()) : 0;
}


LIBSBML_EXTERN
int
SpatialPoints_isSetArrayDataLength(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetArrayDataLength()) : 0;
}


LIBSBML_EXTERN
int
SpatialPoints_isSetDataType(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetDataType()) : 0;
}


LIBSBML_EXTERN
int
SpatialPoints_setId(SpatialPoints_t * sp, const char * id)
{
  if (sp != NULL)
    return (id == NULL) ? sp->setId("") : sp->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoints_setCompression(SpatialPoints_t * sp, CompressionKind_t compression)
{
  if (sp != NULL)
    return sp->setCompression(compression);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoints_setArrayDataLength(SpatialPoints_t * sp, int arrayDataLength)
{
  if (sp != NULL)
    return sp->setArrayDataLength(arrayDataLength);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoints_setDataType(SpatialPoints_t * sp, DataKind_t dataType)
{
  if (sp != NULL)
    return sp->setDataType(dataType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoints_unsetId(SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoints_unsetCompression(SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->unsetCompression() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoints_unsetArrayData(SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->unsetArrayData() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoints_unsetArrayDataLength(SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->unsetArrayDataLength() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoints_unsetDataType(SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->unsetDataType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoints_hasRequiredAttributes(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


