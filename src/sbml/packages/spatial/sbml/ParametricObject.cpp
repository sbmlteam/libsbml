/**
 * @file ParametricObject.cpp
 * @brief Implementation of the ParametricObject class.
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
#include <sbml/packages/spatial/sbml/ParametricObject.h>
#include <sbml/packages/spatial/sbml/ListOfParametricObjects.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ParametricObject using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
ParametricObject::ParametricObject(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion)
  : SBase(level, version)
  , mPolygonType (SPATIAL_POLYGONKIND_INVALID)
  , mDomainType ("")
  , mPointIndex (NULL)
  , mPointIndexLength (SBML_INT_MAX)
  , mIsSetPointIndexLength (false)
  , mCompression (SPATIAL_COMPRESSIONKIND_INVALID)
  , mDataType (SPATIAL_DATAKIND_INVALID)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new ParametricObject using the given SpatialPkgNamespaces object.
 */
ParametricObject::ParametricObject(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mPolygonType (SPATIAL_POLYGONKIND_INVALID)
  , mDomainType ("")
  , mPointIndex (NULL)
  , mPointIndexLength (SBML_INT_MAX)
  , mIsSetPointIndexLength (false)
  , mCompression (SPATIAL_COMPRESSIONKIND_INVALID)
  , mDataType (SPATIAL_DATAKIND_INVALID)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for ParametricObject.
 */
ParametricObject::ParametricObject(const ParametricObject& orig)
  : SBase( orig )
  , mPolygonType ( orig.mPolygonType )
  , mDomainType ( orig.mDomainType )
  , mPointIndex ( NULL )
  , mPointIndexLength ( orig.mPointIndexLength )
  , mIsSetPointIndexLength ( orig.mIsSetPointIndexLength )
  , mCompression ( orig.mCompression )
  , mDataType ( orig.mDataType )
{
  setPointIndex(orig.mPointIndex, orig.mPointIndexLength);

}


/*
 * Assignment operator for ParametricObject.
 */
ParametricObject&
ParametricObject::operator=(const ParametricObject& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mPolygonType = rhs.mPolygonType;
    mDomainType = rhs.mDomainType;
    mPointIndex = NULL;
    setPointIndex(rhs.mPointIndex, rhs.mPointIndexLength);
    mPointIndexLength = rhs.mPointIndexLength;
    mIsSetPointIndexLength = rhs.mIsSetPointIndexLength;
    mCompression = rhs.mCompression;
    mDataType = rhs.mDataType;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ParametricObject object.
 */
ParametricObject*
ParametricObject::clone() const
{
  return new ParametricObject(*this);
}


/*
 * Destructor for ParametricObject.
 */
ParametricObject::~ParametricObject()
{
  if (mPointIndex != NULL)
  {
    delete [] mPointIndex;
  }

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
 * Returns the value of the "name" attribute of this ParametricObject.
 */
const std::string&
ParametricObject::getName() const
{
  return mName;
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
 * Returns the value of the "polygonType" attribute of this ParametricObject.
 */
std::string
ParametricObject::getPolygonTypeAsString() const
{
  std::string code_str = PolygonKind_toString(mPolygonType);
  return code_str;
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
 * Returns the value of the "pointIndex" attribute of this ParametricObject.
 */
void
ParametricObject::getPointIndex(int* outArray) const
{
  if (outArray == NULL || mPointIndex == NULL)
  {
    return;
  }

  memcpy(outArray, mPointIndex, sizeof(int)*mPointIndexLength);
}


/*
 * Returns the value of the "pointIndexLength" attribute of this
 * ParametricObject.
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
 * Returns the value of the "compression" attribute of this ParametricObject.
 */
const std::string&
ParametricObject::getCompressionAsString() const
{
  static const std::string code_str = CompressionKind_toString(mCompression);
  return code_str;
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
 * Returns the value of the "dataType" attribute of this ParametricObject.
 */
std::string
ParametricObject::getDataTypeAsString() const
{
  std::string code_str = DataKind_toString(mDataType);
  return code_str;
}


/*
 * Predicate returning @c true if this ParametricObject's "id" attribute is
 * set.
 */
bool
ParametricObject::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this ParametricObject's "name" attribute is
 * set.
 */
bool
ParametricObject::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this ParametricObject's "polygonType"
 * attribute is set.
 */
bool
ParametricObject::isSetPolygonType() const
{
  return (mPolygonType != SPATIAL_POLYGONKIND_INVALID);
}


/*
 * Predicate returning @c true if this ParametricObject's "domainType"
 * attribute is set.
 */
bool
ParametricObject::isSetDomainType() const
{
  return (mDomainType.empty() == false);
}


/*
 * Predicate returning @c true if this ParametricObject's "pointIndex"
 * attribute is set.
 */
bool
ParametricObject::isSetPointIndex() const
{
  return (mPointIndex != NULL);
}


/*
 * Predicate returning @c true if this ParametricObject's "pointIndexLength"
 * attribute is set.
 */
bool
ParametricObject::isSetPointIndexLength() const
{
  return mIsSetPointIndexLength;
}


/*
 * Predicate returning @c true if this ParametricObject's "compression"
 * attribute is set.
 */
bool
ParametricObject::isSetCompression() const
{
  return (mCompression != SPATIAL_COMPRESSIONKIND_INVALID);
}


/*
 * Predicate returning @c true if this ParametricObject's "dataType" attribute
 * is set.
 */
bool
ParametricObject::isSetDataType() const
{
  return (mDataType != SPATIAL_DATAKIND_INVALID);
}


/*
 * Sets the value of the "id" attribute of this ParametricObject.
 */
int
ParametricObject::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this ParametricObject.
 */
int
ParametricObject::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "polygonType" attribute of this ParametricObject.
 */
int
ParametricObject::setPolygonType(const PolygonKind_t polygonType)
{
  if (PolygonKind_isValid(polygonType) == 0)
  {
    mPolygonType = SPATIAL_POLYGONKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mPolygonType = polygonType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "polygonType" attribute of this ParametricObject.
 */
int
ParametricObject::setPolygonType(const std::string& polygonType)
{
  if (PolygonKind_isValidString(polygonType.c_str()) == 0)
  {
    mPolygonType = SPATIAL_POLYGONKIND_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mPolygonType = PolygonKind_fromString(polygonType.c_str());
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "domainType" attribute of this ParametricObject.
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
 * Sets the value of the "pointIndex" attribute of this ParametricObject.
 */
int
ParametricObject::setPointIndex(int* inArray, int arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  if (mPointIndex != NULL)
  {
    delete[] mPointIndex;
  }

  mPointIndex = new int[arrayLength];
  memcpy(mPointIndex, inArray, sizeof(int)*arrayLength);
  mIsSetPointIndexLength = true;
  mPointIndexLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "pointIndexLength" attribute of this ParametricObject.
 */
int
ParametricObject::setPointIndexLength(int pointIndexLength)
{
  mPointIndexLength = pointIndexLength;
  mIsSetPointIndexLength = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "compression" attribute of this ParametricObject.
 */
int
ParametricObject::setCompression(const CompressionKind_t compression)
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
 * Sets the value of the "compression" attribute of this ParametricObject.
 */
int
ParametricObject::setCompression(const std::string& compression)
{
  mCompression = CompressionKind_fromString(compression.c_str());

  if (mCompression == SPATIAL_COMPRESSIONKIND_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "dataType" attribute of this ParametricObject.
 */
int
ParametricObject::setDataType(const DataKind_t dataType)
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
 * Sets the value of the "dataType" attribute of this ParametricObject.
 */
int
ParametricObject::setDataType(const std::string& dataType)
{
  mDataType = DataKind_fromString(dataType.c_str());

  if (mDataType == SPATIAL_DATAKIND_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this ParametricObject.
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
 * Unsets the value of the "name" attribute of this ParametricObject.
 */
int
ParametricObject::unsetName()
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
 * Unsets the value of the "polygonType" attribute of this ParametricObject.
 */
int
ParametricObject::unsetPolygonType()
{
  mPolygonType = SPATIAL_POLYGONKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "domainType" attribute of this ParametricObject.
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
 * Unsets the value of the "pointIndex" attribute of this ParametricObject.
 */
int
ParametricObject::unsetPointIndex()
{
  if (mPointIndex != NULL)
  {
    delete[] mPointIndex;
  }

  mPointIndex = NULL;

  return unsetPointIndexLength();
}


/*
 * Unsets the value of the "pointIndexLength" attribute of this
 * ParametricObject.
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
 * Unsets the value of the "compression" attribute of this ParametricObject.
 */
int
ParametricObject::unsetCompression()
{
  mCompression = SPATIAL_COMPRESSIONKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "dataType" attribute of this ParametricObject.
 */
int
ParametricObject::unsetDataType()
{
  mDataType = SPATIAL_DATAKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * @copydoc doc_renamesidref_common
 */
void
ParametricObject::renameSIdRefs(const std::string& oldid,
                                const std::string& newid)
{
  if (isSetDomainType() && mDomainType == oldid)
  {
    setDomainType(newid);
  }
}


/*
 * Returns the XML element name of this ParametricObject object.
 */
const std::string&
ParametricObject::getElementName() const
{
  static const string name = "parametricObject";
  return name;
}


/*
 * Returns the libSBML type code for this ParametricObject object.
 */
int
ParametricObject::getTypeCode() const
{
  return SBML_SPATIAL_PARAMETRICOBJECT;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * ParametricObject object have been set.
 */
bool
ParametricObject::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetId() == false)
  {
    allPresent = false;
  }

  if (isSetPolygonType() == false)
  {
    allPresent = false;
  }

  if (isSetDomainType() == false)
  {
    allPresent = false;
  }

  if (isSetPointIndex() == false)
  {
    allPresent = false;
  }

  if (isSetPointIndexLength() == false)
  {
    allPresent = false;
  }

  if (isSetCompression() == false)
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
ParametricObject::writeElements(XMLOutputStream& stream) const
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
ParametricObject::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
ParametricObject::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * used to write arrays
 */
void
ParametricObject::write(XMLOutputStream& stream) const
{
  stream.startElement(getElementName(), getPrefix());
  writeAttributes(stream);

  if (isSetPointIndex())
  {
    for (int i = 0; i < mPointIndexLength; ++i)
    {
      stream << (long)mPointIndex[i] << " ";
    }
  }

  stream.endElement(getElementName(), getPrefix());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
ParametricObject::enablePackageInternal(const std::string& pkgURI,
                                        const std::string& pkgPrefix,
                                        bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ParametricObject.
 */
int
ParametricObject::getAttribute(const std::string& attributeName,
                               bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ParametricObject.
 */
int
ParametricObject::getAttribute(const std::string& attributeName,
                               int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "pointIndexLength")
  {
    value = getPointIndexLength();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ParametricObject.
 */
int
ParametricObject::getAttribute(const std::string& attributeName,
                               double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ParametricObject.
 */
int
ParametricObject::getAttribute(const std::string& attributeName,
                               unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ParametricObject.
 */
int
ParametricObject::getAttribute(const std::string& attributeName,
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
  else if (attributeName == "polygonType")
  {
    value = getPolygonTypeAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "domainType")
  {
    value = getDomainType();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "compression")
  {
    value = getCompressionAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "dataType")
  {
    value = getDataTypeAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this ParametricObject's attribute
 * "attributeName" is set.
 */
bool
ParametricObject::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "polygonType")
  {
    value = isSetPolygonType();
  }
  else if (attributeName == "domainType")
  {
    value = isSetDomainType();
  }
  else if (attributeName == "pointIndex")
  {
    value = isSetPointIndex();
  }
  else if (attributeName == "pointIndexLength")
  {
    value = isSetPointIndexLength();
  }
  else if (attributeName == "compression")
  {
    value = isSetCompression();
  }
  else if (attributeName == "dataType")
  {
    value = isSetDataType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ParametricObject.
 */
int
ParametricObject::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ParametricObject.
 */
int
ParametricObject::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "pointIndexLength")
  {
    return_value = setPointIndexLength(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ParametricObject.
 */
int
ParametricObject::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ParametricObject.
 */
int
ParametricObject::setAttribute(const std::string& attributeName,
                               unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ParametricObject.
 */
int
ParametricObject::setAttribute(const std::string& attributeName,
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
  else if (attributeName == "polygonType")
  {
    return_value = setPolygonType(value);
  }
  else if (attributeName == "domainType")
  {
    return_value = setDomainType(value);
  }
  else if (attributeName == "compression")
  {
    return_value = setCompression(value);
  }
  else if (attributeName == "dataType")
  {
    return_value = setDataType(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this ParametricObject.
 */
int
ParametricObject::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "polygonType")
  {
    value = unsetPolygonType();
  }
  else if (attributeName == "domainType")
  {
    value = unsetDomainType();
  }
  else if (attributeName == "pointIndex")
  {
    value = unsetPointIndex();
  }
  else if (attributeName == "pointIndexLength")
  {
    value = unsetPointIndexLength();
  }
  else if (attributeName == "compression")
  {
    value = unsetCompression();
  }
  else if (attributeName == "dataType")
  {
    value = unsetDataType();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
ParametricObject::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("polygonType");

  attributes.add("domainType");

  attributes.add("pointIndexLength");

  attributes.add("compression");

  attributes.add("dataType");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
ParametricObject::readAttributes(const XMLAttributes& attributes,
                                 const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfParametricObjects*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial",
          SpatialParametricObjectAllowedAttributes, pkgVersion, level, version,
            details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialParametricGeometryLOParametricObjectsAllowedCoreAttributes,
            pkgVersion, level, version, details);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial",
          SpatialParametricObjectAllowedAttributes, pkgVersion, level, version,
            details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialParametricObjectAllowedCoreAttributes, pkgVersion, level,
            version, details);
      }
    }
  }

  // 
  // id SId (use = "required" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<ParametricObject>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("spatial", SpatialIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from the "
      "<ParametricObject> element.";
    log->logPackageError("spatial", SpatialParametricObjectAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<ParametricObject>");
    }
  }

  // 
  // polygonType enum (use = "required" )
  // 

  std::string polygonType;
  assigned = attributes.readInto("polygonType", polygonType);

  if (assigned == true)
  {
    if (polygonType.empty() == true)
    {
      logEmptyString(polygonType, level, version, "<ParametricObject>");
    }
    else
    {
      mPolygonType = PolygonKind_fromString(polygonType.c_str());

      if (PolygonKind_isValid(mPolygonType) == 0)
      {
        std::string msg = "The polygonType on the <ParametricObject> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + polygonType + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialParametricObjectPolygonTypeMustBePolygonKindEnum, pkgVersion,
            level, version, msg);
      }
    }
  }
  else
  {
    std::string message = "Spatial attribute 'polygonType' is missing.";
    log->logPackageError("spatial", SpatialParametricObjectAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // domainType SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("domainType", mDomainType);

  if (assigned == true)
  {
    if (mDomainType.empty() == true)
    {
      logEmptyString(mDomainType, level, version, "<ParametricObject>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomainType) == false)
    {
      std::string msg = "The domainType attribute on the <" + getElementName()
        + ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mDomainType + "', which does not conform to the "
        "syntax.";
      log->logPackageError("spatial",
        SpatialParametricObjectDomainTypeMustBeDomainType, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domainType' is missing from the "
      "<ParametricObject> element.";
    log->logPackageError("spatial", SpatialParametricObjectAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // pointIndexLength int (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetPointIndexLength = attributes.readInto("pointIndexLength",
    mPointIndexLength);

  if ( mIsSetPointIndexLength == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'pointIndexLength' from the "
        "<ParametricObject> element must be an integer.";
      log->logPackageError("spatial",
        SpatialParametricObjectPointIndexLengthMustBeInteger, pkgVersion, level,
          version, message);
    }
    else
    {
      std::string message = "Spatial attribute 'pointIndexLength' is missing "
        "from the <ParametricObject> element.";
      log->logPackageError("spatial", SpatialParametricObjectAllowedAttributes,
        pkgVersion, level, version, message);
    }
  }

  // 
  // compression enum (use = "required" )
  // 

  std::string compression;
  assigned = attributes.readInto("compression", compression);

  if (assigned == true)
  {
    if (compression.empty() == true)
    {
      logEmptyString(compression, level, version, "<ParametricObject>");
    }
    else
    {
      mCompression = CompressionKind_fromString(compression.c_str());

      if (CompressionKind_isValid(mCompression) == 0)
      {
        std::string msg = "The compression on the <ParametricObject> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + compression + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialParametricObjectCompressionMustBeCompressionKindEnum,
            pkgVersion, level, version, msg);
      }
    }
  }
  else
  {
    std::string message = "Spatial attribute 'compression' is missing.";
    log->logPackageError("spatial", SpatialParametricObjectAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // dataType enum (use = "optional" )
  // 

  std::string dataType;
  assigned = attributes.readInto("dataType", dataType);

  if (assigned == true)
  {
    if (dataType.empty() == true)
    {
      logEmptyString(dataType, level, version, "<ParametricObject>");
    }
    else
    {
      mDataType = DataKind_fromString(dataType.c_str());

      if (DataKind_isValid(mDataType) == 0)
      {
        std::string msg = "The dataType on the <ParametricObject> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + dataType + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialParametricObjectDataTypeMustBeDataKindEnum, pkgVersion, level,
            version, msg);
      }
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
ParametricObject::writeAttributes(XMLOutputStream& stream) const
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

  if (isSetPolygonType() == true)
  {
    stream.writeAttribute("polygonType", getPrefix(),
      PolygonKind_toString(mPolygonType));
  }

  if (isSetDomainType() == true)
  {
    stream.writeAttribute("domainType", getPrefix(), mDomainType);
  }

  if (isSetPointIndexLength() == true)
  {
    stream.writeAttribute("pointIndexLength", getPrefix(), mPointIndexLength);
  }

  if (isSetCompression() == true)
  {
    stream.writeAttribute("compression", getPrefix(),
      CompressionKind_toString(mCompression));
  }

  if (isSetDataType() == true)
  {
    stream.writeAttribute("dataType", getPrefix(),
      DataKind_toString(mDataType));
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the array data as a text element
 */
void
ParametricObject::setElementText(const std::string& text)
{
  stringstream strStream(text);
  int val;
  vector<int> valuesVector;

  while (strStream >> val)
  {
    valuesVector.push_back(val);
  }

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

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new ParametricObject_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
ParametricObject_t *
ParametricObject_create(unsigned int level,
                        unsigned int version,
                        unsigned int pkgVersion)
{
  return new ParametricObject(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this ParametricObject_t object.
 */
LIBSBML_EXTERN
ParametricObject_t*
ParametricObject_clone(const ParametricObject_t* po)
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


/*
 * Frees this ParametricObject_t object.
 */
LIBSBML_EXTERN
void
ParametricObject_free(ParametricObject_t* po)
{
  if (po != NULL)
  {
    delete po;
  }
}


/*
 * Returns the value of the "id" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
char *
ParametricObject_getId(const ParametricObject_t * po)
{
  if (po == NULL)
  {
    return NULL;
  }

  return po->getId().empty() ? NULL : safe_strdup(po->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
char *
ParametricObject_getName(const ParametricObject_t * po)
{
  if (po == NULL)
  {
    return NULL;
  }

  return po->getName().empty() ? NULL : safe_strdup(po->getName().c_str());
}


/*
 * Returns the value of the "polygonType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
PolygonKind_t
ParametricObject_getPolygonType(const ParametricObject_t * po)
{
  if (po == NULL)
  {
    return SPATIAL_POLYGONKIND_INVALID;
  }

  return po->getPolygonType();
}


/*
 * Returns the value of the "polygonType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
char *
ParametricObject_getPolygonTypeAsString(const ParametricObject_t * po)
{
  return (char*)(PolygonKind_toString(po->getPolygonType()));
}


/*
 * Returns the value of the "domainType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
char *
ParametricObject_getDomainType(const ParametricObject_t * po)
{
  if (po == NULL)
  {
    return NULL;
  }

  return po->getDomainType().empty() ? NULL :
    safe_strdup(po->getDomainType().c_str());
}


/*
 * Returns the value of the "pointIndexLength" attribute of this
 * ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_getPointIndexLength(const ParametricObject_t * po)
{
  return (po != NULL) ? po->getPointIndexLength() : SBML_INT_MAX;
}


/*
 * Returns the value of the "compression" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
CompressionKind_t
ParametricObject_getCompression(const ParametricObject_t * po)
{
  if (po == NULL)
  {
    return SPATIAL_COMPRESSIONKIND_INVALID;
  }

  return po->getCompression();
}


/*
 * Returns the value of the "compression" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
char *
ParametricObject_getCompressionAsString(const ParametricObject_t * po)
{
  return (char*)(CompressionKind_toString(po->getCompression()));
}


/*
 * Returns the value of the "dataType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
DataKind_t
ParametricObject_getDataType(const ParametricObject_t * po)
{
  if (po == NULL)
  {
    return SPATIAL_DATAKIND_INVALID;
  }

  return po->getDataType();
}


/*
 * Returns the value of the "dataType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
char *
ParametricObject_getDataTypeAsString(const ParametricObject_t * po)
{
  return (char*)(DataKind_toString(po->getDataType()));
}


/*
 * Predicate returning @c 1 (true) if this ParametricObject_t's "id" attribute
 * is set.
 */
LIBSBML_EXTERN
int
ParametricObject_isSetId(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ParametricObject_t's "name"
 * attribute is set.
 */
LIBSBML_EXTERN
int
ParametricObject_isSetName(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ParametricObject_t's "polygonType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
ParametricObject_isSetPolygonType(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetPolygonType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ParametricObject_t's "domainType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
ParametricObject_isSetDomainType(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetDomainType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ParametricObject_t's "pointIndex"
 * attribute is set.
 */
LIBSBML_EXTERN
int
ParametricObject_isSetPointIndex(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetPointIndex()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ParametricObject_t's
 * "pointIndexLength" attribute is set.
 */
LIBSBML_EXTERN
int
ParametricObject_isSetPointIndexLength(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetPointIndexLength()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ParametricObject_t's "compression"
 * attribute is set.
 */
LIBSBML_EXTERN
int
ParametricObject_isSetCompression(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetCompression()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this ParametricObject_t's "dataType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
ParametricObject_isSetDataType(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetDataType()) : 0;
}


/*
 * Sets the value of the "id" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_setId(ParametricObject_t * po, const char * id)
{
  return (po != NULL) ? po->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_setName(ParametricObject_t * po, const char * name)
{
  return (po != NULL) ? po->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "polygonType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_setPolygonType(ParametricObject_t * po,
                                PolygonKind_t polygonType)
{
  return (po != NULL) ? po->setPolygonType(polygonType) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "polygonType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_setPolygonTypeAsString(ParametricObject_t * po,
                                        const char * polygonType)
{
  return (po != NULL) ? po->setPolygonType(polygonType):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "domainType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_setDomainType(ParametricObject_t * po,
                               const char * domainType)
{
  return (po != NULL) ? po->setDomainType(domainType) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "pointIndex" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_setPointIndex(ParametricObject_t* po,
                               int* pointIndex,
                               int arrayLength)
{
  return (po != NULL) ? po->setPointIndex(pointIndex, arrayLength) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "pointIndexLength" attribute of this
 * ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_setPointIndexLength(ParametricObject_t * po,
                                     int pointIndexLength)
{
  return (po != NULL) ? po->setPointIndexLength(pointIndexLength) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "compression" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_setCompression(ParametricObject_t * po,
                                CompressionKind_t compression)
{
  return (po != NULL) ? po->setCompression(compression) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "compression" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_setCompressionAsString(ParametricObject_t * po,
                                        const char * compression)
{
  return (po != NULL) ? po->setCompression(compression):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "dataType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_setDataType(ParametricObject_t * po, DataKind_t dataType)
{
  return (po != NULL) ? po->setDataType(dataType) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "dataType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_setDataTypeAsString(ParametricObject_t * po,
                                     const char * dataType)
{
  return (po != NULL) ? po->setDataType(dataType): LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_unsetId(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_unsetName(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "polygonType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_unsetPolygonType(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetPolygonType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "domainType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_unsetDomainType(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetDomainType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "pointIndex" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_unsetPointIndex(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetPointIndex() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "pointIndexLength" attribute of this
 * ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_unsetPointIndexLength(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetPointIndexLength() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "compression" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_unsetCompression(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetCompression() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "dataType" attribute of this ParametricObject_t.
 */
LIBSBML_EXTERN
int
ParametricObject_unsetDataType(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetDataType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * ParametricObject_t object have been set.
 */
LIBSBML_EXTERN
int
ParametricObject_hasRequiredAttributes(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


