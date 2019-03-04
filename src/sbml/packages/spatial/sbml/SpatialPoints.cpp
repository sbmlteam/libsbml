/**
 * @file SpatialPoints.cpp
 * @brief Implementation of the SpatialPoints class.
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
#include <sbml/packages/spatial/sbml/SpatialPoints.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new SpatialPoints using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
SpatialPoints::SpatialPoints(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
  : SBase(level, version)
  , mCompression (SPATIAL_COMPRESSIONKIND_INVALID)
  , mArrayData (NULL)
  , mArrayDataLength (SBML_INT_MAX)
  , mIsSetArrayDataLength (false)
  , mDataType (SPATIAL_DATAKIND_INVALID)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new SpatialPoints using the given SpatialPkgNamespaces object.
 */
SpatialPoints::SpatialPoints(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mCompression (SPATIAL_COMPRESSIONKIND_INVALID)
  , mArrayData (NULL)
  , mArrayDataLength (SBML_INT_MAX)
  , mIsSetArrayDataLength (false)
  , mDataType (SPATIAL_DATAKIND_INVALID)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for SpatialPoints.
 */
SpatialPoints::SpatialPoints(const SpatialPoints& orig)
  : SBase( orig )
  , mCompression ( orig.mCompression )
  , mArrayData ( NULL )
  , mArrayDataLength ( orig.mArrayDataLength )
  , mIsSetArrayDataLength ( orig.mIsSetArrayDataLength )
  , mDataType ( orig.mDataType )
{
  setArrayData(orig.mArrayData, orig.mArrayDataLength);

}


/*
 * Assignment operator for SpatialPoints.
 */
SpatialPoints&
SpatialPoints::operator=(const SpatialPoints& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mCompression = rhs.mCompression;
    mArrayData = NULL;
    setArrayData(rhs.mArrayData, rhs.mArrayDataLength);
    mArrayDataLength = rhs.mArrayDataLength;
    mIsSetArrayDataLength = rhs.mIsSetArrayDataLength;
    mDataType = rhs.mDataType;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SpatialPoints object.
 */
SpatialPoints*
SpatialPoints::clone() const
{
  return new SpatialPoints(*this);
}


/*
 * Destructor for SpatialPoints.
 */
SpatialPoints::~SpatialPoints()
{
  if (mArrayData != NULL)
  {
    delete [] mArrayData;
  }

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
 * Returns the value of the "name" attribute of this SpatialPoints.
 */
const std::string&
SpatialPoints::getName() const
{
  return mName;
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
 * Returns the value of the "compression" attribute of this SpatialPoints.
 */
const std::string&
SpatialPoints::getCompressionAsString() const
{
  static const std::string code_str = CompressionKind_toString(mCompression);
  return code_str;
}


/*
 * Returns the value of the "arrayData" attribute of this SpatialPoints.
 */
void
SpatialPoints::getArrayData(double* outArray) const
{
  if (outArray == NULL || mArrayData == NULL)
  {
    return;
  }

  memcpy(outArray, mArrayData, sizeof(double)*mArrayDataLength);
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
 * Returns the value of the "dataType" attribute of this SpatialPoints.
 */
std::string
SpatialPoints::getDataTypeAsString() const
{
  std::string code_str = DataKind_toString(mDataType);
  return code_str;
}


/*
 * Predicate returning @c true if this SpatialPoints's "id" attribute is set.
 */
bool
SpatialPoints::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this SpatialPoints's "name" attribute is set.
 */
bool
SpatialPoints::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this SpatialPoints's "compression" attribute
 * is set.
 */
bool
SpatialPoints::isSetCompression() const
{
  return (mCompression != SPATIAL_COMPRESSIONKIND_INVALID);
}


/*
 * Predicate returning @c true if this SpatialPoints's "arrayData" attribute is
 * set.
 */
bool
SpatialPoints::isSetArrayData() const
{
  return (mArrayData != NULL);
}


/*
 * Predicate returning @c true if this SpatialPoints's "arrayDataLength"
 * attribute is set.
 */
bool
SpatialPoints::isSetArrayDataLength() const
{
  return mIsSetArrayDataLength;
}


/*
 * Predicate returning @c true if this SpatialPoints's "dataType" attribute is
 * set.
 */
bool
SpatialPoints::isSetDataType() const
{
  return (mDataType != SPATIAL_DATAKIND_INVALID);
}


/*
 * Sets the value of the "id" attribute of this SpatialPoints.
 */
int
SpatialPoints::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this SpatialPoints.
 */
int
SpatialPoints::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "compression" attribute of this SpatialPoints.
 */
int
SpatialPoints::setCompression(const CompressionKind_t compression)
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
 * Sets the value of the "compression" attribute of this SpatialPoints.
 */
int
SpatialPoints::setCompression(const std::string& compression)
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
}


/*
 * Sets the value of the "arrayData" attribute of this SpatialPoints.
 */
int
SpatialPoints::setArrayData(double* inArray, int arrayLength)
{
  if (inArray == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  if (mArrayData != NULL)
  {
    delete[] mArrayData;
  }

  mArrayData = new double[arrayLength];
  memcpy(mArrayData, inArray, sizeof(double)*arrayLength);
  mIsSetArrayDataLength = true;
  mArrayDataLength = arrayLength;

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "arrayDataLength" attribute of this SpatialPoints.
 */
int
SpatialPoints::setArrayDataLength(int arrayDataLength)
{
  mArrayDataLength = arrayDataLength;
  mIsSetArrayDataLength = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "dataType" attribute of this SpatialPoints.
 */
int
SpatialPoints::setDataType(const DataKind_t dataType)
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
 * Sets the value of the "dataType" attribute of this SpatialPoints.
 */
int
SpatialPoints::setDataType(const std::string& dataType)
{
  mDataType = DataKind_fromString(dataType.c_str());

  if (mDataType == SPATIAL_DATAKIND_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this SpatialPoints.
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
 * Unsets the value of the "name" attribute of this SpatialPoints.
 */
int
SpatialPoints::unsetName()
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
 * Unsets the value of the "compression" attribute of this SpatialPoints.
 */
int
SpatialPoints::unsetCompression()
{
  mCompression = SPATIAL_COMPRESSIONKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "arrayData" attribute of this SpatialPoints.
 */
int
SpatialPoints::unsetArrayData()
{
  if (mArrayData != NULL)
  {
    delete[] mArrayData;
  }

  mArrayData = NULL;

  return unsetArrayDataLength();
}


/*
 * Unsets the value of the "arrayDataLength" attribute of this SpatialPoints.
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
 * Unsets the value of the "dataType" attribute of this SpatialPoints.
 */
int
SpatialPoints::unsetDataType()
{
  mDataType = SPATIAL_DATAKIND_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this SpatialPoints object.
 */
const std::string&
SpatialPoints::getElementName() const
{
  static const string name = "spatialPoints";
  return name;
}


/*
 * Returns the libSBML type code for this SpatialPoints object.
 */
int
SpatialPoints::getTypeCode() const
{
  return SBML_SPATIAL_SPATIALPOINTS;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * SpatialPoints object have been set.
 */
bool
SpatialPoints::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetCompression() == false)
  {
    allPresent = false;
  }

  if (isSetArrayData() == false)
  {
    allPresent = false;
  }

  if (isSetArrayDataLength() == false)
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
SpatialPoints::writeElements(XMLOutputStream& stream) const
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
SpatialPoints::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
SpatialPoints::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * used to write arrays
 */
void
SpatialPoints::write(XMLOutputStream& stream) const
{
  stream.startElement(getElementName(), getPrefix());
  writeAttributes(stream);

  if (isSetArrayData())
  {
    for (int i = 0; i < mArrayDataLength; ++i)
    {
      stream << (double)mArrayData[i] << " ";
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
SpatialPoints::enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpatialPoints.
 */
int
SpatialPoints::getAttribute(const std::string& attributeName,
                            bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpatialPoints.
 */
int
SpatialPoints::getAttribute(const std::string& attributeName,
                            int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "arrayDataLength")
  {
    value = getArrayDataLength();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpatialPoints.
 */
int
SpatialPoints::getAttribute(const std::string& attributeName,
                            double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpatialPoints.
 */
int
SpatialPoints::getAttribute(const std::string& attributeName,
                            unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SpatialPoints.
 */
int
SpatialPoints::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this SpatialPoints's attribute
 * "attributeName" is set.
 */
bool
SpatialPoints::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "compression")
  {
    value = isSetCompression();
  }
  else if (attributeName == "arrayData")
  {
    value = isSetArrayData();
  }
  else if (attributeName == "arrayDataLength")
  {
    value = isSetArrayDataLength();
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
 * Sets the value of the "attributeName" attribute of this SpatialPoints.
 */
int
SpatialPoints::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpatialPoints.
 */
int
SpatialPoints::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "arrayDataLength")
  {
    return_value = setArrayDataLength(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpatialPoints.
 */
int
SpatialPoints::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpatialPoints.
 */
int
SpatialPoints::setAttribute(const std::string& attributeName,
                            unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SpatialPoints.
 */
int
SpatialPoints::setAttribute(const std::string& attributeName,
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
 * Unsets the value of the "attributeName" attribute of this SpatialPoints.
 */
int
SpatialPoints::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "compression")
  {
    value = unsetCompression();
  }
  else if (attributeName == "arrayData")
  {
    value = unsetArrayData();
  }
  else if (attributeName == "arrayDataLength")
  {
    value = unsetArrayDataLength();
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
SpatialPoints::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("compression");

  attributes.add("arrayDataLength");

  attributes.add("dataType");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
SpatialPoints::readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

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
        log->logPackageError("spatial", SpatialSpatialPointsAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialSpatialPointsAllowedCoreAttributes, pkgVersion, level, version,
            details);
      }
    }
  }

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<SpatialPoints>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("spatial", SpatialIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<SpatialPoints>");
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
      logEmptyString(compression, level, version, "<SpatialPoints>");
    }
    else
    {
      mCompression = CompressionKind_fromString(compression.c_str());

      if (CompressionKind_isValid(mCompression) == 0)
      {
        std::string msg = "The compression on the <SpatialPoints> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + compression + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialSpatialPointsCompressionMustBeCompressionKindEnum, pkgVersion,
            level, version, msg);
      }
    }
  }
  else
  {
    std::string message = "Spatial attribute 'compression' is missing.";
    log->logPackageError("spatial", SpatialSpatialPointsAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // arrayDataLength int (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetArrayDataLength = attributes.readInto("arrayDataLength",
    mArrayDataLength);

  if ( mIsSetArrayDataLength == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'arrayDataLength' from the "
        "<SpatialPoints> element must be an integer.";
      log->logPackageError("spatial",
        SpatialSpatialPointsArrayDataLengthMustBeInteger, pkgVersion, level,
          version, message);
    }
    else
    {
      std::string message = "Spatial attribute 'arrayDataLength' is missing "
        "from the <SpatialPoints> element.";
      log->logPackageError("spatial", SpatialSpatialPointsAllowedAttributes,
        pkgVersion, level, version, message);
    }
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
      logEmptyString(dataType, level, version, "<SpatialPoints>");
    }
    else
    {
      mDataType = DataKind_fromString(dataType.c_str());

      if (DataKind_isValid(mDataType) == 0)
      {
        std::string msg = "The dataType on the <SpatialPoints> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + dataType + "', which is not a valid option.";

        log->logPackageError("spatial",
          SpatialSpatialPointsDataTypeMustBeDataKindEnum, pkgVersion, level,
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
SpatialPoints::writeAttributes(XMLOutputStream& stream) const
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

  if (isSetCompression() == true)
  {
    stream.writeAttribute("compression", getPrefix(),
      CompressionKind_toString(mCompression));
  }

  if (isSetArrayDataLength() == true)
  {
    stream.writeAttribute("arrayDataLength", getPrefix(), mArrayDataLength);
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
SpatialPoints::setElementText(const std::string& text)
{
  stringstream strStream(text);
  double val;
  vector<double> valuesVector;

  while (strStream >> val)
  {
    valuesVector.push_back(val);
  }

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

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new SpatialPoints_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
SpatialPoints_t *
SpatialPoints_create(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion)
{
  return new SpatialPoints(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this SpatialPoints_t object.
 */
LIBSBML_EXTERN
SpatialPoints_t*
SpatialPoints_clone(const SpatialPoints_t* sp)
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


/*
 * Frees this SpatialPoints_t object.
 */
LIBSBML_EXTERN
void
SpatialPoints_free(SpatialPoints_t* sp)
{
  if (sp != NULL)
  {
    delete sp;
  }
}


/*
 * Returns the value of the "id" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
char *
SpatialPoints_getId(const SpatialPoints_t * sp)
{
  if (sp == NULL)
  {
    return NULL;
  }

  return sp->getId().empty() ? NULL : safe_strdup(sp->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
char *
SpatialPoints_getName(const SpatialPoints_t * sp)
{
  if (sp == NULL)
  {
    return NULL;
  }

  return sp->getName().empty() ? NULL : safe_strdup(sp->getName().c_str());
}


/*
 * Returns the value of the "compression" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
CompressionKind_t
SpatialPoints_getCompression(const SpatialPoints_t * sp)
{
  if (sp == NULL)
  {
    return SPATIAL_COMPRESSIONKIND_INVALID;
  }

  return sp->getCompression();
}


/*
 * Returns the value of the "compression" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
char *
SpatialPoints_getCompressionAsString(const SpatialPoints_t * sp)
{
  return (char*)(CompressionKind_toString(sp->getCompression()));
}


/*
 * Returns the value of the "arrayDataLength" attribute of this
 * SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_getArrayDataLength(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->getArrayDataLength() : SBML_INT_MAX;
}


/*
 * Returns the value of the "dataType" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
DataKind_t
SpatialPoints_getDataType(const SpatialPoints_t * sp)
{
  if (sp == NULL)
  {
    return SPATIAL_DATAKIND_INVALID;
  }

  return sp->getDataType();
}


/*
 * Returns the value of the "dataType" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
char *
SpatialPoints_getDataTypeAsString(const SpatialPoints_t * sp)
{
  return (char*)(DataKind_toString(sp->getDataType()));
}


/*
 * Predicate returning @c 1 (true) if this SpatialPoints_t's "id" attribute is
 * set.
 */
LIBSBML_EXTERN
int
SpatialPoints_isSetId(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SpatialPoints_t's "name" attribute
 * is set.
 */
LIBSBML_EXTERN
int
SpatialPoints_isSetName(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SpatialPoints_t's "compression"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SpatialPoints_isSetCompression(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetCompression()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SpatialPoints_t's "arrayData"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SpatialPoints_isSetArrayData(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetArrayData()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SpatialPoints_t's "arrayDataLength"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SpatialPoints_isSetArrayDataLength(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetArrayDataLength()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SpatialPoints_t's "dataType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SpatialPoints_isSetDataType(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetDataType()) : 0;
}


/*
 * Sets the value of the "id" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_setId(SpatialPoints_t * sp, const char * id)
{
  return (sp != NULL) ? sp->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_setName(SpatialPoints_t * sp, const char * name)
{
  return (sp != NULL) ? sp->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "compression" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_setCompression(SpatialPoints_t * sp,
                             CompressionKind_t compression)
{
  return (sp != NULL) ? sp->setCompression(compression) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "compression" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_setCompressionAsString(SpatialPoints_t * sp,
                                     const char * compression)
{
  return (sp != NULL) ? sp->setCompression(compression):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "arrayData" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_setArrayData(SpatialPoints_t* sp,
                           double* arrayData,
                           int arrayLength)
{
  return (sp != NULL) ? sp->setArrayData(arrayData, arrayLength) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "arrayDataLength" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_setArrayDataLength(SpatialPoints_t * sp, int arrayDataLength)
{
  return (sp != NULL) ? sp->setArrayDataLength(arrayDataLength) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "dataType" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_setDataType(SpatialPoints_t * sp, DataKind_t dataType)
{
  return (sp != NULL) ? sp->setDataType(dataType) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "dataType" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_setDataTypeAsString(SpatialPoints_t * sp, const char * dataType)
{
  return (sp != NULL) ? sp->setDataType(dataType): LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_unsetId(SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_unsetName(SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "compression" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_unsetCompression(SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->unsetCompression() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "arrayData" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_unsetArrayData(SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->unsetArrayData() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "arrayDataLength" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_unsetArrayDataLength(SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->unsetArrayDataLength() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "dataType" attribute of this SpatialPoints_t.
 */
LIBSBML_EXTERN
int
SpatialPoints_unsetDataType(SpatialPoints_t * sp)
{
  return (sp != NULL) ? sp->unsetDataType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SpatialPoints_t object have been set.
 */
LIBSBML_EXTERN
int
SpatialPoints_hasRequiredAttributes(const SpatialPoints_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


