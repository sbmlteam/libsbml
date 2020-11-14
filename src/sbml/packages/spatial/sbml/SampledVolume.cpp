/**
 * @file SampledVolume.cpp
 * @brief Implementation of the SampledVolume class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
#include <sbml/packages/spatial/sbml/SampledVolume.h>
#include <sbml/packages/spatial/sbml/ListOfSampledVolumes.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new SampledVolume using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
SampledVolume::SampledVolume(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
  : SBase(level, version)
  , mDomainType ("")
  , mSampledValue (util_NaN())
  , mIsSetSampledValue (false)
  , mMinValue (util_NaN())
  , mIsSetMinValue (false)
  , mMaxValue (util_NaN())
  , mIsSetMaxValue (false)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new SampledVolume using the given SpatialPkgNamespaces object.
 */
SampledVolume::SampledVolume(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mDomainType ("")
  , mSampledValue (util_NaN())
  , mIsSetSampledValue (false)
  , mMinValue (util_NaN())
  , mIsSetMinValue (false)
  , mMaxValue (util_NaN())
  , mIsSetMaxValue (false)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for SampledVolume.
 */
SampledVolume::SampledVolume(const SampledVolume& orig)
  : SBase( orig )
  , mDomainType ( orig.mDomainType )
  , mSampledValue ( orig.mSampledValue )
  , mIsSetSampledValue ( orig.mIsSetSampledValue )
  , mMinValue ( orig.mMinValue )
  , mIsSetMinValue ( orig.mIsSetMinValue )
  , mMaxValue ( orig.mMaxValue )
  , mIsSetMaxValue ( orig.mIsSetMaxValue )
{
}


/*
 * Assignment operator for SampledVolume.
 */
SampledVolume&
SampledVolume::operator=(const SampledVolume& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mDomainType = rhs.mDomainType;
    mSampledValue = rhs.mSampledValue;
    mIsSetSampledValue = rhs.mIsSetSampledValue;
    mMinValue = rhs.mMinValue;
    mIsSetMinValue = rhs.mIsSetMinValue;
    mMaxValue = rhs.mMaxValue;
    mIsSetMaxValue = rhs.mIsSetMaxValue;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this SampledVolume object.
 */
SampledVolume*
SampledVolume::clone() const
{
  return new SampledVolume(*this);
}


/*
 * Destructor for SampledVolume.
 */
SampledVolume::~SampledVolume()
{
}


/*
 * Returns the value of the "id" attribute of this SampledVolume.
 */
const std::string&
SampledVolume::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this SampledVolume.
 */
const std::string&
SampledVolume::getName() const
{
  return mName;
}


/*
 * Returns the value of the "domainType" attribute of this SampledVolume.
 */
const std::string&
SampledVolume::getDomainType() const
{
  return mDomainType;
}


/*
 * Returns the value of the "sampledValue" attribute of this SampledVolume.
 */
double
SampledVolume::getSampledValue() const
{
  return mSampledValue;
}


/*
 * Returns the value of the "minValue" attribute of this SampledVolume.
 */
double
SampledVolume::getMinValue() const
{
  return mMinValue;
}


/*
 * Returns the value of the "maxValue" attribute of this SampledVolume.
 */
double
SampledVolume::getMaxValue() const
{
  return mMaxValue;
}


/*
 * Predicate returning @c true if this SampledVolume's "id" attribute is set.
 */
bool
SampledVolume::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this SampledVolume's "name" attribute is set.
 */
bool
SampledVolume::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this SampledVolume's "domainType" attribute
 * is set.
 */
bool
SampledVolume::isSetDomainType() const
{
  return (mDomainType.empty() == false);
}


/*
 * Predicate returning @c true if this SampledVolume's "sampledValue" attribute
 * is set.
 */
bool
SampledVolume::isSetSampledValue() const
{
  return mIsSetSampledValue;
}


/*
 * Predicate returning @c true if this SampledVolume's "minValue" attribute is
 * set.
 */
bool
SampledVolume::isSetMinValue() const
{
  return mIsSetMinValue;
}


/*
 * Predicate returning @c true if this SampledVolume's "maxValue" attribute is
 * set.
 */
bool
SampledVolume::isSetMaxValue() const
{
  return mIsSetMaxValue;
}


/*
 * Sets the value of the "id" attribute of this SampledVolume.
 */
int
SampledVolume::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this SampledVolume.
 */
int
SampledVolume::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "domainType" attribute of this SampledVolume.
 */
int
SampledVolume::setDomainType(const std::string& domainType)
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
 * Sets the value of the "sampledValue" attribute of this SampledVolume.
 */
int
SampledVolume::setSampledValue(double sampledValue)
{
  mSampledValue = sampledValue;
  mIsSetSampledValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "minValue" attribute of this SampledVolume.
 */
int
SampledVolume::setMinValue(double minValue)
{
  mMinValue = minValue;
  mIsSetMinValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "maxValue" attribute of this SampledVolume.
 */
int
SampledVolume::setMaxValue(double maxValue)
{
  mMaxValue = maxValue;
  mIsSetMaxValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this SampledVolume.
 */
int
SampledVolume::unsetId()
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
 * Unsets the value of the "name" attribute of this SampledVolume.
 */
int
SampledVolume::unsetName()
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
 * Unsets the value of the "domainType" attribute of this SampledVolume.
 */
int
SampledVolume::unsetDomainType()
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
 * Unsets the value of the "sampledValue" attribute of this SampledVolume.
 */
int
SampledVolume::unsetSampledValue()
{
  mSampledValue = util_NaN();
  mIsSetSampledValue = false;

  if (isSetSampledValue() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "minValue" attribute of this SampledVolume.
 */
int
SampledVolume::unsetMinValue()
{
  mMinValue = util_NaN();
  mIsSetMinValue = false;

  if (isSetMinValue() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "maxValue" attribute of this SampledVolume.
 */
int
SampledVolume::unsetMaxValue()
{
  mMaxValue = util_NaN();
  mIsSetMaxValue = false;

  if (isSetMaxValue() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * @copydoc doc_renamesidref_common
 */
void
SampledVolume::renameSIdRefs(const std::string& oldid,
                             const std::string& newid)
{
  if (isSetDomainType() && mDomainType == oldid)
  {
    setDomainType(newid);
  }
}


/*
 * Returns the XML element name of this SampledVolume object.
 */
const std::string&
SampledVolume::getElementName() const
{
  static const string name = "sampledVolume";
  return name;
}


/*
 * Returns the libSBML type code for this SampledVolume object.
 */
int
SampledVolume::getTypeCode() const
{
  return SBML_SPATIAL_SAMPLEDVOLUME;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * SampledVolume object have been set.
 */
bool
SampledVolume::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetId() == false)
  {
    allPresent = false;
  }

  if (isSetDomainType() == false)
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
SampledVolume::writeElements(XMLOutputStream& stream) const
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
SampledVolume::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
SampledVolume::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
SampledVolume::enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SampledVolume.
 */
int
SampledVolume::getAttribute(const std::string& attributeName,
                            bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SampledVolume.
 */
int
SampledVolume::getAttribute(const std::string& attributeName,
                            int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SampledVolume.
 */
int
SampledVolume::getAttribute(const std::string& attributeName,
                            double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "sampledValue")
  {
    value = getSampledValue();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "minValue")
  {
    value = getMinValue();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "maxValue")
  {
    value = getMaxValue();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SampledVolume.
 */
int
SampledVolume::getAttribute(const std::string& attributeName,
                            unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this SampledVolume.
 */
int
SampledVolume::getAttribute(const std::string& attributeName,
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
  else if (attributeName == "domainType")
  {
    value = getDomainType();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this SampledVolume's attribute
 * "attributeName" is set.
 */
bool
SampledVolume::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "domainType")
  {
    value = isSetDomainType();
  }
  else if (attributeName == "sampledValue")
  {
    value = isSetSampledValue();
  }
  else if (attributeName == "minValue")
  {
    value = isSetMinValue();
  }
  else if (attributeName == "maxValue")
  {
    value = isSetMaxValue();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SampledVolume.
 */
int
SampledVolume::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SampledVolume.
 */
int
SampledVolume::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SampledVolume.
 */
int
SampledVolume::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "sampledValue")
  {
    return_value = setSampledValue(value);
  }
  else if (attributeName == "minValue")
  {
    return_value = setMinValue(value);
  }
  else if (attributeName == "maxValue")
  {
    return_value = setMaxValue(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SampledVolume.
 */
int
SampledVolume::setAttribute(const std::string& attributeName,
                            unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this SampledVolume.
 */
int
SampledVolume::setAttribute(const std::string& attributeName,
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
  else if (attributeName == "domainType")
  {
    return_value = setDomainType(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this SampledVolume.
 */
int
SampledVolume::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "domainType")
  {
    value = unsetDomainType();
  }
  else if (attributeName == "sampledValue")
  {
    value = unsetSampledValue();
  }
  else if (attributeName == "minValue")
  {
    value = unsetMinValue();
  }
  else if (attributeName == "maxValue")
  {
    value = unsetMaxValue();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
SampledVolume::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("domainType");

  attributes.add("sampledValue");

  attributes.add("minValue");

  attributes.add("maxValue");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
SampledVolume::readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfSampledVolumes*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialSampledVolumeAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialSampledFieldGeometryLOSampledVolumesAllowedCoreAttributes,
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
        log->logPackageError("spatial", SpatialSampledVolumeAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialSampledVolumeAllowedCoreAttributes, pkgVersion, level, version,
            details);
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
      logEmptyString(mId, level, version, "<SampledVolume>");
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
      "<SampledVolume> element.";
    log->logPackageError("spatial", SpatialSampledVolumeAllowedAttributes,
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
      logEmptyString(mName, level, version, "<SampledVolume>");
    }
  }

  // 
  // domainType SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("domainType", mDomainType);

  if (assigned == true)
  {
    if (mDomainType.empty() == true)
    {
      logEmptyString(mDomainType, level, version, "<SampledVolume>");
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
        SpatialSampledVolumeDomainTypeMustBeDomainType, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domainType' is missing from the "
      "<SampledVolume> element.";
    log->logPackageError("spatial", SpatialSampledVolumeAllowedAttributes,
      pkgVersion, level, version, message);
  }

  // 
  // sampledValue double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetSampledValue = attributes.readInto("sampledValue", mSampledValue);

  if ( mIsSetSampledValue == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'sampledValue' from the "
        "<SampledVolume> element must be an integer.";
      log->logPackageError("spatial",
        SpatialSampledVolumeSampledValueMustBeDouble, pkgVersion, level, version,
          message);
    }
  }

  // 
  // minValue double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetMinValue = attributes.readInto("minValue", mMinValue);

  if ( mIsSetMinValue == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'minValue' from the "
        "<SampledVolume> element must be an integer.";
      log->logPackageError("spatial", SpatialSampledVolumeMinValueMustBeDouble,
        pkgVersion, level, version, message);
    }
  }

  // 
  // maxValue double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetMaxValue = attributes.readInto("maxValue", mMaxValue);

  if ( mIsSetMaxValue == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'maxValue' from the "
        "<SampledVolume> element must be an integer.";
      log->logPackageError("spatial", SpatialSampledVolumeMaxValueMustBeDouble,
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
SampledVolume::writeAttributes(XMLOutputStream& stream) const
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

  if (isSetDomainType() == true)
  {
    stream.writeAttribute("domainType", getPrefix(), mDomainType);
  }

  if (isSetSampledValue() == true)
  {
    stream.writeAttribute("sampledValue", getPrefix(), mSampledValue);
  }

  if (isSetMinValue() == true)
  {
    stream.writeAttribute("minValue", getPrefix(), mMinValue);
  }

  if (isSetMaxValue() == true)
  {
    stream.writeAttribute("maxValue", getPrefix(), mMaxValue);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new SampledVolume_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
SampledVolume_t *
SampledVolume_create(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion)
{
  return new SampledVolume(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this SampledVolume_t object.
 */
LIBSBML_EXTERN
SampledVolume_t*
SampledVolume_clone(const SampledVolume_t* sv)
{
  if (sv != NULL)
  {
    return static_cast<SampledVolume_t*>(sv->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this SampledVolume_t object.
 */
LIBSBML_EXTERN
void
SampledVolume_free(SampledVolume_t* sv)
{
  if (sv != NULL)
  {
    delete sv;
  }
}


/*
 * Returns the value of the "id" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
char *
SampledVolume_getId(const SampledVolume_t * sv)
{
  if (sv == NULL)
  {
    return NULL;
  }

  return sv->getId().empty() ? NULL : safe_strdup(sv->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
char *
SampledVolume_getName(const SampledVolume_t * sv)
{
  if (sv == NULL)
  {
    return NULL;
  }

  return sv->getName().empty() ? NULL : safe_strdup(sv->getName().c_str());
}


/*
 * Returns the value of the "domainType" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
char *
SampledVolume_getDomainType(const SampledVolume_t * sv)
{
  if (sv == NULL)
  {
    return NULL;
  }

  return sv->getDomainType().empty() ? NULL :
    safe_strdup(sv->getDomainType().c_str());
}


/*
 * Returns the value of the "sampledValue" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
double
SampledVolume_getSampledValue(const SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->getSampledValue() : util_NaN();
}


/*
 * Returns the value of the "minValue" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
double
SampledVolume_getMinValue(const SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->getMinValue() : util_NaN();
}


/*
 * Returns the value of the "maxValue" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
double
SampledVolume_getMaxValue(const SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->getMaxValue() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this SampledVolume_t's "id" attribute is
 * set.
 */
LIBSBML_EXTERN
int
SampledVolume_isSetId(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledVolume_t's "name" attribute
 * is set.
 */
LIBSBML_EXTERN
int
SampledVolume_isSetName(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledVolume_t's "domainType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SampledVolume_isSetDomainType(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->isSetDomainType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledVolume_t's "sampledValue"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SampledVolume_isSetSampledValue(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->isSetSampledValue()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledVolume_t's "minValue"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SampledVolume_isSetMinValue(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->isSetMinValue()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this SampledVolume_t's "maxValue"
 * attribute is set.
 */
LIBSBML_EXTERN
int
SampledVolume_isSetMaxValue(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->isSetMaxValue()) : 0;
}


/*
 * Sets the value of the "id" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_setId(SampledVolume_t * sv, const char * id)
{
  return (sv != NULL) ? sv->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_setName(SampledVolume_t * sv, const char * name)
{
  return (sv != NULL) ? sv->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "domainType" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_setDomainType(SampledVolume_t * sv, const char * domainType)
{
  return (sv != NULL) ? sv->setDomainType(domainType) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "sampledValue" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_setSampledValue(SampledVolume_t * sv, double sampledValue)
{
  return (sv != NULL) ? sv->setSampledValue(sampledValue) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "minValue" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_setMinValue(SampledVolume_t * sv, double minValue)
{
  return (sv != NULL) ? sv->setMinValue(minValue) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "maxValue" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_setMaxValue(SampledVolume_t * sv, double maxValue)
{
  return (sv != NULL) ? sv->setMaxValue(maxValue) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_unsetId(SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_unsetName(SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "domainType" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_unsetDomainType(SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->unsetDomainType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "sampledValue" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_unsetSampledValue(SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->unsetSampledValue() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "minValue" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_unsetMinValue(SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->unsetMinValue() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "maxValue" attribute of this SampledVolume_t.
 */
LIBSBML_EXTERN
int
SampledVolume_unsetMaxValue(SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->unsetMaxValue() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SampledVolume_t object have been set.
 */
LIBSBML_EXTERN
int
SampledVolume_hasRequiredAttributes(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


