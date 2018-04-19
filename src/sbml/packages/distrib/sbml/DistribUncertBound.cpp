/**
 * @file DistribUncertBound.cpp
 * @brief Implementation of the DistribUncertBound class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/packages/distrib/sbml/DistribUncertBound.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribUncertBound using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
DistribUncertBound::DistribUncertBound(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion)
  : DistribUncertValue(level, version)
  , mInclusive (false)
  , mIsSetInclusive (false)
  , mElementName("uncertBound")
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new DistribUncertBound using the given DistribPkgNamespaces
 * object.
 */
DistribUncertBound::DistribUncertBound(DistribPkgNamespaces *distribns)
  : DistribUncertValue(distribns)
  , mInclusive (false)
  , mIsSetInclusive (false)
  , mElementName("uncertBound")
{
  setElementNamespace(distribns->getURI());
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribUncertBound.
 */
DistribUncertBound::DistribUncertBound(const DistribUncertBound& orig)
  : DistribUncertValue( orig )
  , mInclusive ( orig.mInclusive )
  , mIsSetInclusive ( orig.mIsSetInclusive )
  , mElementName ( orig.mElementName )
{
}


/*
 * Assignment operator for DistribUncertBound.
 */
DistribUncertBound&
DistribUncertBound::operator=(const DistribUncertBound& rhs)
{
  if (&rhs != this)
  {
    DistribUncertValue::operator=(rhs);
    mInclusive = rhs.mInclusive;
    mIsSetInclusive = rhs.mIsSetInclusive;
    mElementName = rhs.mElementName;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribUncertBound object.
 */
DistribUncertBound*
DistribUncertBound::clone() const
{
  return new DistribUncertBound(*this);
}


/*
 * Destructor for DistribUncertBound.
 */
DistribUncertBound::~DistribUncertBound()
{
}


/*
 * Returns the value of the "id" attribute of this DistribUncertBound.
 */
const std::string&
DistribUncertBound::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this DistribUncertBound.
 */
const std::string&
DistribUncertBound::getName() const
{
  return mName;
}


/*
 * Returns the value of the "inclusive" attribute of this DistribUncertBound.
 */
bool
DistribUncertBound::getInclusive() const
{
  return mInclusive;
}


/*
 * Predicate returning @c true if this DistribUncertBound's "id" attribute is
 * set.
 */
bool
DistribUncertBound::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribUncertBound's "name" attribute is
 * set.
 */
bool
DistribUncertBound::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this DistribUncertBound's "inclusive"
 * attribute is set.
 */
bool
DistribUncertBound::isSetInclusive() const
{
  return mIsSetInclusive;
}


/*
 * Sets the value of the "id" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "inclusive" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::setInclusive(bool inclusive)
{
  mInclusive = inclusive;
  mIsSetInclusive = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::unsetId()
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
 * Unsets the value of the "name" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::unsetName()
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
 * Unsets the value of the "inclusive" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::unsetInclusive()
{
  mInclusive = false;
  mIsSetInclusive = false;

  if (isSetInclusive() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the XML element name of this DistribUncertBound object.
 */
const std::string&
DistribUncertBound::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this DistribUncertBound object.
 */
void
DistribUncertBound::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this DistribUncertBound object.
 */
int
DistribUncertBound::getTypeCode() const
{
  return SBML_DISTRIB_UNCERTBOUND;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribUncertBound object have been set.
 */
bool
DistribUncertBound::hasRequiredAttributes() const
{
  bool allPresent = DistribUncertValue::hasRequiredAttributes();

  if (isSetInclusive() == false)
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
DistribUncertBound::writeElements(XMLOutputStream& stream) const
{
  DistribUncertValue::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribUncertBound::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribUncertBound::setSBMLDocument(SBMLDocument* d)
{
  DistribUncertValue::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribUncertBound::enablePackageInternal(const std::string& pkgURI,
                                          const std::string& pkgPrefix,
                                          bool flag)
{
  DistribUncertValue::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::getAttribute(const std::string& attributeName,
                                 bool& value) const
{
  int return_value = DistribUncertValue::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "inclusive")
  {
    value = getInclusive();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::getAttribute(const std::string& attributeName,
                                 int& value) const
{
  int return_value = DistribUncertValue::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::getAttribute(const std::string& attributeName,
                                 double& value) const
{
  int return_value = DistribUncertValue::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::getAttribute(const std::string& attributeName,
                                 unsigned int& value) const
{
  int return_value = DistribUncertValue::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::getAttribute(const std::string& attributeName,
                                 std::string& value) const
{
  int return_value = DistribUncertValue::getAttribute(attributeName, value);

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

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribUncertBound's attribute
 * "attributeName" is set.
 */
bool
DistribUncertBound::isSetAttribute(const std::string& attributeName) const
{
  bool value = DistribUncertValue::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }
  else if (attributeName == "inclusive")
  {
    value = isSetInclusive();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = DistribUncertValue::setAttribute(attributeName, value);

  if (attributeName == "inclusive")
  {
    return_value = setInclusive(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::setAttribute(const std::string& attributeName, int value)
{
  int return_value = DistribUncertValue::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::setAttribute(const std::string& attributeName,
                                 double value)
{
  int return_value = DistribUncertValue::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::setAttribute(const std::string& attributeName,
                                 unsigned int value)
{
  int return_value = DistribUncertValue::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertBound.
 */
int
DistribUncertBound::setAttribute(const std::string& attributeName,
                                 const std::string& value)
{
  int return_value = DistribUncertValue::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribUncertBound.
 */
int
DistribUncertBound::unsetAttribute(const std::string& attributeName)
{
  int value = DistribUncertValue::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }
  else if (attributeName == "inclusive")
  {
    value = unsetInclusive();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribUncertBound::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribUncertValue::createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DistribUncertBound::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribUncertValue::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("id");
    attributes.add("name");
    attributes.add("inclusive");
  }

  if (level == 3 && coreVersion == 2 && pkgVersion == 1)
  {
    attributes.add("inclusive");
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribUncertBound::readAttributes(const XMLAttributes& attributes,
                                   const ExpectedAttributes&
                                     expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribUncertValue::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("distrib",
          DistribDistribUncertBoundAllowedAttributes, pkgVersion, level, version,
            details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribUncertBoundAllowedCoreAttributes, pkgVersion, level,
            version, details);
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    readL3V1V1Attributes(attributes);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    readL3V2V1Attributes(attributes);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribUncertBound::readL3V1V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs;

  // 
  // id SId (use = "optional" )
  // 

  XMLTriple tripleID("id", mURI, getPrefix());
  assigned = attributes.readInto(tripleID, mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribUncertBound>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  XMLTriple tripleNAME("name", mURI, getPrefix());
  assigned = attributes.readInto(tripleNAME, mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<DistribUncertBound>");
    }
  }

  // 
  // inclusive bool (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetInclusive = attributes.readInto("inclusive", mInclusive);

  if (mIsSetInclusive == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      log->logPackageError("distrib",
        DistribDistribUncertBoundInclusiveMustBeBoolean, pkgVersion, level,
          version);
    }
    else
    {
      std::string message = "Distrib attribute 'inclusive' is missing from the "
        "<DistribUncertBound> element.";
      log->logPackageError("distrib",
        DistribDistribUncertBoundAllowedAttributes, pkgVersion, level, version,
          message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribUncertBound::readL3V2V1Attributes(const XMLAttributes& attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
  unsigned int numErrs;

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribUncertBound>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false)
    {
      log->logPackageError("distrib", DistribIdSyntaxRule, pkgVersion, level,
        version, "The id on the <" + getElementName() + "> is '" + mId + "', "
          "which does not conform to the syntax.", getLine(), getColumn());
    }
  }

  // 
  // name string (use = "optional" )
  // 

  // read by SBase;

  // 
  // inclusive bool (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetInclusive = attributes.readInto("inclusive", mInclusive);

  if (mIsSetInclusive == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      log->logPackageError("distrib",
        DistribDistribUncertBoundInclusiveMustBeBoolean, pkgVersion, level,
          version);
    }
    else
    {
      std::string message = "Distrib attribute 'inclusive' is missing from the "
        "<DistribUncertBound> element.";
      log->logPackageError("distrib",
        DistribDistribUncertBoundAllowedAttributes, pkgVersion, level, version,
          message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribUncertBound::writeAttributes(XMLOutputStream& stream) const
{
  DistribUncertValue::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    writeL3V1V1Attributes(stream);
  }

  if (level == 3 && version == 2 && pkgVersion == 1)
  {
    writeL3V2V1Attributes(stream);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribUncertBound::writeL3V1V1Attributes(XMLOutputStream& stream) const
{
  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }

  if (isSetInclusive() == true)
  {
    stream.writeAttribute("inclusive", getPrefix(), mInclusive);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribUncertBound::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
  if (isSetInclusive() == true)
  {
    stream.writeAttribute("inclusive", getPrefix(), mInclusive);
  }
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribUncertBound_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribUncertBound_t *
DistribUncertBound_create(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion)
{
  return new DistribUncertBound(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribUncertBound_t object.
 */
LIBSBML_EXTERN
DistribUncertBound_t*
DistribUncertBound_clone(const DistribUncertBound_t* dub)
{
  if (dub != NULL)
  {
    return static_cast<DistribUncertBound_t*>(dub->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribUncertBound_t object.
 */
LIBSBML_EXTERN
void
DistribUncertBound_free(DistribUncertBound_t* dub)
{
  if (dub != NULL)
  {
    delete dub;
  }
}


/*
 * Returns the value of the "id" attribute of this DistribUncertBound_t.
 */
LIBSBML_EXTERN
char *
DistribUncertBound_getId(const DistribUncertBound_t * dub)
{
  if (dub == NULL)
  {
    return NULL;
  }

  return dub->getId().empty() ? NULL : safe_strdup(dub->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this DistribUncertBound_t.
 */
LIBSBML_EXTERN
char *
DistribUncertBound_getName(const DistribUncertBound_t * dub)
{
  if (dub == NULL)
  {
    return NULL;
  }

  return dub->getName().empty() ? NULL : safe_strdup(dub->getName().c_str());
}


/*
 * Returns the value of the "inclusive" attribute of this DistribUncertBound_t.
 */
LIBSBML_EXTERN
int
DistribUncertBound_getInclusive(const DistribUncertBound_t * dub)
{
  return (dub != NULL) ? static_cast<int>(dub->getInclusive()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertBound_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DistribUncertBound_isSetId(const DistribUncertBound_t * dub)
{
  return (dub != NULL) ? static_cast<int>(dub->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertBound_t's "name"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DistribUncertBound_isSetName(const DistribUncertBound_t * dub)
{
  return (dub != NULL) ? static_cast<int>(dub->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertBound_t's "inclusive"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DistribUncertBound_isSetInclusive(const DistribUncertBound_t * dub)
{
  return (dub != NULL) ? static_cast<int>(dub->isSetInclusive()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribUncertBound_t.
 */
LIBSBML_EXTERN
int
DistribUncertBound_setId(DistribUncertBound_t * dub, const char * id)
{
  return (dub != NULL) ? dub->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this DistribUncertBound_t.
 */
LIBSBML_EXTERN
int
DistribUncertBound_setName(DistribUncertBound_t * dub, const char * name)
{
  return (dub != NULL) ? dub->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "inclusive" attribute of this DistribUncertBound_t.
 */
LIBSBML_EXTERN
int
DistribUncertBound_setInclusive(DistribUncertBound_t * dub, int inclusive)
{
  return (dub != NULL) ? dub->setInclusive(inclusive) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this DistribUncertBound_t.
 */
LIBSBML_EXTERN
int
DistribUncertBound_unsetId(DistribUncertBound_t * dub)
{
  return (dub != NULL) ? dub->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this DistribUncertBound_t.
 */
LIBSBML_EXTERN
int
DistribUncertBound_unsetName(DistribUncertBound_t * dub)
{
  return (dub != NULL) ? dub->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "inclusive" attribute of this DistribUncertBound_t.
 */
LIBSBML_EXTERN
int
DistribUncertBound_unsetInclusive(DistribUncertBound_t * dub)
{
  return (dub != NULL) ? dub->unsetInclusive() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUncertBound_t object have been set.
 */
LIBSBML_EXTERN
int
DistribUncertBound_hasRequiredAttributes(const DistribUncertBound_t * dub)
{
  return (dub != NULL) ? static_cast<int>(dub->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


