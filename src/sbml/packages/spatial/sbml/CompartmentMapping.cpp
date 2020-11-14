/**
 * @file CompartmentMapping.cpp
 * @brief Implementation of the CompartmentMapping class.
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
#include <sbml/packages/spatial/sbml/CompartmentMapping.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CompartmentMapping using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CompartmentMapping::CompartmentMapping(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion)
  : SBase(level, version)
  , mDomainType ("")
  , mUnitSize (util_NaN())
  , mIsSetUnitSize (false)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new CompartmentMapping using the given SpatialPkgNamespaces
 * object.
 */
CompartmentMapping::CompartmentMapping(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mDomainType ("")
  , mUnitSize (util_NaN())
  , mIsSetUnitSize (false)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CompartmentMapping.
 */
CompartmentMapping::CompartmentMapping(const CompartmentMapping& orig)
  : SBase( orig )
  , mDomainType ( orig.mDomainType )
  , mUnitSize ( orig.mUnitSize )
  , mIsSetUnitSize ( orig.mIsSetUnitSize )
{
}


/*
 * Assignment operator for CompartmentMapping.
 */
CompartmentMapping&
CompartmentMapping::operator=(const CompartmentMapping& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mDomainType = rhs.mDomainType;
    mUnitSize = rhs.mUnitSize;
    mIsSetUnitSize = rhs.mIsSetUnitSize;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CompartmentMapping object.
 */
CompartmentMapping*
CompartmentMapping::clone() const
{
  return new CompartmentMapping(*this);
}


/*
 * Destructor for CompartmentMapping.
 */
CompartmentMapping::~CompartmentMapping()
{
}


/*
 * Returns the value of the "id" attribute of this CompartmentMapping.
 */
const std::string&
CompartmentMapping::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this CompartmentMapping.
 */
const std::string&
CompartmentMapping::getName() const
{
  return mName;
}


/*
 * Returns the value of the "domainType" attribute of this CompartmentMapping.
 */
const std::string&
CompartmentMapping::getDomainType() const
{
  return mDomainType;
}


/*
 * Returns the value of the "unitSize" attribute of this CompartmentMapping.
 */
double
CompartmentMapping::getUnitSize() const
{
  return mUnitSize;
}


/*
 * Predicate returning @c true if this CompartmentMapping's "id" attribute is
 * set.
 */
bool
CompartmentMapping::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this CompartmentMapping's "name" attribute is
 * set.
 */
bool
CompartmentMapping::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this CompartmentMapping's "domainType"
 * attribute is set.
 */
bool
CompartmentMapping::isSetDomainType() const
{
  return (mDomainType.empty() == false);
}


/*
 * Predicate returning @c true if this CompartmentMapping's "unitSize"
 * attribute is set.
 */
bool
CompartmentMapping::isSetUnitSize() const
{
  return mIsSetUnitSize;
}


/*
 * Sets the value of the "id" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "domainType" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::setDomainType(const std::string& domainType)
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
 * Sets the value of the "unitSize" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::setUnitSize(double unitSize)
{
  mUnitSize = unitSize;
  mIsSetUnitSize = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::unsetId()
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
 * Unsets the value of the "name" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::unsetName()
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
 * Unsets the value of the "domainType" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::unsetDomainType()
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
 * Unsets the value of the "unitSize" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::unsetUnitSize()
{
  mUnitSize = util_NaN();
  mIsSetUnitSize = false;

  if (isSetUnitSize() == false)
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
CompartmentMapping::renameSIdRefs(const std::string& oldid,
                                  const std::string& newid)
{
  if (isSetDomainType() && mDomainType == oldid)
  {
    setDomainType(newid);
  }
}


/*
 * Returns the XML element name of this CompartmentMapping object.
 */
const std::string&
CompartmentMapping::getElementName() const
{
  static const string name = "compartmentMapping";
  return name;
}


/*
 * Returns the libSBML type code for this CompartmentMapping object.
 */
int
CompartmentMapping::getTypeCode() const
{
  return SBML_SPATIAL_COMPARTMENTMAPPING;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * CompartmentMapping object have been set.
 */
bool
CompartmentMapping::hasRequiredAttributes() const
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

  if (isSetUnitSize() == false)
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
CompartmentMapping::writeElements(XMLOutputStream& stream) const
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
CompartmentMapping::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
CompartmentMapping::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CompartmentMapping::enablePackageInternal(const std::string& pkgURI,
                                          const std::string& pkgPrefix,
                                          bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::getAttribute(const std::string& attributeName,
                                 bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::getAttribute(const std::string& attributeName,
                                 int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::getAttribute(const std::string& attributeName,
                                 double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "unitSize")
  {
    value = getUnitSize();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::getAttribute(const std::string& attributeName,
                                 unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this CompartmentMapping's attribute
 * "attributeName" is set.
 */
bool
CompartmentMapping::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "unitSize")
  {
    value = isSetUnitSize();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::setAttribute(const std::string& attributeName,
                                 double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "unitSize")
  {
    return_value = setUnitSize(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::setAttribute(const std::string& attributeName,
                                 unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CompartmentMapping.
 */
int
CompartmentMapping::setAttribute(const std::string& attributeName,
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
 * Unsets the value of the "attributeName" attribute of this
 * CompartmentMapping.
 */
int
CompartmentMapping::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "unitSize")
  {
    value = unsetUnitSize();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
CompartmentMapping::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("domainType");

  attributes.add("unitSize");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CompartmentMapping::readAttributes(const XMLAttributes& attributes,
                                   const ExpectedAttributes&
                                     expectedAttributes)
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
        log->logPackageError("spatial",
          SpatialCompartmentMappingAllowedAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialCompartmentMappingAllowedCoreAttributes, pkgVersion, level,
            version, details, getLine(), getColumn());
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
      logEmptyString(mId, level, version, "<CompartmentMapping>");
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
      "<CompartmentMapping> element.";
    log->logPackageError("spatial", SpatialCompartmentMappingAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }

  // 
  // name string (use = "optional" )
  // 

  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    if (mName.empty() == true)
    {
      logEmptyString(mName, level, version, "<CompartmentMapping>");
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
      logEmptyString(mDomainType, level, version, "<CompartmentMapping>");
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
        SpatialCompartmentMappingDomainTypeMustBeDomainType, pkgVersion, level,
          version, msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domainType' is missing from the "
      "<CompartmentMapping> element.";
    log->logPackageError("spatial", SpatialCompartmentMappingAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }

  // 
  // unitSize double (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetUnitSize = attributes.readInto("unitSize", mUnitSize);

  if ( mIsSetUnitSize == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'unitSize' from the "
        "<CompartmentMapping> element must be an integer.";
      log->logPackageError("spatial",
        SpatialCompartmentMappingUnitSizeMustBeDouble, pkgVersion, level,
          version, message, getLine(), getColumn());
    }
    else
    {
      std::string message = "Spatial attribute 'unitSize' is missing from the "
        "<CompartmentMapping> element.";
      log->logPackageError("spatial",
        SpatialCompartmentMappingAllowedAttributes, pkgVersion, level, version,
          message, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
CompartmentMapping::writeAttributes(XMLOutputStream& stream) const
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

  if (isSetUnitSize() == true)
  {
    stream.writeAttribute("unitSize", getPrefix(), mUnitSize);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CompartmentMapping_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CompartmentMapping_t *
CompartmentMapping_create(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion)
{
  return new CompartmentMapping(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CompartmentMapping_t object.
 */
LIBSBML_EXTERN
CompartmentMapping_t*
CompartmentMapping_clone(const CompartmentMapping_t* cm)
{
  if (cm != NULL)
  {
    return static_cast<CompartmentMapping_t*>(cm->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this CompartmentMapping_t object.
 */
LIBSBML_EXTERN
void
CompartmentMapping_free(CompartmentMapping_t* cm)
{
  if (cm != NULL)
  {
    delete cm;
  }
}


/*
 * Returns the value of the "id" attribute of this CompartmentMapping_t.
 */
LIBSBML_EXTERN
char *
CompartmentMapping_getId(const CompartmentMapping_t * cm)
{
  if (cm == NULL)
  {
    return NULL;
  }

  return cm->getId().empty() ? NULL : safe_strdup(cm->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this CompartmentMapping_t.
 */
LIBSBML_EXTERN
char *
CompartmentMapping_getName(const CompartmentMapping_t * cm)
{
  if (cm == NULL)
  {
    return NULL;
  }

  return cm->getName().empty() ? NULL : safe_strdup(cm->getName().c_str());
}


/*
 * Returns the value of the "domainType" attribute of this
 * CompartmentMapping_t.
 */
LIBSBML_EXTERN
char *
CompartmentMapping_getDomainType(const CompartmentMapping_t * cm)
{
  if (cm == NULL)
  {
    return NULL;
  }

  return cm->getDomainType().empty() ? NULL :
    safe_strdup(cm->getDomainType().c_str());
}


/*
 * Returns the value of the "unitSize" attribute of this CompartmentMapping_t.
 */
LIBSBML_EXTERN
double
CompartmentMapping_getUnitSize(const CompartmentMapping_t * cm)
{
  return (cm != NULL) ? cm->getUnitSize() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this CompartmentMapping_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CompartmentMapping_isSetId(const CompartmentMapping_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CompartmentMapping_t's "name"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CompartmentMapping_isSetName(const CompartmentMapping_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CompartmentMapping_t's "domainType"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CompartmentMapping_isSetDomainType(const CompartmentMapping_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->isSetDomainType()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CompartmentMapping_t's "unitSize"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CompartmentMapping_isSetUnitSize(const CompartmentMapping_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->isSetUnitSize()) : 0;
}


/*
 * Sets the value of the "id" attribute of this CompartmentMapping_t.
 */
LIBSBML_EXTERN
int
CompartmentMapping_setId(CompartmentMapping_t * cm, const char * id)
{
  return (cm != NULL) ? cm->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this CompartmentMapping_t.
 */
LIBSBML_EXTERN
int
CompartmentMapping_setName(CompartmentMapping_t * cm, const char * name)
{
  return (cm != NULL) ? cm->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "domainType" attribute of this CompartmentMapping_t.
 */
LIBSBML_EXTERN
int
CompartmentMapping_setDomainType(CompartmentMapping_t * cm,
                                 const char * domainType)
{
  return (cm != NULL) ? cm->setDomainType(domainType) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "unitSize" attribute of this CompartmentMapping_t.
 */
LIBSBML_EXTERN
int
CompartmentMapping_setUnitSize(CompartmentMapping_t * cm, double unitSize)
{
  return (cm != NULL) ? cm->setUnitSize(unitSize) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this CompartmentMapping_t.
 */
LIBSBML_EXTERN
int
CompartmentMapping_unsetId(CompartmentMapping_t * cm)
{
  return (cm != NULL) ? cm->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this CompartmentMapping_t.
 */
LIBSBML_EXTERN
int
CompartmentMapping_unsetName(CompartmentMapping_t * cm)
{
  return (cm != NULL) ? cm->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "domainType" attribute of this CompartmentMapping_t.
 */
LIBSBML_EXTERN
int
CompartmentMapping_unsetDomainType(CompartmentMapping_t * cm)
{
  return (cm != NULL) ? cm->unsetDomainType() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "unitSize" attribute of this CompartmentMapping_t.
 */
LIBSBML_EXTERN
int
CompartmentMapping_unsetUnitSize(CompartmentMapping_t * cm)
{
  return (cm != NULL) ? cm->unsetUnitSize() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CompartmentMapping_t object have been set.
 */
LIBSBML_EXTERN
int
CompartmentMapping_hasRequiredAttributes(const CompartmentMapping_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


