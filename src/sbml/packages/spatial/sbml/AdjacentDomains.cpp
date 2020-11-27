/**
 * @file AdjacentDomains.cpp
 * @brief Implementation of the AdjacentDomains class.
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
#include <sbml/packages/spatial/sbml/AdjacentDomains.h>
#include <sbml/packages/spatial/sbml/ListOfAdjacentDomains.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new AdjacentDomains using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
AdjacentDomains::AdjacentDomains(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion)
  : SBase(level, version)
  , mDomain1 ("")
  , mDomain2 ("")
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new AdjacentDomains using the given SpatialPkgNamespaces object.
 */
AdjacentDomains::AdjacentDomains(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mDomain1 ("")
  , mDomain2 ("")
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for AdjacentDomains.
 */
AdjacentDomains::AdjacentDomains(const AdjacentDomains& orig)
  : SBase( orig )
  , mDomain1 ( orig.mDomain1 )
  , mDomain2 ( orig.mDomain2 )
{
}


/*
 * Assignment operator for AdjacentDomains.
 */
AdjacentDomains&
AdjacentDomains::operator=(const AdjacentDomains& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mDomain1 = rhs.mDomain1;
    mDomain2 = rhs.mDomain2;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this AdjacentDomains object.
 */
AdjacentDomains*
AdjacentDomains::clone() const
{
  return new AdjacentDomains(*this);
}


/*
 * Destructor for AdjacentDomains.
 */
AdjacentDomains::~AdjacentDomains()
{
}


/*
 * Returns the value of the "id" attribute of this AdjacentDomains.
 */
const std::string&
AdjacentDomains::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this AdjacentDomains.
 */
const std::string&
AdjacentDomains::getName() const
{
  return mName;
}


/*
 * Returns the value of the "domain1" attribute of this AdjacentDomains.
 */
const std::string&
AdjacentDomains::getDomain1() const
{
  return mDomain1;
}


/*
 * Returns the value of the "domain2" attribute of this AdjacentDomains.
 */
const std::string&
AdjacentDomains::getDomain2() const
{
  return mDomain2;
}


/*
 * Predicate returning @c true if this AdjacentDomains's "id" attribute is set.
 */
bool
AdjacentDomains::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this AdjacentDomains's "name" attribute is
 * set.
 */
bool
AdjacentDomains::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Predicate returning @c true if this AdjacentDomains's "domain1" attribute is
 * set.
 */
bool
AdjacentDomains::isSetDomain1() const
{
  return (mDomain1.empty() == false);
}


/*
 * Predicate returning @c true if this AdjacentDomains's "domain2" attribute is
 * set.
 */
bool
AdjacentDomains::isSetDomain2() const
{
  return (mDomain2.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "domain1" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::setDomain1(const std::string& domain1)
{
  if (!(SyntaxChecker::isValidInternalSId(domain1)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomain1 = domain1;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "domain2" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::setDomain2(const std::string& domain2)
{
  if (!(SyntaxChecker::isValidInternalSId(domain2)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomain2 = domain2;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the value of the "id" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::unsetId()
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
 * Unsets the value of the "name" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::unsetName()
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
 * Unsets the value of the "domain1" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::unsetDomain1()
{
  mDomain1.erase();

  if (mDomain1.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "domain2" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::unsetDomain2()
{
  mDomain2.erase();

  if (mDomain2.empty() == true)
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
AdjacentDomains::renameSIdRefs(const std::string& oldid,
                               const std::string& newid)
{
  if (isSetDomain1() && mDomain1 == oldid)
  {
    setDomain1(newid);
  }

  if (isSetDomain2() && mDomain2 == oldid)
  {
    setDomain2(newid);
  }
}


/*
 * Returns the XML element name of this AdjacentDomains object.
 */
const std::string&
AdjacentDomains::getElementName() const
{
  static const string name = "adjacentDomains";
  return name;
}


/*
 * Returns the libSBML type code for this AdjacentDomains object.
 */
int
AdjacentDomains::getTypeCode() const
{
  return SBML_SPATIAL_ADJACENTDOMAINS;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * AdjacentDomains object have been set.
 */
bool
AdjacentDomains::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetId() == false)
  {
    allPresent = false;
  }

  if (isSetDomain1() == false)
  {
    allPresent = false;
  }

  if (isSetDomain2() == false)
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
AdjacentDomains::writeElements(XMLOutputStream& stream) const
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
AdjacentDomains::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
AdjacentDomains::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
AdjacentDomains::enablePackageInternal(const std::string& pkgURI,
                                       const std::string& pkgPrefix,
                                       bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::getAttribute(const std::string& attributeName,
                              bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::getAttribute(const std::string& attributeName,
                              int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::getAttribute(const std::string& attributeName,
                              double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::getAttribute(const std::string& attributeName,
                              unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::getAttribute(const std::string& attributeName,
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
  else if (attributeName == "domain1")
  {
    value = getDomain1();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "domain2")
  {
    value = getDomain2();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this AdjacentDomains's attribute
 * "attributeName" is set.
 */
bool
AdjacentDomains::isSetAttribute(const std::string& attributeName) const
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
  else if (attributeName == "domain1")
  {
    value = isSetDomain1();
  }
  else if (attributeName == "domain2")
  {
    value = isSetDomain2();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::setAttribute(const std::string& attributeName,
                              unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::setAttribute(const std::string& attributeName,
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
  else if (attributeName == "domain1")
  {
    return_value = setDomain1(value);
  }
  else if (attributeName == "domain2")
  {
    return_value = setDomain2(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this AdjacentDomains.
 */
int
AdjacentDomains::unsetAttribute(const std::string& attributeName)
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
  else if (attributeName == "domain1")
  {
    value = unsetDomain1();
  }
  else if (attributeName == "domain2")
  {
    value = unsetDomain2();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
AdjacentDomains::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");

  attributes.add("name");

  attributes.add("domain1");

  attributes.add("domain2");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
AdjacentDomains::readAttributes(const XMLAttributes& attributes,
                                const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfAdjacentDomains*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial",
          SpatialAdjacentDomainsAllowedAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialGeometryLOAdjacentDomainsAllowedCoreAttributes, pkgVersion,
            level, version, details, getLine(), getColumn());
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
          SpatialAdjacentDomainsAllowedAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialAdjacentDomainsAllowedCoreAttributes, pkgVersion, level,
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
      logEmptyString(mId, level, version, "<AdjacentDomains>");
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
      "<AdjacentDomains> element.";
    log->logPackageError("spatial", SpatialAdjacentDomainsAllowedAttributes,
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
      logEmptyString(mName, level, version, "<AdjacentDomains>");
    }
  }

  // 
  // domain1 SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("domain1", mDomain1);

  if (assigned == true)
  {
    if (mDomain1.empty() == true)
    {
      logEmptyString(mDomain1, level, version, "<AdjacentDomains>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomain1) == false)
    {
      std::string msg = "The domain1 attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mDomain1 + "', which does not conform to the syntax.";
      log->logPackageError("spatial",
        SpatialAdjacentDomainsDomain1MustBeDomain, pkgVersion, level, version,
          msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domain1' is missing from the "
      "<AdjacentDomains> element.";
    log->logPackageError("spatial", SpatialAdjacentDomainsAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }

  // 
  // domain2 SIdRef (use = "required" )
  // 

  assigned = attributes.readInto("domain2", mDomain2);

  if (assigned == true)
  {
    if (mDomain2.empty() == true)
    {
      logEmptyString(mDomain2, level, version, "<AdjacentDomains>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomain2) == false)
    {
      std::string msg = "The domain2 attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mDomain2 + "', which does not conform to the syntax.";
      log->logPackageError("spatial",
        SpatialAdjacentDomainsDomain2MustBeDomain, pkgVersion, level, version,
          msg, getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domain2' is missing from the "
      "<AdjacentDomains> element.";
    log->logPackageError("spatial", SpatialAdjacentDomainsAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
AdjacentDomains::writeAttributes(XMLOutputStream& stream) const
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

  if (isSetDomain1() == true)
  {
    stream.writeAttribute("domain1", getPrefix(), mDomain1);
  }

  if (isSetDomain2() == true)
  {
    stream.writeAttribute("domain2", getPrefix(), mDomain2);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new AdjacentDomains_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
AdjacentDomains_t *
AdjacentDomains_create(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion)
{
  return new AdjacentDomains(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this AdjacentDomains_t object.
 */
LIBSBML_EXTERN
AdjacentDomains_t*
AdjacentDomains_clone(const AdjacentDomains_t* ad)
{
  if (ad != NULL)
  {
    return static_cast<AdjacentDomains_t*>(ad->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this AdjacentDomains_t object.
 */
LIBSBML_EXTERN
void
AdjacentDomains_free(AdjacentDomains_t* ad)
{
  if (ad != NULL)
  {
    delete ad;
  }
}


/*
 * Returns the value of the "id" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
char *
AdjacentDomains_getId(const AdjacentDomains_t * ad)
{
  if (ad == NULL)
  {
    return NULL;
  }

  return ad->getId().empty() ? NULL : safe_strdup(ad->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
char *
AdjacentDomains_getName(const AdjacentDomains_t * ad)
{
  if (ad == NULL)
  {
    return NULL;
  }

  return ad->getName().empty() ? NULL : safe_strdup(ad->getName().c_str());
}


/*
 * Returns the value of the "domain1" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
char *
AdjacentDomains_getDomain1(const AdjacentDomains_t * ad)
{
  if (ad == NULL)
  {
    return NULL;
  }

  return ad->getDomain1().empty() ? NULL :
    safe_strdup(ad->getDomain1().c_str());
}


/*
 * Returns the value of the "domain2" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
char *
AdjacentDomains_getDomain2(const AdjacentDomains_t * ad)
{
  if (ad == NULL)
  {
    return NULL;
  }

  return ad->getDomain2().empty() ? NULL :
    safe_strdup(ad->getDomain2().c_str());
}


/*
 * Predicate returning @c 1 (true) if this AdjacentDomains_t's "id" attribute
 * is set.
 */
LIBSBML_EXTERN
int
AdjacentDomains_isSetId(const AdjacentDomains_t * ad)
{
  return (ad != NULL) ? static_cast<int>(ad->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this AdjacentDomains_t's "name" attribute
 * is set.
 */
LIBSBML_EXTERN
int
AdjacentDomains_isSetName(const AdjacentDomains_t * ad)
{
  return (ad != NULL) ? static_cast<int>(ad->isSetName()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this AdjacentDomains_t's "domain1"
 * attribute is set.
 */
LIBSBML_EXTERN
int
AdjacentDomains_isSetDomain1(const AdjacentDomains_t * ad)
{
  return (ad != NULL) ? static_cast<int>(ad->isSetDomain1()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this AdjacentDomains_t's "domain2"
 * attribute is set.
 */
LIBSBML_EXTERN
int
AdjacentDomains_isSetDomain2(const AdjacentDomains_t * ad)
{
  return (ad != NULL) ? static_cast<int>(ad->isSetDomain2()) : 0;
}


/*
 * Sets the value of the "id" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
int
AdjacentDomains_setId(AdjacentDomains_t * ad, const char * id)
{
  return (ad != NULL) ? ad->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
int
AdjacentDomains_setName(AdjacentDomains_t * ad, const char * name)
{
  return (ad != NULL) ? ad->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "domain1" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
int
AdjacentDomains_setDomain1(AdjacentDomains_t * ad, const char * domain1)
{
  return (ad != NULL) ? ad->setDomain1(domain1) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "domain2" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
int
AdjacentDomains_setDomain2(AdjacentDomains_t * ad, const char * domain2)
{
  return (ad != NULL) ? ad->setDomain2(domain2) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
int
AdjacentDomains_unsetId(AdjacentDomains_t * ad)
{
  return (ad != NULL) ? ad->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
int
AdjacentDomains_unsetName(AdjacentDomains_t * ad)
{
  return (ad != NULL) ? ad->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "domain1" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
int
AdjacentDomains_unsetDomain1(AdjacentDomains_t * ad)
{
  return (ad != NULL) ? ad->unsetDomain1() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "domain2" attribute of this AdjacentDomains_t.
 */
LIBSBML_EXTERN
int
AdjacentDomains_unsetDomain2(AdjacentDomains_t * ad)
{
  return (ad != NULL) ? ad->unsetDomain2() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * AdjacentDomains_t object have been set.
 */
LIBSBML_EXTERN
int
AdjacentDomains_hasRequiredAttributes(const AdjacentDomains_t * ad)
{
  return (ad != NULL) ? static_cast<int>(ad->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


