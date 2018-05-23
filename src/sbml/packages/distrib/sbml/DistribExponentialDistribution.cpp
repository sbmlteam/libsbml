/**
 * @file DistribExponentialDistribution.cpp
 * @brief Implementation of the DistribExponentialDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribExponentialDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribExponentialDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribExponentialDistribution::DistribExponentialDistribution(
                                                               unsigned int
                                                                 level,
                                                               unsigned int
                                                                 version,
                                                               unsigned int
                                                                 pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version)
  , mRate (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribExponentialDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribExponentialDistribution::DistribExponentialDistribution(DistribPkgNamespaces
  *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mRate (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribExponentialDistribution.
 */
DistribExponentialDistribution::DistribExponentialDistribution(const
  DistribExponentialDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mRate ( NULL )
{
  if (orig.mRate != NULL)
  {
    mRate = orig.mRate->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribExponentialDistribution.
 */
DistribExponentialDistribution&
DistribExponentialDistribution::operator=(const DistribExponentialDistribution&
  rhs)
{
  if (&rhs != this)
  {
    DistribContinuousUnivariateDistribution::operator=(rhs);
    delete mRate;
    if (rhs.mRate != NULL)
    {
      mRate = rhs.mRate->clone();
    }
    else
    {
      mRate = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribExponentialDistribution
 * object.
 */
DistribExponentialDistribution*
DistribExponentialDistribution::clone() const
{
  return new DistribExponentialDistribution(*this);
}


/*
 * Destructor for DistribExponentialDistribution.
 */
DistribExponentialDistribution::~DistribExponentialDistribution()
{
  delete mRate;
  mRate = NULL;
}


/*
 * Returns the value of the "id" attribute of this
 * DistribExponentialDistribution.
 */
const std::string&
DistribExponentialDistribution::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * DistribExponentialDistribution.
 */
const std::string&
DistribExponentialDistribution::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribExponentialDistribution's "id"
 * attribute is set.
 */
bool
DistribExponentialDistribution::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribExponentialDistribution's "name"
 * attribute is set.
 */
bool
DistribExponentialDistribution::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::unsetId()
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
 * Unsets the value of the "name" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::unsetName()
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
 * Returns the value of the "rate" element of this
 * DistribExponentialDistribution.
 */
const DistribUncertValue*
DistribExponentialDistribution::getRate() const
{
  return mRate;
}


/*
 * Returns the value of the "rate" element of this
 * DistribExponentialDistribution.
 */
DistribUncertValue*
DistribExponentialDistribution::getRate()
{
  return mRate;
}


/*
 * Predicate returning @c true if this DistribExponentialDistribution's "rate"
 * element is set.
 */
bool
DistribExponentialDistribution::isSetRate() const
{
  return (mRate != NULL);
}


/*
 * Sets the value of the "rate" element of this DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::setRate(const DistribUncertValue* rate)
{
  if (rate == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (rate->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != rate->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != rate->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != rate->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mRate;
    mRate = (rate != NULL) ? static_cast<DistribUncertValue*>(rate->clone()) :
      NULL;
    if (mRate != NULL) mRate->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribExponentialDistribution object and returns the DistribUncertValue
 * object created.
 */
DistribUncertValue*
DistribExponentialDistribution::createRate()
{
  if (mRate != NULL)
  {
    delete mRate;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mRate = new DistribUncertValue(distribns);

  mRate->setElementName("rate");

  delete distribns;

  connectToChild();

  return mRate;
}


/*
 * Unsets the value of the "rate" element of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::unsetRate()
{
  delete mRate;
  mRate = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribExponentialDistribution object.
 */
const std::string&
DistribExponentialDistribution::getElementName() const
{
  static const string name = "exponentialDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribExponentialDistribution
 * object.
 */
int
DistribExponentialDistribution::getTypeCode() const
{
  return SBML_DISTRIB_EXPONENTIALDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribExponentialDistribution object have been set.
 */
bool
DistribExponentialDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribExponentialDistribution object have been set.
 */
bool
DistribExponentialDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredElements();

  if (isSetRate() == false)
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
DistribExponentialDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeElements(stream);

  if (isSetRate() == true)
  {
    mRate->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribExponentialDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mRate != NULL)
  {
    mRate->accept(v);
  }

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribExponentialDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribContinuousUnivariateDistribution::setSBMLDocument(d);

  if (mRate != NULL)
  {
    mRate->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribExponentialDistribution::connectToChild()
{
  DistribContinuousUnivariateDistribution::connectToChild();

  if (mRate != NULL)
  {
    mRate->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribExponentialDistribution::enablePackageInternal(
                                                      const std::string&
                                                        pkgURI,
                                                      const std::string&
                                                        pkgPrefix,
                                                      bool flag)
{
  DistribContinuousUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetRate())
  {
    mRate->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribExponentialDistribution::updateSBMLNamespace(const std::string& package,
                                                    unsigned int level,
                                                    unsigned int version)
{
  DistribContinuousUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mRate != NULL)
  {
    mRate->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::getAttribute(const std::string& attributeName,
                                             bool& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::getAttribute(const std::string& attributeName,
                                             int& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::getAttribute(const std::string& attributeName,
                                             double& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::getAttribute(const std::string& attributeName,
                                             unsigned int& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::getAttribute(const std::string& attributeName,
                                             std::string& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

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
 * Predicate returning @c true if this DistribExponentialDistribution's
 * attribute "attributeName" is set.
 */
bool
DistribExponentialDistribution::isSetAttribute(const std::string&
  attributeName) const
{
  bool value =
    DistribContinuousUnivariateDistribution::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::setAttribute(const std::string& attributeName,
                                             bool value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::setAttribute(const std::string& attributeName,
                                             int value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::setAttribute(const std::string& attributeName,
                                             double value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::setAttribute(const std::string& attributeName,
                                             unsigned int value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::setAttribute(const std::string& attributeName,
                                             const std::string& value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

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
 * DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::unsetAttribute(const std::string&
  attributeName)
{
  int value =
    DistribContinuousUnivariateDistribution::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribExponentialDistribution.
 */
SBase*
DistribExponentialDistribution::createChildObject(const std::string&
  elementName)
{
  DistribContinuousUnivariateDistribution* obj = NULL;

  if (elementName == "rate")
  {
    return createRate();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribExponentialDistribution.
 */
int
DistribExponentialDistribution::addChildObject(const std::string& elementName,
                                               const SBase* element)
{
  if (elementName == "rate" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setRate((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribExponentialDistribution.
 */
SBase*
DistribExponentialDistribution::removeChildObject(
                                                  const std::string&
                                                    elementName,
                                                  const std::string& id)
{
  if (elementName == "rate")
  {
    DistribUncertValue * obj = getRate();
    if (unsetRate() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribExponentialDistribution.
 */
unsigned int
DistribExponentialDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "rate")
  {
    if (isSetRate())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this
 * DistribExponentialDistribution.
 */
SBase*
DistribExponentialDistribution::getObject(const std::string& elementName,
                                          unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "rate")
  {
    return getRate();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribExponentialDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mRate != NULL)
  {
    if (mRate->getId() == id)
    {
      return mRate;
    }

    obj = mRate->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
DistribExponentialDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mRate != NULL)
  {
    if (mRate->getMetaId() == metaid)
    {
      return mRate;
    }

    obj = mRate->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
DistribExponentialDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mRate, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribExponentialDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "rate")
  {
    if (isSetRate())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribExponentialDistributionAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
    }

    delete mRate;
    mRate = new DistribUncertValue(distribns);
    mRate->setElementName(name);
    obj = mRate;
  }

  delete distribns;

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DistribExponentialDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribContinuousUnivariateDistribution::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
    attributes.add("id");
    attributes.add("name");
  }

  if (level == 3 && coreVersion == 2 && pkgVersion == 1)
  {
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribExponentialDistribution::readAttributes(const XMLAttributes& attributes,
                                               const ExpectedAttributes&
                                                 expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribContinuousUnivariateDistribution::readAttributes(attributes,
    expectedAttributes);

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
          DistribDistribExponentialDistributionAllowedAttributes, pkgVersion,
            level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribExponentialDistributionAllowedCoreAttributes, pkgVersion,
            level, version, details);
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
DistribExponentialDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeAttributes(stream);
  SBase::writeExtensionAttributes(stream);
}

/** @endcond */


#endif /* __cplusplus */


/*
 * Creates a new DistribExponentialDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribExponentialDistribution_t *
DistribExponentialDistribution_create(unsigned int level,
                                      unsigned int version,
                                      unsigned int pkgVersion)
{
  return new DistribExponentialDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribExponentialDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribExponentialDistribution_t*
DistribExponentialDistribution_clone(const DistribExponentialDistribution_t*
  ded)
{
  if (ded != NULL)
  {
    return static_cast<DistribExponentialDistribution_t*>(ded->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribExponentialDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribExponentialDistribution_free(DistribExponentialDistribution_t* ded)
{
  if (ded != NULL)
  {
    delete ded;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * DistribExponentialDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribExponentialDistribution_getId(const DistribExponentialDistribution_t *
  ded)
{
  if (ded == NULL)
  {
    return NULL;
  }

  return ded->getId().empty() ? NULL : safe_strdup(ded->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * DistribExponentialDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribExponentialDistribution_getName(const DistribExponentialDistribution_t *
  ded)
{
  if (ded == NULL)
  {
    return NULL;
  }

  return ded->getName().empty() ? NULL : safe_strdup(ded->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribExponentialDistribution_t's
 * "id" attribute is set.
 */
LIBSBML_EXTERN
int
DistribExponentialDistribution_isSetId(const DistribExponentialDistribution_t *
  ded)
{
  return (ded != NULL) ? static_cast<int>(ded->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribExponentialDistribution_t's
 * "name" attribute is set.
 */
LIBSBML_EXTERN
int
DistribExponentialDistribution_isSetName(const DistribExponentialDistribution_t
  * ded)
{
  return (ded != NULL) ? static_cast<int>(ded->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this
 * DistribExponentialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribExponentialDistribution_setId(DistribExponentialDistribution_t * ded,
                                     const char * id)
{
  return (ded != NULL) ? ded->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this
 * DistribExponentialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribExponentialDistribution_setName(DistribExponentialDistribution_t * ded,
                                       const char * name)
{
  return (ded != NULL) ? ded->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribExponentialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribExponentialDistribution_unsetId(DistribExponentialDistribution_t * ded)
{
  return (ded != NULL) ? ded->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribExponentialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribExponentialDistribution_unsetName(DistribExponentialDistribution_t *
  ded)
{
  return (ded != NULL) ? ded->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "rate" element of this
 * DistribExponentialDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribExponentialDistribution_getRate(const DistribExponentialDistribution_t *
  ded)
{
  if (ded == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(ded->getRate());
}


/*
 * Predicate returning @c 1 (true) if this DistribExponentialDistribution_t's
 * "rate" element is set.
 */
LIBSBML_EXTERN
int
DistribExponentialDistribution_isSetRate(const DistribExponentialDistribution_t
  * ded)
{
  return (ded != NULL) ? static_cast<int>(ded->isSetRate()) : 0;
}


/*
 * Sets the value of the "rate" element of this
 * DistribExponentialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribExponentialDistribution_setRate(DistribExponentialDistribution_t * ded,
                                       const DistribUncertValue_t* rate)
{
  return (ded != NULL) ? ded->setRate(rate) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribExponentialDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribExponentialDistribution_createRate(DistribExponentialDistribution_t*
  ded)
{
  if (ded == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(ded->createRate());
}


/*
 * Unsets the value of the "rate" element of this
 * DistribExponentialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribExponentialDistribution_unsetRate(DistribExponentialDistribution_t *
  ded)
{
  return (ded != NULL) ? ded->unsetRate() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribExponentialDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribExponentialDistribution_hasRequiredAttributes(const
  DistribExponentialDistribution_t * ded)
{
  return (ded != NULL) ? static_cast<int>(ded->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribExponentialDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribExponentialDistribution_hasRequiredElements(const
  DistribExponentialDistribution_t * ded)
{
  return (ded != NULL) ? static_cast<int>(ded->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


