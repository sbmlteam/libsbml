/**
 * @file DistribChiSquareDistribution.cpp
 * @brief Implementation of the DistribChiSquareDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribChiSquareDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribChiSquareDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribChiSquareDistribution::DistribChiSquareDistribution(unsigned int level,
                                                           unsigned int
                                                             version,
                                                           unsigned int
                                                             pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version)
  , mDegreesOfFreedom (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribChiSquareDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribChiSquareDistribution::DistribChiSquareDistribution(DistribPkgNamespaces
  *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mDegreesOfFreedom (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribChiSquareDistribution.
 */
DistribChiSquareDistribution::DistribChiSquareDistribution(const
  DistribChiSquareDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mDegreesOfFreedom ( NULL )
{
  if (orig.mDegreesOfFreedom != NULL)
  {
    mDegreesOfFreedom = orig.mDegreesOfFreedom->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribChiSquareDistribution.
 */
DistribChiSquareDistribution&
DistribChiSquareDistribution::operator=(const DistribChiSquareDistribution&
  rhs)
{
  if (&rhs != this)
  {
    DistribContinuousUnivariateDistribution::operator=(rhs);
    delete mDegreesOfFreedom;
    if (rhs.mDegreesOfFreedom != NULL)
    {
      mDegreesOfFreedom = rhs.mDegreesOfFreedom->clone();
    }
    else
    {
      mDegreesOfFreedom = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribChiSquareDistribution object.
 */
DistribChiSquareDistribution*
DistribChiSquareDistribution::clone() const
{
  return new DistribChiSquareDistribution(*this);
}


/*
 * Destructor for DistribChiSquareDistribution.
 */
DistribChiSquareDistribution::~DistribChiSquareDistribution()
{
  delete mDegreesOfFreedom;
  mDegreesOfFreedom = NULL;
}


/*
 * Returns the value of the "id" attribute of this
 * DistribChiSquareDistribution.
 */
const std::string&
DistribChiSquareDistribution::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * DistribChiSquareDistribution.
 */
const std::string&
DistribChiSquareDistribution::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribChiSquareDistribution's "id"
 * attribute is set.
 */
bool
DistribChiSquareDistribution::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribChiSquareDistribution's "name"
 * attribute is set.
 */
bool
DistribChiSquareDistribution::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::unsetId()
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
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::unsetName()
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
 * Returns the value of the "degreesOfFreedom" element of this
 * DistribChiSquareDistribution.
 */
const DistribUncertValue*
DistribChiSquareDistribution::getDegreesOfFreedom() const
{
  return mDegreesOfFreedom;
}


/*
 * Returns the value of the "degreesOfFreedom" element of this
 * DistribChiSquareDistribution.
 */
DistribUncertValue*
DistribChiSquareDistribution::getDegreesOfFreedom()
{
  return mDegreesOfFreedom;
}


/*
 * Predicate returning @c true if this DistribChiSquareDistribution's
 * "degreesOfFreedom" element is set.
 */
bool
DistribChiSquareDistribution::isSetDegreesOfFreedom() const
{
  return (mDegreesOfFreedom != NULL);
}


/*
 * Sets the value of the "degreesOfFreedom" element of this
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::setDegreesOfFreedom(const DistribUncertValue*
  degreesOfFreedom)
{
  if (degreesOfFreedom == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (degreesOfFreedom->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != degreesOfFreedom->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != degreesOfFreedom->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != degreesOfFreedom->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mDegreesOfFreedom;
    mDegreesOfFreedom = (degreesOfFreedom != NULL) ?
      static_cast<DistribUncertValue*>(degreesOfFreedom->clone()) : NULL;
    if (mDegreesOfFreedom != NULL) mDegreesOfFreedom->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribChiSquareDistribution object and returns the DistribUncertValue
 * object created.
 */
DistribUncertValue*
DistribChiSquareDistribution::createDegreesOfFreedom()
{
  if (mDegreesOfFreedom != NULL)
  {
    delete mDegreesOfFreedom;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDegreesOfFreedom = new DistribUncertValue(distribns);

  mDegreesOfFreedom->setElementName("degreesOfFreedom");

  delete distribns;

  connectToChild();

  return mDegreesOfFreedom;
}


/*
 * Unsets the value of the "degreesOfFreedom" element of this
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::unsetDegreesOfFreedom()
{
  delete mDegreesOfFreedom;
  mDegreesOfFreedom = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribChiSquareDistribution object.
 */
const std::string&
DistribChiSquareDistribution::getElementName() const
{
  static const string name = "chiSquareDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribChiSquareDistribution object.
 */
int
DistribChiSquareDistribution::getTypeCode() const
{
  return SBML_DISTRIB_CHISQUAREDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribChiSquareDistribution object have been set.
 */
bool
DistribChiSquareDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribChiSquareDistribution object have been set.
 */
bool
DistribChiSquareDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredElements();

  if (isSetDegreesOfFreedom() == false)
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
DistribChiSquareDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeElements(stream);

  if (isSetDegreesOfFreedom() == true)
  {
    mDegreesOfFreedom->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribChiSquareDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mDegreesOfFreedom != NULL)
  {
    mDegreesOfFreedom->accept(v);
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
DistribChiSquareDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribContinuousUnivariateDistribution::setSBMLDocument(d);

  if (mDegreesOfFreedom != NULL)
  {
    mDegreesOfFreedom->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribChiSquareDistribution::connectToChild()
{
  DistribContinuousUnivariateDistribution::connectToChild();

  if (mDegreesOfFreedom != NULL)
  {
    mDegreesOfFreedom->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribChiSquareDistribution::enablePackageInternal(const std::string& pkgURI,
                                                    const std::string&
                                                      pkgPrefix,
                                                    bool flag)
{
  DistribContinuousUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetDegreesOfFreedom())
  {
    mDegreesOfFreedom->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribChiSquareDistribution::updateSBMLNamespace(const std::string& package,
                                                  unsigned int level,
                                                  unsigned int version)
{
  DistribContinuousUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mDegreesOfFreedom != NULL)
  {
    mDegreesOfFreedom->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::getAttribute(const std::string& attributeName,
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
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::getAttribute(const std::string& attributeName,
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
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::getAttribute(const std::string& attributeName,
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
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::getAttribute(const std::string& attributeName,
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
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this DistribChiSquareDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribChiSquareDistribution::isSetAttribute(const std::string& attributeName)
  const
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
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::setAttribute(const std::string& attributeName,
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
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::setAttribute(const std::string& attributeName,
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
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::setAttribute(const std::string& attributeName,
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
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::setAttribute(const std::string& attributeName,
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
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::setAttribute(const std::string& attributeName,
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
 * DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::unsetAttribute(const std::string& attributeName)
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
 * DistribChiSquareDistribution.
 */
SBase*
DistribChiSquareDistribution::createChildObject(const std::string& elementName)
{
  DistribContinuousUnivariateDistribution* obj = NULL;

  if (elementName == "degreesOfFreedom")
  {
    return createDegreesOfFreedom();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribChiSquareDistribution.
 */
int
DistribChiSquareDistribution::addChildObject(const std::string& elementName,
                                             const SBase* element)
{
  if (elementName == "degreesOfFreedom" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setDegreesOfFreedom((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribChiSquareDistribution.
 */
SBase*
DistribChiSquareDistribution::removeChildObject(const std::string& elementName,
                                                const std::string& id)
{
  if (elementName == "degreesOfFreedom")
  {
    DistribUncertValue * obj = getDegreesOfFreedom();
    if (unsetDegreesOfFreedom() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribChiSquareDistribution.
 */
unsigned int
DistribChiSquareDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "degreesOfFreedom")
  {
    if (isSetDegreesOfFreedom())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribChiSquareDistribution.
 */
SBase*
DistribChiSquareDistribution::getObject(const std::string& elementName,
                                        unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "degreesOfFreedom")
  {
    return getDegreesOfFreedom();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribChiSquareDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDegreesOfFreedom != NULL)
  {
    if (mDegreesOfFreedom->getId() == id)
    {
      return mDegreesOfFreedom;
    }

    obj = mDegreesOfFreedom->getElementBySId(id);
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
DistribChiSquareDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDegreesOfFreedom != NULL)
  {
    if (mDegreesOfFreedom->getMetaId() == metaid)
    {
      return mDegreesOfFreedom;
    }

    obj = mDegreesOfFreedom->getElementByMetaId(metaid);
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
DistribChiSquareDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mDegreesOfFreedom, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribChiSquareDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "degreesOfFreedom")
  {
    if (isSetDegreesOfFreedom())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribChiSquareDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDegreesOfFreedom;
    mDegreesOfFreedom = new DistribUncertValue(distribns);
    mDegreesOfFreedom->setElementName(name);
    obj = mDegreesOfFreedom;
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
DistribChiSquareDistribution::addExpectedAttributes(ExpectedAttributes&
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
DistribChiSquareDistribution::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribChiSquareDistributionAllowedAttributes, pkgVersion,
            level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribChiSquareDistributionAllowedCoreAttributes, pkgVersion,
            level, version, details);
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
DistribChiSquareDistribution::readL3V1V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // id SId (use = "optional" )
  // 

  XMLTriple tripleID("id", mURI, getPrefix());
  assigned = attributes.readInto(tripleID, mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribChiSquareDistribution>");
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
      logEmptyString(mName, level, version, "<DistribChiSquareDistribution>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribChiSquareDistribution::readL3V2V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();

  // 
  // id SId (use = "optional" )
  // 

  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    if (mId.empty() == true)
    {
      logEmptyString(mId, level, version, "<DistribChiSquareDistribution>");
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
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribChiSquareDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeAttributes(stream);

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
DistribChiSquareDistribution::writeL3V1V1Attributes(XMLOutputStream& stream)
  const
{
  if (isSetId() == true)
  {
    stream.writeAttribute("id", getPrefix(), mId);
  }

  if (isSetName() == true)
  {
    stream.writeAttribute("name", getPrefix(), mName);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribChiSquareDistribution::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribChiSquareDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribChiSquareDistribution_t *
DistribChiSquareDistribution_create(unsigned int level,
                                    unsigned int version,
                                    unsigned int pkgVersion)
{
  return new DistribChiSquareDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribChiSquareDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribChiSquareDistribution_t*
DistribChiSquareDistribution_clone(const DistribChiSquareDistribution_t* dcsd)
{
  if (dcsd != NULL)
  {
    return static_cast<DistribChiSquareDistribution_t*>(dcsd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribChiSquareDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribChiSquareDistribution_free(DistribChiSquareDistribution_t* dcsd)
{
  if (dcsd != NULL)
  {
    delete dcsd;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * DistribChiSquareDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribChiSquareDistribution_getId(const DistribChiSquareDistribution_t * dcsd)
{
  if (dcsd == NULL)
  {
    return NULL;
  }

  return dcsd->getId().empty() ? NULL : safe_strdup(dcsd->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * DistribChiSquareDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribChiSquareDistribution_getName(const DistribChiSquareDistribution_t *
  dcsd)
{
  if (dcsd == NULL)
  {
    return NULL;
  }

  return dcsd->getName().empty() ? NULL : safe_strdup(dcsd->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribChiSquareDistribution_t's
 * "id" attribute is set.
 */
LIBSBML_EXTERN
int
DistribChiSquareDistribution_isSetId(const DistribChiSquareDistribution_t *
  dcsd)
{
  return (dcsd != NULL) ? static_cast<int>(dcsd->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribChiSquareDistribution_t's
 * "name" attribute is set.
 */
LIBSBML_EXTERN
int
DistribChiSquareDistribution_isSetName(const DistribChiSquareDistribution_t *
  dcsd)
{
  return (dcsd != NULL) ? static_cast<int>(dcsd->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribChiSquareDistribution_t.
 */
LIBSBML_EXTERN
int
DistribChiSquareDistribution_setId(DistribChiSquareDistribution_t * dcsd,
                                   const char * id)
{
  return (dcsd != NULL) ? dcsd->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this
 * DistribChiSquareDistribution_t.
 */
LIBSBML_EXTERN
int
DistribChiSquareDistribution_setName(DistribChiSquareDistribution_t * dcsd,
                                     const char * name)
{
  return (dcsd != NULL) ? dcsd->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribChiSquareDistribution_t.
 */
LIBSBML_EXTERN
int
DistribChiSquareDistribution_unsetId(DistribChiSquareDistribution_t * dcsd)
{
  return (dcsd != NULL) ? dcsd->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribChiSquareDistribution_t.
 */
LIBSBML_EXTERN
int
DistribChiSquareDistribution_unsetName(DistribChiSquareDistribution_t * dcsd)
{
  return (dcsd != NULL) ? dcsd->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "degreesOfFreedom" element of this
 * DistribChiSquareDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribChiSquareDistribution_getDegreesOfFreedom(const
  DistribChiSquareDistribution_t * dcsd)
{
  if (dcsd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dcsd->getDegreesOfFreedom());
}


/*
 * Predicate returning @c 1 (true) if this DistribChiSquareDistribution_t's
 * "degreesOfFreedom" element is set.
 */
LIBSBML_EXTERN
int
DistribChiSquareDistribution_isSetDegreesOfFreedom(const
  DistribChiSquareDistribution_t * dcsd)
{
  return (dcsd != NULL) ? static_cast<int>(dcsd->isSetDegreesOfFreedom()) : 0;
}


/*
 * Sets the value of the "degreesOfFreedom" element of this
 * DistribChiSquareDistribution_t.
 */
LIBSBML_EXTERN
int
DistribChiSquareDistribution_setDegreesOfFreedom(
                                                 DistribChiSquareDistribution_t
                                                   * dcsd,
                                                 const DistribUncertValue_t*
                                                   degreesOfFreedom)
{
  return (dcsd != NULL) ? dcsd->setDegreesOfFreedom(degreesOfFreedom) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribChiSquareDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribChiSquareDistribution_createDegreesOfFreedom(DistribChiSquareDistribution_t*
  dcsd)
{
  if (dcsd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dcsd->createDegreesOfFreedom());
}


/*
 * Unsets the value of the "degreesOfFreedom" element of this
 * DistribChiSquareDistribution_t.
 */
LIBSBML_EXTERN
int
DistribChiSquareDistribution_unsetDegreesOfFreedom(DistribChiSquareDistribution_t
  * dcsd)
{
  return (dcsd != NULL) ? dcsd->unsetDegreesOfFreedom() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribChiSquareDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribChiSquareDistribution_hasRequiredAttributes(const
  DistribChiSquareDistribution_t * dcsd)
{
  return (dcsd != NULL) ? static_cast<int>(dcsd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribChiSquareDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribChiSquareDistribution_hasRequiredElements(const
  DistribChiSquareDistribution_t * dcsd)
{
  return (dcsd != NULL) ? static_cast<int>(dcsd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


