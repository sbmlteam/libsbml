/**
 * @file DistribCauchyDistribution.cpp
 * @brief Implementation of the DistribCauchyDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribCauchyDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribCauchyDistribution using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
DistribCauchyDistribution::DistribCauchyDistribution(unsigned int level,
                                                     unsigned int version,
                                                     unsigned int pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version)
  , mLocation (NULL)
  , mScale (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribCauchyDistribution using the given DistribPkgNamespaces
 * object.
 */
DistribCauchyDistribution::DistribCauchyDistribution(DistribPkgNamespaces
  *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mLocation (NULL)
  , mScale (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribCauchyDistribution.
 */
DistribCauchyDistribution::DistribCauchyDistribution(const
  DistribCauchyDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mLocation ( NULL )
  , mScale ( NULL )
{
  if (orig.mLocation != NULL)
  {
    mLocation = orig.mLocation->clone();
  }

  if (orig.mScale != NULL)
  {
    mScale = orig.mScale->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribCauchyDistribution.
 */
DistribCauchyDistribution&
DistribCauchyDistribution::operator=(const DistribCauchyDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribContinuousUnivariateDistribution::operator=(rhs);
    delete mLocation;
    if (rhs.mLocation != NULL)
    {
      mLocation = rhs.mLocation->clone();
    }
    else
    {
      mLocation = NULL;
    }

    delete mScale;
    if (rhs.mScale != NULL)
    {
      mScale = rhs.mScale->clone();
    }
    else
    {
      mScale = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribCauchyDistribution object.
 */
DistribCauchyDistribution*
DistribCauchyDistribution::clone() const
{
  return new DistribCauchyDistribution(*this);
}


/*
 * Destructor for DistribCauchyDistribution.
 */
DistribCauchyDistribution::~DistribCauchyDistribution()
{
  delete mLocation;
  mLocation = NULL;
  delete mScale;
  mScale = NULL;
}


/*
 * Returns the value of the "location" element of this
 * DistribCauchyDistribution.
 */
const DistribUncertValue*
DistribCauchyDistribution::getLocation() const
{
  return mLocation;
}


/*
 * Returns the value of the "location" element of this
 * DistribCauchyDistribution.
 */
DistribUncertValue*
DistribCauchyDistribution::getLocation()
{
  return mLocation;
}


/*
 * Returns the value of the "scale" element of this DistribCauchyDistribution.
 */
const DistribUncertValue*
DistribCauchyDistribution::getScale() const
{
  return mScale;
}


/*
 * Returns the value of the "scale" element of this DistribCauchyDistribution.
 */
DistribUncertValue*
DistribCauchyDistribution::getScale()
{
  return mScale;
}


/*
 * Predicate returning @c true if this DistribCauchyDistribution's "location"
 * element is set.
 */
bool
DistribCauchyDistribution::isSetLocation() const
{
  return (mLocation != NULL);
}


/*
 * Predicate returning @c true if this DistribCauchyDistribution's "scale"
 * element is set.
 */
bool
DistribCauchyDistribution::isSetScale() const
{
  return (mScale != NULL);
}


/*
 * Sets the value of the "location" element of this DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::setLocation(const DistribUncertValue* location)
{
  if (location == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (location->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != location->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != location->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != location->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mLocation;
    mLocation = (location != NULL) ?
      static_cast<DistribUncertValue*>(location->clone()) : NULL;
    if (mLocation != NULL) mLocation->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "scale" element of this DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::setScale(const DistribUncertValue* scale)
{
  if (scale == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (scale->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != scale->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != scale->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != scale->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mScale;
    mScale = (scale != NULL) ? static_cast<DistribUncertValue*>(scale->clone())
      : NULL;
    if (mScale != NULL) mScale->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribCauchyDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribCauchyDistribution::createLocation()
{
  if (mLocation != NULL)
  {
    delete mLocation;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mLocation = new DistribUncertValue(distribns);

  mLocation->setElementName("location");

  delete distribns;

  connectToChild();

  return mLocation;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribCauchyDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribCauchyDistribution::createScale()
{
  if (mScale != NULL)
  {
    delete mScale;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mScale = new DistribUncertValue(distribns);

  mScale->setElementName("scale");

  delete distribns;

  connectToChild();

  return mScale;
}


/*
 * Unsets the value of the "location" element of this
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::unsetLocation()
{
  delete mLocation;
  mLocation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "scale" element of this DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::unsetScale()
{
  delete mScale;
  mScale = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribCauchyDistribution object.
 */
const std::string&
DistribCauchyDistribution::getElementName() const
{
  static const string name = "cauchyDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribCauchyDistribution object.
 */
int
DistribCauchyDistribution::getTypeCode() const
{
  return SBML_DISTRIB_CAUCHYDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribCauchyDistribution object have been set.
 */
bool
DistribCauchyDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribCauchyDistribution object have been set.
 */
bool
DistribCauchyDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredElements();

  if (isSetLocation() == false)
  {
    allPresent = false;
  }

  if (isSetScale() == false)
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
DistribCauchyDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeElements(stream);

  if (isSetLocation() == true)
  {
    mLocation->write(stream);
  }

  if (isSetScale() == true)
  {
    mScale->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribCauchyDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mLocation != NULL)
  {
    mLocation->accept(v);
  }

  if (mScale != NULL)
  {
    mScale->accept(v);
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
DistribCauchyDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribContinuousUnivariateDistribution::setSBMLDocument(d);

  if (mLocation != NULL)
  {
    mLocation->setSBMLDocument(d);
  }

  if (mScale != NULL)
  {
    mScale->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribCauchyDistribution::connectToChild()
{
  DistribContinuousUnivariateDistribution::connectToChild();

  if (mLocation != NULL)
  {
    mLocation->connectToParent(this);
  }

  if (mScale != NULL)
  {
    mScale->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribCauchyDistribution::enablePackageInternal(const std::string& pkgURI,
                                                 const std::string& pkgPrefix,
                                                 bool flag)
{
  DistribContinuousUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetLocation())
  {
    mLocation->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetScale())
  {
    mScale->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribCauchyDistribution::updateSBMLNamespace(const std::string& package,
                                               unsigned int level,
                                               unsigned int version)
{
  DistribContinuousUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mLocation != NULL)
  {
    mLocation->updateSBMLNamespace(package, level, version);
  }

  if (mScale != NULL)
  {
    mScale->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::getAttribute(const std::string& attributeName,
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
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::getAttribute(const std::string& attributeName,
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
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::getAttribute(const std::string& attributeName,
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
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::getAttribute(const std::string& attributeName,
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
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::getAttribute(const std::string& attributeName,
                                        std::string& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribCauchyDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribCauchyDistribution::isSetAttribute(const std::string& attributeName)
  const
{
  bool value =
    DistribContinuousUnivariateDistribution::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::setAttribute(const std::string& attributeName,
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
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::setAttribute(const std::string& attributeName,
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
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::setAttribute(const std::string& attributeName,
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
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::setAttribute(const std::string& attributeName,
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
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::setAttribute(const std::string& attributeName,
                                        const std::string& value)
{
  int return_value =
    DistribContinuousUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::unsetAttribute(const std::string& attributeName)
{
  int value =
    DistribContinuousUnivariateDistribution::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribCauchyDistribution.
 */
SBase*
DistribCauchyDistribution::createChildObject(const std::string& elementName)
{
  DistribContinuousUnivariateDistribution* obj = NULL;

  if (elementName == "location")
  {
    return createLocation();
  }
  else if (elementName == "scale")
  {
    return createScale();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribCauchyDistribution.
 */
int
DistribCauchyDistribution::addChildObject(const std::string& elementName,
                                          const SBase* element)
{
  if (elementName == "location" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setLocation((const DistribUncertValue*)(element));
  }
  else if (elementName == "scale" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setScale((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribCauchyDistribution.
 */
SBase*
DistribCauchyDistribution::removeChildObject(const std::string& elementName,
                                             const std::string& id)
{
  if (elementName == "location")
  {
    DistribUncertValue * obj = getLocation();
    if (unsetLocation() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "scale")
  {
    DistribUncertValue * obj = getScale();
    if (unsetScale() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribCauchyDistribution.
 */
unsigned int
DistribCauchyDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "location")
  {
    if (isSetLocation())
    {
      return 1;
    }
  }
  else if (elementName == "scale")
  {
    if (isSetScale())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribCauchyDistribution.
 */
SBase*
DistribCauchyDistribution::getObject(const std::string& elementName,
                                     unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "location")
  {
    return getLocation();
  }
  else if (elementName == "scale")
  {
    return getScale();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribCauchyDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mLocation != NULL)
  {
    if (mLocation->getId() == id)
    {
      return mLocation;
    }

    obj = mLocation->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mScale != NULL)
  {
    if (mScale->getId() == id)
    {
      return mScale;
    }

    obj = mScale->getElementBySId(id);
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
DistribCauchyDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mLocation != NULL)
  {
    if (mLocation->getMetaId() == metaid)
    {
      return mLocation;
    }

    obj = mLocation->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mScale != NULL)
  {
    if (mScale->getMetaId() == metaid)
    {
      return mScale;
    }

    obj = mScale->getElementByMetaId(metaid);
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
DistribCauchyDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mLocation, filter);
  ADD_FILTERED_POINTER(ret, sublist, mScale, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribCauchyDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "location")
  {
    if (isSetLocation())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribCauchyDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mLocation;
    mLocation = new DistribUncertValue(distribns);
    mLocation->setElementName(name);
    obj = mLocation;
  }
  else if (name == "scale")
  {
    if (isSetScale())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribCauchyDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mScale;
    mScale = new DistribUncertValue(distribns);
    mScale->setElementName(name);
    obj = mScale;
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
DistribCauchyDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribContinuousUnivariateDistribution::addExpectedAttributes(attributes);

  unsigned int level = getLevel();
  unsigned int coreVersion = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && coreVersion == 1 && pkgVersion == 1)
  {
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
DistribCauchyDistribution::readAttributes(const XMLAttributes& attributes,
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
        log->logPackageError("distrib", DistribUnknown, pkgVersion, level,
          version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribCauchyDistributionAllowedCoreAttributes, pkgVersion,
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
DistribCauchyDistribution::readL3V1V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribCauchyDistribution::readL3V2V1Attributes(const XMLAttributes&
  attributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  bool assigned = false;
  unsigned int pkgVersion = getPackageVersion();
  SBMLErrorLog* log = getErrorLog();
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribCauchyDistribution::writeAttributes(XMLOutputStream& stream) const
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
DistribCauchyDistribution::writeL3V1V1Attributes(XMLOutputStream& stream) const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribCauchyDistribution::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribCauchyDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribCauchyDistribution_t *
DistribCauchyDistribution_create(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion)
{
  return new DistribCauchyDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribCauchyDistribution_t object.
 */
LIBSBML_EXTERN
DistribCauchyDistribution_t*
DistribCauchyDistribution_clone(const DistribCauchyDistribution_t* dcd)
{
  if (dcd != NULL)
  {
    return static_cast<DistribCauchyDistribution_t*>(dcd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribCauchyDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribCauchyDistribution_free(DistribCauchyDistribution_t* dcd)
{
  if (dcd != NULL)
  {
    delete dcd;
  }
}


/*
 * Returns the value of the "location" element of this
 * DistribCauchyDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribCauchyDistribution_getLocation(const DistribCauchyDistribution_t * dcd)
{
  if (dcd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dcd->getLocation());
}


/*
 * Returns the value of the "scale" element of this
 * DistribCauchyDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribCauchyDistribution_getScale(const DistribCauchyDistribution_t * dcd)
{
  if (dcd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dcd->getScale());
}


/*
 * Predicate returning @c 1 (true) if this DistribCauchyDistribution_t's
 * "location" element is set.
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_isSetLocation(const DistribCauchyDistribution_t *
  dcd)
{
  return (dcd != NULL) ? static_cast<int>(dcd->isSetLocation()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribCauchyDistribution_t's
 * "scale" element is set.
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_isSetScale(const DistribCauchyDistribution_t * dcd)
{
  return (dcd != NULL) ? static_cast<int>(dcd->isSetScale()) : 0;
}


/*
 * Sets the value of the "location" element of this
 * DistribCauchyDistribution_t.
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_setLocation(DistribCauchyDistribution_t * dcd,
                                      const DistribUncertValue_t* location)
{
  return (dcd != NULL) ? dcd->setLocation(location) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "scale" element of this DistribCauchyDistribution_t.
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_setScale(DistribCauchyDistribution_t * dcd,
                                   const DistribUncertValue_t* scale)
{
  return (dcd != NULL) ? dcd->setScale(scale) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribCauchyDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribCauchyDistribution_createLocation(DistribCauchyDistribution_t* dcd)
{
  if (dcd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dcd->createLocation());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribCauchyDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribCauchyDistribution_createScale(DistribCauchyDistribution_t* dcd)
{
  if (dcd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dcd->createScale());
}


/*
 * Unsets the value of the "location" element of this
 * DistribCauchyDistribution_t.
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_unsetLocation(DistribCauchyDistribution_t * dcd)
{
  return (dcd != NULL) ? dcd->unsetLocation() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "scale" element of this DistribCauchyDistribution_t.
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_unsetScale(DistribCauchyDistribution_t * dcd)
{
  return (dcd != NULL) ? dcd->unsetScale() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribCauchyDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_hasRequiredAttributes(const
  DistribCauchyDistribution_t * dcd)
{
  return (dcd != NULL) ? static_cast<int>(dcd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribCauchyDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribCauchyDistribution_hasRequiredElements(const DistribCauchyDistribution_t
  * dcd)
{
  return (dcd != NULL) ? static_cast<int>(dcd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


