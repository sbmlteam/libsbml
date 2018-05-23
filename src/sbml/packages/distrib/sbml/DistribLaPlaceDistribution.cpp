/**
 * @file DistribLaPlaceDistribution.cpp
 * @brief Implementation of the DistribLaPlaceDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribLaPlaceDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribLaPlaceDistribution using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
DistribLaPlaceDistribution::DistribLaPlaceDistribution(unsigned int level,
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
 * Creates a new DistribLaPlaceDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribLaPlaceDistribution::DistribLaPlaceDistribution(DistribPkgNamespaces
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
 * Copy constructor for DistribLaPlaceDistribution.
 */
DistribLaPlaceDistribution::DistribLaPlaceDistribution(const
  DistribLaPlaceDistribution& orig)
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
 * Assignment operator for DistribLaPlaceDistribution.
 */
DistribLaPlaceDistribution&
DistribLaPlaceDistribution::operator=(const DistribLaPlaceDistribution& rhs)
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
 * Creates and returns a deep copy of this DistribLaPlaceDistribution object.
 */
DistribLaPlaceDistribution*
DistribLaPlaceDistribution::clone() const
{
  return new DistribLaPlaceDistribution(*this);
}


/*
 * Destructor for DistribLaPlaceDistribution.
 */
DistribLaPlaceDistribution::~DistribLaPlaceDistribution()
{
  delete mLocation;
  mLocation = NULL;
  delete mScale;
  mScale = NULL;
}


/*
 * Returns the value of the "id" attribute of this DistribLaPlaceDistribution.
 */
const std::string&
DistribLaPlaceDistribution::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * DistribLaPlaceDistribution.
 */
const std::string&
DistribLaPlaceDistribution::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribLaPlaceDistribution's "id"
 * attribute is set.
 */
bool
DistribLaPlaceDistribution::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribLaPlaceDistribution's "name"
 * attribute is set.
 */
bool
DistribLaPlaceDistribution::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::unsetId()
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
 * Unsets the value of the "name" attribute of this DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::unsetName()
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
 * Returns the value of the "location" element of this
 * DistribLaPlaceDistribution.
 */
const DistribUncertValue*
DistribLaPlaceDistribution::getLocation() const
{
  return mLocation;
}


/*
 * Returns the value of the "location" element of this
 * DistribLaPlaceDistribution.
 */
DistribUncertValue*
DistribLaPlaceDistribution::getLocation()
{
  return mLocation;
}


/*
 * Returns the value of the "scale" element of this DistribLaPlaceDistribution.
 */
const DistribUncertValue*
DistribLaPlaceDistribution::getScale() const
{
  return mScale;
}


/*
 * Returns the value of the "scale" element of this DistribLaPlaceDistribution.
 */
DistribUncertValue*
DistribLaPlaceDistribution::getScale()
{
  return mScale;
}


/*
 * Predicate returning @c true if this DistribLaPlaceDistribution's "location"
 * element is set.
 */
bool
DistribLaPlaceDistribution::isSetLocation() const
{
  return (mLocation != NULL);
}


/*
 * Predicate returning @c true if this DistribLaPlaceDistribution's "scale"
 * element is set.
 */
bool
DistribLaPlaceDistribution::isSetScale() const
{
  return (mScale != NULL);
}


/*
 * Sets the value of the "location" element of this DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::setLocation(const DistribUncertValue* location)
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
 * Sets the value of the "scale" element of this DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::setScale(const DistribUncertValue* scale)
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
 * DistribLaPlaceDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribLaPlaceDistribution::createLocation()
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
 * DistribLaPlaceDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribLaPlaceDistribution::createScale()
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::unsetLocation()
{
  delete mLocation;
  mLocation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "scale" element of this DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::unsetScale()
{
  delete mScale;
  mScale = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribLaPlaceDistribution object.
 */
const std::string&
DistribLaPlaceDistribution::getElementName() const
{
  static const string name = "laPlaceDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribLaPlaceDistribution object.
 */
int
DistribLaPlaceDistribution::getTypeCode() const
{
  return SBML_DISTRIB_LAPLACEDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribLaPlaceDistribution object have been set.
 */
bool
DistribLaPlaceDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribLaPlaceDistribution object have been set.
 */
bool
DistribLaPlaceDistribution::hasRequiredElements() const
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
DistribLaPlaceDistribution::writeElements(XMLOutputStream& stream) const
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
DistribLaPlaceDistribution::accept(SBMLVisitor& v) const
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
DistribLaPlaceDistribution::setSBMLDocument(SBMLDocument* d)
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
DistribLaPlaceDistribution::connectToChild()
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
DistribLaPlaceDistribution::enablePackageInternal(const std::string& pkgURI,
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
DistribLaPlaceDistribution::updateSBMLNamespace(const std::string& package,
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::getAttribute(const std::string& attributeName,
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::getAttribute(const std::string& attributeName,
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::getAttribute(const std::string& attributeName,
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::getAttribute(const std::string& attributeName,
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this DistribLaPlaceDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribLaPlaceDistribution::isSetAttribute(const std::string& attributeName)
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::setAttribute(const std::string& attributeName,
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::setAttribute(const std::string& attributeName,
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::setAttribute(const std::string& attributeName,
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::setAttribute(const std::string& attributeName,
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::setAttribute(const std::string& attributeName,
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
 * DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::unsetAttribute(const std::string& attributeName)
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
 * DistribLaPlaceDistribution.
 */
SBase*
DistribLaPlaceDistribution::createChildObject(const std::string& elementName)
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
 * Adds a new "elementName" object to this DistribLaPlaceDistribution.
 */
int
DistribLaPlaceDistribution::addChildObject(const std::string& elementName,
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
 * DistribLaPlaceDistribution.
 */
SBase*
DistribLaPlaceDistribution::removeChildObject(const std::string& elementName,
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
 * Returns the number of "elementName" in this DistribLaPlaceDistribution.
 */
unsigned int
DistribLaPlaceDistribution::getNumObjects(const std::string& elementName)
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
 * Returns the nth object of "objectName" in this DistribLaPlaceDistribution.
 */
SBase*
DistribLaPlaceDistribution::getObject(const std::string& elementName,
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
DistribLaPlaceDistribution::getElementBySId(const std::string& id)
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
DistribLaPlaceDistribution::getElementByMetaId(const std::string& metaid)
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
DistribLaPlaceDistribution::getAllElements(ElementFilter* filter)
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
DistribLaPlaceDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "location")
  {
    if (isSetLocation())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribLaPlaceDistributionAllowedElements, getPackageVersion(),
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
        DistribDistribLaPlaceDistributionAllowedElements, getPackageVersion(),
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
DistribLaPlaceDistribution::addExpectedAttributes(ExpectedAttributes&
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
DistribLaPlaceDistribution::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribLaPlaceDistributionAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribLaPlaceDistributionAllowedCoreAttributes, pkgVersion,
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
DistribLaPlaceDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeAttributes(stream);
  SBase::writeExtensionAttributes(stream);
}

/** @endcond */


#endif /* __cplusplus */


/*
 * Creates a new DistribLaPlaceDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribLaPlaceDistribution_t *
DistribLaPlaceDistribution_create(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion)
{
  return new DistribLaPlaceDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribLaPlaceDistribution_t object.
 */
LIBSBML_EXTERN
DistribLaPlaceDistribution_t*
DistribLaPlaceDistribution_clone(const DistribLaPlaceDistribution_t* dlpd)
{
  if (dlpd != NULL)
  {
    return static_cast<DistribLaPlaceDistribution_t*>(dlpd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribLaPlaceDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribLaPlaceDistribution_free(DistribLaPlaceDistribution_t* dlpd)
{
  if (dlpd != NULL)
  {
    delete dlpd;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribLaPlaceDistribution_getId(const DistribLaPlaceDistribution_t * dlpd)
{
  if (dlpd == NULL)
  {
    return NULL;
  }

  return dlpd->getId().empty() ? NULL : safe_strdup(dlpd->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribLaPlaceDistribution_getName(const DistribLaPlaceDistribution_t * dlpd)
{
  if (dlpd == NULL)
  {
    return NULL;
  }

  return dlpd->getName().empty() ? NULL : safe_strdup(dlpd->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribLaPlaceDistribution_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_isSetId(const DistribLaPlaceDistribution_t * dlpd)
{
  return (dlpd != NULL) ? static_cast<int>(dlpd->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribLaPlaceDistribution_t's
 * "name" attribute is set.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_isSetName(const DistribLaPlaceDistribution_t * dlpd)
{
  return (dlpd != NULL) ? static_cast<int>(dlpd->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_setId(DistribLaPlaceDistribution_t * dlpd,
                                 const char * id)
{
  return (dlpd != NULL) ? dlpd->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_setName(DistribLaPlaceDistribution_t * dlpd,
                                   const char * name)
{
  return (dlpd != NULL) ? dlpd->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_unsetId(DistribLaPlaceDistribution_t * dlpd)
{
  return (dlpd != NULL) ? dlpd->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_unsetName(DistribLaPlaceDistribution_t * dlpd)
{
  return (dlpd != NULL) ? dlpd->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "location" element of this
 * DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribLaPlaceDistribution_getLocation(const DistribLaPlaceDistribution_t *
  dlpd)
{
  if (dlpd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dlpd->getLocation());
}


/*
 * Returns the value of the "scale" element of this
 * DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribLaPlaceDistribution_getScale(const DistribLaPlaceDistribution_t * dlpd)
{
  if (dlpd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dlpd->getScale());
}


/*
 * Predicate returning @c 1 (true) if this DistribLaPlaceDistribution_t's
 * "location" element is set.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_isSetLocation(const DistribLaPlaceDistribution_t *
  dlpd)
{
  return (dlpd != NULL) ? static_cast<int>(dlpd->isSetLocation()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribLaPlaceDistribution_t's
 * "scale" element is set.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_isSetScale(const DistribLaPlaceDistribution_t *
  dlpd)
{
  return (dlpd != NULL) ? static_cast<int>(dlpd->isSetScale()) : 0;
}


/*
 * Sets the value of the "location" element of this
 * DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_setLocation(DistribLaPlaceDistribution_t * dlpd,
                                       const DistribUncertValue_t* location)
{
  return (dlpd != NULL) ? dlpd->setLocation(location) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "scale" element of this DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_setScale(DistribLaPlaceDistribution_t * dlpd,
                                    const DistribUncertValue_t* scale)
{
  return (dlpd != NULL) ? dlpd->setScale(scale) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribLaPlaceDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribLaPlaceDistribution_createLocation(DistribLaPlaceDistribution_t* dlpd)
{
  if (dlpd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dlpd->createLocation());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribLaPlaceDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribLaPlaceDistribution_createScale(DistribLaPlaceDistribution_t* dlpd)
{
  if (dlpd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dlpd->createScale());
}


/*
 * Unsets the value of the "location" element of this
 * DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_unsetLocation(DistribLaPlaceDistribution_t * dlpd)
{
  return (dlpd != NULL) ? dlpd->unsetLocation() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "scale" element of this
 * DistribLaPlaceDistribution_t.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_unsetScale(DistribLaPlaceDistribution_t * dlpd)
{
  return (dlpd != NULL) ? dlpd->unsetScale() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribLaPlaceDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_hasRequiredAttributes(const
  DistribLaPlaceDistribution_t * dlpd)
{
  return (dlpd != NULL) ? static_cast<int>(dlpd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribLaPlaceDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribLaPlaceDistribution_hasRequiredElements(const
  DistribLaPlaceDistribution_t * dlpd)
{
  return (dlpd != NULL) ? static_cast<int>(dlpd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


