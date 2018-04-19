/**
 * @file DistribStudentTDistribution.cpp
 * @brief Implementation of the DistribStudentTDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribStudentTDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribStudentTDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribStudentTDistribution::DistribStudentTDistribution(unsigned int level,
                                                         unsigned int version,
                                                         unsigned int
                                                           pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version)
  , mLocation (NULL)
  , mScale (NULL)
  , mDegreesOfFreedom (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribStudentTDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribStudentTDistribution::DistribStudentTDistribution(DistribPkgNamespaces
  *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mLocation (NULL)
  , mScale (NULL)
  , mDegreesOfFreedom (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribStudentTDistribution.
 */
DistribStudentTDistribution::DistribStudentTDistribution(const
  DistribStudentTDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mLocation ( NULL )
  , mScale ( NULL )
  , mDegreesOfFreedom ( NULL )
{
  if (orig.mLocation != NULL)
  {
    mLocation = orig.mLocation->clone();
  }

  if (orig.mScale != NULL)
  {
    mScale = orig.mScale->clone();
  }

  if (orig.mDegreesOfFreedom != NULL)
  {
    mDegreesOfFreedom = orig.mDegreesOfFreedom->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribStudentTDistribution.
 */
DistribStudentTDistribution&
DistribStudentTDistribution::operator=(const DistribStudentTDistribution& rhs)
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
 * Creates and returns a deep copy of this DistribStudentTDistribution object.
 */
DistribStudentTDistribution*
DistribStudentTDistribution::clone() const
{
  return new DistribStudentTDistribution(*this);
}


/*
 * Destructor for DistribStudentTDistribution.
 */
DistribStudentTDistribution::~DistribStudentTDistribution()
{
  delete mLocation;
  mLocation = NULL;
  delete mScale;
  mScale = NULL;
  delete mDegreesOfFreedom;
  mDegreesOfFreedom = NULL;
}


/*
 * Returns the value of the "id" attribute of this DistribStudentTDistribution.
 */
const std::string&
DistribStudentTDistribution::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * DistribStudentTDistribution.
 */
const std::string&
DistribStudentTDistribution::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribStudentTDistribution's "id"
 * attribute is set.
 */
bool
DistribStudentTDistribution::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribStudentTDistribution's "name"
 * attribute is set.
 */
bool
DistribStudentTDistribution::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::unsetId()
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
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::unsetName()
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
 * DistribStudentTDistribution.
 */
const DistribUncertValue*
DistribStudentTDistribution::getLocation() const
{
  return mLocation;
}


/*
 * Returns the value of the "location" element of this
 * DistribStudentTDistribution.
 */
DistribUncertValue*
DistribStudentTDistribution::getLocation()
{
  return mLocation;
}


/*
 * Returns the value of the "scale" element of this
 * DistribStudentTDistribution.
 */
const DistribUncertValue*
DistribStudentTDistribution::getScale() const
{
  return mScale;
}


/*
 * Returns the value of the "scale" element of this
 * DistribStudentTDistribution.
 */
DistribUncertValue*
DistribStudentTDistribution::getScale()
{
  return mScale;
}


/*
 * Returns the value of the "degreesOfFreedom" element of this
 * DistribStudentTDistribution.
 */
const DistribUncertValue*
DistribStudentTDistribution::getDegreesOfFreedom() const
{
  return mDegreesOfFreedom;
}


/*
 * Returns the value of the "degreesOfFreedom" element of this
 * DistribStudentTDistribution.
 */
DistribUncertValue*
DistribStudentTDistribution::getDegreesOfFreedom()
{
  return mDegreesOfFreedom;
}


/*
 * Predicate returning @c true if this DistribStudentTDistribution's "location"
 * element is set.
 */
bool
DistribStudentTDistribution::isSetLocation() const
{
  return (mLocation != NULL);
}


/*
 * Predicate returning @c true if this DistribStudentTDistribution's "scale"
 * element is set.
 */
bool
DistribStudentTDistribution::isSetScale() const
{
  return (mScale != NULL);
}


/*
 * Predicate returning @c true if this DistribStudentTDistribution's
 * "degreesOfFreedom" element is set.
 */
bool
DistribStudentTDistribution::isSetDegreesOfFreedom() const
{
  return (mDegreesOfFreedom != NULL);
}


/*
 * Sets the value of the "location" element of this
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::setLocation(const DistribUncertValue* location)
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
 * Sets the value of the "scale" element of this DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::setScale(const DistribUncertValue* scale)
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
 * Sets the value of the "degreesOfFreedom" element of this
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::setDegreesOfFreedom(const DistribUncertValue*
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
 * DistribStudentTDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribStudentTDistribution::createLocation()
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
 * DistribStudentTDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribStudentTDistribution::createScale()
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
 * Creates a new DistribUncertValue object, adds it to this
 * DistribStudentTDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribStudentTDistribution::createDegreesOfFreedom()
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
 * Unsets the value of the "location" element of this
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::unsetLocation()
{
  delete mLocation;
  mLocation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "scale" element of this DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::unsetScale()
{
  delete mScale;
  mScale = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "degreesOfFreedom" element of this
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::unsetDegreesOfFreedom()
{
  delete mDegreesOfFreedom;
  mDegreesOfFreedom = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribStudentTDistribution object.
 */
const std::string&
DistribStudentTDistribution::getElementName() const
{
  static const string name = "studentTDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribStudentTDistribution object.
 */
int
DistribStudentTDistribution::getTypeCode() const
{
  return SBML_DISTRIB_STUDENTTDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribStudentTDistribution object have been set.
 */
bool
DistribStudentTDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribStudentTDistribution object have been set.
 */
bool
DistribStudentTDistribution::hasRequiredElements() const
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
DistribStudentTDistribution::writeElements(XMLOutputStream& stream) const
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
DistribStudentTDistribution::accept(SBMLVisitor& v) const
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
DistribStudentTDistribution::setSBMLDocument(SBMLDocument* d)
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
DistribStudentTDistribution::connectToChild()
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
DistribStudentTDistribution::enablePackageInternal(const std::string& pkgURI,
                                                   const std::string&
                                                     pkgPrefix,
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
DistribStudentTDistribution::updateSBMLNamespace(const std::string& package,
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

  if (mDegreesOfFreedom != NULL)
  {
    mDegreesOfFreedom->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::getAttribute(const std::string& attributeName,
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
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::getAttribute(const std::string& attributeName,
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
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::getAttribute(const std::string& attributeName,
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
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::getAttribute(const std::string& attributeName,
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
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::getAttribute(const std::string& attributeName,
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
 * Predicate returning @c true if this DistribStudentTDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribStudentTDistribution::isSetAttribute(const std::string& attributeName)
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
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::setAttribute(const std::string& attributeName,
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
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::setAttribute(const std::string& attributeName,
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
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::setAttribute(const std::string& attributeName,
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
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::setAttribute(const std::string& attributeName,
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
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::setAttribute(const std::string& attributeName,
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
 * DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::unsetAttribute(const std::string& attributeName)
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
 * DistribStudentTDistribution.
 */
SBase*
DistribStudentTDistribution::createChildObject(const std::string& elementName)
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
  else if (elementName == "degreesOfFreedom")
  {
    return createDegreesOfFreedom();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribStudentTDistribution.
 */
int
DistribStudentTDistribution::addChildObject(const std::string& elementName,
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
  else if (elementName == "degreesOfFreedom" && element->getTypeCode() ==
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
 * DistribStudentTDistribution.
 */
SBase*
DistribStudentTDistribution::removeChildObject(const std::string& elementName,
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
  else if (elementName == "degreesOfFreedom")
  {
    DistribUncertValue * obj = getDegreesOfFreedom();
    if (unsetDegreesOfFreedom() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribStudentTDistribution.
 */
unsigned int
DistribStudentTDistribution::getNumObjects(const std::string& elementName)
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
  else if (elementName == "degreesOfFreedom")
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
 * Returns the nth object of "objectName" in this DistribStudentTDistribution.
 */
SBase*
DistribStudentTDistribution::getObject(const std::string& elementName,
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
  else if (elementName == "degreesOfFreedom")
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
DistribStudentTDistribution::getElementBySId(const std::string& id)
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
DistribStudentTDistribution::getElementByMetaId(const std::string& metaid)
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
DistribStudentTDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mLocation, filter);
  ADD_FILTERED_POINTER(ret, sublist, mScale, filter);
  ADD_FILTERED_POINTER(ret, sublist, mDegreesOfFreedom, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribStudentTDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "location")
  {
    if (isSetLocation())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribStudentTDistributionAllowedElements, getPackageVersion(),
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
        DistribDistribStudentTDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mScale;
    mScale = new DistribUncertValue(distribns);
    mScale->setElementName(name);
    obj = mScale;
  }
  else if (name == "degreesOfFreedom")
  {
    if (isSetDegreesOfFreedom())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribStudentTDistributionAllowedElements, getPackageVersion(),
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
DistribStudentTDistribution::addExpectedAttributes(ExpectedAttributes&
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
DistribStudentTDistribution::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribStudentTDistributionAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribStudentTDistributionAllowedCoreAttributes, pkgVersion,
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
DistribStudentTDistribution::readL3V1V1Attributes(const XMLAttributes&
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
      logEmptyString(mId, level, version, "<DistribStudentTDistribution>");
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
      logEmptyString(mName, level, version, "<DistribStudentTDistribution>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribStudentTDistribution::readL3V2V1Attributes(const XMLAttributes&
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
      logEmptyString(mId, level, version, "<DistribStudentTDistribution>");
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
DistribStudentTDistribution::writeAttributes(XMLOutputStream& stream) const
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
DistribStudentTDistribution::writeL3V1V1Attributes(XMLOutputStream& stream)
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
DistribStudentTDistribution::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribStudentTDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribStudentTDistribution_t *
DistribStudentTDistribution_create(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion)
{
  return new DistribStudentTDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribStudentTDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribStudentTDistribution_t*
DistribStudentTDistribution_clone(const DistribStudentTDistribution_t* dstd)
{
  if (dstd != NULL)
  {
    return static_cast<DistribStudentTDistribution_t*>(dstd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribStudentTDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribStudentTDistribution_free(DistribStudentTDistribution_t* dstd)
{
  if (dstd != NULL)
  {
    delete dstd;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribStudentTDistribution_getId(const DistribStudentTDistribution_t * dstd)
{
  if (dstd == NULL)
  {
    return NULL;
  }

  return dstd->getId().empty() ? NULL : safe_strdup(dstd->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribStudentTDistribution_getName(const DistribStudentTDistribution_t * dstd)
{
  if (dstd == NULL)
  {
    return NULL;
  }

  return dstd->getName().empty() ? NULL : safe_strdup(dstd->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribStudentTDistribution_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_isSetId(const DistribStudentTDistribution_t * dstd)
{
  return (dstd != NULL) ? static_cast<int>(dstd->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribStudentTDistribution_t's
 * "name" attribute is set.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_isSetName(const DistribStudentTDistribution_t *
  dstd)
{
  return (dstd != NULL) ? static_cast<int>(dstd->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_setId(DistribStudentTDistribution_t * dstd,
                                  const char * id)
{
  return (dstd != NULL) ? dstd->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_setName(DistribStudentTDistribution_t * dstd,
                                    const char * name)
{
  return (dstd != NULL) ? dstd->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_unsetId(DistribStudentTDistribution_t * dstd)
{
  return (dstd != NULL) ? dstd->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_unsetName(DistribStudentTDistribution_t * dstd)
{
  return (dstd != NULL) ? dstd->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "location" element of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribStudentTDistribution_getLocation(const DistribStudentTDistribution_t *
  dstd)
{
  if (dstd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dstd->getLocation());
}


/*
 * Returns the value of the "scale" element of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribStudentTDistribution_getScale(const DistribStudentTDistribution_t *
  dstd)
{
  if (dstd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dstd->getScale());
}


/*
 * Returns the value of the "degreesOfFreedom" element of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribStudentTDistribution_getDegreesOfFreedom(const
  DistribStudentTDistribution_t * dstd)
{
  if (dstd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dstd->getDegreesOfFreedom());
}


/*
 * Predicate returning @c 1 (true) if this DistribStudentTDistribution_t's
 * "location" element is set.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_isSetLocation(const DistribStudentTDistribution_t *
  dstd)
{
  return (dstd != NULL) ? static_cast<int>(dstd->isSetLocation()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribStudentTDistribution_t's
 * "scale" element is set.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_isSetScale(const DistribStudentTDistribution_t *
  dstd)
{
  return (dstd != NULL) ? static_cast<int>(dstd->isSetScale()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribStudentTDistribution_t's
 * "degreesOfFreedom" element is set.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_isSetDegreesOfFreedom(const
  DistribStudentTDistribution_t * dstd)
{
  return (dstd != NULL) ? static_cast<int>(dstd->isSetDegreesOfFreedom()) : 0;
}


/*
 * Sets the value of the "location" element of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_setLocation(DistribStudentTDistribution_t * dstd,
                                        const DistribUncertValue_t* location)
{
  return (dstd != NULL) ? dstd->setLocation(location) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "scale" element of this DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_setScale(DistribStudentTDistribution_t * dstd,
                                     const DistribUncertValue_t* scale)
{
  return (dstd != NULL) ? dstd->setScale(scale) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "degreesOfFreedom" element of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_setDegreesOfFreedom(
                                                DistribStudentTDistribution_t *
                                                  dstd,
                                                const DistribUncertValue_t*
                                                  degreesOfFreedom)
{
  return (dstd != NULL) ? dstd->setDegreesOfFreedom(degreesOfFreedom) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribStudentTDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribStudentTDistribution_createLocation(DistribStudentTDistribution_t* dstd)
{
  if (dstd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dstd->createLocation());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribStudentTDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribStudentTDistribution_createScale(DistribStudentTDistribution_t* dstd)
{
  if (dstd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dstd->createScale());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribStudentTDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribStudentTDistribution_createDegreesOfFreedom(DistribStudentTDistribution_t*
  dstd)
{
  if (dstd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dstd->createDegreesOfFreedom());
}


/*
 * Unsets the value of the "location" element of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_unsetLocation(DistribStudentTDistribution_t * dstd)
{
  return (dstd != NULL) ? dstd->unsetLocation() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "scale" element of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_unsetScale(DistribStudentTDistribution_t * dstd)
{
  return (dstd != NULL) ? dstd->unsetScale() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "degreesOfFreedom" element of this
 * DistribStudentTDistribution_t.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_unsetDegreesOfFreedom(DistribStudentTDistribution_t
  * dstd)
{
  return (dstd != NULL) ? dstd->unsetDegreesOfFreedom() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribStudentTDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_hasRequiredAttributes(const
  DistribStudentTDistribution_t * dstd)
{
  return (dstd != NULL) ? static_cast<int>(dstd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribStudentTDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_hasRequiredElements(const
  DistribStudentTDistribution_t * dstd)
{
  return (dstd != NULL) ? static_cast<int>(dstd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


