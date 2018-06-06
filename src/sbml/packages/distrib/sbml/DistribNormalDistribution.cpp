/**
 * @file DistribNormalDistribution.cpp
 * @brief Implementation of the DistribNormalDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribNormalDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribNormalDistribution using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
DistribNormalDistribution::DistribNormalDistribution(unsigned int level,
                                                     unsigned int version,
                                                     unsigned int pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version, pkgVersion)
  , mMean (NULL)
  , mStddev (NULL)
  , mVariance (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribNormalDistribution using the given DistribPkgNamespaces
 * object.
 */
DistribNormalDistribution::DistribNormalDistribution(DistribPkgNamespaces
  *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mMean (NULL)
  , mStddev (NULL)
  , mVariance (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribNormalDistribution.
 */
DistribNormalDistribution::DistribNormalDistribution(const
  DistribNormalDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mMean ( NULL )
  , mStddev ( NULL )
  , mVariance ( NULL )
{
  if (orig.mMean != NULL)
  {
    mMean = orig.mMean->clone();
  }

  if (orig.mStddev != NULL)
  {
    mStddev = orig.mStddev->clone();
  }

  if (orig.mVariance != NULL)
  {
    mVariance = orig.mVariance->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribNormalDistribution.
 */
DistribNormalDistribution&
DistribNormalDistribution::operator=(const DistribNormalDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribContinuousUnivariateDistribution::operator=(rhs);
    delete mMean;
    if (rhs.mMean != NULL)
    {
      mMean = rhs.mMean->clone();
    }
    else
    {
      mMean = NULL;
    }

    delete mStddev;
    if (rhs.mStddev != NULL)
    {
      mStddev = rhs.mStddev->clone();
    }
    else
    {
      mStddev = NULL;
    }

    delete mVariance;
    if (rhs.mVariance != NULL)
    {
      mVariance = rhs.mVariance->clone();
    }
    else
    {
      mVariance = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribNormalDistribution object.
 */
DistribNormalDistribution*
DistribNormalDistribution::clone() const
{
  return new DistribNormalDistribution(*this);
}


/*
 * Destructor for DistribNormalDistribution.
 */
DistribNormalDistribution::~DistribNormalDistribution()
{
  delete mMean;
  mMean = NULL;
  delete mStddev;
  mStddev = NULL;
  delete mVariance;
  mVariance = NULL;
}


/*
 * Returns the value of the "mean" element of this DistribNormalDistribution.
 */
const DistribUncertValue*
DistribNormalDistribution::getMean() const
{
  return mMean;
}


/*
 * Returns the value of the "mean" element of this DistribNormalDistribution.
 */
DistribUncertValue*
DistribNormalDistribution::getMean()
{
  return mMean;
}


/*
 * Returns the value of the "stddev" element of this DistribNormalDistribution.
 */
const DistribUncertValue*
DistribNormalDistribution::getStddev() const
{
  return mStddev;
}


/*
 * Returns the value of the "stddev" element of this DistribNormalDistribution.
 */
DistribUncertValue*
DistribNormalDistribution::getStddev()
{
  return mStddev;
}


/*
 * Returns the value of the "variance" element of this
 * DistribNormalDistribution.
 */
const DistribUncertValue*
DistribNormalDistribution::getVariance() const
{
  return mVariance;
}


/*
 * Returns the value of the "variance" element of this
 * DistribNormalDistribution.
 */
DistribUncertValue*
DistribNormalDistribution::getVariance()
{
  return mVariance;
}


/*
 * Predicate returning @c true if this DistribNormalDistribution's "mean"
 * element is set.
 */
bool
DistribNormalDistribution::isSetMean() const
{
  return (mMean != NULL);
}


/*
 * Predicate returning @c true if this DistribNormalDistribution's "stddev"
 * element is set.
 */
bool
DistribNormalDistribution::isSetStddev() const
{
  return (mStddev != NULL);
}


/*
 * Predicate returning @c true if this DistribNormalDistribution's "variance"
 * element is set.
 */
bool
DistribNormalDistribution::isSetVariance() const
{
  return (mVariance != NULL);
}


/*
 * Sets the value of the "mean" element of this DistribNormalDistribution.
 */
int
DistribNormalDistribution::setMean(const DistribUncertValue* mean)
{
  if (mean == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (mean->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != mean->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != mean->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != mean->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mMean;
    mMean = (mean != NULL) ? static_cast<DistribUncertValue*>(mean->clone()) :
      NULL;
    if (mMean != NULL) mMean->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "stddev" element of this DistribNormalDistribution.
 */
int
DistribNormalDistribution::setStddev(const DistribUncertValue* stddev)
{
  if (stddev == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (stddev->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != stddev->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != stddev->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != stddev->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mStddev;
    mStddev = (stddev != NULL) ?
      static_cast<DistribUncertValue*>(stddev->clone()) : NULL;
    if (mStddev != NULL) mStddev->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "variance" element of this DistribNormalDistribution.
 */
int
DistribNormalDistribution::setVariance(const DistribUncertValue* variance)
{
  if (variance == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (variance->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != variance->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != variance->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != variance->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mVariance;
    mVariance = (variance != NULL) ?
      static_cast<DistribUncertValue*>(variance->clone()) : NULL;
    if (mVariance != NULL) mVariance->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribNormalDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribNormalDistribution::createMean()
{
  if (mMean != NULL)
  {
    delete mMean;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mMean = new DistribUncertValue(distribns);

  mMean->setElementName("mean");

  delete distribns;

  connectToChild();

  return mMean;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribNormalDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribNormalDistribution::createStddev()
{
  if (mStddev != NULL)
  {
    delete mStddev;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mStddev = new DistribUncertValue(distribns);

  mStddev->setElementName("stddev");

  delete distribns;

  connectToChild();

  return mStddev;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribNormalDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribNormalDistribution::createVariance()
{
  if (mVariance != NULL)
  {
    delete mVariance;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mVariance = new DistribUncertValue(distribns);

  mVariance->setElementName("variance");

  delete distribns;

  connectToChild();

  return mVariance;
}


/*
 * Unsets the value of the "mean" element of this DistribNormalDistribution.
 */
int
DistribNormalDistribution::unsetMean()
{
  delete mMean;
  mMean = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "stddev" element of this DistribNormalDistribution.
 */
int
DistribNormalDistribution::unsetStddev()
{
  delete mStddev;
  mStddev = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "variance" element of this
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::unsetVariance()
{
  delete mVariance;
  mVariance = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribNormalDistribution object.
 */
const std::string&
DistribNormalDistribution::getElementName() const
{
  static const string name = "normalDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribNormalDistribution object.
 */
int
DistribNormalDistribution::getTypeCode() const
{
  return SBML_DISTRIB_NORMALDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribNormalDistribution object have been set.
 */
bool
DistribNormalDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribNormalDistribution object have been set.
 */
bool
DistribNormalDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredElements();

  if (isSetMean() == false)
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
DistribNormalDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeElements(stream);

  if (isSetMean() == true)
  {
    mMean->write(stream);
  }

  if (isSetStddev() == true)
  {
    mStddev->write(stream);
  }

  if (isSetVariance() == true)
  {
    mVariance->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribNormalDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mMean != NULL)
  {
    mMean->accept(v);
  }

  if (mStddev != NULL)
  {
    mStddev->accept(v);
  }

  if (mVariance != NULL)
  {
    mVariance->accept(v);
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
DistribNormalDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribContinuousUnivariateDistribution::setSBMLDocument(d);

  if (mMean != NULL)
  {
    mMean->setSBMLDocument(d);
  }

  if (mStddev != NULL)
  {
    mStddev->setSBMLDocument(d);
  }

  if (mVariance != NULL)
  {
    mVariance->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribNormalDistribution::connectToChild()
{
  DistribContinuousUnivariateDistribution::connectToChild();

  if (mMean != NULL)
  {
    mMean->connectToParent(this);
  }

  if (mStddev != NULL)
  {
    mStddev->connectToParent(this);
  }

  if (mVariance != NULL)
  {
    mVariance->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribNormalDistribution::enablePackageInternal(const std::string& pkgURI,
                                                 const std::string& pkgPrefix,
                                                 bool flag)
{
  DistribContinuousUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetMean())
  {
    mMean->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetStddev())
  {
    mStddev->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetVariance())
  {
    mVariance->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribNormalDistribution::updateSBMLNamespace(const std::string& package,
                                               unsigned int level,
                                               unsigned int version)
{
  DistribContinuousUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mMean != NULL)
  {
    mMean->updateSBMLNamespace(package, level, version);
  }

  if (mStddev != NULL)
  {
    mStddev->updateSBMLNamespace(package, level, version);
  }

  if (mVariance != NULL)
  {
    mVariance->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::getAttribute(const std::string& attributeName,
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
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::getAttribute(const std::string& attributeName,
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
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::getAttribute(const std::string& attributeName,
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
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::getAttribute(const std::string& attributeName,
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
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::getAttribute(const std::string& attributeName,
                                        std::string& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribNormalDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribNormalDistribution::isSetAttribute(const std::string& attributeName)
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
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::setAttribute(const std::string& attributeName,
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
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::setAttribute(const std::string& attributeName,
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
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::setAttribute(const std::string& attributeName,
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
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::setAttribute(const std::string& attributeName,
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
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::setAttribute(const std::string& attributeName,
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
 * DistribNormalDistribution.
 */
int
DistribNormalDistribution::unsetAttribute(const std::string& attributeName)
{
  int value =
    DistribContinuousUnivariateDistribution::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribNormalDistribution.
 */
SBase*
DistribNormalDistribution::createChildObject(const std::string& elementName)
{
  DistribContinuousUnivariateDistribution* obj = NULL;

  if (elementName == "mean")
  {
    return createMean();
  }
  else if (elementName == "stddev")
  {
    return createStddev();
  }
  else if (elementName == "variance")
  {
    return createVariance();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribNormalDistribution.
 */
int
DistribNormalDistribution::addChildObject(const std::string& elementName,
                                          const SBase* element)
{
  if (elementName == "mean" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setMean((const DistribUncertValue*)(element));
  }
  else if (elementName == "stddev" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setStddev((const DistribUncertValue*)(element));
  }
  else if (elementName == "variance" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setVariance((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribNormalDistribution.
 */
SBase*
DistribNormalDistribution::removeChildObject(const std::string& elementName,
                                             const std::string& id)
{
  if (elementName == "mean")
  {
    DistribUncertValue * obj = getMean();
    if (unsetMean() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "stddev")
  {
    DistribUncertValue * obj = getStddev();
    if (unsetStddev() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "variance")
  {
    DistribUncertValue * obj = getVariance();
    if (unsetVariance() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribNormalDistribution.
 */
unsigned int
DistribNormalDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "mean")
  {
    if (isSetMean())
    {
      return 1;
    }
  }
  else if (elementName == "stddev")
  {
    if (isSetStddev())
    {
      return 1;
    }
  }
  else if (elementName == "variance")
  {
    if (isSetVariance())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribNormalDistribution.
 */
SBase*
DistribNormalDistribution::getObject(const std::string& elementName,
                                     unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "mean")
  {
    return getMean();
  }
  else if (elementName == "stddev")
  {
    return getStddev();
  }
  else if (elementName == "variance")
  {
    return getVariance();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribNormalDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mMean != NULL)
  {
    if (mMean->getId() == id)
    {
      return mMean;
    }

    obj = mMean->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mStddev != NULL)
  {
    if (mStddev->getId() == id)
    {
      return mStddev;
    }

    obj = mStddev->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mVariance != NULL)
  {
    if (mVariance->getId() == id)
    {
      return mVariance;
    }

    obj = mVariance->getElementBySId(id);
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
DistribNormalDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mMean != NULL)
  {
    if (mMean->getMetaId() == metaid)
    {
      return mMean;
    }

    obj = mMean->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mStddev != NULL)
  {
    if (mStddev->getMetaId() == metaid)
    {
      return mStddev;
    }

    obj = mStddev->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mVariance != NULL)
  {
    if (mVariance->getMetaId() == metaid)
    {
      return mVariance;
    }

    obj = mVariance->getElementByMetaId(metaid);
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
DistribNormalDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mMean, filter);
  ADD_FILTERED_POINTER(ret, sublist, mStddev, filter);
  ADD_FILTERED_POINTER(ret, sublist, mVariance, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribNormalDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "mean")
  {
    if (isSetMean())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribNormalDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mMean;
    mMean = new DistribUncertValue(distribns);
    mMean->setElementName(name);
    obj = mMean;
  }
  else if (name == "stddev")
  {
    if (isSetStddev())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribNormalDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mStddev;
    mStddev = new DistribUncertValue(distribns);
    mStddev->setElementName(name);
    obj = mStddev;
  }
  else if (name == "variance")
  {
    if (isSetVariance())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribNormalDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mVariance;
    mVariance = new DistribUncertValue(distribns);
    mVariance->setElementName(name);
    obj = mVariance;
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
DistribNormalDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribContinuousUnivariateDistribution::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribNormalDistribution::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribNormalDistributionAllowedCoreAttributes, pkgVersion,
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
DistribNormalDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribNormalDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribNormalDistribution_t *
DistribNormalDistribution_create(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion)
{
  return new DistribNormalDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribNormalDistribution_t object.
 */
LIBSBML_EXTERN
DistribNormalDistribution_t*
DistribNormalDistribution_clone(const DistribNormalDistribution_t* dnd)
{
  if (dnd != NULL)
  {
    return static_cast<DistribNormalDistribution_t*>(dnd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribNormalDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribNormalDistribution_free(DistribNormalDistribution_t* dnd)
{
  if (dnd != NULL)
  {
    delete dnd;
  }
}


/*
 * Returns the value of the "mean" element of this DistribNormalDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribNormalDistribution_getMean(const DistribNormalDistribution_t * dnd)
{
  if (dnd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dnd->getMean());
}


/*
 * Returns the value of the "stddev" element of this
 * DistribNormalDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribNormalDistribution_getStddev(const DistribNormalDistribution_t * dnd)
{
  if (dnd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dnd->getStddev());
}


/*
 * Returns the value of the "variance" element of this
 * DistribNormalDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribNormalDistribution_getVariance(const DistribNormalDistribution_t * dnd)
{
  if (dnd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dnd->getVariance());
}


/*
 * Predicate returning @c 1 (true) if this DistribNormalDistribution_t's "mean"
 * element is set.
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_isSetMean(const DistribNormalDistribution_t * dnd)
{
  return (dnd != NULL) ? static_cast<int>(dnd->isSetMean()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribNormalDistribution_t's
 * "stddev" element is set.
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_isSetStddev(const DistribNormalDistribution_t * dnd)
{
  return (dnd != NULL) ? static_cast<int>(dnd->isSetStddev()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribNormalDistribution_t's
 * "variance" element is set.
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_isSetVariance(const DistribNormalDistribution_t *
  dnd)
{
  return (dnd != NULL) ? static_cast<int>(dnd->isSetVariance()) : 0;
}


/*
 * Sets the value of the "mean" element of this DistribNormalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_setMean(DistribNormalDistribution_t * dnd,
                                  const DistribUncertValue_t* mean)
{
  return (dnd != NULL) ? dnd->setMean(mean) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "stddev" element of this DistribNormalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_setStddev(DistribNormalDistribution_t * dnd,
                                    const DistribUncertValue_t* stddev)
{
  return (dnd != NULL) ? dnd->setStddev(stddev) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "variance" element of this
 * DistribNormalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_setVariance(DistribNormalDistribution_t * dnd,
                                      const DistribUncertValue_t* variance)
{
  return (dnd != NULL) ? dnd->setVariance(variance) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribNormalDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribNormalDistribution_createMean(DistribNormalDistribution_t* dnd)
{
  if (dnd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dnd->createMean());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribNormalDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribNormalDistribution_createStddev(DistribNormalDistribution_t* dnd)
{
  if (dnd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dnd->createStddev());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribNormalDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribNormalDistribution_createVariance(DistribNormalDistribution_t* dnd)
{
  if (dnd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dnd->createVariance());
}


/*
 * Unsets the value of the "mean" element of this DistribNormalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_unsetMean(DistribNormalDistribution_t * dnd)
{
  return (dnd != NULL) ? dnd->unsetMean() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "stddev" element of this
 * DistribNormalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_unsetStddev(DistribNormalDistribution_t * dnd)
{
  return (dnd != NULL) ? dnd->unsetStddev() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "variance" element of this
 * DistribNormalDistribution_t.
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_unsetVariance(DistribNormalDistribution_t * dnd)
{
  return (dnd != NULL) ? dnd->unsetVariance() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribNormalDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_hasRequiredAttributes(const
  DistribNormalDistribution_t * dnd)
{
  return (dnd != NULL) ? static_cast<int>(dnd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribNormalDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribNormalDistribution_hasRequiredElements(const DistribNormalDistribution_t
  * dnd)
{
  return (dnd != NULL) ? static_cast<int>(dnd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


