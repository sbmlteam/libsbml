/**
 * @file DistribUniformDistribution.cpp
 * @brief Implementation of the DistribUniformDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribUniformDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribUniformDistribution using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
DistribUniformDistribution::DistribUniformDistribution(unsigned int level,
                                                       unsigned int version,
                                                       unsigned int pkgVersion)
  : DistribContinuousUnivariateDistribution(level, version, pkgVersion)
  , mMinimum (NULL)
  , mMaximum (NULL)
  , mNumberOfClasses (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribUniformDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribUniformDistribution::DistribUniformDistribution(DistribPkgNamespaces
  *distribns)
  : DistribContinuousUnivariateDistribution(distribns)
  , mMinimum (NULL)
  , mMaximum (NULL)
  , mNumberOfClasses (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribUniformDistribution.
 */
DistribUniformDistribution::DistribUniformDistribution(const
  DistribUniformDistribution& orig)
  : DistribContinuousUnivariateDistribution( orig )
  , mMinimum ( NULL )
  , mMaximum ( NULL )
  , mNumberOfClasses ( NULL )
{
  if (orig.mMinimum != NULL)
  {
    mMinimum = orig.mMinimum->clone();
  }

  if (orig.mMaximum != NULL)
  {
    mMaximum = orig.mMaximum->clone();
  }

  if (orig.mNumberOfClasses != NULL)
  {
    mNumberOfClasses = orig.mNumberOfClasses->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribUniformDistribution.
 */
DistribUniformDistribution&
DistribUniformDistribution::operator=(const DistribUniformDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribContinuousUnivariateDistribution::operator=(rhs);
    delete mMinimum;
    if (rhs.mMinimum != NULL)
    {
      mMinimum = rhs.mMinimum->clone();
    }
    else
    {
      mMinimum = NULL;
    }

    delete mMaximum;
    if (rhs.mMaximum != NULL)
    {
      mMaximum = rhs.mMaximum->clone();
    }
    else
    {
      mMaximum = NULL;
    }

    delete mNumberOfClasses;
    if (rhs.mNumberOfClasses != NULL)
    {
      mNumberOfClasses = rhs.mNumberOfClasses->clone();
    }
    else
    {
      mNumberOfClasses = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribUniformDistribution object.
 */
DistribUniformDistribution*
DistribUniformDistribution::clone() const
{
  return new DistribUniformDistribution(*this);
}


/*
 * Destructor for DistribUniformDistribution.
 */
DistribUniformDistribution::~DistribUniformDistribution()
{
  delete mMinimum;
  mMinimum = NULL;
  delete mMaximum;
  mMaximum = NULL;
  delete mNumberOfClasses;
  mNumberOfClasses = NULL;
}


/*
 * Returns the value of the "minimum" element of this
 * DistribUniformDistribution.
 */
const DistribUncertValue*
DistribUniformDistribution::getMinimum() const
{
  return mMinimum;
}


/*
 * Returns the value of the "minimum" element of this
 * DistribUniformDistribution.
 */
DistribUncertValue*
DistribUniformDistribution::getMinimum()
{
  return mMinimum;
}


/*
 * Returns the value of the "maximum" element of this
 * DistribUniformDistribution.
 */
const DistribUncertValue*
DistribUniformDistribution::getMaximum() const
{
  return mMaximum;
}


/*
 * Returns the value of the "maximum" element of this
 * DistribUniformDistribution.
 */
DistribUncertValue*
DistribUniformDistribution::getMaximum()
{
  return mMaximum;
}


/*
 * Returns the value of the "numberOfClasses" element of this
 * DistribUniformDistribution.
 */
const DistribUncertValue*
DistribUniformDistribution::getNumberOfClasses() const
{
  return mNumberOfClasses;
}


/*
 * Returns the value of the "numberOfClasses" element of this
 * DistribUniformDistribution.
 */
DistribUncertValue*
DistribUniformDistribution::getNumberOfClasses()
{
  return mNumberOfClasses;
}


/*
 * Predicate returning @c true if this DistribUniformDistribution's "minimum"
 * element is set.
 */
bool
DistribUniformDistribution::isSetMinimum() const
{
  return (mMinimum != NULL);
}


/*
 * Predicate returning @c true if this DistribUniformDistribution's "maximum"
 * element is set.
 */
bool
DistribUniformDistribution::isSetMaximum() const
{
  return (mMaximum != NULL);
}


/*
 * Predicate returning @c true if this DistribUniformDistribution's
 * "numberOfClasses" element is set.
 */
bool
DistribUniformDistribution::isSetNumberOfClasses() const
{
  return (mNumberOfClasses != NULL);
}


/*
 * Sets the value of the "minimum" element of this DistribUniformDistribution.
 */
int
DistribUniformDistribution::setMinimum(const DistribUncertValue* minimum)
{
  if (minimum == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (minimum->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != minimum->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != minimum->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != minimum->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mMinimum;
    mMinimum = (minimum != NULL) ?
      static_cast<DistribUncertValue*>(minimum->clone()) : NULL;
    if (mMinimum != NULL) mMinimum->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "maximum" element of this DistribUniformDistribution.
 */
int
DistribUniformDistribution::setMaximum(const DistribUncertValue* maximum)
{
  if (maximum == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (maximum->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != maximum->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != maximum->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != maximum->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mMaximum;
    mMaximum = (maximum != NULL) ?
      static_cast<DistribUncertValue*>(maximum->clone()) : NULL;
    if (mMaximum != NULL) mMaximum->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "numberOfClasses" element of this
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::setNumberOfClasses(const DistribUncertValue*
  numberOfClasses)
{
  if (numberOfClasses == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (numberOfClasses->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != numberOfClasses->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != numberOfClasses->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != numberOfClasses->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mNumberOfClasses;
    mNumberOfClasses = (numberOfClasses != NULL) ?
      static_cast<DistribUncertValue*>(numberOfClasses->clone()) : NULL;
    if (mNumberOfClasses != NULL) mNumberOfClasses->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribUniformDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribUniformDistribution::createMinimum()
{
  if (mMinimum != NULL)
  {
    delete mMinimum;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mMinimum = new DistribUncertValue(distribns);

  mMinimum->setElementName("minimum");

  delete distribns;

  connectToChild();

  return mMinimum;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribUniformDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribUniformDistribution::createMaximum()
{
  if (mMaximum != NULL)
  {
    delete mMaximum;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mMaximum = new DistribUncertValue(distribns);

  mMaximum->setElementName("maximum");

  delete distribns;

  connectToChild();

  return mMaximum;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribUniformDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribUniformDistribution::createNumberOfClasses()
{
  if (mNumberOfClasses != NULL)
  {
    delete mNumberOfClasses;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mNumberOfClasses = new DistribUncertValue(distribns);

  mNumberOfClasses->setElementName("numberOfClasses");

  delete distribns;

  connectToChild();

  return mNumberOfClasses;
}


/*
 * Unsets the value of the "minimum" element of this
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::unsetMinimum()
{
  delete mMinimum;
  mMinimum = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "maximum" element of this
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::unsetMaximum()
{
  delete mMaximum;
  mMaximum = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "numberOfClasses" element of this
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::unsetNumberOfClasses()
{
  delete mNumberOfClasses;
  mNumberOfClasses = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribUniformDistribution object.
 */
const std::string&
DistribUniformDistribution::getElementName() const
{
  static const string name = "uniformDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribUniformDistribution object.
 */
int
DistribUniformDistribution::getTypeCode() const
{
  return SBML_DISTRIB_UNIFORMDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribUniformDistribution object have been set.
 */
bool
DistribUniformDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribUniformDistribution object have been set.
 */
bool
DistribUniformDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribContinuousUnivariateDistribution::hasRequiredElements();

  if (isSetMinimum() == false)
  {
    allPresent = false;
  }

  if (isSetMaximum() == false)
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
DistribUniformDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribContinuousUnivariateDistribution::writeElements(stream);

  if (isSetMinimum() == true)
  {
    mMinimum->write(stream);
  }

  if (isSetMaximum() == true)
  {
    mMaximum->write(stream);
  }

  if (isSetNumberOfClasses() == true)
  {
    mNumberOfClasses->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribUniformDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mMinimum != NULL)
  {
    mMinimum->accept(v);
  }

  if (mMaximum != NULL)
  {
    mMaximum->accept(v);
  }

  if (mNumberOfClasses != NULL)
  {
    mNumberOfClasses->accept(v);
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
DistribUniformDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribContinuousUnivariateDistribution::setSBMLDocument(d);

  if (mMinimum != NULL)
  {
    mMinimum->setSBMLDocument(d);
  }

  if (mMaximum != NULL)
  {
    mMaximum->setSBMLDocument(d);
  }

  if (mNumberOfClasses != NULL)
  {
    mNumberOfClasses->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribUniformDistribution::connectToChild()
{
  DistribContinuousUnivariateDistribution::connectToChild();

  if (mMinimum != NULL)
  {
    mMinimum->connectToParent(this);
  }

  if (mMaximum != NULL)
  {
    mMaximum->connectToParent(this);
  }

  if (mNumberOfClasses != NULL)
  {
    mNumberOfClasses->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribUniformDistribution::enablePackageInternal(const std::string& pkgURI,
                                                  const std::string& pkgPrefix,
                                                  bool flag)
{
  DistribContinuousUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetMinimum())
  {
    mMinimum->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetMaximum())
  {
    mMaximum->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetNumberOfClasses())
  {
    mNumberOfClasses->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribUniformDistribution::updateSBMLNamespace(const std::string& package,
                                                unsigned int level,
                                                unsigned int version)
{
  DistribContinuousUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mMinimum != NULL)
  {
    mMinimum->updateSBMLNamespace(package, level, version);
  }

  if (mMaximum != NULL)
  {
    mMaximum->updateSBMLNamespace(package, level, version);
  }

  if (mNumberOfClasses != NULL)
  {
    mNumberOfClasses->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::getAttribute(const std::string& attributeName,
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
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::getAttribute(const std::string& attributeName,
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
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::getAttribute(const std::string& attributeName,
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
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::getAttribute(const std::string& attributeName,
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
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::getAttribute(const std::string& attributeName,
                                         std::string& value) const
{
  int return_value =
    DistribContinuousUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribUniformDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribUniformDistribution::isSetAttribute(const std::string& attributeName)
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
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::setAttribute(const std::string& attributeName,
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
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::setAttribute(const std::string& attributeName,
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
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::setAttribute(const std::string& attributeName,
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
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::setAttribute(const std::string& attributeName,
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
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::setAttribute(const std::string& attributeName,
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
 * DistribUniformDistribution.
 */
int
DistribUniformDistribution::unsetAttribute(const std::string& attributeName)
{
  int value =
    DistribContinuousUnivariateDistribution::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribUniformDistribution.
 */
SBase*
DistribUniformDistribution::createChildObject(const std::string& elementName)
{
  DistribContinuousUnivariateDistribution* obj = NULL;

  if (elementName == "minimum")
  {
    return createMinimum();
  }
  else if (elementName == "maximum")
  {
    return createMaximum();
  }
  else if (elementName == "numberOfClasses")
  {
    return createNumberOfClasses();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribUniformDistribution.
 */
int
DistribUniformDistribution::addChildObject(const std::string& elementName,
                                           const SBase* element)
{
  if (elementName == "minimum" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setMinimum((const DistribUncertValue*)(element));
  }
  else if (elementName == "maximum" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setMaximum((const DistribUncertValue*)(element));
  }
  else if (elementName == "numberOfClasses" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setNumberOfClasses((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribUniformDistribution.
 */
SBase*
DistribUniformDistribution::removeChildObject(const std::string& elementName,
                                              const std::string& id)
{
  if (elementName == "minimum")
  {
    DistribUncertValue * obj = getMinimum();
    if (unsetMinimum() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "maximum")
  {
    DistribUncertValue * obj = getMaximum();
    if (unsetMaximum() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "numberOfClasses")
  {
    DistribUncertValue * obj = getNumberOfClasses();
    if (unsetNumberOfClasses() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribUniformDistribution.
 */
unsigned int
DistribUniformDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "minimum")
  {
    if (isSetMinimum())
    {
      return 1;
    }
  }
  else if (elementName == "maximum")
  {
    if (isSetMaximum())
    {
      return 1;
    }
  }
  else if (elementName == "numberOfClasses")
  {
    if (isSetNumberOfClasses())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribUniformDistribution.
 */
SBase*
DistribUniformDistribution::getObject(const std::string& elementName,
                                      unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "minimum")
  {
    return getMinimum();
  }
  else if (elementName == "maximum")
  {
    return getMaximum();
  }
  else if (elementName == "numberOfClasses")
  {
    return getNumberOfClasses();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribUniformDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mMinimum != NULL)
  {
    if (mMinimum->getId() == id)
    {
      return mMinimum;
    }

    obj = mMinimum->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mMaximum != NULL)
  {
    if (mMaximum->getId() == id)
    {
      return mMaximum;
    }

    obj = mMaximum->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mNumberOfClasses != NULL)
  {
    if (mNumberOfClasses->getId() == id)
    {
      return mNumberOfClasses;
    }

    obj = mNumberOfClasses->getElementBySId(id);
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
DistribUniformDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mMinimum != NULL)
  {
    if (mMinimum->getMetaId() == metaid)
    {
      return mMinimum;
    }

    obj = mMinimum->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mMaximum != NULL)
  {
    if (mMaximum->getMetaId() == metaid)
    {
      return mMaximum;
    }

    obj = mMaximum->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mNumberOfClasses != NULL)
  {
    if (mNumberOfClasses->getMetaId() == metaid)
    {
      return mNumberOfClasses;
    }

    obj = mNumberOfClasses->getElementByMetaId(metaid);
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
DistribUniformDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mMinimum, filter);
  ADD_FILTERED_POINTER(ret, sublist, mMaximum, filter);
  ADD_FILTERED_POINTER(ret, sublist, mNumberOfClasses, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribUniformDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribContinuousUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "minimum")
  {
    if (isSetMinimum())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUniformDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mMinimum;
    mMinimum = new DistribUncertValue(distribns);
    mMinimum->setElementName(name);
    obj = mMinimum;
  }
  else if (name == "maximum")
  {
    if (isSetMaximum())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUniformDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mMaximum;
    mMaximum = new DistribUncertValue(distribns);
    mMaximum->setElementName(name);
    obj = mMaximum;
  }
  else if (name == "numberOfClasses")
  {
    if (isSetNumberOfClasses())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUniformDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mNumberOfClasses;
    mNumberOfClasses = new DistribUncertValue(distribns);
    mNumberOfClasses->setElementName(name);
    obj = mNumberOfClasses;
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
DistribUniformDistribution::addExpectedAttributes(ExpectedAttributes&
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
DistribUniformDistribution::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribUniformDistributionAllowedCoreAttributes, pkgVersion,
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
DistribUniformDistribution::readL3V1V1Attributes(const XMLAttributes&
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
DistribUniformDistribution::readL3V2V1Attributes(const XMLAttributes&
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
DistribUniformDistribution::writeAttributes(XMLOutputStream& stream) const
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
DistribUniformDistribution::writeL3V1V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribUniformDistribution::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribUniformDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribUniformDistribution_t *
DistribUniformDistribution_create(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion)
{
  return new DistribUniformDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribUniformDistribution_t object.
 */
LIBSBML_EXTERN
DistribUniformDistribution_t*
DistribUniformDistribution_clone(const DistribUniformDistribution_t* dud)
{
  if (dud != NULL)
  {
    return static_cast<DistribUniformDistribution_t*>(dud->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribUniformDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribUniformDistribution_free(DistribUniformDistribution_t* dud)
{
  if (dud != NULL)
  {
    delete dud;
  }
}


/*
 * Returns the value of the "minimum" element of this
 * DistribUniformDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUniformDistribution_getMinimum(const DistribUniformDistribution_t * dud)
{
  if (dud == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dud->getMinimum());
}


/*
 * Returns the value of the "maximum" element of this
 * DistribUniformDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUniformDistribution_getMaximum(const DistribUniformDistribution_t * dud)
{
  if (dud == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dud->getMaximum());
}


/*
 * Returns the value of the "numberOfClasses" element of this
 * DistribUniformDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUniformDistribution_getNumberOfClasses(const
  DistribUniformDistribution_t * dud)
{
  if (dud == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dud->getNumberOfClasses());
}


/*
 * Predicate returning @c 1 (true) if this DistribUniformDistribution_t's
 * "minimum" element is set.
 */
LIBSBML_EXTERN
int
DistribUniformDistribution_isSetMinimum(const DistribUniformDistribution_t *
  dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isSetMinimum()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUniformDistribution_t's
 * "maximum" element is set.
 */
LIBSBML_EXTERN
int
DistribUniformDistribution_isSetMaximum(const DistribUniformDistribution_t *
  dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isSetMaximum()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUniformDistribution_t's
 * "numberOfClasses" element is set.
 */
LIBSBML_EXTERN
int
DistribUniformDistribution_isSetNumberOfClasses(const
  DistribUniformDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->isSetNumberOfClasses()) : 0;
}


/*
 * Sets the value of the "minimum" element of this
 * DistribUniformDistribution_t.
 */
LIBSBML_EXTERN
int
DistribUniformDistribution_setMinimum(DistribUniformDistribution_t * dud,
                                      const DistribUncertValue_t* minimum)
{
  return (dud != NULL) ? dud->setMinimum(minimum) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "maximum" element of this
 * DistribUniformDistribution_t.
 */
LIBSBML_EXTERN
int
DistribUniformDistribution_setMaximum(DistribUniformDistribution_t * dud,
                                      const DistribUncertValue_t* maximum)
{
  return (dud != NULL) ? dud->setMaximum(maximum) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "numberOfClasses" element of this
 * DistribUniformDistribution_t.
 */
LIBSBML_EXTERN
int
DistribUniformDistribution_setNumberOfClasses(
                                              DistribUniformDistribution_t *
                                                dud,
                                              const DistribUncertValue_t*
                                                numberOfClasses)
{
  return (dud != NULL) ? dud->setNumberOfClasses(numberOfClasses) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUniformDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUniformDistribution_createMinimum(DistribUniformDistribution_t* dud)
{
  if (dud == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dud->createMinimum());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUniformDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUniformDistribution_createMaximum(DistribUniformDistribution_t* dud)
{
  if (dud == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dud->createMaximum());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUniformDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUniformDistribution_createNumberOfClasses(DistribUniformDistribution_t*
  dud)
{
  if (dud == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dud->createNumberOfClasses());
}


/*
 * Unsets the value of the "minimum" element of this
 * DistribUniformDistribution_t.
 */
LIBSBML_EXTERN
int
DistribUniformDistribution_unsetMinimum(DistribUniformDistribution_t * dud)
{
  return (dud != NULL) ? dud->unsetMinimum() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "maximum" element of this
 * DistribUniformDistribution_t.
 */
LIBSBML_EXTERN
int
DistribUniformDistribution_unsetMaximum(DistribUniformDistribution_t * dud)
{
  return (dud != NULL) ? dud->unsetMaximum() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "numberOfClasses" element of this
 * DistribUniformDistribution_t.
 */
LIBSBML_EXTERN
int
DistribUniformDistribution_unsetNumberOfClasses(DistribUniformDistribution_t *
  dud)
{
  return (dud != NULL) ? dud->unsetNumberOfClasses() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUniformDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribUniformDistribution_hasRequiredAttributes(const
  DistribUniformDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribUniformDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribUniformDistribution_hasRequiredElements(const
  DistribUniformDistribution_t * dud)
{
  return (dud != NULL) ? static_cast<int>(dud->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


