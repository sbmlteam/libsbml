/**
 * @file DistribBinomialDistribution.cpp
 * @brief Implementation of the DistribBinomialDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribBinomialDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribBinomialDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribBinomialDistribution::DistribBinomialDistribution(unsigned int level,
                                                         unsigned int version,
                                                         unsigned int
                                                           pkgVersion)
  : DistribDiscreteUnivariateDistribution(level, version, pkgVersion)
  , mNumberOfTrials (NULL)
  , mProbabilityOfSuccess (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribBinomialDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribBinomialDistribution::DistribBinomialDistribution(DistribPkgNamespaces
  *distribns)
  : DistribDiscreteUnivariateDistribution(distribns)
  , mNumberOfTrials (NULL)
  , mProbabilityOfSuccess (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribBinomialDistribution.
 */
DistribBinomialDistribution::DistribBinomialDistribution(const
  DistribBinomialDistribution& orig)
  : DistribDiscreteUnivariateDistribution( orig )
  , mNumberOfTrials ( NULL )
  , mProbabilityOfSuccess ( NULL )
{
  if (orig.mNumberOfTrials != NULL)
  {
    mNumberOfTrials = orig.mNumberOfTrials->clone();
  }

  if (orig.mProbabilityOfSuccess != NULL)
  {
    mProbabilityOfSuccess = orig.mProbabilityOfSuccess->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribBinomialDistribution.
 */
DistribBinomialDistribution&
DistribBinomialDistribution::operator=(const DistribBinomialDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribDiscreteUnivariateDistribution::operator=(rhs);
    delete mNumberOfTrials;
    if (rhs.mNumberOfTrials != NULL)
    {
      mNumberOfTrials = rhs.mNumberOfTrials->clone();
    }
    else
    {
      mNumberOfTrials = NULL;
    }

    delete mProbabilityOfSuccess;
    if (rhs.mProbabilityOfSuccess != NULL)
    {
      mProbabilityOfSuccess = rhs.mProbabilityOfSuccess->clone();
    }
    else
    {
      mProbabilityOfSuccess = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribBinomialDistribution object.
 */
DistribBinomialDistribution*
DistribBinomialDistribution::clone() const
{
  return new DistribBinomialDistribution(*this);
}


/*
 * Destructor for DistribBinomialDistribution.
 */
DistribBinomialDistribution::~DistribBinomialDistribution()
{
  delete mNumberOfTrials;
  mNumberOfTrials = NULL;
  delete mProbabilityOfSuccess;
  mProbabilityOfSuccess = NULL;
}


/*
 * Returns the value of the "numberOfTrials" element of this
 * DistribBinomialDistribution.
 */
const DistribUncertValue*
DistribBinomialDistribution::getNumberOfTrials() const
{
  return mNumberOfTrials;
}


/*
 * Returns the value of the "numberOfTrials" element of this
 * DistribBinomialDistribution.
 */
DistribUncertValue*
DistribBinomialDistribution::getNumberOfTrials()
{
  return mNumberOfTrials;
}


/*
 * Returns the value of the "probabilityOfSuccess" element of this
 * DistribBinomialDistribution.
 */
const DistribUncertValue*
DistribBinomialDistribution::getProbabilityOfSuccess() const
{
  return mProbabilityOfSuccess;
}


/*
 * Returns the value of the "probabilityOfSuccess" element of this
 * DistribBinomialDistribution.
 */
DistribUncertValue*
DistribBinomialDistribution::getProbabilityOfSuccess()
{
  return mProbabilityOfSuccess;
}


/*
 * Predicate returning @c true if this DistribBinomialDistribution's
 * "numberOfTrials" element is set.
 */
bool
DistribBinomialDistribution::isSetNumberOfTrials() const
{
  return (mNumberOfTrials != NULL);
}


/*
 * Predicate returning @c true if this DistribBinomialDistribution's
 * "probabilityOfSuccess" element is set.
 */
bool
DistribBinomialDistribution::isSetProbabilityOfSuccess() const
{
  return (mProbabilityOfSuccess != NULL);
}


/*
 * Sets the value of the "numberOfTrials" element of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::setNumberOfTrials(const DistribUncertValue*
  numberOfTrials)
{
  if (numberOfTrials == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (numberOfTrials->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != numberOfTrials->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != numberOfTrials->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != numberOfTrials->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mNumberOfTrials;
    mNumberOfTrials = (numberOfTrials != NULL) ?
      static_cast<DistribUncertValue*>(numberOfTrials->clone()) : NULL;
    if (mNumberOfTrials != NULL) mNumberOfTrials->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "probabilityOfSuccess" element of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::setProbabilityOfSuccess(const DistribUncertValue*
  probabilityOfSuccess)
{
  if (probabilityOfSuccess == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (probabilityOfSuccess->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != probabilityOfSuccess->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != probabilityOfSuccess->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != probabilityOfSuccess->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mProbabilityOfSuccess;
    mProbabilityOfSuccess = (probabilityOfSuccess != NULL) ?
      static_cast<DistribUncertValue*>(probabilityOfSuccess->clone()) : NULL;
    if (mProbabilityOfSuccess != NULL)
      mProbabilityOfSuccess->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribBinomialDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribBinomialDistribution::createNumberOfTrials()
{
  if (mNumberOfTrials != NULL)
  {
    delete mNumberOfTrials;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mNumberOfTrials = new DistribUncertValue(distribns);

  mNumberOfTrials->setElementName("numberOfTrials");

  delete distribns;

  connectToChild();

  return mNumberOfTrials;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribBinomialDistribution object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribBinomialDistribution::createProbabilityOfSuccess()
{
  if (mProbabilityOfSuccess != NULL)
  {
    delete mProbabilityOfSuccess;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mProbabilityOfSuccess = new DistribUncertValue(distribns);

  mProbabilityOfSuccess->setElementName("probabilityOfSuccess");

  delete distribns;

  connectToChild();

  return mProbabilityOfSuccess;
}


/*
 * Unsets the value of the "numberOfTrials" element of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::unsetNumberOfTrials()
{
  delete mNumberOfTrials;
  mNumberOfTrials = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "probabilityOfSuccess" element of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::unsetProbabilityOfSuccess()
{
  delete mProbabilityOfSuccess;
  mProbabilityOfSuccess = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribBinomialDistribution object.
 */
const std::string&
DistribBinomialDistribution::getElementName() const
{
  static const string name = "binomialDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribBinomialDistribution object.
 */
int
DistribBinomialDistribution::getTypeCode() const
{
  return SBML_DISTRIB_BINOMIALDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribBinomialDistribution object have been set.
 */
bool
DistribBinomialDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribDiscreteUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribBinomialDistribution object have been set.
 */
bool
DistribBinomialDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribDiscreteUnivariateDistribution::hasRequiredElements();

  if (isSetNumberOfTrials() == false)
  {
    allPresent = false;
  }

  if (isSetProbabilityOfSuccess() == false)
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
DistribBinomialDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribDiscreteUnivariateDistribution::writeElements(stream);

  if (isSetNumberOfTrials() == true)
  {
    mNumberOfTrials->write(stream);
  }

  if (isSetProbabilityOfSuccess() == true)
  {
    mProbabilityOfSuccess->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribBinomialDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mNumberOfTrials != NULL)
  {
    mNumberOfTrials->accept(v);
  }

  if (mProbabilityOfSuccess != NULL)
  {
    mProbabilityOfSuccess->accept(v);
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
DistribBinomialDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribDiscreteUnivariateDistribution::setSBMLDocument(d);

  if (mNumberOfTrials != NULL)
  {
    mNumberOfTrials->setSBMLDocument(d);
  }

  if (mProbabilityOfSuccess != NULL)
  {
    mProbabilityOfSuccess->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribBinomialDistribution::connectToChild()
{
  DistribDiscreteUnivariateDistribution::connectToChild();

  if (mNumberOfTrials != NULL)
  {
    mNumberOfTrials->connectToParent(this);
  }

  if (mProbabilityOfSuccess != NULL)
  {
    mProbabilityOfSuccess->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribBinomialDistribution::enablePackageInternal(const std::string& pkgURI,
                                                   const std::string&
                                                     pkgPrefix,
                                                   bool flag)
{
  DistribDiscreteUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetNumberOfTrials())
  {
    mNumberOfTrials->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetProbabilityOfSuccess())
  {
    mProbabilityOfSuccess->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribBinomialDistribution::updateSBMLNamespace(const std::string& package,
                                                 unsigned int level,
                                                 unsigned int version)
{
  DistribDiscreteUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mNumberOfTrials != NULL)
  {
    mNumberOfTrials->updateSBMLNamespace(package, level, version);
  }

  if (mProbabilityOfSuccess != NULL)
  {
    mProbabilityOfSuccess->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::getAttribute(const std::string& attributeName,
                                          bool& value) const
{
  int return_value =
    DistribDiscreteUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::getAttribute(const std::string& attributeName,
                                          int& value) const
{
  int return_value =
    DistribDiscreteUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::getAttribute(const std::string& attributeName,
                                          double& value) const
{
  int return_value =
    DistribDiscreteUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::getAttribute(const std::string& attributeName,
                                          unsigned int& value) const
{
  int return_value =
    DistribDiscreteUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::getAttribute(const std::string& attributeName,
                                          std::string& value) const
{
  int return_value =
    DistribDiscreteUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribBinomialDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribBinomialDistribution::isSetAttribute(const std::string& attributeName)
  const
{
  bool value =
    DistribDiscreteUnivariateDistribution::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::setAttribute(const std::string& attributeName,
                                          bool value)
{
  int return_value =
    DistribDiscreteUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::setAttribute(const std::string& attributeName,
                                          int value)
{
  int return_value =
    DistribDiscreteUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::setAttribute(const std::string& attributeName,
                                          double value)
{
  int return_value =
    DistribDiscreteUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::setAttribute(const std::string& attributeName,
                                          unsigned int value)
{
  int return_value =
    DistribDiscreteUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::setAttribute(const std::string& attributeName,
                                          const std::string& value)
{
  int return_value =
    DistribDiscreteUnivariateDistribution::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::unsetAttribute(const std::string& attributeName)
{
  int value =
    DistribDiscreteUnivariateDistribution::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribBinomialDistribution.
 */
SBase*
DistribBinomialDistribution::createChildObject(const std::string& elementName)
{
  DistribDiscreteUnivariateDistribution* obj = NULL;

  if (elementName == "numberOfTrials")
  {
    return createNumberOfTrials();
  }
  else if (elementName == "probabilityOfSuccess")
  {
    return createProbabilityOfSuccess();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribBinomialDistribution.
 */
int
DistribBinomialDistribution::addChildObject(const std::string& elementName,
                                            const SBase* element)
{
  if (elementName == "numberOfTrials" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setNumberOfTrials((const DistribUncertValue*)(element));
  }
  else if (elementName == "probabilityOfSuccess" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setProbabilityOfSuccess((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribBinomialDistribution.
 */
SBase*
DistribBinomialDistribution::removeChildObject(const std::string& elementName,
                                               const std::string& id)
{
  if (elementName == "numberOfTrials")
  {
    DistribUncertValue * obj = getNumberOfTrials();
    if (unsetNumberOfTrials() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "probabilityOfSuccess")
  {
    DistribUncertValue * obj = getProbabilityOfSuccess();
    if (unsetProbabilityOfSuccess() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribBinomialDistribution.
 */
unsigned int
DistribBinomialDistribution::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "numberOfTrials")
  {
    if (isSetNumberOfTrials())
    {
      return 1;
    }
  }
  else if (elementName == "probabilityOfSuccess")
  {
    if (isSetProbabilityOfSuccess())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribBinomialDistribution.
 */
SBase*
DistribBinomialDistribution::getObject(const std::string& elementName,
                                       unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "numberOfTrials")
  {
    return getNumberOfTrials();
  }
  else if (elementName == "probabilityOfSuccess")
  {
    return getProbabilityOfSuccess();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribBinomialDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mNumberOfTrials != NULL)
  {
    if (mNumberOfTrials->getId() == id)
    {
      return mNumberOfTrials;
    }

    obj = mNumberOfTrials->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mProbabilityOfSuccess != NULL)
  {
    if (mProbabilityOfSuccess->getId() == id)
    {
      return mProbabilityOfSuccess;
    }

    obj = mProbabilityOfSuccess->getElementBySId(id);
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
DistribBinomialDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mNumberOfTrials != NULL)
  {
    if (mNumberOfTrials->getMetaId() == metaid)
    {
      return mNumberOfTrials;
    }

    obj = mNumberOfTrials->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mProbabilityOfSuccess != NULL)
  {
    if (mProbabilityOfSuccess->getMetaId() == metaid)
    {
      return mProbabilityOfSuccess;
    }

    obj = mProbabilityOfSuccess->getElementByMetaId(metaid);
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
DistribBinomialDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mNumberOfTrials, filter);
  ADD_FILTERED_POINTER(ret, sublist, mProbabilityOfSuccess, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribBinomialDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribDiscreteUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "numberOfTrials")
  {
    if (isSetNumberOfTrials())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribBinomialDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mNumberOfTrials;
    mNumberOfTrials = new DistribUncertValue(distribns);
    mNumberOfTrials->setElementName(name);
    obj = mNumberOfTrials;
  }
  else if (name == "probabilityOfSuccess")
  {
    if (isSetProbabilityOfSuccess())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribBinomialDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mProbabilityOfSuccess;
    mProbabilityOfSuccess = new DistribUncertValue(distribns);
    mProbabilityOfSuccess->setElementName(name);
    obj = mProbabilityOfSuccess;
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
DistribBinomialDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribDiscreteUnivariateDistribution::addExpectedAttributes(attributes);

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
DistribBinomialDistribution::readAttributes(const XMLAttributes& attributes,
                                            const ExpectedAttributes&
                                              expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribDiscreteUnivariateDistribution::readAttributes(attributes,
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
          DistribDistribBinomialDistributionAllowedCoreAttributes, pkgVersion,
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
DistribBinomialDistribution::readL3V1V1Attributes(const XMLAttributes&
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
DistribBinomialDistribution::readL3V2V1Attributes(const XMLAttributes&
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
DistribBinomialDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribDiscreteUnivariateDistribution::writeAttributes(stream);

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
DistribBinomialDistribution::writeL3V1V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribBinomialDistribution::writeL3V2V1Attributes(XMLOutputStream& stream)
  const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribBinomialDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribBinomialDistribution_t *
DistribBinomialDistribution_create(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion)
{
  return new DistribBinomialDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribBinomialDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribBinomialDistribution_t*
DistribBinomialDistribution_clone(const DistribBinomialDistribution_t* dbd)
{
  if (dbd != NULL)
  {
    return static_cast<DistribBinomialDistribution_t*>(dbd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribBinomialDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribBinomialDistribution_free(DistribBinomialDistribution_t* dbd)
{
  if (dbd != NULL)
  {
    delete dbd;
  }
}


/*
 * Returns the value of the "numberOfTrials" element of this
 * DistribBinomialDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribBinomialDistribution_getNumberOfTrials(const
  DistribBinomialDistribution_t * dbd)
{
  if (dbd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dbd->getNumberOfTrials());
}


/*
 * Returns the value of the "probabilityOfSuccess" element of this
 * DistribBinomialDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribBinomialDistribution_getProbabilityOfSuccess(const
  DistribBinomialDistribution_t * dbd)
{
  if (dbd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dbd->getProbabilityOfSuccess());
}


/*
 * Predicate returning @c 1 (true) if this DistribBinomialDistribution_t's
 * "numberOfTrials" element is set.
 */
LIBSBML_EXTERN
int
DistribBinomialDistribution_isSetNumberOfTrials(const
  DistribBinomialDistribution_t * dbd)
{
  return (dbd != NULL) ? static_cast<int>(dbd->isSetNumberOfTrials()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribBinomialDistribution_t's
 * "probabilityOfSuccess" element is set.
 */
LIBSBML_EXTERN
int
DistribBinomialDistribution_isSetProbabilityOfSuccess(const
  DistribBinomialDistribution_t * dbd)
{
  return (dbd != NULL) ? static_cast<int>(dbd->isSetProbabilityOfSuccess()) :
    0;
}


/*
 * Sets the value of the "numberOfTrials" element of this
 * DistribBinomialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribBinomialDistribution_setNumberOfTrials(
                                              DistribBinomialDistribution_t *
                                                dbd,
                                              const DistribUncertValue_t*
                                                numberOfTrials)
{
  return (dbd != NULL) ? dbd->setNumberOfTrials(numberOfTrials) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "probabilityOfSuccess" element of this
 * DistribBinomialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribBinomialDistribution_setProbabilityOfSuccess(
                                                    DistribBinomialDistribution_t
                                                      * dbd,
                                                    const DistribUncertValue_t*
                                                      probabilityOfSuccess)
{
  return (dbd != NULL) ? dbd->setProbabilityOfSuccess(probabilityOfSuccess) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribBinomialDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribBinomialDistribution_createNumberOfTrials(DistribBinomialDistribution_t*
  dbd)
{
  if (dbd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dbd->createNumberOfTrials());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribBinomialDistribution_t object and returns the DistribUncertValue_t
 * object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribBinomialDistribution_createProbabilityOfSuccess(DistribBinomialDistribution_t*
  dbd)
{
  if (dbd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dbd->createProbabilityOfSuccess());
}


/*
 * Unsets the value of the "numberOfTrials" element of this
 * DistribBinomialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribBinomialDistribution_unsetNumberOfTrials(DistribBinomialDistribution_t *
  dbd)
{
  return (dbd != NULL) ? dbd->unsetNumberOfTrials() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "probabilityOfSuccess" element of this
 * DistribBinomialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribBinomialDistribution_unsetProbabilityOfSuccess(DistribBinomialDistribution_t
  * dbd)
{
  return (dbd != NULL) ? dbd->unsetProbabilityOfSuccess() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribBinomialDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribBinomialDistribution_hasRequiredAttributes(const
  DistribBinomialDistribution_t * dbd)
{
  return (dbd != NULL) ? static_cast<int>(dbd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribBinomialDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribBinomialDistribution_hasRequiredElements(const
  DistribBinomialDistribution_t * dbd)
{
  return (dbd != NULL) ? static_cast<int>(dbd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


