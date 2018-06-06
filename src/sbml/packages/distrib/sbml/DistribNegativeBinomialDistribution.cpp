/**
 * @file DistribNegativeBinomialDistribution.cpp
 * @brief Implementation of the DistribNegativeBinomialDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribNegativeBinomialDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribNegativeBinomialDistribution using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
DistribNegativeBinomialDistribution::DistribNegativeBinomialDistribution(
                                                                         unsigned
                                                                           int
                                                                             level,
                                                                         unsigned
                                                                           int
                                                                             version,
                                                                         unsigned
                                                                           int
                                                                             pkgVersion)
  : DistribDiscreteUnivariateDistribution(level, version, pkgVersion)
  , mNumberOfFailures (NULL)
  , mProbability (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribNegativeBinomialDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribNegativeBinomialDistribution::DistribNegativeBinomialDistribution(DistribPkgNamespaces
  *distribns)
  : DistribDiscreteUnivariateDistribution(distribns)
  , mNumberOfFailures (NULL)
  , mProbability (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribNegativeBinomialDistribution.
 */
DistribNegativeBinomialDistribution::DistribNegativeBinomialDistribution(const
  DistribNegativeBinomialDistribution& orig)
  : DistribDiscreteUnivariateDistribution( orig )
  , mNumberOfFailures ( NULL )
  , mProbability ( NULL )
{
  if (orig.mNumberOfFailures != NULL)
  {
    mNumberOfFailures = orig.mNumberOfFailures->clone();
  }

  if (orig.mProbability != NULL)
  {
    mProbability = orig.mProbability->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribNegativeBinomialDistribution.
 */
DistribNegativeBinomialDistribution&
DistribNegativeBinomialDistribution::operator=(const
  DistribNegativeBinomialDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribDiscreteUnivariateDistribution::operator=(rhs);
    delete mNumberOfFailures;
    if (rhs.mNumberOfFailures != NULL)
    {
      mNumberOfFailures = rhs.mNumberOfFailures->clone();
    }
    else
    {
      mNumberOfFailures = NULL;
    }

    delete mProbability;
    if (rhs.mProbability != NULL)
    {
      mProbability = rhs.mProbability->clone();
    }
    else
    {
      mProbability = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribNegativeBinomialDistribution
 * object.
 */
DistribNegativeBinomialDistribution*
DistribNegativeBinomialDistribution::clone() const
{
  return new DistribNegativeBinomialDistribution(*this);
}


/*
 * Destructor for DistribNegativeBinomialDistribution.
 */
DistribNegativeBinomialDistribution::~DistribNegativeBinomialDistribution()
{
  delete mNumberOfFailures;
  mNumberOfFailures = NULL;
  delete mProbability;
  mProbability = NULL;
}


/*
 * Returns the value of the "numberOfFailures" element of this
 * DistribNegativeBinomialDistribution.
 */
const DistribUncertValue*
DistribNegativeBinomialDistribution::getNumberOfFailures() const
{
  return mNumberOfFailures;
}


/*
 * Returns the value of the "numberOfFailures" element of this
 * DistribNegativeBinomialDistribution.
 */
DistribUncertValue*
DistribNegativeBinomialDistribution::getNumberOfFailures()
{
  return mNumberOfFailures;
}


/*
 * Returns the value of the "probability" element of this
 * DistribNegativeBinomialDistribution.
 */
const DistribUncertValue*
DistribNegativeBinomialDistribution::getProbability() const
{
  return mProbability;
}


/*
 * Returns the value of the "probability" element of this
 * DistribNegativeBinomialDistribution.
 */
DistribUncertValue*
DistribNegativeBinomialDistribution::getProbability()
{
  return mProbability;
}


/*
 * Predicate returning @c true if this DistribNegativeBinomialDistribution's
 * "numberOfFailures" element is set.
 */
bool
DistribNegativeBinomialDistribution::isSetNumberOfFailures() const
{
  return (mNumberOfFailures != NULL);
}


/*
 * Predicate returning @c true if this DistribNegativeBinomialDistribution's
 * "probability" element is set.
 */
bool
DistribNegativeBinomialDistribution::isSetProbability() const
{
  return (mProbability != NULL);
}


/*
 * Sets the value of the "numberOfFailures" element of this
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::setNumberOfFailures(const
  DistribUncertValue* numberOfFailures)
{
  if (numberOfFailures == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (numberOfFailures->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != numberOfFailures->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != numberOfFailures->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != numberOfFailures->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mNumberOfFailures;
    mNumberOfFailures = (numberOfFailures != NULL) ?
      static_cast<DistribUncertValue*>(numberOfFailures->clone()) : NULL;
    if (mNumberOfFailures != NULL) mNumberOfFailures->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "probability" element of this
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::setProbability(const DistribUncertValue*
  probability)
{
  if (probability == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (probability->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != probability->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != probability->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != probability->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mProbability;
    mProbability = (probability != NULL) ?
      static_cast<DistribUncertValue*>(probability->clone()) : NULL;
    if (mProbability != NULL) mProbability->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribNegativeBinomialDistribution object and returns the
 * DistribUncertValue object created.
 */
DistribUncertValue*
DistribNegativeBinomialDistribution::createNumberOfFailures()
{
  if (mNumberOfFailures != NULL)
  {
    delete mNumberOfFailures;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mNumberOfFailures = new DistribUncertValue(distribns);

  mNumberOfFailures->setElementName("numberOfFailures");

  delete distribns;

  connectToChild();

  return mNumberOfFailures;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribNegativeBinomialDistribution object and returns the
 * DistribUncertValue object created.
 */
DistribUncertValue*
DistribNegativeBinomialDistribution::createProbability()
{
  if (mProbability != NULL)
  {
    delete mProbability;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mProbability = new DistribUncertValue(distribns);

  mProbability->setElementName("probability");

  delete distribns;

  connectToChild();

  return mProbability;
}


/*
 * Unsets the value of the "numberOfFailures" element of this
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::unsetNumberOfFailures()
{
  delete mNumberOfFailures;
  mNumberOfFailures = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "probability" element of this
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::unsetProbability()
{
  delete mProbability;
  mProbability = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribNegativeBinomialDistribution
 * object.
 */
const std::string&
DistribNegativeBinomialDistribution::getElementName() const
{
  static const string name = "negativeBinomialDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribNegativeBinomialDistribution
 * object.
 */
int
DistribNegativeBinomialDistribution::getTypeCode() const
{
  return SBML_DISTRIB_NEGATIVEBINOMIALDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribNegativeBinomialDistribution object have been set.
 */
bool
DistribNegativeBinomialDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribDiscreteUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribNegativeBinomialDistribution object have been set.
 */
bool
DistribNegativeBinomialDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribDiscreteUnivariateDistribution::hasRequiredElements();

  if (isSetNumberOfFailures() == false)
  {
    allPresent = false;
  }

  if (isSetProbability() == false)
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
DistribNegativeBinomialDistribution::writeElements(XMLOutputStream& stream)
  const
{
  DistribDiscreteUnivariateDistribution::writeElements(stream);

  if (isSetNumberOfFailures() == true)
  {
    mNumberOfFailures->write(stream);
  }

  if (isSetProbability() == true)
  {
    mProbability->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribNegativeBinomialDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mNumberOfFailures != NULL)
  {
    mNumberOfFailures->accept(v);
  }

  if (mProbability != NULL)
  {
    mProbability->accept(v);
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
DistribNegativeBinomialDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribDiscreteUnivariateDistribution::setSBMLDocument(d);

  if (mNumberOfFailures != NULL)
  {
    mNumberOfFailures->setSBMLDocument(d);
  }

  if (mProbability != NULL)
  {
    mProbability->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribNegativeBinomialDistribution::connectToChild()
{
  DistribDiscreteUnivariateDistribution::connectToChild();

  if (mNumberOfFailures != NULL)
  {
    mNumberOfFailures->connectToParent(this);
  }

  if (mProbability != NULL)
  {
    mProbability->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribNegativeBinomialDistribution::enablePackageInternal(
                                                           const std::string&
                                                             pkgURI,
                                                           const std::string&
                                                             pkgPrefix,
                                                           bool flag)
{
  DistribDiscreteUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetNumberOfFailures())
  {
    mNumberOfFailures->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetProbability())
  {
    mProbability->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribNegativeBinomialDistribution::updateSBMLNamespace(
                                                         const std::string&
                                                           package,
                                                         unsigned int level,
                                                         unsigned int version)
{
  DistribDiscreteUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mNumberOfFailures != NULL)
  {
    mNumberOfFailures->updateSBMLNamespace(package, level, version);
  }

  if (mProbability != NULL)
  {
    mProbability->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::getAttribute(
                                                  const std::string&
                                                    attributeName,
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
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::getAttribute(
                                                  const std::string&
                                                    attributeName,
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
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::getAttribute(
                                                  const std::string&
                                                    attributeName,
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
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::getAttribute(
                                                  const std::string&
                                                    attributeName,
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
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::getAttribute(
                                                  const std::string&
                                                    attributeName,
                                                  std::string& value) const
{
  int return_value =
    DistribDiscreteUnivariateDistribution::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribNegativeBinomialDistribution's
 * attribute "attributeName" is set.
 */
bool
DistribNegativeBinomialDistribution::isSetAttribute(const std::string&
  attributeName) const
{
  bool value =
    DistribDiscreteUnivariateDistribution::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::setAttribute(
                                                  const std::string&
                                                    attributeName,
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
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::setAttribute(
                                                  const std::string&
                                                    attributeName,
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
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::setAttribute(
                                                  const std::string&
                                                    attributeName,
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
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::setAttribute(
                                                  const std::string&
                                                    attributeName,
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
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::setAttribute(
                                                  const std::string&
                                                    attributeName,
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
 * DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::unsetAttribute(const std::string&
  attributeName)
{
  int value =
    DistribDiscreteUnivariateDistribution::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribNegativeBinomialDistribution.
 */
SBase*
DistribNegativeBinomialDistribution::createChildObject(const std::string&
  elementName)
{
  DistribDiscreteUnivariateDistribution* obj = NULL;

  if (elementName == "numberOfFailures")
  {
    return createNumberOfFailures();
  }
  else if (elementName == "probability")
  {
    return createProbability();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribNegativeBinomialDistribution.
 */
int
DistribNegativeBinomialDistribution::addChildObject(
                                                    const std::string&
                                                      elementName,
                                                    const SBase* element)
{
  if (elementName == "numberOfFailures" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setNumberOfFailures((const DistribUncertValue*)(element));
  }
  else if (elementName == "probability" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setProbability((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribNegativeBinomialDistribution.
 */
SBase*
DistribNegativeBinomialDistribution::removeChildObject(
                                                       const std::string&
                                                         elementName,
                                                       const std::string& id)
{
  if (elementName == "numberOfFailures")
  {
    DistribUncertValue * obj = getNumberOfFailures();
    if (unsetNumberOfFailures() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "probability")
  {
    DistribUncertValue * obj = getProbability();
    if (unsetProbability() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this
 * DistribNegativeBinomialDistribution.
 */
unsigned int
DistribNegativeBinomialDistribution::getNumObjects(const std::string&
  elementName)
{
  unsigned int n = 0;

  if (elementName == "numberOfFailures")
  {
    if (isSetNumberOfFailures())
    {
      return 1;
    }
  }
  else if (elementName == "probability")
  {
    if (isSetProbability())
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
 * DistribNegativeBinomialDistribution.
 */
SBase*
DistribNegativeBinomialDistribution::getObject(const std::string& elementName,
                                               unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "numberOfFailures")
  {
    return getNumberOfFailures();
  }
  else if (elementName == "probability")
  {
    return getProbability();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribNegativeBinomialDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mNumberOfFailures != NULL)
  {
    if (mNumberOfFailures->getId() == id)
    {
      return mNumberOfFailures;
    }

    obj = mNumberOfFailures->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mProbability != NULL)
  {
    if (mProbability->getId() == id)
    {
      return mProbability;
    }

    obj = mProbability->getElementBySId(id);
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
DistribNegativeBinomialDistribution::getElementByMetaId(const std::string&
  metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mNumberOfFailures != NULL)
  {
    if (mNumberOfFailures->getMetaId() == metaid)
    {
      return mNumberOfFailures;
    }

    obj = mNumberOfFailures->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mProbability != NULL)
  {
    if (mProbability->getMetaId() == metaid)
    {
      return mProbability;
    }

    obj = mProbability->getElementByMetaId(metaid);
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
DistribNegativeBinomialDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mNumberOfFailures, filter);
  ADD_FILTERED_POINTER(ret, sublist, mProbability, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribNegativeBinomialDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribDiscreteUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "numberOfFailures")
  {
    if (isSetNumberOfFailures())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribNegativeBinomialDistributionAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
    }

    delete mNumberOfFailures;
    mNumberOfFailures = new DistribUncertValue(distribns);
    mNumberOfFailures->setElementName(name);
    obj = mNumberOfFailures;
  }
  else if (name == "probability")
  {
    if (isSetProbability())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribNegativeBinomialDistributionAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
    }

    delete mProbability;
    mProbability = new DistribUncertValue(distribns);
    mProbability->setElementName(name);
    obj = mProbability;
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
DistribNegativeBinomialDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribDiscreteUnivariateDistribution::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribNegativeBinomialDistribution::readAttributes(
                                                    const XMLAttributes&
                                                      attributes,
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
          DistribDistribNegativeBinomialDistributionAllowedCoreAttributes,
            pkgVersion, level, version, details);
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
DistribNegativeBinomialDistribution::writeAttributes(XMLOutputStream& stream)
  const
{
  DistribDiscreteUnivariateDistribution::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribNegativeBinomialDistribution_t using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribNegativeBinomialDistribution_t *
DistribNegativeBinomialDistribution_create(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion)
{
  return new DistribNegativeBinomialDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this
 * DistribNegativeBinomialDistribution_t object.
 */
LIBSBML_EXTERN
DistribNegativeBinomialDistribution_t*
DistribNegativeBinomialDistribution_clone(const
  DistribNegativeBinomialDistribution_t* dnbd)
{
  if (dnbd != NULL)
  {
    return static_cast<DistribNegativeBinomialDistribution_t*>(dnbd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribNegativeBinomialDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribNegativeBinomialDistribution_free(DistribNegativeBinomialDistribution_t*
  dnbd)
{
  if (dnbd != NULL)
  {
    delete dnbd;
  }
}


/*
 * Returns the value of the "numberOfFailures" element of this
 * DistribNegativeBinomialDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribNegativeBinomialDistribution_getNumberOfFailures(const
  DistribNegativeBinomialDistribution_t * dnbd)
{
  if (dnbd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dnbd->getNumberOfFailures());
}


/*
 * Returns the value of the "probability" element of this
 * DistribNegativeBinomialDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribNegativeBinomialDistribution_getProbability(const
  DistribNegativeBinomialDistribution_t * dnbd)
{
  if (dnbd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dnbd->getProbability());
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribNegativeBinomialDistribution_t's "numberOfFailures" element is set.
 */
LIBSBML_EXTERN
int
DistribNegativeBinomialDistribution_isSetNumberOfFailures(const
  DistribNegativeBinomialDistribution_t * dnbd)
{
  return (dnbd != NULL) ? static_cast<int>(dnbd->isSetNumberOfFailures()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribNegativeBinomialDistribution_t's "probability" element is set.
 */
LIBSBML_EXTERN
int
DistribNegativeBinomialDistribution_isSetProbability(const
  DistribNegativeBinomialDistribution_t * dnbd)
{
  return (dnbd != NULL) ? static_cast<int>(dnbd->isSetProbability()) : 0;
}


/*
 * Sets the value of the "numberOfFailures" element of this
 * DistribNegativeBinomialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribNegativeBinomialDistribution_setNumberOfFailures(
                                                        DistribNegativeBinomialDistribution_t
                                                          * dnbd,
                                                        const
                                                          DistribUncertValue_t*
                                                            numberOfFailures)
{
  return (dnbd != NULL) ? dnbd->setNumberOfFailures(numberOfFailures) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "probability" element of this
 * DistribNegativeBinomialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribNegativeBinomialDistribution_setProbability(
                                                   DistribNegativeBinomialDistribution_t
                                                     * dnbd,
                                                   const DistribUncertValue_t*
                                                     probability)
{
  return (dnbd != NULL) ? dnbd->setProbability(probability) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribNegativeBinomialDistribution_t object and returns the
 * DistribUncertValue_t object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribNegativeBinomialDistribution_createNumberOfFailures(DistribNegativeBinomialDistribution_t*
  dnbd)
{
  if (dnbd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dnbd->createNumberOfFailures());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribNegativeBinomialDistribution_t object and returns the
 * DistribUncertValue_t object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribNegativeBinomialDistribution_createProbability(DistribNegativeBinomialDistribution_t*
  dnbd)
{
  if (dnbd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dnbd->createProbability());
}


/*
 * Unsets the value of the "numberOfFailures" element of this
 * DistribNegativeBinomialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribNegativeBinomialDistribution_unsetNumberOfFailures(DistribNegativeBinomialDistribution_t
  * dnbd)
{
  return (dnbd != NULL) ? dnbd->unsetNumberOfFailures() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "probability" element of this
 * DistribNegativeBinomialDistribution_t.
 */
LIBSBML_EXTERN
int
DistribNegativeBinomialDistribution_unsetProbability(DistribNegativeBinomialDistribution_t
  * dnbd)
{
  return (dnbd != NULL) ? dnbd->unsetProbability() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribNegativeBinomialDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribNegativeBinomialDistribution_hasRequiredAttributes(const
  DistribNegativeBinomialDistribution_t * dnbd)
{
  return (dnbd != NULL) ? static_cast<int>(dnbd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribNegativeBinomialDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribNegativeBinomialDistribution_hasRequiredElements(const
  DistribNegativeBinomialDistribution_t * dnbd)
{
  return (dnbd != NULL) ? static_cast<int>(dnbd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


