/**
 * @file DistribHypergeometricDistribution.cpp
 * @brief Implementation of the DistribHypergeometricDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribHypergeometricDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribHypergeometricDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribHypergeometricDistribution::DistribHypergeometricDistribution(
                                                                     unsigned
                                                                       int level,
                                                                     unsigned
                                                                       int
                                                                         version,
                                                                     unsigned
                                                                       int
                                                                         pkgVersion)
  : DistribDiscreteUnivariateDistribution(level, version, pkgVersion)
  , mNumberOfSuccesses (NULL)
  , mNumberOfTrials (NULL)
  , mPopulationSize (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribHypergeometricDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribHypergeometricDistribution::DistribHypergeometricDistribution(DistribPkgNamespaces
  *distribns)
  : DistribDiscreteUnivariateDistribution(distribns)
  , mNumberOfSuccesses (NULL)
  , mNumberOfTrials (NULL)
  , mPopulationSize (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribHypergeometricDistribution.
 */
DistribHypergeometricDistribution::DistribHypergeometricDistribution(const
  DistribHypergeometricDistribution& orig)
  : DistribDiscreteUnivariateDistribution( orig )
  , mNumberOfSuccesses ( NULL )
  , mNumberOfTrials ( NULL )
  , mPopulationSize ( NULL )
{
  if (orig.mNumberOfSuccesses != NULL)
  {
    mNumberOfSuccesses = orig.mNumberOfSuccesses->clone();
  }

  if (orig.mNumberOfTrials != NULL)
  {
    mNumberOfTrials = orig.mNumberOfTrials->clone();
  }

  if (orig.mPopulationSize != NULL)
  {
    mPopulationSize = orig.mPopulationSize->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribHypergeometricDistribution.
 */
DistribHypergeometricDistribution&
DistribHypergeometricDistribution::operator=(const
  DistribHypergeometricDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribDiscreteUnivariateDistribution::operator=(rhs);
    delete mNumberOfSuccesses;
    if (rhs.mNumberOfSuccesses != NULL)
    {
      mNumberOfSuccesses = rhs.mNumberOfSuccesses->clone();
    }
    else
    {
      mNumberOfSuccesses = NULL;
    }

    delete mNumberOfTrials;
    if (rhs.mNumberOfTrials != NULL)
    {
      mNumberOfTrials = rhs.mNumberOfTrials->clone();
    }
    else
    {
      mNumberOfTrials = NULL;
    }

    delete mPopulationSize;
    if (rhs.mPopulationSize != NULL)
    {
      mPopulationSize = rhs.mPopulationSize->clone();
    }
    else
    {
      mPopulationSize = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribHypergeometricDistribution
 * object.
 */
DistribHypergeometricDistribution*
DistribHypergeometricDistribution::clone() const
{
  return new DistribHypergeometricDistribution(*this);
}


/*
 * Destructor for DistribHypergeometricDistribution.
 */
DistribHypergeometricDistribution::~DistribHypergeometricDistribution()
{
  delete mNumberOfSuccesses;
  mNumberOfSuccesses = NULL;
  delete mNumberOfTrials;
  mNumberOfTrials = NULL;
  delete mPopulationSize;
  mPopulationSize = NULL;
}


/*
 * Returns the value of the "numberOfSuccesses" element of this
 * DistribHypergeometricDistribution.
 */
const DistribUncertValue*
DistribHypergeometricDistribution::getNumberOfSuccesses() const
{
  return mNumberOfSuccesses;
}


/*
 * Returns the value of the "numberOfSuccesses" element of this
 * DistribHypergeometricDistribution.
 */
DistribUncertValue*
DistribHypergeometricDistribution::getNumberOfSuccesses()
{
  return mNumberOfSuccesses;
}


/*
 * Returns the value of the "numberOfTrials" element of this
 * DistribHypergeometricDistribution.
 */
const DistribUncertValue*
DistribHypergeometricDistribution::getNumberOfTrials() const
{
  return mNumberOfTrials;
}


/*
 * Returns the value of the "numberOfTrials" element of this
 * DistribHypergeometricDistribution.
 */
DistribUncertValue*
DistribHypergeometricDistribution::getNumberOfTrials()
{
  return mNumberOfTrials;
}


/*
 * Returns the value of the "populationSize" element of this
 * DistribHypergeometricDistribution.
 */
const DistribUncertValue*
DistribHypergeometricDistribution::getPopulationSize() const
{
  return mPopulationSize;
}


/*
 * Returns the value of the "populationSize" element of this
 * DistribHypergeometricDistribution.
 */
DistribUncertValue*
DistribHypergeometricDistribution::getPopulationSize()
{
  return mPopulationSize;
}


/*
 * Predicate returning @c true if this DistribHypergeometricDistribution's
 * "numberOfSuccesses" element is set.
 */
bool
DistribHypergeometricDistribution::isSetNumberOfSuccesses() const
{
  return (mNumberOfSuccesses != NULL);
}


/*
 * Predicate returning @c true if this DistribHypergeometricDistribution's
 * "numberOfTrials" element is set.
 */
bool
DistribHypergeometricDistribution::isSetNumberOfTrials() const
{
  return (mNumberOfTrials != NULL);
}


/*
 * Predicate returning @c true if this DistribHypergeometricDistribution's
 * "populationSize" element is set.
 */
bool
DistribHypergeometricDistribution::isSetPopulationSize() const
{
  return (mPopulationSize != NULL);
}


/*
 * Sets the value of the "numberOfSuccesses" element of this
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::setNumberOfSuccesses(const
  DistribUncertValue* numberOfSuccesses)
{
  if (numberOfSuccesses == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (numberOfSuccesses->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != numberOfSuccesses->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != numberOfSuccesses->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != numberOfSuccesses->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mNumberOfSuccesses;
    mNumberOfSuccesses = (numberOfSuccesses != NULL) ?
      static_cast<DistribUncertValue*>(numberOfSuccesses->clone()) : NULL;
    if (mNumberOfSuccesses != NULL) mNumberOfSuccesses->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "numberOfTrials" element of this
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::setNumberOfTrials(const DistribUncertValue*
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
 * Sets the value of the "populationSize" element of this
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::setPopulationSize(const DistribUncertValue*
  populationSize)
{
  if (populationSize == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (populationSize->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != populationSize->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != populationSize->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != populationSize->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mPopulationSize;
    mPopulationSize = (populationSize != NULL) ?
      static_cast<DistribUncertValue*>(populationSize->clone()) : NULL;
    if (mPopulationSize != NULL) mPopulationSize->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribHypergeometricDistribution object and returns the DistribUncertValue
 * object created.
 */
DistribUncertValue*
DistribHypergeometricDistribution::createNumberOfSuccesses()
{
  if (mNumberOfSuccesses != NULL)
  {
    delete mNumberOfSuccesses;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mNumberOfSuccesses = new DistribUncertValue(distribns);

  mNumberOfSuccesses->setElementName("numberOfSuccesses");

  delete distribns;

  connectToChild();

  return mNumberOfSuccesses;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribHypergeometricDistribution object and returns the DistribUncertValue
 * object created.
 */
DistribUncertValue*
DistribHypergeometricDistribution::createNumberOfTrials()
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
 * DistribHypergeometricDistribution object and returns the DistribUncertValue
 * object created.
 */
DistribUncertValue*
DistribHypergeometricDistribution::createPopulationSize()
{
  if (mPopulationSize != NULL)
  {
    delete mPopulationSize;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mPopulationSize = new DistribUncertValue(distribns);

  mPopulationSize->setElementName("populationSize");

  delete distribns;

  connectToChild();

  return mPopulationSize;
}


/*
 * Unsets the value of the "numberOfSuccesses" element of this
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::unsetNumberOfSuccesses()
{
  delete mNumberOfSuccesses;
  mNumberOfSuccesses = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "numberOfTrials" element of this
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::unsetNumberOfTrials()
{
  delete mNumberOfTrials;
  mNumberOfTrials = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "populationSize" element of this
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::unsetPopulationSize()
{
  delete mPopulationSize;
  mPopulationSize = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribHypergeometricDistribution
 * object.
 */
const std::string&
DistribHypergeometricDistribution::getElementName() const
{
  static const string name = "hypergeometricDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribHypergeometricDistribution
 * object.
 */
int
DistribHypergeometricDistribution::getTypeCode() const
{
  return SBML_DISTRIB_HYPERGEOMETRICDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribHypergeometricDistribution object have been set.
 */
bool
DistribHypergeometricDistribution::hasRequiredAttributes() const
{
  bool allPresent =
    DistribDiscreteUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribHypergeometricDistribution object have been set.
 */
bool
DistribHypergeometricDistribution::hasRequiredElements() const
{
  bool allPresent =
    DistribDiscreteUnivariateDistribution::hasRequiredElements();

  if (isSetNumberOfSuccesses() == false)
  {
    allPresent = false;
  }

  if (isSetNumberOfTrials() == false)
  {
    allPresent = false;
  }

  if (isSetPopulationSize() == false)
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
DistribHypergeometricDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribDiscreteUnivariateDistribution::writeElements(stream);

  if (isSetNumberOfSuccesses() == true)
  {
    mNumberOfSuccesses->write(stream);
  }

  if (isSetNumberOfTrials() == true)
  {
    mNumberOfTrials->write(stream);
  }

  if (isSetPopulationSize() == true)
  {
    mPopulationSize->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribHypergeometricDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mNumberOfSuccesses != NULL)
  {
    mNumberOfSuccesses->accept(v);
  }

  if (mNumberOfTrials != NULL)
  {
    mNumberOfTrials->accept(v);
  }

  if (mPopulationSize != NULL)
  {
    mPopulationSize->accept(v);
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
DistribHypergeometricDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribDiscreteUnivariateDistribution::setSBMLDocument(d);

  if (mNumberOfSuccesses != NULL)
  {
    mNumberOfSuccesses->setSBMLDocument(d);
  }

  if (mNumberOfTrials != NULL)
  {
    mNumberOfTrials->setSBMLDocument(d);
  }

  if (mPopulationSize != NULL)
  {
    mPopulationSize->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribHypergeometricDistribution::connectToChild()
{
  DistribDiscreteUnivariateDistribution::connectToChild();

  if (mNumberOfSuccesses != NULL)
  {
    mNumberOfSuccesses->connectToParent(this);
  }

  if (mNumberOfTrials != NULL)
  {
    mNumberOfTrials->connectToParent(this);
  }

  if (mPopulationSize != NULL)
  {
    mPopulationSize->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribHypergeometricDistribution::enablePackageInternal(
                                                         const std::string&
                                                           pkgURI,
                                                         const std::string&
                                                           pkgPrefix,
                                                         bool flag)
{
  DistribDiscreteUnivariateDistribution::enablePackageInternal(pkgURI,
    pkgPrefix, flag);

  if (isSetNumberOfSuccesses())
  {
    mNumberOfSuccesses->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetNumberOfTrials())
  {
    mNumberOfTrials->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetPopulationSize())
  {
    mPopulationSize->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribHypergeometricDistribution::updateSBMLNamespace(
                                                       const std::string&
                                                         package,
                                                       unsigned int level,
                                                       unsigned int version)
{
  DistribDiscreteUnivariateDistribution::updateSBMLNamespace(package, level,
    version);

  if (mNumberOfSuccesses != NULL)
  {
    mNumberOfSuccesses->updateSBMLNamespace(package, level, version);
  }

  if (mNumberOfTrials != NULL)
  {
    mNumberOfTrials->updateSBMLNamespace(package, level, version);
  }

  if (mPopulationSize != NULL)
  {
    mPopulationSize->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::getAttribute(
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
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::getAttribute(
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
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::getAttribute(
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
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::getAttribute(
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
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::getAttribute(
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
 * Predicate returning @c true if this DistribHypergeometricDistribution's
 * attribute "attributeName" is set.
 */
bool
DistribHypergeometricDistribution::isSetAttribute(const std::string&
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
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::setAttribute(
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
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::setAttribute(
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
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::setAttribute(
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
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::setAttribute(
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
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::setAttribute(
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
 * DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::unsetAttribute(const std::string&
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
 * DistribHypergeometricDistribution.
 */
SBase*
DistribHypergeometricDistribution::createChildObject(const std::string&
  elementName)
{
  DistribDiscreteUnivariateDistribution* obj = NULL;

  if (elementName == "numberOfSuccesses")
  {
    return createNumberOfSuccesses();
  }
  else if (elementName == "numberOfTrials")
  {
    return createNumberOfTrials();
  }
  else if (elementName == "populationSize")
  {
    return createPopulationSize();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribHypergeometricDistribution.
 */
int
DistribHypergeometricDistribution::addChildObject(
                                                  const std::string&
                                                    elementName,
                                                  const SBase* element)
{
  if (elementName == "numberOfSuccesses" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setNumberOfSuccesses((const DistribUncertValue*)(element));
  }
  else if (elementName == "numberOfTrials" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setNumberOfTrials((const DistribUncertValue*)(element));
  }
  else if (elementName == "populationSize" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setPopulationSize((const DistribUncertValue*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribHypergeometricDistribution.
 */
SBase*
DistribHypergeometricDistribution::removeChildObject(
                                                     const std::string&
                                                       elementName,
                                                     const std::string& id)
{
  if (elementName == "numberOfSuccesses")
  {
    DistribUncertValue * obj = getNumberOfSuccesses();
    if (unsetNumberOfSuccesses() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "numberOfTrials")
  {
    DistribUncertValue * obj = getNumberOfTrials();
    if (unsetNumberOfTrials() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "populationSize")
  {
    DistribUncertValue * obj = getPopulationSize();
    if (unsetPopulationSize() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this
 * DistribHypergeometricDistribution.
 */
unsigned int
DistribHypergeometricDistribution::getNumObjects(const std::string&
  elementName)
{
  unsigned int n = 0;

  if (elementName == "numberOfSuccesses")
  {
    if (isSetNumberOfSuccesses())
    {
      return 1;
    }
  }
  else if (elementName == "numberOfTrials")
  {
    if (isSetNumberOfTrials())
    {
      return 1;
    }
  }
  else if (elementName == "populationSize")
  {
    if (isSetPopulationSize())
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
 * DistribHypergeometricDistribution.
 */
SBase*
DistribHypergeometricDistribution::getObject(const std::string& elementName,
                                             unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "numberOfSuccesses")
  {
    return getNumberOfSuccesses();
  }
  else if (elementName == "numberOfTrials")
  {
    return getNumberOfTrials();
  }
  else if (elementName == "populationSize")
  {
    return getPopulationSize();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribHypergeometricDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mNumberOfSuccesses != NULL)
  {
    if (mNumberOfSuccesses->getId() == id)
    {
      return mNumberOfSuccesses;
    }

    obj = mNumberOfSuccesses->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

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

  if (mPopulationSize != NULL)
  {
    if (mPopulationSize->getId() == id)
    {
      return mPopulationSize;
    }

    obj = mPopulationSize->getElementBySId(id);
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
DistribHypergeometricDistribution::getElementByMetaId(const std::string&
  metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mNumberOfSuccesses != NULL)
  {
    if (mNumberOfSuccesses->getMetaId() == metaid)
    {
      return mNumberOfSuccesses;
    }

    obj = mNumberOfSuccesses->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

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

  if (mPopulationSize != NULL)
  {
    if (mPopulationSize->getMetaId() == metaid)
    {
      return mPopulationSize;
    }

    obj = mPopulationSize->getElementByMetaId(metaid);
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
DistribHypergeometricDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mNumberOfSuccesses, filter);
  ADD_FILTERED_POINTER(ret, sublist, mNumberOfTrials, filter);
  ADD_FILTERED_POINTER(ret, sublist, mPopulationSize, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribHypergeometricDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribDiscreteUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "numberOfSuccesses")
  {
    if (isSetNumberOfSuccesses())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribHypergeometricDistributionAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
    }

    delete mNumberOfSuccesses;
    mNumberOfSuccesses = new DistribUncertValue(distribns);
    mNumberOfSuccesses->setElementName(name);
    obj = mNumberOfSuccesses;
  }
  else if (name == "numberOfTrials")
  {
    if (isSetNumberOfTrials())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribHypergeometricDistributionAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
    }

    delete mNumberOfTrials;
    mNumberOfTrials = new DistribUncertValue(distribns);
    mNumberOfTrials->setElementName(name);
    obj = mNumberOfTrials;
  }
  else if (name == "populationSize")
  {
    if (isSetPopulationSize())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribHypergeometricDistributionAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
    }

    delete mPopulationSize;
    mPopulationSize = new DistribUncertValue(distribns);
    mPopulationSize->setElementName(name);
    obj = mPopulationSize;
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
DistribHypergeometricDistribution::addExpectedAttributes(ExpectedAttributes&
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
DistribHypergeometricDistribution::readAttributes(
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
          DistribDistribHypergeometricDistributionAllowedCoreAttributes,
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
DistribHypergeometricDistribution::writeAttributes(XMLOutputStream& stream)
  const
{
  DistribDiscreteUnivariateDistribution::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribHypergeometricDistribution_t using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribHypergeometricDistribution_t *
DistribHypergeometricDistribution_create(unsigned int level,
                                         unsigned int version,
                                         unsigned int pkgVersion)
{
  return new DistribHypergeometricDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribHypergeometricDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribHypergeometricDistribution_t*
DistribHypergeometricDistribution_clone(const
  DistribHypergeometricDistribution_t* dhd)
{
  if (dhd != NULL)
  {
    return static_cast<DistribHypergeometricDistribution_t*>(dhd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribHypergeometricDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribHypergeometricDistribution_free(DistribHypergeometricDistribution_t*
  dhd)
{
  if (dhd != NULL)
  {
    delete dhd;
  }
}


/*
 * Returns the value of the "numberOfSuccesses" element of this
 * DistribHypergeometricDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribHypergeometricDistribution_getNumberOfSuccesses(const
  DistribHypergeometricDistribution_t * dhd)
{
  if (dhd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dhd->getNumberOfSuccesses());
}


/*
 * Returns the value of the "numberOfTrials" element of this
 * DistribHypergeometricDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribHypergeometricDistribution_getNumberOfTrials(const
  DistribHypergeometricDistribution_t * dhd)
{
  if (dhd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dhd->getNumberOfTrials());
}


/*
 * Returns the value of the "populationSize" element of this
 * DistribHypergeometricDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribHypergeometricDistribution_getPopulationSize(const
  DistribHypergeometricDistribution_t * dhd)
{
  if (dhd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dhd->getPopulationSize());
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribHypergeometricDistribution_t's "numberOfSuccesses" element is set.
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_isSetNumberOfSuccesses(const
  DistribHypergeometricDistribution_t * dhd)
{
  return (dhd != NULL) ? static_cast<int>(dhd->isSetNumberOfSuccesses()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribHypergeometricDistribution_t's "numberOfTrials" element is set.
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_isSetNumberOfTrials(const
  DistribHypergeometricDistribution_t * dhd)
{
  return (dhd != NULL) ? static_cast<int>(dhd->isSetNumberOfTrials()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribHypergeometricDistribution_t's "populationSize" element is set.
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_isSetPopulationSize(const
  DistribHypergeometricDistribution_t * dhd)
{
  return (dhd != NULL) ? static_cast<int>(dhd->isSetPopulationSize()) : 0;
}


/*
 * Sets the value of the "numberOfSuccesses" element of this
 * DistribHypergeometricDistribution_t.
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_setNumberOfSuccesses(
                                                       DistribHypergeometricDistribution_t
                                                         * dhd,
                                                       const
                                                         DistribUncertValue_t*
                                                           numberOfSuccesses)
{
  return (dhd != NULL) ? dhd->setNumberOfSuccesses(numberOfSuccesses) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "numberOfTrials" element of this
 * DistribHypergeometricDistribution_t.
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_setNumberOfTrials(
                                                    DistribHypergeometricDistribution_t
                                                      * dhd,
                                                    const DistribUncertValue_t*
                                                      numberOfTrials)
{
  return (dhd != NULL) ? dhd->setNumberOfTrials(numberOfTrials) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "populationSize" element of this
 * DistribHypergeometricDistribution_t.
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_setPopulationSize(
                                                    DistribHypergeometricDistribution_t
                                                      * dhd,
                                                    const DistribUncertValue_t*
                                                      populationSize)
{
  return (dhd != NULL) ? dhd->setPopulationSize(populationSize) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribHypergeometricDistribution_t object and returns the
 * DistribUncertValue_t object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribHypergeometricDistribution_createNumberOfSuccesses(DistribHypergeometricDistribution_t*
  dhd)
{
  if (dhd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dhd->createNumberOfSuccesses());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribHypergeometricDistribution_t object and returns the
 * DistribUncertValue_t object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribHypergeometricDistribution_createNumberOfTrials(DistribHypergeometricDistribution_t*
  dhd)
{
  if (dhd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dhd->createNumberOfTrials());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribHypergeometricDistribution_t object and returns the
 * DistribUncertValue_t object created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribHypergeometricDistribution_createPopulationSize(DistribHypergeometricDistribution_t*
  dhd)
{
  if (dhd == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dhd->createPopulationSize());
}


/*
 * Unsets the value of the "numberOfSuccesses" element of this
 * DistribHypergeometricDistribution_t.
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_unsetNumberOfSuccesses(DistribHypergeometricDistribution_t
  * dhd)
{
  return (dhd != NULL) ? dhd->unsetNumberOfSuccesses() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "numberOfTrials" element of this
 * DistribHypergeometricDistribution_t.
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_unsetNumberOfTrials(DistribHypergeometricDistribution_t
  * dhd)
{
  return (dhd != NULL) ? dhd->unsetNumberOfTrials() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "populationSize" element of this
 * DistribHypergeometricDistribution_t.
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_unsetPopulationSize(DistribHypergeometricDistribution_t
  * dhd)
{
  return (dhd != NULL) ? dhd->unsetPopulationSize() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribHypergeometricDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_hasRequiredAttributes(const
  DistribHypergeometricDistribution_t * dhd)
{
  return (dhd != NULL) ? static_cast<int>(dhd->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribHypergeometricDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribHypergeometricDistribution_hasRequiredElements(const
  DistribHypergeometricDistribution_t * dhd)
{
  return (dhd != NULL) ? static_cast<int>(dhd->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


