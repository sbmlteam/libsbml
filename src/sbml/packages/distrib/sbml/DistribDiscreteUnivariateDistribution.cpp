/**
 * @file DistribDiscreteUnivariateDistribution.cpp
 * @brief Implementation of the DistribDiscreteUnivariateDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribDiscreteUnivariateDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>

#include <sbml/packages/distrib/sbml/DistribBinomialDistribution.h>
#include <sbml/packages/distrib/sbml/DistribGeometricDistribution.h>
#include <sbml/packages/distrib/sbml/DistribHypergeometricDistribution.h>
#include <sbml/packages/distrib/sbml/DistribNegativeBinomialDistribution.h>
#include <sbml/packages/distrib/sbml/DistribPoissonDistribution.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribDiscreteUnivariateDistribution using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
DistribDiscreteUnivariateDistribution::DistribDiscreteUnivariateDistribution(
                                                                             unsigned
                                                                               int
                                                                                 level,
                                                                             unsigned
                                                                               int
                                                                                 version,
                                                                             unsigned
                                                                               int
                                                                                 pkgVersion)
  : DistribUnivariateDistribution(level, version)
  , mTruncationLowerBound (NULL)
  , mTruncationUpperBound (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribDiscreteUnivariateDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribDiscreteUnivariateDistribution::DistribDiscreteUnivariateDistribution(DistribPkgNamespaces
  *distribns)
  : DistribUnivariateDistribution(distribns)
  , mTruncationLowerBound (NULL)
  , mTruncationUpperBound (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribDiscreteUnivariateDistribution.
 */
DistribDiscreteUnivariateDistribution::DistribDiscreteUnivariateDistribution(const
  DistribDiscreteUnivariateDistribution& orig)
  : DistribUnivariateDistribution( orig )
  , mTruncationLowerBound ( NULL )
  , mTruncationUpperBound ( NULL )
{
  if (orig.mTruncationLowerBound != NULL)
  {
    mTruncationLowerBound = orig.mTruncationLowerBound->clone();
  }

  if (orig.mTruncationUpperBound != NULL)
  {
    mTruncationUpperBound = orig.mTruncationUpperBound->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribDiscreteUnivariateDistribution.
 */
DistribDiscreteUnivariateDistribution&
DistribDiscreteUnivariateDistribution::operator=(const
  DistribDiscreteUnivariateDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribUnivariateDistribution::operator=(rhs);
    delete mTruncationLowerBound;
    if (rhs.mTruncationLowerBound != NULL)
    {
      mTruncationLowerBound = rhs.mTruncationLowerBound->clone();
    }
    else
    {
      mTruncationLowerBound = NULL;
    }

    delete mTruncationUpperBound;
    if (rhs.mTruncationUpperBound != NULL)
    {
      mTruncationUpperBound = rhs.mTruncationUpperBound->clone();
    }
    else
    {
      mTruncationUpperBound = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this
 * DistribDiscreteUnivariateDistribution object.
 */
DistribDiscreteUnivariateDistribution*
DistribDiscreteUnivariateDistribution::clone() const
{
  return new DistribDiscreteUnivariateDistribution(*this);
}


/*
 * Destructor for DistribDiscreteUnivariateDistribution.
 */
DistribDiscreteUnivariateDistribution::~DistribDiscreteUnivariateDistribution()
{
  delete mTruncationLowerBound;
  mTruncationLowerBound = NULL;
  delete mTruncationUpperBound;
  mTruncationUpperBound = NULL;
}


/*
 * Returns the value of the "id" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
const std::string&
DistribDiscreteUnivariateDistribution::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
const std::string&
DistribDiscreteUnivariateDistribution::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribDiscreteUnivariateDistribution's
 * "id" attribute is set.
 */
bool
DistribDiscreteUnivariateDistribution::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribDiscreteUnivariateDistribution's
 * "name" attribute is set.
 */
bool
DistribDiscreteUnivariateDistribution::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::unsetId()
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
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::unsetName()
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
 * Returns the value of the "truncationLowerBound" element of this
 * DistribDiscreteUnivariateDistribution.
 */
const DistribUncertBound*
DistribDiscreteUnivariateDistribution::getTruncationLowerBound() const
{
  return mTruncationLowerBound;
}


/*
 * Returns the value of the "truncationLowerBound" element of this
 * DistribDiscreteUnivariateDistribution.
 */
DistribUncertBound*
DistribDiscreteUnivariateDistribution::getTruncationLowerBound()
{
  return mTruncationLowerBound;
}


/*
 * Returns the value of the "truncationUpperBound" element of this
 * DistribDiscreteUnivariateDistribution.
 */
const DistribUncertBound*
DistribDiscreteUnivariateDistribution::getTruncationUpperBound() const
{
  return mTruncationUpperBound;
}


/*
 * Returns the value of the "truncationUpperBound" element of this
 * DistribDiscreteUnivariateDistribution.
 */
DistribUncertBound*
DistribDiscreteUnivariateDistribution::getTruncationUpperBound()
{
  return mTruncationUpperBound;
}


/*
 * Predicate returning @c true if this DistribDiscreteUnivariateDistribution's
 * "truncationLowerBound" element is set.
 */
bool
DistribDiscreteUnivariateDistribution::isSetTruncationLowerBound() const
{
  return (mTruncationLowerBound != NULL);
}


/*
 * Predicate returning @c true if this DistribDiscreteUnivariateDistribution's
 * "truncationUpperBound" element is set.
 */
bool
DistribDiscreteUnivariateDistribution::isSetTruncationUpperBound() const
{
  return (mTruncationUpperBound != NULL);
}


/*
 * Sets the value of the "truncationLowerBound" element of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::setTruncationLowerBound(const
  DistribUncertBound* truncationLowerBound)
{
  if (truncationLowerBound == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (truncationLowerBound->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != truncationLowerBound->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != truncationLowerBound->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != truncationLowerBound->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mTruncationLowerBound;
    mTruncationLowerBound = (truncationLowerBound != NULL) ?
      static_cast<DistribUncertBound*>(truncationLowerBound->clone()) : NULL;
    if (mTruncationLowerBound != NULL)
      mTruncationLowerBound->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "truncationUpperBound" element of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::setTruncationUpperBound(const
  DistribUncertBound* truncationUpperBound)
{
  if (truncationUpperBound == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (truncationUpperBound->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != truncationUpperBound->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != truncationUpperBound->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != truncationUpperBound->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mTruncationUpperBound;
    mTruncationUpperBound = (truncationUpperBound != NULL) ?
      static_cast<DistribUncertBound*>(truncationUpperBound->clone()) : NULL;
    if (mTruncationUpperBound != NULL)
      mTruncationUpperBound->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertBound object, adds it to this
 * DistribDiscreteUnivariateDistribution object and returns the
 * DistribUncertBound object created.
 */
DistribUncertBound*
DistribDiscreteUnivariateDistribution::createTruncationLowerBound()
{
  if (mTruncationLowerBound != NULL)
  {
    delete mTruncationLowerBound;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mTruncationLowerBound = new DistribUncertBound(distribns);

  mTruncationLowerBound->setElementName("truncationLowerBound");

  delete distribns;

  connectToChild();

  return mTruncationLowerBound;
}


/*
 * Creates a new DistribUncertBound object, adds it to this
 * DistribDiscreteUnivariateDistribution object and returns the
 * DistribUncertBound object created.
 */
DistribUncertBound*
DistribDiscreteUnivariateDistribution::createTruncationUpperBound()
{
  if (mTruncationUpperBound != NULL)
  {
    delete mTruncationUpperBound;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mTruncationUpperBound = new DistribUncertBound(distribns);

  mTruncationUpperBound->setElementName("truncationUpperBound");

  delete distribns;

  connectToChild();

  return mTruncationUpperBound;
}


/*
 * Unsets the value of the "truncationLowerBound" element of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::unsetTruncationLowerBound()
{
  delete mTruncationLowerBound;
  mTruncationLowerBound = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "truncationUpperBound" element of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::unsetTruncationUpperBound()
{
  delete mTruncationUpperBound;
  mTruncationUpperBound = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribDiscreteUnivariateDistribution" is of type
 * DistribBinomialDistribution
 */
bool
DistribDiscreteUnivariateDistribution::isDistribBinomialDistribution() const
{
  return dynamic_cast<const DistribBinomialDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribDiscreteUnivariateDistribution" is of type
 * DistribGeometricDistribution
 */
bool
DistribDiscreteUnivariateDistribution::isDistribGeometricDistribution() const
{
  return dynamic_cast<const DistribGeometricDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribDiscreteUnivariateDistribution" is of type
 * DistribHypergeometricDistribution
 */
bool
DistribDiscreteUnivariateDistribution::isDistribHypergeometricDistribution()
  const
{
  return dynamic_cast<const DistribHypergeometricDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribDiscreteUnivariateDistribution" is of type
 * DistribNegativeBinomialDistribution
 */
bool
DistribDiscreteUnivariateDistribution::isDistribNegativeBinomialDistribution()
  const
{
  return dynamic_cast<const DistribNegativeBinomialDistribution*>(this) !=
    NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribDiscreteUnivariateDistribution" is of type
 * DistribPoissonDistribution
 */
bool
DistribDiscreteUnivariateDistribution::isDistribPoissonDistribution() const
{
  return dynamic_cast<const DistribPoissonDistribution*>(this) != NULL;
}


/*
 * Returns the XML element name of this DistribDiscreteUnivariateDistribution
 * object.
 */
const std::string&
DistribDiscreteUnivariateDistribution::getElementName() const
{
  static const string name = "discreteUnivariateDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribDiscreteUnivariateDistribution
 * object.
 */
int
DistribDiscreteUnivariateDistribution::getTypeCode() const
{
  return SBML_DISTRIB_DISCRETEUNIVARIATEDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribDiscreteUnivariateDistribution object have been set.
 */
bool
DistribDiscreteUnivariateDistribution::hasRequiredAttributes() const
{
  bool allPresent = DistribUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribDiscreteUnivariateDistribution object have been set.
 */
bool
DistribDiscreteUnivariateDistribution::hasRequiredElements() const
{
  bool allPresent = DistribUnivariateDistribution::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribDiscreteUnivariateDistribution::writeElements(XMLOutputStream& stream)
  const
{
  DistribUnivariateDistribution::writeElements(stream);

  if (isSetTruncationLowerBound() == true)
  {
    mTruncationLowerBound->write(stream);
  }

  if (isSetTruncationUpperBound() == true)
  {
    mTruncationUpperBound->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribDiscreteUnivariateDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mTruncationLowerBound != NULL)
  {
    mTruncationLowerBound->accept(v);
  }

  if (mTruncationUpperBound != NULL)
  {
    mTruncationUpperBound->accept(v);
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
DistribDiscreteUnivariateDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribUnivariateDistribution::setSBMLDocument(d);

  if (mTruncationLowerBound != NULL)
  {
    mTruncationLowerBound->setSBMLDocument(d);
  }

  if (mTruncationUpperBound != NULL)
  {
    mTruncationUpperBound->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribDiscreteUnivariateDistribution::connectToChild()
{
  DistribUnivariateDistribution::connectToChild();

  if (mTruncationLowerBound != NULL)
  {
    mTruncationLowerBound->connectToParent(this);
  }

  if (mTruncationUpperBound != NULL)
  {
    mTruncationUpperBound->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribDiscreteUnivariateDistribution::enablePackageInternal(
                                                             const std::string&
                                                               pkgURI,
                                                             const std::string&
                                                               pkgPrefix,
                                                             bool flag)
{
  DistribUnivariateDistribution::enablePackageInternal(pkgURI, pkgPrefix,
    flag);

  if (isSetTruncationLowerBound())
  {
    mTruncationLowerBound->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetTruncationUpperBound())
  {
    mTruncationUpperBound->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribDiscreteUnivariateDistribution::updateSBMLNamespace(
                                                           const std::string&
                                                             package,
                                                           unsigned int level,
                                                           unsigned int
                                                             version)
{
  DistribUnivariateDistribution::updateSBMLNamespace(package, level, version);

  if (mTruncationLowerBound != NULL)
  {
    mTruncationLowerBound->updateSBMLNamespace(package, level, version);
  }

  if (mTruncationUpperBound != NULL)
  {
    mTruncationUpperBound->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::getAttribute(
                                                    const std::string&
                                                      attributeName,
                                                    bool& value) const
{
  int return_value = DistribUnivariateDistribution::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::getAttribute(
                                                    const std::string&
                                                      attributeName,
                                                    int& value) const
{
  int return_value = DistribUnivariateDistribution::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::getAttribute(
                                                    const std::string&
                                                      attributeName,
                                                    double& value) const
{
  int return_value = DistribUnivariateDistribution::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::getAttribute(
                                                    const std::string&
                                                      attributeName,
                                                    unsigned int& value) const
{
  int return_value = DistribUnivariateDistribution::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::getAttribute(
                                                    const std::string&
                                                      attributeName,
                                                    std::string& value) const
{
  int return_value = DistribUnivariateDistribution::getAttribute(attributeName,
    value);

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
 * Predicate returning @c true if this DistribDiscreteUnivariateDistribution's
 * attribute "attributeName" is set.
 */
bool
DistribDiscreteUnivariateDistribution::isSetAttribute(const std::string&
  attributeName) const
{
  bool value = DistribUnivariateDistribution::isSetAttribute(attributeName);

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
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::setAttribute(
                                                    const std::string&
                                                      attributeName,
                                                    bool value)
{
  int return_value = DistribUnivariateDistribution::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::setAttribute(
                                                    const std::string&
                                                      attributeName,
                                                    int value)
{
  int return_value = DistribUnivariateDistribution::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::setAttribute(
                                                    const std::string&
                                                      attributeName,
                                                    double value)
{
  int return_value = DistribUnivariateDistribution::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::setAttribute(
                                                    const std::string&
                                                      attributeName,
                                                    unsigned int value)
{
  int return_value = DistribUnivariateDistribution::setAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::setAttribute(
                                                    const std::string&
                                                      attributeName,
                                                    const std::string& value)
{
  int return_value = DistribUnivariateDistribution::setAttribute(attributeName,
    value);

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
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::unsetAttribute(const std::string&
  attributeName)
{
  int value = DistribUnivariateDistribution::unsetAttribute(attributeName);

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
 * DistribDiscreteUnivariateDistribution.
 */
SBase*
DistribDiscreteUnivariateDistribution::createChildObject(const std::string&
  elementName)
{
  DistribUnivariateDistribution* obj = NULL;

  if (elementName == "truncationLowerBound")
  {
    return createTruncationLowerBound();
  }
  else if (elementName == "truncationUpperBound")
  {
    return createTruncationUpperBound();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this
 * DistribDiscreteUnivariateDistribution.
 */
int
DistribDiscreteUnivariateDistribution::addChildObject(
                                                      const std::string&
                                                        elementName,
                                                      const SBase* element)
{
  if (elementName == "truncationLowerBound" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTBOUND)
  {
    return setTruncationLowerBound((const DistribUncertBound*)(element));
  }
  else if (elementName == "truncationUpperBound" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTBOUND)
  {
    return setTruncationUpperBound((const DistribUncertBound*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribDiscreteUnivariateDistribution.
 */
SBase*
DistribDiscreteUnivariateDistribution::removeChildObject(
                                                         const std::string&
                                                           elementName,
                                                         const std::string& id)
{
  if (elementName == "truncationLowerBound")
  {
    DistribUncertBound * obj = getTruncationLowerBound();
    if (unsetTruncationLowerBound() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "truncationUpperBound")
  {
    DistribUncertBound * obj = getTruncationUpperBound();
    if (unsetTruncationUpperBound() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this
 * DistribDiscreteUnivariateDistribution.
 */
unsigned int
DistribDiscreteUnivariateDistribution::getNumObjects(const std::string&
  elementName)
{
  unsigned int n = 0;

  if (elementName == "truncationLowerBound")
  {
    if (isSetTruncationLowerBound())
    {
      return 1;
    }
  }
  else if (elementName == "truncationUpperBound")
  {
    if (isSetTruncationUpperBound())
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
 * DistribDiscreteUnivariateDistribution.
 */
SBase*
DistribDiscreteUnivariateDistribution::getObject(
                                                 const std::string&
                                                   elementName,
                                                 unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "truncationLowerBound")
  {
    return getTruncationLowerBound();
  }
  else if (elementName == "truncationUpperBound")
  {
    return getTruncationUpperBound();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribDiscreteUnivariateDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mTruncationLowerBound != NULL)
  {
    if (mTruncationLowerBound->getId() == id)
    {
      return mTruncationLowerBound;
    }

    obj = mTruncationLowerBound->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mTruncationUpperBound != NULL)
  {
    if (mTruncationUpperBound->getId() == id)
    {
      return mTruncationUpperBound;
    }

    obj = mTruncationUpperBound->getElementBySId(id);
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
DistribDiscreteUnivariateDistribution::getElementByMetaId(const std::string&
  metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mTruncationLowerBound != NULL)
  {
    if (mTruncationLowerBound->getMetaId() == metaid)
    {
      return mTruncationLowerBound;
    }

    obj = mTruncationLowerBound->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mTruncationUpperBound != NULL)
  {
    if (mTruncationUpperBound->getMetaId() == metaid)
    {
      return mTruncationUpperBound;
    }

    obj = mTruncationUpperBound->getElementByMetaId(metaid);
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
DistribDiscreteUnivariateDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mTruncationLowerBound, filter);
  ADD_FILTERED_POINTER(ret, sublist, mTruncationUpperBound, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribDiscreteUnivariateDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "truncationLowerBound")
  {
    if (isSetTruncationLowerBound())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDiscreteUnivariateDistributionAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
    }

    delete mTruncationLowerBound;
    mTruncationLowerBound = new DistribUncertBound(distribns);
    mTruncationLowerBound->setElementName(name);
    obj = mTruncationLowerBound;
  }
  else if (name == "truncationUpperBound")
  {
    if (isSetTruncationUpperBound())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDiscreteUnivariateDistributionAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
    }

    delete mTruncationUpperBound;
    mTruncationUpperBound = new DistribUncertBound(distribns);
    mTruncationUpperBound->setElementName(name);
    obj = mTruncationUpperBound;
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
DistribDiscreteUnivariateDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribUnivariateDistribution::addExpectedAttributes(attributes);

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
DistribDiscreteUnivariateDistribution::readAttributes(
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

  DistribUnivariateDistribution::readAttributes(attributes,
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
          DistribDistribDiscreteUnivariateDistributionAllowedAttributes,
            pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribDiscreteUnivariateDistributionAllowedCoreAttributes,
            pkgVersion, level, version, details);
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
DistribDiscreteUnivariateDistribution::readL3V1V1Attributes(const
  XMLAttributes& attributes)
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
      logEmptyString(mId, level, version,
        "<DistribDiscreteUnivariateDistribution>");
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
      logEmptyString(mName, level, version,
        "<DistribDiscreteUnivariateDistribution>");
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DistribDiscreteUnivariateDistribution::readL3V2V1Attributes(const
  XMLAttributes& attributes)
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
      logEmptyString(mId, level, version,
        "<DistribDiscreteUnivariateDistribution>");
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
DistribDiscreteUnivariateDistribution::writeAttributes(XMLOutputStream& stream)
  const
{
  DistribUnivariateDistribution::writeAttributes(stream);

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
DistribDiscreteUnivariateDistribution::writeL3V1V1Attributes(XMLOutputStream&
  stream) const
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
DistribDiscreteUnivariateDistribution::writeL3V2V1Attributes(XMLOutputStream&
  stream) const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribDiscreteUnivariateDistribution_t using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribDiscreteUnivariateDistribution_t *
DistribDiscreteUnivariateDistribution_create(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion)
{
  return new DistribDiscreteUnivariateDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this
 * DistribDiscreteUnivariateDistribution_t object.
 */
LIBSBML_EXTERN
DistribDiscreteUnivariateDistribution_t*
DistribDiscreteUnivariateDistribution_clone(const
  DistribDiscreteUnivariateDistribution_t* ddud)
{
  if (ddud != NULL)
  {
    return
      static_cast<DistribDiscreteUnivariateDistribution_t*>(ddud->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribDiscreteUnivariateDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribDiscreteUnivariateDistribution_free(DistribDiscreteUnivariateDistribution_t*
  ddud)
{
  if (ddud != NULL)
  {
    delete ddud;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribDiscreteUnivariateDistribution_getId(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  if (ddud == NULL)
  {
    return NULL;
  }

  return ddud->getId().empty() ? NULL : safe_strdup(ddud->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribDiscreteUnivariateDistribution_getName(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  if (ddud == NULL)
  {
    return NULL;
  }

  return ddud->getName().empty() ? NULL : safe_strdup(ddud->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribDiscreteUnivariateDistribution_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isSetId(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  return (ddud != NULL) ? static_cast<int>(ddud->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribDiscreteUnivariateDistribution_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isSetName(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  return (ddud != NULL) ? static_cast<int>(ddud->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_setId(
                                            DistribDiscreteUnivariateDistribution_t
                                              * ddud,
                                            const char * id)
{
  return (ddud != NULL) ? ddud->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_setName(
                                              DistribDiscreteUnivariateDistribution_t
                                                * ddud,
                                              const char * name)
{
  return (ddud != NULL) ? ddud->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_unsetId(DistribDiscreteUnivariateDistribution_t
  * ddud)
{
  return (ddud != NULL) ? ddud->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_unsetName(DistribDiscreteUnivariateDistribution_t
  * ddud)
{
  return (ddud != NULL) ? ddud->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "truncationLowerBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertBound_t*
DistribDiscreteUnivariateDistribution_getTruncationLowerBound(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  if (ddud == NULL)
  {
    return NULL;
  }

  return (DistribUncertBound_t*)(ddud->getTruncationLowerBound());
}


/*
 * Returns the value of the "truncationUpperBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertBound_t*
DistribDiscreteUnivariateDistribution_getTruncationUpperBound(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  if (ddud == NULL)
  {
    return NULL;
  }

  return (DistribUncertBound_t*)(ddud->getTruncationUpperBound());
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribDiscreteUnivariateDistribution_t's "truncationLowerBound" element is
 * set.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isSetTruncationLowerBound(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  return (ddud != NULL) ? static_cast<int>(ddud->isSetTruncationLowerBound()) :
    0;
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribDiscreteUnivariateDistribution_t's "truncationUpperBound" element is
 * set.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isSetTruncationUpperBound(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  return (ddud != NULL) ? static_cast<int>(ddud->isSetTruncationUpperBound()) :
    0;
}


/*
 * Sets the value of the "truncationLowerBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_setTruncationLowerBound(
                                                              DistribDiscreteUnivariateDistribution_t
                                                                * ddud,
                                                              const
                                                                DistribUncertBound_t*
                                                                  truncationLowerBound)
{
  return (ddud != NULL) ? ddud->setTruncationLowerBound(truncationLowerBound) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "truncationUpperBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_setTruncationUpperBound(
                                                              DistribDiscreteUnivariateDistribution_t
                                                                * ddud,
                                                              const
                                                                DistribUncertBound_t*
                                                                  truncationUpperBound)
{
  return (ddud != NULL) ? ddud->setTruncationUpperBound(truncationUpperBound) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertBound_t object, adds it to this
 * DistribDiscreteUnivariateDistribution_t object and returns the
 * DistribUncertBound_t object created.
 */
LIBSBML_EXTERN
DistribUncertBound_t*
DistribDiscreteUnivariateDistribution_createTruncationLowerBound(DistribDiscreteUnivariateDistribution_t*
  ddud)
{
  if (ddud == NULL)
  {
    return NULL;
  }

  return (DistribUncertBound_t*)(ddud->createTruncationLowerBound());
}


/*
 * Creates a new DistribUncertBound_t object, adds it to this
 * DistribDiscreteUnivariateDistribution_t object and returns the
 * DistribUncertBound_t object created.
 */
LIBSBML_EXTERN
DistribUncertBound_t*
DistribDiscreteUnivariateDistribution_createTruncationUpperBound(DistribDiscreteUnivariateDistribution_t*
  ddud)
{
  if (ddud == NULL)
  {
    return NULL;
  }

  return (DistribUncertBound_t*)(ddud->createTruncationUpperBound());
}


/*
 * Unsets the value of the "truncationLowerBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_unsetTruncationLowerBound(DistribDiscreteUnivariateDistribution_t
  * ddud)
{
  return (ddud != NULL) ? ddud->unsetTruncationLowerBound() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "truncationUpperBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_unsetTruncationUpperBound(DistribDiscreteUnivariateDistribution_t
  * ddud)
{
  return (ddud != NULL) ? ddud->unsetTruncationUpperBound() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this DistribDiscreteUnivariateDistribution_t is
 * of type DistribBinomialDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isDistribBinomialDistribution(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  return (ddud != NULL) ?
    static_cast<int>(ddud->isDistribBinomialDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribDiscreteUnivariateDistribution_t is
 * of type DistribGeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isDistribGeometricDistribution(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  return (ddud != NULL) ?
    static_cast<int>(ddud->isDistribGeometricDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribDiscreteUnivariateDistribution_t is
 * of type DistribHypergeometricDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isDistribHypergeometricDistribution(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  return (ddud != NULL) ?
    static_cast<int>(ddud->isDistribHypergeometricDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribDiscreteUnivariateDistribution_t is
 * of type DistribNegativeBinomialDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isDistribNegativeBinomialDistribution(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  return (ddud != NULL) ?
    static_cast<int>(ddud->isDistribNegativeBinomialDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribDiscreteUnivariateDistribution_t is
 * of type DistribPoissonDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isDistribPoissonDistribution(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  return (ddud != NULL) ?
    static_cast<int>(ddud->isDistribPoissonDistribution()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribDiscreteUnivariateDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_hasRequiredAttributes(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  return (ddud != NULL) ? static_cast<int>(ddud->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribDiscreteUnivariateDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_hasRequiredElements(const
  DistribDiscreteUnivariateDistribution_t * ddud)
{
  return (ddud != NULL) ? static_cast<int>(ddud->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


