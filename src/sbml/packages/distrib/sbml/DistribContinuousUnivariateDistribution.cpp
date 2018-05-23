/**
 * @file DistribContinuousUnivariateDistribution.cpp
 * @brief Implementation of the DistribContinuousUnivariateDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribContinuousUnivariateDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>

#include <sbml/packages/distrib/sbml/DistribBetaDistribution.h>
#include <sbml/packages/distrib/sbml/DistribCauchyDistribution.h>
#include <sbml/packages/distrib/sbml/DistribChiSquareDistribution.h>
#include <sbml/packages/distrib/sbml/DistribExponentialDistribution.h>
#include <sbml/packages/distrib/sbml/DistribFDistribution.h>
#include <sbml/packages/distrib/sbml/DistribGammaDistribution.h>
#include <sbml/packages/distrib/sbml/DistribInverseGammaDistribution.h>
#include <sbml/packages/distrib/sbml/DistribLaPlaceDistribution.h>
#include <sbml/packages/distrib/sbml/DistribLogNormalDistribution.h>
#include <sbml/packages/distrib/sbml/DistribLogisticDistribution.h>
#include <sbml/packages/distrib/sbml/DistribNormalDistribution.h>
#include <sbml/packages/distrib/sbml/DistribParetoDistribution.h>
#include <sbml/packages/distrib/sbml/DistribRayleighDistribution.h>
#include <sbml/packages/distrib/sbml/DistribStudentTDistribution.h>
#include <sbml/packages/distrib/sbml/DistribUniformDistribution.h>
#include <sbml/packages/distrib/sbml/DistribWeibullDistribution.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribContinuousUnivariateDistribution using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
DistribContinuousUnivariateDistribution::DistribContinuousUnivariateDistribution(
                                                                                 unsigned int level,
                                                                                 unsigned int version,
                                                                                 unsigned int pkgVersion)
  : DistribUnivariateDistribution(level, version)
  , mTruncationLowerBound (NULL)
  , mTruncationUpperBound (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribContinuousUnivariateDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribContinuousUnivariateDistribution::DistribContinuousUnivariateDistribution(DistribPkgNamespaces
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
 * Copy constructor for DistribContinuousUnivariateDistribution.
 */
DistribContinuousUnivariateDistribution::DistribContinuousUnivariateDistribution(const
  DistribContinuousUnivariateDistribution& orig)
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
 * Assignment operator for DistribContinuousUnivariateDistribution.
 */
DistribContinuousUnivariateDistribution&
DistribContinuousUnivariateDistribution::operator=(const
  DistribContinuousUnivariateDistribution& rhs)
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
 * DistribContinuousUnivariateDistribution object.
 */
DistribContinuousUnivariateDistribution*
DistribContinuousUnivariateDistribution::clone() const
{
  return new DistribContinuousUnivariateDistribution(*this);
}


/*
 * Destructor for DistribContinuousUnivariateDistribution.
 */
DistribContinuousUnivariateDistribution::~DistribContinuousUnivariateDistribution()
{
  delete mTruncationLowerBound;
  mTruncationLowerBound = NULL;
  delete mTruncationUpperBound;
  mTruncationUpperBound = NULL;
}


/*
 * Returns the value of the "id" attribute of this
 * DistribContinuousUnivariateDistribution.
 */
const std::string&
DistribContinuousUnivariateDistribution::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * DistribContinuousUnivariateDistribution.
 */
const std::string&
DistribContinuousUnivariateDistribution::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this
 * DistribContinuousUnivariateDistribution's "id" attribute is set.
 */
bool
DistribContinuousUnivariateDistribution::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this
 * DistribContinuousUnivariateDistribution's "name" attribute is set.
 */
bool
DistribContinuousUnivariateDistribution::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::unsetId()
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::unsetName()
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
 * DistribContinuousUnivariateDistribution.
 */
const DistribUncertBound*
DistribContinuousUnivariateDistribution::getTruncationLowerBound() const
{
  return mTruncationLowerBound;
}


/*
 * Returns the value of the "truncationLowerBound" element of this
 * DistribContinuousUnivariateDistribution.
 */
DistribUncertBound*
DistribContinuousUnivariateDistribution::getTruncationLowerBound()
{
  return mTruncationLowerBound;
}


/*
 * Returns the value of the "truncationUpperBound" element of this
 * DistribContinuousUnivariateDistribution.
 */
const DistribUncertBound*
DistribContinuousUnivariateDistribution::getTruncationUpperBound() const
{
  return mTruncationUpperBound;
}


/*
 * Returns the value of the "truncationUpperBound" element of this
 * DistribContinuousUnivariateDistribution.
 */
DistribUncertBound*
DistribContinuousUnivariateDistribution::getTruncationUpperBound()
{
  return mTruncationUpperBound;
}


/*
 * Predicate returning @c true if this
 * DistribContinuousUnivariateDistribution's "truncationLowerBound" element is
 * set.
 */
bool
DistribContinuousUnivariateDistribution::isSetTruncationLowerBound() const
{
  return (mTruncationLowerBound != NULL);
}


/*
 * Predicate returning @c true if this
 * DistribContinuousUnivariateDistribution's "truncationUpperBound" element is
 * set.
 */
bool
DistribContinuousUnivariateDistribution::isSetTruncationUpperBound() const
{
  return (mTruncationUpperBound != NULL);
}


/*
 * Sets the value of the "truncationLowerBound" element of this
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::setTruncationLowerBound(const
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::setTruncationUpperBound(const
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
 * DistribContinuousUnivariateDistribution object and returns the
 * DistribUncertBound object created.
 */
DistribUncertBound*
DistribContinuousUnivariateDistribution::createTruncationLowerBound()
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
 * DistribContinuousUnivariateDistribution object and returns the
 * DistribUncertBound object created.
 */
DistribUncertBound*
DistribContinuousUnivariateDistribution::createTruncationUpperBound()
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::unsetTruncationLowerBound()
{
  delete mTruncationLowerBound;
  mTruncationLowerBound = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "truncationUpperBound" element of this
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::unsetTruncationUpperBound()
{
  delete mTruncationUpperBound;
  mTruncationUpperBound = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type DistribBetaDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribBetaDistribution() const
{
  return dynamic_cast<const DistribBetaDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribCauchyDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribCauchyDistribution() const
{
  return dynamic_cast<const DistribCauchyDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribChiSquareDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribChiSquareDistribution() const
{
  return dynamic_cast<const DistribChiSquareDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribExponentialDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribExponentialDistribution()
  const
{
  return dynamic_cast<const DistribExponentialDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type DistribFDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribFDistribution() const
{
  return dynamic_cast<const DistribFDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribGammaDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribGammaDistribution() const
{
  return dynamic_cast<const DistribGammaDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribInverseGammaDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribInverseGammaDistribution()
  const
{
  return dynamic_cast<const DistribInverseGammaDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribLaPlaceDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribLaPlaceDistribution() const
{
  return dynamic_cast<const DistribLaPlaceDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribLogNormalDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribLogNormalDistribution() const
{
  return dynamic_cast<const DistribLogNormalDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribLogisticDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribLogisticDistribution() const
{
  return dynamic_cast<const DistribLogisticDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribNormalDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribNormalDistribution() const
{
  return dynamic_cast<const DistribNormalDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribParetoDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribParetoDistribution() const
{
  return dynamic_cast<const DistribParetoDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribRayleighDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribRayleighDistribution() const
{
  return dynamic_cast<const DistribRayleighDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribStudentTDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribStudentTDistribution() const
{
  return dynamic_cast<const DistribStudentTDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribUniformDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribUniformDistribution() const
{
  return dynamic_cast<const DistribUniformDistribution*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract
 * "DistribContinuousUnivariateDistribution" is of type
 * DistribWeibullDistribution
 */
bool
DistribContinuousUnivariateDistribution::isDistribWeibullDistribution() const
{
  return dynamic_cast<const DistribWeibullDistribution*>(this) != NULL;
}


/*
 * Returns the XML element name of this DistribContinuousUnivariateDistribution
 * object.
 */
const std::string&
DistribContinuousUnivariateDistribution::getElementName() const
{
  static const string name = "continuousUnivariateDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this
 * DistribContinuousUnivariateDistribution object.
 */
int
DistribContinuousUnivariateDistribution::getTypeCode() const
{
  return SBML_DISTRIB_CONTINUOUSUNIVARIATEDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribContinuousUnivariateDistribution object have been set.
 */
bool
DistribContinuousUnivariateDistribution::hasRequiredAttributes() const
{
  bool allPresent = DistribUnivariateDistribution::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribContinuousUnivariateDistribution object have been set.
 */
bool
DistribContinuousUnivariateDistribution::hasRequiredElements() const
{
  bool allPresent = DistribUnivariateDistribution::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribContinuousUnivariateDistribution::writeElements(XMLOutputStream& stream)
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
DistribContinuousUnivariateDistribution::accept(SBMLVisitor& v) const
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
DistribContinuousUnivariateDistribution::setSBMLDocument(SBMLDocument* d)
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
DistribContinuousUnivariateDistribution::connectToChild()
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
DistribContinuousUnivariateDistribution::enablePackageInternal(
                                                               const
                                                                 std::string&
                                                                   pkgURI,
                                                               const
                                                                 std::string&
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
DistribContinuousUnivariateDistribution::updateSBMLNamespace(
                                                             const std::string&
                                                               package,
                                                             unsigned int
                                                               level,
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::getAttribute(
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::getAttribute(
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::getAttribute(
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::getAttribute(
                                                      const std::string&
                                                        attributeName,
                                                      unsigned int& value)
                                                        const
{
  int return_value = DistribUnivariateDistribution::getAttribute(attributeName,
    value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::getAttribute(
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
 * Predicate returning @c true if this
 * DistribContinuousUnivariateDistribution's attribute "attributeName" is set.
 */
bool
DistribContinuousUnivariateDistribution::isSetAttribute(const std::string&
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::setAttribute(
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::setAttribute(
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::setAttribute(
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::setAttribute(
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::setAttribute(
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::unsetAttribute(const std::string&
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
 * DistribContinuousUnivariateDistribution.
 */
SBase*
DistribContinuousUnivariateDistribution::createChildObject(const std::string&
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
 * DistribContinuousUnivariateDistribution.
 */
int
DistribContinuousUnivariateDistribution::addChildObject(
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
 * DistribContinuousUnivariateDistribution.
 */
SBase*
DistribContinuousUnivariateDistribution::removeChildObject(
                                                           const std::string&
                                                             elementName,
                                                           const std::string&
                                                             id)
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
 * DistribContinuousUnivariateDistribution.
 */
unsigned int
DistribContinuousUnivariateDistribution::getNumObjects(const std::string&
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
 * DistribContinuousUnivariateDistribution.
 */
SBase*
DistribContinuousUnivariateDistribution::getObject(
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
DistribContinuousUnivariateDistribution::getElementBySId(const std::string& id)
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
DistribContinuousUnivariateDistribution::getElementByMetaId(const std::string&
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
DistribContinuousUnivariateDistribution::getAllElements(ElementFilter* filter)
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
DistribContinuousUnivariateDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribUnivariateDistribution::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "truncationLowerBound")
  {
    if (isSetTruncationLowerBound())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribContinuousUnivariateDistributionAllowedElements,
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
        DistribDistribContinuousUnivariateDistributionAllowedElements,
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
DistribContinuousUnivariateDistribution::addExpectedAttributes(ExpectedAttributes&
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
DistribContinuousUnivariateDistribution::readAttributes(
                                                        const XMLAttributes&
                                                          attributes,
                                                        const
                                                          ExpectedAttributes&
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
          DistribDistribContinuousUnivariateDistributionAllowedAttributes,
            pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribContinuousUnivariateDistributionAllowedCoreAttributes,
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
DistribContinuousUnivariateDistribution::writeAttributes(XMLOutputStream&
  stream) const
{
  DistribUnivariateDistribution::writeAttributes(stream);
  SBase::writeExtensionAttributes(stream);
}

/** @endcond */



#endif /* __cplusplus */


/*
 * Creates a new DistribContinuousUnivariateDistribution_t using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribContinuousUnivariateDistribution_t *
DistribContinuousUnivariateDistribution_create(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion)
{
  return new DistribContinuousUnivariateDistribution(level, version,
    pkgVersion);
}


/*
 * Creates and returns a deep copy of this
 * DistribContinuousUnivariateDistribution_t object.
 */
LIBSBML_EXTERN
DistribContinuousUnivariateDistribution_t*
DistribContinuousUnivariateDistribution_clone(const
  DistribContinuousUnivariateDistribution_t* dcud)
{
  if (dcud != NULL)
  {
    return
      static_cast<DistribContinuousUnivariateDistribution_t*>(dcud->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribContinuousUnivariateDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribContinuousUnivariateDistribution_free(DistribContinuousUnivariateDistribution_t*
  dcud)
{
  if (dcud != NULL)
  {
    delete dcud;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribContinuousUnivariateDistribution_getId(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  if (dcud == NULL)
  {
    return NULL;
  }

  return dcud->getId().empty() ? NULL : safe_strdup(dcud->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribContinuousUnivariateDistribution_getName(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  if (dcud == NULL)
  {
    return NULL;
  }

  return dcud->getName().empty() ? NULL : safe_strdup(dcud->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribContinuousUnivariateDistribution_t's "id" attribute is set.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isSetId(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribContinuousUnivariateDistribution_t's "name" attribute is set.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isSetName(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_setId(
                                              DistribContinuousUnivariateDistribution_t
                                                * dcud,
                                              const char * id)
{
  return (dcud != NULL) ? dcud->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_setName(
                                                DistribContinuousUnivariateDistribution_t
                                                  * dcud,
                                                const char * name)
{
  return (dcud != NULL) ? dcud->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_unsetId(DistribContinuousUnivariateDistribution_t
  * dcud)
{
  return (dcud != NULL) ? dcud->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_unsetName(DistribContinuousUnivariateDistribution_t
  * dcud)
{
  return (dcud != NULL) ? dcud->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "truncationLowerBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertBound_t*
DistribContinuousUnivariateDistribution_getTruncationLowerBound(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  if (dcud == NULL)
  {
    return NULL;
  }

  return (DistribUncertBound_t*)(dcud->getTruncationLowerBound());
}


/*
 * Returns the value of the "truncationUpperBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
const DistribUncertBound_t*
DistribContinuousUnivariateDistribution_getTruncationUpperBound(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  if (dcud == NULL)
  {
    return NULL;
  }

  return (DistribUncertBound_t*)(dcud->getTruncationUpperBound());
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribContinuousUnivariateDistribution_t's "truncationLowerBound" element
 * is set.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isSetTruncationLowerBound(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->isSetTruncationLowerBound()) :
    0;
}


/*
 * Predicate returning @c 1 (true) if this
 * DistribContinuousUnivariateDistribution_t's "truncationUpperBound" element
 * is set.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isSetTruncationUpperBound(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->isSetTruncationUpperBound()) :
    0;
}


/*
 * Sets the value of the "truncationLowerBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_setTruncationLowerBound(
                                                                DistribContinuousUnivariateDistribution_t
                                                                  * dcud,
                                                                const
                                                                  DistribUncertBound_t*
                                                                    truncationLowerBound)
{
  return (dcud != NULL) ? dcud->setTruncationLowerBound(truncationLowerBound) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "truncationUpperBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_setTruncationUpperBound(
                                                                DistribContinuousUnivariateDistribution_t
                                                                  * dcud,
                                                                const
                                                                  DistribUncertBound_t*
                                                                    truncationUpperBound)
{
  return (dcud != NULL) ? dcud->setTruncationUpperBound(truncationUpperBound) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertBound_t object, adds it to this
 * DistribContinuousUnivariateDistribution_t object and returns the
 * DistribUncertBound_t object created.
 */
LIBSBML_EXTERN
DistribUncertBound_t*
DistribContinuousUnivariateDistribution_createTruncationLowerBound(DistribContinuousUnivariateDistribution_t*
  dcud)
{
  if (dcud == NULL)
  {
    return NULL;
  }

  return (DistribUncertBound_t*)(dcud->createTruncationLowerBound());
}


/*
 * Creates a new DistribUncertBound_t object, adds it to this
 * DistribContinuousUnivariateDistribution_t object and returns the
 * DistribUncertBound_t object created.
 */
LIBSBML_EXTERN
DistribUncertBound_t*
DistribContinuousUnivariateDistribution_createTruncationUpperBound(DistribContinuousUnivariateDistribution_t*
  dcud)
{
  if (dcud == NULL)
  {
    return NULL;
  }

  return (DistribUncertBound_t*)(dcud->createTruncationUpperBound());
}


/*
 * Unsets the value of the "truncationLowerBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_unsetTruncationLowerBound(DistribContinuousUnivariateDistribution_t
  * dcud)
{
  return (dcud != NULL) ? dcud->unsetTruncationLowerBound() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "truncationUpperBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_unsetTruncationUpperBound(DistribContinuousUnivariateDistribution_t
  * dcud)
{
  return (dcud != NULL) ? dcud->unsetTruncationUpperBound() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribBetaDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribBetaDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->isDistribBetaDistribution()) :
    0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribCauchyDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribCauchyDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->isDistribCauchyDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribChiSquareDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribChiSquareDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribChiSquareDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribExponentialDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribExponentialDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribExponentialDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribFDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribFDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->isDistribFDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribGammaDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribGammaDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->isDistribGammaDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribInverseGammaDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribInverseGammaDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribInverseGammaDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribLaPlaceDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribLaPlaceDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribLaPlaceDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribLogNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribLogNormalDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribLogNormalDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribLogisticDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribLogisticDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribLogisticDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribNormalDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribNormalDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->isDistribNormalDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribParetoDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribParetoDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->isDistribParetoDistribution())
    : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribRayleighDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribRayleighDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribRayleighDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribStudentTDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribStudentTDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribUniformDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribUniformDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribUniformDistribution()) : 0;
}


/*
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribWeibullDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribWeibullDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ?
    static_cast<int>(dcud->isDistribWeibullDistribution()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribContinuousUnivariateDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_hasRequiredAttributes(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribContinuousUnivariateDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_hasRequiredElements(const
  DistribContinuousUnivariateDistribution_t * dcud)
{
  return (dcud != NULL) ? static_cast<int>(dcud->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


