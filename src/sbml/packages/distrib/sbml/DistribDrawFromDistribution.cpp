/**
 * @file DistribDrawFromDistribution.cpp
 * @brief Implementation of the DistribDrawFromDistribution class.
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
#include <sbml/packages/distrib/sbml/DistribDrawFromDistribution.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>

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
#include <sbml/packages/distrib/sbml/DistribBinomialDistribution.h>
#include <sbml/packages/distrib/sbml/DistribGeometricDistribution.h>
#include <sbml/packages/distrib/sbml/DistribHypergeometricDistribution.h>
#include <sbml/packages/distrib/sbml/DistribNegativeBinomialDistribution.h>
#include <sbml/packages/distrib/sbml/DistribPoissonDistribution.h>
#include <sbml/packages/distrib/sbml/DistribBernoulliDistribution.h>
#include <sbml/packages/distrib/sbml/DistribCategoricalDistribution.h>
#include <sbml/packages/distrib/sbml/DistribMultivariateDistribution.h>
#include <sbml/packages/distrib/sbml/DistribExternalDistribution.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribDrawFromDistribution using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
DistribDrawFromDistribution::DistribDrawFromDistribution(unsigned int level,
                                                         unsigned int version,
                                                         unsigned int
                                                           pkgVersion)
  : DistribBase(level, version, pkgVersion)
  , mDistribInputs (level, version, pkgVersion)
  , mDistribution (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribDrawFromDistribution using the given
 * DistribPkgNamespaces object.
 */
DistribDrawFromDistribution::DistribDrawFromDistribution(DistribPkgNamespaces
  *distribns)
  : DistribBase(distribns)
  , mDistribInputs (distribns)
  , mDistribution (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribDrawFromDistribution.
 */
DistribDrawFromDistribution::DistribDrawFromDistribution(const
  DistribDrawFromDistribution& orig)
  : DistribBase( orig )
  , mDistribInputs ( orig.mDistribInputs )
  , mDistribution ( NULL )
{
  if (orig.mDistribution != NULL)
  {
    mDistribution = orig.mDistribution->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribDrawFromDistribution.
 */
DistribDrawFromDistribution&
DistribDrawFromDistribution::operator=(const DistribDrawFromDistribution& rhs)
{
  if (&rhs != this)
  {
    DistribBase::operator=(rhs);
    mDistribInputs = rhs.mDistribInputs;
    delete mDistribution;
    if (rhs.mDistribution != NULL)
    {
      mDistribution = rhs.mDistribution->clone();
    }
    else
    {
      mDistribution = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribDrawFromDistribution object.
 */
DistribDrawFromDistribution*
DistribDrawFromDistribution::clone() const
{
  return new DistribDrawFromDistribution(*this);
}


/*
 * Destructor for DistribDrawFromDistribution.
 */
DistribDrawFromDistribution::~DistribDrawFromDistribution()
{
  delete mDistribution;
  mDistribution = NULL;
}


/*
 * Returns the value of the "id" attribute of this DistribDrawFromDistribution.
 */
const std::string&
DistribDrawFromDistribution::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this
 * DistribDrawFromDistribution.
 */
const std::string&
DistribDrawFromDistribution::getName() const
{
  return mName;
}


/*
 * Predicate returning @c true if this DistribDrawFromDistribution's "id"
 * attribute is set.
 */
bool
DistribDrawFromDistribution::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Predicate returning @c true if this DistribDrawFromDistribution's "name"
 * attribute is set.
 */
bool
DistribDrawFromDistribution::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Sets the value of the "id" attribute of this DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets the value of the "name" attribute of this DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "id" attribute of this DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::unsetId()
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
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::unsetName()
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
 * Returns the value of the "distribution" element of this
 * DistribDrawFromDistribution.
 */
const DistribDistribution*
DistribDrawFromDistribution::getDistribution() const
{
  return mDistribution;
}


/*
 * Returns the value of the "distribution" element of this
 * DistribDrawFromDistribution.
 */
DistribDistribution*
DistribDrawFromDistribution::getDistribution()
{
  return mDistribution;
}


/*
 * Predicate returning @c true if this DistribDrawFromDistribution's
 * "distribution" element is set.
 */
bool
DistribDrawFromDistribution::isSetDistribution() const
{
  return (mDistribution != NULL);
}


/*
 * Sets the value of the "distribution" element of this
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::setDistribution(const DistribDistribution*
  distribution)
{
  if (distribution == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (distribution->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != distribution->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != distribution->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != distribution->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mDistribution;
    mDistribution = (distribution != NULL) ?
      static_cast<DistribDistribution*>(distribution->clone()) : NULL;
    if (mDistribution != NULL) mDistribution->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribBetaDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the DistribBetaDistribution
 * object created.
 */
DistribBetaDistribution*
DistribDrawFromDistribution::createDistribBetaDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribBetaDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribBetaDistribution*>(mDistribution);
}


/*
 * Creates a new DistribCauchyDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the DistribCauchyDistribution
 * object created.
 */
DistribCauchyDistribution*
DistribDrawFromDistribution::createDistribCauchyDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribCauchyDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribCauchyDistribution*>(mDistribution);
}


/*
 * Creates a new DistribChiSquareDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribChiSquareDistribution object created.
 */
DistribChiSquareDistribution*
DistribDrawFromDistribution::createDistribChiSquareDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribChiSquareDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribChiSquareDistribution*>(mDistribution);
}


/*
 * Creates a new DistribExponentialDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribExponentialDistribution object created.
 */
DistribExponentialDistribution*
DistribDrawFromDistribution::createDistribExponentialDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribExponentialDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribExponentialDistribution*>(mDistribution);
}


/*
 * Creates a new DistribFDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the DistribFDistribution
 * object created.
 */
DistribFDistribution*
DistribDrawFromDistribution::createDistribFDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribFDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribFDistribution*>(mDistribution);
}


/*
 * Creates a new DistribGammaDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the DistribGammaDistribution
 * object created.
 */
DistribGammaDistribution*
DistribDrawFromDistribution::createDistribGammaDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribGammaDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribGammaDistribution*>(mDistribution);
}


/*
 * Creates a new DistribInverseGammaDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribInverseGammaDistribution object created.
 */
DistribInverseGammaDistribution*
DistribDrawFromDistribution::createDistribInverseGammaDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribInverseGammaDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribInverseGammaDistribution*>(mDistribution);
}


/*
 * Creates a new DistribLaPlaceDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribLaPlaceDistribution object created.
 */
DistribLaPlaceDistribution*
DistribDrawFromDistribution::createDistribLaPlaceDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribLaPlaceDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribLaPlaceDistribution*>(mDistribution);
}


/*
 * Creates a new DistribLogNormalDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribLogNormalDistribution object created.
 */
DistribLogNormalDistribution*
DistribDrawFromDistribution::createDistribLogNormalDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribLogNormalDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribLogNormalDistribution*>(mDistribution);
}


/*
 * Creates a new DistribLogisticDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribLogisticDistribution object created.
 */
DistribLogisticDistribution*
DistribDrawFromDistribution::createDistribLogisticDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribLogisticDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribLogisticDistribution*>(mDistribution);
}


/*
 * Creates a new DistribNormalDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the DistribNormalDistribution
 * object created.
 */
DistribNormalDistribution*
DistribDrawFromDistribution::createDistribNormalDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribNormalDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribNormalDistribution*>(mDistribution);
}


/*
 * Creates a new DistribParetoDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the DistribParetoDistribution
 * object created.
 */
DistribParetoDistribution*
DistribDrawFromDistribution::createDistribParetoDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribParetoDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribParetoDistribution*>(mDistribution);
}


/*
 * Creates a new DistribRayleighDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribRayleighDistribution object created.
 */
DistribRayleighDistribution*
DistribDrawFromDistribution::createDistribRayleighDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribRayleighDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribRayleighDistribution*>(mDistribution);
}


/*
 * Creates a new DistribStudentTDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribStudentTDistribution object created.
 */
DistribStudentTDistribution*
DistribDrawFromDistribution::createDistribStudentTDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribStudentTDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribStudentTDistribution*>(mDistribution);
}


/*
 * Creates a new DistribUniformDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribUniformDistribution object created.
 */
DistribUniformDistribution*
DistribDrawFromDistribution::createDistribUniformDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribUniformDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribUniformDistribution*>(mDistribution);
}


/*
 * Creates a new DistribWeibullDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribWeibullDistribution object created.
 */
DistribWeibullDistribution*
DistribDrawFromDistribution::createDistribWeibullDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribWeibullDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribWeibullDistribution*>(mDistribution);
}


/*
 * Creates a new DistribBinomialDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribBinomialDistribution object created.
 */
DistribBinomialDistribution*
DistribDrawFromDistribution::createDistribBinomialDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribBinomialDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribBinomialDistribution*>(mDistribution);
}


/*
 * Creates a new DistribGeometricDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribGeometricDistribution object created.
 */
DistribGeometricDistribution*
DistribDrawFromDistribution::createDistribGeometricDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribGeometricDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribGeometricDistribution*>(mDistribution);
}


/*
 * Creates a new DistribHypergeometricDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribHypergeometricDistribution object created.
 */
DistribHypergeometricDistribution*
DistribDrawFromDistribution::createDistribHypergeometricDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribHypergeometricDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribHypergeometricDistribution*>(mDistribution);
}


/*
 * Creates a new DistribNegativeBinomialDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribNegativeBinomialDistribution object created.
 */
DistribNegativeBinomialDistribution*
DistribDrawFromDistribution::createDistribNegativeBinomialDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribNegativeBinomialDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribNegativeBinomialDistribution*>(mDistribution);
}


/*
 * Creates a new DistribPoissonDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribPoissonDistribution object created.
 */
DistribPoissonDistribution*
DistribDrawFromDistribution::createDistribPoissonDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribPoissonDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribPoissonDistribution*>(mDistribution);
}


/*
 * Creates a new DistribBernoulliDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribBernoulliDistribution object created.
 */
DistribBernoulliDistribution*
DistribDrawFromDistribution::createDistribBernoulliDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribBernoulliDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribBernoulliDistribution*>(mDistribution);
}


/*
 * Creates a new DistribCategoricalDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribCategoricalDistribution object created.
 */
DistribCategoricalDistribution*
DistribDrawFromDistribution::createDistribCategoricalDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribCategoricalDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribCategoricalDistribution*>(mDistribution);
}


/*
 * Creates a new DistribMultivariateDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribMultivariateDistribution object created.
 */
DistribMultivariateDistribution*
DistribDrawFromDistribution::createDistribMultivariateDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribMultivariateDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribMultivariateDistribution*>(mDistribution);
}


/*
 * Creates a new DistribExternalDistribution object, adds it to this
 * DistribDrawFromDistribution object and returns the
 * DistribExternalDistribution object created.
 */
DistribExternalDistribution*
DistribDrawFromDistribution::createDistribExternalDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new DistribExternalDistribution(distribns);

  delete distribns;

  connectToChild();

  return static_cast<DistribExternalDistribution*>(mDistribution);
}


/*
 * Unsets the value of the "distribution" element of this
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::unsetDistribution()
{
  delete mDistribution;
  mDistribution = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the ListOfDistribInputs from this DistribDrawFromDistribution.
 */
const ListOfDistribInputs*
DistribDrawFromDistribution::getListOfDistribInputs() const
{
  return &mDistribInputs;
}


/*
 * Returns the ListOfDistribInputs from this DistribDrawFromDistribution.
 */
ListOfDistribInputs*
DistribDrawFromDistribution::getListOfDistribInputs()
{
  return &mDistribInputs;
}


/*
 * Get a DistribInput from the DistribDrawFromDistribution.
 */
DistribInput*
DistribDrawFromDistribution::getDistribInput(unsigned int n)
{
  return mDistribInputs.get(n);
}


/*
 * Get a DistribInput from the DistribDrawFromDistribution.
 */
const DistribInput*
DistribDrawFromDistribution::getDistribInput(unsigned int n) const
{
  return mDistribInputs.get(n);
}


/*
 * Get a DistribInput from the DistribDrawFromDistribution based on its
 * identifier.
 */
DistribInput*
DistribDrawFromDistribution::getDistribInput(const std::string& sid)
{
  return mDistribInputs.get(sid);
}


/*
 * Get a DistribInput from the DistribDrawFromDistribution based on its
 * identifier.
 */
const DistribInput*
DistribDrawFromDistribution::getDistribInput(const std::string& sid) const
{
  return mDistribInputs.get(sid);
}


/*
 * Get a DistribInput from the DistribDrawFromDistribution.
 */
const DistribInput*
DistribDrawFromDistribution::getDistribInputByIndex(unsigned int n) const
{
  return mDistribInputs.getByIndex(n);
}


/*
 * Get a DistribInput from the DistribDrawFromDistribution.
 */
DistribInput*
DistribDrawFromDistribution::getDistribInputByIndex(unsigned int n)
{
  return mDistribInputs.getByIndex(n);
}


/*
 * Adds a copy of the given DistribInput to this DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::addDistribInput(const DistribInput* di)
{
  if (di == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (di->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != di->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != di->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(di)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else if (di->isSetId() && (mDistribInputs.get(di->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mDistribInputs.append(di);
  }
}


/*
 * Get the number of DistribInput objects in this DistribDrawFromDistribution.
 */
unsigned int
DistribDrawFromDistribution::getNumDistribInputs() const
{
  return mDistribInputs.size();
}


/*
 * Creates a new DistribInput object, adds it to this
 * DistribDrawFromDistribution object and returns the DistribInput object
 * created.
 */
DistribInput*
DistribDrawFromDistribution::createDistribInput()
{
  DistribInput* di = NULL;

  try
  {
    DISTRIB_CREATE_NS_WITH_VERSION(distribns, getSBMLNamespaces(),
      getPackageVersion());
    di = new DistribInput(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (di != NULL)
  {
    mDistribInputs.appendAndOwn(di);
  }

  return di;
}


/*
 * Removes the nth DistribInput from this DistribDrawFromDistribution and
 * returns a pointer to it.
 */
DistribInput*
DistribDrawFromDistribution::removeDistribInput(unsigned int n)
{
  return mDistribInputs.remove(n);
}


/*
 * Removes the DistribInput from this DistribDrawFromDistribution based on its
 * identifier and returns a pointer to it.
 */
DistribInput*
DistribDrawFromDistribution::removeDistribInput(const std::string& sid)
{
  return mDistribInputs.remove(sid);
}


/*
 * Returns the XML element name of this DistribDrawFromDistribution object.
 */
const std::string&
DistribDrawFromDistribution::getElementName() const
{
  static const string name = "drawFromDistribution";
  return name;
}


/*
 * Returns the libSBML type code for this DistribDrawFromDistribution object.
 */
int
DistribDrawFromDistribution::getTypeCode() const
{
  return SBML_DISTRIB_DRAWFROMDISTRIBUTION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribDrawFromDistribution object have been set.
 */
bool
DistribDrawFromDistribution::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribDrawFromDistribution::writeElements(XMLOutputStream& stream) const
{
  DistribBase::writeElements(stream);

  if (isSetDistribution() == true)
  {
    mDistribution->write(stream);
  }

  if (getNumDistribInputs() > 0)
  {
    mDistribInputs.write(stream);
  }

  DistribBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribDrawFromDistribution::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mDistribution != NULL)
  {
    mDistribution->accept(v);
  }

  mDistribInputs.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribDrawFromDistribution::setSBMLDocument(SBMLDocument* d)
{
  DistribBase::setSBMLDocument(d);

  if (mDistribution != NULL)
  {
    mDistribution->setSBMLDocument(d);
  }

  mDistribInputs.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribDrawFromDistribution::connectToChild()
{
  DistribBase::connectToChild();

  if (mDistribution != NULL)
  {
    mDistribution->connectToParent(this);
  }

  mDistribInputs.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribDrawFromDistribution::enablePackageInternal(const std::string& pkgURI,
                                                   const std::string&
                                                     pkgPrefix,
                                                   bool flag)
{
  DistribBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  if (isSetDistribution())
  {
    mDistribution->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  mDistribInputs.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribDrawFromDistribution::updateSBMLNamespace(const std::string& package,
                                                 unsigned int level,
                                                 unsigned int version)
{
  DistribBase::updateSBMLNamespace(package, level, version);

  if (mDistribution != NULL)
  {
    mDistribution->updateSBMLNamespace(package, level, version);
  }

  mDistribInputs.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::getAttribute(const std::string& attributeName,
                                          bool& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::getAttribute(const std::string& attributeName,
                                          int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::getAttribute(const std::string& attributeName,
                                          double& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::getAttribute(const std::string& attributeName,
                                          unsigned int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::getAttribute(const std::string& attributeName,
                                          std::string& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

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
 * Predicate returning @c true if this DistribDrawFromDistribution's attribute
 * "attributeName" is set.
 */
bool
DistribDrawFromDistribution::isSetAttribute(const std::string& attributeName)
  const
{
  bool value = DistribBase::isSetAttribute(attributeName);

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
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::setAttribute(const std::string& attributeName,
                                          bool value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::setAttribute(const std::string& attributeName,
                                          int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::setAttribute(const std::string& attributeName,
                                          double value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::setAttribute(const std::string& attributeName,
                                          unsigned int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::setAttribute(const std::string& attributeName,
                                          const std::string& value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

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
 * DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::unsetAttribute(const std::string& attributeName)
{
  int value = DistribBase::unsetAttribute(attributeName);

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
 * DistribDrawFromDistribution.
 */
SBase*
DistribDrawFromDistribution::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  // TO DO

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribDrawFromDistribution.
 */
int
DistribDrawFromDistribution::addChildObject(const std::string& elementName,
                                            const SBase* element)
{
  // TO DO

  return -1;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribDrawFromDistribution.
 */
SBase*
DistribDrawFromDistribution::removeChildObject(const std::string& elementName,
                                               const std::string& id)
{
  // TO DO

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribDrawFromDistribution.
 */
unsigned int
DistribDrawFromDistribution::getNumObjects(const std::string& elementName)
{
  // TO DO

  return 0;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribDrawFromDistribution.
 */
SBase*
DistribDrawFromDistribution::getObject(const std::string& elementName,
                                       unsigned int index)
{
  // TO DO

  return NULL;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribDrawFromDistribution::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDistribution != NULL)
  {
    if (mDistribution->getId() == id)
    {
      return mDistribution;
    }

    obj = mDistribution->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  obj = mDistribInputs.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
DistribDrawFromDistribution::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mDistribution != NULL)
  {
    if (mDistribution->getMetaId() == metaid)
    {
      return mDistribution;
    }

    obj = mDistribution->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mDistribInputs.getMetaId() == metaid)
  {
    return &mDistribInputs;
  }

  obj = mDistribInputs.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
DistribDrawFromDistribution::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mDistribution, filter);

  ADD_FILTERED_LIST(ret, sublist, mDistribInputs, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribDrawFromDistribution::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "betaDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribBetaDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "cauchyDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribCauchyDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "chiSquareDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribChiSquareDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "exponentialDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribExponentialDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "fDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribFDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "gammaDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribGammaDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "inverseGammaDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribInverseGammaDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "laPlaceDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribLaPlaceDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "logNormalDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribLogNormalDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "logisticDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribLogisticDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "normalDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribNormalDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "paretoDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribParetoDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "rayleighDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribRayleighDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "studentTDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribStudentTDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "uniformDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribUniformDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "weibullDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribWeibullDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "binomialDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribBinomialDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "geometricDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribGeometricDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "hypergeometricDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribHypergeometricDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "negativeBinomialDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribNegativeBinomialDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "poissonDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribPoissonDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "bernoulliDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribBernoulliDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "categoricalDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribCategoricalDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "multivariateDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribMultivariateDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "externalDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribExternalDistribution(distribns);
    obj = mDistribution;
  }

  if (name == "listOfInputs")
  {
    if (mDistribInputs.size() != 0)
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribDrawFromDistributionAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    obj = &mDistribInputs;
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
DistribDrawFromDistribution::addExpectedAttributes(ExpectedAttributes&
  attributes)
{
  DistribBase::addExpectedAttributes(attributes);

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
DistribDrawFromDistribution::readAttributes(const XMLAttributes& attributes,
                                            const ExpectedAttributes&
                                              expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  DistribBase::readAttributes(attributes, expectedAttributes);

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
          DistribDistribDrawFromDistributionAllowedAttributes, pkgVersion, level,
            version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribDrawFromDistributionAllowedCoreAttributes, pkgVersion,
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
DistribDrawFromDistribution::writeAttributes(XMLOutputStream& stream) const
{
  DistribBase::writeAttributes(stream);
  DistribBase::writeExtensionAttributes(stream);
}

/** @endcond */



#endif /* __cplusplus */


/*
 * Creates a new DistribDrawFromDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribDrawFromDistribution_t *
DistribDrawFromDistribution_create(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion)
{
  return new DistribDrawFromDistribution(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribDrawFromDistribution_t
 * object.
 */
LIBSBML_EXTERN
DistribDrawFromDistribution_t*
DistribDrawFromDistribution_clone(const DistribDrawFromDistribution_t* ddfd)
{
  if (ddfd != NULL)
  {
    return static_cast<DistribDrawFromDistribution_t*>(ddfd->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribDrawFromDistribution_t object.
 */
LIBSBML_EXTERN
void
DistribDrawFromDistribution_free(DistribDrawFromDistribution_t* ddfd)
{
  if (ddfd != NULL)
  {
    delete ddfd;
  }
}


/*
 * Returns the value of the "id" attribute of this
 * DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribDrawFromDistribution_getId(const DistribDrawFromDistribution_t * ddfd)
{
  if (ddfd == NULL)
  {
    return NULL;
  }

  return ddfd->getId().empty() ? NULL : safe_strdup(ddfd->getId().c_str());
}


/*
 * Returns the value of the "name" attribute of this
 * DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
char *
DistribDrawFromDistribution_getName(const DistribDrawFromDistribution_t * ddfd)
{
  if (ddfd == NULL)
  {
    return NULL;
  }

  return ddfd->getName().empty() ? NULL : safe_strdup(ddfd->getName().c_str());
}


/*
 * Predicate returning @c 1 (true) if this DistribDrawFromDistribution_t's "id"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_isSetId(const DistribDrawFromDistribution_t * ddfd)
{
  return (ddfd != NULL) ? static_cast<int>(ddfd->isSetId()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribDrawFromDistribution_t's
 * "name" attribute is set.
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_isSetName(const DistribDrawFromDistribution_t *
  ddfd)
{
  return (ddfd != NULL) ? static_cast<int>(ddfd->isSetName()) : 0;
}


/*
 * Sets the value of the "id" attribute of this DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_setId(DistribDrawFromDistribution_t * ddfd,
                                  const char * id)
{
  return (ddfd != NULL) ? ddfd->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "name" attribute of this
 * DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_setName(DistribDrawFromDistribution_t * ddfd,
                                    const char * name)
{
  return (ddfd != NULL) ? ddfd->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "id" attribute of this
 * DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_unsetId(DistribDrawFromDistribution_t * ddfd)
{
  return (ddfd != NULL) ? ddfd->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "name" attribute of this
 * DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_unsetName(DistribDrawFromDistribution_t * ddfd)
{
  return (ddfd != NULL) ? ddfd->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "distribution" element of this
 * DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
const DistribDistribution_t*
DistribDrawFromDistribution_getDistribution(const DistribDrawFromDistribution_t
  * ddfd)
{
  if (ddfd == NULL)
  {
    return NULL;
  }

  return (DistribDistribution_t*)(ddfd->getDistribution());
}


/*
 * Predicate returning @c 1 (true) if this DistribDrawFromDistribution_t's
 * "distribution" element is set.
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_isSetDistribution(const
  DistribDrawFromDistribution_t * ddfd)
{
  return (ddfd != NULL) ? static_cast<int>(ddfd->isSetDistribution()) : 0;
}


/*
 * Sets the value of the "distribution" element of this
 * DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_setDistribution(
                                            DistribDrawFromDistribution_t *
                                              ddfd,
                                            const DistribDistribution_t*
                                              distribution)
{
  return (ddfd != NULL) ? ddfd->setDistribution(distribution) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribBetaDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribBetaDistribution_t object created.
 */
LIBSBML_EXTERN
DistribBetaDistribution_t*
DistribDrawFromDistribution_createDistribBetaDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribBetaDistribution() : NULL;
}


/*
 * Creates a new DistribCauchyDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribCauchyDistribution_t object created.
 */
LIBSBML_EXTERN
DistribCauchyDistribution_t*
DistribDrawFromDistribution_createDistribCauchyDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribCauchyDistribution() : NULL;
}


/*
 * Creates a new DistribChiSquareDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribChiSquareDistribution_t object created.
 */
LIBSBML_EXTERN
DistribChiSquareDistribution_t*
DistribDrawFromDistribution_createDistribChiSquareDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribChiSquareDistribution() : NULL;
}


/*
 * Creates a new DistribExponentialDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribExponentialDistribution_t object created.
 */
LIBSBML_EXTERN
DistribExponentialDistribution_t*
DistribDrawFromDistribution_createDistribExponentialDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribExponentialDistribution() : NULL;
}


/*
 * Creates a new DistribFDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the DistribFDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribFDistribution_t*
DistribDrawFromDistribution_createDistribFDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribFDistribution() : NULL;
}


/*
 * Creates a new DistribGammaDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribGammaDistribution_t object created.
 */
LIBSBML_EXTERN
DistribGammaDistribution_t*
DistribDrawFromDistribution_createDistribGammaDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribGammaDistribution() : NULL;
}


/*
 * Creates a new DistribInverseGammaDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribInverseGammaDistribution_t object created.
 */
LIBSBML_EXTERN
DistribInverseGammaDistribution_t*
DistribDrawFromDistribution_createDistribInverseGammaDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribInverseGammaDistribution() : NULL;
}


/*
 * Creates a new DistribLaPlaceDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribLaPlaceDistribution_t object created.
 */
LIBSBML_EXTERN
DistribLaPlaceDistribution_t*
DistribDrawFromDistribution_createDistribLaPlaceDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribLaPlaceDistribution() : NULL;
}


/*
 * Creates a new DistribLogNormalDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribLogNormalDistribution_t object created.
 */
LIBSBML_EXTERN
DistribLogNormalDistribution_t*
DistribDrawFromDistribution_createDistribLogNormalDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribLogNormalDistribution() : NULL;
}


/*
 * Creates a new DistribLogisticDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribLogisticDistribution_t object created.
 */
LIBSBML_EXTERN
DistribLogisticDistribution_t*
DistribDrawFromDistribution_createDistribLogisticDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribLogisticDistribution() : NULL;
}


/*
 * Creates a new DistribNormalDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribNormalDistribution_t object created.
 */
LIBSBML_EXTERN
DistribNormalDistribution_t*
DistribDrawFromDistribution_createDistribNormalDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribNormalDistribution() : NULL;
}


/*
 * Creates a new DistribParetoDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribParetoDistribution_t object created.
 */
LIBSBML_EXTERN
DistribParetoDistribution_t*
DistribDrawFromDistribution_createDistribParetoDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribParetoDistribution() : NULL;
}


/*
 * Creates a new DistribRayleighDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribRayleighDistribution_t object created.
 */
LIBSBML_EXTERN
DistribRayleighDistribution_t*
DistribDrawFromDistribution_createDistribRayleighDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribRayleighDistribution() : NULL;
}


/*
 * Creates a new DistribStudentTDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribStudentTDistribution_t object created.
 */
LIBSBML_EXTERN
DistribStudentTDistribution_t*
DistribDrawFromDistribution_createDistribStudentTDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribStudentTDistribution() : NULL;
}


/*
 * Creates a new DistribUniformDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribUniformDistribution_t object created.
 */
LIBSBML_EXTERN
DistribUniformDistribution_t*
DistribDrawFromDistribution_createDistribUniformDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribUniformDistribution() : NULL;
}


/*
 * Creates a new DistribWeibullDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribWeibullDistribution_t object created.
 */
LIBSBML_EXTERN
DistribWeibullDistribution_t*
DistribDrawFromDistribution_createDistribWeibullDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribWeibullDistribution() : NULL;
}


/*
 * Creates a new DistribBinomialDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribBinomialDistribution_t object created.
 */
LIBSBML_EXTERN
DistribBinomialDistribution_t*
DistribDrawFromDistribution_createDistribBinomialDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribBinomialDistribution() : NULL;
}


/*
 * Creates a new DistribGeometricDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribGeometricDistribution_t object created.
 */
LIBSBML_EXTERN
DistribGeometricDistribution_t*
DistribDrawFromDistribution_createDistribGeometricDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribGeometricDistribution() : NULL;
}


/*
 * Creates a new DistribHypergeometricDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribHypergeometricDistribution_t object created.
 */
LIBSBML_EXTERN
DistribHypergeometricDistribution_t*
DistribDrawFromDistribution_createDistribHypergeometricDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribHypergeometricDistribution() :
    NULL;
}


/*
 * Creates a new DistribNegativeBinomialDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribNegativeBinomialDistribution_t object created.
 */
LIBSBML_EXTERN
DistribNegativeBinomialDistribution_t*
DistribDrawFromDistribution_createDistribNegativeBinomialDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribNegativeBinomialDistribution() :
    NULL;
}


/*
 * Creates a new DistribPoissonDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribPoissonDistribution_t object created.
 */
LIBSBML_EXTERN
DistribPoissonDistribution_t*
DistribDrawFromDistribution_createDistribPoissonDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribPoissonDistribution() : NULL;
}


/*
 * Creates a new DistribBernoulliDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribBernoulliDistribution_t object created.
 */
LIBSBML_EXTERN
DistribBernoulliDistribution_t*
DistribDrawFromDistribution_createDistribBernoulliDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribBernoulliDistribution() : NULL;
}


/*
 * Creates a new DistribCategoricalDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribCategoricalDistribution_t object created.
 */
LIBSBML_EXTERN
DistribCategoricalDistribution_t*
DistribDrawFromDistribution_createDistribCategoricalDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribCategoricalDistribution() : NULL;
}


/*
 * Creates a new DistribMultivariateDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribMultivariateDistribution_t object created.
 */
LIBSBML_EXTERN
DistribMultivariateDistribution_t*
DistribDrawFromDistribution_createDistribMultivariateDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribMultivariateDistribution() : NULL;
}


/*
 * Creates a new DistribExternalDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribExternalDistribution_t object created.
 */
LIBSBML_EXTERN
DistribExternalDistribution_t*
DistribDrawFromDistribution_createDistribExternalDistribution(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribExternalDistribution() : NULL;
}


/*
 * Unsets the value of the "distribution" element of this
 * DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_unsetDistribution(DistribDrawFromDistribution_t *
  ddfd)
{
  return (ddfd != NULL) ? ddfd->unsetDistribution() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing DistribInput_t objects from this
 * DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
ListOf_t*
DistribDrawFromDistribution_getListOfDistribInputs(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->getListOfDistribInputs() : NULL;
}


/*
 * Get a DistribInput_t from the DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
DistribInput_t*
DistribDrawFromDistribution_getDistribInput(
                                            DistribDrawFromDistribution_t*
                                              ddfd,
                                            unsigned int n)
{
  return (ddfd != NULL) ? ddfd->getDistribInput(n) : NULL;
}


/*
 * Get a DistribInput_t from the DistribDrawFromDistribution_t based on its
 * identifier.
 */
LIBSBML_EXTERN
DistribInput_t*
DistribDrawFromDistribution_getDistribInputById(
                                                DistribDrawFromDistribution_t*
                                                  ddfd,
                                                const char *sid)
{
  return (ddfd != NULL && sid != NULL) ? ddfd->getDistribInput(sid) : NULL;
}


/*
 * Adds a copy of the given DistribInput_t to this
 * DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_addDistribInput(
                                            DistribDrawFromDistribution_t*
                                              ddfd,
                                            const DistribInput_t* di)
{
  return (ddfd != NULL) ? ddfd->addDistribInput(di) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of DistribInput_t objects in this
 * DistribDrawFromDistribution_t.
 */
LIBSBML_EXTERN
unsigned int
DistribDrawFromDistribution_getNumDistribInputs(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->getNumDistribInputs() : SBML_INT_MAX;
}


/*
 * Creates a new DistribInput_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the DistribInput_t object
 * created.
 */
LIBSBML_EXTERN
DistribInput_t*
DistribDrawFromDistribution_createDistribInput(DistribDrawFromDistribution_t*
  ddfd)
{
  return (ddfd != NULL) ? ddfd->createDistribInput() : NULL;
}


/*
 * Removes the nth DistribInput_t from this DistribDrawFromDistribution_t and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
DistribInput_t*
DistribDrawFromDistribution_removeDistribInput(
                                               DistribDrawFromDistribution_t*
                                                 ddfd,
                                               unsigned int n)
{
  return (ddfd != NULL) ? ddfd->removeDistribInput(n) : NULL;
}


/*
 * Removes the DistribInput_t from this DistribDrawFromDistribution_t based on
 * its identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
DistribInput_t*
DistribDrawFromDistribution_removeDistribInputById(
                                                   DistribDrawFromDistribution_t*
                                                     ddfd,
                                                   const char* sid)
{
  return (ddfd != NULL && sid != NULL) ? ddfd->removeDistribInput(sid) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribDrawFromDistribution_t object have been set.
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_hasRequiredAttributes(const
  DistribDrawFromDistribution_t * ddfd)
{
  return (ddfd != NULL) ? static_cast<int>(ddfd->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


