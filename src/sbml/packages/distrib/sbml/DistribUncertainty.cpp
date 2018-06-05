/**
 * @file DistribUncertainty.cpp
 * @brief Implementation of the DistribUncertainty class.
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
#include <sbml/packages/distrib/sbml/DistribUncertainty.h>
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
 * Creates a new DistribUncertainty using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
DistribUncertainty::DistribUncertainty(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion)
  : DistribBase(level, version, pkgVersion)
  , mUncertStatistics (NULL)
  , mDistribution (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribUncertainty using the given DistribPkgNamespaces
 * object.
 */
DistribUncertainty::DistribUncertainty(DistribPkgNamespaces *distribns)
  : DistribBase(distribns)
  , mUncertStatistics (NULL)
  , mDistribution (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribUncertainty.
 */
DistribUncertainty::DistribUncertainty(const DistribUncertainty& orig)
  : DistribBase( orig )
  , mUncertStatistics ( NULL )
  , mDistribution ( NULL )
{
  if (orig.mUncertStatistics != NULL)
  {
    mUncertStatistics = orig.mUncertStatistics->clone();
  }

  if (orig.mDistribution != NULL)
  {
    mDistribution = orig.mDistribution->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribUncertainty.
 */
DistribUncertainty&
DistribUncertainty::operator=(const DistribUncertainty& rhs)
{
  if (&rhs != this)
  {
    DistribBase::operator=(rhs);
    delete mUncertStatistics;
    if (rhs.mUncertStatistics != NULL)
    {
      mUncertStatistics = rhs.mUncertStatistics->clone();
    }
    else
    {
      mUncertStatistics = NULL;
    }

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
 * Creates and returns a deep copy of this DistribUncertainty object.
 */
DistribUncertainty*
DistribUncertainty::clone() const
{
  return new DistribUncertainty(*this);
}


/*
 * Destructor for DistribUncertainty.
 */
DistribUncertainty::~DistribUncertainty()
{
  delete mUncertStatistics;
  mUncertStatistics = NULL;
  delete mDistribution;
  mDistribution = NULL;
}


/*
 * Returns the value of the "uncertStatistics" element of this
 * DistribUncertainty.
 */
const DistribUncertStatistics*
DistribUncertainty::getUncertStatistics() const
{
  return mUncertStatistics;
}


/*
 * Returns the value of the "uncertStatistics" element of this
 * DistribUncertainty.
 */
DistribUncertStatistics*
DistribUncertainty::getUncertStatistics()
{
  return mUncertStatistics;
}


/*
 * Returns the value of the "distribution" element of this DistribUncertainty.
 */
const DistribDistribution*
DistribUncertainty::getDistribution() const
{
  return mDistribution;
}


/*
 * Returns the value of the "distribution" element of this DistribUncertainty.
 */
DistribDistribution*
DistribUncertainty::getDistribution()
{
  return mDistribution;
}


/*
 * Predicate returning @c true if this DistribUncertainty's "uncertStatistics"
 * element is set.
 */
bool
DistribUncertainty::isSetUncertStatistics() const
{
  return (mUncertStatistics != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertainty's "distribution"
 * element is set.
 */
bool
DistribUncertainty::isSetDistribution() const
{
  return (mDistribution != NULL);
}


/*
 * Sets the value of the "uncertStatistics" element of this DistribUncertainty.
 */
int
DistribUncertainty::setUncertStatistics(const DistribUncertStatistics*
  uncertStatistics)
{
  if (uncertStatistics == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (uncertStatistics->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != uncertStatistics->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != uncertStatistics->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != uncertStatistics->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mUncertStatistics;
    mUncertStatistics = (uncertStatistics != NULL) ?
      static_cast<DistribUncertStatistics*>(uncertStatistics->clone()) : NULL;
    if (mUncertStatistics != NULL) mUncertStatistics->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "distribution" element of this DistribUncertainty.
 */
int
DistribUncertainty::setDistribution(const DistribDistribution* distribution)
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
 * Creates a new DistribUncertStatistics object, adds it to this
 * DistribUncertainty object and returns the DistribUncertStatistics object
 * created.
 */
DistribUncertStatistics*
DistribUncertainty::createUncertStatistics()
{
  if (mUncertStatistics != NULL)
  {
    delete mUncertStatistics;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mUncertStatistics = new DistribUncertStatistics(distribns);

  mUncertStatistics->setElementName("uncertStatistics");

  delete distribns;

  connectToChild();

  return mUncertStatistics;
}


/*
 * Creates a new DistribBetaDistribution object, adds it to this
 * DistribUncertainty object and returns the DistribBetaDistribution object
 * created.
 */
DistribBetaDistribution*
DistribUncertainty::createDistribBetaDistribution()
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
 * DistribUncertainty object and returns the DistribCauchyDistribution object
 * created.
 */
DistribCauchyDistribution*
DistribUncertainty::createDistribCauchyDistribution()
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
 * DistribUncertainty object and returns the DistribChiSquareDistribution
 * object created.
 */
DistribChiSquareDistribution*
DistribUncertainty::createDistribChiSquareDistribution()
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
 * DistribUncertainty object and returns the DistribExponentialDistribution
 * object created.
 */
DistribExponentialDistribution*
DistribUncertainty::createDistribExponentialDistribution()
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
 * DistribUncertainty object and returns the DistribFDistribution object
 * created.
 */
DistribFDistribution*
DistribUncertainty::createDistribFDistribution()
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
 * DistribUncertainty object and returns the DistribGammaDistribution object
 * created.
 */
DistribGammaDistribution*
DistribUncertainty::createDistribGammaDistribution()
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
 * DistribUncertainty object and returns the DistribInverseGammaDistribution
 * object created.
 */
DistribInverseGammaDistribution*
DistribUncertainty::createDistribInverseGammaDistribution()
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
 * DistribUncertainty object and returns the DistribLaPlaceDistribution object
 * created.
 */
DistribLaPlaceDistribution*
DistribUncertainty::createDistribLaPlaceDistribution()
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
 * DistribUncertainty object and returns the DistribLogNormalDistribution
 * object created.
 */
DistribLogNormalDistribution*
DistribUncertainty::createDistribLogNormalDistribution()
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
 * DistribUncertainty object and returns the DistribLogisticDistribution object
 * created.
 */
DistribLogisticDistribution*
DistribUncertainty::createDistribLogisticDistribution()
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
 * DistribUncertainty object and returns the DistribNormalDistribution object
 * created.
 */
DistribNormalDistribution*
DistribUncertainty::createDistribNormalDistribution()
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
 * DistribUncertainty object and returns the DistribParetoDistribution object
 * created.
 */
DistribParetoDistribution*
DistribUncertainty::createDistribParetoDistribution()
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
 * DistribUncertainty object and returns the DistribRayleighDistribution object
 * created.
 */
DistribRayleighDistribution*
DistribUncertainty::createDistribRayleighDistribution()
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
 * DistribUncertainty object and returns the DistribStudentTDistribution object
 * created.
 */
DistribStudentTDistribution*
DistribUncertainty::createDistribStudentTDistribution()
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
 * DistribUncertainty object and returns the DistribUniformDistribution object
 * created.
 */
DistribUniformDistribution*
DistribUncertainty::createDistribUniformDistribution()
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
 * DistribUncertainty object and returns the DistribWeibullDistribution object
 * created.
 */
DistribWeibullDistribution*
DistribUncertainty::createDistribWeibullDistribution()
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
 * DistribUncertainty object and returns the DistribBinomialDistribution object
 * created.
 */
DistribBinomialDistribution*
DistribUncertainty::createDistribBinomialDistribution()
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
 * DistribUncertainty object and returns the DistribGeometricDistribution
 * object created.
 */
DistribGeometricDistribution*
DistribUncertainty::createDistribGeometricDistribution()
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
 * DistribUncertainty object and returns the DistribHypergeometricDistribution
 * object created.
 */
DistribHypergeometricDistribution*
DistribUncertainty::createDistribHypergeometricDistribution()
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
 * DistribUncertainty object and returns the
 * DistribNegativeBinomialDistribution object created.
 */
DistribNegativeBinomialDistribution*
DistribUncertainty::createDistribNegativeBinomialDistribution()
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
 * DistribUncertainty object and returns the DistribPoissonDistribution object
 * created.
 */
DistribPoissonDistribution*
DistribUncertainty::createDistribPoissonDistribution()
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
 * DistribUncertainty object and returns the DistribBernoulliDistribution
 * object created.
 */
DistribBernoulliDistribution*
DistribUncertainty::createDistribBernoulliDistribution()
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
 * DistribUncertainty object and returns the DistribCategoricalDistribution
 * object created.
 */
DistribCategoricalDistribution*
DistribUncertainty::createDistribCategoricalDistribution()
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
 * DistribUncertainty object and returns the DistribMultivariateDistribution
 * object created.
 */
DistribMultivariateDistribution*
DistribUncertainty::createDistribMultivariateDistribution()
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
 * DistribUncertainty object and returns the DistribExternalDistribution object
 * created.
 */
DistribExternalDistribution*
DistribUncertainty::createDistribExternalDistribution()
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
 * Unsets the value of the "uncertStatistics" element of this
 * DistribUncertainty.
 */
int
DistribUncertainty::unsetUncertStatistics()
{
  delete mUncertStatistics;
  mUncertStatistics = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "distribution" element of this DistribUncertainty.
 */
int
DistribUncertainty::unsetDistribution()
{
  delete mDistribution;
  mDistribution = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this DistribUncertainty object.
 */
const std::string&
DistribUncertainty::getElementName() const
{
  static const string name = "uncertainty";
  return name;
}


/*
 * Returns the libSBML type code for this DistribUncertainty object.
 */
int
DistribUncertainty::getTypeCode() const
{
  return SBML_DISTRIB_UNCERTAINTY;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribUncertainty object have been set.
 */
bool
DistribUncertainty::hasRequiredAttributes() const
{
  bool allPresent = DistribBase::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribUncertainty object have been set.
 */
bool
DistribUncertainty::hasRequiredElements() const
{
  bool allPresent = DistribBase::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribUncertainty::writeElements(XMLOutputStream& stream) const
{
  DistribBase::writeElements(stream);

  if (isSetUncertStatistics() == true)
  {
    mUncertStatistics->write(stream);
  }

  if (isSetDistribution() == true)
  {
    mDistribution->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribUncertainty::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mUncertStatistics != NULL)
  {
    mUncertStatistics->accept(v);
  }

  if (mDistribution != NULL)
  {
    mDistribution->accept(v);
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
DistribUncertainty::setSBMLDocument(SBMLDocument* d)
{
  DistribBase::setSBMLDocument(d);

  if (mUncertStatistics != NULL)
  {
    mUncertStatistics->setSBMLDocument(d);
  }

  if (mDistribution != NULL)
  {
    mDistribution->setSBMLDocument(d);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribUncertainty::connectToChild()
{
  DistribBase::connectToChild();

  if (mUncertStatistics != NULL)
  {
    mUncertStatistics->connectToParent(this);
  }

  if (mDistribution != NULL)
  {
    mDistribution->connectToParent(this);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribUncertainty::enablePackageInternal(const std::string& pkgURI,
                                          const std::string& pkgPrefix,
                                          bool flag)
{
  DistribBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  if (isSetUncertStatistics())
  {
    mUncertStatistics->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetDistribution())
  {
    mDistribution->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribUncertainty::updateSBMLNamespace(const std::string& package,
                                        unsigned int level,
                                        unsigned int version)
{
  DistribBase::updateSBMLNamespace(package, level, version);

  if (mUncertStatistics != NULL)
  {
    mUncertStatistics->updateSBMLNamespace(package, level, version);
  }

  if (mDistribution != NULL)
  {
    mDistribution->updateSBMLNamespace(package, level, version);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertainty.
 */
int
DistribUncertainty::getAttribute(const std::string& attributeName,
                                 bool& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertainty.
 */
int
DistribUncertainty::getAttribute(const std::string& attributeName,
                                 int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertainty.
 */
int
DistribUncertainty::getAttribute(const std::string& attributeName,
                                 double& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertainty.
 */
int
DistribUncertainty::getAttribute(const std::string& attributeName,
                                 unsigned int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this DistribUncertainty.
 */
int
DistribUncertainty::getAttribute(const std::string& attributeName,
                                 std::string& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribUncertainty's attribute
 * "attributeName" is set.
 */
bool
DistribUncertainty::isSetAttribute(const std::string& attributeName) const
{
  bool value = DistribBase::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertainty.
 */
int
DistribUncertainty::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertainty.
 */
int
DistribUncertainty::setAttribute(const std::string& attributeName, int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertainty.
 */
int
DistribUncertainty::setAttribute(const std::string& attributeName,
                                 double value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertainty.
 */
int
DistribUncertainty::setAttribute(const std::string& attributeName,
                                 unsigned int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DistribUncertainty.
 */
int
DistribUncertainty::setAttribute(const std::string& attributeName,
                                 const std::string& value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribUncertainty.
 */
int
DistribUncertainty::unsetAttribute(const std::string& attributeName)
{
  int value = DistribBase::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this DistribUncertainty.
 */
SBase*
DistribUncertainty::createChildObject(const std::string& elementName)
{
  DistribBase* obj = NULL;

  if (elementName == "uncertStatistics")
  {
    return createUncertStatistics();
  }
  else if (elementName == "betaDistribution")
  {
    return createDistribBetaDistribution();
  }
  else if (elementName == "cauchyDistribution")
  {
    return createDistribCauchyDistribution();
  }
  else if (elementName == "chiSquareDistribution")
  {
    return createDistribChiSquareDistribution();
  }
  else if (elementName == "exponentialDistribution")
  {
    return createDistribExponentialDistribution();
  }
  else if (elementName == "fDistribution")
  {
    return createDistribFDistribution();
  }
  else if (elementName == "gammaDistribution")
  {
    return createDistribGammaDistribution();
  }
  else if (elementName == "inverseGammaDistribution")
  {
    return createDistribInverseGammaDistribution();
  }
  else if (elementName == "laPlaceDistribution")
  {
    return createDistribLaPlaceDistribution();
  }
  else if (elementName == "logNormalDistribution")
  {
    return createDistribLogNormalDistribution();
  }
  else if (elementName == "logisticDistribution")
  {
    return createDistribLogisticDistribution();
  }
  else if (elementName == "normalDistribution")
  {
    return createDistribNormalDistribution();
  }
  else if (elementName == "paretoDistribution")
  {
    return createDistribParetoDistribution();
  }
  else if (elementName == "rayleighDistribution")
  {
    return createDistribRayleighDistribution();
  }
  else if (elementName == "studentTDistribution")
  {
    return createDistribStudentTDistribution();
  }
  else if (elementName == "uniformDistribution")
  {
    return createDistribUniformDistribution();
  }
  else if (elementName == "weibullDistribution")
  {
    return createDistribWeibullDistribution();
  }
  else if (elementName == "binomialDistribution")
  {
    return createDistribBinomialDistribution();
  }
  else if (elementName == "geometricDistribution")
  {
    return createDistribGeometricDistribution();
  }
  else if (elementName == "hypergeometricDistribution")
  {
    return createDistribHypergeometricDistribution();
  }
  else if (elementName == "negativeBinomialDistribution")
  {
    return createDistribNegativeBinomialDistribution();
  }
  else if (elementName == "poissonDistribution")
  {
    return createDistribPoissonDistribution();
  }
  else if (elementName == "bernoulliDistribution")
  {
    return createDistribBernoulliDistribution();
  }
  else if (elementName == "categoricalDistribution")
  {
    return createDistribCategoricalDistribution();
  }
  else if (elementName == "multivariateDistribution")
  {
    return createDistribMultivariateDistribution();
  }
  else if (elementName == "externalDistribution")
  {
    return createDistribExternalDistribution();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribUncertainty.
 */
int
DistribUncertainty::addChildObject(const std::string& elementName,
                                   const SBase* element)
{
  if (elementName == "uncertStatistics" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTSTATISTICS)
  {
    return setUncertStatistics((const DistribUncertStatistics*)(element));
  }
  else if (elementName == "betaDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_BETADISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "cauchyDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_CAUCHYDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "chiSquareDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_CHISQUAREDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "exponentialDistribution" && element->getTypeCode()
    == SBML_DISTRIB_EXPONENTIALDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "fDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_FDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "gammaDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_GAMMADISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "inverseGammaDistribution" && element->getTypeCode()
    == SBML_DISTRIB_INVERSEGAMMADISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "laPlaceDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_LAPLACEDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "logNormalDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_LOGNORMALDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "logisticDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_LOGISTICDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "normalDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_NORMALDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "paretoDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_PARETODISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "rayleighDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_RAYLEIGHDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "studentTDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_STUDENTTDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "uniformDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_UNIFORMDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "weibullDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_WEIBULLDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "binomialDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_BINOMIALDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "geometricDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_GEOMETRICLDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "hypergeometricDistribution" &&
    element->getTypeCode() == SBML_DISTRIB_HYPERGEOMETRICDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "negativeBinomialDistribution" &&
    element->getTypeCode() == SBML_DISTRIB_NEGATIVEBINOMIALDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "poissonDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_POISSONDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "bernoulliDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_BERNOULLIDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "categoricalDistribution" && element->getTypeCode()
    == SBML_DISTRIB_CATEGORICALDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "multivariateDistribution" && element->getTypeCode()
    == SBML_DISTRIB_MULTIVARIATEDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }
  else if (elementName == "externalDistribution" && element->getTypeCode() ==
    SBML_DISTRIB_EXTERNALDISTRIBUTION)
  {
    return setDistribution((const DistribDistribution*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribUncertainty.
 */
SBase*
DistribUncertainty::removeChildObject(const std::string& elementName,
                                      const std::string& id)
{
  if (elementName == "uncertStatistics")
  {
    DistribUncertStatistics * obj = getUncertStatistics();
    if (unsetUncertStatistics() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "betaDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "cauchyDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "chiSquareDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "exponentialDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "fDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "gammaDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "inverseGammaDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "laPlaceDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "logNormalDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "logisticDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "normalDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "paretoDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "rayleighDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "studentTDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "uniformDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "weibullDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "binomialDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "geometricDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "hypergeometricDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "negativeBinomialDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "poissonDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "bernoulliDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "categoricalDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "multivariateDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "externalDistribution")
  {
    DistribDistribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribUncertainty.
 */
unsigned int
DistribUncertainty::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "uncertStatistics")
  {
    if (isSetUncertStatistics())
    {
      return 1;
    }
  }
  else if (elementName == "distribution")
  {
    if (isSetDistribution())
    {
      return 1;
    }
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribUncertainty.
 */
SBase*
DistribUncertainty::getObject(const std::string& elementName,
                              unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "uncertStatistics")
  {
    return getUncertStatistics();
  }
  else if (elementName == "distribution")
  {
    return getDistribution();
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribUncertainty::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mUncertStatistics != NULL)
  {
    if (mUncertStatistics->getId() == id)
    {
      return mUncertStatistics;
    }

    obj = mUncertStatistics->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

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

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
DistribUncertainty::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mUncertStatistics != NULL)
  {
    if (mUncertStatistics->getMetaId() == metaid)
    {
      return mUncertStatistics;
    }

    obj = mUncertStatistics->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

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

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
DistribUncertainty::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mUncertStatistics, filter);
  ADD_FILTERED_POINTER(ret, sublist, mDistribution, filter);


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribUncertainty::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribBase::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "betaDistribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
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
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mDistribution;
    mDistribution = new DistribExternalDistribution(distribns);
    obj = mDistribution;
  }
  else if (name == "uncertStatistics")
  {
    if (isSetUncertStatistics())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertaintyAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mUncertStatistics;
    mUncertStatistics = new DistribUncertStatistics(distribns);
    mUncertStatistics->setElementName(name);
    obj = mUncertStatistics;
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
DistribUncertainty::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribBase::addExpectedAttributes(attributes);

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
DistribUncertainty::readAttributes(const XMLAttributes& attributes,
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
        log->logPackageError("distrib", DistribUnknown, pkgVersion, level,
          version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("distrib",
          DistribDistribUncertaintyAllowedCoreAttributes, pkgVersion, level,
            version, details);
      }
    }
  }

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    readL3V1V1Attributes(attributes);
  }

  else
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
DistribUncertainty::readL3V1V1Attributes(const XMLAttributes& attributes)
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
DistribUncertainty::readL3V2V1Attributes(const XMLAttributes& attributes)
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
DistribUncertainty::writeAttributes(XMLOutputStream& stream) const
{
  DistribBase::writeAttributes(stream);

  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();

  if (level == 3 && version == 1 && pkgVersion == 1)
  {
    writeL3V1V1Attributes(stream);
  }

  else
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
DistribUncertainty::writeL3V1V1Attributes(XMLOutputStream& stream) const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribUncertainty::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribUncertainty_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribUncertainty_t *
DistribUncertainty_create(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion)
{
  return new DistribUncertainty(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribUncertainty_t object.
 */
LIBSBML_EXTERN
DistribUncertainty_t*
DistribUncertainty_clone(const DistribUncertainty_t* du)
{
  if (du != NULL)
  {
    return static_cast<DistribUncertainty_t*>(du->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribUncertainty_t object.
 */
LIBSBML_EXTERN
void
DistribUncertainty_free(DistribUncertainty_t* du)
{
  if (du != NULL)
  {
    delete du;
  }
}


/*
 * Returns the value of the "uncertStatistics" element of this
 * DistribUncertainty_t.
 */
LIBSBML_EXTERN
const DistribUncertStatistics_t*
DistribUncertainty_getUncertStatistics(const DistribUncertainty_t * du)
{
  if (du == NULL)
  {
    return NULL;
  }

  return (DistribUncertStatistics_t*)(du->getUncertStatistics());
}


/*
 * Returns the value of the "distribution" element of this
 * DistribUncertainty_t.
 */
LIBSBML_EXTERN
const DistribDistribution_t*
DistribUncertainty_getDistribution(const DistribUncertainty_t * du)
{
  if (du == NULL)
  {
    return NULL;
  }

  return (DistribDistribution_t*)(du->getDistribution());
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertainty_t's
 * "uncertStatistics" element is set.
 */
LIBSBML_EXTERN
int
DistribUncertainty_isSetUncertStatistics(const DistribUncertainty_t * du)
{
  return (du != NULL) ? static_cast<int>(du->isSetUncertStatistics()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertainty_t's
 * "distribution" element is set.
 */
LIBSBML_EXTERN
int
DistribUncertainty_isSetDistribution(const DistribUncertainty_t * du)
{
  return (du != NULL) ? static_cast<int>(du->isSetDistribution()) : 0;
}


/*
 * Sets the value of the "uncertStatistics" element of this
 * DistribUncertainty_t.
 */
LIBSBML_EXTERN
int
DistribUncertainty_setUncertStatistics(DistribUncertainty_t * du,
                                       const DistribUncertStatistics_t*
                                         uncertStatistics)
{
  return (du != NULL) ? du->setUncertStatistics(uncertStatistics) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "distribution" element of this DistribUncertainty_t.
 */
LIBSBML_EXTERN
int
DistribUncertainty_setDistribution(DistribUncertainty_t * du,
                                   const DistribDistribution_t* distribution)
{
  return (du != NULL) ? du->setDistribution(distribution) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertStatistics_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribUncertStatistics_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertStatistics_t*
DistribUncertainty_createUncertStatistics(DistribUncertainty_t* du)
{
  if (du == NULL)
  {
    return NULL;
  }

  return (DistribUncertStatistics_t*)(du->createUncertStatistics());
}


/*
 * Creates a new DistribBetaDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribBetaDistribution_t object
 * created.
 */
LIBSBML_EXTERN
DistribBetaDistribution_t*
DistribUncertainty_createDistribBetaDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribBetaDistribution() : NULL;
}


/*
 * Creates a new DistribCauchyDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribCauchyDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribCauchyDistribution_t*
DistribUncertainty_createDistribCauchyDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribCauchyDistribution() : NULL;
}


/*
 * Creates a new DistribChiSquareDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribChiSquareDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribChiSquareDistribution_t*
DistribUncertainty_createDistribChiSquareDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribChiSquareDistribution() : NULL;
}


/*
 * Creates a new DistribExponentialDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribExponentialDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribExponentialDistribution_t*
DistribUncertainty_createDistribExponentialDistribution(DistribUncertainty_t*
  du)
{
  return (du != NULL) ? du->createDistribExponentialDistribution() : NULL;
}


/*
 * Creates a new DistribFDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribFDistribution_t object
 * created.
 */
LIBSBML_EXTERN
DistribFDistribution_t*
DistribUncertainty_createDistribFDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribFDistribution() : NULL;
}


/*
 * Creates a new DistribGammaDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribGammaDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribGammaDistribution_t*
DistribUncertainty_createDistribGammaDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribGammaDistribution() : NULL;
}


/*
 * Creates a new DistribInverseGammaDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the
 * DistribInverseGammaDistribution_t object created.
 */
LIBSBML_EXTERN
DistribInverseGammaDistribution_t*
DistribUncertainty_createDistribInverseGammaDistribution(DistribUncertainty_t*
  du)
{
  return (du != NULL) ? du->createDistribInverseGammaDistribution() : NULL;
}


/*
 * Creates a new DistribLaPlaceDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribLaPlaceDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribLaPlaceDistribution_t*
DistribUncertainty_createDistribLaPlaceDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribLaPlaceDistribution() : NULL;
}


/*
 * Creates a new DistribLogNormalDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribLogNormalDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribLogNormalDistribution_t*
DistribUncertainty_createDistribLogNormalDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribLogNormalDistribution() : NULL;
}


/*
 * Creates a new DistribLogisticDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribLogisticDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribLogisticDistribution_t*
DistribUncertainty_createDistribLogisticDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribLogisticDistribution() : NULL;
}


/*
 * Creates a new DistribNormalDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribNormalDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribNormalDistribution_t*
DistribUncertainty_createDistribNormalDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribNormalDistribution() : NULL;
}


/*
 * Creates a new DistribParetoDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribParetoDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribParetoDistribution_t*
DistribUncertainty_createDistribParetoDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribParetoDistribution() : NULL;
}


/*
 * Creates a new DistribRayleighDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribRayleighDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribRayleighDistribution_t*
DistribUncertainty_createDistribRayleighDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribRayleighDistribution() : NULL;
}


/*
 * Creates a new DistribStudentTDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribStudentTDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribStudentTDistribution_t*
DistribUncertainty_createDistribStudentTDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribStudentTDistribution() : NULL;
}


/*
 * Creates a new DistribUniformDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribUniformDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribUniformDistribution_t*
DistribUncertainty_createDistribUniformDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribUniformDistribution() : NULL;
}


/*
 * Creates a new DistribWeibullDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribWeibullDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribWeibullDistribution_t*
DistribUncertainty_createDistribWeibullDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribWeibullDistribution() : NULL;
}


/*
 * Creates a new DistribBinomialDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribBinomialDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribBinomialDistribution_t*
DistribUncertainty_createDistribBinomialDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribBinomialDistribution() : NULL;
}


/*
 * Creates a new DistribGeometricDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribGeometricDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribGeometricDistribution_t*
DistribUncertainty_createDistribGeometricDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribGeometricDistribution() : NULL;
}


/*
 * Creates a new DistribHypergeometricDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the
 * DistribHypergeometricDistribution_t object created.
 */
LIBSBML_EXTERN
DistribHypergeometricDistribution_t*
DistribUncertainty_createDistribHypergeometricDistribution(DistribUncertainty_t*
  du)
{
  return (du != NULL) ? du->createDistribHypergeometricDistribution() : NULL;
}


/*
 * Creates a new DistribNegativeBinomialDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the
 * DistribNegativeBinomialDistribution_t object created.
 */
LIBSBML_EXTERN
DistribNegativeBinomialDistribution_t*
DistribUncertainty_createDistribNegativeBinomialDistribution(DistribUncertainty_t*
  du)
{
  return (du != NULL) ? du->createDistribNegativeBinomialDistribution() : NULL;
}


/*
 * Creates a new DistribPoissonDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribPoissonDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribPoissonDistribution_t*
DistribUncertainty_createDistribPoissonDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribPoissonDistribution() : NULL;
}


/*
 * Creates a new DistribBernoulliDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribBernoulliDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribBernoulliDistribution_t*
DistribUncertainty_createDistribBernoulliDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribBernoulliDistribution() : NULL;
}


/*
 * Creates a new DistribCategoricalDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribCategoricalDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribCategoricalDistribution_t*
DistribUncertainty_createDistribCategoricalDistribution(DistribUncertainty_t*
  du)
{
  return (du != NULL) ? du->createDistribCategoricalDistribution() : NULL;
}


/*
 * Creates a new DistribMultivariateDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the
 * DistribMultivariateDistribution_t object created.
 */
LIBSBML_EXTERN
DistribMultivariateDistribution_t*
DistribUncertainty_createDistribMultivariateDistribution(DistribUncertainty_t*
  du)
{
  return (du != NULL) ? du->createDistribMultivariateDistribution() : NULL;
}


/*
 * Creates a new DistribExternalDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribExternalDistribution_t
 * object created.
 */
LIBSBML_EXTERN
DistribExternalDistribution_t*
DistribUncertainty_createDistribExternalDistribution(DistribUncertainty_t* du)
{
  return (du != NULL) ? du->createDistribExternalDistribution() : NULL;
}


/*
 * Unsets the value of the "uncertStatistics" element of this
 * DistribUncertainty_t.
 */
LIBSBML_EXTERN
int
DistribUncertainty_unsetUncertStatistics(DistribUncertainty_t * du)
{
  return (du != NULL) ? du->unsetUncertStatistics() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "distribution" element of this DistribUncertainty_t.
 */
LIBSBML_EXTERN
int
DistribUncertainty_unsetDistribution(DistribUncertainty_t * du)
{
  return (du != NULL) ? du->unsetDistribution() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUncertainty_t object have been set.
 */
LIBSBML_EXTERN
int
DistribUncertainty_hasRequiredAttributes(const DistribUncertainty_t * du)
{
  return (du != NULL) ? static_cast<int>(du->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribUncertainty_t object have been set.
 */
LIBSBML_EXTERN
int
DistribUncertainty_hasRequiredElements(const DistribUncertainty_t * du)
{
  return (du != NULL) ? static_cast<int>(du->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


