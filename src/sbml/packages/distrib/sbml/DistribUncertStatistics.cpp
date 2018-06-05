/**
 * @file DistribUncertStatistics.cpp
 * @brief Implementation of the DistribUncertStatistics class.
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
#include <sbml/packages/distrib/sbml/DistribUncertStatistics.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DistribUncertStatistics using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
DistribUncertStatistics::DistribUncertStatistics(unsigned int level,
                                                 unsigned int version,
                                                 unsigned int pkgVersion)
  : DistribBase(level, version, pkgVersion)
  , mCoefficientOfVariation (NULL)
  , mKurtosis (NULL)
  , mMean (NULL)
  , mMedian (NULL)
  , mMode (NULL)
  , mSkewness (NULL)
  , mStandardDeviation (NULL)
  , mVariance (NULL)
  , mConfidenceInterval (NULL)
  , mCredibleInterval (NULL)
  , mInterquartileRange (NULL)
  , mRange (NULL)
  , mDistribExternalParameters (level, version, pkgVersion)
  , mElementName("uncertStatistics")
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new DistribUncertStatistics using the given DistribPkgNamespaces
 * object.
 */
DistribUncertStatistics::DistribUncertStatistics(DistribPkgNamespaces
  *distribns)
  : DistribBase(distribns)
  , mCoefficientOfVariation (NULL)
  , mKurtosis (NULL)
  , mMean (NULL)
  , mMedian (NULL)
  , mMode (NULL)
  , mSkewness (NULL)
  , mStandardDeviation (NULL)
  , mVariance (NULL)
  , mConfidenceInterval (NULL)
  , mCredibleInterval (NULL)
  , mInterquartileRange (NULL)
  , mRange (NULL)
  , mDistribExternalParameters (distribns)
  , mElementName("uncertStatistics")
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for DistribUncertStatistics.
 */
DistribUncertStatistics::DistribUncertStatistics(const DistribUncertStatistics&
  orig)
  : DistribBase( orig )
  , mCoefficientOfVariation ( NULL )
  , mKurtosis ( NULL )
  , mMean ( NULL )
  , mMedian ( NULL )
  , mMode ( NULL )
  , mSkewness ( NULL )
  , mStandardDeviation ( NULL )
  , mVariance ( NULL )
  , mConfidenceInterval ( NULL )
  , mCredibleInterval ( NULL )
  , mInterquartileRange ( NULL )
  , mRange ( NULL )
  , mDistribExternalParameters ( orig.mDistribExternalParameters )
  , mElementName ( orig.mElementName )
{
  if (orig.mCoefficientOfVariation != NULL)
  {
    mCoefficientOfVariation = orig.mCoefficientOfVariation->clone();
  }

  if (orig.mKurtosis != NULL)
  {
    mKurtosis = orig.mKurtosis->clone();
  }

  if (orig.mMean != NULL)
  {
    mMean = orig.mMean->clone();
  }

  if (orig.mMedian != NULL)
  {
    mMedian = orig.mMedian->clone();
  }

  if (orig.mMode != NULL)
  {
    mMode = orig.mMode->clone();
  }

  if (orig.mSkewness != NULL)
  {
    mSkewness = orig.mSkewness->clone();
  }

  if (orig.mStandardDeviation != NULL)
  {
    mStandardDeviation = orig.mStandardDeviation->clone();
  }

  if (orig.mVariance != NULL)
  {
    mVariance = orig.mVariance->clone();
  }

  if (orig.mConfidenceInterval != NULL)
  {
    mConfidenceInterval = orig.mConfidenceInterval->clone();
  }

  if (orig.mCredibleInterval != NULL)
  {
    mCredibleInterval = orig.mCredibleInterval->clone();
  }

  if (orig.mInterquartileRange != NULL)
  {
    mInterquartileRange = orig.mInterquartileRange->clone();
  }

  if (orig.mRange != NULL)
  {
    mRange = orig.mRange->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for DistribUncertStatistics.
 */
DistribUncertStatistics&
DistribUncertStatistics::operator=(const DistribUncertStatistics& rhs)
{
  if (&rhs != this)
  {
    DistribBase::operator=(rhs);
    mDistribExternalParameters = rhs.mDistribExternalParameters;
    mElementName = rhs.mElementName;
    delete mCoefficientOfVariation;
    if (rhs.mCoefficientOfVariation != NULL)
    {
      mCoefficientOfVariation = rhs.mCoefficientOfVariation->clone();
    }
    else
    {
      mCoefficientOfVariation = NULL;
    }

    delete mKurtosis;
    if (rhs.mKurtosis != NULL)
    {
      mKurtosis = rhs.mKurtosis->clone();
    }
    else
    {
      mKurtosis = NULL;
    }

    delete mMean;
    if (rhs.mMean != NULL)
    {
      mMean = rhs.mMean->clone();
    }
    else
    {
      mMean = NULL;
    }

    delete mMedian;
    if (rhs.mMedian != NULL)
    {
      mMedian = rhs.mMedian->clone();
    }
    else
    {
      mMedian = NULL;
    }

    delete mMode;
    if (rhs.mMode != NULL)
    {
      mMode = rhs.mMode->clone();
    }
    else
    {
      mMode = NULL;
    }

    delete mSkewness;
    if (rhs.mSkewness != NULL)
    {
      mSkewness = rhs.mSkewness->clone();
    }
    else
    {
      mSkewness = NULL;
    }

    delete mStandardDeviation;
    if (rhs.mStandardDeviation != NULL)
    {
      mStandardDeviation = rhs.mStandardDeviation->clone();
    }
    else
    {
      mStandardDeviation = NULL;
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

    delete mConfidenceInterval;
    if (rhs.mConfidenceInterval != NULL)
    {
      mConfidenceInterval = rhs.mConfidenceInterval->clone();
    }
    else
    {
      mConfidenceInterval = NULL;
    }

    delete mCredibleInterval;
    if (rhs.mCredibleInterval != NULL)
    {
      mCredibleInterval = rhs.mCredibleInterval->clone();
    }
    else
    {
      mCredibleInterval = NULL;
    }

    delete mInterquartileRange;
    if (rhs.mInterquartileRange != NULL)
    {
      mInterquartileRange = rhs.mInterquartileRange->clone();
    }
    else
    {
      mInterquartileRange = NULL;
    }

    delete mRange;
    if (rhs.mRange != NULL)
    {
      mRange = rhs.mRange->clone();
    }
    else
    {
      mRange = NULL;
    }

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DistribUncertStatistics object.
 */
DistribUncertStatistics*
DistribUncertStatistics::clone() const
{
  return new DistribUncertStatistics(*this);
}


/*
 * Destructor for DistribUncertStatistics.
 */
DistribUncertStatistics::~DistribUncertStatistics()
{
  delete mCoefficientOfVariation;
  mCoefficientOfVariation = NULL;
  delete mKurtosis;
  mKurtosis = NULL;
  delete mMean;
  mMean = NULL;
  delete mMedian;
  mMedian = NULL;
  delete mMode;
  mMode = NULL;
  delete mSkewness;
  mSkewness = NULL;
  delete mStandardDeviation;
  mStandardDeviation = NULL;
  delete mVariance;
  mVariance = NULL;
  delete mConfidenceInterval;
  mConfidenceInterval = NULL;
  delete mCredibleInterval;
  mCredibleInterval = NULL;
  delete mInterquartileRange;
  mInterquartileRange = NULL;
  delete mRange;
  mRange = NULL;
}


/*
 * Returns the value of the "coefficientOfVariation" element of this
 * DistribUncertStatistics.
 */
const DistribUncertValue*
DistribUncertStatistics::getCoefficientOfVariation() const
{
  return mCoefficientOfVariation;
}


/*
 * Returns the value of the "coefficientOfVariation" element of this
 * DistribUncertStatistics.
 */
DistribUncertValue*
DistribUncertStatistics::getCoefficientOfVariation()
{
  return mCoefficientOfVariation;
}


/*
 * Returns the value of the "kurtosis" element of this DistribUncertStatistics.
 */
const DistribUncertValue*
DistribUncertStatistics::getKurtosis() const
{
  return mKurtosis;
}


/*
 * Returns the value of the "kurtosis" element of this DistribUncertStatistics.
 */
DistribUncertValue*
DistribUncertStatistics::getKurtosis()
{
  return mKurtosis;
}


/*
 * Returns the value of the "mean" element of this DistribUncertStatistics.
 */
const DistribUncertValue*
DistribUncertStatistics::getMean() const
{
  return mMean;
}


/*
 * Returns the value of the "mean" element of this DistribUncertStatistics.
 */
DistribUncertValue*
DistribUncertStatistics::getMean()
{
  return mMean;
}


/*
 * Returns the value of the "median" element of this DistribUncertStatistics.
 */
const DistribUncertValue*
DistribUncertStatistics::getMedian() const
{
  return mMedian;
}


/*
 * Returns the value of the "median" element of this DistribUncertStatistics.
 */
DistribUncertValue*
DistribUncertStatistics::getMedian()
{
  return mMedian;
}


/*
 * Returns the value of the "mode" element of this DistribUncertStatistics.
 */
const DistribUncertValue*
DistribUncertStatistics::getMode() const
{
  return mMode;
}


/*
 * Returns the value of the "mode" element of this DistribUncertStatistics.
 */
DistribUncertValue*
DistribUncertStatistics::getMode()
{
  return mMode;
}


/*
 * Returns the value of the "skewness" element of this DistribUncertStatistics.
 */
const DistribUncertValue*
DistribUncertStatistics::getSkewness() const
{
  return mSkewness;
}


/*
 * Returns the value of the "skewness" element of this DistribUncertStatistics.
 */
DistribUncertValue*
DistribUncertStatistics::getSkewness()
{
  return mSkewness;
}


/*
 * Returns the value of the "standardDeviation" element of this
 * DistribUncertStatistics.
 */
const DistribUncertValue*
DistribUncertStatistics::getStandardDeviation() const
{
  return mStandardDeviation;
}


/*
 * Returns the value of the "standardDeviation" element of this
 * DistribUncertStatistics.
 */
DistribUncertValue*
DistribUncertStatistics::getStandardDeviation()
{
  return mStandardDeviation;
}


/*
 * Returns the value of the "variance" element of this DistribUncertStatistics.
 */
const DistribUncertValue*
DistribUncertStatistics::getVariance() const
{
  return mVariance;
}


/*
 * Returns the value of the "variance" element of this DistribUncertStatistics.
 */
DistribUncertValue*
DistribUncertStatistics::getVariance()
{
  return mVariance;
}


/*
 * Returns the value of the "confidenceInterval" element of this
 * DistribUncertStatistics.
 */
const DistribUncertStatisticSpan*
DistribUncertStatistics::getConfidenceInterval() const
{
  return mConfidenceInterval;
}


/*
 * Returns the value of the "confidenceInterval" element of this
 * DistribUncertStatistics.
 */
DistribUncertStatisticSpan*
DistribUncertStatistics::getConfidenceInterval()
{
  return mConfidenceInterval;
}


/*
 * Returns the value of the "credibleInterval" element of this
 * DistribUncertStatistics.
 */
const DistribUncertStatisticSpan*
DistribUncertStatistics::getCredibleInterval() const
{
  return mCredibleInterval;
}


/*
 * Returns the value of the "credibleInterval" element of this
 * DistribUncertStatistics.
 */
DistribUncertStatisticSpan*
DistribUncertStatistics::getCredibleInterval()
{
  return mCredibleInterval;
}


/*
 * Returns the value of the "interquartileRange" element of this
 * DistribUncertStatistics.
 */
const DistribUncertStatisticSpan*
DistribUncertStatistics::getInterquartileRange() const
{
  return mInterquartileRange;
}


/*
 * Returns the value of the "interquartileRange" element of this
 * DistribUncertStatistics.
 */
DistribUncertStatisticSpan*
DistribUncertStatistics::getInterquartileRange()
{
  return mInterquartileRange;
}


/*
 * Returns the value of the "range" element of this DistribUncertStatistics.
 */
const DistribUncertStatisticSpan*
DistribUncertStatistics::getRange() const
{
  return mRange;
}


/*
 * Returns the value of the "range" element of this DistribUncertStatistics.
 */
DistribUncertStatisticSpan*
DistribUncertStatistics::getRange()
{
  return mRange;
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's
 * "coefficientOfVariation" element is set.
 */
bool
DistribUncertStatistics::isSetCoefficientOfVariation() const
{
  return (mCoefficientOfVariation != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's "kurtosis"
 * element is set.
 */
bool
DistribUncertStatistics::isSetKurtosis() const
{
  return (mKurtosis != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's "mean" element
 * is set.
 */
bool
DistribUncertStatistics::isSetMean() const
{
  return (mMean != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's "median"
 * element is set.
 */
bool
DistribUncertStatistics::isSetMedian() const
{
  return (mMedian != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's "mode" element
 * is set.
 */
bool
DistribUncertStatistics::isSetMode() const
{
  return (mMode != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's "skewness"
 * element is set.
 */
bool
DistribUncertStatistics::isSetSkewness() const
{
  return (mSkewness != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's
 * "standardDeviation" element is set.
 */
bool
DistribUncertStatistics::isSetStandardDeviation() const
{
  return (mStandardDeviation != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's "variance"
 * element is set.
 */
bool
DistribUncertStatistics::isSetVariance() const
{
  return (mVariance != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's
 * "confidenceInterval" element is set.
 */
bool
DistribUncertStatistics::isSetConfidenceInterval() const
{
  return (mConfidenceInterval != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's
 * "credibleInterval" element is set.
 */
bool
DistribUncertStatistics::isSetCredibleInterval() const
{
  return (mCredibleInterval != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's
 * "interquartileRange" element is set.
 */
bool
DistribUncertStatistics::isSetInterquartileRange() const
{
  return (mInterquartileRange != NULL);
}


/*
 * Predicate returning @c true if this DistribUncertStatistics's "range"
 * element is set.
 */
bool
DistribUncertStatistics::isSetRange() const
{
  return (mRange != NULL);
}


/*
 * Sets the value of the "coefficientOfVariation" element of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::setCoefficientOfVariation(const DistribUncertValue*
  coefficientOfVariation)
{
  if (coefficientOfVariation == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (coefficientOfVariation->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != coefficientOfVariation->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != coefficientOfVariation->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != coefficientOfVariation->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mCoefficientOfVariation;
    mCoefficientOfVariation = (coefficientOfVariation != NULL) ?
      static_cast<DistribUncertValue*>(coefficientOfVariation->clone()) : NULL;
    if (mCoefficientOfVariation != NULL)
      mCoefficientOfVariation->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "kurtosis" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::setKurtosis(const DistribUncertValue* kurtosis)
{
  if (kurtosis == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (kurtosis->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != kurtosis->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != kurtosis->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != kurtosis->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mKurtosis;
    mKurtosis = (kurtosis != NULL) ?
      static_cast<DistribUncertValue*>(kurtosis->clone()) : NULL;
    if (mKurtosis != NULL) mKurtosis->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "mean" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::setMean(const DistribUncertValue* mean)
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
 * Sets the value of the "median" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::setMedian(const DistribUncertValue* median)
{
  if (median == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (median->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != median->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != median->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != median->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mMedian;
    mMedian = (median != NULL) ?
      static_cast<DistribUncertValue*>(median->clone()) : NULL;
    if (mMedian != NULL) mMedian->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "mode" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::setMode(const DistribUncertValue* mode)
{
  if (mode == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (mode->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != mode->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != mode->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != mode->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mMode;
    mMode = (mode != NULL) ? static_cast<DistribUncertValue*>(mode->clone()) :
      NULL;
    if (mMode != NULL) mMode->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "skewness" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::setSkewness(const DistribUncertValue* skewness)
{
  if (skewness == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (skewness->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != skewness->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != skewness->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != skewness->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mSkewness;
    mSkewness = (skewness != NULL) ?
      static_cast<DistribUncertValue*>(skewness->clone()) : NULL;
    if (mSkewness != NULL) mSkewness->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "standardDeviation" element of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::setStandardDeviation(const DistribUncertValue*
  standardDeviation)
{
  if (standardDeviation == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (standardDeviation->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != standardDeviation->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != standardDeviation->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != standardDeviation->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mStandardDeviation;
    mStandardDeviation = (standardDeviation != NULL) ?
      static_cast<DistribUncertValue*>(standardDeviation->clone()) : NULL;
    if (mStandardDeviation != NULL) mStandardDeviation->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "variance" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::setVariance(const DistribUncertValue* variance)
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
 * Sets the value of the "confidenceInterval" element of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::setConfidenceInterval(const
  DistribUncertStatisticSpan* confidenceInterval)
{
  if (confidenceInterval == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (confidenceInterval->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != confidenceInterval->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != confidenceInterval->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != confidenceInterval->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mConfidenceInterval;
    mConfidenceInterval = (confidenceInterval != NULL) ?
      static_cast<DistribUncertStatisticSpan*>(confidenceInterval->clone()) :
        NULL;
    if (mConfidenceInterval != NULL)
      mConfidenceInterval->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "credibleInterval" element of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::setCredibleInterval(const DistribUncertStatisticSpan*
  credibleInterval)
{
  if (credibleInterval == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (credibleInterval->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != credibleInterval->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != credibleInterval->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != credibleInterval->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mCredibleInterval;
    mCredibleInterval = (credibleInterval != NULL) ?
      static_cast<DistribUncertStatisticSpan*>(credibleInterval->clone()) : NULL;
    if (mCredibleInterval != NULL) mCredibleInterval->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "interquartileRange" element of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::setInterquartileRange(const
  DistribUncertStatisticSpan* interquartileRange)
{
  if (interquartileRange == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (interquartileRange->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != interquartileRange->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != interquartileRange->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != interquartileRange->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mInterquartileRange;
    mInterquartileRange = (interquartileRange != NULL) ?
      static_cast<DistribUncertStatisticSpan*>(interquartileRange->clone()) :
        NULL;
    if (mInterquartileRange != NULL)
      mInterquartileRange->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "range" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::setRange(const DistribUncertStatisticSpan* range)
{
  if (range == NULL)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (range->hasRequiredElements() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != range->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != range->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != range->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    delete mRange;
    mRange = (range != NULL) ?
      static_cast<DistribUncertStatisticSpan*>(range->clone()) : NULL;
    if (mRange != NULL) mRange->connectToParent(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribUncertStatistics object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribUncertStatistics::createCoefficientOfVariation()
{
  if (mCoefficientOfVariation != NULL)
  {
    delete mCoefficientOfVariation;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mCoefficientOfVariation = new DistribUncertValue(distribns);

  mCoefficientOfVariation->setElementName("coefficientOfVariation");

  delete distribns;

  connectToChild();

  return mCoefficientOfVariation;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribUncertStatistics object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribUncertStatistics::createKurtosis()
{
  if (mKurtosis != NULL)
  {
    delete mKurtosis;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mKurtosis = new DistribUncertValue(distribns);

  mKurtosis->setElementName("kurtosis");

  delete distribns;

  connectToChild();

  return mKurtosis;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribUncertStatistics object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribUncertStatistics::createMean()
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
 * DistribUncertStatistics object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribUncertStatistics::createMedian()
{
  if (mMedian != NULL)
  {
    delete mMedian;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mMedian = new DistribUncertValue(distribns);

  mMedian->setElementName("median");

  delete distribns;

  connectToChild();

  return mMedian;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribUncertStatistics object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribUncertStatistics::createMode()
{
  if (mMode != NULL)
  {
    delete mMode;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mMode = new DistribUncertValue(distribns);

  mMode->setElementName("mode");

  delete distribns;

  connectToChild();

  return mMode;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribUncertStatistics object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribUncertStatistics::createSkewness()
{
  if (mSkewness != NULL)
  {
    delete mSkewness;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mSkewness = new DistribUncertValue(distribns);

  mSkewness->setElementName("skewness");

  delete distribns;

  connectToChild();

  return mSkewness;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribUncertStatistics object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribUncertStatistics::createStandardDeviation()
{
  if (mStandardDeviation != NULL)
  {
    delete mStandardDeviation;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mStandardDeviation = new DistribUncertValue(distribns);

  mStandardDeviation->setElementName("standardDeviation");

  delete distribns;

  connectToChild();

  return mStandardDeviation;
}


/*
 * Creates a new DistribUncertValue object, adds it to this
 * DistribUncertStatistics object and returns the DistribUncertValue object
 * created.
 */
DistribUncertValue*
DistribUncertStatistics::createVariance()
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
 * Creates a new DistribUncertStatisticSpan object, adds it to this
 * DistribUncertStatistics object and returns the DistribUncertStatisticSpan
 * object created.
 */
DistribUncertStatisticSpan*
DistribUncertStatistics::createConfidenceInterval()
{
  if (mConfidenceInterval != NULL)
  {
    delete mConfidenceInterval;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mConfidenceInterval = new DistribUncertStatisticSpan(distribns);

  mConfidenceInterval->setElementName("confidenceInterval");

  delete distribns;

  connectToChild();

  return mConfidenceInterval;
}


/*
 * Creates a new DistribUncertStatisticSpan object, adds it to this
 * DistribUncertStatistics object and returns the DistribUncertStatisticSpan
 * object created.
 */
DistribUncertStatisticSpan*
DistribUncertStatistics::createCredibleInterval()
{
  if (mCredibleInterval != NULL)
  {
    delete mCredibleInterval;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mCredibleInterval = new DistribUncertStatisticSpan(distribns);

  mCredibleInterval->setElementName("credibleInterval");

  delete distribns;

  connectToChild();

  return mCredibleInterval;
}


/*
 * Creates a new DistribUncertStatisticSpan object, adds it to this
 * DistribUncertStatistics object and returns the DistribUncertStatisticSpan
 * object created.
 */
DistribUncertStatisticSpan*
DistribUncertStatistics::createInterquartileRange()
{
  if (mInterquartileRange != NULL)
  {
    delete mInterquartileRange;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mInterquartileRange = new DistribUncertStatisticSpan(distribns);

  mInterquartileRange->setElementName("interquartileRange");

  delete distribns;

  connectToChild();

  return mInterquartileRange;
}


/*
 * Creates a new DistribUncertStatisticSpan object, adds it to this
 * DistribUncertStatistics object and returns the DistribUncertStatisticSpan
 * object created.
 */
DistribUncertStatisticSpan*
DistribUncertStatistics::createRange()
{
  if (mRange != NULL)
  {
    delete mRange;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mRange = new DistribUncertStatisticSpan(distribns);

  mRange->setElementName("range");

  delete distribns;

  connectToChild();

  return mRange;
}


/*
 * Unsets the value of the "coefficientOfVariation" element of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetCoefficientOfVariation()
{
  delete mCoefficientOfVariation;
  mCoefficientOfVariation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "kurtosis" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetKurtosis()
{
  delete mKurtosis;
  mKurtosis = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "mean" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetMean()
{
  delete mMean;
  mMean = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "median" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetMedian()
{
  delete mMedian;
  mMedian = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "mode" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetMode()
{
  delete mMode;
  mMode = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "skewness" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetSkewness()
{
  delete mSkewness;
  mSkewness = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "standardDeviation" element of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetStandardDeviation()
{
  delete mStandardDeviation;
  mStandardDeviation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "variance" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetVariance()
{
  delete mVariance;
  mVariance = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "confidenceInterval" element of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetConfidenceInterval()
{
  delete mConfidenceInterval;
  mConfidenceInterval = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "credibleInterval" element of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetCredibleInterval()
{
  delete mCredibleInterval;
  mCredibleInterval = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "interquartileRange" element of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetInterquartileRange()
{
  delete mInterquartileRange;
  mInterquartileRange = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "range" element of this DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetRange()
{
  delete mRange;
  mRange = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the ListOfDistribExternalParameters from this
 * DistribUncertStatistics.
 */
const ListOfDistribExternalParameters*
DistribUncertStatistics::getListOfDistribExternalParameters() const
{
  return &mDistribExternalParameters;
}


/*
 * Returns the ListOfDistribExternalParameters from this
 * DistribUncertStatistics.
 */
ListOfDistribExternalParameters*
DistribUncertStatistics::getListOfDistribExternalParameters()
{
  return &mDistribExternalParameters;
}


/*
 * Get a DistribExternalParameter from the DistribUncertStatistics.
 */
DistribExternalParameter*
DistribUncertStatistics::getDistribExternalParameter(unsigned int n)
{
  return mDistribExternalParameters.get(n);
}


/*
 * Get a DistribExternalParameter from the DistribUncertStatistics.
 */
const DistribExternalParameter*
DistribUncertStatistics::getDistribExternalParameter(unsigned int n) const
{
  return mDistribExternalParameters.get(n);
}


/*
 * Adds a copy of the given DistribExternalParameter to this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::addDistribExternalParameter(const
  DistribExternalParameter* dep)
{
  if (dep == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (dep->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != dep->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != dep->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(dep)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mDistribExternalParameters.append(dep);
  }
}


/*
 * Get the number of DistribExternalParameter objects in this
 * DistribUncertStatistics.
 */
unsigned int
DistribUncertStatistics::getNumDistribExternalParameters() const
{
  return mDistribExternalParameters.size();
}


/*
 * Creates a new DistribExternalParameter object, adds it to this
 * DistribUncertStatistics object and returns the DistribExternalParameter
 * object created.
 */
DistribExternalParameter*
DistribUncertStatistics::createDistribExternalParameter()
{
  DistribExternalParameter* dep = NULL;

  try
  {
    DISTRIB_CREATE_NS_WITH_VERSION(distribns, getSBMLNamespaces(),
      getPackageVersion());
    dep = new DistribExternalParameter(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (dep != NULL)
  {
    mDistribExternalParameters.appendAndOwn(dep);
  }

  return dep;
}


/*
 * Removes the nth DistribExternalParameter from this DistribUncertStatistics
 * and returns a pointer to it.
 */
DistribExternalParameter*
DistribUncertStatistics::removeDistribExternalParameter(unsigned int n)
{
  return mDistribExternalParameters.remove(n);
}


/*
 * Returns the XML element name of this DistribUncertStatistics object.
 */
const std::string&
DistribUncertStatistics::getElementName() const
{
  return mElementName;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this DistribUncertStatistics object.
 */
void
DistribUncertStatistics::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this DistribUncertStatistics object.
 */
int
DistribUncertStatistics::getTypeCode() const
{
  return SBML_DISTRIB_UNCERTSTATISTICS;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DistribUncertStatistics object have been set.
 */
bool
DistribUncertStatistics::hasRequiredAttributes() const
{
  bool allPresent = DistribBase::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * DistribUncertStatistics object have been set.
 */
bool
DistribUncertStatistics::hasRequiredElements() const
{
  bool allPresent = DistribBase::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DistribUncertStatistics::writeElements(XMLOutputStream& stream) const
{
  DistribBase::writeElements(stream);

  if (isSetCoefficientOfVariation() == true)
  {
    mCoefficientOfVariation->write(stream);
  }

  if (isSetKurtosis() == true)
  {
    mKurtosis->write(stream);
  }

  if (isSetMean() == true)
  {
    mMean->write(stream);
  }

  if (isSetMedian() == true)
  {
    mMedian->write(stream);
  }

  if (isSetMode() == true)
  {
    mMode->write(stream);
  }

  if (isSetSkewness() == true)
  {
    mSkewness->write(stream);
  }

  if (isSetStandardDeviation() == true)
  {
    mStandardDeviation->write(stream);
  }

  if (isSetVariance() == true)
  {
    mVariance->write(stream);
  }

  if (isSetConfidenceInterval() == true)
  {
    mConfidenceInterval->write(stream);
  }

  if (isSetCredibleInterval() == true)
  {
    mCredibleInterval->write(stream);
  }

  if (isSetInterquartileRange() == true)
  {
    mInterquartileRange->write(stream);
  }

  if (isSetRange() == true)
  {
    mRange->write(stream);
  }

  if (getNumDistribExternalParameters() > 0)
  {
    mDistribExternalParameters.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DistribUncertStatistics::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  if (mCoefficientOfVariation != NULL)
  {
    mCoefficientOfVariation->accept(v);
  }

  if (mKurtosis != NULL)
  {
    mKurtosis->accept(v);
  }

  if (mMean != NULL)
  {
    mMean->accept(v);
  }

  if (mMedian != NULL)
  {
    mMedian->accept(v);
  }

  if (mMode != NULL)
  {
    mMode->accept(v);
  }

  if (mSkewness != NULL)
  {
    mSkewness->accept(v);
  }

  if (mStandardDeviation != NULL)
  {
    mStandardDeviation->accept(v);
  }

  if (mVariance != NULL)
  {
    mVariance->accept(v);
  }

  if (mConfidenceInterval != NULL)
  {
    mConfidenceInterval->accept(v);
  }

  if (mCredibleInterval != NULL)
  {
    mCredibleInterval->accept(v);
  }

  if (mInterquartileRange != NULL)
  {
    mInterquartileRange->accept(v);
  }

  if (mRange != NULL)
  {
    mRange->accept(v);
  }

  mDistribExternalParameters.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DistribUncertStatistics::setSBMLDocument(SBMLDocument* d)
{
  DistribBase::setSBMLDocument(d);

  if (mCoefficientOfVariation != NULL)
  {
    mCoefficientOfVariation->setSBMLDocument(d);
  }

  if (mKurtosis != NULL)
  {
    mKurtosis->setSBMLDocument(d);
  }

  if (mMean != NULL)
  {
    mMean->setSBMLDocument(d);
  }

  if (mMedian != NULL)
  {
    mMedian->setSBMLDocument(d);
  }

  if (mMode != NULL)
  {
    mMode->setSBMLDocument(d);
  }

  if (mSkewness != NULL)
  {
    mSkewness->setSBMLDocument(d);
  }

  if (mStandardDeviation != NULL)
  {
    mStandardDeviation->setSBMLDocument(d);
  }

  if (mVariance != NULL)
  {
    mVariance->setSBMLDocument(d);
  }

  if (mConfidenceInterval != NULL)
  {
    mConfidenceInterval->setSBMLDocument(d);
  }

  if (mCredibleInterval != NULL)
  {
    mCredibleInterval->setSBMLDocument(d);
  }

  if (mInterquartileRange != NULL)
  {
    mInterquartileRange->setSBMLDocument(d);
  }

  if (mRange != NULL)
  {
    mRange->setSBMLDocument(d);
  }

  mDistribExternalParameters.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
DistribUncertStatistics::connectToChild()
{
  DistribBase::connectToChild();

  if (mCoefficientOfVariation != NULL)
  {
    mCoefficientOfVariation->connectToParent(this);
  }

  if (mKurtosis != NULL)
  {
    mKurtosis->connectToParent(this);
  }

  if (mMean != NULL)
  {
    mMean->connectToParent(this);
  }

  if (mMedian != NULL)
  {
    mMedian->connectToParent(this);
  }

  if (mMode != NULL)
  {
    mMode->connectToParent(this);
  }

  if (mSkewness != NULL)
  {
    mSkewness->connectToParent(this);
  }

  if (mStandardDeviation != NULL)
  {
    mStandardDeviation->connectToParent(this);
  }

  if (mVariance != NULL)
  {
    mVariance->connectToParent(this);
  }

  if (mConfidenceInterval != NULL)
  {
    mConfidenceInterval->connectToParent(this);
  }

  if (mCredibleInterval != NULL)
  {
    mCredibleInterval->connectToParent(this);
  }

  if (mInterquartileRange != NULL)
  {
    mInterquartileRange->connectToParent(this);
  }

  if (mRange != NULL)
  {
    mRange->connectToParent(this);
  }

  mDistribExternalParameters.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DistribUncertStatistics::enablePackageInternal(const std::string& pkgURI,
                                               const std::string& pkgPrefix,
                                               bool flag)
{
  DistribBase::enablePackageInternal(pkgURI, pkgPrefix, flag);

  if (isSetCoefficientOfVariation())
  {
    mCoefficientOfVariation->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetKurtosis())
  {
    mKurtosis->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetMean())
  {
    mMean->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetMedian())
  {
    mMedian->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetMode())
  {
    mMode->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetSkewness())
  {
    mSkewness->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetStandardDeviation())
  {
    mStandardDeviation->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetVariance())
  {
    mVariance->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetConfidenceInterval())
  {
    mConfidenceInterval->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetCredibleInterval())
  {
    mCredibleInterval->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetInterquartileRange())
  {
    mInterquartileRange->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetRange())
  {
    mRange->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  mDistribExternalParameters.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
DistribUncertStatistics::updateSBMLNamespace(const std::string& package,
                                             unsigned int level,
                                             unsigned int version)
{
  DistribBase::updateSBMLNamespace(package, level, version);

  if (mCoefficientOfVariation != NULL)
  {
    mCoefficientOfVariation->updateSBMLNamespace(package, level, version);
  }

  if (mKurtosis != NULL)
  {
    mKurtosis->updateSBMLNamespace(package, level, version);
  }

  if (mMean != NULL)
  {
    mMean->updateSBMLNamespace(package, level, version);
  }

  if (mMedian != NULL)
  {
    mMedian->updateSBMLNamespace(package, level, version);
  }

  if (mMode != NULL)
  {
    mMode->updateSBMLNamespace(package, level, version);
  }

  if (mSkewness != NULL)
  {
    mSkewness->updateSBMLNamespace(package, level, version);
  }

  if (mStandardDeviation != NULL)
  {
    mStandardDeviation->updateSBMLNamespace(package, level, version);
  }

  if (mVariance != NULL)
  {
    mVariance->updateSBMLNamespace(package, level, version);
  }

  if (mConfidenceInterval != NULL)
  {
    mConfidenceInterval->updateSBMLNamespace(package, level, version);
  }

  if (mCredibleInterval != NULL)
  {
    mCredibleInterval->updateSBMLNamespace(package, level, version);
  }

  if (mInterquartileRange != NULL)
  {
    mInterquartileRange->updateSBMLNamespace(package, level, version);
  }

  if (mRange != NULL)
  {
    mRange->updateSBMLNamespace(package, level, version);
  }

  mDistribExternalParameters.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::getAttribute(const std::string& attributeName,
                                      bool& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::getAttribute(const std::string& attributeName,
                                      int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::getAttribute(const std::string& attributeName,
                                      double& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::getAttribute(const std::string& attributeName,
                                      unsigned int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::getAttribute(const std::string& attributeName,
                                      std::string& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DistribUncertStatistics's attribute
 * "attributeName" is set.
 */
bool
DistribUncertStatistics::isSetAttribute(const std::string& attributeName) const
{
  bool value = DistribBase::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::setAttribute(const std::string& attributeName,
                                      bool value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::setAttribute(const std::string& attributeName,
                                      int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::setAttribute(const std::string& attributeName,
                                      double value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::setAttribute(const std::string& attributeName,
                                      unsigned int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::setAttribute(const std::string& attributeName,
                                      const std::string& value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * DistribUncertStatistics.
 */
int
DistribUncertStatistics::unsetAttribute(const std::string& attributeName)
{
  int value = DistribBase::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * DistribUncertStatistics.
 */
SBase*
DistribUncertStatistics::createChildObject(const std::string& elementName)
{
  DistribBase* obj = NULL;

  if (elementName == "coefficientOfVariation")
  {
    return createCoefficientOfVariation();
  }
  else if (elementName == "kurtosis")
  {
    return createKurtosis();
  }
  else if (elementName == "mean")
  {
    return createMean();
  }
  else if (elementName == "median")
  {
    return createMedian();
  }
  else if (elementName == "mode")
  {
    return createMode();
  }
  else if (elementName == "skewness")
  {
    return createSkewness();
  }
  else if (elementName == "standardDeviation")
  {
    return createStandardDeviation();
  }
  else if (elementName == "variance")
  {
    return createVariance();
  }
  else if (elementName == "confidenceInterval")
  {
    return createConfidenceInterval();
  }
  else if (elementName == "credibleInterval")
  {
    return createCredibleInterval();
  }
  else if (elementName == "interquartileRange")
  {
    return createInterquartileRange();
  }
  else if (elementName == "range")
  {
    return createRange();
  }
  else if (elementName == "externalParameter")
  {
    return createDistribExternalParameter();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this DistribUncertStatistics.
 */
int
DistribUncertStatistics::addChildObject(const std::string& elementName,
                                        const SBase* element)
{
  if (elementName == "coefficientOfVariation" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setCoefficientOfVariation((const DistribUncertValue*)(element));
  }
  else if (elementName == "kurtosis" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setKurtosis((const DistribUncertValue*)(element));
  }
  else if (elementName == "mean" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setMean((const DistribUncertValue*)(element));
  }
  else if (elementName == "median" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setMedian((const DistribUncertValue*)(element));
  }
  else if (elementName == "mode" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setMode((const DistribUncertValue*)(element));
  }
  else if (elementName == "skewness" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setSkewness((const DistribUncertValue*)(element));
  }
  else if (elementName == "standardDeviation" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setStandardDeviation((const DistribUncertValue*)(element));
  }
  else if (elementName == "variance" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setVariance((const DistribUncertValue*)(element));
  }
  else if (elementName == "confidenceInterval" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTSTATISTICSPAN)
  {
    return setConfidenceInterval((const DistribUncertStatisticSpan*)(element));
  }
  else if (elementName == "credibleInterval" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTSTATISTICSPAN)
  {
    return setCredibleInterval((const DistribUncertStatisticSpan*)(element));
  }
  else if (elementName == "interquartileRange" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTSTATISTICSPAN)
  {
    return setInterquartileRange((const DistribUncertStatisticSpan*)(element));
  }
  else if (elementName == "range" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTSTATISTICSPAN)
  {
    return setRange((const DistribUncertStatisticSpan*)(element));
  }
  else if (elementName == "externalParameter" && element->getTypeCode()
    == SBML_DISTRIB_EXTERNALPARAMETER)
  {
    return addDistribExternalParameter((const
      DistribExternalParameter*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * DistribUncertStatistics.
 */
SBase*
DistribUncertStatistics::removeChildObject(const std::string& elementName,
                                           const std::string& id)
{
  if (elementName == "coefficientOfVariation")
  {
    DistribUncertValue * obj = getCoefficientOfVariation();
    if (unsetCoefficientOfVariation() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "kurtosis")
  {
    DistribUncertValue * obj = getKurtosis();
    if (unsetKurtosis() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "mean")
  {
    DistribUncertValue * obj = getMean();
    if (unsetMean() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "median")
  {
    DistribUncertValue * obj = getMedian();
    if (unsetMedian() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "mode")
  {
    DistribUncertValue * obj = getMode();
    if (unsetMode() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "skewness")
  {
    DistribUncertValue * obj = getSkewness();
    if (unsetSkewness() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "standardDeviation")
  {
    DistribUncertValue * obj = getStandardDeviation();
    if (unsetStandardDeviation() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "variance")
  {
    DistribUncertValue * obj = getVariance();
    if (unsetVariance() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "confidenceInterval")
  {
    DistribUncertStatisticSpan * obj = getConfidenceInterval();
    if (unsetConfidenceInterval() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "credibleInterval")
  {
    DistribUncertStatisticSpan * obj = getCredibleInterval();
    if (unsetCredibleInterval() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "interquartileRange")
  {
    DistribUncertStatisticSpan * obj = getInterquartileRange();
    if (unsetInterquartileRange() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "range")
  {
    DistribUncertStatisticSpan * obj = getRange();
    if (unsetRange() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "externalParameter")
  {
    for (unsigned int i = 0; i < getNumDistribExternalParameters(); i++)
    {
      if (getDistribExternalParameter(i)->getId() == id)
      {
        return removeDistribExternalParameter(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this DistribUncertStatistics.
 */
unsigned int
DistribUncertStatistics::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "coefficientOfVariation")
  {
    if (isSetCoefficientOfVariation())
    {
      return 1;
    }
  }
  else if (elementName == "kurtosis")
  {
    if (isSetKurtosis())
    {
      return 1;
    }
  }
  else if (elementName == "mean")
  {
    if (isSetMean())
    {
      return 1;
    }
  }
  else if (elementName == "median")
  {
    if (isSetMedian())
    {
      return 1;
    }
  }
  else if (elementName == "mode")
  {
    if (isSetMode())
    {
      return 1;
    }
  }
  else if (elementName == "skewness")
  {
    if (isSetSkewness())
    {
      return 1;
    }
  }
  else if (elementName == "standardDeviation")
  {
    if (isSetStandardDeviation())
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
  else if (elementName == "confidenceInterval")
  {
    if (isSetConfidenceInterval())
    {
      return 1;
    }
  }
  else if (elementName == "credibleInterval")
  {
    if (isSetCredibleInterval())
    {
      return 1;
    }
  }
  else if (elementName == "interquartileRange")
  {
    if (isSetInterquartileRange())
    {
      return 1;
    }
  }
  else if (elementName == "range")
  {
    if (isSetRange())
    {
      return 1;
    }
  }
  else if (elementName == "externalParameter")
  {
    return getNumDistribExternalParameters();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this DistribUncertStatistics.
 */
SBase*
DistribUncertStatistics::getObject(const std::string& elementName,
                                   unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "coefficientOfVariation")
  {
    return getCoefficientOfVariation();
  }
  else if (elementName == "kurtosis")
  {
    return getKurtosis();
  }
  else if (elementName == "mean")
  {
    return getMean();
  }
  else if (elementName == "median")
  {
    return getMedian();
  }
  else if (elementName == "mode")
  {
    return getMode();
  }
  else if (elementName == "skewness")
  {
    return getSkewness();
  }
  else if (elementName == "standardDeviation")
  {
    return getStandardDeviation();
  }
  else if (elementName == "variance")
  {
    return getVariance();
  }
  else if (elementName == "confidenceInterval")
  {
    return getConfidenceInterval();
  }
  else if (elementName == "credibleInterval")
  {
    return getCredibleInterval();
  }
  else if (elementName == "interquartileRange")
  {
    return getInterquartileRange();
  }
  else if (elementName == "range")
  {
    return getRange();
  }
  else if (elementName == "externalParameter")
  {
    return getDistribExternalParameter(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
DistribUncertStatistics::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mCoefficientOfVariation != NULL)
  {
    if (mCoefficientOfVariation->getId() == id)
    {
      return mCoefficientOfVariation;
    }

    obj = mCoefficientOfVariation->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mKurtosis != NULL)
  {
    if (mKurtosis->getId() == id)
    {
      return mKurtosis;
    }

    obj = mKurtosis->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

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

  if (mMedian != NULL)
  {
    if (mMedian->getId() == id)
    {
      return mMedian;
    }

    obj = mMedian->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mMode != NULL)
  {
    if (mMode->getId() == id)
    {
      return mMode;
    }

    obj = mMode->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mSkewness != NULL)
  {
    if (mSkewness->getId() == id)
    {
      return mSkewness;
    }

    obj = mSkewness->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mStandardDeviation != NULL)
  {
    if (mStandardDeviation->getId() == id)
    {
      return mStandardDeviation;
    }

    obj = mStandardDeviation->getElementBySId(id);
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

  if (mConfidenceInterval != NULL)
  {
    if (mConfidenceInterval->getId() == id)
    {
      return mConfidenceInterval;
    }

    obj = mConfidenceInterval->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mCredibleInterval != NULL)
  {
    if (mCredibleInterval->getId() == id)
    {
      return mCredibleInterval;
    }

    obj = mCredibleInterval->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mInterquartileRange != NULL)
  {
    if (mInterquartileRange->getId() == id)
    {
      return mInterquartileRange;
    }

    obj = mInterquartileRange->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mRange != NULL)
  {
    if (mRange->getId() == id)
    {
      return mRange;
    }

    obj = mRange->getElementBySId(id);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mDistribExternalParameters.getId() == id)
  {
    return &mDistribExternalParameters;
  }

  obj = mDistribExternalParameters.getElementBySId(id);

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
DistribUncertStatistics::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mCoefficientOfVariation != NULL)
  {
    if (mCoefficientOfVariation->getMetaId() == metaid)
    {
      return mCoefficientOfVariation;
    }

    obj = mCoefficientOfVariation->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mKurtosis != NULL)
  {
    if (mKurtosis->getMetaId() == metaid)
    {
      return mKurtosis;
    }

    obj = mKurtosis->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

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

  if (mMedian != NULL)
  {
    if (mMedian->getMetaId() == metaid)
    {
      return mMedian;
    }

    obj = mMedian->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mMode != NULL)
  {
    if (mMode->getMetaId() == metaid)
    {
      return mMode;
    }

    obj = mMode->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mSkewness != NULL)
  {
    if (mSkewness->getMetaId() == metaid)
    {
      return mSkewness;
    }

    obj = mSkewness->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mStandardDeviation != NULL)
  {
    if (mStandardDeviation->getMetaId() == metaid)
    {
      return mStandardDeviation;
    }

    obj = mStandardDeviation->getElementByMetaId(metaid);
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

  if (mConfidenceInterval != NULL)
  {
    if (mConfidenceInterval->getMetaId() == metaid)
    {
      return mConfidenceInterval;
    }

    obj = mConfidenceInterval->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mCredibleInterval != NULL)
  {
    if (mCredibleInterval->getMetaId() == metaid)
    {
      return mCredibleInterval;
    }

    obj = mCredibleInterval->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mInterquartileRange != NULL)
  {
    if (mInterquartileRange->getMetaId() == metaid)
    {
      return mInterquartileRange;
    }

    obj = mInterquartileRange->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mRange != NULL)
  {
    if (mRange->getMetaId() == metaid)
    {
      return mRange;
    }

    obj = mRange->getElementByMetaId(metaid);
    if (obj != NULL)
    {
      return obj;
    }
  }

  if (mDistribExternalParameters.getMetaId() == metaid)
  {
    return &mDistribExternalParameters;
  }

  obj = mDistribExternalParameters.getElementByMetaId(metaid);

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
DistribUncertStatistics::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mCoefficientOfVariation, filter);
  ADD_FILTERED_POINTER(ret, sublist, mKurtosis, filter);
  ADD_FILTERED_POINTER(ret, sublist, mMean, filter);
  ADD_FILTERED_POINTER(ret, sublist, mMedian, filter);
  ADD_FILTERED_POINTER(ret, sublist, mMode, filter);
  ADD_FILTERED_POINTER(ret, sublist, mSkewness, filter);
  ADD_FILTERED_POINTER(ret, sublist, mStandardDeviation, filter);
  ADD_FILTERED_POINTER(ret, sublist, mVariance, filter);
  ADD_FILTERED_POINTER(ret, sublist, mConfidenceInterval, filter);
  ADD_FILTERED_POINTER(ret, sublist, mCredibleInterval, filter);
  ADD_FILTERED_POINTER(ret, sublist, mInterquartileRange, filter);
  ADD_FILTERED_POINTER(ret, sublist, mRange, filter);

  ADD_FILTERED_LIST(ret, sublist, mDistribExternalParameters, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
DistribUncertStatistics::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribBase::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "coefficientOfVariation")
  {
    if (isSetCoefficientOfVariation())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mCoefficientOfVariation;
    mCoefficientOfVariation = new DistribUncertValue(distribns);
    mCoefficientOfVariation->setElementName(name);
    obj = mCoefficientOfVariation;
  }
  else if (name == "kurtosis")
  {
    if (isSetKurtosis())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mKurtosis;
    mKurtosis = new DistribUncertValue(distribns);
    mKurtosis->setElementName(name);
    obj = mKurtosis;
  }
  else if (name == "mean")
  {
    if (isSetMean())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mMean;
    mMean = new DistribUncertValue(distribns);
    mMean->setElementName(name);
    obj = mMean;
  }
  else if (name == "median")
  {
    if (isSetMedian())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mMedian;
    mMedian = new DistribUncertValue(distribns);
    mMedian->setElementName(name);
    obj = mMedian;
  }
  else if (name == "mode")
  {
    if (isSetMode())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mMode;
    mMode = new DistribUncertValue(distribns);
    mMode->setElementName(name);
    obj = mMode;
  }
  else if (name == "skewness")
  {
    if (isSetSkewness())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mSkewness;
    mSkewness = new DistribUncertValue(distribns);
    mSkewness->setElementName(name);
    obj = mSkewness;
  }
  else if (name == "standardDeviation")
  {
    if (isSetStandardDeviation())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mStandardDeviation;
    mStandardDeviation = new DistribUncertValue(distribns);
    mStandardDeviation->setElementName(name);
    obj = mStandardDeviation;
  }
  else if (name == "variance")
  {
    if (isSetVariance())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mVariance;
    mVariance = new DistribUncertValue(distribns);
    mVariance->setElementName(name);
    obj = mVariance;
  }
  else if (name == "confidenceInterval")
  {
    if (isSetConfidenceInterval())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mConfidenceInterval;
    mConfidenceInterval = new DistribUncertStatisticSpan(distribns);
    mConfidenceInterval->setElementName(name);
    obj = mConfidenceInterval;
  }
  else if (name == "credibleInterval")
  {
    if (isSetCredibleInterval())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mCredibleInterval;
    mCredibleInterval = new DistribUncertStatisticSpan(distribns);
    mCredibleInterval->setElementName(name);
    obj = mCredibleInterval;
  }
  else if (name == "interquartileRange")
  {
    if (isSetInterquartileRange())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mInterquartileRange;
    mInterquartileRange = new DistribUncertStatisticSpan(distribns);
    mInterquartileRange->setElementName(name);
    obj = mInterquartileRange;
  }
  else if (name == "range")
  {
    if (isSetRange())
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    delete mRange;
    mRange = new DistribUncertStatisticSpan(distribns);
    mRange->setElementName(name);
    obj = mRange;
  }
  else if (name == "listOfExternalParameters")
  {
    if (mDistribExternalParameters.size() != 0)
    {
      getErrorLog()->logPackageError("distrib",
        DistribDistribUncertStatisticsAllowedElements, getPackageVersion(),
          getLevel(), getVersion());
    }

    obj = &mDistribExternalParameters;
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
DistribUncertStatistics::addExpectedAttributes(ExpectedAttributes& attributes)
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
DistribUncertStatistics::readAttributes(const XMLAttributes& attributes,
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
          DistribDistribUncertStatisticsAllowedCoreAttributes, pkgVersion, level,
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
DistribUncertStatistics::readL3V1V1Attributes(const XMLAttributes& attributes)
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
DistribUncertStatistics::readL3V2V1Attributes(const XMLAttributes& attributes)
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
DistribUncertStatistics::writeAttributes(XMLOutputStream& stream) const
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
DistribUncertStatistics::writeL3V1V1Attributes(XMLOutputStream& stream) const
{
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DistribUncertStatistics::writeL3V2V1Attributes(XMLOutputStream& stream) const
{
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DistribUncertStatistics_t using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
DistribUncertStatistics_t *
DistribUncertStatistics_create(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
{
  return new DistribUncertStatistics(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DistribUncertStatistics_t object.
 */
LIBSBML_EXTERN
DistribUncertStatistics_t*
DistribUncertStatistics_clone(const DistribUncertStatistics_t* dus)
{
  if (dus != NULL)
  {
    return static_cast<DistribUncertStatistics_t*>(dus->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DistribUncertStatistics_t object.
 */
LIBSBML_EXTERN
void
DistribUncertStatistics_free(DistribUncertStatistics_t* dus)
{
  if (dus != NULL)
  {
    delete dus;
  }
}


/*
 * Returns the value of the "coefficientOfVariation" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getCoefficientOfVariation(const
  DistribUncertStatistics_t * dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->getCoefficientOfVariation());
}


/*
 * Returns the value of the "kurtosis" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getKurtosis(const DistribUncertStatistics_t * dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->getKurtosis());
}


/*
 * Returns the value of the "mean" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getMean(const DistribUncertStatistics_t * dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->getMean());
}


/*
 * Returns the value of the "median" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getMedian(const DistribUncertStatistics_t * dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->getMedian());
}


/*
 * Returns the value of the "mode" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getMode(const DistribUncertStatistics_t * dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->getMode());
}


/*
 * Returns the value of the "skewness" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getSkewness(const DistribUncertStatistics_t * dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->getSkewness());
}


/*
 * Returns the value of the "standardDeviation" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getStandardDeviation(const DistribUncertStatistics_t *
  dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->getStandardDeviation());
}


/*
 * Returns the value of the "variance" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getVariance(const DistribUncertStatistics_t * dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->getVariance());
}


/*
 * Returns the value of the "confidenceInterval" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertStatisticSpan_t*
DistribUncertStatistics_getConfidenceInterval(const DistribUncertStatistics_t *
  dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertStatisticSpan_t*)(dus->getConfidenceInterval());
}


/*
 * Returns the value of the "credibleInterval" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertStatisticSpan_t*
DistribUncertStatistics_getCredibleInterval(const DistribUncertStatistics_t *
  dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertStatisticSpan_t*)(dus->getCredibleInterval());
}


/*
 * Returns the value of the "interquartileRange" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertStatisticSpan_t*
DistribUncertStatistics_getInterquartileRange(const DistribUncertStatistics_t *
  dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertStatisticSpan_t*)(dus->getInterquartileRange());
}


/*
 * Returns the value of the "range" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
const DistribUncertStatisticSpan_t*
DistribUncertStatistics_getRange(const DistribUncertStatistics_t * dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertStatisticSpan_t*)(dus->getRange());
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "coefficientOfVariation" element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetCoefficientOfVariation(const
  DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetCoefficientOfVariation()) :
    0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "kurtosis" element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetKurtosis(const DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetKurtosis()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's "mean"
 * element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetMean(const DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetMean()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's "median"
 * element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetMedian(const DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetMedian()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's "mode"
 * element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetMode(const DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetMode()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "skewness" element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetSkewness(const DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetSkewness()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "standardDeviation" element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetStandardDeviation(const DistribUncertStatistics_t
  * dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetStandardDeviation()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "variance" element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetVariance(const DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetVariance()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "confidenceInterval" element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetConfidenceInterval(const DistribUncertStatistics_t
  * dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetConfidenceInterval()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "credibleInterval" element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetCredibleInterval(const DistribUncertStatistics_t *
  dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetCredibleInterval()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "interquartileRange" element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetInterquartileRange(const DistribUncertStatistics_t
  * dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetInterquartileRange()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's "range"
 * element is set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetRange(const DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? static_cast<int>(dus->isSetRange()) : 0;
}


/*
 * Sets the value of the "coefficientOfVariation" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setCoefficientOfVariation(
                                                  DistribUncertStatistics_t *
                                                    dus,
                                                  const DistribUncertValue_t*
                                                    coefficientOfVariation)
{
  return (dus != NULL) ? dus->setCoefficientOfVariation(coefficientOfVariation)
    : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "kurtosis" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setKurtosis(DistribUncertStatistics_t * dus,
                                    const DistribUncertValue_t* kurtosis)
{
  return (dus != NULL) ? dus->setKurtosis(kurtosis) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "mean" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setMean(DistribUncertStatistics_t * dus,
                                const DistribUncertValue_t* mean)
{
  return (dus != NULL) ? dus->setMean(mean) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "median" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setMedian(DistribUncertStatistics_t * dus,
                                  const DistribUncertValue_t* median)
{
  return (dus != NULL) ? dus->setMedian(median) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "mode" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setMode(DistribUncertStatistics_t * dus,
                                const DistribUncertValue_t* mode)
{
  return (dus != NULL) ? dus->setMode(mode) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "skewness" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setSkewness(DistribUncertStatistics_t * dus,
                                    const DistribUncertValue_t* skewness)
{
  return (dus != NULL) ? dus->setSkewness(skewness) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "standardDeviation" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setStandardDeviation(DistribUncertStatistics_t * dus,
                                             const DistribUncertValue_t*
                                               standardDeviation)
{
  return (dus != NULL) ? dus->setStandardDeviation(standardDeviation) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "variance" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setVariance(DistribUncertStatistics_t * dus,
                                    const DistribUncertValue_t* variance)
{
  return (dus != NULL) ? dus->setVariance(variance) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "confidenceInterval" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setConfidenceInterval(DistribUncertStatistics_t * dus,
                                              const
                                                DistribUncertStatisticSpan_t*
                                                  confidenceInterval)
{
  return (dus != NULL) ? dus->setConfidenceInterval(confidenceInterval) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "credibleInterval" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setCredibleInterval(DistribUncertStatistics_t * dus,
                                            const DistribUncertStatisticSpan_t*
                                              credibleInterval)
{
  return (dus != NULL) ? dus->setCredibleInterval(credibleInterval) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "interquartileRange" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setInterquartileRange(DistribUncertStatistics_t * dus,
                                              const
                                                DistribUncertStatisticSpan_t*
                                                  interquartileRange)
{
  return (dus != NULL) ? dus->setInterquartileRange(interquartileRange) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "range" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setRange(DistribUncertStatistics_t * dus,
                                 const DistribUncertStatisticSpan_t* range)
{
  return (dus != NULL) ? dus->setRange(range) : LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createCoefficientOfVariation(DistribUncertStatistics_t*
  dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->createCoefficientOfVariation());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createKurtosis(DistribUncertStatistics_t* dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->createKurtosis());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createMean(DistribUncertStatistics_t* dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->createMean());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createMedian(DistribUncertStatistics_t* dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->createMedian());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createMode(DistribUncertStatistics_t* dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->createMode());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createSkewness(DistribUncertStatistics_t* dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->createSkewness());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createStandardDeviation(DistribUncertStatistics_t* dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->createStandardDeviation());
}


/*
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createVariance(DistribUncertStatistics_t* dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertValue_t*)(dus->createVariance());
}


/*
 * Creates a new DistribUncertStatisticSpan_t object, adds it to this
 * DistribUncertStatistics_t object and returns the
 * DistribUncertStatisticSpan_t object created.
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t*
DistribUncertStatistics_createConfidenceInterval(DistribUncertStatistics_t*
  dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertStatisticSpan_t*)(dus->createConfidenceInterval());
}


/*
 * Creates a new DistribUncertStatisticSpan_t object, adds it to this
 * DistribUncertStatistics_t object and returns the
 * DistribUncertStatisticSpan_t object created.
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t*
DistribUncertStatistics_createCredibleInterval(DistribUncertStatistics_t* dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertStatisticSpan_t*)(dus->createCredibleInterval());
}


/*
 * Creates a new DistribUncertStatisticSpan_t object, adds it to this
 * DistribUncertStatistics_t object and returns the
 * DistribUncertStatisticSpan_t object created.
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t*
DistribUncertStatistics_createInterquartileRange(DistribUncertStatistics_t*
  dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertStatisticSpan_t*)(dus->createInterquartileRange());
}


/*
 * Creates a new DistribUncertStatisticSpan_t object, adds it to this
 * DistribUncertStatistics_t object and returns the
 * DistribUncertStatisticSpan_t object created.
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t*
DistribUncertStatistics_createRange(DistribUncertStatistics_t* dus)
{
  if (dus == NULL)
  {
    return NULL;
  }

  return (DistribUncertStatisticSpan_t*)(dus->createRange());
}


/*
 * Unsets the value of the "coefficientOfVariation" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetCoefficientOfVariation(DistribUncertStatistics_t *
  dus)
{
  return (dus != NULL) ? dus->unsetCoefficientOfVariation() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "kurtosis" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetKurtosis(DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? dus->unsetKurtosis() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "mean" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetMean(DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? dus->unsetMean() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "median" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetMedian(DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? dus->unsetMedian() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "mode" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetMode(DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? dus->unsetMode() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "skewness" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetSkewness(DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? dus->unsetSkewness() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "standardDeviation" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetStandardDeviation(DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? dus->unsetStandardDeviation() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "variance" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetVariance(DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? dus->unsetVariance() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "confidenceInterval" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetConfidenceInterval(DistribUncertStatistics_t *
  dus)
{
  return (dus != NULL) ? dus->unsetConfidenceInterval() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "credibleInterval" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetCredibleInterval(DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? dus->unsetCredibleInterval() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "interquartileRange" element of this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetInterquartileRange(DistribUncertStatistics_t *
  dus)
{
  return (dus != NULL) ? dus->unsetInterquartileRange() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "range" element of this DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetRange(DistribUncertStatistics_t * dus)
{
  return (dus != NULL) ? dus->unsetRange() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing DistribExternalParameter_t objects from this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
ListOf_t*
DistribUncertStatistics_getListOfDistribExternalParameters(DistribUncertStatistics_t*
  dus)
{
  return (dus != NULL) ? dus->getListOfDistribExternalParameters() : NULL;
}


/*
 * Get a DistribExternalParameter_t from the DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribUncertStatistics_getDistribExternalParameter(
                                                    DistribUncertStatistics_t*
                                                      dus,
                                                    unsigned int n)
{
  return (dus != NULL) ? dus->getDistribExternalParameter(n) : NULL;
}


/*
 * Adds a copy of the given DistribExternalParameter_t to this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_addDistribExternalParameter(
                                                    DistribUncertStatistics_t*
                                                      dus,
                                                    const
                                                      DistribExternalParameter_t*
                                                        dep)
{
  return (dus != NULL) ? dus->addDistribExternalParameter(dep) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of DistribExternalParameter_t objects in this
 * DistribUncertStatistics_t.
 */
LIBSBML_EXTERN
unsigned int
DistribUncertStatistics_getNumDistribExternalParameters(DistribUncertStatistics_t*
  dus)
{
  return (dus != NULL) ? dus->getNumDistribExternalParameters() : SBML_INT_MAX;
}


/*
 * Creates a new DistribExternalParameter_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribExternalParameter_t
 * object created.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribUncertStatistics_createDistribExternalParameter(DistribUncertStatistics_t*
  dus)
{
  return (dus != NULL) ? dus->createDistribExternalParameter() : NULL;
}


/*
 * Removes the nth DistribExternalParameter_t from this
 * DistribUncertStatistics_t and returns a pointer to it.
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribUncertStatistics_removeDistribExternalParameter(
                                                       DistribUncertStatistics_t*
                                                         dus,
                                                       unsigned int n)
{
  return (dus != NULL) ? dus->removeDistribExternalParameter(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUncertStatistics_t object have been set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_hasRequiredAttributes(const DistribUncertStatistics_t *
  dus)
{
  return (dus != NULL) ? static_cast<int>(dus->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribUncertStatistics_t object have been set.
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_hasRequiredElements(const DistribUncertStatistics_t *
  dus)
{
  return (dus != NULL) ? static_cast<int>(dus->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


