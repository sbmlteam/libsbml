/**
 * @file Uncertainty.cpp
 * @brief Implementation of the Uncertainty class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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
#include <sbml/packages/distrib/sbml/Uncertainty.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Uncertainty using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
Uncertainty::Uncertainty(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
  : DistribBase(level, version, pkgVersion)
  , mCoefficientOfVariation (NULL)
  , mKurtosis (NULL)
  , mMean (NULL)
  , mMedian (NULL)
  , mMode (NULL)
  , mSampleSize (NULL)
  , mSkewness (NULL)
  , mStandardDeviation (NULL)
  , mStandardError (NULL)
  , mVariance (NULL)
  , mConfidenceInterval (NULL)
  , mCredibleInterval (NULL)
  , mInterquartileRange (NULL)
  , mRange (NULL)
  , mExternalParameters (level, version, pkgVersion)
  , mDistribution (NULL)
{
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version,
    pkgVersion));
  connectToChild();
}


/*
 * Creates a new Uncertainty using the given DistribPkgNamespaces object.
 */
Uncertainty::Uncertainty(DistribPkgNamespaces *distribns)
  : DistribBase(distribns)
  , mCoefficientOfVariation (NULL)
  , mKurtosis (NULL)
  , mMean (NULL)
  , mMedian (NULL)
  , mMode (NULL)
  , mSampleSize (NULL)
  , mSkewness (NULL)
  , mStandardDeviation (NULL)
  , mStandardError (NULL)
  , mVariance (NULL)
  , mConfidenceInterval (NULL)
  , mCredibleInterval (NULL)
  , mInterquartileRange (NULL)
  , mRange (NULL)
  , mExternalParameters (distribns)
  , mDistribution (NULL)
{
  setElementNamespace(distribns->getURI());
  connectToChild();
  loadPlugins(distribns);
}


/*
 * Copy constructor for Uncertainty.
 */
Uncertainty::Uncertainty(const Uncertainty& orig)
  : DistribBase( orig )
  , mCoefficientOfVariation ( NULL )
  , mKurtosis ( NULL )
  , mMean ( NULL )
  , mMedian ( NULL )
  , mMode ( NULL )
  , mSampleSize ( NULL )
  , mSkewness ( NULL )
  , mStandardDeviation ( NULL )
  , mStandardError ( NULL )
  , mVariance ( NULL )
  , mConfidenceInterval ( NULL )
  , mCredibleInterval ( NULL )
  , mInterquartileRange ( NULL )
  , mRange ( NULL )
  , mExternalParameters ( orig.mExternalParameters )
  , mDistribution ( NULL )
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

  if (orig.mSampleSize != NULL)
  {
    mSampleSize = orig.mSampleSize->clone();
  }

  if (orig.mSkewness != NULL)
  {
    mSkewness = orig.mSkewness->clone();
  }

  if (orig.mStandardDeviation != NULL)
  {
    mStandardDeviation = orig.mStandardDeviation->clone();
  }

  if (orig.mStandardError != NULL)
  {
    mStandardError = orig.mStandardError->clone();
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

  if (orig.mDistribution != NULL)
  {
    mDistribution = orig.mDistribution->clone();
  }

  connectToChild();
}


/*
 * Assignment operator for Uncertainty.
 */
Uncertainty&
Uncertainty::operator=(const Uncertainty& rhs)
{
  if (&rhs != this)
  {
    DistribBase::operator=(rhs);
    mExternalParameters = rhs.mExternalParameters;
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

    delete mSampleSize;
    if (rhs.mSampleSize != NULL)
    {
      mSampleSize = rhs.mSampleSize->clone();
    }
    else
    {
      mSampleSize = NULL;
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

    delete mStandardError;
    if (rhs.mStandardError != NULL)
    {
      mStandardError = rhs.mStandardError->clone();
    }
    else
    {
      mStandardError = NULL;
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
 * Creates and returns a deep copy of this Uncertainty object.
 */
Uncertainty*
Uncertainty::clone() const
{
  return new Uncertainty(*this);
}


/*
 * Destructor for Uncertainty.
 */
Uncertainty::~Uncertainty()
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
  delete mSampleSize;
  mSampleSize = NULL;
  delete mSkewness;
  mSkewness = NULL;
  delete mStandardDeviation;
  mStandardDeviation = NULL;
  delete mStandardError;
  mStandardError = NULL;
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
  delete mDistribution;
  mDistribution = NULL;
}


/*
 * Returns the value of the "coefficientOfVariation" element of this
 * Uncertainty.
 */
const UncertValue*
Uncertainty::getCoefficientOfVariation() const
{
  return mCoefficientOfVariation;
}


/*
 * Returns the value of the "coefficientOfVariation" element of this
 * Uncertainty.
 */
UncertValue*
Uncertainty::getCoefficientOfVariation()
{
  return mCoefficientOfVariation;
}


/*
 * Returns the value of the "kurtosis" element of this Uncertainty.
 */
const UncertValue*
Uncertainty::getKurtosis() const
{
  return mKurtosis;
}


/*
 * Returns the value of the "kurtosis" element of this Uncertainty.
 */
UncertValue*
Uncertainty::getKurtosis()
{
  return mKurtosis;
}


/*
 * Returns the value of the "mean" element of this Uncertainty.
 */
const UncertValue*
Uncertainty::getMean() const
{
  return mMean;
}


/*
 * Returns the value of the "mean" element of this Uncertainty.
 */
UncertValue*
Uncertainty::getMean()
{
  return mMean;
}


/*
 * Returns the value of the "median" element of this Uncertainty.
 */
const UncertValue*
Uncertainty::getMedian() const
{
  return mMedian;
}


/*
 * Returns the value of the "median" element of this Uncertainty.
 */
UncertValue*
Uncertainty::getMedian()
{
  return mMedian;
}


/*
 * Returns the value of the "mode" element of this Uncertainty.
 */
const UncertValue*
Uncertainty::getMode() const
{
  return mMode;
}


/*
 * Returns the value of the "mode" element of this Uncertainty.
 */
UncertValue*
Uncertainty::getMode()
{
  return mMode;
}


/*
 * Returns the value of the "sampleSize" element of this Uncertainty.
 */
const UncertValue*
Uncertainty::getSampleSize() const
{
  return mSampleSize;
}


/*
 * Returns the value of the "sampleSize" element of this Uncertainty.
 */
UncertValue*
Uncertainty::getSampleSize()
{
  return mSampleSize;
}


/*
 * Returns the value of the "skewness" element of this Uncertainty.
 */
const UncertValue*
Uncertainty::getSkewness() const
{
  return mSkewness;
}


/*
 * Returns the value of the "skewness" element of this Uncertainty.
 */
UncertValue*
Uncertainty::getSkewness()
{
  return mSkewness;
}


/*
 * Returns the value of the "standardDeviation" element of this Uncertainty.
 */
const UncertValue*
Uncertainty::getStandardDeviation() const
{
  return mStandardDeviation;
}


/*
 * Returns the value of the "standardDeviation" element of this Uncertainty.
 */
UncertValue*
Uncertainty::getStandardDeviation()
{
  return mStandardDeviation;
}


/*
 * Returns the value of the "standardError" element of this Uncertainty.
 */
const UncertValue*
Uncertainty::getStandardError() const
{
  return mStandardError;
}


/*
 * Returns the value of the "standardError" element of this Uncertainty.
 */
UncertValue*
Uncertainty::getStandardError()
{
  return mStandardError;
}


/*
 * Returns the value of the "variance" element of this Uncertainty.
 */
const UncertValue*
Uncertainty::getVariance() const
{
  return mVariance;
}


/*
 * Returns the value of the "variance" element of this Uncertainty.
 */
UncertValue*
Uncertainty::getVariance()
{
  return mVariance;
}


/*
 * Returns the value of the "confidenceInterval" element of this Uncertainty.
 */
const UncertStatisticSpan*
Uncertainty::getConfidenceInterval() const
{
  return mConfidenceInterval;
}


/*
 * Returns the value of the "confidenceInterval" element of this Uncertainty.
 */
UncertStatisticSpan*
Uncertainty::getConfidenceInterval()
{
  return mConfidenceInterval;
}


/*
 * Returns the value of the "credibleInterval" element of this Uncertainty.
 */
const UncertStatisticSpan*
Uncertainty::getCredibleInterval() const
{
  return mCredibleInterval;
}


/*
 * Returns the value of the "credibleInterval" element of this Uncertainty.
 */
UncertStatisticSpan*
Uncertainty::getCredibleInterval()
{
  return mCredibleInterval;
}


/*
 * Returns the value of the "interquartileRange" element of this Uncertainty.
 */
const UncertStatisticSpan*
Uncertainty::getInterquartileRange() const
{
  return mInterquartileRange;
}


/*
 * Returns the value of the "interquartileRange" element of this Uncertainty.
 */
UncertStatisticSpan*
Uncertainty::getInterquartileRange()
{
  return mInterquartileRange;
}


/*
 * Returns the value of the "range" element of this Uncertainty.
 */
const UncertStatisticSpan*
Uncertainty::getRange() const
{
  return mRange;
}


/*
 * Returns the value of the "range" element of this Uncertainty.
 */
UncertStatisticSpan*
Uncertainty::getRange()
{
  return mRange;
}


/*
 * Returns the value of the "distribution" element of this Uncertainty.
 */
const Distribution*
Uncertainty::getDistribution() const
{
  return mDistribution;
}


/*
 * Returns the value of the "distribution" element of this Uncertainty.
 */
Distribution*
Uncertainty::getDistribution()
{
  return mDistribution;
}


/*
 * Predicate returning @c true if this Uncertainty's "coefficientOfVariation"
 * element is set.
 */
bool
Uncertainty::isSetCoefficientOfVariation() const
{
  return (mCoefficientOfVariation != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "kurtosis" element is set.
 */
bool
Uncertainty::isSetKurtosis() const
{
  return (mKurtosis != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "mean" element is set.
 */
bool
Uncertainty::isSetMean() const
{
  return (mMean != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "median" element is set.
 */
bool
Uncertainty::isSetMedian() const
{
  return (mMedian != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "mode" element is set.
 */
bool
Uncertainty::isSetMode() const
{
  return (mMode != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "sampleSize" element is
 * set.
 */
bool
Uncertainty::isSetSampleSize() const
{
  return (mSampleSize != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "skewness" element is set.
 */
bool
Uncertainty::isSetSkewness() const
{
  return (mSkewness != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "standardDeviation"
 * element is set.
 */
bool
Uncertainty::isSetStandardDeviation() const
{
  return (mStandardDeviation != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "standardError" element is
 * set.
 */
bool
Uncertainty::isSetStandardError() const
{
  return (mStandardError != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "variance" element is set.
 */
bool
Uncertainty::isSetVariance() const
{
  return (mVariance != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "confidenceInterval"
 * element is set.
 */
bool
Uncertainty::isSetConfidenceInterval() const
{
  return (mConfidenceInterval != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "credibleInterval" element
 * is set.
 */
bool
Uncertainty::isSetCredibleInterval() const
{
  return (mCredibleInterval != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "interquartileRange"
 * element is set.
 */
bool
Uncertainty::isSetInterquartileRange() const
{
  return (mInterquartileRange != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "range" element is set.
 */
bool
Uncertainty::isSetRange() const
{
  return (mRange != NULL);
}


/*
 * Predicate returning @c true if this Uncertainty's "distribution" element is
 * set.
 */
bool
Uncertainty::isSetDistribution() const
{
  return (mDistribution != NULL);
}


/*
 * Sets the value of the "coefficientOfVariation" element of this Uncertainty.
 */
int
Uncertainty::setCoefficientOfVariation(const UncertValue*
  coefficientOfVariation)
{
  if (mCoefficientOfVariation == coefficientOfVariation)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (coefficientOfVariation == NULL)
  {
    delete mCoefficientOfVariation;
    mCoefficientOfVariation = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mCoefficientOfVariation;
    mCoefficientOfVariation = (coefficientOfVariation != NULL) ?
      coefficientOfVariation->clone() : NULL;
    if (mCoefficientOfVariation != NULL)
    {
      mCoefficientOfVariation->setElementName("coefficientOfVariation");
      mCoefficientOfVariation->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "kurtosis" element of this Uncertainty.
 */
int
Uncertainty::setKurtosis(const UncertValue* kurtosis)
{
  if (mKurtosis == kurtosis)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (kurtosis == NULL)
  {
    delete mKurtosis;
    mKurtosis = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mKurtosis;
    mKurtosis = (kurtosis != NULL) ? kurtosis->clone() : NULL;
    if (mKurtosis != NULL)
    {
      mKurtosis->setElementName("kurtosis");
      mKurtosis->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "mean" element of this Uncertainty.
 */
int
Uncertainty::setMean(const UncertValue* mean)
{
  if (mMean == mean)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (mean == NULL)
  {
    delete mMean;
    mMean = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mMean;
    mMean = (mean != NULL) ? mean->clone() : NULL;
    if (mMean != NULL)
    {
      mMean->setElementName("mean");
      mMean->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "median" element of this Uncertainty.
 */
int
Uncertainty::setMedian(const UncertValue* median)
{
  if (mMedian == median)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (median == NULL)
  {
    delete mMedian;
    mMedian = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mMedian;
    mMedian = (median != NULL) ? median->clone() : NULL;
    if (mMedian != NULL)
    {
      mMedian->setElementName("median");
      mMedian->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "mode" element of this Uncertainty.
 */
int
Uncertainty::setMode(const UncertValue* mode)
{
  if (mMode == mode)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (mode == NULL)
  {
    delete mMode;
    mMode = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mMode;
    mMode = (mode != NULL) ? mode->clone() : NULL;
    if (mMode != NULL)
    {
      mMode->setElementName("mode");
      mMode->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "sampleSize" element of this Uncertainty.
 */
int
Uncertainty::setSampleSize(const UncertValue* sampleSize)
{
  if (mSampleSize == sampleSize)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (sampleSize == NULL)
  {
    delete mSampleSize;
    mSampleSize = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mSampleSize;
    mSampleSize = (sampleSize != NULL) ? sampleSize->clone() : NULL;
    if (mSampleSize != NULL)
    {
      mSampleSize->setElementName("sampleSize");
      mSampleSize->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "skewness" element of this Uncertainty.
 */
int
Uncertainty::setSkewness(const UncertValue* skewness)
{
  if (mSkewness == skewness)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (skewness == NULL)
  {
    delete mSkewness;
    mSkewness = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mSkewness;
    mSkewness = (skewness != NULL) ? skewness->clone() : NULL;
    if (mSkewness != NULL)
    {
      mSkewness->setElementName("skewness");
      mSkewness->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "standardDeviation" element of this Uncertainty.
 */
int
Uncertainty::setStandardDeviation(const UncertValue* standardDeviation)
{
  if (mStandardDeviation == standardDeviation)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (standardDeviation == NULL)
  {
    delete mStandardDeviation;
    mStandardDeviation = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mStandardDeviation;
    mStandardDeviation = (standardDeviation != NULL) ?
      standardDeviation->clone() : NULL;
    if (mStandardDeviation != NULL)
    {
      mStandardDeviation->setElementName("standardDeviation");
      mStandardDeviation->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "standardError" element of this Uncertainty.
 */
int
Uncertainty::setStandardError(const UncertValue* standardError)
{
  if (mStandardError == standardError)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (standardError == NULL)
  {
    delete mStandardError;
    mStandardError = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mStandardError;
    mStandardError = (standardError != NULL) ? standardError->clone() : NULL;
    if (mStandardError != NULL)
    {
      mStandardError->setElementName("standardError");
      mStandardError->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "variance" element of this Uncertainty.
 */
int
Uncertainty::setVariance(const UncertValue* variance)
{
  if (mVariance == variance)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (variance == NULL)
  {
    delete mVariance;
    mVariance = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mVariance;
    mVariance = (variance != NULL) ? variance->clone() : NULL;
    if (mVariance != NULL)
    {
      mVariance->setElementName("variance");
      mVariance->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "confidenceInterval" element of this Uncertainty.
 */
int
Uncertainty::setConfidenceInterval(const UncertStatisticSpan*
  confidenceInterval)
{
  if (mConfidenceInterval == confidenceInterval)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (confidenceInterval == NULL)
  {
    delete mConfidenceInterval;
    mConfidenceInterval = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mConfidenceInterval;
    mConfidenceInterval = (confidenceInterval != NULL) ?
      confidenceInterval->clone() : NULL;
    if (mConfidenceInterval != NULL)
    {
      mConfidenceInterval->setElementName("confidenceInterval");
      mConfidenceInterval->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "credibleInterval" element of this Uncertainty.
 */
int
Uncertainty::setCredibleInterval(const UncertStatisticSpan* credibleInterval)
{
  if (mCredibleInterval == credibleInterval)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (credibleInterval == NULL)
  {
    delete mCredibleInterval;
    mCredibleInterval = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mCredibleInterval;
    mCredibleInterval = (credibleInterval != NULL) ? credibleInterval->clone()
      : NULL;
    if (mCredibleInterval != NULL)
    {
      mCredibleInterval->setElementName("credibleInterval");
      mCredibleInterval->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "interquartileRange" element of this Uncertainty.
 */
int
Uncertainty::setInterquartileRange(const UncertStatisticSpan*
  interquartileRange)
{
  if (mInterquartileRange == interquartileRange)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (interquartileRange == NULL)
  {
    delete mInterquartileRange;
    mInterquartileRange = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mInterquartileRange;
    mInterquartileRange = (interquartileRange != NULL) ?
      interquartileRange->clone() : NULL;
    if (mInterquartileRange != NULL)
    {
      mInterquartileRange->setElementName("interquartileRange");
      mInterquartileRange->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "range" element of this Uncertainty.
 */
int
Uncertainty::setRange(const UncertStatisticSpan* range)
{
  if (mRange == range)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (range == NULL)
  {
    delete mRange;
    mRange = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mRange;
    mRange = (range != NULL) ? range->clone() : NULL;
    if (mRange != NULL)
    {
      mRange->setElementName("range");
      mRange->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "distribution" element of this Uncertainty.
 */
int
Uncertainty::setDistribution(const Distribution* distribution)
{
  if (mDistribution == distribution)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (distribution == NULL)
  {
    delete mDistribution;
    mDistribution = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mDistribution;
    mDistribution = (distribution != NULL) ? distribution->clone() : NULL;
    if (mDistribution != NULL)
    {
      mDistribution->connectToParent(this);
    }

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new UncertValue object, adds it to this Uncertainty object and
 * returns the UncertValue object created.
 */
UncertValue*
Uncertainty::createCoefficientOfVariation()
{
  if (mCoefficientOfVariation != NULL)
  {
    delete mCoefficientOfVariation;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mCoefficientOfVariation = new UncertValue(distribns);

  mCoefficientOfVariation->setElementName("coefficientOfVariation");

  delete distribns;

  connectToChild();

  return mCoefficientOfVariation;
}


/*
 * Creates a new UncertValue object, adds it to this Uncertainty object and
 * returns the UncertValue object created.
 */
UncertValue*
Uncertainty::createKurtosis()
{
  if (mKurtosis != NULL)
  {
    delete mKurtosis;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mKurtosis = new UncertValue(distribns);

  mKurtosis->setElementName("kurtosis");

  delete distribns;

  connectToChild();

  return mKurtosis;
}


/*
 * Creates a new UncertValue object, adds it to this Uncertainty object and
 * returns the UncertValue object created.
 */
UncertValue*
Uncertainty::createMean()
{
  if (mMean != NULL)
  {
    delete mMean;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mMean = new UncertValue(distribns);

  mMean->setElementName("mean");

  delete distribns;

  connectToChild();

  return mMean;
}


/*
 * Creates a new UncertValue object, adds it to this Uncertainty object and
 * returns the UncertValue object created.
 */
UncertValue*
Uncertainty::createMedian()
{
  if (mMedian != NULL)
  {
    delete mMedian;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mMedian = new UncertValue(distribns);

  mMedian->setElementName("median");

  delete distribns;

  connectToChild();

  return mMedian;
}


/*
 * Creates a new UncertValue object, adds it to this Uncertainty object and
 * returns the UncertValue object created.
 */
UncertValue*
Uncertainty::createMode()
{
  if (mMode != NULL)
  {
    delete mMode;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mMode = new UncertValue(distribns);

  mMode->setElementName("mode");

  delete distribns;

  connectToChild();

  return mMode;
}


/*
 * Creates a new UncertValue object, adds it to this Uncertainty object and
 * returns the UncertValue object created.
 */
UncertValue*
Uncertainty::createSampleSize()
{
  if (mSampleSize != NULL)
  {
    delete mSampleSize;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mSampleSize = new UncertValue(distribns);

  mSampleSize->setElementName("sampleSize");

  delete distribns;

  connectToChild();

  return mSampleSize;
}


/*
 * Creates a new UncertValue object, adds it to this Uncertainty object and
 * returns the UncertValue object created.
 */
UncertValue*
Uncertainty::createSkewness()
{
  if (mSkewness != NULL)
  {
    delete mSkewness;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mSkewness = new UncertValue(distribns);

  mSkewness->setElementName("skewness");

  delete distribns;

  connectToChild();

  return mSkewness;
}


/*
 * Creates a new UncertValue object, adds it to this Uncertainty object and
 * returns the UncertValue object created.
 */
UncertValue*
Uncertainty::createStandardDeviation()
{
  if (mStandardDeviation != NULL)
  {
    delete mStandardDeviation;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mStandardDeviation = new UncertValue(distribns);

  mStandardDeviation->setElementName("standardDeviation");

  delete distribns;

  connectToChild();

  return mStandardDeviation;
}


/*
 * Creates a new UncertValue object, adds it to this Uncertainty object and
 * returns the UncertValue object created.
 */
UncertValue*
Uncertainty::createStandardError()
{
  if (mStandardError != NULL)
  {
    delete mStandardError;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mStandardError = new UncertValue(distribns);

  mStandardError->setElementName("standardError");

  delete distribns;

  connectToChild();

  return mStandardError;
}


/*
 * Creates a new UncertValue object, adds it to this Uncertainty object and
 * returns the UncertValue object created.
 */
UncertValue*
Uncertainty::createVariance()
{
  if (mVariance != NULL)
  {
    delete mVariance;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mVariance = new UncertValue(distribns);

  mVariance->setElementName("variance");

  delete distribns;

  connectToChild();

  return mVariance;
}


/*
 * Creates a new UncertStatisticSpan object, adds it to this Uncertainty object
 * and returns the UncertStatisticSpan object created.
 */
UncertStatisticSpan*
Uncertainty::createConfidenceInterval()
{
  if (mConfidenceInterval != NULL)
  {
    delete mConfidenceInterval;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mConfidenceInterval = new UncertStatisticSpan(distribns);

  mConfidenceInterval->setElementName("confidenceInterval");

  delete distribns;

  connectToChild();

  return mConfidenceInterval;
}


/*
 * Creates a new UncertStatisticSpan object, adds it to this Uncertainty object
 * and returns the UncertStatisticSpan object created.
 */
UncertStatisticSpan*
Uncertainty::createCredibleInterval()
{
  if (mCredibleInterval != NULL)
  {
    delete mCredibleInterval;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mCredibleInterval = new UncertStatisticSpan(distribns);

  mCredibleInterval->setElementName("credibleInterval");

  delete distribns;

  connectToChild();

  return mCredibleInterval;
}


/*
 * Creates a new UncertStatisticSpan object, adds it to this Uncertainty object
 * and returns the UncertStatisticSpan object created.
 */
UncertStatisticSpan*
Uncertainty::createInterquartileRange()
{
  if (mInterquartileRange != NULL)
  {
    delete mInterquartileRange;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mInterquartileRange = new UncertStatisticSpan(distribns);

  mInterquartileRange->setElementName("interquartileRange");

  delete distribns;

  connectToChild();

  return mInterquartileRange;
}


/*
 * Creates a new UncertStatisticSpan object, adds it to this Uncertainty object
 * and returns the UncertStatisticSpan object created.
 */
UncertStatisticSpan*
Uncertainty::createRange()
{
  if (mRange != NULL)
  {
    delete mRange;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mRange = new UncertStatisticSpan(distribns);

  mRange->setElementName("range");

  delete distribns;

  connectToChild();

  return mRange;
}


/*
 * Creates a new Distribution object, adds it to this Uncertainty object and
 * returns the Distribution object created.
 */
Distribution*
Uncertainty::createDistribution()
{
  if (mDistribution != NULL)
  {
    delete mDistribution;
  }

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  mDistribution = new Distribution(distribns);

  delete distribns;

  connectToChild();

  return mDistribution;
}


/*
 * Unsets the value of the "coefficientOfVariation" element of this
 * Uncertainty.
 */
int
Uncertainty::unsetCoefficientOfVariation()
{
  delete mCoefficientOfVariation;
  mCoefficientOfVariation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "kurtosis" element of this Uncertainty.
 */
int
Uncertainty::unsetKurtosis()
{
  delete mKurtosis;
  mKurtosis = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "mean" element of this Uncertainty.
 */
int
Uncertainty::unsetMean()
{
  delete mMean;
  mMean = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "median" element of this Uncertainty.
 */
int
Uncertainty::unsetMedian()
{
  delete mMedian;
  mMedian = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "mode" element of this Uncertainty.
 */
int
Uncertainty::unsetMode()
{
  delete mMode;
  mMode = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "sampleSize" element of this Uncertainty.
 */
int
Uncertainty::unsetSampleSize()
{
  delete mSampleSize;
  mSampleSize = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "skewness" element of this Uncertainty.
 */
int
Uncertainty::unsetSkewness()
{
  delete mSkewness;
  mSkewness = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "standardDeviation" element of this Uncertainty.
 */
int
Uncertainty::unsetStandardDeviation()
{
  delete mStandardDeviation;
  mStandardDeviation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "standardError" element of this Uncertainty.
 */
int
Uncertainty::unsetStandardError()
{
  delete mStandardError;
  mStandardError = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "variance" element of this Uncertainty.
 */
int
Uncertainty::unsetVariance()
{
  delete mVariance;
  mVariance = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "confidenceInterval" element of this Uncertainty.
 */
int
Uncertainty::unsetConfidenceInterval()
{
  delete mConfidenceInterval;
  mConfidenceInterval = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "credibleInterval" element of this Uncertainty.
 */
int
Uncertainty::unsetCredibleInterval()
{
  delete mCredibleInterval;
  mCredibleInterval = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "interquartileRange" element of this Uncertainty.
 */
int
Uncertainty::unsetInterquartileRange()
{
  delete mInterquartileRange;
  mInterquartileRange = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "range" element of this Uncertainty.
 */
int
Uncertainty::unsetRange()
{
  delete mRange;
  mRange = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "distribution" element of this Uncertainty.
 */
int
Uncertainty::unsetDistribution()
{
  delete mDistribution;
  mDistribution = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the ListOfExternalParameters from this Uncertainty.
 */
const ListOfExternalParameters*
Uncertainty::getListOfExternalParameters() const
{
  return &mExternalParameters;
}


/*
 * Returns the ListOfExternalParameters from this Uncertainty.
 */
ListOfExternalParameters*
Uncertainty::getListOfExternalParameters()
{
  return &mExternalParameters;
}


/*
 * Get an ExternalParameter from the Uncertainty.
 */
ExternalParameter*
Uncertainty::getExternalParameter(unsigned int n)
{
  return mExternalParameters.get(n);
}


/*
 * Get an ExternalParameter from the Uncertainty.
 */
const ExternalParameter*
Uncertainty::getExternalParameter(unsigned int n) const
{
  return mExternalParameters.get(n);
}


/*
 * Adds a copy of the given ExternalParameter to this Uncertainty.
 */
int
Uncertainty::addExternalParameter(const ExternalParameter* ep)
{
  if (ep == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (ep->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != ep->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != ep->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(ep)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mExternalParameters.append(ep);
  }
}


/*
 * Get the number of ExternalParameter objects in this Uncertainty.
 */
unsigned int
Uncertainty::getNumExternalParameters() const
{
  return mExternalParameters.size();
}


/*
 * Creates a new ExternalParameter object, adds it to this Uncertainty object
 * and returns the ExternalParameter object created.
 */
ExternalParameter*
Uncertainty::createExternalParameter()
{
  ExternalParameter* ep = NULL;

  try
  {
    DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
    ep = new ExternalParameter(distribns);
    delete distribns;
  }
  catch (...)
  {
  }

  if (ep != NULL)
  {
    mExternalParameters.appendAndOwn(ep);
  }

  return ep;
}


/*
 * Removes the nth ExternalParameter from this Uncertainty and returns a
 * pointer to it.
 */
ExternalParameter*
Uncertainty::removeExternalParameter(unsigned int n)
{
  return mExternalParameters.remove(n);
}


/*
 * Returns the XML element name of this Uncertainty object.
 */
const std::string&
Uncertainty::getElementName() const
{
  static const string name = "uncertainty";
  return name;
}


/*
 * Returns the libSBML type code for this Uncertainty object.
 */
int
Uncertainty::getTypeCode() const
{
  return SBML_DISTRIB_UNCERTAINTY;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * Uncertainty object have been set.
 */
bool
Uncertainty::hasRequiredAttributes() const
{
  bool allPresent = DistribBase::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * Uncertainty object have been set.
 */
bool
Uncertainty::hasRequiredElements() const
{
  bool allPresent = DistribBase::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
Uncertainty::writeElements(XMLOutputStream& stream) const
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

  if (isSetSampleSize() == true)
  {
    mSampleSize->write(stream);
  }

  if (isSetSkewness() == true)
  {
    mSkewness->write(stream);
  }

  if (isSetStandardDeviation() == true)
  {
    mStandardDeviation->write(stream);
  }

  if (isSetStandardError() == true)
  {
    mStandardError->write(stream);
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

  if (isSetDistribution() == true)
  {
    mDistribution->write(stream);
  }

  if (getNumExternalParameters() > 0)
  {
    mExternalParameters.write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Uncertainty::accept(SBMLVisitor& v) const
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

  if (mSampleSize != NULL)
  {
    mSampleSize->accept(v);
  }

  if (mSkewness != NULL)
  {
    mSkewness->accept(v);
  }

  if (mStandardDeviation != NULL)
  {
    mStandardDeviation->accept(v);
  }

  if (mStandardError != NULL)
  {
    mStandardError->accept(v);
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

  if (mDistribution != NULL)
  {
    mDistribution->accept(v);
  }

  mExternalParameters.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
Uncertainty::setSBMLDocument(SBMLDocument* d)
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

  if (mSampleSize != NULL)
  {
    mSampleSize->setSBMLDocument(d);
  }

  if (mSkewness != NULL)
  {
    mSkewness->setSBMLDocument(d);
  }

  if (mStandardDeviation != NULL)
  {
    mStandardDeviation->setSBMLDocument(d);
  }

  if (mStandardError != NULL)
  {
    mStandardError->setSBMLDocument(d);
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

  if (mDistribution != NULL)
  {
    mDistribution->setSBMLDocument(d);
  }

  mExternalParameters.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
Uncertainty::connectToChild()
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

  if (mSampleSize != NULL)
  {
    mSampleSize->connectToParent(this);
  }

  if (mSkewness != NULL)
  {
    mSkewness->connectToParent(this);
  }

  if (mStandardDeviation != NULL)
  {
    mStandardDeviation->connectToParent(this);
  }

  if (mStandardError != NULL)
  {
    mStandardError->connectToParent(this);
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

  if (mDistribution != NULL)
  {
    mDistribution->connectToParent(this);
  }

  mExternalParameters.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
Uncertainty::enablePackageInternal(const std::string& pkgURI,
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

  if (isSetSampleSize())
  {
    mSampleSize->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetSkewness())
  {
    mSkewness->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetStandardDeviation())
  {
    mStandardDeviation->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  if (isSetStandardError())
  {
    mStandardError->enablePackageInternal(pkgURI, pkgPrefix, flag);
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

  if (isSetDistribution())
  {
    mDistribution->enablePackageInternal(pkgURI, pkgPrefix, flag);
  }

  mExternalParameters.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Updates the namespaces when setLevelVersion is used
 */
void
Uncertainty::updateSBMLNamespace(const std::string& package,
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

  if (mSampleSize != NULL)
  {
    mSampleSize->updateSBMLNamespace(package, level, version);
  }

  if (mSkewness != NULL)
  {
    mSkewness->updateSBMLNamespace(package, level, version);
  }

  if (mStandardDeviation != NULL)
  {
    mStandardDeviation->updateSBMLNamespace(package, level, version);
  }

  if (mStandardError != NULL)
  {
    mStandardError->updateSBMLNamespace(package, level, version);
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

  if (mDistribution != NULL)
  {
    mDistribution->updateSBMLNamespace(package, level, version);
  }

  mExternalParameters.updateSBMLNamespace(package, level, version);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::getAttribute(const std::string& attributeName,
                          double& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::getAttribute(const std::string& attributeName,
                          std::string& value) const
{
  int return_value = DistribBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Uncertainty's attribute "attributeName"
 * is set.
 */
bool
Uncertainty::isSetAttribute(const std::string& attributeName) const
{
  bool value = DistribBase::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::setAttribute(const std::string& attributeName, int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::setAttribute(const std::string& attributeName, double value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::setAttribute(const std::string& attributeName,
                          const std::string& value)
{
  int return_value = DistribBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Uncertainty.
 */
int
Uncertainty::unsetAttribute(const std::string& attributeName)
{
  int value = DistribBase::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this Uncertainty.
 */
SBase*
Uncertainty::createChildObject(const std::string& elementName)
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
  else if (elementName == "sampleSize")
  {
    return createSampleSize();
  }
  else if (elementName == "skewness")
  {
    return createSkewness();
  }
  else if (elementName == "standardDeviation")
  {
    return createStandardDeviation();
  }
  else if (elementName == "standardError")
  {
    return createStandardError();
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
  else if (elementName == "distribution")
  {
    return createDistribution();
  }
  else if (elementName == "externalParameter")
  {
    return createExternalParameter();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this Uncertainty.
 */
int
Uncertainty::addChildObject(const std::string& elementName,
                            const SBase* element)
{
  if (elementName == "coefficientOfVariation" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setCoefficientOfVariation((const UncertValue*)(element));
  }
  else if (elementName == "kurtosis" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setKurtosis((const UncertValue*)(element));
  }
  else if (elementName == "mean" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setMean((const UncertValue*)(element));
  }
  else if (elementName == "median" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setMedian((const UncertValue*)(element));
  }
  else if (elementName == "mode" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setMode((const UncertValue*)(element));
  }
  else if (elementName == "sampleSize" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setSampleSize((const UncertValue*)(element));
  }
  else if (elementName == "skewness" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setSkewness((const UncertValue*)(element));
  }
  else if (elementName == "standardDeviation" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setStandardDeviation((const UncertValue*)(element));
  }
  else if (elementName == "standardError" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setStandardError((const UncertValue*)(element));
  }
  else if (elementName == "variance" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTVALUE)
  {
    return setVariance((const UncertValue*)(element));
  }
  else if (elementName == "confidenceInterval" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTSTATISTICSPAN)
  {
    return setConfidenceInterval((const UncertStatisticSpan*)(element));
  }
  else if (elementName == "credibleInterval" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTSTATISTICSPAN)
  {
    return setCredibleInterval((const UncertStatisticSpan*)(element));
  }
  else if (elementName == "interquartileRange" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTSTATISTICSPAN)
  {
    return setInterquartileRange((const UncertStatisticSpan*)(element));
  }
  else if (elementName == "range" && element->getTypeCode() ==
    SBML_DISTRIB_UNCERTSTATISTICSPAN)
  {
    return setRange((const UncertStatisticSpan*)(element));
  }
  else if (elementName == "distribution" && element->getTypeCode() ==
    SBML_DISTRIB_DISTRIBUTION)
  {
    return setDistribution((const Distribution*)(element));
  }
  else if (elementName == "externalParameter" && element->getTypeCode() ==
    SBML_DISTRIB_EXTERNALPARAMETER)
  {
    return addExternalParameter((const ExternalParameter*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * Uncertainty.
 */
SBase*
Uncertainty::removeChildObject(const std::string& elementName,
                               const std::string& id)
{
  if (elementName == "coefficientOfVariation")
  {
    UncertValue * obj = getCoefficientOfVariation();
    if (unsetCoefficientOfVariation() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "kurtosis")
  {
    UncertValue * obj = getKurtosis();
    if (unsetKurtosis() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "mean")
  {
    UncertValue * obj = getMean();
    if (unsetMean() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "median")
  {
    UncertValue * obj = getMedian();
    if (unsetMedian() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "mode")
  {
    UncertValue * obj = getMode();
    if (unsetMode() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "sampleSize")
  {
    UncertValue * obj = getSampleSize();
    if (unsetSampleSize() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "skewness")
  {
    UncertValue * obj = getSkewness();
    if (unsetSkewness() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "standardDeviation")
  {
    UncertValue * obj = getStandardDeviation();
    if (unsetStandardDeviation() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "standardError")
  {
    UncertValue * obj = getStandardError();
    if (unsetStandardError() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "variance")
  {
    UncertValue * obj = getVariance();
    if (unsetVariance() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "confidenceInterval")
  {
    UncertStatisticSpan * obj = getConfidenceInterval();
    if (unsetConfidenceInterval() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "credibleInterval")
  {
    UncertStatisticSpan * obj = getCredibleInterval();
    if (unsetCredibleInterval() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "interquartileRange")
  {
    UncertStatisticSpan * obj = getInterquartileRange();
    if (unsetInterquartileRange() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "range")
  {
    UncertStatisticSpan * obj = getRange();
    if (unsetRange() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "distribution")
  {
    Distribution * obj = getDistribution();
    if (unsetDistribution() == LIBSBML_OPERATION_SUCCESS) return obj;
  }
  else if (elementName == "externalParameter")
  {
    for (unsigned int i = 0; i < getNumExternalParameters(); i++)
    {
      if (getExternalParameter(i)->getId() == id)
      {
        return removeExternalParameter(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this Uncertainty.
 */
unsigned int
Uncertainty::getNumObjects(const std::string& elementName)
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
  else if (elementName == "sampleSize")
  {
    if (isSetSampleSize())
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
  else if (elementName == "standardError")
  {
    if (isSetStandardError())
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
  else if (elementName == "distribution")
  {
    if (isSetDistribution())
    {
      return 1;
    }
  }
  else if (elementName == "externalParameter")
  {
    return getNumExternalParameters();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this Uncertainty.
 */
SBase*
Uncertainty::getObject(const std::string& elementName, unsigned int index)
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
  else if (elementName == "sampleSize")
  {
    return getSampleSize();
  }
  else if (elementName == "skewness")
  {
    return getSkewness();
  }
  else if (elementName == "standardDeviation")
  {
    return getStandardDeviation();
  }
  else if (elementName == "standardError")
  {
    return getStandardError();
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
  else if (elementName == "distribution")
  {
    return getDistribution();
  }
  else if (elementName == "externalParameter")
  {
    return getExternalParameter(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
Uncertainty::getElementBySId(const std::string& id)
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

  if (mSampleSize != NULL)
  {
    if (mSampleSize->getId() == id)
    {
      return mSampleSize;
    }

    obj = mSampleSize->getElementBySId(id);
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

  if (mStandardError != NULL)
  {
    if (mStandardError->getId() == id)
    {
      return mStandardError;
    }

    obj = mStandardError->getElementBySId(id);
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

  if (mExternalParameters.getId() == id)
  {
    return &mExternalParameters;
  }

  obj = mExternalParameters.getElementBySId(id);

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
Uncertainty::getElementByMetaId(const std::string& metaid)
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

  if (mSampleSize != NULL)
  {
    if (mSampleSize->getMetaId() == metaid)
    {
      return mSampleSize;
    }

    obj = mSampleSize->getElementByMetaId(metaid);
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

  if (mStandardError != NULL)
  {
    if (mStandardError->getMetaId() == metaid)
    {
      return mStandardError;
    }

    obj = mStandardError->getElementByMetaId(metaid);
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

  if (mExternalParameters.getMetaId() == metaid)
  {
    return &mExternalParameters;
  }

  obj = mExternalParameters.getElementByMetaId(metaid);

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
Uncertainty::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mCoefficientOfVariation, filter);
  ADD_FILTERED_POINTER(ret, sublist, mKurtosis, filter);
  ADD_FILTERED_POINTER(ret, sublist, mMean, filter);
  ADD_FILTERED_POINTER(ret, sublist, mMedian, filter);
  ADD_FILTERED_POINTER(ret, sublist, mMode, filter);
  ADD_FILTERED_POINTER(ret, sublist, mSampleSize, filter);
  ADD_FILTERED_POINTER(ret, sublist, mSkewness, filter);
  ADD_FILTERED_POINTER(ret, sublist, mStandardDeviation, filter);
  ADD_FILTERED_POINTER(ret, sublist, mStandardError, filter);
  ADD_FILTERED_POINTER(ret, sublist, mVariance, filter);
  ADD_FILTERED_POINTER(ret, sublist, mConfidenceInterval, filter);
  ADD_FILTERED_POINTER(ret, sublist, mCredibleInterval, filter);
  ADD_FILTERED_POINTER(ret, sublist, mInterquartileRange, filter);
  ADD_FILTERED_POINTER(ret, sublist, mRange, filter);
  ADD_FILTERED_POINTER(ret, sublist, mDistribution, filter);

  ADD_FILTERED_LIST(ret, sublist, mExternalParameters, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
Uncertainty::createObject(XMLInputStream& stream)
{
  SBase* obj = DistribBase::createObject(stream);

  const std::string& name = stream.peek().getName();

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());

  if (name == "coefficientOfVariation")
  {
    if (isSetCoefficientOfVariation())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mCoefficientOfVariation;
    mCoefficientOfVariation = new UncertValue(distribns);
    mCoefficientOfVariation->setElementName(name);
    obj = mCoefficientOfVariation;
  }
  else if (name == "kurtosis")
  {
    if (isSetKurtosis())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mKurtosis;
    mKurtosis = new UncertValue(distribns);
    mKurtosis->setElementName(name);
    obj = mKurtosis;
  }
  else if (name == "mean")
  {
    if (isSetMean())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mMean;
    mMean = new UncertValue(distribns);
    mMean->setElementName(name);
    obj = mMean;
  }
  else if (name == "median")
  {
    if (isSetMedian())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mMedian;
    mMedian = new UncertValue(distribns);
    mMedian->setElementName(name);
    obj = mMedian;
  }
  else if (name == "mode")
  {
    if (isSetMode())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mMode;
    mMode = new UncertValue(distribns);
    mMode->setElementName(name);
    obj = mMode;
  }
  else if (name == "sampleSize")
  {
    if (isSetSampleSize())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mSampleSize;
    mSampleSize = new UncertValue(distribns);
    mSampleSize->setElementName(name);
    obj = mSampleSize;
  }
  else if (name == "skewness")
  {
    if (isSetSkewness())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mSkewness;
    mSkewness = new UncertValue(distribns);
    mSkewness->setElementName(name);
    obj = mSkewness;
  }
  else if (name == "standardDeviation")
  {
    if (isSetStandardDeviation())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mStandardDeviation;
    mStandardDeviation = new UncertValue(distribns);
    mStandardDeviation->setElementName(name);
    obj = mStandardDeviation;
  }
  else if (name == "standardError")
  {
    if (isSetStandardError())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mStandardError;
    mStandardError = new UncertValue(distribns);
    mStandardError->setElementName(name);
    obj = mStandardError;
  }
  else if (name == "variance")
  {
    if (isSetVariance())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mVariance;
    mVariance = new UncertValue(distribns);
    mVariance->setElementName(name);
    obj = mVariance;
  }
  else if (name == "confidenceInterval")
  {
    if (isSetConfidenceInterval())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mConfidenceInterval;
    mConfidenceInterval = new UncertStatisticSpan(distribns);
    mConfidenceInterval->setElementName(name);
    obj = mConfidenceInterval;
  }
  else if (name == "credibleInterval")
  {
    if (isSetCredibleInterval())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mCredibleInterval;
    mCredibleInterval = new UncertStatisticSpan(distribns);
    mCredibleInterval->setElementName(name);
    obj = mCredibleInterval;
  }
  else if (name == "interquartileRange")
  {
    if (isSetInterquartileRange())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mInterquartileRange;
    mInterquartileRange = new UncertStatisticSpan(distribns);
    mInterquartileRange->setElementName(name);
    obj = mInterquartileRange;
  }
  else if (name == "range")
  {
    if (isSetRange())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mRange;
    mRange = new UncertStatisticSpan(distribns);
    mRange->setElementName(name);
    obj = mRange;
  }
  else if (name == "distribution")
  {
    if (isSetDistribution())
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    delete mDistribution;
    mDistribution = new Distribution(distribns);
    obj = mDistribution;
  }
  else if (name == "listOfExternalParameters")
  {
    if (mExternalParameters.size() != 0)
    {
      getErrorLog()->logPackageError("distrib",
        DistribUncertaintyAllowedElements, getPackageVersion(), getLevel(),
          getVersion());
    }

    obj = &mExternalParameters;
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
Uncertainty::addExpectedAttributes(ExpectedAttributes& attributes)
{
  DistribBase::addExpectedAttributes(attributes);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Uncertainty::readAttributes(const XMLAttributes& attributes,
                            const ExpectedAttributes& expectedAttributes)
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
          DistribUncertaintyAllowedCoreAttributes, pkgVersion, level, version,
            details);
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
Uncertainty::writeAttributes(XMLOutputStream& stream) const
{
  DistribBase::writeAttributes(stream);

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new Uncertainty_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 */
LIBSBML_EXTERN
Uncertainty_t *
Uncertainty_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion)
{
  return new Uncertainty(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Uncertainty_t object.
 */
LIBSBML_EXTERN
Uncertainty_t*
Uncertainty_clone(const Uncertainty_t* u)
{
  if (u != NULL)
  {
    return static_cast<Uncertainty_t*>(u->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Uncertainty_t object.
 */
LIBSBML_EXTERN
void
Uncertainty_free(Uncertainty_t* u)
{
  if (u != NULL)
  {
    delete u;
  }
}


/*
 * Returns the value of the "coefficientOfVariation" element of this
 * Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getCoefficientOfVariation(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->getCoefficientOfVariation());
}


/*
 * Returns the value of the "kurtosis" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getKurtosis(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->getKurtosis());
}


/*
 * Returns the value of the "mean" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getMean(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->getMean());
}


/*
 * Returns the value of the "median" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getMedian(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->getMedian());
}


/*
 * Returns the value of the "mode" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getMode(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->getMode());
}


/*
 * Returns the value of the "sampleSize" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getSampleSize(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->getSampleSize());
}


/*
 * Returns the value of the "skewness" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getSkewness(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->getSkewness());
}


/*
 * Returns the value of the "standardDeviation" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getStandardDeviation(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->getStandardDeviation());
}


/*
 * Returns the value of the "standardError" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getStandardError(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->getStandardError());
}


/*
 * Returns the value of the "variance" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getVariance(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->getVariance());
}


/*
 * Returns the value of the "confidenceInterval" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertStatisticSpan_t*
Uncertainty_getConfidenceInterval(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertStatisticSpan_t*)(u->getConfidenceInterval());
}


/*
 * Returns the value of the "credibleInterval" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertStatisticSpan_t*
Uncertainty_getCredibleInterval(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertStatisticSpan_t*)(u->getCredibleInterval());
}


/*
 * Returns the value of the "interquartileRange" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertStatisticSpan_t*
Uncertainty_getInterquartileRange(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertStatisticSpan_t*)(u->getInterquartileRange());
}


/*
 * Returns the value of the "range" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const UncertStatisticSpan_t*
Uncertainty_getRange(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertStatisticSpan_t*)(u->getRange());
}


/*
 * Returns the value of the "distribution" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
const Distribution_t*
Uncertainty_getDistribution(const Uncertainty_t * u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (Distribution_t*)(u->getDistribution());
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's
 * "coefficientOfVariation" element is set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetCoefficientOfVariation(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetCoefficientOfVariation()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "kurtosis" element
 * is set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetKurtosis(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetKurtosis()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "mean" element is
 * set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetMean(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetMean()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "median" element is
 * set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetMedian(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetMedian()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "mode" element is
 * set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetMode(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetMode()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "sampleSize" element
 * is set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetSampleSize(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetSampleSize()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "skewness" element
 * is set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetSkewness(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetSkewness()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "standardDeviation"
 * element is set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetStandardDeviation(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetStandardDeviation()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "standardError"
 * element is set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetStandardError(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetStandardError()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "variance" element
 * is set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetVariance(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetVariance()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "confidenceInterval"
 * element is set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetConfidenceInterval(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetConfidenceInterval()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "credibleInterval"
 * element is set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetCredibleInterval(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetCredibleInterval()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "interquartileRange"
 * element is set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetInterquartileRange(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetInterquartileRange()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "range" element is
 * set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetRange(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetRange()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Uncertainty_t's "distribution"
 * element is set.
 */
LIBSBML_EXTERN
int
Uncertainty_isSetDistribution(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetDistribution()) : 0;
}


/*
 * Sets the value of the "coefficientOfVariation" element of this
 * Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setCoefficientOfVariation(Uncertainty_t * u,
                                      const UncertValue_t*
                                        coefficientOfVariation)
{
  return (u != NULL) ? u->setCoefficientOfVariation(coefficientOfVariation) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "kurtosis" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setKurtosis(Uncertainty_t * u, const UncertValue_t* kurtosis)
{
  return (u != NULL) ? u->setKurtosis(kurtosis) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "mean" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setMean(Uncertainty_t * u, const UncertValue_t* mean)
{
  return (u != NULL) ? u->setMean(mean) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "median" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setMedian(Uncertainty_t * u, const UncertValue_t* median)
{
  return (u != NULL) ? u->setMedian(median) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "mode" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setMode(Uncertainty_t * u, const UncertValue_t* mode)
{
  return (u != NULL) ? u->setMode(mode) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "sampleSize" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setSampleSize(Uncertainty_t * u, const UncertValue_t* sampleSize)
{
  return (u != NULL) ? u->setSampleSize(sampleSize) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "skewness" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setSkewness(Uncertainty_t * u, const UncertValue_t* skewness)
{
  return (u != NULL) ? u->setSkewness(skewness) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "standardDeviation" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setStandardDeviation(Uncertainty_t * u,
                                 const UncertValue_t* standardDeviation)
{
  return (u != NULL) ? u->setStandardDeviation(standardDeviation) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "standardError" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setStandardError(Uncertainty_t * u,
                             const UncertValue_t* standardError)
{
  return (u != NULL) ? u->setStandardError(standardError) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "variance" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setVariance(Uncertainty_t * u, const UncertValue_t* variance)
{
  return (u != NULL) ? u->setVariance(variance) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "confidenceInterval" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setConfidenceInterval(Uncertainty_t * u,
                                  const UncertStatisticSpan_t*
                                    confidenceInterval)
{
  return (u != NULL) ? u->setConfidenceInterval(confidenceInterval) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "credibleInterval" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setCredibleInterval(Uncertainty_t * u,
                                const UncertStatisticSpan_t* credibleInterval)
{
  return (u != NULL) ? u->setCredibleInterval(credibleInterval) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "interquartileRange" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setInterquartileRange(Uncertainty_t * u,
                                  const UncertStatisticSpan_t*
                                    interquartileRange)
{
  return (u != NULL) ? u->setInterquartileRange(interquartileRange) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "range" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setRange(Uncertainty_t * u, const UncertStatisticSpan_t* range)
{
  return (u != NULL) ? u->setRange(range) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "distribution" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_setDistribution(Uncertainty_t * u,
                            const Distribution_t* distribution)
{
  return (u != NULL) ? u->setDistribution(distribution) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createCoefficientOfVariation(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->createCoefficientOfVariation());
}


/*
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createKurtosis(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->createKurtosis());
}


/*
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createMean(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->createMean());
}


/*
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createMedian(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->createMedian());
}


/*
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createMode(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->createMode());
}


/*
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createSampleSize(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->createSampleSize());
}


/*
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createSkewness(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->createSkewness());
}


/*
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createStandardDeviation(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->createStandardDeviation());
}


/*
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createStandardError(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->createStandardError());
}


/*
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createVariance(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertValue_t*)(u->createVariance());
}


/*
 * Creates a new UncertStatisticSpan_t object, adds it to this Uncertainty_t
 * object and returns the UncertStatisticSpan_t object created.
 */
LIBSBML_EXTERN
UncertStatisticSpan_t*
Uncertainty_createConfidenceInterval(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertStatisticSpan_t*)(u->createConfidenceInterval());
}


/*
 * Creates a new UncertStatisticSpan_t object, adds it to this Uncertainty_t
 * object and returns the UncertStatisticSpan_t object created.
 */
LIBSBML_EXTERN
UncertStatisticSpan_t*
Uncertainty_createCredibleInterval(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertStatisticSpan_t*)(u->createCredibleInterval());
}


/*
 * Creates a new UncertStatisticSpan_t object, adds it to this Uncertainty_t
 * object and returns the UncertStatisticSpan_t object created.
 */
LIBSBML_EXTERN
UncertStatisticSpan_t*
Uncertainty_createInterquartileRange(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertStatisticSpan_t*)(u->createInterquartileRange());
}


/*
 * Creates a new UncertStatisticSpan_t object, adds it to this Uncertainty_t
 * object and returns the UncertStatisticSpan_t object created.
 */
LIBSBML_EXTERN
UncertStatisticSpan_t*
Uncertainty_createRange(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (UncertStatisticSpan_t*)(u->createRange());
}


/*
 * Creates a new Distribution_t object, adds it to this Uncertainty_t object
 * and returns the Distribution_t object created.
 */
LIBSBML_EXTERN
Distribution_t*
Uncertainty_createDistribution(Uncertainty_t* u)
{
  if (u == NULL)
  {
    return NULL;
  }

  return (Distribution_t*)(u->createDistribution());
}


/*
 * Unsets the value of the "coefficientOfVariation" element of this
 * Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetCoefficientOfVariation(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetCoefficientOfVariation() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "kurtosis" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetKurtosis(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetKurtosis() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "mean" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetMean(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetMean() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "median" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetMedian(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetMedian() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "mode" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetMode(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetMode() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "sampleSize" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetSampleSize(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetSampleSize() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "skewness" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetSkewness(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetSkewness() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "standardDeviation" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetStandardDeviation(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetStandardDeviation() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "standardError" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetStandardError(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetStandardError() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "variance" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetVariance(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetVariance() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "confidenceInterval" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetConfidenceInterval(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetConfidenceInterval() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "credibleInterval" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetCredibleInterval(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetCredibleInterval() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "interquartileRange" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetInterquartileRange(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetInterquartileRange() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "range" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetRange(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetRange() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "distribution" element of this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_unsetDistribution(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetDistribution() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing ExternalParameter_t objects from this
 * Uncertainty_t.
 */
LIBSBML_EXTERN
ListOf_t*
Uncertainty_getListOfExternalParameters(Uncertainty_t* u)
{
  return (u != NULL) ? u->getListOfExternalParameters() : NULL;
}


/*
 * Get an ExternalParameter_t from the Uncertainty_t.
 */
LIBSBML_EXTERN
ExternalParameter_t*
Uncertainty_getExternalParameter(Uncertainty_t* u, unsigned int n)
{
  return (u != NULL) ? u->getExternalParameter(n) : NULL;
}


/*
 * Adds a copy of the given ExternalParameter_t to this Uncertainty_t.
 */
LIBSBML_EXTERN
int
Uncertainty_addExternalParameter(Uncertainty_t* u,
                                 const ExternalParameter_t* ep)
{
  return (u != NULL) ? u->addExternalParameter(ep) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of ExternalParameter_t objects in this Uncertainty_t.
 */
LIBSBML_EXTERN
unsigned int
Uncertainty_getNumExternalParameters(Uncertainty_t* u)
{
  return (u != NULL) ? u->getNumExternalParameters() : SBML_INT_MAX;
}


/*
 * Creates a new ExternalParameter_t object, adds it to this Uncertainty_t
 * object and returns the ExternalParameter_t object created.
 */
LIBSBML_EXTERN
ExternalParameter_t*
Uncertainty_createExternalParameter(Uncertainty_t* u)
{
  return (u != NULL) ? u->createExternalParameter() : NULL;
}


/*
 * Removes the nth ExternalParameter_t from this Uncertainty_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
ExternalParameter_t*
Uncertainty_removeExternalParameter(Uncertainty_t* u, unsigned int n)
{
  return (u != NULL) ? u->removeExternalParameter(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Uncertainty_t object have been set.
 */
LIBSBML_EXTERN
int
Uncertainty_hasRequiredAttributes(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * Uncertainty_t object have been set.
 */
LIBSBML_EXTERN
int
Uncertainty_hasRequiredElements(const Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


