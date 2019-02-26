/**
 * @file Uncertainty.h
 * @brief Definition of the Uncertainty class.
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
 *
 * @class Uncertainty
 * @sbmlbrief{distrib} TODO:Definition of the Uncertainty class.
 */


#ifndef Uncertainty_H__
#define Uncertainty_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/UncertValue.h>
#include <sbml/packages/distrib/sbml/UncertStatisticSpan.h>
#include <sbml/packages/distrib/sbml/Distribution.h>
#include <sbml/packages/distrib/sbml/ListOfExternalParameters.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Uncertainty : public DistribBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  UncertValue* mCoefficientOfVariation;
  UncertValue* mKurtosis;
  UncertValue* mMean;
  UncertValue* mMedian;
  UncertValue* mMode;
  UncertValue* mSampleSize;
  UncertValue* mSkewness;
  UncertValue* mStandardDeviation;
  UncertValue* mStandardError;
  UncertValue* mVariance;
  UncertStatisticSpan* mConfidenceInterval;
  UncertStatisticSpan* mCredibleInterval;
  UncertStatisticSpan* mInterquartileRange;
  UncertStatisticSpan* mRange;
  ListOfExternalParameters mExternalParameters;
  Distribution* mDistribution;

  /** @endcond */

public:

  /**
   * Creates a new Uncertainty using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * Uncertainty.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * Uncertainty.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this Uncertainty.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Uncertainty(unsigned int level = DistribExtension::getDefaultLevel(),
              unsigned int version = DistribExtension::getDefaultVersion(),
              unsigned int pkgVersion =
                DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new Uncertainty using the given DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Uncertainty(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for Uncertainty.
   *
   * @param orig the Uncertainty instance to copy.
   */
  Uncertainty(const Uncertainty& orig);


  /**
   * Assignment operator for Uncertainty.
   *
   * @param rhs the Uncertainty object whose values are to be used as the basis
   * of the assignment.
   */
  Uncertainty& operator=(const Uncertainty& rhs);


  /**
   * Creates and returns a deep copy of this Uncertainty object.
   *
   * @return a (deep) copy of this Uncertainty object.
   */
  virtual Uncertainty* clone() const;


  /**
   * Destructor for Uncertainty.
   */
  virtual ~Uncertainty();


  /**
   * Returns the value of the "coefficientOfVariation" element of this
   * Uncertainty.
   *
   * @return the value of the "coefficientOfVariation" element of this
   * Uncertainty as a UncertValue*.
   */
  const UncertValue* getCoefficientOfVariation() const;


  /**
   * Returns the value of the "coefficientOfVariation" element of this
   * Uncertainty.
   *
   * @return the value of the "coefficientOfVariation" element of this
   * Uncertainty as a UncertValue*.
   */
  UncertValue* getCoefficientOfVariation();


  /**
   * Returns the value of the "kurtosis" element of this Uncertainty.
   *
   * @return the value of the "kurtosis" element of this Uncertainty as a
   * UncertValue*.
   */
  const UncertValue* getKurtosis() const;


  /**
   * Returns the value of the "kurtosis" element of this Uncertainty.
   *
   * @return the value of the "kurtosis" element of this Uncertainty as a
   * UncertValue*.
   */
  UncertValue* getKurtosis();


  /**
   * Returns the value of the "mean" element of this Uncertainty.
   *
   * @return the value of the "mean" element of this Uncertainty as a
   * UncertValue*.
   */
  const UncertValue* getMean() const;


  /**
   * Returns the value of the "mean" element of this Uncertainty.
   *
   * @return the value of the "mean" element of this Uncertainty as a
   * UncertValue*.
   */
  UncertValue* getMean();


  /**
   * Returns the value of the "median" element of this Uncertainty.
   *
   * @return the value of the "median" element of this Uncertainty as a
   * UncertValue*.
   */
  const UncertValue* getMedian() const;


  /**
   * Returns the value of the "median" element of this Uncertainty.
   *
   * @return the value of the "median" element of this Uncertainty as a
   * UncertValue*.
   */
  UncertValue* getMedian();


  /**
   * Returns the value of the "mode" element of this Uncertainty.
   *
   * @return the value of the "mode" element of this Uncertainty as a
   * UncertValue*.
   */
  const UncertValue* getMode() const;


  /**
   * Returns the value of the "mode" element of this Uncertainty.
   *
   * @return the value of the "mode" element of this Uncertainty as a
   * UncertValue*.
   */
  UncertValue* getMode();


  /**
   * Returns the value of the "sampleSize" element of this Uncertainty.
   *
   * @return the value of the "sampleSize" element of this Uncertainty as a
   * UncertValue*.
   */
  const UncertValue* getSampleSize() const;


  /**
   * Returns the value of the "sampleSize" element of this Uncertainty.
   *
   * @return the value of the "sampleSize" element of this Uncertainty as a
   * UncertValue*.
   */
  UncertValue* getSampleSize();


  /**
   * Returns the value of the "skewness" element of this Uncertainty.
   *
   * @return the value of the "skewness" element of this Uncertainty as a
   * UncertValue*.
   */
  const UncertValue* getSkewness() const;


  /**
   * Returns the value of the "skewness" element of this Uncertainty.
   *
   * @return the value of the "skewness" element of this Uncertainty as a
   * UncertValue*.
   */
  UncertValue* getSkewness();


  /**
   * Returns the value of the "standardDeviation" element of this Uncertainty.
   *
   * @return the value of the "standardDeviation" element of this Uncertainty
   * as a UncertValue*.
   */
  const UncertValue* getStandardDeviation() const;


  /**
   * Returns the value of the "standardDeviation" element of this Uncertainty.
   *
   * @return the value of the "standardDeviation" element of this Uncertainty
   * as a UncertValue*.
   */
  UncertValue* getStandardDeviation();


  /**
   * Returns the value of the "standardError" element of this Uncertainty.
   *
   * @return the value of the "standardError" element of this Uncertainty as a
   * UncertValue*.
   */
  const UncertValue* getStandardError() const;


  /**
   * Returns the value of the "standardError" element of this Uncertainty.
   *
   * @return the value of the "standardError" element of this Uncertainty as a
   * UncertValue*.
   */
  UncertValue* getStandardError();


  /**
   * Returns the value of the "variance" element of this Uncertainty.
   *
   * @return the value of the "variance" element of this Uncertainty as a
   * UncertValue*.
   */
  const UncertValue* getVariance() const;


  /**
   * Returns the value of the "variance" element of this Uncertainty.
   *
   * @return the value of the "variance" element of this Uncertainty as a
   * UncertValue*.
   */
  UncertValue* getVariance();


  /**
   * Returns the value of the "confidenceInterval" element of this Uncertainty.
   *
   * @return the value of the "confidenceInterval" element of this Uncertainty
   * as a UncertStatisticSpan*.
   */
  const UncertStatisticSpan* getConfidenceInterval() const;


  /**
   * Returns the value of the "confidenceInterval" element of this Uncertainty.
   *
   * @return the value of the "confidenceInterval" element of this Uncertainty
   * as a UncertStatisticSpan*.
   */
  UncertStatisticSpan* getConfidenceInterval();


  /**
   * Returns the value of the "credibleInterval" element of this Uncertainty.
   *
   * @return the value of the "credibleInterval" element of this Uncertainty as
   * a UncertStatisticSpan*.
   */
  const UncertStatisticSpan* getCredibleInterval() const;


  /**
   * Returns the value of the "credibleInterval" element of this Uncertainty.
   *
   * @return the value of the "credibleInterval" element of this Uncertainty as
   * a UncertStatisticSpan*.
   */
  UncertStatisticSpan* getCredibleInterval();


  /**
   * Returns the value of the "interquartileRange" element of this Uncertainty.
   *
   * @return the value of the "interquartileRange" element of this Uncertainty
   * as a UncertStatisticSpan*.
   */
  const UncertStatisticSpan* getInterquartileRange() const;


  /**
   * Returns the value of the "interquartileRange" element of this Uncertainty.
   *
   * @return the value of the "interquartileRange" element of this Uncertainty
   * as a UncertStatisticSpan*.
   */
  UncertStatisticSpan* getInterquartileRange();


  /**
   * Returns the value of the "range" element of this Uncertainty.
   *
   * @return the value of the "range" element of this Uncertainty as a
   * UncertStatisticSpan*.
   */
  const UncertStatisticSpan* getRange() const;


  /**
   * Returns the value of the "range" element of this Uncertainty.
   *
   * @return the value of the "range" element of this Uncertainty as a
   * UncertStatisticSpan*.
   */
  UncertStatisticSpan* getRange();


  /**
   * Returns the value of the "distribution" element of this Uncertainty.
   *
   * @return the value of the "distribution" element of this Uncertainty as a
   * Distribution*.
   */
  const Distribution* getDistribution() const;


  /**
   * Returns the value of the "distribution" element of this Uncertainty.
   *
   * @return the value of the "distribution" element of this Uncertainty as a
   * Distribution*.
   */
  Distribution* getDistribution();


  /**
   * Predicate returning @c true if this Uncertainty's "coefficientOfVariation"
   * element is set.
   *
   * @return @c true if this Uncertainty's "coefficientOfVariation" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetCoefficientOfVariation() const;


  /**
   * Predicate returning @c true if this Uncertainty's "kurtosis" element is
   * set.
   *
   * @return @c true if this Uncertainty's "kurtosis" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetKurtosis() const;


  /**
   * Predicate returning @c true if this Uncertainty's "mean" element is set.
   *
   * @return @c true if this Uncertainty's "mean" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetMean() const;


  /**
   * Predicate returning @c true if this Uncertainty's "median" element is set.
   *
   * @return @c true if this Uncertainty's "median" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetMedian() const;


  /**
   * Predicate returning @c true if this Uncertainty's "mode" element is set.
   *
   * @return @c true if this Uncertainty's "mode" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetMode() const;


  /**
   * Predicate returning @c true if this Uncertainty's "sampleSize" element is
   * set.
   *
   * @return @c true if this Uncertainty's "sampleSize" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetSampleSize() const;


  /**
   * Predicate returning @c true if this Uncertainty's "skewness" element is
   * set.
   *
   * @return @c true if this Uncertainty's "skewness" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetSkewness() const;


  /**
   * Predicate returning @c true if this Uncertainty's "standardDeviation"
   * element is set.
   *
   * @return @c true if this Uncertainty's "standardDeviation" element has been
   * set, otherwise @c false is returned.
   */
  bool isSetStandardDeviation() const;


  /**
   * Predicate returning @c true if this Uncertainty's "standardError" element
   * is set.
   *
   * @return @c true if this Uncertainty's "standardError" element has been
   * set, otherwise @c false is returned.
   */
  bool isSetStandardError() const;


  /**
   * Predicate returning @c true if this Uncertainty's "variance" element is
   * set.
   *
   * @return @c true if this Uncertainty's "variance" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetVariance() const;


  /**
   * Predicate returning @c true if this Uncertainty's "confidenceInterval"
   * element is set.
   *
   * @return @c true if this Uncertainty's "confidenceInterval" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetConfidenceInterval() const;


  /**
   * Predicate returning @c true if this Uncertainty's "credibleInterval"
   * element is set.
   *
   * @return @c true if this Uncertainty's "credibleInterval" element has been
   * set, otherwise @c false is returned.
   */
  bool isSetCredibleInterval() const;


  /**
   * Predicate returning @c true if this Uncertainty's "interquartileRange"
   * element is set.
   *
   * @return @c true if this Uncertainty's "interquartileRange" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetInterquartileRange() const;


  /**
   * Predicate returning @c true if this Uncertainty's "range" element is set.
   *
   * @return @c true if this Uncertainty's "range" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetRange() const;


  /**
   * Predicate returning @c true if this Uncertainty's "distribution" element
   * is set.
   *
   * @return @c true if this Uncertainty's "distribution" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetDistribution() const;


  /**
   * Sets the value of the "coefficientOfVariation" element of this
   * Uncertainty.
   *
   * @param coefficientOfVariation UncertValue* value of the
   * "coefficientOfVariation" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCoefficientOfVariation(const UncertValue* coefficientOfVariation);


  /**
   * Sets the value of the "kurtosis" element of this Uncertainty.
   *
   * @param kurtosis UncertValue* value of the "kurtosis" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setKurtosis(const UncertValue* kurtosis);


  /**
   * Sets the value of the "mean" element of this Uncertainty.
   *
   * @param mean UncertValue* value of the "mean" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMean(const UncertValue* mean);


  /**
   * Sets the value of the "median" element of this Uncertainty.
   *
   * @param median UncertValue* value of the "median" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMedian(const UncertValue* median);


  /**
   * Sets the value of the "mode" element of this Uncertainty.
   *
   * @param mode UncertValue* value of the "mode" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMode(const UncertValue* mode);


  /**
   * Sets the value of the "sampleSize" element of this Uncertainty.
   *
   * @param sampleSize UncertValue* value of the "sampleSize" element to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSampleSize(const UncertValue* sampleSize);


  /**
   * Sets the value of the "skewness" element of this Uncertainty.
   *
   * @param skewness UncertValue* value of the "skewness" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSkewness(const UncertValue* skewness);


  /**
   * Sets the value of the "standardDeviation" element of this Uncertainty.
   *
   * @param standardDeviation UncertValue* value of the "standardDeviation"
   * element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setStandardDeviation(const UncertValue* standardDeviation);


  /**
   * Sets the value of the "standardError" element of this Uncertainty.
   *
   * @param standardError UncertValue* value of the "standardError" element to
   * be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setStandardError(const UncertValue* standardError);


  /**
   * Sets the value of the "variance" element of this Uncertainty.
   *
   * @param variance UncertValue* value of the "variance" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVariance(const UncertValue* variance);


  /**
   * Sets the value of the "confidenceInterval" element of this Uncertainty.
   *
   * @param confidenceInterval UncertStatisticSpan* value of the
   * "confidenceInterval" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setConfidenceInterval(const UncertStatisticSpan* confidenceInterval);


  /**
   * Sets the value of the "credibleInterval" element of this Uncertainty.
   *
   * @param credibleInterval UncertStatisticSpan* value of the
   * "credibleInterval" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCredibleInterval(const UncertStatisticSpan* credibleInterval);


  /**
   * Sets the value of the "interquartileRange" element of this Uncertainty.
   *
   * @param interquartileRange UncertStatisticSpan* value of the
   * "interquartileRange" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setInterquartileRange(const UncertStatisticSpan* interquartileRange);


  /**
   * Sets the value of the "range" element of this Uncertainty.
   *
   * @param range UncertStatisticSpan* value of the "range" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setRange(const UncertStatisticSpan* range);


  /**
   * Sets the value of the "distribution" element of this Uncertainty.
   *
   * @param distribution Distribution* value of the "distribution" element to
   * be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDistribution(const Distribution* distribution);


  /**
   * Creates a new UncertValue object, adds it to this Uncertainty object and
   * returns the UncertValue object created.
   *
   * @return a new UncertValue object instance.
   */
  UncertValue* createCoefficientOfVariation();


  /**
   * Creates a new UncertValue object, adds it to this Uncertainty object and
   * returns the UncertValue object created.
   *
   * @return a new UncertValue object instance.
   */
  UncertValue* createKurtosis();


  /**
   * Creates a new UncertValue object, adds it to this Uncertainty object and
   * returns the UncertValue object created.
   *
   * @return a new UncertValue object instance.
   */
  UncertValue* createMean();


  /**
   * Creates a new UncertValue object, adds it to this Uncertainty object and
   * returns the UncertValue object created.
   *
   * @return a new UncertValue object instance.
   */
  UncertValue* createMedian();


  /**
   * Creates a new UncertValue object, adds it to this Uncertainty object and
   * returns the UncertValue object created.
   *
   * @return a new UncertValue object instance.
   */
  UncertValue* createMode();


  /**
   * Creates a new UncertValue object, adds it to this Uncertainty object and
   * returns the UncertValue object created.
   *
   * @return a new UncertValue object instance.
   */
  UncertValue* createSampleSize();


  /**
   * Creates a new UncertValue object, adds it to this Uncertainty object and
   * returns the UncertValue object created.
   *
   * @return a new UncertValue object instance.
   */
  UncertValue* createSkewness();


  /**
   * Creates a new UncertValue object, adds it to this Uncertainty object and
   * returns the UncertValue object created.
   *
   * @return a new UncertValue object instance.
   */
  UncertValue* createStandardDeviation();


  /**
   * Creates a new UncertValue object, adds it to this Uncertainty object and
   * returns the UncertValue object created.
   *
   * @return a new UncertValue object instance.
   */
  UncertValue* createStandardError();


  /**
   * Creates a new UncertValue object, adds it to this Uncertainty object and
   * returns the UncertValue object created.
   *
   * @return a new UncertValue object instance.
   */
  UncertValue* createVariance();


  /**
   * Creates a new UncertStatisticSpan object, adds it to this Uncertainty
   * object and returns the UncertStatisticSpan object created.
   *
   * @return a new UncertStatisticSpan object instance.
   */
  UncertStatisticSpan* createConfidenceInterval();


  /**
   * Creates a new UncertStatisticSpan object, adds it to this Uncertainty
   * object and returns the UncertStatisticSpan object created.
   *
   * @return a new UncertStatisticSpan object instance.
   */
  UncertStatisticSpan* createCredibleInterval();


  /**
   * Creates a new UncertStatisticSpan object, adds it to this Uncertainty
   * object and returns the UncertStatisticSpan object created.
   *
   * @return a new UncertStatisticSpan object instance.
   */
  UncertStatisticSpan* createInterquartileRange();


  /**
   * Creates a new UncertStatisticSpan object, adds it to this Uncertainty
   * object and returns the UncertStatisticSpan object created.
   *
   * @return a new UncertStatisticSpan object instance.
   */
  UncertStatisticSpan* createRange();


  /**
   * Creates a new Distribution object, adds it to this Uncertainty object and
   * returns the Distribution object created.
   *
   * @return a new Distribution object instance.
   */
  Distribution* createDistribution();


  /**
   * Unsets the value of the "coefficientOfVariation" element of this
   * Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCoefficientOfVariation();


  /**
   * Unsets the value of the "kurtosis" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetKurtosis();


  /**
   * Unsets the value of the "mean" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMean();


  /**
   * Unsets the value of the "median" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMedian();


  /**
   * Unsets the value of the "mode" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMode();


  /**
   * Unsets the value of the "sampleSize" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSampleSize();


  /**
   * Unsets the value of the "skewness" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSkewness();


  /**
   * Unsets the value of the "standardDeviation" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStandardDeviation();


  /**
   * Unsets the value of the "standardError" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStandardError();


  /**
   * Unsets the value of the "variance" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVariance();


  /**
   * Unsets the value of the "confidenceInterval" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetConfidenceInterval();


  /**
   * Unsets the value of the "credibleInterval" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCredibleInterval();


  /**
   * Unsets the value of the "interquartileRange" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetInterquartileRange();


  /**
   * Unsets the value of the "range" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRange();


  /**
   * Unsets the value of the "distribution" element of this Uncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDistribution();


  /**
   * Returns the ListOfExternalParameters from this Uncertainty.
   *
   * @return the ListOfExternalParameters from this Uncertainty.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  const ListOfExternalParameters* getListOfExternalParameters() const;


  /**
   * Returns the ListOfExternalParameters from this Uncertainty.
   *
   * @return the ListOfExternalParameters from this Uncertainty.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  ListOfExternalParameters* getListOfExternalParameters();


  /**
   * Get an ExternalParameter from the Uncertainty.
   *
   * @param n an unsigned int representing the index of the ExternalParameter
   * to retrieve.
   *
   * @return the nth ExternalParameter in the ListOfExternalParameters within
   * this Uncertainty.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  ExternalParameter* getExternalParameter(unsigned int n);


  /**
   * Get an ExternalParameter from the Uncertainty.
   *
   * @param n an unsigned int representing the index of the ExternalParameter
   * to retrieve.
   *
   * @return the nth ExternalParameter in the ListOfExternalParameters within
   * this Uncertainty.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  const ExternalParameter* getExternalParameter(unsigned int n) const;


  /**
   * Adds a copy of the given ExternalParameter to this Uncertainty.
   *
   * @param ep the ExternalParameter object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  int addExternalParameter(const ExternalParameter* ep);


  /**
   * Get the number of ExternalParameter objects in this Uncertainty.
   *
   * @return the number of ExternalParameter objects in this Uncertainty.
   *
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  unsigned int getNumExternalParameters() const;


  /**
   * Creates a new ExternalParameter object, adds it to this Uncertainty object
   * and returns the ExternalParameter object created.
   *
   * @return a new ExternalParameter object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   * @see removeExternalParameter(unsigned int n)
   */
  ExternalParameter* createExternalParameter();


  /**
   * Removes the nth ExternalParameter from this Uncertainty and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the ExternalParameter
   * to remove.
   *
   * @return a pointer to the nth ExternalParameter in this Uncertainty.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addExternalParameter(const ExternalParameter* object)
   * @see createExternalParameter()
   * @see getExternalParameter(const std::string& sid)
   * @see getExternalParameter(unsigned int n)
   * @see getNumExternalParameters()
   * @see removeExternalParameter(const std::string& sid)
   */
  ExternalParameter* removeExternalParameter(unsigned int n);


  /**
   * Returns the XML element name of this Uncertainty object.
   *
   * For Uncertainty, the XML element name is always @c "uncertainty".
   *
   * @return the name of this element, i.e. @c "uncertainty".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Uncertainty object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_UNCERTAINTY, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * Uncertainty object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * Uncertainty have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * Uncertainty object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * Uncertainty have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the Uncertainty object are:
   */
  virtual bool hasRequiredElements() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Write any contained elements
   */
  virtual void writeElements(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument
   */
  virtual void setSBMLDocument(SBMLDocument* d);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to child elements
   */
  virtual void connectToChild();

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Updates the namespaces when setLevelVersion is used
   */
  virtual void updateSBMLNamespace(const std::string& package,
                                   unsigned int level,
                                   unsigned int version);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this Uncertainty.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, bool& value)
    const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this Uncertainty.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this Uncertainty.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           double& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this Uncertainty.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           unsigned int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this Uncertainty.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           std::string& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Predicate returning @c true if this Uncertainty's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this Uncertainty's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Uncertainty.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, bool value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Uncertainty.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Uncertainty.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, double value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Uncertainty.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           unsigned int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Uncertainty.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           const std::string& value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Unsets the value of the "attributeName" attribute of this Uncertainty.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates and returns an new "elementName" object in this Uncertainty.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this Uncertainty.
   *
   * @param elementName, the name of the element to create.
   *
   * @param element, pointer to the element to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int addChildObject(const std::string& elementName,
                             const SBase* element);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Removes and returns the new "elementName" object with the given id in this
   * Uncertainty.
   *
   * @param elementName, the name of the element to remove.
   *
   * @param id, the id of the element to remove.
   *
   * @return pointer to the element removed.
   */
  virtual SBase* removeChildObject(const std::string& elementName,
                                   const std::string& id);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the number of "elementName" in this Uncertainty.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this Uncertainty.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @param index, unsigned int the index of the object to retrieve.
   *
   * @return pointer to the object.
   */
  virtual SBase* getObject(const std::string& elementName, unsigned int index);

  /** @endcond */




  #endif /* !SWIG */


  /**
   * Returns the first child element that has the given @p id in the model-wide
   * SId namespace, or @c NULL if no such object is found.
   *
   * @param id a string representing the id attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p id. If no such
   * object is found, this method returns @c NULL.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element that has the given @p metaid, or @c NULL
   * if no such object is found.
   *
   * @param metaid a string representing the metaid attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p metaid. If no
   * such object is found this method returns @c NULL.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @param filter an ElementFilter that may impose restrictions on the objects
   * to be retrieved.
   *
   * @return a List* pointer of pointers to all SBase child objects with any
   * restriction imposed.
   */
  virtual List* getAllElements(ElementFilter * filter = NULL);


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new object from the next XMLToken on the XMLInputStream
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds the expected attributes for this element
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual void readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new Uncertainty_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Uncertainty_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Uncertainty_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this Uncertainty_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
Uncertainty_t *
Uncertainty_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Uncertainty_t object.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return a (deep) copy of this Uncertainty_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
Uncertainty_t*
Uncertainty_clone(const Uncertainty_t* u);


/**
 * Frees this Uncertainty_t object.
 *
 * @param u the Uncertainty_t structure.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
void
Uncertainty_free(Uncertainty_t* u);


/**
 * Returns the value of the "coefficientOfVariation" element of this
 * Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose coefficientOfVariation is sought.
 *
 * @return the value of the "coefficientOfVariation" element of this
 * Uncertainty_t as a UncertValue*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getCoefficientOfVariation(const Uncertainty_t * u);


/**
 * Returns the value of the "kurtosis" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose kurtosis is sought.
 *
 * @return the value of the "kurtosis" element of this Uncertainty_t as a
 * UncertValue*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getKurtosis(const Uncertainty_t * u);


/**
 * Returns the value of the "mean" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose mean is sought.
 *
 * @return the value of the "mean" element of this Uncertainty_t as a
 * UncertValue*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getMean(const Uncertainty_t * u);


/**
 * Returns the value of the "median" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose median is sought.
 *
 * @return the value of the "median" element of this Uncertainty_t as a
 * UncertValue*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getMedian(const Uncertainty_t * u);


/**
 * Returns the value of the "mode" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose mode is sought.
 *
 * @return the value of the "mode" element of this Uncertainty_t as a
 * UncertValue*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getMode(const Uncertainty_t * u);


/**
 * Returns the value of the "sampleSize" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose sampleSize is sought.
 *
 * @return the value of the "sampleSize" element of this Uncertainty_t as a
 * UncertValue*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getSampleSize(const Uncertainty_t * u);


/**
 * Returns the value of the "skewness" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose skewness is sought.
 *
 * @return the value of the "skewness" element of this Uncertainty_t as a
 * UncertValue*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getSkewness(const Uncertainty_t * u);


/**
 * Returns the value of the "standardDeviation" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose standardDeviation is sought.
 *
 * @return the value of the "standardDeviation" element of this Uncertainty_t
 * as a UncertValue*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getStandardDeviation(const Uncertainty_t * u);


/**
 * Returns the value of the "standardError" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose standardError is sought.
 *
 * @return the value of the "standardError" element of this Uncertainty_t as a
 * UncertValue*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getStandardError(const Uncertainty_t * u);


/**
 * Returns the value of the "variance" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose variance is sought.
 *
 * @return the value of the "variance" element of this Uncertainty_t as a
 * UncertValue*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertValue_t*
Uncertainty_getVariance(const Uncertainty_t * u);


/**
 * Returns the value of the "confidenceInterval" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose confidenceInterval is sought.
 *
 * @return the value of the "confidenceInterval" element of this Uncertainty_t
 * as a UncertStatisticSpan*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertStatisticSpan_t*
Uncertainty_getConfidenceInterval(const Uncertainty_t * u);


/**
 * Returns the value of the "credibleInterval" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose credibleInterval is sought.
 *
 * @return the value of the "credibleInterval" element of this Uncertainty_t as
 * a UncertStatisticSpan*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertStatisticSpan_t*
Uncertainty_getCredibleInterval(const Uncertainty_t * u);


/**
 * Returns the value of the "interquartileRange" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose interquartileRange is sought.
 *
 * @return the value of the "interquartileRange" element of this Uncertainty_t
 * as a UncertStatisticSpan*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertStatisticSpan_t*
Uncertainty_getInterquartileRange(const Uncertainty_t * u);


/**
 * Returns the value of the "range" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose range is sought.
 *
 * @return the value of the "range" element of this Uncertainty_t as a
 * UncertStatisticSpan*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const UncertStatisticSpan_t*
Uncertainty_getRange(const Uncertainty_t * u);


/**
 * Returns the value of the "distribution" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose distribution is sought.
 *
 * @return the value of the "distribution" element of this Uncertainty_t as a
 * Distribution*.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
const Distribution_t*
Uncertainty_getDistribution(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's
 * "coefficientOfVariation" element is set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "coefficientOfVariation" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetCoefficientOfVariation(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "kurtosis" element
 * is set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "kurtosis" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetKurtosis(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "mean" element is
 * set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "mean" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetMean(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "median" element is
 * set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "median" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetMedian(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "mode" element is
 * set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "mode" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetMode(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "sampleSize" element
 * is set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "sampleSize" element has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetSampleSize(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "skewness" element
 * is set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "skewness" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetSkewness(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "standardDeviation"
 * element is set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "standardDeviation" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetStandardDeviation(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "standardError"
 * element is set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "standardError" element has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetStandardError(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "variance" element
 * is set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "variance" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetVariance(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "confidenceInterval"
 * element is set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "confidenceInterval" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetConfidenceInterval(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "credibleInterval"
 * element is set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "credibleInterval" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetCredibleInterval(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "interquartileRange"
 * element is set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "interquartileRange" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetInterquartileRange(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "range" element is
 * set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "range" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetRange(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if this Uncertainty_t's "distribution"
 * element is set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) if this Uncertainty_t's "distribution" element has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_isSetDistribution(const Uncertainty_t * u);


/**
 * Sets the value of the "coefficientOfVariation" element of this
 * Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param coefficientOfVariation UncertValue_t* value of the
 * "coefficientOfVariation" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setCoefficientOfVariation(Uncertainty_t * u,
                                      const UncertValue_t*
                                        coefficientOfVariation);


/**
 * Sets the value of the "kurtosis" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param kurtosis UncertValue_t* value of the "kurtosis" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setKurtosis(Uncertainty_t * u, const UncertValue_t* kurtosis);


/**
 * Sets the value of the "mean" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param mean UncertValue_t* value of the "mean" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setMean(Uncertainty_t * u, const UncertValue_t* mean);


/**
 * Sets the value of the "median" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param median UncertValue_t* value of the "median" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setMedian(Uncertainty_t * u, const UncertValue_t* median);


/**
 * Sets the value of the "mode" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param mode UncertValue_t* value of the "mode" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setMode(Uncertainty_t * u, const UncertValue_t* mode);


/**
 * Sets the value of the "sampleSize" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param sampleSize UncertValue_t* value of the "sampleSize" element to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setSampleSize(Uncertainty_t * u, const UncertValue_t* sampleSize);


/**
 * Sets the value of the "skewness" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param skewness UncertValue_t* value of the "skewness" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setSkewness(Uncertainty_t * u, const UncertValue_t* skewness);


/**
 * Sets the value of the "standardDeviation" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param standardDeviation UncertValue_t* value of the "standardDeviation"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setStandardDeviation(Uncertainty_t * u,
                                 const UncertValue_t* standardDeviation);


/**
 * Sets the value of the "standardError" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param standardError UncertValue_t* value of the "standardError" element to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setStandardError(Uncertainty_t * u,
                             const UncertValue_t* standardError);


/**
 * Sets the value of the "variance" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param variance UncertValue_t* value of the "variance" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setVariance(Uncertainty_t * u, const UncertValue_t* variance);


/**
 * Sets the value of the "confidenceInterval" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param confidenceInterval UncertStatisticSpan_t* value of the
 * "confidenceInterval" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setConfidenceInterval(Uncertainty_t * u,
                                  const UncertStatisticSpan_t*
                                    confidenceInterval);


/**
 * Sets the value of the "credibleInterval" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param credibleInterval UncertStatisticSpan_t* value of the
 * "credibleInterval" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setCredibleInterval(Uncertainty_t * u,
                                const UncertStatisticSpan_t* credibleInterval);


/**
 * Sets the value of the "interquartileRange" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param interquartileRange UncertStatisticSpan_t* value of the
 * "interquartileRange" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setInterquartileRange(Uncertainty_t * u,
                                  const UncertStatisticSpan_t*
                                    interquartileRange);


/**
 * Sets the value of the "range" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param range UncertStatisticSpan_t* value of the "range" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setRange(Uncertainty_t * u, const UncertStatisticSpan_t* range);


/**
 * Sets the value of the "distribution" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @param distribution Distribution_t* value of the "distribution" element to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_setDistribution(Uncertainty_t * u,
                            const Distribution_t* distribution);


/**
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertValue_t should be
 * added.
 *
 * @return a new UncertValue_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createCoefficientOfVariation(Uncertainty_t* u);


/**
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertValue_t should be
 * added.
 *
 * @return a new UncertValue_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createKurtosis(Uncertainty_t* u);


/**
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertValue_t should be
 * added.
 *
 * @return a new UncertValue_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createMean(Uncertainty_t* u);


/**
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertValue_t should be
 * added.
 *
 * @return a new UncertValue_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createMedian(Uncertainty_t* u);


/**
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertValue_t should be
 * added.
 *
 * @return a new UncertValue_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createMode(Uncertainty_t* u);


/**
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertValue_t should be
 * added.
 *
 * @return a new UncertValue_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createSampleSize(Uncertainty_t* u);


/**
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertValue_t should be
 * added.
 *
 * @return a new UncertValue_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createSkewness(Uncertainty_t* u);


/**
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertValue_t should be
 * added.
 *
 * @return a new UncertValue_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createStandardDeviation(Uncertainty_t* u);


/**
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertValue_t should be
 * added.
 *
 * @return a new UncertValue_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createStandardError(Uncertainty_t* u);


/**
 * Creates a new UncertValue_t object, adds it to this Uncertainty_t object and
 * returns the UncertValue_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertValue_t should be
 * added.
 *
 * @return a new UncertValue_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertValue_t*
Uncertainty_createVariance(Uncertainty_t* u);


/**
 * Creates a new UncertStatisticSpan_t object, adds it to this Uncertainty_t
 * object and returns the UncertStatisticSpan_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertStatisticSpan_t
 * should be added.
 *
 * @return a new UncertStatisticSpan_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertStatisticSpan_t*
Uncertainty_createConfidenceInterval(Uncertainty_t* u);


/**
 * Creates a new UncertStatisticSpan_t object, adds it to this Uncertainty_t
 * object and returns the UncertStatisticSpan_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertStatisticSpan_t
 * should be added.
 *
 * @return a new UncertStatisticSpan_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertStatisticSpan_t*
Uncertainty_createCredibleInterval(Uncertainty_t* u);


/**
 * Creates a new UncertStatisticSpan_t object, adds it to this Uncertainty_t
 * object and returns the UncertStatisticSpan_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertStatisticSpan_t
 * should be added.
 *
 * @return a new UncertStatisticSpan_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertStatisticSpan_t*
Uncertainty_createInterquartileRange(Uncertainty_t* u);


/**
 * Creates a new UncertStatisticSpan_t object, adds it to this Uncertainty_t
 * object and returns the UncertStatisticSpan_t object created.
 *
 * @param u the Uncertainty_t structure to which the UncertStatisticSpan_t
 * should be added.
 *
 * @return a new UncertStatisticSpan_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
UncertStatisticSpan_t*
Uncertainty_createRange(Uncertainty_t* u);


/**
 * Creates a new Distribution_t object, adds it to this Uncertainty_t object
 * and returns the Distribution_t object created.
 *
 * @param u the Uncertainty_t structure to which the Distribution_t should be
 * added.
 *
 * @return a new Distribution_t object instance.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
Distribution_t*
Uncertainty_createDistribution(Uncertainty_t* u);


/**
 * Unsets the value of the "coefficientOfVariation" element of this
 * Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetCoefficientOfVariation(Uncertainty_t * u);


/**
 * Unsets the value of the "kurtosis" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetKurtosis(Uncertainty_t * u);


/**
 * Unsets the value of the "mean" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetMean(Uncertainty_t * u);


/**
 * Unsets the value of the "median" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetMedian(Uncertainty_t * u);


/**
 * Unsets the value of the "mode" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetMode(Uncertainty_t * u);


/**
 * Unsets the value of the "sampleSize" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetSampleSize(Uncertainty_t * u);


/**
 * Unsets the value of the "skewness" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetSkewness(Uncertainty_t * u);


/**
 * Unsets the value of the "standardDeviation" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetStandardDeviation(Uncertainty_t * u);


/**
 * Unsets the value of the "standardError" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetStandardError(Uncertainty_t * u);


/**
 * Unsets the value of the "variance" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetVariance(Uncertainty_t * u);


/**
 * Unsets the value of the "confidenceInterval" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetConfidenceInterval(Uncertainty_t * u);


/**
 * Unsets the value of the "credibleInterval" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetCredibleInterval(Uncertainty_t * u);


/**
 * Unsets the value of the "interquartileRange" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetInterquartileRange(Uncertainty_t * u);


/**
 * Unsets the value of the "range" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetRange(Uncertainty_t * u);


/**
 * Unsets the value of the "distribution" element of this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_unsetDistribution(Uncertainty_t * u);


/**
 * Returns a ListOf_t * containing ExternalParameter_t objects from this
 * Uncertainty_t.
 *
 * @param u the Uncertainty_t structure whose ListOfExternalParameters is
 * sought.
 *
 * @return the ListOfExternalParameters from this Uncertainty_t as a ListOf_t
 * *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see Uncertainty_addExternalParameter()
 * @see Uncertainty_createExternalParameter()
 * @see Uncertainty_getExternalParameterById()
 * @see Uncertainty_getExternalParameter()
 * @see Uncertainty_getNumExternalParameters()
 * @see Uncertainty_removeExternalParameterById()
 * @see Uncertainty_removeExternalParameter()
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
ListOf_t*
Uncertainty_getListOfExternalParameters(Uncertainty_t* u);


/**
 * Get an ExternalParameter_t from the Uncertainty_t.
 *
 * @param u the Uncertainty_t structure to search.
 *
 * @param n an unsigned int representing the index of the ExternalParameter_t
 * to retrieve.
 *
 * @return the nth ExternalParameter_t in the ListOfExternalParameters within
 * this Uncertainty.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
ExternalParameter_t*
Uncertainty_getExternalParameter(Uncertainty_t* u, unsigned int n);


/**
 * Adds a copy of the given ExternalParameter_t to this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure to which the ExternalParameter_t should
 * be added.
 *
 * @param ep the ExternalParameter_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_addExternalParameter(Uncertainty_t* u,
                                 const ExternalParameter_t* ep);


/**
 * Get the number of ExternalParameter_t objects in this Uncertainty_t.
 *
 * @param u the Uncertainty_t structure to query.
 *
 * @return the number of ExternalParameter_t objects in this Uncertainty_t.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
unsigned int
Uncertainty_getNumExternalParameters(Uncertainty_t* u);


/**
 * Creates a new ExternalParameter_t object, adds it to this Uncertainty_t
 * object and returns the ExternalParameter_t object created.
 *
 * @param u the Uncertainty_t structure to which the ExternalParameter_t should
 * be added.
 *
 * @return a new ExternalParameter_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
ExternalParameter_t*
Uncertainty_createExternalParameter(Uncertainty_t* u);


/**
 * Removes the nth ExternalParameter_t from this Uncertainty_t and returns a
 * pointer to it.
 *
 * @param u the Uncertainty_t structure to search.
 *
 * @param n an unsigned int representing the index of the ExternalParameter_t
 * to remove.
 *
 * @return a pointer to the nth ExternalParameter_t in this Uncertainty_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
ExternalParameter_t*
Uncertainty_removeExternalParameter(Uncertainty_t* u, unsigned int n);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Uncertainty_t object have been set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * Uncertainty_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_hasRequiredAttributes(const Uncertainty_t * u);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * Uncertainty_t object have been set.
 *
 * @param u the Uncertainty_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * Uncertainty_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the Uncertainty_t object are:
 *
 * @memberof Uncertainty_t
 */
LIBSBML_EXTERN
int
Uncertainty_hasRequiredElements(const Uncertainty_t * u);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Uncertainty_H__ */


