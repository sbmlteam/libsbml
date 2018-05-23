/**
 * @file DistribUncertStatistics.h
 * @brief Definition of the DistribUncertStatistics class.
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
 * @class DistribUncertStatistics
 * @sbmlbrief{distrib} TODO:Definition of the DistribUncertStatistics class.
 */


#ifndef DistribUncertStatistics_H__
#define DistribUncertStatistics_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertValue.h>
#include <sbml/packages/distrib/sbml/DistribUncertStatisticSpan.h>
#include <sbml/packages/distrib/sbml/ListOfExternalParameters.h>
#include <sbml/packages/distrib/sbml/DistribBase.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribUncertStatistics : public DistribBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribUncertValue* mCoefficientOfVariation;
  DistribUncertValue* mKurtosis;
  DistribUncertValue* mMean;
  DistribUncertValue* mMedian;
  DistribUncertValue* mMode;
  DistribUncertValue* mSkewness;
  DistribUncertValue* mStandardDeviation;
  DistribUncertValue* mVariance;
  DistribUncertStatisticSpan* mConfidenceInterval;
  DistribUncertStatisticSpan* mCredibleInterval;
  DistribUncertStatisticSpan* mInterquartileRange;
  DistribUncertStatisticSpan* mRange;
  ListOfExternalParameters mDistribExternalParameters;
  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new DistribUncertStatistics using the given SBML Level, Version
   * and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribUncertStatistics.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribUncertStatistics.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribUncertStatistics.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUncertStatistics(
                          unsigned int level =
                            DistribExtension::getDefaultLevel(),
                          unsigned int version =
                            DistribExtension::getDefaultVersion(),
                          unsigned int pkgVersion =
                            DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribUncertStatistics using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUncertStatistics(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribUncertStatistics.
   *
   * @param orig the DistribUncertStatistics instance to copy.
   */
  DistribUncertStatistics(const DistribUncertStatistics& orig);


  /**
   * Assignment operator for DistribUncertStatistics.
   *
   * @param rhs the DistribUncertStatistics object whose values are to be used
   * as the basis of the assignment.
   */
  DistribUncertStatistics& operator=(const DistribUncertStatistics& rhs);


  /**
   * Creates and returns a deep copy of this DistribUncertStatistics object.
   *
   * @return a (deep) copy of this DistribUncertStatistics object.
   */
  virtual DistribUncertStatistics* clone() const;


  /**
   * Destructor for DistribUncertStatistics.
   */
  virtual ~DistribUncertStatistics();


  /**
   * Returns the value of the "id" attribute of this DistribUncertStatistics.
   *
   * @return the value of the "id" attribute of this DistribUncertStatistics as
   * a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this DistribUncertStatistics.
   *
   * @return the value of the "name" attribute of this DistribUncertStatistics
   * as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's "id"
   * attribute is set.
   *
   * @return @c true if this DistribUncertStatistics's "id" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's "name"
   * attribute is set.
   *
   * @return @c true if this DistribUncertStatistics's "name" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this DistribUncertStatistics.
   *
   * @param id std::string& value of the "id" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * Calling this function with @p id = @c NULL or an empty string is
   * equivalent to calling unsetId().
   */
  virtual int setId(const std::string& id);


  /**
   * Sets the value of the "name" attribute of this DistribUncertStatistics.
   *
   * @param name std::string& value of the "name" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p name = @c NULL or an empty string is
   * equivalent to calling unsetName().
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "id" attribute of this DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "coefficientOfVariation" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "coefficientOfVariation" element of this
   * DistribUncertStatistics as a DistribUncertValue*.
   */
  const DistribUncertValue* getCoefficientOfVariation() const;


  /**
   * Returns the value of the "coefficientOfVariation" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "coefficientOfVariation" element of this
   * DistribUncertStatistics as a DistribUncertValue*.
   */
  DistribUncertValue* getCoefficientOfVariation();


  /**
   * Returns the value of the "kurtosis" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "kurtosis" element of this
   * DistribUncertStatistics as a DistribUncertValue*.
   */
  const DistribUncertValue* getKurtosis() const;


  /**
   * Returns the value of the "kurtosis" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "kurtosis" element of this
   * DistribUncertStatistics as a DistribUncertValue*.
   */
  DistribUncertValue* getKurtosis();


  /**
   * Returns the value of the "mean" element of this DistribUncertStatistics.
   *
   * @return the value of the "mean" element of this DistribUncertStatistics as
   * a DistribUncertValue*.
   */
  const DistribUncertValue* getMean() const;


  /**
   * Returns the value of the "mean" element of this DistribUncertStatistics.
   *
   * @return the value of the "mean" element of this DistribUncertStatistics as
   * a DistribUncertValue*.
   */
  DistribUncertValue* getMean();


  /**
   * Returns the value of the "median" element of this DistribUncertStatistics.
   *
   * @return the value of the "median" element of this DistribUncertStatistics
   * as a DistribUncertValue*.
   */
  const DistribUncertValue* getMedian() const;


  /**
   * Returns the value of the "median" element of this DistribUncertStatistics.
   *
   * @return the value of the "median" element of this DistribUncertStatistics
   * as a DistribUncertValue*.
   */
  DistribUncertValue* getMedian();


  /**
   * Returns the value of the "mode" element of this DistribUncertStatistics.
   *
   * @return the value of the "mode" element of this DistribUncertStatistics as
   * a DistribUncertValue*.
   */
  const DistribUncertValue* getMode() const;


  /**
   * Returns the value of the "mode" element of this DistribUncertStatistics.
   *
   * @return the value of the "mode" element of this DistribUncertStatistics as
   * a DistribUncertValue*.
   */
  DistribUncertValue* getMode();


  /**
   * Returns the value of the "skewness" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "skewness" element of this
   * DistribUncertStatistics as a DistribUncertValue*.
   */
  const DistribUncertValue* getSkewness() const;


  /**
   * Returns the value of the "skewness" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "skewness" element of this
   * DistribUncertStatistics as a DistribUncertValue*.
   */
  DistribUncertValue* getSkewness();


  /**
   * Returns the value of the "standardDeviation" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "standardDeviation" element of this
   * DistribUncertStatistics as a DistribUncertValue*.
   */
  const DistribUncertValue* getStandardDeviation() const;


  /**
   * Returns the value of the "standardDeviation" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "standardDeviation" element of this
   * DistribUncertStatistics as a DistribUncertValue*.
   */
  DistribUncertValue* getStandardDeviation();


  /**
   * Returns the value of the "variance" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "variance" element of this
   * DistribUncertStatistics as a DistribUncertValue*.
   */
  const DistribUncertValue* getVariance() const;


  /**
   * Returns the value of the "variance" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "variance" element of this
   * DistribUncertStatistics as a DistribUncertValue*.
   */
  DistribUncertValue* getVariance();


  /**
   * Returns the value of the "confidenceInterval" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "confidenceInterval" element of this
   * DistribUncertStatistics as a DistribUncertStatisticSpan*.
   */
  const DistribUncertStatisticSpan* getConfidenceInterval() const;


  /**
   * Returns the value of the "confidenceInterval" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "confidenceInterval" element of this
   * DistribUncertStatistics as a DistribUncertStatisticSpan*.
   */
  DistribUncertStatisticSpan* getConfidenceInterval();


  /**
   * Returns the value of the "credibleInterval" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "credibleInterval" element of this
   * DistribUncertStatistics as a DistribUncertStatisticSpan*.
   */
  const DistribUncertStatisticSpan* getCredibleInterval() const;


  /**
   * Returns the value of the "credibleInterval" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "credibleInterval" element of this
   * DistribUncertStatistics as a DistribUncertStatisticSpan*.
   */
  DistribUncertStatisticSpan* getCredibleInterval();


  /**
   * Returns the value of the "interquartileRange" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "interquartileRange" element of this
   * DistribUncertStatistics as a DistribUncertStatisticSpan*.
   */
  const DistribUncertStatisticSpan* getInterquartileRange() const;


  /**
   * Returns the value of the "interquartileRange" element of this
   * DistribUncertStatistics.
   *
   * @return the value of the "interquartileRange" element of this
   * DistribUncertStatistics as a DistribUncertStatisticSpan*.
   */
  DistribUncertStatisticSpan* getInterquartileRange();


  /**
   * Returns the value of the "range" element of this DistribUncertStatistics.
   *
   * @return the value of the "range" element of this DistribUncertStatistics
   * as a DistribUncertStatisticSpan*.
   */
  const DistribUncertStatisticSpan* getRange() const;


  /**
   * Returns the value of the "range" element of this DistribUncertStatistics.
   *
   * @return the value of the "range" element of this DistribUncertStatistics
   * as a DistribUncertStatisticSpan*.
   */
  DistribUncertStatisticSpan* getRange();


  /**
   * Predicate returning @c true if this DistribUncertStatistics's
   * "coefficientOfVariation" element is set.
   *
   * @return @c true if this DistribUncertStatistics's "coefficientOfVariation"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetCoefficientOfVariation() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's "kurtosis"
   * element is set.
   *
   * @return @c true if this DistribUncertStatistics's "kurtosis" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetKurtosis() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's "mean"
   * element is set.
   *
   * @return @c true if this DistribUncertStatistics's "mean" element has been
   * set, otherwise @c false is returned.
   */
  bool isSetMean() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's "median"
   * element is set.
   *
   * @return @c true if this DistribUncertStatistics's "median" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetMedian() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's "mode"
   * element is set.
   *
   * @return @c true if this DistribUncertStatistics's "mode" element has been
   * set, otherwise @c false is returned.
   */
  bool isSetMode() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's "skewness"
   * element is set.
   *
   * @return @c true if this DistribUncertStatistics's "skewness" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetSkewness() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's
   * "standardDeviation" element is set.
   *
   * @return @c true if this DistribUncertStatistics's "standardDeviation"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetStandardDeviation() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's "variance"
   * element is set.
   *
   * @return @c true if this DistribUncertStatistics's "variance" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetVariance() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's
   * "confidenceInterval" element is set.
   *
   * @return @c true if this DistribUncertStatistics's "confidenceInterval"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetConfidenceInterval() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's
   * "credibleInterval" element is set.
   *
   * @return @c true if this DistribUncertStatistics's "credibleInterval"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetCredibleInterval() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's
   * "interquartileRange" element is set.
   *
   * @return @c true if this DistribUncertStatistics's "interquartileRange"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetInterquartileRange() const;


  /**
   * Predicate returning @c true if this DistribUncertStatistics's "range"
   * element is set.
   *
   * @return @c true if this DistribUncertStatistics's "range" element has been
   * set, otherwise @c false is returned.
   */
  bool isSetRange() const;


  /**
   * Sets the value of the "coefficientOfVariation" element of this
   * DistribUncertStatistics.
   *
   * @param coefficientOfVariation DistribUncertValue* value of the
   * "coefficientOfVariation" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCoefficientOfVariation(const DistribUncertValue*
    coefficientOfVariation);


  /**
   * Sets the value of the "kurtosis" element of this DistribUncertStatistics.
   *
   * @param kurtosis DistribUncertValue* value of the "kurtosis" element to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setKurtosis(const DistribUncertValue* kurtosis);


  /**
   * Sets the value of the "mean" element of this DistribUncertStatistics.
   *
   * @param mean DistribUncertValue* value of the "mean" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMean(const DistribUncertValue* mean);


  /**
   * Sets the value of the "median" element of this DistribUncertStatistics.
   *
   * @param median DistribUncertValue* value of the "median" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMedian(const DistribUncertValue* median);


  /**
   * Sets the value of the "mode" element of this DistribUncertStatistics.
   *
   * @param mode DistribUncertValue* value of the "mode" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setMode(const DistribUncertValue* mode);


  /**
   * Sets the value of the "skewness" element of this DistribUncertStatistics.
   *
   * @param skewness DistribUncertValue* value of the "skewness" element to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSkewness(const DistribUncertValue* skewness);


  /**
   * Sets the value of the "standardDeviation" element of this
   * DistribUncertStatistics.
   *
   * @param standardDeviation DistribUncertValue* value of the
   * "standardDeviation" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setStandardDeviation(const DistribUncertValue* standardDeviation);


  /**
   * Sets the value of the "variance" element of this DistribUncertStatistics.
   *
   * @param variance DistribUncertValue* value of the "variance" element to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVariance(const DistribUncertValue* variance);


  /**
   * Sets the value of the "confidenceInterval" element of this
   * DistribUncertStatistics.
   *
   * @param confidenceInterval DistribUncertStatisticSpan* value of the
   * "confidenceInterval" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setConfidenceInterval(const DistribUncertStatisticSpan*
    confidenceInterval);


  /**
   * Sets the value of the "credibleInterval" element of this
   * DistribUncertStatistics.
   *
   * @param credibleInterval DistribUncertStatisticSpan* value of the
   * "credibleInterval" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCredibleInterval(const DistribUncertStatisticSpan* credibleInterval);


  /**
   * Sets the value of the "interquartileRange" element of this
   * DistribUncertStatistics.
   *
   * @param interquartileRange DistribUncertStatisticSpan* value of the
   * "interquartileRange" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setInterquartileRange(const DistribUncertStatisticSpan*
    interquartileRange);


  /**
   * Sets the value of the "range" element of this DistribUncertStatistics.
   *
   * @param range DistribUncertStatisticSpan* value of the "range" element to
   * be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setRange(const DistribUncertStatisticSpan* range);


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createCoefficientOfVariation();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createKurtosis();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createMean();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createMedian();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createMode();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createSkewness();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createStandardDeviation();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertValue object
   * created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createVariance();


  /**
   * Creates a new DistribUncertStatisticSpan object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertStatisticSpan
   * object created.
   *
   * @return a new DistribUncertStatisticSpan object instance.
   */
  DistribUncertStatisticSpan* createConfidenceInterval();


  /**
   * Creates a new DistribUncertStatisticSpan object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertStatisticSpan
   * object created.
   *
   * @return a new DistribUncertStatisticSpan object instance.
   */
  DistribUncertStatisticSpan* createCredibleInterval();


  /**
   * Creates a new DistribUncertStatisticSpan object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertStatisticSpan
   * object created.
   *
   * @return a new DistribUncertStatisticSpan object instance.
   */
  DistribUncertStatisticSpan* createInterquartileRange();


  /**
   * Creates a new DistribUncertStatisticSpan object, adds it to this
   * DistribUncertStatistics object and returns the DistribUncertStatisticSpan
   * object created.
   *
   * @return a new DistribUncertStatisticSpan object instance.
   */
  DistribUncertStatisticSpan* createRange();


  /**
   * Unsets the value of the "coefficientOfVariation" element of this
   * DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCoefficientOfVariation();


  /**
   * Unsets the value of the "kurtosis" element of this
   * DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetKurtosis();


  /**
   * Unsets the value of the "mean" element of this DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMean();


  /**
   * Unsets the value of the "median" element of this DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMedian();


  /**
   * Unsets the value of the "mode" element of this DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetMode();


  /**
   * Unsets the value of the "skewness" element of this
   * DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSkewness();


  /**
   * Unsets the value of the "standardDeviation" element of this
   * DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStandardDeviation();


  /**
   * Unsets the value of the "variance" element of this
   * DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetVariance();


  /**
   * Unsets the value of the "confidenceInterval" element of this
   * DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetConfidenceInterval();


  /**
   * Unsets the value of the "credibleInterval" element of this
   * DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCredibleInterval();


  /**
   * Unsets the value of the "interquartileRange" element of this
   * DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetInterquartileRange();


  /**
   * Unsets the value of the "range" element of this DistribUncertStatistics.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRange();


  /**
   * Returns the ListOfExternalParameters from this DistribUncertStatistics.
   *
   * @return the ListOfExternalParameters from this DistribUncertStatistics.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see getDistribExternalParameter(const std::string& sid)
   * @see getDistribExternalParameter(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see removeDistribExternalParameter(const std::string& sid)
   * @see removeDistribExternalParameter(unsigned int n)
   */
  const ListOfExternalParameters* getListOfDistribExternalParameters() const;


  /**
   * Returns the ListOfExternalParameters from this DistribUncertStatistics.
   *
   * @return the ListOfExternalParameters from this DistribUncertStatistics.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see getDistribExternalParameter(const std::string& sid)
   * @see getDistribExternalParameter(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see removeDistribExternalParameter(const std::string& sid)
   * @see removeDistribExternalParameter(unsigned int n)
   */
  ListOfExternalParameters* getListOfDistribExternalParameters();


  /**
   * Get a DistribExternalParameter from the DistribUncertStatistics.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to retrieve.
   *
   * @return the nth DistribExternalParameter in the ListOfExternalParameters
   * within this DistribUncertStatistics.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see getDistribExternalParameter(const std::string& sid)
   * @see getNumDistribExternalParameters()
   * @see removeDistribExternalParameter(const std::string& sid)
   * @see removeDistribExternalParameter(unsigned int n)
   */
  DistribExternalParameter* getDistribExternalParameter(unsigned int n);


  /**
   * Get a DistribExternalParameter from the DistribUncertStatistics.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to retrieve.
   *
   * @return the nth DistribExternalParameter in the ListOfExternalParameters
   * within this DistribUncertStatistics.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see getDistribExternalParameter(const std::string& sid)
   * @see getNumDistribExternalParameters()
   * @see removeDistribExternalParameter(const std::string& sid)
   * @see removeDistribExternalParameter(unsigned int n)
   */
  const DistribExternalParameter* getDistribExternalParameter(unsigned int n)
    const;


  /**
   * Get a DistribExternalParameter from the DistribUncertStatistics based on
   * its identifier.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to retrieve.
   *
   * @return the DistribExternalParameter in the ListOfExternalParameters
   * within this DistribUncertStatistics with the given @p sid or @c NULL if no
   * such DistribExternalParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see getDistribExternalParameter(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see removeDistribExternalParameter(const std::string& sid)
   * @see removeDistribExternalParameter(unsigned int n)
   */
  DistribExternalParameter* getDistribExternalParameter(const std::string&
    sid);


  /**
   * Get a DistribExternalParameter from the DistribUncertStatistics based on
   * its identifier.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to retrieve.
   *
   * @return the DistribExternalParameter in the ListOfExternalParameters
   * within this DistribUncertStatistics with the given @p sid or @c NULL if no
   * such DistribExternalParameter exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see getDistribExternalParameter(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see removeDistribExternalParameter(const std::string& sid)
   * @see removeDistribExternalParameter(unsigned int n)
   */
  const DistribExternalParameter* getDistribExternalParameter(const
    std::string& sid) const;


  /**
   * Adds a copy of the given DistribExternalParameter to this
   * DistribUncertStatistics.
   *
   * @param dep the DistribExternalParameter object to add.
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
   * @see createDistribExternalParameter()
   * @see getDistribExternalParameter(const std::string& sid)
   * @see getDistribExternalParameter(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see removeDistribExternalParameter(const std::string& sid)
   * @see removeDistribExternalParameter(unsigned int n)
   */
  int addDistribExternalParameter(const DistribExternalParameter* dep);


  /**
   * Get the number of DistribExternalParameter objects in this
   * DistribUncertStatistics.
   *
   * @return the number of DistribExternalParameter objects in this
   * DistribUncertStatistics.
   *
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see getDistribExternalParameter(const std::string& sid)
   * @see getDistribExternalParameter(unsigned int n)
   * @see removeDistribExternalParameter(const std::string& sid)
   * @see removeDistribExternalParameter(unsigned int n)
   */
  unsigned int getNumDistribExternalParameters() const;


  /**
   * Creates a new DistribExternalParameter object, adds it to this
   * DistribUncertStatistics object and returns the DistribExternalParameter
   * object created.
   *
   * @return a new DistribExternalParameter object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see getDistribExternalParameter(const std::string& sid)
   * @see getDistribExternalParameter(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see removeDistribExternalParameter(const std::string& sid)
   * @see removeDistribExternalParameter(unsigned int n)
   */
  DistribExternalParameter* createDistribExternalParameter();


  /**
   * Removes the nth DistribExternalParameter from this DistribUncertStatistics
   * and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to remove.
   *
   * @return a pointer to the nth DistribExternalParameter in this
   * DistribUncertStatistics.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see getDistribExternalParameter(const std::string& sid)
   * @see getDistribExternalParameter(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see removeDistribExternalParameter(const std::string& sid)
   */
  DistribExternalParameter* removeDistribExternalParameter(unsigned int n);


  /**
   * Removes the DistribExternalParameter from this DistribUncertStatistics
   * based on its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to remove.
   *
   * @return the DistribExternalParameter in this DistribUncertStatistics based
   * on the identifier or NULL if no such DistribExternalParameter exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribExternalParameter(const DistribExternalParameter* object)
   * @see createDistribExternalParameter()
   * @see getDistribExternalParameter(const std::string& sid)
   * @see getDistribExternalParameter(unsigned int n)
   * @see getNumDistribExternalParameters()
   * @see removeDistribExternalParameter(unsigned int n)
   */
  DistribExternalParameter* removeDistribExternalParameter(const std::string&
    sid);


  /**
   * Returns the XML element name of this DistribUncertStatistics object.
   *
   * For DistribUncertStatistics, the XML element name is always
   * @c "uncertStatistics".
   *
   * @return the name of this element, i.e. @c "uncertStatistics".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this DistribUncertStatistics object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this DistribUncertStatistics object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_UNCERTSTATISTICS, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribUncertStatistics object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribUncertStatistics have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;



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
   * Gets the value of the "attributeName" attribute of this
   * DistribUncertStatistics.
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
   * Gets the value of the "attributeName" attribute of this
   * DistribUncertStatistics.
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
   * Gets the value of the "attributeName" attribute of this
   * DistribUncertStatistics.
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
   * Gets the value of the "attributeName" attribute of this
   * DistribUncertStatistics.
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
   * Gets the value of the "attributeName" attribute of this
   * DistribUncertStatistics.
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
   * Predicate returning @c true if this DistribUncertStatistics's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribUncertStatistics's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribUncertStatistics.
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
   * Sets the value of the "attributeName" attribute of this
   * DistribUncertStatistics.
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
   * Sets the value of the "attributeName" attribute of this
   * DistribUncertStatistics.
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
   * Sets the value of the "attributeName" attribute of this
   * DistribUncertStatistics.
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
   * Sets the value of the "attributeName" attribute of this
   * DistribUncertStatistics.
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
   * Unsets the value of the "attributeName" attribute of this
   * DistribUncertStatistics.
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
   * Creates and returns an new "elementName" object in this
   * DistribUncertStatistics.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribUncertStatistics.
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
   * DistribUncertStatistics.
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
   * Returns the number of "elementName" in this DistribUncertStatistics.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this DistribUncertStatistics.
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
 * Creates a new DistribUncertStatistics_t using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribUncertStatistics_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribUncertStatistics_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribUncertStatistics_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertStatistics_t *
DistribUncertStatistics_create(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribUncertStatistics_t object.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return a (deep) copy of this DistribUncertStatistics_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertStatistics_t*
DistribUncertStatistics_clone(const DistribUncertStatistics_t* dus);


/**
 * Frees this DistribUncertStatistics_t object.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
void
DistribUncertStatistics_free(DistribUncertStatistics_t* dus);


/**
 * Returns the value of the "id" attribute of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this DistribUncertStatistics_t as
 * a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
char *
DistribUncertStatistics_getId(const DistribUncertStatistics_t * dus);


/**
 * Returns the value of the "name" attribute of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this DistribUncertStatistics_t
 * as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
char *
DistribUncertStatistics_getName(const DistribUncertStatistics_t * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's "id"
 * attribute is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "id" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetId(const DistribUncertStatistics_t * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's "name"
 * attribute is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "name" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetName(const DistribUncertStatistics_t * dus);


/**
 * Sets the value of the "id" attribute of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribUncertStatistics_unsetId().
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setId(DistribUncertStatistics_t * dus,
                              const char * id);


/**
 * Sets the value of the "name" attribute of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribUncertStatistics_unsetName().
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setName(DistribUncertStatistics_t * dus,
                                const char * name);


/**
 * Unsets the value of the "id" attribute of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetId(DistribUncertStatistics_t * dus);


/**
 * Unsets the value of the "name" attribute of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetName(DistribUncertStatistics_t * dus);


/**
 * Returns the value of the "coefficientOfVariation" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose
 * coefficientOfVariation is sought.
 *
 * @return the value of the "coefficientOfVariation" element of this
 * DistribUncertStatistics_t as a DistribUncertValue*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getCoefficientOfVariation(const
  DistribUncertStatistics_t * dus);


/**
 * Returns the value of the "kurtosis" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose kurtosis is sought.
 *
 * @return the value of the "kurtosis" element of this
 * DistribUncertStatistics_t as a DistribUncertValue*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getKurtosis(const DistribUncertStatistics_t * dus);


/**
 * Returns the value of the "mean" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose mean is sought.
 *
 * @return the value of the "mean" element of this DistribUncertStatistics_t as
 * a DistribUncertValue*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getMean(const DistribUncertStatistics_t * dus);


/**
 * Returns the value of the "median" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose median is sought.
 *
 * @return the value of the "median" element of this DistribUncertStatistics_t
 * as a DistribUncertValue*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getMedian(const DistribUncertStatistics_t * dus);


/**
 * Returns the value of the "mode" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose mode is sought.
 *
 * @return the value of the "mode" element of this DistribUncertStatistics_t as
 * a DistribUncertValue*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getMode(const DistribUncertStatistics_t * dus);


/**
 * Returns the value of the "skewness" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose skewness is sought.
 *
 * @return the value of the "skewness" element of this
 * DistribUncertStatistics_t as a DistribUncertValue*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getSkewness(const DistribUncertStatistics_t * dus);


/**
 * Returns the value of the "standardDeviation" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose standardDeviation
 * is sought.
 *
 * @return the value of the "standardDeviation" element of this
 * DistribUncertStatistics_t as a DistribUncertValue*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getStandardDeviation(const DistribUncertStatistics_t *
  dus);


/**
 * Returns the value of the "variance" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose variance is sought.
 *
 * @return the value of the "variance" element of this
 * DistribUncertStatistics_t as a DistribUncertValue*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribUncertStatistics_getVariance(const DistribUncertStatistics_t * dus);


/**
 * Returns the value of the "confidenceInterval" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose confidenceInterval
 * is sought.
 *
 * @return the value of the "confidenceInterval" element of this
 * DistribUncertStatistics_t as a DistribUncertStatisticSpan*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertStatisticSpan_t*
DistribUncertStatistics_getConfidenceInterval(const DistribUncertStatistics_t *
  dus);


/**
 * Returns the value of the "credibleInterval" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose credibleInterval is
 * sought.
 *
 * @return the value of the "credibleInterval" element of this
 * DistribUncertStatistics_t as a DistribUncertStatisticSpan*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertStatisticSpan_t*
DistribUncertStatistics_getCredibleInterval(const DistribUncertStatistics_t *
  dus);


/**
 * Returns the value of the "interquartileRange" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose interquartileRange
 * is sought.
 *
 * @return the value of the "interquartileRange" element of this
 * DistribUncertStatistics_t as a DistribUncertStatisticSpan*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertStatisticSpan_t*
DistribUncertStatistics_getInterquartileRange(const DistribUncertStatistics_t *
  dus);


/**
 * Returns the value of the "range" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose range is sought.
 *
 * @return the value of the "range" element of this DistribUncertStatistics_t
 * as a DistribUncertStatisticSpan*.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
const DistribUncertStatisticSpan_t*
DistribUncertStatistics_getRange(const DistribUncertStatistics_t * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "coefficientOfVariation" element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's
 * "coefficientOfVariation" element has been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetCoefficientOfVariation(const
  DistribUncertStatistics_t * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "kurtosis" element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "kurtosis" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetKurtosis(const DistribUncertStatistics_t * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's "mean"
 * element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "mean" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetMean(const DistribUncertStatistics_t * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's "median"
 * element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "median" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetMedian(const DistribUncertStatistics_t * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's "mode"
 * element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "mode" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetMode(const DistribUncertStatistics_t * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "skewness" element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "skewness" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetSkewness(const DistribUncertStatistics_t * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "standardDeviation" element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "standardDeviation"
 * element has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetStandardDeviation(const DistribUncertStatistics_t
  * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "variance" element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "variance" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetVariance(const DistribUncertStatistics_t * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "confidenceInterval" element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "confidenceInterval"
 * element has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetConfidenceInterval(const DistribUncertStatistics_t
  * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "credibleInterval" element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "credibleInterval"
 * element has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetCredibleInterval(const DistribUncertStatistics_t *
  dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's
 * "interquartileRange" element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "interquartileRange"
 * element has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetInterquartileRange(const DistribUncertStatistics_t
  * dus);


/**
 * Predicate returning @c 1 (true) if this DistribUncertStatistics_t's "range"
 * element is set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) if this DistribUncertStatistics_t's "range" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_isSetRange(const DistribUncertStatistics_t * dus);


/**
 * Sets the value of the "coefficientOfVariation" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param coefficientOfVariation DistribUncertValue_t* value of the
 * "coefficientOfVariation" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setCoefficientOfVariation(
                                                  DistribUncertStatistics_t *
                                                    dus,
                                                  const DistribUncertValue_t*
                                                    coefficientOfVariation);


/**
 * Sets the value of the "kurtosis" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param kurtosis DistribUncertValue_t* value of the "kurtosis" element to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setKurtosis(DistribUncertStatistics_t * dus,
                                    const DistribUncertValue_t* kurtosis);


/**
 * Sets the value of the "mean" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param mean DistribUncertValue_t* value of the "mean" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setMean(DistribUncertStatistics_t * dus,
                                const DistribUncertValue_t* mean);


/**
 * Sets the value of the "median" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param median DistribUncertValue_t* value of the "median" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setMedian(DistribUncertStatistics_t * dus,
                                  const DistribUncertValue_t* median);


/**
 * Sets the value of the "mode" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param mode DistribUncertValue_t* value of the "mode" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setMode(DistribUncertStatistics_t * dus,
                                const DistribUncertValue_t* mode);


/**
 * Sets the value of the "skewness" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param skewness DistribUncertValue_t* value of the "skewness" element to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setSkewness(DistribUncertStatistics_t * dus,
                                    const DistribUncertValue_t* skewness);


/**
 * Sets the value of the "standardDeviation" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param standardDeviation DistribUncertValue_t* value of the
 * "standardDeviation" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setStandardDeviation(DistribUncertStatistics_t * dus,
                                             const DistribUncertValue_t*
                                               standardDeviation);


/**
 * Sets the value of the "variance" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param variance DistribUncertValue_t* value of the "variance" element to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setVariance(DistribUncertStatistics_t * dus,
                                    const DistribUncertValue_t* variance);


/**
 * Sets the value of the "confidenceInterval" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param confidenceInterval DistribUncertStatisticSpan_t* value of the
 * "confidenceInterval" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setConfidenceInterval(DistribUncertStatistics_t * dus,
                                              const
                                                DistribUncertStatisticSpan_t*
                                                  confidenceInterval);


/**
 * Sets the value of the "credibleInterval" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param credibleInterval DistribUncertStatisticSpan_t* value of the
 * "credibleInterval" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setCredibleInterval(DistribUncertStatistics_t * dus,
                                            const DistribUncertStatisticSpan_t*
                                              credibleInterval);


/**
 * Sets the value of the "interquartileRange" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param interquartileRange DistribUncertStatisticSpan_t* value of the
 * "interquartileRange" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setInterquartileRange(DistribUncertStatistics_t * dus,
                                              const
                                                DistribUncertStatisticSpan_t*
                                                  interquartileRange);


/**
 * Sets the value of the "range" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @param range DistribUncertStatisticSpan_t* value of the "range" element to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_setRange(DistribUncertStatistics_t * dus,
                                 const DistribUncertStatisticSpan_t* range);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createCoefficientOfVariation(DistribUncertStatistics_t*
  dus);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createKurtosis(DistribUncertStatistics_t* dus);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createMean(DistribUncertStatistics_t* dus);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createMedian(DistribUncertStatistics_t* dus);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createMode(DistribUncertStatistics_t* dus);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createSkewness(DistribUncertStatistics_t* dus);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createStandardDeviation(DistribUncertStatistics_t*
  dus);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribUncertValue_t object
 * created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribUncertStatistics_createVariance(DistribUncertStatistics_t* dus);


/**
 * Creates a new DistribUncertStatisticSpan_t object, adds it to this
 * DistribUncertStatistics_t object and returns the
 * DistribUncertStatisticSpan_t object created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertStatisticSpan_t should be added.
 *
 * @return a new DistribUncertStatisticSpan_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t*
DistribUncertStatistics_createConfidenceInterval(DistribUncertStatistics_t*
  dus);


/**
 * Creates a new DistribUncertStatisticSpan_t object, adds it to this
 * DistribUncertStatistics_t object and returns the
 * DistribUncertStatisticSpan_t object created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertStatisticSpan_t should be added.
 *
 * @return a new DistribUncertStatisticSpan_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t*
DistribUncertStatistics_createCredibleInterval(DistribUncertStatistics_t* dus);


/**
 * Creates a new DistribUncertStatisticSpan_t object, adds it to this
 * DistribUncertStatistics_t object and returns the
 * DistribUncertStatisticSpan_t object created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertStatisticSpan_t should be added.
 *
 * @return a new DistribUncertStatisticSpan_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t*
DistribUncertStatistics_createInterquartileRange(DistribUncertStatistics_t*
  dus);


/**
 * Creates a new DistribUncertStatisticSpan_t object, adds it to this
 * DistribUncertStatistics_t object and returns the
 * DistribUncertStatisticSpan_t object created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribUncertStatisticSpan_t should be added.
 *
 * @return a new DistribUncertStatisticSpan_t object instance.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribUncertStatisticSpan_t*
DistribUncertStatistics_createRange(DistribUncertStatistics_t* dus);


/**
 * Unsets the value of the "coefficientOfVariation" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetCoefficientOfVariation(DistribUncertStatistics_t *
  dus);


/**
 * Unsets the value of the "kurtosis" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetKurtosis(DistribUncertStatistics_t * dus);


/**
 * Unsets the value of the "mean" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetMean(DistribUncertStatistics_t * dus);


/**
 * Unsets the value of the "median" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetMedian(DistribUncertStatistics_t * dus);


/**
 * Unsets the value of the "mode" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetMode(DistribUncertStatistics_t * dus);


/**
 * Unsets the value of the "skewness" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetSkewness(DistribUncertStatistics_t * dus);


/**
 * Unsets the value of the "standardDeviation" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetStandardDeviation(DistribUncertStatistics_t *
  dus);


/**
 * Unsets the value of the "variance" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetVariance(DistribUncertStatistics_t * dus);


/**
 * Unsets the value of the "confidenceInterval" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetConfidenceInterval(DistribUncertStatistics_t *
  dus);


/**
 * Unsets the value of the "credibleInterval" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetCredibleInterval(DistribUncertStatistics_t * dus);


/**
 * Unsets the value of the "interquartileRange" element of this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetInterquartileRange(DistribUncertStatistics_t *
  dus);


/**
 * Unsets the value of the "range" element of this DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_unsetRange(DistribUncertStatistics_t * dus);


/**
 * Returns a ListOf_t * containing DistribExternalParameter_t objects from this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure whose
 * ListOfExternalParameters is sought.
 *
 * @return the ListOfExternalParameters from this DistribUncertStatistics_t as
 * a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see DistribUncertStatistics_addDistribExternalParameter()
 * @see DistribUncertStatistics_createDistribExternalParameter()
 * @see DistribUncertStatistics_getDistribExternalParameterById()
 * @see DistribUncertStatistics_getDistribExternalParameter()
 * @see DistribUncertStatistics_getNumDistribExternalParameters()
 * @see DistribUncertStatistics_removeDistribExternalParameterById()
 * @see DistribUncertStatistics_removeDistribExternalParameter()
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
ListOf_t*
DistribUncertStatistics_getListOfDistribExternalParameters(DistribUncertStatistics_t*
  dus);


/**
 * Get a DistribExternalParameter_t from the DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * DistribExternalParameter_t to retrieve.
 *
 * @return the nth DistribExternalParameter_t in the ListOfExternalParameters
 * within this DistribUncertStatistics.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribUncertStatistics_getDistribExternalParameter(
                                                    DistribUncertStatistics_t*
                                                      dus,
                                                    unsigned int n);


/**
 * Get a DistribExternalParameter_t from the DistribUncertStatistics_t based on
 * its identifier.
 *
 * @param dus the DistribUncertStatistics_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * DistribExternalParameter_t to retrieve.
 *
 * @return the DistribExternalParameter_t in the ListOfExternalParameters
 * within this DistribUncertStatistics with the given @p sid or @c NULL if no
 * such DistribExternalParameter_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribUncertStatistics_getDistribExternalParameterById(
                                                        DistribUncertStatistics_t*
                                                          dus,
                                                        const char *sid);


/**
 * Adds a copy of the given DistribExternalParameter_t to this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribExternalParameter_t should be added.
 *
 * @param dep the DistribExternalParameter_t object to add.
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
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_addDistribExternalParameter(
                                                    DistribUncertStatistics_t*
                                                      dus,
                                                    const
                                                      DistribExternalParameter_t*
                                                        dep);


/**
 * Get the number of DistribExternalParameter_t objects in this
 * DistribUncertStatistics_t.
 *
 * @param dus the DistribUncertStatistics_t structure to query.
 *
 * @return the number of DistribExternalParameter_t objects in this
 * DistribUncertStatistics_t.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
unsigned int
DistribUncertStatistics_getNumDistribExternalParameters(DistribUncertStatistics_t*
  dus);


/**
 * Creates a new DistribExternalParameter_t object, adds it to this
 * DistribUncertStatistics_t object and returns the DistribExternalParameter_t
 * object created.
 *
 * @param dus the DistribUncertStatistics_t structure to which the
 * DistribExternalParameter_t should be added.
 *
 * @return a new DistribExternalParameter_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribUncertStatistics_createDistribExternalParameter(DistribUncertStatistics_t*
  dus);


/**
 * Removes the nth DistribExternalParameter_t from this
 * DistribUncertStatistics_t and returns a pointer to it.
 *
 * @param dus the DistribUncertStatistics_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * DistribExternalParameter_t to remove.
 *
 * @return a pointer to the nth DistribExternalParameter_t in this
 * DistribUncertStatistics_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribUncertStatistics_removeDistribExternalParameter(
                                                       DistribUncertStatistics_t*
                                                         dus,
                                                       unsigned int n);


/**
 * Removes the DistribExternalParameter_t from this DistribUncertStatistics_t
 * based on its identifier and returns a pointer to it.
 *
 * @param dus the DistribUncertStatistics_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * DistribExternalParameter_t to remove.
 *
 * @return the DistribExternalParameter_t in this DistribUncertStatistics_t
 * based on the identifier or NULL if no such DistribExternalParameter_t
 * exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribUncertStatistics_removeDistribExternalParameterById(
                                                           DistribUncertStatistics_t*
                                                             dus,
                                                           const char* sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUncertStatistics_t object have been set.
 *
 * @param dus the DistribUncertStatistics_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribUncertStatistics_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertStatistics_t
 */
LIBSBML_EXTERN
int
DistribUncertStatistics_hasRequiredAttributes(const DistribUncertStatistics_t *
  dus);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribUncertStatistics_H__ */


