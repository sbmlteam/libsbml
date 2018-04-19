/**
 * @file DistribUncertainty.h
 * @brief Definition of the DistribUncertainty class.
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
 * @class DistribUncertainty
 * @sbmlbrief{distrib} TODO:Definition of the DistribUncertainty class.
 */


#ifndef DistribUncertainty_H__
#define DistribUncertainty_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertStatistics.h>
#include <sbml/packages/distrib/sbml/DistribDistribution.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribUncertainty : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribUncertStatistics* mUncertStatistics;
  DistribDistribution* mDistribution;

  /** @endcond */

public:

  /**
   * Creates a new DistribUncertainty using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribUncertainty.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribUncertainty.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribUncertainty.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUncertainty(unsigned int level = DistribExtension::getDefaultLevel(),
                     unsigned int version =
                       DistribExtension::getDefaultVersion(),
                     unsigned int pkgVersion =
                       DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribUncertainty using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUncertainty(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribUncertainty.
   *
   * @param orig the DistribUncertainty instance to copy.
   */
  DistribUncertainty(const DistribUncertainty& orig);


  /**
   * Assignment operator for DistribUncertainty.
   *
   * @param rhs the DistribUncertainty object whose values are to be used as
   * the basis of the assignment.
   */
  DistribUncertainty& operator=(const DistribUncertainty& rhs);


  /**
   * Creates and returns a deep copy of this DistribUncertainty object.
   *
   * @return a (deep) copy of this DistribUncertainty object.
   */
  virtual DistribUncertainty* clone() const;


  /**
   * Destructor for DistribUncertainty.
   */
  virtual ~DistribUncertainty();


  /**
   * Returns the value of the "id" attribute of this DistribUncertainty.
   *
   * @return the value of the "id" attribute of this DistribUncertainty as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this DistribUncertainty.
   *
   * @return the value of the "name" attribute of this DistribUncertainty as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this DistribUncertainty's "id" attribute is
   * set.
   *
   * @return @c true if this DistribUncertainty's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribUncertainty's "name" attribute
   * is set.
   *
   * @return @c true if this DistribUncertainty's "name" attribute has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this DistribUncertainty.
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
   * Sets the value of the "name" attribute of this DistribUncertainty.
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
   * Unsets the value of the "id" attribute of this DistribUncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this DistribUncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "uncertStatistics" element of this
   * DistribUncertainty.
   *
   * @return the value of the "uncertStatistics" element of this
   * DistribUncertainty as a DistribUncertStatistics*.
   */
  const DistribUncertStatistics* getUncertStatistics() const;


  /**
   * Returns the value of the "uncertStatistics" element of this
   * DistribUncertainty.
   *
   * @return the value of the "uncertStatistics" element of this
   * DistribUncertainty as a DistribUncertStatistics*.
   */
  DistribUncertStatistics* getUncertStatistics();


  /**
   * Returns the value of the "distribution" element of this
   * DistribUncertainty.
   *
   * @return the value of the "distribution" element of this DistribUncertainty
   * as a DistribDistribution*.
   */
  const DistribDistribution* getDistribution() const;


  /**
   * Returns the value of the "distribution" element of this
   * DistribUncertainty.
   *
   * @return the value of the "distribution" element of this DistribUncertainty
   * as a DistribDistribution*.
   */
  DistribDistribution* getDistribution();


  /**
   * Predicate returning @c true if this DistribUncertainty's
   * "uncertStatistics" element is set.
   *
   * @return @c true if this DistribUncertainty's "uncertStatistics" element
   * has been set, otherwise @c false is returned.
   */
  bool isSetUncertStatistics() const;


  /**
   * Predicate returning @c true if this DistribUncertainty's "distribution"
   * element is set.
   *
   * @return @c true if this DistribUncertainty's "distribution" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetDistribution() const;


  /**
   * Sets the value of the "uncertStatistics" element of this
   * DistribUncertainty.
   *
   * @param uncertStatistics DistribUncertStatistics* value of the
   * "uncertStatistics" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setUncertStatistics(const DistribUncertStatistics* uncertStatistics);


  /**
   * Sets the value of the "distribution" element of this DistribUncertainty.
   *
   * @param distribution DistribDistribution* value of the "distribution"
   * element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDistribution(const DistribDistribution* distribution);


  /**
   * Creates a new DistribUncertStatistics object, adds it to this
   * DistribUncertainty object and returns the DistribUncertStatistics object
   * created.
   *
   * @return a new DistribUncertStatistics object instance.
   */
  DistribUncertStatistics* createUncertStatistics();


  /**
   * Creates a new DistribBetaDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribBetaDistribution object
   * created.
   *
   * @return a new DistribBetaDistribution object instance.
   */
  DistribBetaDistribution* createDistribBetaDistribution();


  /**
   * Creates a new DistribCauchyDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribCauchyDistribution object
   * created.
   *
   * @return a new DistribCauchyDistribution object instance.
   */
  DistribCauchyDistribution* createDistribCauchyDistribution();


  /**
   * Creates a new DistribChiSquareDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribChiSquareDistribution
   * object created.
   *
   * @return a new DistribChiSquareDistribution object instance.
   */
  DistribChiSquareDistribution* createDistribChiSquareDistribution();


  /**
   * Creates a new DistribExponentialDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribExponentialDistribution
   * object created.
   *
   * @return a new DistribExponentialDistribution object instance.
   */
  DistribExponentialDistribution* createDistribExponentialDistribution();


  /**
   * Creates a new DistribFDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribFDistribution object
   * created.
   *
   * @return a new DistribFDistribution object instance.
   */
  DistribFDistribution* createDistribFDistribution();


  /**
   * Creates a new DistribGammaDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribGammaDistribution object
   * created.
   *
   * @return a new DistribGammaDistribution object instance.
   */
  DistribGammaDistribution* createDistribGammaDistribution();


  /**
   * Creates a new DistribInverseGammaDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribInverseGammaDistribution
   * object created.
   *
   * @return a new DistribInverseGammaDistribution object instance.
   */
  DistribInverseGammaDistribution* createDistribInverseGammaDistribution();


  /**
   * Creates a new DistribLaPlaceDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribLaPlaceDistribution
   * object created.
   *
   * @return a new DistribLaPlaceDistribution object instance.
   */
  DistribLaPlaceDistribution* createDistribLaPlaceDistribution();


  /**
   * Creates a new DistribLogNormalDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribLogNormalDistribution
   * object created.
   *
   * @return a new DistribLogNormalDistribution object instance.
   */
  DistribLogNormalDistribution* createDistribLogNormalDistribution();


  /**
   * Creates a new DistribLogisticDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribLogisticDistribution
   * object created.
   *
   * @return a new DistribLogisticDistribution object instance.
   */
  DistribLogisticDistribution* createDistribLogisticDistribution();


  /**
   * Creates a new DistribNormalDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribNormalDistribution object
   * created.
   *
   * @return a new DistribNormalDistribution object instance.
   */
  DistribNormalDistribution* createDistribNormalDistribution();


  /**
   * Creates a new DistribParetoDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribParetoDistribution object
   * created.
   *
   * @return a new DistribParetoDistribution object instance.
   */
  DistribParetoDistribution* createDistribParetoDistribution();


  /**
   * Creates a new DistribRayleighDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribRayleighDistribution
   * object created.
   *
   * @return a new DistribRayleighDistribution object instance.
   */
  DistribRayleighDistribution* createDistribRayleighDistribution();


  /**
   * Creates a new DistribStudentTDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribStudentTDistribution
   * object created.
   *
   * @return a new DistribStudentTDistribution object instance.
   */
  DistribStudentTDistribution* createDistribStudentTDistribution();


  /**
   * Creates a new DistribUniformDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribUniformDistribution
   * object created.
   *
   * @return a new DistribUniformDistribution object instance.
   */
  DistribUniformDistribution* createDistribUniformDistribution();


  /**
   * Creates a new DistribWeibullDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribWeibullDistribution
   * object created.
   *
   * @return a new DistribWeibullDistribution object instance.
   */
  DistribWeibullDistribution* createDistribWeibullDistribution();


  /**
   * Creates a new DistribBinomialDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribBinomialDistribution
   * object created.
   *
   * @return a new DistribBinomialDistribution object instance.
   */
  DistribBinomialDistribution* createDistribBinomialDistribution();


  /**
   * Creates a new DistribGeometricDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribGeometricDistribution
   * object created.
   *
   * @return a new DistribGeometricDistribution object instance.
   */
  DistribGeometricDistribution* createDistribGeometricDistribution();


  /**
   * Creates a new DistribHypergeometricDistribution object, adds it to this
   * DistribUncertainty object and returns the
   * DistribHypergeometricDistribution object created.
   *
   * @return a new DistribHypergeometricDistribution object instance.
   */
  DistribHypergeometricDistribution* createDistribHypergeometricDistribution();


  /**
   * Creates a new DistribNegativeBinomialDistribution object, adds it to this
   * DistribUncertainty object and returns the
   * DistribNegativeBinomialDistribution object created.
   *
   * @return a new DistribNegativeBinomialDistribution object instance.
   */
  DistribNegativeBinomialDistribution*
    createDistribNegativeBinomialDistribution();


  /**
   * Creates a new DistribPoissonDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribPoissonDistribution
   * object created.
   *
   * @return a new DistribPoissonDistribution object instance.
   */
  DistribPoissonDistribution* createDistribPoissonDistribution();


  /**
   * Creates a new DistribBernoulliDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribBernoulliDistribution
   * object created.
   *
   * @return a new DistribBernoulliDistribution object instance.
   */
  DistribBernoulliDistribution* createDistribBernoulliDistribution();


  /**
   * Creates a new DistribCategoricalDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribCategoricalDistribution
   * object created.
   *
   * @return a new DistribCategoricalDistribution object instance.
   */
  DistribCategoricalDistribution* createDistribCategoricalDistribution();


  /**
   * Creates a new DistribMultivariateDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribMultivariateDistribution
   * object created.
   *
   * @return a new DistribMultivariateDistribution object instance.
   */
  DistribMultivariateDistribution* createDistribMultivariateDistribution();


  /**
   * Creates a new DistribExternalDistribution object, adds it to this
   * DistribUncertainty object and returns the DistribExternalDistribution
   * object created.
   *
   * @return a new DistribExternalDistribution object instance.
   */
  DistribExternalDistribution* createDistribExternalDistribution();


  /**
   * Unsets the value of the "uncertStatistics" element of this
   * DistribUncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetUncertStatistics();


  /**
   * Unsets the value of the "distribution" element of this DistribUncertainty.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDistribution();


  /**
   * Returns the XML element name of this DistribUncertainty object.
   *
   * For DistribUncertainty, the XML element name is always @c "uncertainty".
   *
   * @return the name of this element, i.e. @c "uncertainty".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribUncertainty object.
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
   * DistribUncertainty object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribUncertainty have been set, otherwise @c false is returned.
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
   * DistribUncertainty.
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
   * DistribUncertainty.
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
   * DistribUncertainty.
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
   * DistribUncertainty.
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
   * DistribUncertainty.
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
   * Predicate returning @c true if this DistribUncertainty's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribUncertainty's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribUncertainty.
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
   * DistribUncertainty.
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
   * DistribUncertainty.
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
   * DistribUncertainty.
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
   * DistribUncertainty.
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
   * DistribUncertainty.
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
   * DistribUncertainty.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribUncertainty.
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
   * DistribUncertainty.
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
   * Returns the number of "elementName" in this DistribUncertainty.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this DistribUncertainty.
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
   * Reads the expected attributes into the member data variables
   */
  virtual void readL3V1V1Attributes(const XMLAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual void readL3V2V1Attributes(const XMLAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeL3V1V1Attributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeL3V2V1Attributes(XMLOutputStream& stream) const;

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new DistribUncertainty_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribUncertainty_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribUncertainty_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribUncertainty_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribUncertainty_t *
DistribUncertainty_create(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribUncertainty_t object.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @return a (deep) copy of this DistribUncertainty_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribUncertainty_t*
DistribUncertainty_clone(const DistribUncertainty_t* du);


/**
 * Frees this DistribUncertainty_t object.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
void
DistribUncertainty_free(DistribUncertainty_t* du);


/**
 * Returns the value of the "id" attribute of this DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this DistribUncertainty_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
char *
DistribUncertainty_getId(const DistribUncertainty_t * du);


/**
 * Returns the value of the "name" attribute of this DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this DistribUncertainty_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
char *
DistribUncertainty_getName(const DistribUncertainty_t * du);


/**
 * Predicate returning @c 1 (true) if this DistribUncertainty_t's "id"
 * attribute is set.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @return @c 1 (true) if this DistribUncertainty_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_isSetId(const DistribUncertainty_t * du);


/**
 * Predicate returning @c 1 (true) if this DistribUncertainty_t's "name"
 * attribute is set.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @return @c 1 (true) if this DistribUncertainty_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_isSetName(const DistribUncertainty_t * du);


/**
 * Sets the value of the "id" attribute of this DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribUncertainty_unsetId().
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_setId(DistribUncertainty_t * du, const char * id);


/**
 * Sets the value of the "name" attribute of this DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribUncertainty_unsetName().
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_setName(DistribUncertainty_t * du, const char * name);


/**
 * Unsets the value of the "id" attribute of this DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_unsetId(DistribUncertainty_t * du);


/**
 * Unsets the value of the "name" attribute of this DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_unsetName(DistribUncertainty_t * du);


/**
 * Returns the value of the "uncertStatistics" element of this
 * DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure whose uncertStatistics is
 * sought.
 *
 * @return the value of the "uncertStatistics" element of this
 * DistribUncertainty_t as a DistribUncertStatistics*.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
const DistribUncertStatistics_t*
DistribUncertainty_getUncertStatistics(const DistribUncertainty_t * du);


/**
 * Returns the value of the "distribution" element of this
 * DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure whose distribution is sought.
 *
 * @return the value of the "distribution" element of this DistribUncertainty_t
 * as a DistribDistribution*.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
const DistribDistribution_t*
DistribUncertainty_getDistribution(const DistribUncertainty_t * du);


/**
 * Predicate returning @c 1 (true) if this DistribUncertainty_t's
 * "uncertStatistics" element is set.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @return @c 1 (true) if this DistribUncertainty_t's "uncertStatistics"
 * element has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_isSetUncertStatistics(const DistribUncertainty_t * du);


/**
 * Predicate returning @c 1 (true) if this DistribUncertainty_t's
 * "distribution" element is set.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @return @c 1 (true) if this DistribUncertainty_t's "distribution" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_isSetDistribution(const DistribUncertainty_t * du);


/**
 * Sets the value of the "uncertStatistics" element of this
 * DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @param uncertStatistics DistribUncertStatistics_t* value of the
 * "uncertStatistics" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_setUncertStatistics(DistribUncertainty_t * du,
                                       const DistribUncertStatistics_t*
                                         uncertStatistics);


/**
 * Sets the value of the "distribution" element of this DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @param distribution DistribDistribution_t* value of the "distribution"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_setDistribution(DistribUncertainty_t * du,
                                   const DistribDistribution_t* distribution);


/**
 * Creates a new DistribUncertStatistics_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribUncertStatistics_t object
 * created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribUncertStatistics_t should be added.
 *
 * @return a new DistribUncertStatistics_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribUncertStatistics_t*
DistribUncertainty_createUncertStatistics(DistribUncertainty_t* du);


/**
 * Creates a new DistribBetaDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribBetaDistribution_t object
 * created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribBetaDistribution_t should be added.
 *
 * @return a new DistribBetaDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribBetaDistribution_t*
DistribUncertainty_createDistribBetaDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribCauchyDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribCauchyDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribCauchyDistribution_t should be added.
 *
 * @return a new DistribCauchyDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribCauchyDistribution_t*
DistribUncertainty_createDistribCauchyDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribChiSquareDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribChiSquareDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribChiSquareDistribution_t should be added.
 *
 * @return a new DistribChiSquareDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribChiSquareDistribution_t*
DistribUncertainty_createDistribChiSquareDistribution(DistribUncertainty_t*
  du);


/**
 * Creates a new DistribExponentialDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribExponentialDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribExponentialDistribution_t should be added.
 *
 * @return a new DistribExponentialDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribExponentialDistribution_t*
DistribUncertainty_createDistribExponentialDistribution(DistribUncertainty_t*
  du);


/**
 * Creates a new DistribFDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribFDistribution_t object
 * created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribFDistribution_t should be added.
 *
 * @return a new DistribFDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribFDistribution_t*
DistribUncertainty_createDistribFDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribGammaDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribGammaDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribGammaDistribution_t should be added.
 *
 * @return a new DistribGammaDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribGammaDistribution_t*
DistribUncertainty_createDistribGammaDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribInverseGammaDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the
 * DistribInverseGammaDistribution_t object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribInverseGammaDistribution_t should be added.
 *
 * @return a new DistribInverseGammaDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribInverseGammaDistribution_t*
DistribUncertainty_createDistribInverseGammaDistribution(DistribUncertainty_t*
  du);


/**
 * Creates a new DistribLaPlaceDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribLaPlaceDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribLaPlaceDistribution_t should be added.
 *
 * @return a new DistribLaPlaceDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribLaPlaceDistribution_t*
DistribUncertainty_createDistribLaPlaceDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribLogNormalDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribLogNormalDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribLogNormalDistribution_t should be added.
 *
 * @return a new DistribLogNormalDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribLogNormalDistribution_t*
DistribUncertainty_createDistribLogNormalDistribution(DistribUncertainty_t*
  du);


/**
 * Creates a new DistribLogisticDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribLogisticDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribLogisticDistribution_t should be added.
 *
 * @return a new DistribLogisticDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribLogisticDistribution_t*
DistribUncertainty_createDistribLogisticDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribNormalDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribNormalDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribNormalDistribution_t should be added.
 *
 * @return a new DistribNormalDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribNormalDistribution_t*
DistribUncertainty_createDistribNormalDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribParetoDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribParetoDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribParetoDistribution_t should be added.
 *
 * @return a new DistribParetoDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribParetoDistribution_t*
DistribUncertainty_createDistribParetoDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribRayleighDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribRayleighDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribRayleighDistribution_t should be added.
 *
 * @return a new DistribRayleighDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribRayleighDistribution_t*
DistribUncertainty_createDistribRayleighDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribStudentTDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribStudentTDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribStudentTDistribution_t should be added.
 *
 * @return a new DistribStudentTDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribStudentTDistribution_t*
DistribUncertainty_createDistribStudentTDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribUniformDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribUniformDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribUniformDistribution_t should be added.
 *
 * @return a new DistribUniformDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribUniformDistribution_t*
DistribUncertainty_createDistribUniformDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribWeibullDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribWeibullDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribWeibullDistribution_t should be added.
 *
 * @return a new DistribWeibullDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribWeibullDistribution_t*
DistribUncertainty_createDistribWeibullDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribBinomialDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribBinomialDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribBinomialDistribution_t should be added.
 *
 * @return a new DistribBinomialDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribBinomialDistribution_t*
DistribUncertainty_createDistribBinomialDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribGeometricDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribGeometricDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribGeometricDistribution_t should be added.
 *
 * @return a new DistribGeometricDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribGeometricDistribution_t*
DistribUncertainty_createDistribGeometricDistribution(DistribUncertainty_t*
  du);


/**
 * Creates a new DistribHypergeometricDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the
 * DistribHypergeometricDistribution_t object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribHypergeometricDistribution_t should be added.
 *
 * @return a new DistribHypergeometricDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribHypergeometricDistribution_t*
DistribUncertainty_createDistribHypergeometricDistribution(DistribUncertainty_t*
  du);


/**
 * Creates a new DistribNegativeBinomialDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the
 * DistribNegativeBinomialDistribution_t object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribNegativeBinomialDistribution_t should be added.
 *
 * @return a new DistribNegativeBinomialDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribNegativeBinomialDistribution_t*
DistribUncertainty_createDistribNegativeBinomialDistribution(DistribUncertainty_t*
  du);


/**
 * Creates a new DistribPoissonDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribPoissonDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribPoissonDistribution_t should be added.
 *
 * @return a new DistribPoissonDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribPoissonDistribution_t*
DistribUncertainty_createDistribPoissonDistribution(DistribUncertainty_t* du);


/**
 * Creates a new DistribBernoulliDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribBernoulliDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribBernoulliDistribution_t should be added.
 *
 * @return a new DistribBernoulliDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribBernoulliDistribution_t*
DistribUncertainty_createDistribBernoulliDistribution(DistribUncertainty_t*
  du);


/**
 * Creates a new DistribCategoricalDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribCategoricalDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribCategoricalDistribution_t should be added.
 *
 * @return a new DistribCategoricalDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribCategoricalDistribution_t*
DistribUncertainty_createDistribCategoricalDistribution(DistribUncertainty_t*
  du);


/**
 * Creates a new DistribMultivariateDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the
 * DistribMultivariateDistribution_t object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribMultivariateDistribution_t should be added.
 *
 * @return a new DistribMultivariateDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribMultivariateDistribution_t*
DistribUncertainty_createDistribMultivariateDistribution(DistribUncertainty_t*
  du);


/**
 * Creates a new DistribExternalDistribution_t object, adds it to this
 * DistribUncertainty_t object and returns the DistribExternalDistribution_t
 * object created.
 *
 * @param du the DistribUncertainty_t structure to which the
 * DistribExternalDistribution_t should be added.
 *
 * @return a new DistribExternalDistribution_t object instance.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
DistribExternalDistribution_t*
DistribUncertainty_createDistribExternalDistribution(DistribUncertainty_t* du);


/**
 * Unsets the value of the "uncertStatistics" element of this
 * DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_unsetUncertStatistics(DistribUncertainty_t * du);


/**
 * Unsets the value of the "distribution" element of this DistribUncertainty_t.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_unsetDistribution(DistribUncertainty_t * du);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUncertainty_t object have been set.
 *
 * @param du the DistribUncertainty_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribUncertainty_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribUncertainty_t
 */
LIBSBML_EXTERN
int
DistribUncertainty_hasRequiredAttributes(const DistribUncertainty_t * du);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribUncertainty_H__ */


