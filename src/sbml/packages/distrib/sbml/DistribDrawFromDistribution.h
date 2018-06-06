/**
 * @file DistribDrawFromDistribution.h
 * @brief Definition of the DistribDrawFromDistribution class.
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
 * @class DistribDrawFromDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribDrawFromDistribution
 * class.
 */


#ifndef DistribDrawFromDistribution_H__
#define DistribDrawFromDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribDistribution.h>
#include <sbml/packages/distrib/sbml/ListOfDistribInputs.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribDrawFromDistribution : public DistribBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfDistribInputs mDistribInputs;
  DistribDistribution* mDistribution;

  /** @endcond */

public:

  /**
   * Creates a new DistribDrawFromDistribution using the given SBML Level,
   * Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribDrawFromDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribDrawFromDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribDrawFromDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribDrawFromDistribution(
                              unsigned int level =
                                DistribExtension::getDefaultLevel(),
                              unsigned int version =
                                DistribExtension::getDefaultVersion(),
                              unsigned int pkgVersion =
                                DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribDrawFromDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribDrawFromDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribDrawFromDistribution.
   *
   * @param orig the DistribDrawFromDistribution instance to copy.
   */
  DistribDrawFromDistribution(const DistribDrawFromDistribution& orig);


  /**
   * Assignment operator for DistribDrawFromDistribution.
   *
   * @param rhs the DistribDrawFromDistribution object whose values are to be
   * used as the basis of the assignment.
   */
  DistribDrawFromDistribution& operator=(const DistribDrawFromDistribution&
    rhs);


  /**
   * Creates and returns a deep copy of this DistribDrawFromDistribution
   * object.
   *
   * @return a (deep) copy of this DistribDrawFromDistribution object.
   */
  virtual DistribDrawFromDistribution* clone() const;


  /**
   * Destructor for DistribDrawFromDistribution.
   */
  virtual ~DistribDrawFromDistribution();


  /**
   * Returns the value of the "distribution" element of this
   * DistribDrawFromDistribution.
   *
   * @return the value of the "distribution" element of this
   * DistribDrawFromDistribution as a DistribDistribution*.
   */
  const DistribDistribution* getDistribution() const;


  /**
   * Returns the value of the "distribution" element of this
   * DistribDrawFromDistribution.
   *
   * @return the value of the "distribution" element of this
   * DistribDrawFromDistribution as a DistribDistribution*.
   */
  DistribDistribution* getDistribution();


  /**
   * Predicate returning @c true if this DistribDrawFromDistribution's
   * "distribution" element is set.
   *
   * @return @c true if this DistribDrawFromDistribution's "distribution"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetDistribution() const;


  /**
   * Sets the value of the "distribution" element of this
   * DistribDrawFromDistribution.
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
   * Creates a new DistribBetaDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the DistribBetaDistribution
   * object created.
   *
   * @return a new DistribBetaDistribution object instance.
   */
  DistribBetaDistribution* createDistribBetaDistribution();


  /**
   * Creates a new DistribCauchyDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribCauchyDistribution object created.
   *
   * @return a new DistribCauchyDistribution object instance.
   */
  DistribCauchyDistribution* createDistribCauchyDistribution();


  /**
   * Creates a new DistribChiSquareDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribChiSquareDistribution object created.
   *
   * @return a new DistribChiSquareDistribution object instance.
   */
  DistribChiSquareDistribution* createDistribChiSquareDistribution();


  /**
   * Creates a new DistribExponentialDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribExponentialDistribution object created.
   *
   * @return a new DistribExponentialDistribution object instance.
   */
  DistribExponentialDistribution* createDistribExponentialDistribution();


  /**
   * Creates a new DistribFDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the DistribFDistribution
   * object created.
   *
   * @return a new DistribFDistribution object instance.
   */
  DistribFDistribution* createDistribFDistribution();


  /**
   * Creates a new DistribGammaDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribGammaDistribution object created.
   *
   * @return a new DistribGammaDistribution object instance.
   */
  DistribGammaDistribution* createDistribGammaDistribution();


  /**
   * Creates a new DistribInverseGammaDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribInverseGammaDistribution object created.
   *
   * @return a new DistribInverseGammaDistribution object instance.
   */
  DistribInverseGammaDistribution* createDistribInverseGammaDistribution();


  /**
   * Creates a new DistribLaPlaceDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribLaPlaceDistribution object created.
   *
   * @return a new DistribLaPlaceDistribution object instance.
   */
  DistribLaPlaceDistribution* createDistribLaPlaceDistribution();


  /**
   * Creates a new DistribLogNormalDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribLogNormalDistribution object created.
   *
   * @return a new DistribLogNormalDistribution object instance.
   */
  DistribLogNormalDistribution* createDistribLogNormalDistribution();


  /**
   * Creates a new DistribLogisticDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribLogisticDistribution object created.
   *
   * @return a new DistribLogisticDistribution object instance.
   */
  DistribLogisticDistribution* createDistribLogisticDistribution();


  /**
   * Creates a new DistribNormalDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribNormalDistribution object created.
   *
   * @return a new DistribNormalDistribution object instance.
   */
  DistribNormalDistribution* createDistribNormalDistribution();


  /**
   * Creates a new DistribParetoDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribParetoDistribution object created.
   *
   * @return a new DistribParetoDistribution object instance.
   */
  DistribParetoDistribution* createDistribParetoDistribution();


  /**
   * Creates a new DistribRayleighDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribRayleighDistribution object created.
   *
   * @return a new DistribRayleighDistribution object instance.
   */
  DistribRayleighDistribution* createDistribRayleighDistribution();


  /**
   * Creates a new DistribStudentTDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribStudentTDistribution object created.
   *
   * @return a new DistribStudentTDistribution object instance.
   */
  DistribStudentTDistribution* createDistribStudentTDistribution();


  /**
   * Creates a new DistribUniformDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribUniformDistribution object created.
   *
   * @return a new DistribUniformDistribution object instance.
   */
  DistribUniformDistribution* createDistribUniformDistribution();


  /**
   * Creates a new DistribWeibullDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribWeibullDistribution object created.
   *
   * @return a new DistribWeibullDistribution object instance.
   */
  DistribWeibullDistribution* createDistribWeibullDistribution();


  /**
   * Creates a new DistribBinomialDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribBinomialDistribution object created.
   *
   * @return a new DistribBinomialDistribution object instance.
   */
  DistribBinomialDistribution* createDistribBinomialDistribution();


  /**
   * Creates a new DistribGeometricDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribGeometricDistribution object created.
   *
   * @return a new DistribGeometricDistribution object instance.
   */
  DistribGeometricDistribution* createDistribGeometricDistribution();


  /**
   * Creates a new DistribHypergeometricDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribHypergeometricDistribution object created.
   *
   * @return a new DistribHypergeometricDistribution object instance.
   */
  DistribHypergeometricDistribution* createDistribHypergeometricDistribution();


  /**
   * Creates a new DistribNegativeBinomialDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribNegativeBinomialDistribution object created.
   *
   * @return a new DistribNegativeBinomialDistribution object instance.
   */
  DistribNegativeBinomialDistribution*
    createDistribNegativeBinomialDistribution();


  /**
   * Creates a new DistribPoissonDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribPoissonDistribution object created.
   *
   * @return a new DistribPoissonDistribution object instance.
   */
  DistribPoissonDistribution* createDistribPoissonDistribution();


  /**
   * Creates a new DistribBernoulliDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribBernoulliDistribution object created.
   *
   * @return a new DistribBernoulliDistribution object instance.
   */
  DistribBernoulliDistribution* createDistribBernoulliDistribution();


  /**
   * Creates a new DistribCategoricalDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribCategoricalDistribution object created.
   *
   * @return a new DistribCategoricalDistribution object instance.
   */
  DistribCategoricalDistribution* createDistribCategoricalDistribution();


  /**
   * Creates a new DistribMultivariateDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribMultivariateDistribution object created.
   *
   * @return a new DistribMultivariateDistribution object instance.
   */
  DistribMultivariateDistribution* createDistribMultivariateDistribution();


  /**
   * Creates a new DistribExternalDistribution object, adds it to this
   * DistribDrawFromDistribution object and returns the
   * DistribExternalDistribution object created.
   *
   * @return a new DistribExternalDistribution object instance.
   */
  DistribExternalDistribution* createDistribExternalDistribution();


  /**
   * Unsets the value of the "distribution" element of this
   * DistribDrawFromDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDistribution();


  /**
   * Returns the ListOfDistribInputs from this DistribDrawFromDistribution.
   *
   * @return the ListOfDistribInputs from this DistribDrawFromDistribution.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see getDistribInput(const std::string& sid)
   * @see getDistribInput(unsigned int n)
   * @see getDistribInputByIndex(unsigned int n)
   * @see getNumDistribInputs()
   * @see removeDistribInput(const std::string& sid)
   * @see removeDistribInput(unsigned int n)
   */
  const ListOfDistribInputs* getListOfDistribInputs() const;


  /**
   * Returns the ListOfDistribInputs from this DistribDrawFromDistribution.
   *
   * @return the ListOfDistribInputs from this DistribDrawFromDistribution.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see getDistribInput(const std::string& sid)
   * @see getDistribInput(unsigned int n)
   * @see getDistribInputByIndex(unsigned int n)
   * @see getNumDistribInputs()
   * @see removeDistribInput(const std::string& sid)
   * @see removeDistribInput(unsigned int n)
   */
  ListOfDistribInputs* getListOfDistribInputs();


  /**
   * Get a DistribInput from the DistribDrawFromDistribution.
   *
   * @param n an unsigned int representing the index of the DistribInput to
   * retrieve.
   *
   * @return the nth DistribInput in the ListOfDistribInputs within this
   * DistribDrawFromDistribution.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see getDistribInput(const std::string& sid)
   * @see getDistribInputByIndex(unsigned int n)
   * @see getNumDistribInputs()
   * @see removeDistribInput(const std::string& sid)
   * @see removeDistribInput(unsigned int n)
   */
  DistribInput* getDistribInput(unsigned int n);


  /**
   * Get a DistribInput from the DistribDrawFromDistribution.
   *
   * @param n an unsigned int representing the index of the DistribInput to
   * retrieve.
   *
   * @return the nth DistribInput in the ListOfDistribInputs within this
   * DistribDrawFromDistribution.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see getDistribInput(const std::string& sid)
   * @see getDistribInputByIndex(unsigned int n)
   * @see getNumDistribInputs()
   * @see removeDistribInput(const std::string& sid)
   * @see removeDistribInput(unsigned int n)
   */
  const DistribInput* getDistribInput(unsigned int n) const;


  /**
   * Get a DistribInput from the DistribDrawFromDistribution based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the DistribInput to
   * retrieve.
   *
   * @return the DistribInput in the ListOfDistribInputs within this
   * DistribDrawFromDistribution with the given @p sid or @c NULL if no such
   * DistribInput exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see getDistribInput(unsigned int n)
   * @see getDistribInputByIndex(unsigned int n)
   * @see getNumDistribInputs()
   * @see removeDistribInput(const std::string& sid)
   * @see removeDistribInput(unsigned int n)
   */
  DistribInput* getDistribInput(const std::string& sid);


  /**
   * Get a DistribInput from the DistribDrawFromDistribution based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the DistribInput to
   * retrieve.
   *
   * @return the DistribInput in the ListOfDistribInputs within this
   * DistribDrawFromDistribution with the given @p sid or @c NULL if no such
   * DistribInput exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see getDistribInput(unsigned int n)
   * @see getDistribInputByIndex(unsigned int n)
   * @see getNumDistribInputs()
   * @see removeDistribInput(const std::string& sid)
   * @see removeDistribInput(unsigned int n)
   */
  const DistribInput* getDistribInput(const std::string& sid) const;


  /**
   * Get a DistribInput from the DistribDrawFromDistribution.
   *
   * @param n an unsigned int representing the "index" attribute
   * value of the DistribInput to retrieve.
   *
   * @return the DistribInput in the ListOfDistribInputs with the
   * given "index" value.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see getDistribInput(const std::string& sid)
   * @see getNumDistribInputs()
   * @see removeDistribInput(const std::string& sid)
   * @see removeDistribInput(unsigned int n)
   */
  DistribInput* getDistribInputByIndex(unsigned int n);


  /**
   * Get a DistribInput from the DistribDrawFromDistribution.
   *
   * @param n an unsigned int representing the "index" attribute
   * value of the DistribInput to retrieve.
   *
   * @return the DistribInput in the ListOfDistribInputs with the
   * given "index" value.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see getDistribInput(const std::string& sid)
   * @see getNumDistribInputs()
   * @see removeDistribInput(const std::string& sid)
   * @see removeDistribInput(unsigned int n)
   */
  const DistribInput* getDistribInputByIndex(unsigned int n) const;


  /**
   * Adds a copy of the given DistribInput to this DistribDrawFromDistribution.
   *
   * @param di the DistribInput object to add.
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
   * @see createDistribInput()
   * @see getDistribInput(const std::string& sid)
   * @see getDistribInput(unsigned int n)
   * @see getDistribInputByIndex(unsigned int n)
   * @see getNumDistribInputs()
   * @see removeDistribInput(const std::string& sid)
   * @see removeDistribInput(unsigned int n)
   */
  int addDistribInput(const DistribInput* di);


  /**
   * Get the number of DistribInput objects in this
   * DistribDrawFromDistribution.
   *
   * @return the number of DistribInput objects in this
   * DistribDrawFromDistribution.
   *
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see getDistribInput(const std::string& sid)
   * @see getDistribInput(unsigned int n)
   * @see getDistribInputByIndex(unsigned int n)
   * @see removeDistribInput(const std::string& sid)
   * @see removeDistribInput(unsigned int n)
   */
  unsigned int getNumDistribInputs() const;


  /**
   * Creates a new DistribInput object, adds it to this
   * DistribDrawFromDistribution object and returns the DistribInput object
   * created.
   *
   * @return a new DistribInput object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see getDistribInput(const std::string& sid)
   * @see getDistribInput(unsigned int n)
   * @see getDistribInputByIndex(unsigned int n)
   * @see getNumDistribInputs()
   * @see removeDistribInput(const std::string& sid)
   * @see removeDistribInput(unsigned int n)
   */
  DistribInput* createDistribInput();


  /**
   * Removes the nth DistribInput from this DistribDrawFromDistribution and
   * returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the DistribInput to
   * remove.
   *
   * @return a pointer to the nth DistribInput in this
   * DistribDrawFromDistribution.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see getDistribInput(const std::string& sid)
   * @see getDistribInput(unsigned int n)
   * @see getDistribInputByIndex(unsigned int n)
   * @see getNumDistribInputs()
   * @see removeDistribInput(const std::string& sid)
   */
  DistribInput* removeDistribInput(unsigned int n);


  /**
   * Removes the DistribInput from this DistribDrawFromDistribution based on
   * its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the DistribInput to
   * remove.
   *
   * @return the DistribInput in this DistribDrawFromDistribution based on the
   * identifier or NULL if no such DistribInput exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribInput(const DistribInput* object)
   * @see createDistribInput()
   * @see getDistribInput(const std::string& sid)
   * @see getDistribInput(unsigned int n)
   * @see getDistribInputByIndex(unsigned int n)
   * @see getNumDistribInputs()
   * @see removeDistribInput(unsigned int n)
   */
  DistribInput* removeDistribInput(const std::string& sid);


  /**
   * Returns the XML element name of this DistribDrawFromDistribution object.
   *
   * For DistribDrawFromDistribution, the XML element name is always
   * @c "drawFromDistribution".
   *
   * @return the name of this element, i.e. @c "drawFromDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribDrawFromDistribution object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_DRAWFROMDISTRIBUTION, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribDrawFromDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribDrawFromDistribution have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribDrawFromDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribDrawFromDistribution have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the DistribDrawFromDistribution object
   * are:
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
   * Gets the value of the "attributeName" attribute of this
   * DistribDrawFromDistribution.
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
   * DistribDrawFromDistribution.
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
   * DistribDrawFromDistribution.
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
   * DistribDrawFromDistribution.
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
   * DistribDrawFromDistribution.
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
   * Predicate returning @c true if this DistribDrawFromDistribution's
   * attribute "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribDrawFromDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribDrawFromDistribution.
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
   * DistribDrawFromDistribution.
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
   * DistribDrawFromDistribution.
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
   * DistribDrawFromDistribution.
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
   * DistribDrawFromDistribution.
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
   * DistribDrawFromDistribution.
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
   * DistribDrawFromDistribution.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribDrawFromDistribution.
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
   * DistribDrawFromDistribution.
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
   * Returns the number of "elementName" in this DistribDrawFromDistribution.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this
   * DistribDrawFromDistribution.
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
 * Creates a new DistribDrawFromDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribDrawFromDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribDrawFromDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribDrawFromDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribDrawFromDistribution_t *
DistribDrawFromDistribution_create(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribDrawFromDistribution_t
 * object.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure.
 *
 * @return a (deep) copy of this DistribDrawFromDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribDrawFromDistribution_t*
DistribDrawFromDistribution_clone(const DistribDrawFromDistribution_t* ddfd);


/**
 * Frees this DistribDrawFromDistribution_t object.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
void
DistribDrawFromDistribution_free(DistribDrawFromDistribution_t* ddfd);


/**
 * Returns the value of the "distribution" element of this
 * DistribDrawFromDistribution_t.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure whose distribution
 * is sought.
 *
 * @return the value of the "distribution" element of this
 * DistribDrawFromDistribution_t as a DistribDistribution*.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
const DistribDistribution_t*
DistribDrawFromDistribution_getDistribution(const DistribDrawFromDistribution_t
  * ddfd);


/**
 * Predicate returning @c 1 (true) if this DistribDrawFromDistribution_t's
 * "distribution" element is set.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribDrawFromDistribution_t's "distribution"
 * element has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_isSetDistribution(const
  DistribDrawFromDistribution_t * ddfd);


/**
 * Sets the value of the "distribution" element of this
 * DistribDrawFromDistribution_t.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure.
 *
 * @param distribution DistribDistribution_t* value of the "distribution"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_setDistribution(
                                            DistribDrawFromDistribution_t *
                                              ddfd,
                                            const DistribDistribution_t*
                                              distribution);


/**
 * Creates a new DistribBetaDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribBetaDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribBetaDistribution_t should be added.
 *
 * @return a new DistribBetaDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribBetaDistribution_t*
DistribDrawFromDistribution_createDistribBetaDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribCauchyDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribCauchyDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribCauchyDistribution_t should be added.
 *
 * @return a new DistribCauchyDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribCauchyDistribution_t*
DistribDrawFromDistribution_createDistribCauchyDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribChiSquareDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribChiSquareDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribChiSquareDistribution_t should be added.
 *
 * @return a new DistribChiSquareDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribChiSquareDistribution_t*
DistribDrawFromDistribution_createDistribChiSquareDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribExponentialDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribExponentialDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribExponentialDistribution_t should be added.
 *
 * @return a new DistribExponentialDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribExponentialDistribution_t*
DistribDrawFromDistribution_createDistribExponentialDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribFDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the DistribFDistribution_t
 * object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribFDistribution_t should be added.
 *
 * @return a new DistribFDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribFDistribution_t*
DistribDrawFromDistribution_createDistribFDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribGammaDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribGammaDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribGammaDistribution_t should be added.
 *
 * @return a new DistribGammaDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribGammaDistribution_t*
DistribDrawFromDistribution_createDistribGammaDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribInverseGammaDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribInverseGammaDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribInverseGammaDistribution_t should be added.
 *
 * @return a new DistribInverseGammaDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribInverseGammaDistribution_t*
DistribDrawFromDistribution_createDistribInverseGammaDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribLaPlaceDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribLaPlaceDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribLaPlaceDistribution_t should be added.
 *
 * @return a new DistribLaPlaceDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribLaPlaceDistribution_t*
DistribDrawFromDistribution_createDistribLaPlaceDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribLogNormalDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribLogNormalDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribLogNormalDistribution_t should be added.
 *
 * @return a new DistribLogNormalDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribLogNormalDistribution_t*
DistribDrawFromDistribution_createDistribLogNormalDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribLogisticDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribLogisticDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribLogisticDistribution_t should be added.
 *
 * @return a new DistribLogisticDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribLogisticDistribution_t*
DistribDrawFromDistribution_createDistribLogisticDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribNormalDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribNormalDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribNormalDistribution_t should be added.
 *
 * @return a new DistribNormalDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribNormalDistribution_t*
DistribDrawFromDistribution_createDistribNormalDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribParetoDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribParetoDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribParetoDistribution_t should be added.
 *
 * @return a new DistribParetoDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribParetoDistribution_t*
DistribDrawFromDistribution_createDistribParetoDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribRayleighDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribRayleighDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribRayleighDistribution_t should be added.
 *
 * @return a new DistribRayleighDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribRayleighDistribution_t*
DistribDrawFromDistribution_createDistribRayleighDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribStudentTDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribStudentTDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribStudentTDistribution_t should be added.
 *
 * @return a new DistribStudentTDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribStudentTDistribution_t*
DistribDrawFromDistribution_createDistribStudentTDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribUniformDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribUniformDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribUniformDistribution_t should be added.
 *
 * @return a new DistribUniformDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribUniformDistribution_t*
DistribDrawFromDistribution_createDistribUniformDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribWeibullDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribWeibullDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribWeibullDistribution_t should be added.
 *
 * @return a new DistribWeibullDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribWeibullDistribution_t*
DistribDrawFromDistribution_createDistribWeibullDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribBinomialDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribBinomialDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribBinomialDistribution_t should be added.
 *
 * @return a new DistribBinomialDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribBinomialDistribution_t*
DistribDrawFromDistribution_createDistribBinomialDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribGeometricDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribGeometricDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribGeometricDistribution_t should be added.
 *
 * @return a new DistribGeometricDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribGeometricDistribution_t*
DistribDrawFromDistribution_createDistribGeometricDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribHypergeometricDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribHypergeometricDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribHypergeometricDistribution_t should be added.
 *
 * @return a new DistribHypergeometricDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribHypergeometricDistribution_t*
DistribDrawFromDistribution_createDistribHypergeometricDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribNegativeBinomialDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribNegativeBinomialDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribNegativeBinomialDistribution_t should be added.
 *
 * @return a new DistribNegativeBinomialDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribNegativeBinomialDistribution_t*
DistribDrawFromDistribution_createDistribNegativeBinomialDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribPoissonDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribPoissonDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribPoissonDistribution_t should be added.
 *
 * @return a new DistribPoissonDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribPoissonDistribution_t*
DistribDrawFromDistribution_createDistribPoissonDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribBernoulliDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribBernoulliDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribBernoulliDistribution_t should be added.
 *
 * @return a new DistribBernoulliDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribBernoulliDistribution_t*
DistribDrawFromDistribution_createDistribBernoulliDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribCategoricalDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribCategoricalDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribCategoricalDistribution_t should be added.
 *
 * @return a new DistribCategoricalDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribCategoricalDistribution_t*
DistribDrawFromDistribution_createDistribCategoricalDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribMultivariateDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribMultivariateDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribMultivariateDistribution_t should be added.
 *
 * @return a new DistribMultivariateDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribMultivariateDistribution_t*
DistribDrawFromDistribution_createDistribMultivariateDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribExternalDistribution_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the
 * DistribExternalDistribution_t object created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribExternalDistribution_t should be added.
 *
 * @return a new DistribExternalDistribution_t object instance.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribExternalDistribution_t*
DistribDrawFromDistribution_createDistribExternalDistribution(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Unsets the value of the "distribution" element of this
 * DistribDrawFromDistribution_t.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_unsetDistribution(DistribDrawFromDistribution_t *
  ddfd);


/**
 * Returns a ListOf_t * containing DistribInput_t objects from this
 * DistribDrawFromDistribution_t.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure whose
 * ListOfDistribInputs is sought.
 *
 * @return the ListOfDistribInputs from this DistribDrawFromDistribution_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see DistribDrawFromDistribution_addDistribInput()
 * @see DistribDrawFromDistribution_createDistribInput()
 * @see DistribDrawFromDistribution_getDistribInputById()
 * @see DistribDrawFromDistribution_getDistribInput()
 * @see DistribDrawFromDistribution_getNumDistribInputs()
 * @see DistribDrawFromDistribution_removeDistribInputById()
 * @see DistribDrawFromDistribution_removeDistribInput()
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
ListOf_t*
DistribDrawFromDistribution_getListOfDistribInputs(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Get a DistribInput_t from the DistribDrawFromDistribution_t.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to search.
 *
 * @param n an unsigned int representing the index of the DistribInput_t to
 * retrieve.
 *
 * @return the nth DistribInput_t in the ListOfDistribInputs within this
 * DistribDrawFromDistribution.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribInput_t*
DistribDrawFromDistribution_getDistribInput(
                                            DistribDrawFromDistribution_t*
                                              ddfd,
                                            unsigned int n);


/**
 * Get a DistribInput_t from the DistribDrawFromDistribution_t based on its
 * identifier.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to search.
 *
 * @param sid a string representing the identifier of the DistribInput_t to
 * retrieve.
 *
 * @return the DistribInput_t in the ListOfDistribInputs within this
 * DistribDrawFromDistribution with the given @p sid or @c NULL if no such
 * DistribInput_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribInput_t*
DistribDrawFromDistribution_getDistribInputById(
                                                DistribDrawFromDistribution_t*
                                                  ddfd,
                                                const char *sid);


/**
 * Adds a copy of the given DistribInput_t to this
 * DistribDrawFromDistribution_t.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribInput_t should be added.
 *
 * @param di the DistribInput_t object to add.
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
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_addDistribInput(
                                            DistribDrawFromDistribution_t*
                                              ddfd,
                                            const DistribInput_t* di);


/**
 * Get the number of DistribInput_t objects in this
 * DistribDrawFromDistribution_t.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to query.
 *
 * @return the number of DistribInput_t objects in this
 * DistribDrawFromDistribution_t.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
unsigned int
DistribDrawFromDistribution_getNumDistribInputs(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Creates a new DistribInput_t object, adds it to this
 * DistribDrawFromDistribution_t object and returns the DistribInput_t object
 * created.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to which the
 * DistribInput_t should be added.
 *
 * @return a new DistribInput_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribInput_t*
DistribDrawFromDistribution_createDistribInput(DistribDrawFromDistribution_t*
  ddfd);


/**
 * Removes the nth DistribInput_t from this DistribDrawFromDistribution_t and
 * returns a pointer to it.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to search.
 *
 * @param n an unsigned int representing the index of the DistribInput_t to
 * remove.
 *
 * @return a pointer to the nth DistribInput_t in this
 * DistribDrawFromDistribution_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribInput_t*
DistribDrawFromDistribution_removeDistribInput(
                                               DistribDrawFromDistribution_t*
                                                 ddfd,
                                               unsigned int n);


/**
 * Removes the DistribInput_t from this DistribDrawFromDistribution_t based on
 * its identifier and returns a pointer to it.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure to search.
 *
 * @param sid a string representing the identifier of the DistribInput_t to
 * remove.
 *
 * @return the DistribInput_t in this DistribDrawFromDistribution_t based on
 * the identifier or NULL if no such DistribInput_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
DistribInput_t*
DistribDrawFromDistribution_removeDistribInputById(
                                                   DistribDrawFromDistribution_t*
                                                     ddfd,
                                                   const char* sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribDrawFromDistribution_t object have been set.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribDrawFromDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_hasRequiredAttributes(const
  DistribDrawFromDistribution_t * ddfd);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribDrawFromDistribution_t object have been set.
 *
 * @param ddfd the DistribDrawFromDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribDrawFromDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required elements for the DistribDrawFromDistribution_t object
 * are:
 *
 * @memberof DistribDrawFromDistribution_t
 */
LIBSBML_EXTERN
int
DistribDrawFromDistribution_hasRequiredElements(const
  DistribDrawFromDistribution_t * ddfd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribDrawFromDistribution_H__ */


