/**
 * @file DistribContinuousUnivariateDistribution.h
 * @brief Definition of the DistribContinuousUnivariateDistribution class.
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
 * @class DistribContinuousUnivariateDistribution
 * @sbmlbrief{distrib} TODO:Definition of the
 * DistribContinuousUnivariateDistribution class.
 */


#ifndef DistribContinuousUnivariateDistribution_H__
#define DistribContinuousUnivariateDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribUnivariateDistribution.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertBound.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class DistribBetaDistribution;
class DistribCauchyDistribution;
class DistribChiSquareDistribution;
class DistribExponentialDistribution;
class DistribFDistribution;
class DistribGammaDistribution;
class DistribInverseGammaDistribution;
class DistribLaPlaceDistribution;
class DistribLogNormalDistribution;
class DistribLogisticDistribution;
class DistribNormalDistribution;
class DistribParetoDistribution;
class DistribRayleighDistribution;
class DistribStudentTDistribution;
class DistribUniformDistribution;
class DistribWeibullDistribution;

class LIBSBML_EXTERN DistribContinuousUnivariateDistribution : public
  DistribUnivariateDistribution
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribUncertBound* mTruncationLowerBound;
  DistribUncertBound* mTruncationUpperBound;

  /** @endcond */

public:

  /**
   * Creates a new DistribContinuousUnivariateDistribution using the given SBML
   * Level, Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribContinuousUnivariateDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribContinuousUnivariateDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribContinuousUnivariateDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribContinuousUnivariateDistribution(
                                          unsigned int level =
                                            DistribExtension::getDefaultLevel(),
                                          unsigned int version =
                                            DistribExtension::getDefaultVersion(),
                                          unsigned int pkgVersion =
                                            DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribContinuousUnivariateDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribContinuousUnivariateDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribContinuousUnivariateDistribution.
   *
   * @param orig the DistribContinuousUnivariateDistribution instance to copy.
   */
  DistribContinuousUnivariateDistribution(const
    DistribContinuousUnivariateDistribution& orig);


  /**
   * Assignment operator for DistribContinuousUnivariateDistribution.
   *
   * @param rhs the DistribContinuousUnivariateDistribution object whose values
   * are to be used as the basis of the assignment.
   */
  DistribContinuousUnivariateDistribution& operator=(const
    DistribContinuousUnivariateDistribution& rhs);


  /**
   * Creates and returns a deep copy of this
   * DistribContinuousUnivariateDistribution object.
   *
   * @return a (deep) copy of this DistribContinuousUnivariateDistribution
   * object.
   */
  virtual DistribContinuousUnivariateDistribution* clone() const;


  /**
   * Destructor for DistribContinuousUnivariateDistribution.
   */
  virtual ~DistribContinuousUnivariateDistribution();


  /**
   * Returns the value of the "id" attribute of this
   * DistribContinuousUnivariateDistribution.
   *
   * @return the value of the "id" attribute of this
   * DistribContinuousUnivariateDistribution as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this
   * DistribContinuousUnivariateDistribution.
   *
   * @return the value of the "name" attribute of this
   * DistribContinuousUnivariateDistribution as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this
   * DistribContinuousUnivariateDistribution's "id" attribute is set.
   *
   * @return @c true if this DistribContinuousUnivariateDistribution's "id"
   * attribute has been set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this
   * DistribContinuousUnivariateDistribution's "name" attribute is set.
   *
   * @return @c true if this DistribContinuousUnivariateDistribution's "name"
   * attribute has been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this
   * DistribContinuousUnivariateDistribution.
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
   * Sets the value of the "name" attribute of this
   * DistribContinuousUnivariateDistribution.
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
   * Unsets the value of the "id" attribute of this
   * DistribContinuousUnivariateDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this
   * DistribContinuousUnivariateDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "truncationLowerBound" element of this
   * DistribContinuousUnivariateDistribution.
   *
   * @return the value of the "truncationLowerBound" element of this
   * DistribContinuousUnivariateDistribution as a DistribUncertBound*.
   */
  const DistribUncertBound* getTruncationLowerBound() const;


  /**
   * Returns the value of the "truncationLowerBound" element of this
   * DistribContinuousUnivariateDistribution.
   *
   * @return the value of the "truncationLowerBound" element of this
   * DistribContinuousUnivariateDistribution as a DistribUncertBound*.
   */
  DistribUncertBound* getTruncationLowerBound();


  /**
   * Returns the value of the "truncationUpperBound" element of this
   * DistribContinuousUnivariateDistribution.
   *
   * @return the value of the "truncationUpperBound" element of this
   * DistribContinuousUnivariateDistribution as a DistribUncertBound*.
   */
  const DistribUncertBound* getTruncationUpperBound() const;


  /**
   * Returns the value of the "truncationUpperBound" element of this
   * DistribContinuousUnivariateDistribution.
   *
   * @return the value of the "truncationUpperBound" element of this
   * DistribContinuousUnivariateDistribution as a DistribUncertBound*.
   */
  DistribUncertBound* getTruncationUpperBound();


  /**
   * Predicate returning @c true if this
   * DistribContinuousUnivariateDistribution's "truncationLowerBound" element
   * is set.
   *
   * @return @c true if this DistribContinuousUnivariateDistribution's
   * "truncationLowerBound" element has been set, otherwise @c false is
   * returned.
   */
  bool isSetTruncationLowerBound() const;


  /**
   * Predicate returning @c true if this
   * DistribContinuousUnivariateDistribution's "truncationUpperBound" element
   * is set.
   *
   * @return @c true if this DistribContinuousUnivariateDistribution's
   * "truncationUpperBound" element has been set, otherwise @c false is
   * returned.
   */
  bool isSetTruncationUpperBound() const;


  /**
   * Sets the value of the "truncationLowerBound" element of this
   * DistribContinuousUnivariateDistribution.
   *
   * @param truncationLowerBound DistribUncertBound* value of the
   * "truncationLowerBound" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setTruncationLowerBound(const DistribUncertBound* truncationLowerBound);


  /**
   * Sets the value of the "truncationUpperBound" element of this
   * DistribContinuousUnivariateDistribution.
   *
   * @param truncationUpperBound DistribUncertBound* value of the
   * "truncationUpperBound" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setTruncationUpperBound(const DistribUncertBound* truncationUpperBound);


  /**
   * Creates a new DistribUncertBound object, adds it to this
   * DistribContinuousUnivariateDistribution object and returns the
   * DistribUncertBound object created.
   *
   * @return a new DistribUncertBound object instance.
   */
  DistribUncertBound* createTruncationLowerBound();


  /**
   * Creates a new DistribUncertBound object, adds it to this
   * DistribContinuousUnivariateDistribution object and returns the
   * DistribUncertBound object created.
   *
   * @return a new DistribUncertBound object instance.
   */
  DistribUncertBound* createTruncationUpperBound();


  /**
   * Unsets the value of the "truncationLowerBound" element of this
   * DistribContinuousUnivariateDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetTruncationLowerBound();


  /**
   * Unsets the value of the "truncationUpperBound" element of this
   * DistribContinuousUnivariateDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetTruncationUpperBound();


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribBetaDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribBetaDistribution, @c false otherwise
   */
  virtual bool isDistribBetaDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribCauchyDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribCauchyDistribution, @c false otherwise
   */
  virtual bool isDistribCauchyDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribChiSquareDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribChiSquareDistribution, @c false otherwise
   */
  virtual bool isDistribChiSquareDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribExponentialDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribExponentialDistribution, @c false otherwise
   */
  virtual bool isDistribExponentialDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type DistribFDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribFDistribution, @c false otherwise
   */
  virtual bool isDistribFDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribGammaDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribGammaDistribution, @c false otherwise
   */
  virtual bool isDistribGammaDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribInverseGammaDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribInverseGammaDistribution, @c false otherwise
   */
  virtual bool isDistribInverseGammaDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribLaPlaceDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribLaPlaceDistribution, @c false otherwise
   */
  virtual bool isDistribLaPlaceDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribLogNormalDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribLogNormalDistribution, @c false otherwise
   */
  virtual bool isDistribLogNormalDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribLogisticDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribLogisticDistribution, @c false otherwise
   */
  virtual bool isDistribLogisticDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribNormalDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribNormalDistribution, @c false otherwise
   */
  virtual bool isDistribNormalDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribParetoDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribParetoDistribution, @c false otherwise
   */
  virtual bool isDistribParetoDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribRayleighDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribRayleighDistribution, @c false otherwise
   */
  virtual bool isDistribRayleighDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribStudentTDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribStudentTDistribution, @c false otherwise
   */
  virtual bool isDistribStudentTDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribUniformDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribUniformDistribution, @c false otherwise
   */
  virtual bool isDistribUniformDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribContinuousUnivariateDistribution" is of type
   * DistribWeibullDistribution
   *
   * @return @c true if this abstract "DistribContinuousUnivariateDistribution"
   * is of type DistribWeibullDistribution, @c false otherwise
   */
  virtual bool isDistribWeibullDistribution() const;


  /**
   * Returns the XML element name of this
   * DistribContinuousUnivariateDistribution object.
   *
   * For DistribContinuousUnivariateDistribution, the XML element name is
   * always @c "continuousUnivariateDistribution".
   *
   * @return the name of this element, i.e.
   * @c "continuousUnivariateDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this
   * DistribContinuousUnivariateDistribution object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_CONTINUOUSUNIVARIATEDISTRIBUTION,
   * SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribContinuousUnivariateDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribContinuousUnivariateDistribution have been set, otherwise @c false
   * is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribContinuousUnivariateDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribContinuousUnivariateDistribution have been set, otherwise @c false
   * is returned.
   *
   *
   * @note The required elements for the
   * DistribContinuousUnivariateDistribution object are:
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
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
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
   * Predicate returning @c true if this
   * DistribContinuousUnivariateDistribution's attribute "attributeName" is
   * set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribContinuousUnivariateDistribution's
   * attribute "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
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
   * Returns the number of "elementName" in this
   * DistribContinuousUnivariateDistribution.
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
   * DistribContinuousUnivariateDistribution.
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
 * Creates a new DistribContinuousUnivariateDistribution_t using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribContinuousUnivariateDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
DistribContinuousUnivariateDistribution_t *
DistribContinuousUnivariateDistribution_create(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this
 * DistribContinuousUnivariateDistribution_t object.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return a (deep) copy of this DistribContinuousUnivariateDistribution_t
 * object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
DistribContinuousUnivariateDistribution_t*
DistribContinuousUnivariateDistribution_clone(const
  DistribContinuousUnivariateDistribution_t* dcud);


/**
 * Frees this DistribContinuousUnivariateDistribution_t object.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
void
DistribContinuousUnivariateDistribution_free(DistribContinuousUnivariateDistribution_t*
  dcud);


/**
 * Returns the value of the "id" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure whose id
 * is sought.
 *
 * @return the value of the "id" attribute of this
 * DistribContinuousUnivariateDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
char *
DistribContinuousUnivariateDistribution_getId(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Returns the value of the "name" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure whose
 * name is sought.
 *
 * @return the value of the "name" attribute of this
 * DistribContinuousUnivariateDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
char *
DistribContinuousUnivariateDistribution_getName(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 (true) if this
 * DistribContinuousUnivariateDistribution_t's "id" attribute is set.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribContinuousUnivariateDistribution_t's "id"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isSetId(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 (true) if this
 * DistribContinuousUnivariateDistribution_t's "name" attribute is set.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribContinuousUnivariateDistribution_t's
 * "name" attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isSetName(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Sets the value of the "id" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribContinuousUnivariateDistribution_unsetId().
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_setId(
                                              DistribContinuousUnivariateDistribution_t
                                                * dcud,
                                              const char * id);


/**
 * Sets the value of the "name" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribContinuousUnivariateDistribution_unsetName().
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_setName(
                                                DistribContinuousUnivariateDistribution_t
                                                  * dcud,
                                                const char * name);


/**
 * Unsets the value of the "id" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_unsetId(DistribContinuousUnivariateDistribution_t
  * dcud);


/**
 * Unsets the value of the "name" attribute of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_unsetName(DistribContinuousUnivariateDistribution_t
  * dcud);


/**
 * Returns the value of the "truncationLowerBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure whose
 * truncationLowerBound is sought.
 *
 * @return the value of the "truncationLowerBound" element of this
 * DistribContinuousUnivariateDistribution_t as a DistribUncertBound*.
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertBound_t*
DistribContinuousUnivariateDistribution_getTruncationLowerBound(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Returns the value of the "truncationUpperBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure whose
 * truncationUpperBound is sought.
 *
 * @return the value of the "truncationUpperBound" element of this
 * DistribContinuousUnivariateDistribution_t as a DistribUncertBound*.
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertBound_t*
DistribContinuousUnivariateDistribution_getTruncationUpperBound(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 (true) if this
 * DistribContinuousUnivariateDistribution_t's "truncationLowerBound" element
 * is set.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribContinuousUnivariateDistribution_t's
 * "truncationLowerBound" element has been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isSetTruncationLowerBound(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 (true) if this
 * DistribContinuousUnivariateDistribution_t's "truncationUpperBound" element
 * is set.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribContinuousUnivariateDistribution_t's
 * "truncationUpperBound" element has been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isSetTruncationUpperBound(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Sets the value of the "truncationLowerBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @param truncationLowerBound DistribUncertBound_t* value of the
 * "truncationLowerBound" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_setTruncationLowerBound(
                                                                DistribContinuousUnivariateDistribution_t
                                                                  * dcud,
                                                                const
                                                                  DistribUncertBound_t*
                                                                    truncationLowerBound);


/**
 * Sets the value of the "truncationUpperBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @param truncationUpperBound DistribUncertBound_t* value of the
 * "truncationUpperBound" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_setTruncationUpperBound(
                                                                DistribContinuousUnivariateDistribution_t
                                                                  * dcud,
                                                                const
                                                                  DistribUncertBound_t*
                                                                    truncationUpperBound);


/**
 * Creates a new DistribUncertBound_t object, adds it to this
 * DistribContinuousUnivariateDistribution_t object and returns the
 * DistribUncertBound_t object created.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure to which
 * the DistribUncertBound_t should be added.
 *
 * @return a new DistribUncertBound_t object instance.
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
DistribUncertBound_t*
DistribContinuousUnivariateDistribution_createTruncationLowerBound(DistribContinuousUnivariateDistribution_t*
  dcud);


/**
 * Creates a new DistribUncertBound_t object, adds it to this
 * DistribContinuousUnivariateDistribution_t object and returns the
 * DistribUncertBound_t object created.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure to which
 * the DistribUncertBound_t should be added.
 *
 * @return a new DistribUncertBound_t object instance.
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
DistribUncertBound_t*
DistribContinuousUnivariateDistribution_createTruncationUpperBound(DistribContinuousUnivariateDistribution_t*
  dcud);


/**
 * Unsets the value of the "truncationLowerBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_unsetTruncationLowerBound(DistribContinuousUnivariateDistribution_t
  * dcud);


/**
 * Unsets the value of the "truncationUpperBound" element of this
 * DistribContinuousUnivariateDistribution_t.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_unsetTruncationUpperBound(DistribContinuousUnivariateDistribution_t
  * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribBetaDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribBetaDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribBetaDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribCauchyDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribCauchyDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribCauchyDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribChiSquareDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribChiSquareDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribChiSquareDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribExponentialDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribExponentialDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribExponentialDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribFDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribFDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribFDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribGammaDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribGammaDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribGammaDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribInverseGammaDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribInverseGammaDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribInverseGammaDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribLaPlaceDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribLaPlaceDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribLaPlaceDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribLogNormalDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribLogNormalDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribLogNormalDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribLogisticDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribLogisticDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribLogisticDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribNormalDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribNormalDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribNormalDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribParetoDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribParetoDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribParetoDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribRayleighDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribRayleighDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribRayleighDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribStudentTDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribStudentTDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribStudentTDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribUniformDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribUniformDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribUniformDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 if this DistribContinuousUnivariateDistribution_t
 * is of type DistribWeibullDistribution_t
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribContinuousUnivariateDistribution_t is of type
 * DistribWeibullDistribution_t, @c 0 otherwise
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_isDistribWeibullDistribution(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribContinuousUnivariateDistribution_t object have been set.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribContinuousUnivariateDistribution_t have been set, otherwise @c 0
 * (false) is returned.
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_hasRequiredAttributes(const
  DistribContinuousUnivariateDistribution_t * dcud);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribContinuousUnivariateDistribution_t object have been set.
 *
 * @param dcud the DistribContinuousUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribContinuousUnivariateDistribution_t have been set, otherwise @c 0
 * (false) is returned.
 *
 *
 * @note The required elements for the
 * DistribContinuousUnivariateDistribution_t object are:
 *
 * @memberof DistribContinuousUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribContinuousUnivariateDistribution_hasRequiredElements(const
  DistribContinuousUnivariateDistribution_t * dcud);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribContinuousUnivariateDistribution_H__ */


