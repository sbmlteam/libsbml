/**
 * @file DistribBase.h
 * @brief Definition of the DistribBase class.
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
 * @class DistribBase
 * @sbmlbrief{distrib} TODO:Definition of the DistribBase class.
 */


#ifndef DistribBase_H__
#define DistribBase_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class DistribDrawFromDistribution;
class DistribInput;
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
class DistribBinomialDistribution;
class DistribGeometricDistribution;
class DistribHypergeometricDistribution;
class DistribNegativeBinomialDistribution;
class DistribPoissonDistribution;
class DistribBernoulliDistribution;
class DistribCategoricalDistribution;
class DistribMultivariateDistribution;
class DistribExternalDistribution;
class DistribUncertBound;
class DistribExternalParameter;
class DistribCategory;
class DistribUncertainty;
class DistribUncertStatistics;
class DistribUncertStatisticSpan;

class LIBSBML_EXTERN DistribBase : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */


  /** @endcond */

public:

  /**
   * Creates a new DistribBase using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribBase.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribBase.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribBase.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribBase(unsigned int level = DistribExtension::getDefaultLevel(),
              unsigned int version = DistribExtension::getDefaultVersion(),
              unsigned int pkgVersion =
                DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribBase using the given DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribBase(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribBase.
   *
   * @param orig the DistribBase instance to copy.
   */
  DistribBase(const DistribBase& orig);


  /**
   * Assignment operator for DistribBase.
   *
   * @param rhs the DistribBase object whose values are to be used as the basis
   * of the assignment.
   */
  DistribBase& operator=(const DistribBase& rhs);


  /**
   * Creates and returns a deep copy of this DistribBase object.
   *
   * @return a (deep) copy of this DistribBase object.
   */
  virtual DistribBase* clone() const;


  /**
   * Destructor for DistribBase.
   */
  virtual ~DistribBase();


  /**
   * Returns the value of the "id" attribute of this DistribBase.
   *
   * @return the value of the "id" attribute of this DistribBase as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this DistribBase.
   *
   * @return the value of the "name" attribute of this DistribBase as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this DistribBase's "id" attribute is set.
   *
   * @return @c true if this DistribBase's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribBase's "name" attribute is set.
   *
   * @return @c true if this DistribBase's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this DistribBase.
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
   * Sets the value of the "name" attribute of this DistribBase.
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
   * Unsets the value of the "id" attribute of this DistribBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this DistribBase.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribDrawFromDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribDrawFromDistribution, @c false otherwise
   */
  virtual bool isDistribDrawFromDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribInput
   *
   * @return @c true if this abstract "DistribBase" is of type DistribInput,
   * @c false otherwise
   */
  virtual bool isDistribInput() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribBetaDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribBetaDistribution, @c false otherwise
   */
  virtual bool isDistribBetaDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribCauchyDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribCauchyDistribution, @c false otherwise
   */
  virtual bool isDistribCauchyDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribChiSquareDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribChiSquareDistribution, @c false otherwise
   */
  virtual bool isDistribChiSquareDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribExponentialDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribExponentialDistribution, @c false otherwise
   */
  virtual bool isDistribExponentialDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribFDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribFDistribution, @c false otherwise
   */
  virtual bool isDistribFDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribGammaDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribGammaDistribution, @c false otherwise
   */
  virtual bool isDistribGammaDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribInverseGammaDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribInverseGammaDistribution, @c false otherwise
   */
  virtual bool isDistribInverseGammaDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribLaPlaceDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribLaPlaceDistribution, @c false otherwise
   */
  virtual bool isDistribLaPlaceDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribLogNormalDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribLogNormalDistribution, @c false otherwise
   */
  virtual bool isDistribLogNormalDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribLogisticDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribLogisticDistribution, @c false otherwise
   */
  virtual bool isDistribLogisticDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribNormalDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribNormalDistribution, @c false otherwise
   */
  virtual bool isDistribNormalDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribParetoDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribParetoDistribution, @c false otherwise
   */
  virtual bool isDistribParetoDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribRayleighDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribRayleighDistribution, @c false otherwise
   */
  virtual bool isDistribRayleighDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribStudentTDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribStudentTDistribution, @c false otherwise
   */
  virtual bool isDistribStudentTDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribUniformDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribUniformDistribution, @c false otherwise
   */
  virtual bool isDistribUniformDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribWeibullDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribWeibullDistribution, @c false otherwise
   */
  virtual bool isDistribWeibullDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribBinomialDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribBinomialDistribution, @c false otherwise
   */
  virtual bool isDistribBinomialDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribGeometricDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribGeometricDistribution, @c false otherwise
   */
  virtual bool isDistribGeometricDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribHypergeometricDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribHypergeometricDistribution, @c false otherwise
   */
  virtual bool isDistribHypergeometricDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribNegativeBinomialDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribNegativeBinomialDistribution, @c false otherwise
   */
  virtual bool isDistribNegativeBinomialDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribPoissonDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribPoissonDistribution, @c false otherwise
   */
  virtual bool isDistribPoissonDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribBernoulliDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribBernoulliDistribution, @c false otherwise
   */
  virtual bool isDistribBernoulliDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribCategoricalDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribCategoricalDistribution, @c false otherwise
   */
  virtual bool isDistribCategoricalDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribMultivariateDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribMultivariateDistribution, @c false otherwise
   */
  virtual bool isDistribMultivariateDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribExternalDistribution
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribExternalDistribution, @c false otherwise
   */
  virtual bool isDistribExternalDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribUncertBound
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribUncertBound, @c false otherwise
   */
  virtual bool isDistribUncertBound() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribExternalParameter
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribExternalParameter, @c false otherwise
   */
  virtual bool isDistribExternalParameter() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribCategory
   *
   * @return @c true if this abstract "DistribBase" is of type DistribCategory,
   * @c false otherwise
   */
  virtual bool isDistribCategory() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribUncertainty
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribUncertainty, @c false otherwise
   */
  virtual bool isDistribUncertainty() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribUncertStatistics
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribUncertStatistics, @c false otherwise
   */
  virtual bool isDistribUncertStatistics() const;


  /**
   * Predicate returning @c true if this abstract "DistribBase" is of type
   * DistribUncertStatisticSpan
   *
   * @return @c true if this abstract "DistribBase" is of type
   * DistribUncertStatisticSpan, @c false otherwise
   */
  virtual bool isDistribUncertStatisticSpan() const;


  /**
   * Returns the XML element name of this DistribBase object.
   *
   * For DistribBase, the XML element name is always @c "distribBase".
   *
   * @return the name of this element, i.e. @c "distribBase".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribBase object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_DISTRIBBASE, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribBase object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribBase have been set, otherwise @c false is returned.
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
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this DistribBase.
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
   * Gets the value of the "attributeName" attribute of this DistribBase.
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
   * Gets the value of the "attributeName" attribute of this DistribBase.
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
   * Gets the value of the "attributeName" attribute of this DistribBase.
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
   * Gets the value of the "attributeName" attribute of this DistribBase.
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
   * Predicate returning @c true if this DistribBase's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribBase's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DistribBase.
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
   * Sets the value of the "attributeName" attribute of this DistribBase.
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
   * Sets the value of the "attributeName" attribute of this DistribBase.
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
   * Sets the value of the "attributeName" attribute of this DistribBase.
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
   * Sets the value of the "attributeName" attribute of this DistribBase.
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
   * Unsets the value of the "attributeName" attribute of this DistribBase.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */




  #endif /* !SWIG */


protected:


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
  void readL3V1V1Attributes(const XMLAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  void readL3V2V1Attributes(const XMLAttributes& attributes, int origNumErrs);

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
  void writeL3V1V1Attributes(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  void writeL3V2V1Attributes(XMLOutputStream& stream) const;

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new DistribDrawFromDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribDrawFromDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion);


/**
 * Creates a new DistribInput (DistribBase_t) using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribInput(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion);


/**
 * Creates a new DistribBetaDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribBetaDistribution(unsigned int level,
                                          unsigned int version,
                                          unsigned int pkgVersion);


/**
 * Creates a new DistribCauchyDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribCauchyDistribution(unsigned int level,
                                            unsigned int version,
                                            unsigned int pkgVersion);


/**
 * Creates a new DistribChiSquareDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribChiSquareDistribution(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion);


/**
 * Creates a new DistribExponentialDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribExponentialDistribution(unsigned int level,
                                                 unsigned int version,
                                                 unsigned int pkgVersion);


/**
 * Creates a new DistribFDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribFDistribution(unsigned int level,
                                       unsigned int version,
                                       unsigned int pkgVersion);


/**
 * Creates a new DistribGammaDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribGammaDistribution(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion);


/**
 * Creates a new DistribInverseGammaDistribution (DistribBase_t) using the
 * given SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribInverseGammaDistribution(unsigned int level,
                                                  unsigned int version,
                                                  unsigned int pkgVersion);


/**
 * Creates a new DistribLaPlaceDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribLaPlaceDistribution(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion);


/**
 * Creates a new DistribLogNormalDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribLogNormalDistribution(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion);


/**
 * Creates a new DistribLogisticDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribLogisticDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion);


/**
 * Creates a new DistribNormalDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribNormalDistribution(unsigned int level,
                                            unsigned int version,
                                            unsigned int pkgVersion);


/**
 * Creates a new DistribParetoDistribution (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribParetoDistribution(unsigned int level,
                                            unsigned int version,
                                            unsigned int pkgVersion);


/**
 * Creates a new DistribRayleighDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribRayleighDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion);


/**
 * Creates a new DistribStudentTDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribStudentTDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion);


/**
 * Creates a new DistribUniformDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribUniformDistribution(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion);


/**
 * Creates a new DistribWeibullDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribWeibullDistribution(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion);


/**
 * Creates a new DistribBinomialDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribBinomialDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion);


/**
 * Creates a new DistribGeometricDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribGeometricDistribution(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion);


/**
 * Creates a new DistribHypergeometricDistribution (DistribBase_t) using the
 * given SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribHypergeometricDistribution(unsigned int level,
                                                    unsigned int version,
                                                    unsigned int pkgVersion);


/**
 * Creates a new DistribNegativeBinomialDistribution (DistribBase_t) using the
 * given SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribNegativeBinomialDistribution(unsigned int level,
                                                      unsigned int version,
                                                      unsigned int pkgVersion);


/**
 * Creates a new DistribPoissonDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribPoissonDistribution(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion);


/**
 * Creates a new DistribBernoulliDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribBernoulliDistribution(unsigned int level,
                                               unsigned int version,
                                               unsigned int pkgVersion);


/**
 * Creates a new DistribCategoricalDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribCategoricalDistribution(unsigned int level,
                                                 unsigned int version,
                                                 unsigned int pkgVersion);


/**
 * Creates a new DistribMultivariateDistribution (DistribBase_t) using the
 * given SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribMultivariateDistribution(unsigned int level,
                                                  unsigned int version,
                                                  unsigned int pkgVersion);


/**
 * Creates a new DistribExternalDistribution (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribExternalDistribution(unsigned int level,
                                              unsigned int version,
                                              unsigned int pkgVersion);


/**
 * Creates a new DistribUncertBound (DistribBase_t) using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribUncertBound(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion);


/**
 * Creates a new DistribExternalParameter (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribExternalParameter(unsigned int level,
                                           unsigned int version,
                                           unsigned int pkgVersion);


/**
 * Creates a new DistribCategory (DistribBase_t) using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribCategory(unsigned int level,
                                  unsigned int version,
                                  unsigned int pkgVersion);


/**
 * Creates a new DistribUncertainty (DistribBase_t) using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribUncertainty(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion);


/**
 * Creates a new DistribUncertStatistics (DistribBase_t) using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribUncertStatistics(unsigned int level,
                                          unsigned int version,
                                          unsigned int pkgVersion);


/**
 * Creates a new DistribUncertStatisticSpan (DistribBase_t) using the given
 * SBML Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribBase_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribBase_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribBase_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t *
DistribBase_createDistribUncertStatisticSpan(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribBase_t object.
 *
 * @param db the DistribBase_t structure.
 *
 * @return a (deep) copy of this DistribBase_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
DistribBase_t*
DistribBase_clone(const DistribBase_t* db);


/**
 * Frees this DistribBase_t object.
 *
 * @param db the DistribBase_t structure.
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
void
DistribBase_free(DistribBase_t* db);


/**
 * Returns the value of the "id" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this DistribBase_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
char *
DistribBase_getId(const DistribBase_t * db);


/**
 * Returns the value of the "name" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this DistribBase_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
char *
DistribBase_getName(const DistribBase_t * db);


/**
 * Predicate returning @c 1 (true) if this DistribBase_t's "id" attribute is
 * set.
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 (true) if this DistribBase_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isSetId(const DistribBase_t * db);


/**
 * Predicate returning @c 1 (true) if this DistribBase_t's "name" attribute is
 * set.
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 (true) if this DistribBase_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isSetName(const DistribBase_t * db);


/**
 * Sets the value of the "id" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribBase_unsetId().
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_setId(DistribBase_t * db, const char * id);


/**
 * Sets the value of the "name" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribBase_unsetName().
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_setName(DistribBase_t * db, const char * name);


/**
 * Unsets the value of the "id" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_unsetId(DistribBase_t * db);


/**
 * Unsets the value of the "name" attribute of this DistribBase_t.
 *
 * @param db the DistribBase_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_unsetName(DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribDrawFromDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribDrawFromDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribDrawFromDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type DistribInput_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribInput_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribInput(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribBetaDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribBetaDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribBetaDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribCauchyDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribCauchyDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribCauchyDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribChiSquareDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type
 * DistribChiSquareDistribution_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribChiSquareDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribExponentialDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type
 * DistribExponentialDistribution_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribExponentialDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribFDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribFDistribution_t, @c 0
 * otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribFDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribGammaDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribGammaDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribGammaDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribInverseGammaDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type
 * DistribInverseGammaDistribution_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribInverseGammaDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribLaPlaceDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribLaPlaceDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribLaPlaceDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribLogNormalDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type
 * DistribLogNormalDistribution_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribLogNormalDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribLogisticDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribLogisticDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribLogisticDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribNormalDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribNormalDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribNormalDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribParetoDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribParetoDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribParetoDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribRayleighDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribRayleighDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribRayleighDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribStudentTDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribStudentTDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribStudentTDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribUniformDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribUniformDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribUniformDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribWeibullDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribWeibullDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribWeibullDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribBinomialDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribBinomialDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribBinomialDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribGeometricDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type
 * DistribGeometricDistribution_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribGeometricDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribHypergeometricDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type
 * DistribHypergeometricDistribution_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribHypergeometricDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribNegativeBinomialDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type
 * DistribNegativeBinomialDistribution_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribNegativeBinomialDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribPoissonDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribPoissonDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribPoissonDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribBernoulliDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type
 * DistribBernoulliDistribution_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribBernoulliDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribCategoricalDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type
 * DistribCategoricalDistribution_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribCategoricalDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribMultivariateDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type
 * DistribMultivariateDistribution_t, @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribMultivariateDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribExternalDistribution_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribExternalDistribution_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribExternalDistribution(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribUncertBound_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribUncertBound_t, @c 0
 * otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribUncertBound(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribExternalParameter_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribExternalParameter_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribExternalParameter(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type DistribCategory_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribCategory_t, @c 0
 * otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribCategory(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribUncertainty_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribUncertainty_t, @c 0
 * otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribUncertainty(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribUncertStatistics_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribUncertStatistics_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribUncertStatistics(const DistribBase_t * db);


/**
 * Predicate returning @c 1 if this DistribBase_t is of type
 * DistribUncertStatisticSpan_t
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 if this DistribBase_t is of type DistribUncertStatisticSpan_t,
 * @c 0 otherwise
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_isDistribUncertStatisticSpan(const DistribBase_t * db);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribBase_t object have been set.
 *
 * @param db the DistribBase_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribBase_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribBase_t
 */
LIBSBML_EXTERN
int
DistribBase_hasRequiredAttributes(const DistribBase_t * db);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribBase_H__ */


