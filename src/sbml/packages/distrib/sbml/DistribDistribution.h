/**
 * @file DistribDistribution.h
 * @brief Definition of the DistribDistribution class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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
 * @class DistribDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribDistribution class.
 */


#ifndef DistribDistribution_H__
#define DistribDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribBase.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


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
class DistribBinomialDistribution;
class DistribGeometricDistribution;
class DistribHypergeometricDistribution;
class DistribNegativeBinomialDistribution;
class DistribPoissonDistribution;
class DistribBernoulliDistribution;
class DistribCategoricalDistribution;
class DistribMultivariateDistribution;
class DistribExternalDistribution;

class LIBSBML_EXTERN DistribDistribution : public DistribBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new DistribDistribution using the given SBML Level, Version and
   * &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribDistribution(unsigned int level = DistribExtension::getDefaultLevel(),
                      unsigned int version =
                        DistribExtension::getDefaultVersion(),
                      unsigned int pkgVersion =
                        DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribDistribution using the given DistribPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribDistribution.
   *
   * @param orig the DistribDistribution instance to copy.
   */
  DistribDistribution(const DistribDistribution& orig);


  /**
   * Assignment operator for DistribDistribution.
   *
   * @param rhs the DistribDistribution object whose values are to be used as
   * the basis of the assignment.
   */
  DistribDistribution& operator=(const DistribDistribution& rhs);


  /**
   * Creates and returns a deep copy of this DistribDistribution object.
   *
   * @return a (deep) copy of this DistribDistribution object.
   */
  virtual DistribDistribution* clone() const;


  /**
   * Destructor for DistribDistribution.
   */
  virtual ~DistribDistribution();


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribBetaDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribBetaDistribution, @c false otherwise
   */
  virtual bool isDistribBetaDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribCauchyDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribCauchyDistribution, @c false otherwise
   */
  virtual bool isDistribCauchyDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribChiSquareDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribChiSquareDistribution, @c false otherwise
   */
  virtual bool isDistribChiSquareDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribExponentialDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribExponentialDistribution, @c false otherwise
   */
  virtual bool isDistribExponentialDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribFDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribFDistribution, @c false otherwise
   */
  virtual bool isDistribFDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribGammaDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribGammaDistribution, @c false otherwise
   */
  virtual bool isDistribGammaDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribInverseGammaDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribInverseGammaDistribution, @c false otherwise
   */
  virtual bool isDistribInverseGammaDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribLaPlaceDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribLaPlaceDistribution, @c false otherwise
   */
  virtual bool isDistribLaPlaceDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribLogNormalDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribLogNormalDistribution, @c false otherwise
   */
  virtual bool isDistribLogNormalDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribLogisticDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribLogisticDistribution, @c false otherwise
   */
  virtual bool isDistribLogisticDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribNormalDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribNormalDistribution, @c false otherwise
   */
  virtual bool isDistribNormalDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribParetoDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribParetoDistribution, @c false otherwise
   */
  virtual bool isDistribParetoDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribRayleighDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribRayleighDistribution, @c false otherwise
   */
  virtual bool isDistribRayleighDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribStudentTDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribStudentTDistribution, @c false otherwise
   */
  virtual bool isDistribStudentTDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribUniformDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribUniformDistribution, @c false otherwise
   */
  virtual bool isDistribUniformDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribWeibullDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribWeibullDistribution, @c false otherwise
   */
  virtual bool isDistribWeibullDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribBinomialDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribBinomialDistribution, @c false otherwise
   */
  virtual bool isDistribBinomialDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribGeometricDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribGeometricDistribution, @c false otherwise
   */
  virtual bool isDistribGeometricDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribHypergeometricDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribHypergeometricDistribution, @c false otherwise
   */
  virtual bool isDistribHypergeometricDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribNegativeBinomialDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribNegativeBinomialDistribution, @c false otherwise
   */
  virtual bool isDistribNegativeBinomialDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribPoissonDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribPoissonDistribution, @c false otherwise
   */
  virtual bool isDistribPoissonDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribBernoulliDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribBernoulliDistribution, @c false otherwise
   */
  virtual bool isDistribBernoulliDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribCategoricalDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribCategoricalDistribution, @c false otherwise
   */
  virtual bool isDistribCategoricalDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribMultivariateDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribMultivariateDistribution, @c false otherwise
   */
  virtual bool isDistribMultivariateDistribution() const;


  /**
   * Predicate returning @c true if this abstract "DistribDistribution" is of
   * type DistribExternalDistribution
   *
   * @return @c true if this abstract "DistribDistribution" is of type
   * DistribExternalDistribution, @c false otherwise
   */
  virtual bool isDistribExternalDistribution() const;


  /**
   * Returns the XML element name of this DistribDistribution object.
   *
   * For DistribDistribution, the XML element name is always @c "distribution".
   *
   * @return the name of this element, i.e. @c "distribution".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this DistribDistribution object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this DistribDistribution object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_DISTRIBUTION, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribDistribution have been set, otherwise @c false is returned.
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
   * Gets the value of the "attributeName" attribute of this
   * DistribDistribution.
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
   * DistribDistribution.
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
   * DistribDistribution.
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
   * DistribDistribution.
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
   * DistribDistribution.
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
   * Predicate returning @c true if this DistribDistribution's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribDistribution's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribDistribution.
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
   * DistribDistribution.
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
   * DistribDistribution.
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
   * DistribDistribution.
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
   * DistribDistribution.
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
   * DistribDistribution.
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
 * Creates a new DistribDistribution_t using the given SBML Level, Version and
 * &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
DistribDistribution_t *
DistribDistribution_create(unsigned int level,
                           unsigned int version,
                           unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribDistribution_t object.
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return a (deep) copy of this DistribDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
DistribDistribution_t*
DistribDistribution_clone(const DistribDistribution_t* dd);


/**
 * Frees this DistribDistribution_t object.
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
void
DistribDistribution_free(DistribDistribution_t* dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribBetaDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribBetaDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribBetaDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribCauchyDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribCauchyDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribCauchyDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribChiSquareDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribChiSquareDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribChiSquareDistribution(const DistribDistribution_t
  * dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribExponentialDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribExponentialDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribExponentialDistribution(const
  DistribDistribution_t * dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribFDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribFDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribFDistribution(const DistribDistribution_t * dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribGammaDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribGammaDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribGammaDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribInverseGammaDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribInverseGammaDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribInverseGammaDistribution(const
  DistribDistribution_t * dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribLaPlaceDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribLaPlaceDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribLaPlaceDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribLogNormalDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribLogNormalDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribLogNormalDistribution(const DistribDistribution_t
  * dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribLogisticDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribLogisticDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribLogisticDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribNormalDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribNormalDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribNormalDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribParetoDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribParetoDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribParetoDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribRayleighDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribRayleighDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribRayleighDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribStudentTDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribStudentTDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribStudentTDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribUniformDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribUniformDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribUniformDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribWeibullDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribWeibullDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribWeibullDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribBinomialDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribBinomialDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribBinomialDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribGeometricDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribGeometricDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribGeometricDistribution(const DistribDistribution_t
  * dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribHypergeometricDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribHypergeometricDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribHypergeometricDistribution(const
  DistribDistribution_t * dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribNegativeBinomialDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribNegativeBinomialDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribNegativeBinomialDistribution(const
  DistribDistribution_t * dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribPoissonDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribPoissonDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribPoissonDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribBernoulliDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribBernoulliDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribBernoulliDistribution(const DistribDistribution_t
  * dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribCategoricalDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribCategoricalDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribCategoricalDistribution(const
  DistribDistribution_t * dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribMultivariateDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribMultivariateDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribMultivariateDistribution(const
  DistribDistribution_t * dd);


/**
 * Predicate returning @c 1 if this DistribDistribution_t is of type
 * DistribExternalDistribution_t
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 if this DistribDistribution_t is of type
 * DistribExternalDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_isDistribExternalDistribution(const DistribDistribution_t *
  dd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribDistribution_t object have been set.
 *
 * @param dd the DistribDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribDistribution_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribDistribution_t
 */
LIBSBML_EXTERN
int
DistribDistribution_hasRequiredAttributes(const DistribDistribution_t * dd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribDistribution_H__ */


