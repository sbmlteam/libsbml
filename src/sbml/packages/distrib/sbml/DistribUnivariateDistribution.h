/**
 * @file DistribUnivariateDistribution.h
 * @brief Definition of the DistribUnivariateDistribution class.
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
 * @class DistribUnivariateDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribUnivariateDistribution
 * class.
 */


#ifndef DistribUnivariateDistribution_H__
#define DistribUnivariateDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribDistribution.h>
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

class LIBSBML_EXTERN DistribUnivariateDistribution : public DistribDistribution
{

public:

  /**
   * Creates a new DistribUnivariateDistribution using the given SBML Level,
   * Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribUnivariateDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribUnivariateDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribUnivariateDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUnivariateDistribution(
                                unsigned int level =
                                  DistribExtension::getDefaultLevel(),
                                unsigned int version =
                                  DistribExtension::getDefaultVersion(),
                                unsigned int pkgVersion =
                                  DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribUnivariateDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribUnivariateDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribUnivariateDistribution.
   *
   * @param orig the DistribUnivariateDistribution instance to copy.
   */
  DistribUnivariateDistribution(const DistribUnivariateDistribution& orig);


  /**
   * Assignment operator for DistribUnivariateDistribution.
   *
   * @param rhs the DistribUnivariateDistribution object whose values are to be
   * used as the basis of the assignment.
   */
  DistribUnivariateDistribution& operator=(const DistribUnivariateDistribution&
    rhs);


  /**
   * Creates and returns a deep copy of this DistribUnivariateDistribution
   * object.
   *
   * @return a (deep) copy of this DistribUnivariateDistribution object.
   */
  virtual DistribUnivariateDistribution* clone() const;


  /**
   * Destructor for DistribUnivariateDistribution.
   */
  virtual ~DistribUnivariateDistribution();


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribBetaDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribBetaDistribution, @c false otherwise
   */
  virtual bool isDistribBetaDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribCauchyDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribCauchyDistribution, @c false otherwise
   */
  virtual bool isDistribCauchyDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribChiSquareDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribChiSquareDistribution, @c false otherwise
   */
  virtual bool isDistribChiSquareDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribExponentialDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribExponentialDistribution, @c false otherwise
   */
  virtual bool isDistribExponentialDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribFDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribFDistribution, @c false otherwise
   */
  virtual bool isDistribFDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribGammaDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribGammaDistribution, @c false otherwise
   */
  virtual bool isDistribGammaDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribInverseGammaDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribInverseGammaDistribution, @c false otherwise
   */
  virtual bool isDistribInverseGammaDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribLaPlaceDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribLaPlaceDistribution, @c false otherwise
   */
  virtual bool isDistribLaPlaceDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribLogNormalDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribLogNormalDistribution, @c false otherwise
   */
  virtual bool isDistribLogNormalDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribLogisticDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribLogisticDistribution, @c false otherwise
   */
  virtual bool isDistribLogisticDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribNormalDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribNormalDistribution, @c false otherwise
   */
  virtual bool isDistribNormalDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribParetoDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribParetoDistribution, @c false otherwise
   */
  virtual bool isDistribParetoDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribRayleighDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribRayleighDistribution, @c false otherwise
   */
  virtual bool isDistribRayleighDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribStudentTDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribStudentTDistribution, @c false otherwise
   */
  virtual bool isDistribStudentTDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribUniformDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribUniformDistribution, @c false otherwise
   */
  virtual bool isDistribUniformDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribWeibullDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribWeibullDistribution, @c false otherwise
   */
  virtual bool isDistribWeibullDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribBinomialDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribBinomialDistribution, @c false otherwise
   */
  virtual bool isDistribBinomialDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribGeometricDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribGeometricDistribution, @c false otherwise
   */
  virtual bool isDistribGeometricDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type
   * DistribHypergeometricDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribHypergeometricDistribution, @c false otherwise
   */
  virtual bool isDistribHypergeometricDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type
   * DistribNegativeBinomialDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribNegativeBinomialDistribution, @c false otherwise
   */
  virtual bool isDistribNegativeBinomialDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribPoissonDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribPoissonDistribution, @c false otherwise
   */
  virtual bool isDistribPoissonDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribBernoulliDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribBernoulliDistribution, @c false otherwise
   */
  virtual bool isDistribBernoulliDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribUnivariateDistribution" is of type DistribCategoricalDistribution
   *
   * @return @c true if this abstract "DistribUnivariateDistribution" is of
   * type DistribCategoricalDistribution, @c false otherwise
   */
  virtual bool isDistribCategoricalDistribution() const;


  /**
   * Returns the XML element name of this DistribUnivariateDistribution object.
   *
   * For DistribUnivariateDistribution, the XML element name is always
   * @c "distribUnivariateDistribution".
   *
   * @return the name of this element, i.e. @c "distribUnivariateDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribUnivariateDistribution
   * object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_UNIVARIATEDISTRIBUTION, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribUnivariateDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribUnivariateDistribution have been set, otherwise @c false is
   * returned.
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
   * DistribUnivariateDistribution.
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
   * DistribUnivariateDistribution.
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
   * DistribUnivariateDistribution.
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
   * DistribUnivariateDistribution.
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
   * DistribUnivariateDistribution.
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
   * Predicate returning @c true if this DistribUnivariateDistribution's
   * attribute "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribUnivariateDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribUnivariateDistribution.
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
   * DistribUnivariateDistribution.
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
   * DistribUnivariateDistribution.
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
   * DistribUnivariateDistribution.
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
   * DistribUnivariateDistribution.
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
   * DistribUnivariateDistribution.
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
 * Creates a new DistribUnivariateDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribUnivariateDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribUnivariateDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribUnivariateDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
DistribUnivariateDistribution_t *
DistribUnivariateDistribution_create(unsigned int level,
                                     unsigned int version,
                                     unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribUnivariateDistribution_t
 * object.
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return a (deep) copy of this DistribUnivariateDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
DistribUnivariateDistribution_t*
DistribUnivariateDistribution_clone(const DistribUnivariateDistribution_t*
  dud);


/**
 * Frees this DistribUnivariateDistribution_t object.
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
void
DistribUnivariateDistribution_free(DistribUnivariateDistribution_t* dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribBetaDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribBetaDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribBetaDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribCauchyDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribCauchyDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribCauchyDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribChiSquareDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribChiSquareDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribChiSquareDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribExponentialDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribExponentialDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribExponentialDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribFDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribFDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribFDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribGammaDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribGammaDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribGammaDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribInverseGammaDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribInverseGammaDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribInverseGammaDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribLaPlaceDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribLaPlaceDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribLaPlaceDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribLogNormalDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribLogNormalDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribLogNormalDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribLogisticDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribLogisticDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribLogisticDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribNormalDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribNormalDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribNormalDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribParetoDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribParetoDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribParetoDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribRayleighDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribRayleighDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribRayleighDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribStudentTDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribStudentTDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribStudentTDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribUniformDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribUniformDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribUniformDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribWeibullDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribWeibullDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribWeibullDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribBinomialDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribBinomialDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribBinomialDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribGeometricDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribGeometricDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribGeometricDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribHypergeometricDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribHypergeometricDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribHypergeometricDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribNegativeBinomialDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribNegativeBinomialDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribNegativeBinomialDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribPoissonDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribPoissonDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribPoissonDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribBernoulliDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribBernoulliDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribBernoulliDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribCategoricalDistribution_t
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribUnivariateDistribution_t is of type
 * DistribCategoricalDistribution_t, @c 0 otherwise
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_isDistribCategoricalDistribution(const
  DistribUnivariateDistribution_t * dud);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribUnivariateDistribution_t object have been set.
 *
 * @param dud the DistribUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribUnivariateDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribUnivariateDistribution_hasRequiredAttributes(const
  DistribUnivariateDistribution_t * dud);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribUnivariateDistribution_H__ */


