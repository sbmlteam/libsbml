/**
 * @file DistribDiscreteUnivariateDistribution.h
 * @brief Definition of the DistribDiscreteUnivariateDistribution class.
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
 * @class DistribDiscreteUnivariateDistribution
 * @sbmlbrief{distrib} TODO:Definition of the
 * DistribDiscreteUnivariateDistribution class.
 */


#ifndef DistribDiscreteUnivariateDistribution_H__
#define DistribDiscreteUnivariateDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribUnivariateDistribution.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertBound.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class DistribBinomialDistribution;
class DistribGeometricDistribution;
class DistribHypergeometricDistribution;
class DistribNegativeBinomialDistribution;
class DistribPoissonDistribution;

class LIBSBML_EXTERN DistribDiscreteUnivariateDistribution : public
  DistribUnivariateDistribution
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribUncertBound* mTruncationLowerBound;
  DistribUncertBound* mTruncationUpperBound;

  /** @endcond */

public:

  /**
   * Creates a new DistribDiscreteUnivariateDistribution using the given SBML
   * Level, Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribDiscreteUnivariateDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribDiscreteUnivariateDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribDiscreteUnivariateDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribDiscreteUnivariateDistribution(
                                        unsigned int level =
                                          DistribExtension::getDefaultLevel(),
                                        unsigned int version =
                                          DistribExtension::getDefaultVersion(),
                                        unsigned int pkgVersion =
                                          DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribDiscreteUnivariateDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribDiscreteUnivariateDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribDiscreteUnivariateDistribution.
   *
   * @param orig the DistribDiscreteUnivariateDistribution instance to copy.
   */
  DistribDiscreteUnivariateDistribution(const
    DistribDiscreteUnivariateDistribution& orig);


  /**
   * Assignment operator for DistribDiscreteUnivariateDistribution.
   *
   * @param rhs the DistribDiscreteUnivariateDistribution object whose values
   * are to be used as the basis of the assignment.
   */
  DistribDiscreteUnivariateDistribution& operator=(const
    DistribDiscreteUnivariateDistribution& rhs);


  /**
   * Creates and returns a deep copy of this
   * DistribDiscreteUnivariateDistribution object.
   *
   * @return a (deep) copy of this DistribDiscreteUnivariateDistribution
   * object.
   */
  virtual DistribDiscreteUnivariateDistribution* clone() const;


  /**
   * Destructor for DistribDiscreteUnivariateDistribution.
   */
  virtual ~DistribDiscreteUnivariateDistribution();


  /**
   * Returns the value of the "id" attribute of this
   * DistribDiscreteUnivariateDistribution.
   *
   * @return the value of the "id" attribute of this
   * DistribDiscreteUnivariateDistribution as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this
   * DistribDiscreteUnivariateDistribution.
   *
   * @return the value of the "name" attribute of this
   * DistribDiscreteUnivariateDistribution as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this
   * DistribDiscreteUnivariateDistribution's "id" attribute is set.
   *
   * @return @c true if this DistribDiscreteUnivariateDistribution's "id"
   * attribute has been set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this
   * DistribDiscreteUnivariateDistribution's "name" attribute is set.
   *
   * @return @c true if this DistribDiscreteUnivariateDistribution's "name"
   * attribute has been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this
   * DistribDiscreteUnivariateDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "truncationLowerBound" element of this
   * DistribDiscreteUnivariateDistribution.
   *
   * @return the value of the "truncationLowerBound" element of this
   * DistribDiscreteUnivariateDistribution as a DistribUncertBound*.
   */
  const DistribUncertBound* getTruncationLowerBound() const;


  /**
   * Returns the value of the "truncationLowerBound" element of this
   * DistribDiscreteUnivariateDistribution.
   *
   * @return the value of the "truncationLowerBound" element of this
   * DistribDiscreteUnivariateDistribution as a DistribUncertBound*.
   */
  DistribUncertBound* getTruncationLowerBound();


  /**
   * Returns the value of the "truncationUpperBound" element of this
   * DistribDiscreteUnivariateDistribution.
   *
   * @return the value of the "truncationUpperBound" element of this
   * DistribDiscreteUnivariateDistribution as a DistribUncertBound*.
   */
  const DistribUncertBound* getTruncationUpperBound() const;


  /**
   * Returns the value of the "truncationUpperBound" element of this
   * DistribDiscreteUnivariateDistribution.
   *
   * @return the value of the "truncationUpperBound" element of this
   * DistribDiscreteUnivariateDistribution as a DistribUncertBound*.
   */
  DistribUncertBound* getTruncationUpperBound();


  /**
   * Predicate returning @c true if this
   * DistribDiscreteUnivariateDistribution's "truncationLowerBound" element is
   * set.
   *
   * @return @c true if this DistribDiscreteUnivariateDistribution's
   * "truncationLowerBound" element has been set, otherwise @c false is
   * returned.
   */
  bool isSetTruncationLowerBound() const;


  /**
   * Predicate returning @c true if this
   * DistribDiscreteUnivariateDistribution's "truncationUpperBound" element is
   * set.
   *
   * @return @c true if this DistribDiscreteUnivariateDistribution's
   * "truncationUpperBound" element has been set, otherwise @c false is
   * returned.
   */
  bool isSetTruncationUpperBound() const;


  /**
   * Sets the value of the "truncationLowerBound" element of this
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution object and returns the
   * DistribUncertBound object created.
   *
   * @return a new DistribUncertBound object instance.
   */
  DistribUncertBound* createTruncationLowerBound();


  /**
   * Creates a new DistribUncertBound object, adds it to this
   * DistribDiscreteUnivariateDistribution object and returns the
   * DistribUncertBound object created.
   *
   * @return a new DistribUncertBound object instance.
   */
  DistribUncertBound* createTruncationUpperBound();


  /**
   * Unsets the value of the "truncationLowerBound" element of this
   * DistribDiscreteUnivariateDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetTruncationLowerBound();


  /**
   * Unsets the value of the "truncationUpperBound" element of this
   * DistribDiscreteUnivariateDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetTruncationUpperBound();


  /**
   * Predicate returning @c true if this abstract
   * "DistribDiscreteUnivariateDistribution" is of type
   * DistribBinomialDistribution
   *
   * @return @c true if this abstract "DistribDiscreteUnivariateDistribution"
   * is of type DistribBinomialDistribution, @c false otherwise
   */
  virtual bool isDistribBinomialDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribDiscreteUnivariateDistribution" is of type
   * DistribGeometricDistribution
   *
   * @return @c true if this abstract "DistribDiscreteUnivariateDistribution"
   * is of type DistribGeometricDistribution, @c false otherwise
   */
  virtual bool isDistribGeometricDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribDiscreteUnivariateDistribution" is of type
   * DistribHypergeometricDistribution
   *
   * @return @c true if this abstract "DistribDiscreteUnivariateDistribution"
   * is of type DistribHypergeometricDistribution, @c false otherwise
   */
  virtual bool isDistribHypergeometricDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribDiscreteUnivariateDistribution" is of type
   * DistribNegativeBinomialDistribution
   *
   * @return @c true if this abstract "DistribDiscreteUnivariateDistribution"
   * is of type DistribNegativeBinomialDistribution, @c false otherwise
   */
  virtual bool isDistribNegativeBinomialDistribution() const;


  /**
   * Predicate returning @c true if this abstract
   * "DistribDiscreteUnivariateDistribution" is of type
   * DistribPoissonDistribution
   *
   * @return @c true if this abstract "DistribDiscreteUnivariateDistribution"
   * is of type DistribPoissonDistribution, @c false otherwise
   */
  virtual bool isDistribPoissonDistribution() const;


  /**
   * Returns the XML element name of this DistribDiscreteUnivariateDistribution
   * object.
   *
   * For DistribDiscreteUnivariateDistribution, the XML element name is always
   * @c "discreteUnivariateDistribution".
   *
   * @return the name of this element, i.e.
   * @c "discreteUnivariateDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this
   * DistribDiscreteUnivariateDistribution object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_DISCRETEUNIVARIATEDISTRIBUTION,
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
   * DistribDiscreteUnivariateDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribDiscreteUnivariateDistribution have been set, otherwise @c false is
   * returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribDiscreteUnivariateDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribDiscreteUnivariateDistribution have been set, otherwise @c false is
   * returned.
   *
   *
   * @note The required elements for the DistribDiscreteUnivariateDistribution
   * object are:
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution's attribute "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribDiscreteUnivariateDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
   * DistribDiscreteUnivariateDistribution.
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
 * Creates a new DistribDiscreteUnivariateDistribution_t using the given SBML
 * Level, Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribDiscreteUnivariateDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
DistribDiscreteUnivariateDistribution_t *
DistribDiscreteUnivariateDistribution_create(unsigned int level,
                                             unsigned int version,
                                             unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this
 * DistribDiscreteUnivariateDistribution_t object.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return a (deep) copy of this DistribDiscreteUnivariateDistribution_t
 * object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
DistribDiscreteUnivariateDistribution_t*
DistribDiscreteUnivariateDistribution_clone(const
  DistribDiscreteUnivariateDistribution_t* ddud);


/**
 * Frees this DistribDiscreteUnivariateDistribution_t object.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
void
DistribDiscreteUnivariateDistribution_free(DistribDiscreteUnivariateDistribution_t*
  ddud);


/**
 * Returns the value of the "id" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure whose id
 * is sought.
 *
 * @return the value of the "id" attribute of this
 * DistribDiscreteUnivariateDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
char *
DistribDiscreteUnivariateDistribution_getId(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Returns the value of the "name" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure whose name
 * is sought.
 *
 * @return the value of the "name" attribute of this
 * DistribDiscreteUnivariateDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
char *
DistribDiscreteUnivariateDistribution_getName(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Predicate returning @c 1 (true) if this
 * DistribDiscreteUnivariateDistribution_t's "id" attribute is set.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribDiscreteUnivariateDistribution_t's "id"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isSetId(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Predicate returning @c 1 (true) if this
 * DistribDiscreteUnivariateDistribution_t's "name" attribute is set.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribDiscreteUnivariateDistribution_t's "name"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isSetName(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Sets the value of the "id" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribDiscreteUnivariateDistribution_unsetId().
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_setId(
                                            DistribDiscreteUnivariateDistribution_t
                                              * ddud,
                                            const char * id);


/**
 * Sets the value of the "name" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribDiscreteUnivariateDistribution_unsetName().
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_setName(
                                              DistribDiscreteUnivariateDistribution_t
                                                * ddud,
                                              const char * name);


/**
 * Unsets the value of the "id" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_unsetId(DistribDiscreteUnivariateDistribution_t
  * ddud);


/**
 * Unsets the value of the "name" attribute of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_unsetName(DistribDiscreteUnivariateDistribution_t
  * ddud);


/**
 * Returns the value of the "truncationLowerBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure whose
 * truncationLowerBound is sought.
 *
 * @return the value of the "truncationLowerBound" element of this
 * DistribDiscreteUnivariateDistribution_t as a DistribUncertBound*.
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertBound_t*
DistribDiscreteUnivariateDistribution_getTruncationLowerBound(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Returns the value of the "truncationUpperBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure whose
 * truncationUpperBound is sought.
 *
 * @return the value of the "truncationUpperBound" element of this
 * DistribDiscreteUnivariateDistribution_t as a DistribUncertBound*.
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertBound_t*
DistribDiscreteUnivariateDistribution_getTruncationUpperBound(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Predicate returning @c 1 (true) if this
 * DistribDiscreteUnivariateDistribution_t's "truncationLowerBound" element is
 * set.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribDiscreteUnivariateDistribution_t's
 * "truncationLowerBound" element has been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isSetTruncationLowerBound(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Predicate returning @c 1 (true) if this
 * DistribDiscreteUnivariateDistribution_t's "truncationUpperBound" element is
 * set.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribDiscreteUnivariateDistribution_t's
 * "truncationUpperBound" element has been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isSetTruncationUpperBound(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Sets the value of the "truncationLowerBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @param truncationLowerBound DistribUncertBound_t* value of the
 * "truncationLowerBound" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_setTruncationLowerBound(
                                                              DistribDiscreteUnivariateDistribution_t
                                                                * ddud,
                                                              const
                                                                DistribUncertBound_t*
                                                                  truncationLowerBound);


/**
 * Sets the value of the "truncationUpperBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @param truncationUpperBound DistribUncertBound_t* value of the
 * "truncationUpperBound" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_setTruncationUpperBound(
                                                              DistribDiscreteUnivariateDistribution_t
                                                                * ddud,
                                                              const
                                                                DistribUncertBound_t*
                                                                  truncationUpperBound);


/**
 * Creates a new DistribUncertBound_t object, adds it to this
 * DistribDiscreteUnivariateDistribution_t object and returns the
 * DistribUncertBound_t object created.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure to which
 * the DistribUncertBound_t should be added.
 *
 * @return a new DistribUncertBound_t object instance.
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
DistribUncertBound_t*
DistribDiscreteUnivariateDistribution_createTruncationLowerBound(DistribDiscreteUnivariateDistribution_t*
  ddud);


/**
 * Creates a new DistribUncertBound_t object, adds it to this
 * DistribDiscreteUnivariateDistribution_t object and returns the
 * DistribUncertBound_t object created.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure to which
 * the DistribUncertBound_t should be added.
 *
 * @return a new DistribUncertBound_t object instance.
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
DistribUncertBound_t*
DistribDiscreteUnivariateDistribution_createTruncationUpperBound(DistribDiscreteUnivariateDistribution_t*
  ddud);


/**
 * Unsets the value of the "truncationLowerBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_unsetTruncationLowerBound(DistribDiscreteUnivariateDistribution_t
  * ddud);


/**
 * Unsets the value of the "truncationUpperBound" element of this
 * DistribDiscreteUnivariateDistribution_t.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_unsetTruncationUpperBound(DistribDiscreteUnivariateDistribution_t
  * ddud);


/**
 * Predicate returning @c 1 if this DistribDiscreteUnivariateDistribution_t is
 * of type DistribBinomialDistribution_t
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribDiscreteUnivariateDistribution_t is of type
 * DistribBinomialDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isDistribBinomialDistribution(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Predicate returning @c 1 if this DistribDiscreteUnivariateDistribution_t is
 * of type DistribGeometricDistribution_t
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribDiscreteUnivariateDistribution_t is of type
 * DistribGeometricDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isDistribGeometricDistribution(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Predicate returning @c 1 if this DistribDiscreteUnivariateDistribution_t is
 * of type DistribHypergeometricDistribution_t
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribDiscreteUnivariateDistribution_t is of type
 * DistribHypergeometricDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isDistribHypergeometricDistribution(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Predicate returning @c 1 if this DistribDiscreteUnivariateDistribution_t is
 * of type DistribNegativeBinomialDistribution_t
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribDiscreteUnivariateDistribution_t is of type
 * DistribNegativeBinomialDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isDistribNegativeBinomialDistribution(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Predicate returning @c 1 if this DistribDiscreteUnivariateDistribution_t is
 * of type DistribPoissonDistribution_t
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return @c 1 if this DistribDiscreteUnivariateDistribution_t is of type
 * DistribPoissonDistribution_t, @c 0 otherwise
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_isDistribPoissonDistribution(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribDiscreteUnivariateDistribution_t object have been set.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribDiscreteUnivariateDistribution_t have been set, otherwise @c 0
 * (false) is returned.
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_hasRequiredAttributes(const
  DistribDiscreteUnivariateDistribution_t * ddud);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribDiscreteUnivariateDistribution_t object have been set.
 *
 * @param ddud the DistribDiscreteUnivariateDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribDiscreteUnivariateDistribution_t have been set, otherwise @c 0
 * (false) is returned.
 *
 *
 * @note The required elements for the DistribDiscreteUnivariateDistribution_t
 * object are:
 *
 * @memberof DistribDiscreteUnivariateDistribution_t
 */
LIBSBML_EXTERN
int
DistribDiscreteUnivariateDistribution_hasRequiredElements(const
  DistribDiscreteUnivariateDistribution_t * ddud);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribDiscreteUnivariateDistribution_H__ */


