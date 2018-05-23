/**
 * @file DistribExternalDistribution.h
 * @brief Definition of the DistribExternalDistribution class.
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
 * @class DistribExternalDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribExternalDistribution
 * class.
 */


#ifndef DistribExternalDistribution_H__
#define DistribExternalDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribDistribution.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/ListOfExternalParameters.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribExternalDistribution : public DistribDistribution
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mDefinitionURL;
  ListOfExternalParameters mDistribExternalParameters;

  /** @endcond */

public:

  /**
   * Creates a new DistribExternalDistribution using the given SBML Level,
   * Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribExternalDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribExternalDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribExternalDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribExternalDistribution(
                              unsigned int level =
                                DistribExtension::getDefaultLevel(),
                              unsigned int version =
                                DistribExtension::getDefaultVersion(),
                              unsigned int pkgVersion =
                                DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribExternalDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribExternalDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribExternalDistribution.
   *
   * @param orig the DistribExternalDistribution instance to copy.
   */
  DistribExternalDistribution(const DistribExternalDistribution& orig);


  /**
   * Assignment operator for DistribExternalDistribution.
   *
   * @param rhs the DistribExternalDistribution object whose values are to be
   * used as the basis of the assignment.
   */
  DistribExternalDistribution& operator=(const DistribExternalDistribution&
    rhs);


  /**
   * Creates and returns a deep copy of this DistribExternalDistribution
   * object.
   *
   * @return a (deep) copy of this DistribExternalDistribution object.
   */
  virtual DistribExternalDistribution* clone() const;


  /**
   * Destructor for DistribExternalDistribution.
   */
  virtual ~DistribExternalDistribution();


  /**
   * Returns the value of the "id" attribute of this
   * DistribExternalDistribution.
   *
   * @return the value of the "id" attribute of this
   * DistribExternalDistribution as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this
   * DistribExternalDistribution.
   *
   * @return the value of the "name" attribute of this
   * DistribExternalDistribution as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "definitionURL" attribute of this
   * DistribExternalDistribution.
   *
   * @return the value of the "definitionURL" attribute of this
   * DistribExternalDistribution as a string.
   */
  const std::string& getDefinitionURL() const;


  /**
   * Predicate returning @c true if this DistribExternalDistribution's "id"
   * attribute is set.
   *
   * @return @c true if this DistribExternalDistribution's "id" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribExternalDistribution's "name"
   * attribute is set.
   *
   * @return @c true if this DistribExternalDistribution's "name" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this DistribExternalDistribution's
   * "definitionURL" attribute is set.
   *
   * @return @c true if this DistribExternalDistribution's "definitionURL"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetDefinitionURL() const;


  /**
   * Sets the value of the "id" attribute of this DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
   * Sets the value of the "definitionURL" attribute of this
   * DistribExternalDistribution.
   *
   * @param definitionURL std::string& value of the "definitionURL" attribute
   * to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p definitionURL = @c NULL or an empty string
   * is equivalent to calling unsetDefinitionURL().
   */
  int setDefinitionURL(const std::string& definitionURL);


  /**
   * Unsets the value of the "id" attribute of this
   * DistribExternalDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this
   * DistribExternalDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "definitionURL" attribute of this
   * DistribExternalDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDefinitionURL();


  /**
   * Returns the ListOfExternalParameters from this
   * DistribExternalDistribution.
   *
   * @return the ListOfExternalParameters from this
   * DistribExternalDistribution.
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
   * Returns the ListOfExternalParameters from this
   * DistribExternalDistribution.
   *
   * @return the ListOfExternalParameters from this
   * DistribExternalDistribution.
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
   * Get a DistribExternalParameter from the DistribExternalDistribution.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to retrieve.
   *
   * @return the nth DistribExternalParameter in the ListOfExternalParameters
   * within this DistribExternalDistribution.
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
   * Get a DistribExternalParameter from the DistribExternalDistribution.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to retrieve.
   *
   * @return the nth DistribExternalParameter in the ListOfExternalParameters
   * within this DistribExternalDistribution.
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
   * Get a DistribExternalParameter from the DistribExternalDistribution based
   * on its identifier.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to retrieve.
   *
   * @return the DistribExternalParameter in the ListOfExternalParameters
   * within this DistribExternalDistribution with the given @p sid or @c NULL
   * if no such DistribExternalParameter exists.
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
   * Get a DistribExternalParameter from the DistribExternalDistribution based
   * on its identifier.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to retrieve.
   *
   * @return the DistribExternalParameter in the ListOfExternalParameters
   * within this DistribExternalDistribution with the given @p sid or @c NULL
   * if no such DistribExternalParameter exists.
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
   * DistribExternalDistribution.
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
   * DistribExternalDistribution.
   *
   * @return the number of DistribExternalParameter objects in this
   * DistribExternalDistribution.
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
   * DistribExternalDistribution object and returns the
   * DistribExternalParameter object created.
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
   * Removes the nth DistribExternalParameter from this
   * DistribExternalDistribution and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to remove.
   *
   * @return a pointer to the nth DistribExternalParameter in this
   * DistribExternalDistribution.
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
   * Removes the DistribExternalParameter from this DistribExternalDistribution
   * based on its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to remove.
   *
   * @return the DistribExternalParameter in this DistribExternalDistribution
   * based on the identifier or NULL if no such DistribExternalParameter
   * exists.
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
   * Returns the XML element name of this DistribExternalDistribution object.
   *
   * For DistribExternalDistribution, the XML element name is always
   * @c "externalDistribution".
   *
   * @return the name of this element, i.e. @c "externalDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribExternalDistribution object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_EXTERNALDISTRIBUTION, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribExternalDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribExternalDistribution have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the DistribExternalDistribution object
   * are:
   * @li "definitionURL"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribExternalDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribExternalDistribution have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the DistribExternalDistribution object
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
   * DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
   * Predicate returning @c true if this DistribExternalDistribution's
   * attribute "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribExternalDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
   * DistribExternalDistribution.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
   * Returns the number of "elementName" in this DistribExternalDistribution.
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
   * DistribExternalDistribution.
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
 * Creates a new DistribExternalDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribExternalDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribExternalDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribExternalDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
DistribExternalDistribution_t *
DistribExternalDistribution_create(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribExternalDistribution_t
 * object.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @return a (deep) copy of this DistribExternalDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
DistribExternalDistribution_t*
DistribExternalDistribution_clone(const DistribExternalDistribution_t* ded);


/**
 * Frees this DistribExternalDistribution_t object.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
void
DistribExternalDistribution_free(DistribExternalDistribution_t* ded);


/**
 * Returns the value of the "id" attribute of this
 * DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this
 * DistribExternalDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
char *
DistribExternalDistribution_getId(const DistribExternalDistribution_t * ded);


/**
 * Returns the value of the "name" attribute of this
 * DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this
 * DistribExternalDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
char *
DistribExternalDistribution_getName(const DistribExternalDistribution_t * ded);


/**
 * Returns the value of the "definitionURL" attribute of this
 * DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure whose definitionURL
 * is sought.
 *
 * @return the value of the "definitionURL" attribute of this
 * DistribExternalDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
char *
DistribExternalDistribution_getDefinitionURL(const
  DistribExternalDistribution_t * ded);


/**
 * Predicate returning @c 1 (true) if this DistribExternalDistribution_t's "id"
 * attribute is set.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribExternalDistribution_t's "id" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_isSetId(const DistribExternalDistribution_t * ded);


/**
 * Predicate returning @c 1 (true) if this DistribExternalDistribution_t's
 * "name" attribute is set.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribExternalDistribution_t's "name" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_isSetName(const DistribExternalDistribution_t *
  ded);


/**
 * Predicate returning @c 1 (true) if this DistribExternalDistribution_t's
 * "definitionURL" attribute is set.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribExternalDistribution_t's "definitionURL"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_isSetDefinitionURL(const
  DistribExternalDistribution_t * ded);


/**
 * Sets the value of the "id" attribute of this DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribExternalDistribution_unsetId().
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_setId(DistribExternalDistribution_t * ded,
                                  const char * id);


/**
 * Sets the value of the "name" attribute of this
 * DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribExternalDistribution_unsetName().
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_setName(DistribExternalDistribution_t * ded,
                                    const char * name);


/**
 * Sets the value of the "definitionURL" attribute of this
 * DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @param definitionURL const char * value of the "definitionURL" attribute to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p definitionURL = @c NULL or an empty string is
 * equivalent to calling DistribExternalDistribution_unsetDefinitionURL().
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_setDefinitionURL(
                                             DistribExternalDistribution_t *
                                               ded,
                                             const char * definitionURL);


/**
 * Unsets the value of the "id" attribute of this
 * DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_unsetId(DistribExternalDistribution_t * ded);


/**
 * Unsets the value of the "name" attribute of this
 * DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_unsetName(DistribExternalDistribution_t * ded);


/**
 * Unsets the value of the "definitionURL" attribute of this
 * DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_unsetDefinitionURL(DistribExternalDistribution_t *
  ded);


/**
 * Returns a ListOf_t * containing DistribExternalParameter_t objects from this
 * DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure whose
 * ListOfExternalParameters is sought.
 *
 * @return the ListOfExternalParameters from this DistribExternalDistribution_t
 * as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see DistribExternalDistribution_addDistribExternalParameter()
 * @see DistribExternalDistribution_createDistribExternalParameter()
 * @see DistribExternalDistribution_getDistribExternalParameterById()
 * @see DistribExternalDistribution_getDistribExternalParameter()
 * @see DistribExternalDistribution_getNumDistribExternalParameters()
 * @see DistribExternalDistribution_removeDistribExternalParameterById()
 * @see DistribExternalDistribution_removeDistribExternalParameter()
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
ListOf_t*
DistribExternalDistribution_getListOfDistribExternalParameters(DistribExternalDistribution_t*
  ded);


/**
 * Get a DistribExternalParameter_t from the DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * DistribExternalParameter_t to retrieve.
 *
 * @return the nth DistribExternalParameter_t in the ListOfExternalParameters
 * within this DistribExternalDistribution.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalDistribution_getDistribExternalParameter(
                                                        DistribExternalDistribution_t*
                                                          ded,
                                                        unsigned int n);


/**
 * Get a DistribExternalParameter_t from the DistribExternalDistribution_t
 * based on its identifier.
 *
 * @param ded the DistribExternalDistribution_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * DistribExternalParameter_t to retrieve.
 *
 * @return the DistribExternalParameter_t in the ListOfExternalParameters
 * within this DistribExternalDistribution with the given @p sid or @c NULL if
 * no such DistribExternalParameter_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalDistribution_getDistribExternalParameterById(
                                                            DistribExternalDistribution_t*
                                                              ded,
                                                            const char *sid);


/**
 * Adds a copy of the given DistribExternalParameter_t to this
 * DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure to which the
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
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_addDistribExternalParameter(
                                                        DistribExternalDistribution_t*
                                                          ded,
                                                        const
                                                          DistribExternalParameter_t*
                                                            dep);


/**
 * Get the number of DistribExternalParameter_t objects in this
 * DistribExternalDistribution_t.
 *
 * @param ded the DistribExternalDistribution_t structure to query.
 *
 * @return the number of DistribExternalParameter_t objects in this
 * DistribExternalDistribution_t.
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
unsigned int
DistribExternalDistribution_getNumDistribExternalParameters(DistribExternalDistribution_t*
  ded);


/**
 * Creates a new DistribExternalParameter_t object, adds it to this
 * DistribExternalDistribution_t object and returns the
 * DistribExternalParameter_t object created.
 *
 * @param ded the DistribExternalDistribution_t structure to which the
 * DistribExternalParameter_t should be added.
 *
 * @return a new DistribExternalParameter_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalDistribution_createDistribExternalParameter(DistribExternalDistribution_t*
  ded);


/**
 * Removes the nth DistribExternalParameter_t from this
 * DistribExternalDistribution_t and returns a pointer to it.
 *
 * @param ded the DistribExternalDistribution_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * DistribExternalParameter_t to remove.
 *
 * @return a pointer to the nth DistribExternalParameter_t in this
 * DistribExternalDistribution_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalDistribution_removeDistribExternalParameter(
                                                           DistribExternalDistribution_t*
                                                             ded,
                                                           unsigned int n);


/**
 * Removes the DistribExternalParameter_t from this
 * DistribExternalDistribution_t based on its identifier and returns a pointer
 * to it.
 *
 * @param ded the DistribExternalDistribution_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * DistribExternalParameter_t to remove.
 *
 * @return the DistribExternalParameter_t in this DistribExternalDistribution_t
 * based on the identifier or NULL if no such DistribExternalParameter_t
 * exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalDistribution_removeDistribExternalParameterById(
                                                               DistribExternalDistribution_t*
                                                                 ded,
                                                               const char*
                                                                 sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribExternalDistribution_t object have been set.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribExternalDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required attributes for the DistribExternalDistribution_t object
 * are:
 * @li "definitionURL"
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_hasRequiredAttributes(const
  DistribExternalDistribution_t * ded);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribExternalDistribution_t object have been set.
 *
 * @param ded the DistribExternalDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribExternalDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required elements for the DistribExternalDistribution_t object
 * are:
 *
 * @memberof DistribExternalDistribution_t
 */
LIBSBML_EXTERN
int
DistribExternalDistribution_hasRequiredElements(const
  DistribExternalDistribution_t * ded);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribExternalDistribution_H__ */


