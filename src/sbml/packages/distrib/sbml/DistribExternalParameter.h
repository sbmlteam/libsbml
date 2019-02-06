/**
 * @file DistribExternalParameter.h
 * @brief Definition of the DistribExternalParameter class.
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
 * @class DistribExternalParameter
 * @sbmlbrief{distrib} TODO:Definition of the DistribExternalParameter class.
 */


#ifndef DistribExternalParameter_H__
#define DistribExternalParameter_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribUncertValue.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN

class ListOfDistribExternalParameters;

class LIBSBML_EXTERN DistribExternalParameter : public DistribUncertValue
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mDefinitionURL;
  ListOfDistribExternalParameters* mDistribExternalParameters;

  /** @endcond */

public:

  /**
   * Creates a new DistribExternalParameter using the given SBML Level, Version
   * and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribExternalParameter.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribExternalParameter.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribExternalParameter.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribExternalParameter(
                           unsigned int level =
                             DistribExtension::getDefaultLevel(),
                           unsigned int version =
                             DistribExtension::getDefaultVersion(),
                           unsigned int pkgVersion =
                             DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribExternalParameter using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribExternalParameter(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribExternalParameter.
   *
   * @param orig the DistribExternalParameter instance to copy.
   */
  DistribExternalParameter(const DistribExternalParameter& orig);


  /**
   * Assignment operator for DistribExternalParameter.
   *
   * @param rhs the DistribExternalParameter object whose values are to be used
   * as the basis of the assignment.
   */
  DistribExternalParameter& operator=(const DistribExternalParameter& rhs);


  /**
   * Creates and returns a deep copy of this DistribExternalParameter object.
   *
   * @return a (deep) copy of this DistribExternalParameter object.
   */
  virtual DistribExternalParameter* clone() const;


  /**
   * Destructor for DistribExternalParameter.
   */
  virtual ~DistribExternalParameter();


  /**
   * Returns the value of the "definitionURL" attribute of this
   * DistribExternalParameter.
   *
   * @return the value of the "definitionURL" attribute of this
   * DistribExternalParameter as a string.
   */
  const std::string& getDefinitionURL() const;


  /**
   * Predicate returning @c true if this DistribExternalParameter's
   * "definitionURL" attribute is set.
   *
   * @return @c true if this DistribExternalParameter's "definitionURL"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetDefinitionURL() const;


  /**
   * Sets the value of the "definitionURL" attribute of this
   * DistribExternalParameter.
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
   * Unsets the value of the "definitionURL" attribute of this
   * DistribExternalParameter.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDefinitionURL();


  /**
   * Returns the ListOfDistribExternalParameters from this
   * DistribExternalParameter.
   *
   * @return the ListOfDistribExternalParameters from this
   * DistribExternalParameter.
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
  const ListOfDistribExternalParameters* getListOfDistribExternalParameters()
    const;


  /**
   * Returns the ListOfDistribExternalParameters from this
   * DistribExternalParameter.
   *
   * @return the ListOfDistribExternalParameters from this
   * DistribExternalParameter.
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
  ListOfDistribExternalParameters* getListOfDistribExternalParameters();


  /**
   * Get a DistribExternalParameter from the DistribExternalParameter.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to retrieve.
   *
   * @return the nth DistribExternalParameter in the
   * ListOfDistribExternalParameters within this DistribExternalParameter.
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
   * Get a DistribExternalParameter from the DistribExternalParameter.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to retrieve.
   *
   * @return the nth DistribExternalParameter in the
   * ListOfDistribExternalParameters within this DistribExternalParameter.
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
   * Get a DistribExternalParameter from the DistribExternalParameter based on
   * its identifier.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to retrieve.
   *
   * @return the DistribExternalParameter in the ListOfExternalParameters
   * within this DistribExternalParameter with the given @p sid or @c NULL if
   * no such DistribExternalParameter exists.
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
   * Get a DistribExternalParameter from the DistribExternalParameter based on
   * its identifier.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to retrieve.
   *
   * @return the DistribExternalParameter in the ListOfExternalParameters
   * within this DistribExternalParameter with the given @p sid or @c NULL if
   * no such DistribExternalParameter exists.
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
   * DistribExternalParameter.
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
   * DistribExternalParameter.
   *
   * @return the number of DistribExternalParameter objects in this
   * DistribExternalParameter.
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
   * DistribExternalParameter object and returns the DistribExternalParameter
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
   * Removes the nth DistribExternalParameter from this
   * DistribExternalParameter and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the
   * DistribExternalParameter to remove.
   *
   * @return a pointer to the nth DistribExternalParameter in this
   * DistribExternalParameter.
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
   * Removes the DistribExternalParameter from this DistribExternalParameter
   * based on its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the
   * DistribExternalParameter to remove.
   *
   * @return the DistribExternalParameter in this DistribExternalParameter
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
   * Returns the XML element name of this DistribExternalParameter object.
   *
   * For DistribExternalParameter, the XML element name is always
   * @c "externalParameter".
   *
   * @return the name of this element, i.e. @c "externalParameter".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribExternalParameter object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_EXTERNALPARAMETER, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribExternalParameter object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribExternalParameter have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the DistribExternalParameter object are:
   * @li "definitionURL"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribExternalParameter object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribExternalParameter have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the DistribExternalParameter object are:
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
   * DistribExternalParameter.
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
   * DistribExternalParameter.
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
   * DistribExternalParameter.
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
   * DistribExternalParameter.
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
   * DistribExternalParameter.
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
   * Predicate returning @c true if this DistribExternalParameter's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribExternalParameter's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribExternalParameter.
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
   * DistribExternalParameter.
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
   * DistribExternalParameter.
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
   * DistribExternalParameter.
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
   * DistribExternalParameter.
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
   * DistribExternalParameter.
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
   * DistribExternalParameter.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribExternalParameter.
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
   * DistribExternalParameter.
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
   * Returns the number of "elementName" in this DistribExternalParameter.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this DistribExternalParameter.
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
  void readL3V1V1Attributes(const XMLAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  void readL3V2V1Attributes(const XMLAttributes& attributes);

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
 * Creates a new DistribExternalParameter_t using the given SBML Level, Version
 * and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribExternalParameter_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribExternalParameter_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribExternalParameter_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t *
DistribExternalParameter_create(unsigned int level,
                                unsigned int version,
                                unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribExternalParameter_t object.
 *
 * @param dep the DistribExternalParameter_t structure.
 *
 * @return a (deep) copy of this DistribExternalParameter_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_clone(const DistribExternalParameter_t* dep);


/**
 * Frees this DistribExternalParameter_t object.
 *
 * @param dep the DistribExternalParameter_t structure.
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
void
DistribExternalParameter_free(DistribExternalParameter_t* dep);


/**
 * Returns the value of the "definitionURL" attribute of this
 * DistribExternalParameter_t.
 *
 * @param dep the DistribExternalParameter_t structure whose definitionURL is
 * sought.
 *
 * @return the value of the "definitionURL" attribute of this
 * DistribExternalParameter_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
char *
DistribExternalParameter_getDefinitionURL(const DistribExternalParameter_t *
  dep);


/**
 * Predicate returning @c 1 (true) if this DistribExternalParameter_t's
 * "definitionURL" attribute is set.
 *
 * @param dep the DistribExternalParameter_t structure.
 *
 * @return @c 1 (true) if this DistribExternalParameter_t's "definitionURL"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
int
DistribExternalParameter_isSetDefinitionURL(const DistribExternalParameter_t *
  dep);


/**
 * Sets the value of the "definitionURL" attribute of this
 * DistribExternalParameter_t.
 *
 * @param dep the DistribExternalParameter_t structure.
 *
 * @param definitionURL const char * value of the "definitionURL" attribute to
 * be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p definitionURL = @c NULL or an empty string is
 * equivalent to calling DistribExternalParameter_unsetDefinitionURL().
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
int
DistribExternalParameter_setDefinitionURL(DistribExternalParameter_t * dep,
                                          const char * definitionURL);


/**
 * Unsets the value of the "definitionURL" attribute of this
 * DistribExternalParameter_t.
 *
 * @param dep the DistribExternalParameter_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
int
DistribExternalParameter_unsetDefinitionURL(DistribExternalParameter_t * dep);


/**
 * Returns a ListOf_t * containing DistribExternalParameter_t objects from this
 * DistribExternalParameter_t.
 *
 * @param dep the DistribExternalParameter_t structure whose
 * ListOfDistribExternalParameters is sought.
 *
 * @return the ListOfDistribExternalParameters from this
 * DistribExternalParameter_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see DistribExternalParameter_addDistribExternalParameter()
 * @see DistribExternalParameter_createDistribExternalParameter()
 * @see DistribExternalParameter_getDistribExternalParameterById()
 * @see DistribExternalParameter_getDistribExternalParameter()
 * @see DistribExternalParameter_getNumDistribExternalParameters()
 * @see DistribExternalParameter_removeDistribExternalParameterById()
 * @see DistribExternalParameter_removeDistribExternalParameter()
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
ListOf_t*
DistribExternalParameter_getListOfDistribExternalParameters(DistribExternalParameter_t*
  dep);


/**
 * Get a DistribExternalParameter_t from the DistribExternalParameter_t.
 *
 * @param dep the DistribExternalParameter_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * DistribExternalParameter_t to retrieve.
 *
 * @return the nth DistribExternalParameter_t in the
 * ListOfDistribExternalParameters within this DistribExternalParameter.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_getDistribExternalParameter(
                                                     DistribExternalParameter_t*
                                                       dep,
                                                     unsigned int n);


/**
 * Get a DistribExternalParameter_t from the DistribExternalParameter_t based
 * on its identifier.
 *
 * @param dep the DistribExternalParameter_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * DistribExternalParameter_t to retrieve.
 *
 * @return the DistribExternalParameter_t in the ListOfExternalParameters
 * within this DistribExternalParameter with the given @p sid or @c NULL if no
 * such DistribExternalParameter_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_getDistribExternalParameterById(
                                                         DistribExternalParameter_t*
                                                           dep,
                                                         const char *sid);


/**
 * Adds a copy of the given DistribExternalParameter_t to this
 * DistribExternalParameter_t.
 *
 * @param dep the DistribExternalParameter_t structure to which the
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
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
int
DistribExternalParameter_addDistribExternalParameter(
                                                     DistribExternalParameter_t*
                                                       dep,
                                                     const
                                                       DistribExternalParameter_t*
                                                         newdep);


/**
 * Get the number of DistribExternalParameter_t objects in this
 * DistribExternalParameter_t.
 *
 * @param dep the DistribExternalParameter_t structure to query.
 *
 * @return the number of DistribExternalParameter_t objects in this
 * DistribExternalParameter_t.
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
unsigned int
DistribExternalParameter_getNumDistribExternalParameters(DistribExternalParameter_t*
  dep);


/**
 * Creates a new DistribExternalParameter_t object, adds it to this
 * DistribExternalParameter_t object and returns the DistribExternalParameter_t
 * object created.
 *
 * @param dep the DistribExternalParameter_t structure to which the
 * DistribExternalParameter_t should be added.
 *
 * @return a new DistribExternalParameter_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_createDistribExternalParameter(DistribExternalParameter_t*
  dep);


/**
 * Removes the nth DistribExternalParameter_t from this
 * DistribExternalParameter_t and returns a pointer to it.
 *
 * @param dep the DistribExternalParameter_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * DistribExternalParameter_t to remove.
 *
 * @return a pointer to the nth DistribExternalParameter_t in this
 * DistribExternalParameter_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_removeDistribExternalParameter(
                                                        DistribExternalParameter_t*
                                                          dep,
                                                        unsigned int n);


/**
 * Removes the DistribExternalParameter_t from this DistribExternalParameter_t
 * based on its identifier and returns a pointer to it.
 *
 * @param dep the DistribExternalParameter_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * DistribExternalParameter_t to remove.
 *
 * @return the DistribExternalParameter_t in this DistribExternalParameter_t
 * based on the identifier or NULL if no such DistribExternalParameter_t
 * exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
DistribExternalParameter_t*
DistribExternalParameter_removeDistribExternalParameterById(
                                                            DistribExternalParameter_t*
                                                              dep,
                                                            const char* sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribExternalParameter_t object have been set.
 *
 * @param dep the DistribExternalParameter_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribExternalParameter_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required attributes for the DistribExternalParameter_t object are:
 * @li "definitionURL"
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
int
DistribExternalParameter_hasRequiredAttributes(const DistribExternalParameter_t
  * dep);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribExternalParameter_t object have been set.
 *
 * @param dep the DistribExternalParameter_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribExternalParameter_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required elements for the DistribExternalParameter_t object are:
 *
 * @memberof DistribExternalParameter_t
 */
LIBSBML_EXTERN
int
DistribExternalParameter_hasRequiredElements(const DistribExternalParameter_t *
  dep);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribExternalParameter_H__ */


