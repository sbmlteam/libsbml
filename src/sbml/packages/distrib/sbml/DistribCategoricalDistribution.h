/**
 * @file DistribCategoricalDistribution.h
 * @brief Definition of the DistribCategoricalDistribution class.
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
 * @class DistribCategoricalDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribCategoricalDistribution
 * class.
 */


#ifndef DistribCategoricalDistribution_H__
#define DistribCategoricalDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribCategoricalUnivariateDistribution.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/ListOfCategories.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribCategoricalDistribution : public
  DistribCategoricalUnivariateDistribution
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfCategories mDistribCategories;

  /** @endcond */

public:

  /**
   * Creates a new DistribCategoricalDistribution using the given SBML Level,
   * Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribCategoricalDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribCategoricalDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribCategoricalDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribCategoricalDistribution(
                                 unsigned int level =
                                   DistribExtension::getDefaultLevel(),
                                 unsigned int version =
                                   DistribExtension::getDefaultVersion(),
                                 unsigned int pkgVersion =
                                   DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribCategoricalDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribCategoricalDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribCategoricalDistribution.
   *
   * @param orig the DistribCategoricalDistribution instance to copy.
   */
  DistribCategoricalDistribution(const DistribCategoricalDistribution& orig);


  /**
   * Assignment operator for DistribCategoricalDistribution.
   *
   * @param rhs the DistribCategoricalDistribution object whose values are to
   * be used as the basis of the assignment.
   */
  DistribCategoricalDistribution& operator=(const
    DistribCategoricalDistribution& rhs);


  /**
   * Creates and returns a deep copy of this DistribCategoricalDistribution
   * object.
   *
   * @return a (deep) copy of this DistribCategoricalDistribution object.
   */
  virtual DistribCategoricalDistribution* clone() const;


  /**
   * Destructor for DistribCategoricalDistribution.
   */
  virtual ~DistribCategoricalDistribution();


  /**
   * Returns the value of the "id" attribute of this
   * DistribCategoricalDistribution.
   *
   * @return the value of the "id" attribute of this
   * DistribCategoricalDistribution as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this
   * DistribCategoricalDistribution.
   *
   * @return the value of the "name" attribute of this
   * DistribCategoricalDistribution as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this DistribCategoricalDistribution's "id"
   * attribute is set.
   *
   * @return @c true if this DistribCategoricalDistribution's "id" attribute
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribCategoricalDistribution's
   * "name" attribute is set.
   *
   * @return @c true if this DistribCategoricalDistribution's "name" attribute
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this
   * DistribCategoricalDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the ListOfCategories from this DistribCategoricalDistribution.
   *
   * @return the ListOfCategories from this DistribCategoricalDistribution.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see getDistribCategory(const std::string& sid)
   * @see getDistribCategory(unsigned int n)
   * @see getNumDistribCategories()
   * @see removeDistribCategory(const std::string& sid)
   * @see removeDistribCategory(unsigned int n)
   */
  const ListOfCategories* getListOfDistribCategories() const;


  /**
   * Returns the ListOfCategories from this DistribCategoricalDistribution.
   *
   * @return the ListOfCategories from this DistribCategoricalDistribution.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see getDistribCategory(const std::string& sid)
   * @see getDistribCategory(unsigned int n)
   * @see getNumDistribCategories()
   * @see removeDistribCategory(const std::string& sid)
   * @see removeDistribCategory(unsigned int n)
   */
  ListOfCategories* getListOfDistribCategories();


  /**
   * Get a DistribCategory from the DistribCategoricalDistribution.
   *
   * @param n an unsigned int representing the index of the DistribCategory to
   * retrieve.
   *
   * @return the nth DistribCategory in the ListOfCategories within this
   * DistribCategoricalDistribution.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see getDistribCategory(const std::string& sid)
   * @see getNumDistribCategories()
   * @see removeDistribCategory(const std::string& sid)
   * @see removeDistribCategory(unsigned int n)
   */
  DistribCategory* getDistribCategory(unsigned int n);


  /**
   * Get a DistribCategory from the DistribCategoricalDistribution.
   *
   * @param n an unsigned int representing the index of the DistribCategory to
   * retrieve.
   *
   * @return the nth DistribCategory in the ListOfCategories within this
   * DistribCategoricalDistribution.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see getDistribCategory(const std::string& sid)
   * @see getNumDistribCategories()
   * @see removeDistribCategory(const std::string& sid)
   * @see removeDistribCategory(unsigned int n)
   */
  const DistribCategory* getDistribCategory(unsigned int n) const;


  /**
   * Get a DistribCategory from the DistribCategoricalDistribution based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the DistribCategory to
   * retrieve.
   *
   * @return the DistribCategory in the ListOfCategories within this
   * DistribCategoricalDistribution with the given @p sid or @c NULL if no such
   * DistribCategory exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see getDistribCategory(unsigned int n)
   * @see getNumDistribCategories()
   * @see removeDistribCategory(const std::string& sid)
   * @see removeDistribCategory(unsigned int n)
   */
  DistribCategory* getDistribCategory(const std::string& sid);


  /**
   * Get a DistribCategory from the DistribCategoricalDistribution based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the DistribCategory to
   * retrieve.
   *
   * @return the DistribCategory in the ListOfCategories within this
   * DistribCategoricalDistribution with the given @p sid or @c NULL if no such
   * DistribCategory exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see getDistribCategory(unsigned int n)
   * @see getNumDistribCategories()
   * @see removeDistribCategory(const std::string& sid)
   * @see removeDistribCategory(unsigned int n)
   */
  const DistribCategory* getDistribCategory(const std::string& sid) const;


  /**
   * Adds a copy of the given DistribCategory to this
   * DistribCategoricalDistribution.
   *
   * @param dc the DistribCategory object to add.
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
   * @see createDistribCategory()
   * @see getDistribCategory(const std::string& sid)
   * @see getDistribCategory(unsigned int n)
   * @see getNumDistribCategories()
   * @see removeDistribCategory(const std::string& sid)
   * @see removeDistribCategory(unsigned int n)
   */
  int addDistribCategory(const DistribCategory* dc);


  /**
   * Get the number of DistribCategory objects in this
   * DistribCategoricalDistribution.
   *
   * @return the number of DistribCategory objects in this
   * DistribCategoricalDistribution.
   *
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see getDistribCategory(const std::string& sid)
   * @see getDistribCategory(unsigned int n)
   * @see removeDistribCategory(const std::string& sid)
   * @see removeDistribCategory(unsigned int n)
   */
  unsigned int getNumDistribCategories() const;


  /**
   * Creates a new DistribCategory object, adds it to this
   * DistribCategoricalDistribution object and returns the DistribCategory
   * object created.
   *
   * @return a new DistribCategory object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see getDistribCategory(const std::string& sid)
   * @see getDistribCategory(unsigned int n)
   * @see getNumDistribCategories()
   * @see removeDistribCategory(const std::string& sid)
   * @see removeDistribCategory(unsigned int n)
   */
  DistribCategory* createDistribCategory();


  /**
   * Removes the nth DistribCategory from this DistribCategoricalDistribution
   * and returns a pointer to it.
   *
   * @param n an unsigned int representing the index of the DistribCategory to
   * remove.
   *
   * @return a pointer to the nth DistribCategory in this
   * DistribCategoricalDistribution.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see getDistribCategory(const std::string& sid)
   * @see getDistribCategory(unsigned int n)
   * @see getNumDistribCategories()
   * @see removeDistribCategory(const std::string& sid)
   */
  DistribCategory* removeDistribCategory(unsigned int n);


  /**
   * Removes the DistribCategory from this DistribCategoricalDistribution based
   * on its identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the DistribCategory to
   * remove.
   *
   * @return the DistribCategory in this DistribCategoricalDistribution based
   * on the identifier or NULL if no such DistribCategory exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addDistribCategory(const DistribCategory* object)
   * @see createDistribCategory()
   * @see getDistribCategory(const std::string& sid)
   * @see getDistribCategory(unsigned int n)
   * @see getNumDistribCategories()
   * @see removeDistribCategory(unsigned int n)
   */
  DistribCategory* removeDistribCategory(const std::string& sid);


  /**
   * Returns the XML element name of this DistribCategoricalDistribution
   * object.
   *
   * For DistribCategoricalDistribution, the XML element name is always
   * @c "categoricalDistribution".
   *
   * @return the name of this element, i.e. @c "categoricalDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribCategoricalDistribution
   * object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_CATEGORICALDISTRIBUTION,
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
   * DistribCategoricalDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribCategoricalDistribution have been set, otherwise @c false is
   * returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribCategoricalDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribCategoricalDistribution have been set, otherwise @c false is
   * returned.
   *
   *
   * @note The required elements for the DistribCategoricalDistribution object
   * are:
   * @li "distribCategory"
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * Predicate returning @c true if this DistribCategoricalDistribution's
   * attribute "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribCategoricalDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
   * DistribCategoricalDistribution.
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
 * Creates a new DistribCategoricalDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribCategoricalDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribCategoricalDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribCategoricalDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
DistribCategoricalDistribution_t *
DistribCategoricalDistribution_create(unsigned int level,
                                      unsigned int version,
                                      unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribCategoricalDistribution_t
 * object.
 *
 * @param dcd the DistribCategoricalDistribution_t structure.
 *
 * @return a (deep) copy of this DistribCategoricalDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
DistribCategoricalDistribution_t*
DistribCategoricalDistribution_clone(const DistribCategoricalDistribution_t*
  dcd);


/**
 * Frees this DistribCategoricalDistribution_t object.
 *
 * @param dcd the DistribCategoricalDistribution_t structure.
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
void
DistribCategoricalDistribution_free(DistribCategoricalDistribution_t* dcd);


/**
 * Returns the value of the "id" attribute of this
 * DistribCategoricalDistribution_t.
 *
 * @param dcd the DistribCategoricalDistribution_t structure whose id is
 * sought.
 *
 * @return the value of the "id" attribute of this
 * DistribCategoricalDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
char *
DistribCategoricalDistribution_getId(const DistribCategoricalDistribution_t *
  dcd);


/**
 * Returns the value of the "name" attribute of this
 * DistribCategoricalDistribution_t.
 *
 * @param dcd the DistribCategoricalDistribution_t structure whose name is
 * sought.
 *
 * @return the value of the "name" attribute of this
 * DistribCategoricalDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
char *
DistribCategoricalDistribution_getName(const DistribCategoricalDistribution_t *
  dcd);


/**
 * Predicate returning @c 1 (true) if this DistribCategoricalDistribution_t's
 * "id" attribute is set.
 *
 * @param dcd the DistribCategoricalDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribCategoricalDistribution_t's "id"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_isSetId(const DistribCategoricalDistribution_t *
  dcd);


/**
 * Predicate returning @c 1 (true) if this DistribCategoricalDistribution_t's
 * "name" attribute is set.
 *
 * @param dcd the DistribCategoricalDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribCategoricalDistribution_t's "name"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_isSetName(const DistribCategoricalDistribution_t
  * dcd);


/**
 * Sets the value of the "id" attribute of this
 * DistribCategoricalDistribution_t.
 *
 * @param dcd the DistribCategoricalDistribution_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribCategoricalDistribution_unsetId().
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_setId(DistribCategoricalDistribution_t * dcd,
                                     const char * id);


/**
 * Sets the value of the "name" attribute of this
 * DistribCategoricalDistribution_t.
 *
 * @param dcd the DistribCategoricalDistribution_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribCategoricalDistribution_unsetName().
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_setName(DistribCategoricalDistribution_t * dcd,
                                       const char * name);


/**
 * Unsets the value of the "id" attribute of this
 * DistribCategoricalDistribution_t.
 *
 * @param dcd the DistribCategoricalDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_unsetId(DistribCategoricalDistribution_t * dcd);


/**
 * Unsets the value of the "name" attribute of this
 * DistribCategoricalDistribution_t.
 *
 * @param dcd the DistribCategoricalDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_unsetName(DistribCategoricalDistribution_t *
  dcd);


/**
 * Returns a ListOf_t * containing DistribCategory_t objects from this
 * DistribCategoricalDistribution_t.
 *
 * @param dcd the DistribCategoricalDistribution_t structure whose
 * ListOfCategories is sought.
 *
 * @return the ListOfCategories from this DistribCategoricalDistribution_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see DistribCategoricalDistribution_addDistribCategory()
 * @see DistribCategoricalDistribution_createDistribCategory()
 * @see DistribCategoricalDistribution_getDistribCategoryById()
 * @see DistribCategoricalDistribution_getDistribCategory()
 * @see DistribCategoricalDistribution_getNumDistribCategories()
 * @see DistribCategoricalDistribution_removeDistribCategoryById()
 * @see DistribCategoricalDistribution_removeDistribCategory()
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
ListOf_t*
DistribCategoricalDistribution_getListOfDistribCategories(DistribCategoricalDistribution_t*
  dcd);


/**
 * Get a DistribCategory_t from the DistribCategoricalDistribution_t.
 *
 * @param dcd the DistribCategoricalDistribution_t structure to search.
 *
 * @param n an unsigned int representing the index of the DistribCategory_t to
 * retrieve.
 *
 * @return the nth DistribCategory_t in the ListOfCategories within this
 * DistribCategoricalDistribution.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategoricalDistribution_getDistribCategory(
                                                  DistribCategoricalDistribution_t*
                                                    dcd,
                                                  unsigned int n);


/**
 * Get a DistribCategory_t from the DistribCategoricalDistribution_t based on
 * its identifier.
 *
 * @param dcd the DistribCategoricalDistribution_t structure to search.
 *
 * @param sid a string representing the identifier of the DistribCategory_t to
 * retrieve.
 *
 * @return the DistribCategory_t in the ListOfCategories within this
 * DistribCategoricalDistribution with the given @p sid or @c NULL if no such
 * DistribCategory_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategoricalDistribution_getDistribCategoryById(
                                                      DistribCategoricalDistribution_t*
                                                        dcd,
                                                      const char *sid);


/**
 * Adds a copy of the given DistribCategory_t to this
 * DistribCategoricalDistribution_t.
 *
 * @param dcd the DistribCategoricalDistribution_t structure to which the
 * DistribCategory_t should be added.
 *
 * @param dc the DistribCategory_t object to add.
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
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_addDistribCategory(
                                                  DistribCategoricalDistribution_t*
                                                    dcd,
                                                  const DistribCategory_t* dc);


/**
 * Get the number of DistribCategory_t objects in this
 * DistribCategoricalDistribution_t.
 *
 * @param dcd the DistribCategoricalDistribution_t structure to query.
 *
 * @return the number of DistribCategory_t objects in this
 * DistribCategoricalDistribution_t.
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
unsigned int
DistribCategoricalDistribution_getNumDistribCategories(DistribCategoricalDistribution_t*
  dcd);


/**
 * Creates a new DistribCategory_t object, adds it to this
 * DistribCategoricalDistribution_t object and returns the DistribCategory_t
 * object created.
 *
 * @param dcd the DistribCategoricalDistribution_t structure to which the
 * DistribCategory_t should be added.
 *
 * @return a new DistribCategory_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategoricalDistribution_createDistribCategory(DistribCategoricalDistribution_t*
  dcd);


/**
 * Removes the nth DistribCategory_t from this DistribCategoricalDistribution_t
 * and returns a pointer to it.
 *
 * @param dcd the DistribCategoricalDistribution_t structure to search.
 *
 * @param n an unsigned int representing the index of the DistribCategory_t to
 * remove.
 *
 * @return a pointer to the nth DistribCategory_t in this
 * DistribCategoricalDistribution_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategoricalDistribution_removeDistribCategory(
                                                     DistribCategoricalDistribution_t*
                                                       dcd,
                                                     unsigned int n);


/**
 * Removes the DistribCategory_t from this DistribCategoricalDistribution_t
 * based on its identifier and returns a pointer to it.
 *
 * @param dcd the DistribCategoricalDistribution_t structure to search.
 *
 * @param sid a string representing the identifier of the DistribCategory_t to
 * remove.
 *
 * @return the DistribCategory_t in this DistribCategoricalDistribution_t based
 * on the identifier or NULL if no such DistribCategory_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
DistribCategory_t*
DistribCategoricalDistribution_removeDistribCategoryById(
                                                         DistribCategoricalDistribution_t*
                                                           dcd,
                                                         const char* sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribCategoricalDistribution_t object have been set.
 *
 * @param dcd the DistribCategoricalDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribCategoricalDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_hasRequiredAttributes(const
  DistribCategoricalDistribution_t * dcd);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribCategoricalDistribution_t object have been set.
 *
 * @param dcd the DistribCategoricalDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribCategoricalDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required elements for the DistribCategoricalDistribution_t object
 * are:
 * @li "distribCategory"
 *
 * @memberof DistribCategoricalDistribution_t
 */
LIBSBML_EXTERN
int
DistribCategoricalDistribution_hasRequiredElements(const
  DistribCategoricalDistribution_t * dcd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribCategoricalDistribution_H__ */


