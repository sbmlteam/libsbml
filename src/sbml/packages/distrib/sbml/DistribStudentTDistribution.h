/**
 * @file DistribStudentTDistribution.h
 * @brief Definition of the DistribStudentTDistribution class.
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
 * @class DistribStudentTDistribution
 * @sbmlbrief{distrib} TODO:Definition of the DistribStudentTDistribution
 * class.
 */


#ifndef DistribStudentTDistribution_H__
#define DistribStudentTDistribution_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/distrib/common/distribfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/distrib/sbml/DistribContinuousUnivariateDistribution.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/sbml/DistribUncertValue.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribStudentTDistribution : public
  DistribContinuousUnivariateDistribution
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribUncertValue* mLocation;
  DistribUncertValue* mScale;
  DistribUncertValue* mDegreesOfFreedom;

  /** @endcond */

public:

  /**
   * Creates a new DistribStudentTDistribution using the given SBML Level,
   * Version and &ldquo;distrib&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DistribStudentTDistribution.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DistribStudentTDistribution.
   *
   * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
   * this DistribStudentTDistribution.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribStudentTDistribution(
                              unsigned int level =
                                DistribExtension::getDefaultLevel(),
                              unsigned int version =
                                DistribExtension::getDefaultVersion(),
                              unsigned int pkgVersion =
                                DistribExtension::getDefaultPackageVersion());


  /**
   * Creates a new DistribStudentTDistribution using the given
   * DistribPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param distribns the DistribPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DistribStudentTDistribution(DistribPkgNamespaces *distribns);


  /**
   * Copy constructor for DistribStudentTDistribution.
   *
   * @param orig the DistribStudentTDistribution instance to copy.
   */
  DistribStudentTDistribution(const DistribStudentTDistribution& orig);


  /**
   * Assignment operator for DistribStudentTDistribution.
   *
   * @param rhs the DistribStudentTDistribution object whose values are to be
   * used as the basis of the assignment.
   */
  DistribStudentTDistribution& operator=(const DistribStudentTDistribution&
    rhs);


  /**
   * Creates and returns a deep copy of this DistribStudentTDistribution
   * object.
   *
   * @return a (deep) copy of this DistribStudentTDistribution object.
   */
  virtual DistribStudentTDistribution* clone() const;


  /**
   * Destructor for DistribStudentTDistribution.
   */
  virtual ~DistribStudentTDistribution();


  /**
   * Returns the value of the "id" attribute of this
   * DistribStudentTDistribution.
   *
   * @return the value of the "id" attribute of this
   * DistribStudentTDistribution as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this
   * DistribStudentTDistribution.
   *
   * @return the value of the "name" attribute of this
   * DistribStudentTDistribution as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true if this DistribStudentTDistribution's "id"
   * attribute is set.
   *
   * @return @c true if this DistribStudentTDistribution's "id" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this DistribStudentTDistribution's "name"
   * attribute is set.
   *
   * @return @c true if this DistribStudentTDistribution's "name" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this
   * DistribStudentTDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "location" element of this
   * DistribStudentTDistribution.
   *
   * @return the value of the "location" element of this
   * DistribStudentTDistribution as a DistribUncertValue*.
   */
  const DistribUncertValue* getLocation() const;


  /**
   * Returns the value of the "location" element of this
   * DistribStudentTDistribution.
   *
   * @return the value of the "location" element of this
   * DistribStudentTDistribution as a DistribUncertValue*.
   */
  DistribUncertValue* getLocation();


  /**
   * Returns the value of the "scale" element of this
   * DistribStudentTDistribution.
   *
   * @return the value of the "scale" element of this
   * DistribStudentTDistribution as a DistribUncertValue*.
   */
  const DistribUncertValue* getScale() const;


  /**
   * Returns the value of the "scale" element of this
   * DistribStudentTDistribution.
   *
   * @return the value of the "scale" element of this
   * DistribStudentTDistribution as a DistribUncertValue*.
   */
  DistribUncertValue* getScale();


  /**
   * Returns the value of the "degreesOfFreedom" element of this
   * DistribStudentTDistribution.
   *
   * @return the value of the "degreesOfFreedom" element of this
   * DistribStudentTDistribution as a DistribUncertValue*.
   */
  const DistribUncertValue* getDegreesOfFreedom() const;


  /**
   * Returns the value of the "degreesOfFreedom" element of this
   * DistribStudentTDistribution.
   *
   * @return the value of the "degreesOfFreedom" element of this
   * DistribStudentTDistribution as a DistribUncertValue*.
   */
  DistribUncertValue* getDegreesOfFreedom();


  /**
   * Predicate returning @c true if this DistribStudentTDistribution's
   * "location" element is set.
   *
   * @return @c true if this DistribStudentTDistribution's "location" element
   * has been set, otherwise @c false is returned.
   */
  bool isSetLocation() const;


  /**
   * Predicate returning @c true if this DistribStudentTDistribution's "scale"
   * element is set.
   *
   * @return @c true if this DistribStudentTDistribution's "scale" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetScale() const;


  /**
   * Predicate returning @c true if this DistribStudentTDistribution's
   * "degreesOfFreedom" element is set.
   *
   * @return @c true if this DistribStudentTDistribution's "degreesOfFreedom"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetDegreesOfFreedom() const;


  /**
   * Sets the value of the "location" element of this
   * DistribStudentTDistribution.
   *
   * @param location DistribUncertValue* value of the "location" element to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setLocation(const DistribUncertValue* location);


  /**
   * Sets the value of the "scale" element of this DistribStudentTDistribution.
   *
   * @param scale DistribUncertValue* value of the "scale" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setScale(const DistribUncertValue* scale);


  /**
   * Sets the value of the "degreesOfFreedom" element of this
   * DistribStudentTDistribution.
   *
   * @param degreesOfFreedom DistribUncertValue* value of the
   * "degreesOfFreedom" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDegreesOfFreedom(const DistribUncertValue* degreesOfFreedom);


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribStudentTDistribution object and returns the DistribUncertValue
   * object created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createLocation();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribStudentTDistribution object and returns the DistribUncertValue
   * object created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createScale();


  /**
   * Creates a new DistribUncertValue object, adds it to this
   * DistribStudentTDistribution object and returns the DistribUncertValue
   * object created.
   *
   * @return a new DistribUncertValue object instance.
   */
  DistribUncertValue* createDegreesOfFreedom();


  /**
   * Unsets the value of the "location" element of this
   * DistribStudentTDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLocation();


  /**
   * Unsets the value of the "scale" element of this
   * DistribStudentTDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetScale();


  /**
   * Unsets the value of the "degreesOfFreedom" element of this
   * DistribStudentTDistribution.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDegreesOfFreedom();


  /**
   * Returns the XML element name of this DistribStudentTDistribution object.
   *
   * For DistribStudentTDistribution, the XML element name is always
   * @c "studentTDistribution".
   *
   * @return the name of this element, i.e. @c "studentTDistribution".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DistribStudentTDistribution object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_DISTRIB_STUDENTTDISTRIBUTION, SBMLDistribTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DistribStudentTDistribution object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DistribStudentTDistribution have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * DistribStudentTDistribution object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * DistribStudentTDistribution have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the DistribStudentTDistribution object
   * are:
   * @li "location"
   * @li "scale"
   * @li "degreesOfFreedom"
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
   * DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
   * Predicate returning @c true if this DistribStudentTDistribution's
   * attribute "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribStudentTDistribution's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
   * Returns the number of "elementName" in this DistribStudentTDistribution.
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
   * DistribStudentTDistribution.
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
 * Creates a new DistribStudentTDistribution_t using the given SBML Level,
 * Version and &ldquo;distrib&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DistribStudentTDistribution_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DistribStudentTDistribution_t.
 *
 * @param pkgVersion an unsigned int, the SBML Distrib Version to assign to
 * this DistribStudentTDistribution_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
DistribStudentTDistribution_t *
DistribStudentTDistribution_create(unsigned int level,
                                   unsigned int version,
                                   unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DistribStudentTDistribution_t
 * object.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @return a (deep) copy of this DistribStudentTDistribution_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
DistribStudentTDistribution_t*
DistribStudentTDistribution_clone(const DistribStudentTDistribution_t* dstd);


/**
 * Frees this DistribStudentTDistribution_t object.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
void
DistribStudentTDistribution_free(DistribStudentTDistribution_t* dstd);


/**
 * Returns the value of the "id" attribute of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this
 * DistribStudentTDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
char *
DistribStudentTDistribution_getId(const DistribStudentTDistribution_t * dstd);


/**
 * Returns the value of the "name" attribute of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure whose name is
 * sought.
 *
 * @return the value of the "name" attribute of this
 * DistribStudentTDistribution_t as a pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
char *
DistribStudentTDistribution_getName(const DistribStudentTDistribution_t *
  dstd);


/**
 * Predicate returning @c 1 (true) if this DistribStudentTDistribution_t's "id"
 * attribute is set.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribStudentTDistribution_t's "id" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_isSetId(const DistribStudentTDistribution_t *
  dstd);


/**
 * Predicate returning @c 1 (true) if this DistribStudentTDistribution_t's
 * "name" attribute is set.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribStudentTDistribution_t's "name" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_isSetName(const DistribStudentTDistribution_t *
  dstd);


/**
 * Sets the value of the "id" attribute of this DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling DistribStudentTDistribution_unsetId().
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_setId(DistribStudentTDistribution_t * dstd,
                                  const char * id);


/**
 * Sets the value of the "name" attribute of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling DistribStudentTDistribution_unsetName().
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_setName(DistribStudentTDistribution_t * dstd,
                                    const char * name);


/**
 * Unsets the value of the "id" attribute of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_unsetId(DistribStudentTDistribution_t * dstd);


/**
 * Unsets the value of the "name" attribute of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_unsetName(DistribStudentTDistribution_t * dstd);


/**
 * Returns the value of the "location" element of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure whose location is
 * sought.
 *
 * @return the value of the "location" element of this
 * DistribStudentTDistribution_t as a DistribUncertValue*.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribStudentTDistribution_getLocation(const DistribStudentTDistribution_t *
  dstd);


/**
 * Returns the value of the "scale" element of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure whose scale is
 * sought.
 *
 * @return the value of the "scale" element of this
 * DistribStudentTDistribution_t as a DistribUncertValue*.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribStudentTDistribution_getScale(const DistribStudentTDistribution_t *
  dstd);


/**
 * Returns the value of the "degreesOfFreedom" element of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure whose
 * degreesOfFreedom is sought.
 *
 * @return the value of the "degreesOfFreedom" element of this
 * DistribStudentTDistribution_t as a DistribUncertValue*.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
const DistribUncertValue_t*
DistribStudentTDistribution_getDegreesOfFreedom(const
  DistribStudentTDistribution_t * dstd);


/**
 * Predicate returning @c 1 (true) if this DistribStudentTDistribution_t's
 * "location" element is set.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribStudentTDistribution_t's "location"
 * element has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_isSetLocation(const DistribStudentTDistribution_t *
  dstd);


/**
 * Predicate returning @c 1 (true) if this DistribStudentTDistribution_t's
 * "scale" element is set.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribStudentTDistribution_t's "scale" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_isSetScale(const DistribStudentTDistribution_t *
  dstd);


/**
 * Predicate returning @c 1 (true) if this DistribStudentTDistribution_t's
 * "degreesOfFreedom" element is set.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @return @c 1 (true) if this DistribStudentTDistribution_t's
 * "degreesOfFreedom" element has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_isSetDegreesOfFreedom(const
  DistribStudentTDistribution_t * dstd);


/**
 * Sets the value of the "location" element of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @param location DistribUncertValue_t* value of the "location" element to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_setLocation(DistribStudentTDistribution_t * dstd,
                                        const DistribUncertValue_t* location);


/**
 * Sets the value of the "scale" element of this DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @param scale DistribUncertValue_t* value of the "scale" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_setScale(DistribStudentTDistribution_t * dstd,
                                     const DistribUncertValue_t* scale);


/**
 * Sets the value of the "degreesOfFreedom" element of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @param degreesOfFreedom DistribUncertValue_t* value of the
 * "degreesOfFreedom" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_setDegreesOfFreedom(
                                                DistribStudentTDistribution_t *
                                                  dstd,
                                                const DistribUncertValue_t*
                                                  degreesOfFreedom);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribStudentTDistribution_t object and returns the DistribUncertValue_t
 * object created.
 *
 * @param dstd the DistribStudentTDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribStudentTDistribution_createLocation(DistribStudentTDistribution_t*
  dstd);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribStudentTDistribution_t object and returns the DistribUncertValue_t
 * object created.
 *
 * @param dstd the DistribStudentTDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribStudentTDistribution_createScale(DistribStudentTDistribution_t* dstd);


/**
 * Creates a new DistribUncertValue_t object, adds it to this
 * DistribStudentTDistribution_t object and returns the DistribUncertValue_t
 * object created.
 *
 * @param dstd the DistribStudentTDistribution_t structure to which the
 * DistribUncertValue_t should be added.
 *
 * @return a new DistribUncertValue_t object instance.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
DistribUncertValue_t*
DistribStudentTDistribution_createDegreesOfFreedom(DistribStudentTDistribution_t*
  dstd);


/**
 * Unsets the value of the "location" element of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_unsetLocation(DistribStudentTDistribution_t *
  dstd);


/**
 * Unsets the value of the "scale" element of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_unsetScale(DistribStudentTDistribution_t * dstd);


/**
 * Unsets the value of the "degreesOfFreedom" element of this
 * DistribStudentTDistribution_t.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_unsetDegreesOfFreedom(DistribStudentTDistribution_t
  * dstd);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DistribStudentTDistribution_t object have been set.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DistribStudentTDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_hasRequiredAttributes(const
  DistribStudentTDistribution_t * dstd);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * DistribStudentTDistribution_t object have been set.
 *
 * @param dstd the DistribStudentTDistribution_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * DistribStudentTDistribution_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required elements for the DistribStudentTDistribution_t object
 * are:
 * @li "location"
 * @li "scale"
 * @li "degreesOfFreedom"
 *
 * @memberof DistribStudentTDistribution_t
 */
LIBSBML_EXTERN
int
DistribStudentTDistribution_hasRequiredElements(const
  DistribStudentTDistribution_t * dstd);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribStudentTDistribution_H__ */


