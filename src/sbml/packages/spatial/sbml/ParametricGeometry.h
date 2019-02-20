/**
 * @file ParametricGeometry.h
 * @brief Definition of the ParametricGeometry class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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
 * @class ParametricGeometry
 * @sbmlbrief{spatial} TODO:Definition of the ParametricGeometry class.
 */


#ifndef ParametricGeometry_H__
#define ParametricGeometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/SpatialPoints.h>
#include <sbml/packages/spatial/sbml/ListOfParametricObjects.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ParametricGeometry : public GeometryDefinition
{
protected:

  /** @cond doxygenLibsbmlInternal */

  SpatialPoints* mSpatialPoints;
  ListOfParametricObjects mParametricObjects;

  /** @endcond */

public:

  /**
   * Creates a new ParametricGeometry using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ParametricGeometry.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ParametricGeometry.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this ParametricGeometry.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ParametricGeometry(unsigned int level = SpatialExtension::getDefaultLevel(),
                     unsigned int version =
                       SpatialExtension::getDefaultVersion(),
                     unsigned int pkgVersion =
                       SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ParametricGeometry using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ParametricGeometry(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for ParametricGeometry.
   *
   * @param orig the ParametricGeometry instance to copy.
   */
  ParametricGeometry(const ParametricGeometry& orig);


  /**
   * Assignment operator for ParametricGeometry.
   *
   * @param rhs the ParametricGeometry object whose values are to be used as
   * the basis of the assignment.
   */
  ParametricGeometry& operator=(const ParametricGeometry& rhs);


  /**
   * Creates and returns a deep copy of this ParametricGeometry object.
   *
   * @return a (deep) copy of this ParametricGeometry object.
   */
  virtual ParametricGeometry* clone() const;


  /**
   * Destructor for ParametricGeometry.
   */
  virtual ~ParametricGeometry();


  /**
   * Returns the value of the "spatialPoints" element of this
   * ParametricGeometry.
   *
   * @return the value of the "spatialPoints" element of this
   * ParametricGeometry as a SpatialPoints*.
   */
  const SpatialPoints* getSpatialPoints() const;


  /**
   * Returns the value of the "spatialPoints" element of this
   * ParametricGeometry.
   *
   * @return the value of the "spatialPoints" element of this
   * ParametricGeometry as a SpatialPoints*.
   */
  SpatialPoints* getSpatialPoints();


  /**
   * Predicate returning @c true if this ParametricGeometry's "spatialPoints"
   * element is set.
   *
   * @return @c true if this ParametricGeometry's "spatialPoints" element has
   * been set, otherwise @c false is returned.
   */
  bool isSetSpatialPoints() const;


  /**
   * Sets the value of the "spatialPoints" element of this ParametricGeometry.
   *
   * @param spatialPoints SpatialPoints* value of the "spatialPoints" element
   * to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSpatialPoints(const SpatialPoints* spatialPoints);


  /**
   * Creates a new SpatialPoints object, adds it to this ParametricGeometry
   * object and returns the SpatialPoints object created.
   *
   * @return a new SpatialPoints object instance.
   */
  SpatialPoints* createSpatialPoints();


  /**
   * Unsets the value of the "spatialPoints" element of this
   * ParametricGeometry.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSpatialPoints();


  /**
   * Returns the ListOfParametricObjects from this ParametricGeometry.
   *
   * @return the ListOfParametricObjects from this ParametricGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see getParametricObject(const std::string& sid)
   * @see getParametricObject(unsigned int n)
   * @see getNumParametricObjects()
   * @see removeParametricObject(const std::string& sid)
   * @see removeParametricObject(unsigned int n)
   */
  const ListOfParametricObjects* getListOfParametricObjects() const;


  /**
   * Returns the ListOfParametricObjects from this ParametricGeometry.
   *
   * @return the ListOfParametricObjects from this ParametricGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see getParametricObject(const std::string& sid)
   * @see getParametricObject(unsigned int n)
   * @see getNumParametricObjects()
   * @see removeParametricObject(const std::string& sid)
   * @see removeParametricObject(unsigned int n)
   */
  ListOfParametricObjects* getListOfParametricObjects();


  /**
   * Get a ParametricObject from the ParametricGeometry.
   *
   * @param n an unsigned int representing the index of the ParametricObject to
   * retrieve.
   *
   * @return the nth ParametricObject in the ListOfParametricObjects within
   * this ParametricGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see getParametricObject(const std::string& sid)
   * @see getNumParametricObjects()
   * @see removeParametricObject(const std::string& sid)
   * @see removeParametricObject(unsigned int n)
   */
  ParametricObject* getParametricObject(unsigned int n);


  /**
   * Get a ParametricObject from the ParametricGeometry.
   *
   * @param n an unsigned int representing the index of the ParametricObject to
   * retrieve.
   *
   * @return the nth ParametricObject in the ListOfParametricObjects within
   * this ParametricGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see getParametricObject(const std::string& sid)
   * @see getNumParametricObjects()
   * @see removeParametricObject(const std::string& sid)
   * @see removeParametricObject(unsigned int n)
   */
  const ParametricObject* getParametricObject(unsigned int n) const;


  /**
   * Get a ParametricObject from the ParametricGeometry based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the ParametricObject to
   * retrieve.
   *
   * @return the ParametricObject in the ListOfParametricObjects within this
   * ParametricGeometry with the given @p sid or @c NULL if no such
   * ParametricObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see getParametricObject(unsigned int n)
   * @see getNumParametricObjects()
   * @see removeParametricObject(const std::string& sid)
   * @see removeParametricObject(unsigned int n)
   */
  ParametricObject* getParametricObject(const std::string& sid);


  /**
   * Get a ParametricObject from the ParametricGeometry based on its
   * identifier.
   *
   * @param sid a string representing the identifier of the ParametricObject to
   * retrieve.
   *
   * @return the ParametricObject in the ListOfParametricObjects within this
   * ParametricGeometry with the given @p sid or @c NULL if no such
   * ParametricObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see getParametricObject(unsigned int n)
   * @see getNumParametricObjects()
   * @see removeParametricObject(const std::string& sid)
   * @see removeParametricObject(unsigned int n)
   */
  const ParametricObject* getParametricObject(const std::string& sid) const;


  /**
   * Get a ParametricObject from the ParametricGeometry based on the DomainType
   * to which it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * ParametricObject object to retrieve.
   *
   * @return the first ParametricObject in this ParametricGeometry based on the
   * given domainType attribute or NULL if no such ParametricObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const ParametricObject* getParametricObjectByDomainType(const std::string&
    sid) const;


  /**
   * Get a ParametricObject from the ParametricGeometry based on the DomainType
   * to which it refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * ParametricObject object to retrieve.
   *
   * @return the first ParametricObject in this ParametricGeometry based on the
   * given domainType attribute or NULL if no such ParametricObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  ParametricObject* getParametricObjectByDomainType(const std::string& sid);


  /**
   * Adds a copy of the given ParametricObject to this ParametricGeometry.
   *
   * @param po the ParametricObject object to add.
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
   * @see createParametricObject()
   * @see getParametricObject(const std::string& sid)
   * @see getParametricObject(unsigned int n)
   * @see getNumParametricObjects()
   * @see removeParametricObject(const std::string& sid)
   * @see removeParametricObject(unsigned int n)
   */
  int addParametricObject(const ParametricObject* po);


  /**
   * Get the number of ParametricObject objects in this ParametricGeometry.
   *
   * @return the number of ParametricObject objects in this ParametricGeometry.
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see getParametricObject(const std::string& sid)
   * @see getParametricObject(unsigned int n)
   * @see removeParametricObject(const std::string& sid)
   * @see removeParametricObject(unsigned int n)
   */
  unsigned int getNumParametricObjects() const;


  /**
   * Creates a new ParametricObject object, adds it to this ParametricGeometry
   * object and returns the ParametricObject object created.
   *
   * @return a new ParametricObject object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see getParametricObject(const std::string& sid)
   * @see getParametricObject(unsigned int n)
   * @see getNumParametricObjects()
   * @see removeParametricObject(const std::string& sid)
   * @see removeParametricObject(unsigned int n)
   */
  ParametricObject* createParametricObject();


  /**
   * Removes the nth ParametricObject from this ParametricGeometry and returns
   * a pointer to it.
   *
   * @param n an unsigned int representing the index of the ParametricObject to
   * remove.
   *
   * @return a pointer to the nth ParametricObject in this ParametricGeometry.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see getParametricObject(const std::string& sid)
   * @see getParametricObject(unsigned int n)
   * @see getNumParametricObjects()
   * @see removeParametricObject(const std::string& sid)
   */
  ParametricObject* removeParametricObject(unsigned int n);


  /**
   * Removes the ParametricObject from this ParametricGeometry based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the ParametricObject to
   * remove.
   *
   * @return the ParametricObject in this ParametricGeometry based on the
   * identifier or NULL if no such ParametricObject exists.
   *
   * @copydetails doc_returned_owned_pointer
   *
   * @see addParametricObject(const ParametricObject* object)
   * @see createParametricObject()
   * @see getParametricObject(const std::string& sid)
   * @see getParametricObject(unsigned int n)
   * @see getNumParametricObjects()
   * @see removeParametricObject(unsigned int n)
   */
  ParametricObject* removeParametricObject(const std::string& sid);


  /**
   * Returns the XML element name of this ParametricGeometry object.
   *
   * For ParametricGeometry, the XML element name is always
   * @c "parametricGeometry".
   *
   * @return the name of this element, i.e. @c "parametricGeometry".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ParametricGeometry object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_PARAMETRICGEOMETRY, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * ParametricGeometry object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * ParametricGeometry have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * ParametricGeometry object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * ParametricGeometry have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the ParametricGeometry object are:
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
   * ParametricGeometry.
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
   * ParametricGeometry.
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
   * ParametricGeometry.
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
   * ParametricGeometry.
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
   * ParametricGeometry.
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
   * Predicate returning @c true if this ParametricGeometry's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this ParametricGeometry's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * ParametricGeometry.
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
   * ParametricGeometry.
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
   * ParametricGeometry.
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
   * ParametricGeometry.
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
   * ParametricGeometry.
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
   * ParametricGeometry.
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
   * ParametricGeometry.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this ParametricGeometry.
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
   * ParametricGeometry.
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
   * Returns the number of "elementName" in this ParametricGeometry.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this ParametricGeometry.
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
 * Creates a new ParametricGeometry_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * ParametricGeometry_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * ParametricGeometry_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this ParametricGeometry_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
ParametricGeometry_t *
ParametricGeometry_create(unsigned int level,
                          unsigned int version,
                          unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this ParametricGeometry_t object.
 *
 * @param pg the ParametricGeometry_t structure.
 *
 * @return a (deep) copy of this ParametricGeometry_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
ParametricGeometry_t*
ParametricGeometry_clone(const ParametricGeometry_t* pg);


/**
 * Frees this ParametricGeometry_t object.
 *
 * @param pg the ParametricGeometry_t structure.
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
void
ParametricGeometry_free(ParametricGeometry_t* pg);


/**
 * Returns the value of the "spatialPoints" element of this
 * ParametricGeometry_t.
 *
 * @param pg the ParametricGeometry_t structure whose spatialPoints is sought.
 *
 * @return the value of the "spatialPoints" element of this
 * ParametricGeometry_t as a SpatialPoints*.
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
const SpatialPoints_t*
ParametricGeometry_getSpatialPoints(const ParametricGeometry_t * pg);


/**
 * Predicate returning @c 1 (true) if this ParametricGeometry_t's
 * "spatialPoints" element is set.
 *
 * @param pg the ParametricGeometry_t structure.
 *
 * @return @c 1 (true) if this ParametricGeometry_t's "spatialPoints" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
int
ParametricGeometry_isSetSpatialPoints(const ParametricGeometry_t * pg);


/**
 * Sets the value of the "spatialPoints" element of this ParametricGeometry_t.
 *
 * @param pg the ParametricGeometry_t structure.
 *
 * @param spatialPoints SpatialPoints_t* value of the "spatialPoints" element
 * to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
int
ParametricGeometry_setSpatialPoints(ParametricGeometry_t * pg,
                                    const SpatialPoints_t* spatialPoints);


/**
 * Creates a new SpatialPoints_t object, adds it to this ParametricGeometry_t
 * object and returns the SpatialPoints_t object created.
 *
 * @param pg the ParametricGeometry_t structure to which the SpatialPoints_t
 * should be added.
 *
 * @return a new SpatialPoints_t object instance.
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
SpatialPoints_t*
ParametricGeometry_createSpatialPoints(ParametricGeometry_t* pg);


/**
 * Unsets the value of the "spatialPoints" element of this
 * ParametricGeometry_t.
 *
 * @param pg the ParametricGeometry_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
int
ParametricGeometry_unsetSpatialPoints(ParametricGeometry_t * pg);


/**
 * Returns a ListOf_t * containing ParametricObject_t objects from this
 * ParametricGeometry_t.
 *
 * @param pg the ParametricGeometry_t structure whose ListOfParametricObjects
 * is sought.
 *
 * @return the ListOfParametricObjects from this ParametricGeometry_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see ParametricGeometry_addParametricObject()
 * @see ParametricGeometry_createParametricObject()
 * @see ParametricGeometry_getParametricObjectById()
 * @see ParametricGeometry_getParametricObject()
 * @see ParametricGeometry_getNumParametricObjects()
 * @see ParametricGeometry_removeParametricObjectById()
 * @see ParametricGeometry_removeParametricObject()
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
ListOf_t*
ParametricGeometry_getListOfParametricObjects(ParametricGeometry_t* pg);


/**
 * Get a ParametricObject_t from the ParametricGeometry_t.
 *
 * @param pg the ParametricGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the ParametricObject_t to
 * retrieve.
 *
 * @return the nth ParametricObject_t in the ListOfParametricObjects within
 * this ParametricGeometry.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
ParametricObject_t*
ParametricGeometry_getParametricObject(ParametricGeometry_t* pg,
                                       unsigned int n);


/**
 * Get a ParametricObject_t from the ParametricGeometry_t based on its
 * identifier.
 *
 * @param pg the ParametricGeometry_t structure to search.
 *
 * @param sid a string representing the identifier of the ParametricObject_t to
 * retrieve.
 *
 * @return the ParametricObject_t in the ListOfParametricObjects within this
 * ParametricGeometry with the given @p sid or @c NULL if no such
 * ParametricObject_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
ParametricObject_t*
ParametricGeometry_getParametricObjectById(ParametricGeometry_t* pg,
                                           const char *sid);


/**
 * Get a ParametricObject_t from the ParametricGeometry_t based on the
 * DomainType to which it refers.
 *
 * @param pg the ParametricGeometry_t structure to search.
 *
 * @param sid a string representing the "domainType" attribute of the
 * ParametricObject_t object to retrieve.
 *
 * @return the first ParametricObject_t in this ParametricGeometry_t based on
 * the given domainType attribute or NULL if no such ParametricObject_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
ParametricObject_t*
ParametricGeometry_getParametricObjectByDomainType(ParametricGeometry_t* pg,
                                                   const char *sid);


/**
 * Adds a copy of the given ParametricObject_t to this ParametricGeometry_t.
 *
 * @param pg the ParametricGeometry_t structure to which the ParametricObject_t
 * should be added.
 *
 * @param po the ParametricObject_t object to add.
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
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
int
ParametricGeometry_addParametricObject(ParametricGeometry_t* pg,
                                       const ParametricObject_t* po);


/**
 * Get the number of ParametricObject_t objects in this ParametricGeometry_t.
 *
 * @param pg the ParametricGeometry_t structure to query.
 *
 * @return the number of ParametricObject_t objects in this
 * ParametricGeometry_t.
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
unsigned int
ParametricGeometry_getNumParametricObjects(ParametricGeometry_t* pg);


/**
 * Creates a new ParametricObject_t object, adds it to this
 * ParametricGeometry_t object and returns the ParametricObject_t object
 * created.
 *
 * @param pg the ParametricGeometry_t structure to which the ParametricObject_t
 * should be added.
 *
 * @return a new ParametricObject_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
ParametricObject_t*
ParametricGeometry_createParametricObject(ParametricGeometry_t* pg);


/**
 * Removes the nth ParametricObject_t from this ParametricGeometry_t and
 * returns a pointer to it.
 *
 * @param pg the ParametricGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the ParametricObject_t to
 * remove.
 *
 * @return a pointer to the nth ParametricObject_t in this
 * ParametricGeometry_t.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
ParametricObject_t*
ParametricGeometry_removeParametricObject(ParametricGeometry_t* pg,
                                          unsigned int n);


/**
 * Removes the ParametricObject_t from this ParametricGeometry_t based on its
 * identifier and returns a pointer to it.
 *
 * @param pg the ParametricGeometry_t structure to search.
 *
 * @param sid a string representing the identifier of the ParametricObject_t to
 * remove.
 *
 * @return the ParametricObject_t in this ParametricGeometry_t based on the
 * identifier or NULL if no such ParametricObject_t exists.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
ParametricObject_t*
ParametricGeometry_removeParametricObjectById(ParametricGeometry_t* pg,
                                              const char* sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * ParametricGeometry_t object have been set.
 *
 * @param pg the ParametricGeometry_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * ParametricGeometry_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
int
ParametricGeometry_hasRequiredAttributes(const ParametricGeometry_t * pg);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * ParametricGeometry_t object have been set.
 *
 * @param pg the ParametricGeometry_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * ParametricGeometry_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the ParametricGeometry_t object are:
 *
 * @memberof ParametricGeometry_t
 */
LIBSBML_EXTERN
int
ParametricGeometry_hasRequiredElements(const ParametricGeometry_t * pg);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ParametricGeometry_H__ */


