/**
 * @file CSGeometry.h
 * @brief Definition of the CSGeometry class.
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
 * @class CSGeometry
 * @sbmlbrief{spatial} TODO:Definition of the CSGeometry class.
 */


#ifndef CSGeometry_H__
#define CSGeometry_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/GeometryDefinition.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/ListOfCSGObjects.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGeometry : public GeometryDefinition
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfCSGObjects mCSGObjects;

  /** @endcond */

public:

  /**
   * Creates a new CSGeometry using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGeometry.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CSGeometry.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CSGeometry.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGeometry(unsigned int level = SpatialExtension::getDefaultLevel(),
             unsigned int version = SpatialExtension::getDefaultVersion(),
             unsigned int pkgVersion =
               SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGeometry using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGeometry(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CSGeometry.
   *
   * @param orig the CSGeometry instance to copy.
   */
  CSGeometry(const CSGeometry& orig);


  /**
   * Assignment operator for CSGeometry.
   *
   * @param rhs the CSGeometry object whose values are to be used as the basis
   * of the assignment.
   */
  CSGeometry& operator=(const CSGeometry& rhs);


  /**
   * Creates and returns a deep copy of this CSGeometry object.
   *
   * @return a (deep) copy of this CSGeometry object.
   */
  virtual CSGeometry* clone() const;


  /**
   * Destructor for CSGeometry.
   */
  virtual ~CSGeometry();


  /**
   * Returns the ListOfCSGObjects from this CSGeometry.
   *
   * @return the ListOfCSGObjects from this CSGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see getCSGObject(const std::string& sid)
   * @see getCSGObject(unsigned int n)
   * @see getNumCSGObjects()
   * @see removeCSGObject(const std::string& sid)
   * @see removeCSGObject(unsigned int n)
   */
  const ListOfCSGObjects* getListOfCSGObjects() const;


  /**
   * Returns the ListOfCSGObjects from this CSGeometry.
   *
   * @return the ListOfCSGObjects from this CSGeometry.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see getCSGObject(const std::string& sid)
   * @see getCSGObject(unsigned int n)
   * @see getNumCSGObjects()
   * @see removeCSGObject(const std::string& sid)
   * @see removeCSGObject(unsigned int n)
   */
  ListOfCSGObjects* getListOfCSGObjects();


  /**
   * Get a CSGObject from the CSGeometry.
   *
   * @param n an unsigned int representing the index of the CSGObject to
   * retrieve.
   *
   * @return the nth CSGObject in the ListOfCSGObjects within this CSGeometry
   * or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see getCSGObject(const std::string& sid)
   * @see getNumCSGObjects()
   * @see removeCSGObject(const std::string& sid)
   * @see removeCSGObject(unsigned int n)
   */
  CSGObject* getCSGObject(unsigned int n);


  /**
   * Get a CSGObject from the CSGeometry.
   *
   * @param n an unsigned int representing the index of the CSGObject to
   * retrieve.
   *
   * @return the nth CSGObject in the ListOfCSGObjects within this CSGeometry
   * or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see getCSGObject(const std::string& sid)
   * @see getNumCSGObjects()
   * @see removeCSGObject(const std::string& sid)
   * @see removeCSGObject(unsigned int n)
   */
  const CSGObject* getCSGObject(unsigned int n) const;


  /**
   * Get a CSGObject from the CSGeometry based on its identifier.
   *
   * @param sid a string representing the identifier of the CSGObject to
   * retrieve.
   *
   * @return the CSGObject in the ListOfCSGObjects within this CSGeometry with
   * the given @p sid or @c NULL if no such CSGObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see getCSGObject(unsigned int n)
   * @see getNumCSGObjects()
   * @see removeCSGObject(const std::string& sid)
   * @see removeCSGObject(unsigned int n)
   */
  CSGObject* getCSGObject(const std::string& sid);


  /**
   * Get a CSGObject from the CSGeometry based on its identifier.
   *
   * @param sid a string representing the identifier of the CSGObject to
   * retrieve.
   *
   * @return the CSGObject in the ListOfCSGObjects within this CSGeometry with
   * the given @p sid or @c NULL if no such CSGObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see getCSGObject(unsigned int n)
   * @see getNumCSGObjects()
   * @see removeCSGObject(const std::string& sid)
   * @see removeCSGObject(unsigned int n)
   */
  const CSGObject* getCSGObject(const std::string& sid) const;


  /**
   * Get a CSGObject from the CSGeometry based on the DomainType to which it
   * refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * CSGObject object to retrieve.
   *
   * @return the first CSGObject in this CSGeometry based on the given
   * domainType attribute or NULL if no such CSGObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  const CSGObject* getCSGObjectByDomainType(const std::string& sid) const;


  /**
   * Get a CSGObject from the CSGeometry based on the DomainType to which it
   * refers.
   *
   * @param sid a string representing the "domainType" attribute of the
   * CSGObject object to retrieve.
   *
   * @return the first CSGObject in this CSGeometry based on the given
   * domainType attribute or NULL if no such CSGObject exists.
   *
   * @copydetails doc_returned_unowned_pointer
   */
  CSGObject* getCSGObjectByDomainType(const std::string& sid);


  /**
   * Adds a copy of the given CSGObject to this CSGeometry.
   *
   * @param csgo the CSGObject object to add.
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
   * @see createCSGObject()
   * @see getCSGObject(const std::string& sid)
   * @see getCSGObject(unsigned int n)
   * @see getNumCSGObjects()
   * @see removeCSGObject(const std::string& sid)
   * @see removeCSGObject(unsigned int n)
   */
  int addCSGObject(const CSGObject* csgo);


  /**
   * Get the number of CSGObject objects in this CSGeometry.
   *
   * @return the number of CSGObject objects in this CSGeometry.
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see getCSGObject(const std::string& sid)
   * @see getCSGObject(unsigned int n)
   * @see removeCSGObject(const std::string& sid)
   * @see removeCSGObject(unsigned int n)
   */
  unsigned int getNumCSGObjects() const;


  /**
   * Creates a new CSGObject object, adds it to this CSGeometry object and
   * returns the CSGObject object created.
   *
   * @return a new CSGObject object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see getCSGObject(const std::string& sid)
   * @see getCSGObject(unsigned int n)
   * @see getNumCSGObjects()
   * @see removeCSGObject(const std::string& sid)
   * @see removeCSGObject(unsigned int n)
   */
  CSGObject* createCSGObject();


  /**
   * Removes the nth CSGObject from this CSGeometry and returns a pointer to
   * it.
   *
   * @param n an unsigned int representing the index of the CSGObject to
   * remove.
   *
   * @return a pointer to the nth CSGObject in this CSGeometry.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see getCSGObject(const std::string& sid)
   * @see getCSGObject(unsigned int n)
   * @see getNumCSGObjects()
   * @see removeCSGObject(const std::string& sid)
   */
  CSGObject* removeCSGObject(unsigned int n);


  /**
   * Removes the CSGObject from this CSGeometry based on its identifier and
   * returns a pointer to it.
   *
   * @param sid a string representing the identifier of the CSGObject to
   * remove.
   *
   * @return the CSGObject in this CSGeometry based on the identifier or NULL
   * if no such CSGObject exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addCSGObject(const CSGObject* object)
   * @see createCSGObject()
   * @see getCSGObject(const std::string& sid)
   * @see getCSGObject(unsigned int n)
   * @see getNumCSGObjects()
   * @see removeCSGObject(unsigned int n)
   */
  CSGObject* removeCSGObject(const std::string& sid);


  /**
   * Returns the XML element name of this CSGeometry object.
   *
   * For CSGeometry, the XML element name is always @c "csGeometry".
   *
   * @return the name of this element, i.e. @c "csGeometry".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CSGeometry object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_CSGEOMETRY, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CSGeometry object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CSGeometry have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * CSGeometry object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * CSGeometry have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the CSGeometry object are:
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
   * Gets the value of the "attributeName" attribute of this CSGeometry.
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
   * Gets the value of the "attributeName" attribute of this CSGeometry.
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
   * Gets the value of the "attributeName" attribute of this CSGeometry.
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
   * Gets the value of the "attributeName" attribute of this CSGeometry.
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
   * Gets the value of the "attributeName" attribute of this CSGeometry.
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
   * Predicate returning @c true if this CSGeometry's attribute "attributeName"
   * is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CSGeometry's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this CSGeometry.
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
   * Sets the value of the "attributeName" attribute of this CSGeometry.
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
   * Sets the value of the "attributeName" attribute of this CSGeometry.
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
   * Sets the value of the "attributeName" attribute of this CSGeometry.
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
   * Sets the value of the "attributeName" attribute of this CSGeometry.
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
   * Unsets the value of the "attributeName" attribute of this CSGeometry.
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
   * Creates and returns an new "elementName" object in this CSGeometry.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this CSGeometry.
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
   * CSGeometry.
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
   * Returns the number of "elementName" in this CSGeometry.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this CSGeometry.
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
   * @return a List pointer of pointers to all SBase child objects with any
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
 * Creates a new CSGeometry_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this CSGeometry_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CSGeometry_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CSGeometry_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
CSGeometry_t *
CSGeometry_create(unsigned int level,
                  unsigned int version,
                  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this CSGeometry_t object.
 *
 * @param csg the CSGeometry_t structure.
 *
 * @return a (deep) copy of this CSGeometry_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
CSGeometry_t*
CSGeometry_clone(const CSGeometry_t* csg);


/**
 * Frees this CSGeometry_t object.
 *
 * @param csg the CSGeometry_t structure.
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
void
CSGeometry_free(CSGeometry_t* csg);


/**
 * Returns a ListOf_t * containing CSGObject_t objects from this CSGeometry_t.
 *
 * @param csg the CSGeometry_t structure whose ListOfCSGObjects is sought.
 *
 * @return the ListOfCSGObjects from this CSGeometry_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see CSGeometry_addCSGObject()
 * @see CSGeometry_createCSGObject()
 * @see CSGeometry_getCSGObjectById()
 * @see CSGeometry_getCSGObject()
 * @see CSGeometry_getNumCSGObjects()
 * @see CSGeometry_removeCSGObjectById()
 * @see CSGeometry_removeCSGObject()
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
ListOf_t*
CSGeometry_getListOfCSGObjects(CSGeometry_t* csg);


/**
 * Get a CSGObject_t from the CSGeometry_t.
 *
 * @param csg the CSGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the CSGObject_t to
 * retrieve.
 *
 * @return the nth CSGObject_t in the ListOfCSGObjects within this CSGeometry
 * or @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_getCSGObject(CSGeometry_t* csg, unsigned int n);


/**
 * Get a CSGObject_t from the CSGeometry_t based on its identifier.
 *
 * @param csg the CSGeometry_t structure to search.
 *
 * @param sid a string representing the identifier of the CSGObject_t to
 * retrieve.
 *
 * @return the CSGObject_t in the ListOfCSGObjects within this CSGeometry with
 * the given @p sid or @c NULL if no such CSGObject_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_getCSGObjectById(CSGeometry_t* csg, const char *sid);


/**
 * Get a CSGObject_t from the CSGeometry_t based on the DomainType to which it
 * refers.
 *
 * @param csg the CSGeometry_t structure to search.
 *
 * @param sid a string representing the "domainType" attribute of the
 * CSGObject_t object to retrieve.
 *
 * @return the first CSGObject_t in this CSGeometry_t based on the given
 * domainType attribute or NULL if no such CSGObject_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_getCSGObjectByDomainType(CSGeometry_t* csg, const char *sid);


/**
 * Adds a copy of the given CSGObject_t to this CSGeometry_t.
 *
 * @param csg the CSGeometry_t structure to which the CSGObject_t should be
 * added.
 *
 * @param csgo the CSGObject_t object to add.
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
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
int
CSGeometry_addCSGObject(CSGeometry_t* csg, const CSGObject_t* csgo);


/**
 * Get the number of CSGObject_t objects in this CSGeometry_t.
 *
 * @param csg the CSGeometry_t structure to query.
 *
 * @return the number of CSGObject_t objects in this CSGeometry_t.
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
unsigned int
CSGeometry_getNumCSGObjects(CSGeometry_t* csg);


/**
 * Creates a new CSGObject_t object, adds it to this CSGeometry_t object and
 * returns the CSGObject_t object created.
 *
 * @param csg the CSGeometry_t structure to which the CSGObject_t should be
 * added.
 *
 * @return a new CSGObject_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_createCSGObject(CSGeometry_t* csg);


/**
 * Removes the nth CSGObject_t from this CSGeometry_t and returns a pointer to
 * it.
 *
 * @param csg the CSGeometry_t structure to search.
 *
 * @param n an unsigned int representing the index of the CSGObject_t to
 * remove.
 *
 * @return a pointer to the nth CSGObject_t in this CSGeometry_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_removeCSGObject(CSGeometry_t* csg, unsigned int n);


/**
 * Removes the CSGObject_t from this CSGeometry_t based on its identifier and
 * returns a pointer to it.
 *
 * @param csg the CSGeometry_t structure to search.
 *
 * @param sid a string representing the identifier of the CSGObject_t to
 * remove.
 *
 * @return the CSGObject_t in this CSGeometry_t based on the identifier or NULL
 * if no such CSGObject_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
CSGObject_t*
CSGeometry_removeCSGObjectById(CSGeometry_t* csg, const char* sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGeometry_t object have been set.
 *
 * @param csg the CSGeometry_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CSGeometry_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
int
CSGeometry_hasRequiredAttributes(const CSGeometry_t * csg);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * CSGeometry_t object have been set.
 *
 * @param csg the CSGeometry_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * CSGeometry_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the CSGeometry_t object are:
 *
 * @memberof CSGeometry_t
 */
LIBSBML_EXTERN
int
CSGeometry_hasRequiredElements(const CSGeometry_t * csg);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CSGeometry_H__ */


