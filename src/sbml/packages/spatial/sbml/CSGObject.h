/**
 * @file CSGObject.h
 * @brief Definition of the CSGObject class.
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
 * @class CSGObject
 * @sbmlbrief{spatial} TODO:Definition of the CSGObject class.
 */


#ifndef CSGObject_H__
#define CSGObject_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/CSGNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGObject : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mDomainType;
  int mOrdinal;
  bool mIsSetOrdinal;
  CSGNode* mCSGNode;

  /** @endcond */

public:

  /**
   * Creates a new CSGObject using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this CSGObject.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CSGObject.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CSGObject.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGObject(unsigned int level = SpatialExtension::getDefaultLevel(),
            unsigned int version = SpatialExtension::getDefaultVersion(),
            unsigned int pkgVersion =
              SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGObject using the given SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGObject(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CSGObject.
   *
   * @param orig the CSGObject instance to copy.
   */
  CSGObject(const CSGObject& orig);


  /**
   * Assignment operator for CSGObject.
   *
   * @param rhs the CSGObject object whose values are to be used as the basis
   * of the assignment.
   */
  CSGObject& operator=(const CSGObject& rhs);


  /**
   * Creates and returns a deep copy of this CSGObject object.
   *
   * @return a (deep) copy of this CSGObject object.
   */
  virtual CSGObject* clone() const;


  /**
   * Destructor for CSGObject.
   */
  virtual ~CSGObject();


  /**
   * Returns the value of the "id" attribute of this CSGObject.
   *
   * @return the value of the "id" attribute of this CSGObject as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this CSGObject.
   *
   * @return the value of the "name" attribute of this CSGObject as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns the value of the "domainType" attribute of this CSGObject.
   *
   * @return the value of the "domainType" attribute of this CSGObject as a
   * string.
   */
  const std::string& getDomainType() const;


  /**
   * Returns the value of the "ordinal" attribute of this CSGObject.
   *
   * @return the value of the "ordinal" attribute of this CSGObject as a
   * integer.
   */
  int getOrdinal() const;


  /**
   * Predicate returning @c true if this CSGObject's "id" attribute is set.
   *
   * @return @c true if this CSGObject's "id" attribute has been set, otherwise
   * @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this CSGObject's "name" attribute is set.
   *
   * @return @c true if this CSGObject's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Predicate returning @c true if this CSGObject's "domainType" attribute is
   * set.
   *
   * @return @c true if this CSGObject's "domainType" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetDomainType() const;


  /**
   * Predicate returning @c true if this CSGObject's "ordinal" attribute is
   * set.
   *
   * @return @c true if this CSGObject's "ordinal" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetOrdinal() const;


  /**
   * Sets the value of the "id" attribute of this CSGObject.
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
   * Sets the value of the "name" attribute of this CSGObject.
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
   * Sets the value of the "domainType" attribute of this CSGObject.
   *
   * @param domainType std::string& value of the "domainType" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDomainType(const std::string& domainType);


  /**
   * Sets the value of the "ordinal" attribute of this CSGObject.
   *
   * @param ordinal int value of the "ordinal" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setOrdinal(int ordinal);


  /**
   * Unsets the value of the "id" attribute of this CSGObject.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this CSGObject.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Unsets the value of the "domainType" attribute of this CSGObject.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDomainType();


  /**
   * Unsets the value of the "ordinal" attribute of this CSGObject.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetOrdinal();


  /**
   * Returns the value of the "csgNode" element of this CSGObject.
   *
   * @return the value of the "csgNode" element of this CSGObject as a
   * CSGNode*.
   */
  const CSGNode* getCSGNode() const;


  /**
   * Returns the value of the "csgNode" element of this CSGObject.
   *
   * @return the value of the "csgNode" element of this CSGObject as a
   * CSGNode*.
   */
  CSGNode* getCSGNode();


  /**
   * Predicate returning @c true if this CSGObject's "csgNode" element is set.
   *
   * @return @c true if this CSGObject's "csgNode" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetCSGNode() const;


  /**
   * Sets the value of the "csgNode" element of this CSGObject.
   *
   * @param csgNode CSGNode* value of the "csgNode" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setCSGNode(const CSGNode* csgNode);


  /**
   * Creates a new CSGPrimitive object, adds it to this CSGObject object and
   * returns the CSGPrimitive object created.
   *
   * @return a new CSGPrimitive object instance.
   */
  CSGPrimitive* createCSGPrimitive();


  /**
   * Creates a new CSGTranslation object, adds it to this CSGObject object and
   * returns the CSGTranslation object created.
   *
   * @return a new CSGTranslation object instance.
   */
  CSGTranslation* createCSGTranslation();


  /**
   * Creates a new CSGRotation object, adds it to this CSGObject object and
   * returns the CSGRotation object created.
   *
   * @return a new CSGRotation object instance.
   */
  CSGRotation* createCSGRotation();


  /**
   * Creates a new CSGScale object, adds it to this CSGObject object and
   * returns the CSGScale object created.
   *
   * @return a new CSGScale object instance.
   */
  CSGScale* createCSGScale();


  /**
   * Creates a new CSGHomogeneousTransformation object, adds it to this
   * CSGObject object and returns the CSGHomogeneousTransformation object
   * created.
   *
   * @return a new CSGHomogeneousTransformation object instance.
   */
  CSGHomogeneousTransformation* createCSGHomogeneousTransformation();


  /**
   * Creates a new CSGSetOperator object, adds it to this CSGObject object and
   * returns the CSGSetOperator object created.
   *
   * @return a new CSGSetOperator object instance.
   */
  CSGSetOperator* createCSGSetOperator();


  /**
   * Unsets the value of the "csgNode" element of this CSGObject.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCSGNode();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this CSGObject object.
   *
   * For CSGObject, the XML element name is always @c "csgObject".
   *
   * @return the name of this element, i.e. @c "csgObject".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CSGObject object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_CSGOBJECT, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CSGObject object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CSGObject have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the CSGObject object are:
   * @li "id"
   * @li "domainType"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * CSGObject object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * CSGObject have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the CSGObject object are:
   * @li "csgNode"
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
   * Gets the value of the "attributeName" attribute of this CSGObject.
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
   * Gets the value of the "attributeName" attribute of this CSGObject.
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
   * Gets the value of the "attributeName" attribute of this CSGObject.
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
   * Gets the value of the "attributeName" attribute of this CSGObject.
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
   * Gets the value of the "attributeName" attribute of this CSGObject.
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
   * Predicate returning @c true if this CSGObject's attribute "attributeName"
   * is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CSGObject's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this CSGObject.
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
   * Sets the value of the "attributeName" attribute of this CSGObject.
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
   * Sets the value of the "attributeName" attribute of this CSGObject.
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
   * Sets the value of the "attributeName" attribute of this CSGObject.
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
   * Sets the value of the "attributeName" attribute of this CSGObject.
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
   * Unsets the value of the "attributeName" attribute of this CSGObject.
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
   * Creates and returns an new "elementName" object in this CSGObject.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this CSGObject.
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
   * CSGObject.
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
   * Returns the number of "elementName" in this CSGObject.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this CSGObject.
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
 * Creates a new CSGObject_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this CSGObject_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CSGObject_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CSGObject_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
CSGObject_t *
CSGObject_create(unsigned int level,
                 unsigned int version,
                 unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this CSGObject_t object.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return a (deep) copy of this CSGObject_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
CSGObject_t*
CSGObject_clone(const CSGObject_t* csgo);


/**
 * Frees this CSGObject_t object.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
void
CSGObject_free(CSGObject_t* csgo);


/**
 * Returns the value of the "id" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this CSGObject_t as a pointer to
 * a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
char *
CSGObject_getId(const CSGObject_t * csgo);


/**
 * Returns the value of the "name" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this CSGObject_t as a pointer
 * to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
char *
CSGObject_getName(const CSGObject_t * csgo);


/**
 * Returns the value of the "domainType" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure whose domainType is sought.
 *
 * @return the value of the "domainType" attribute of this CSGObject_t as a
 * pointer to a string.
 *
 * @copydetails doc_returned_owned_char
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
char *
CSGObject_getDomainType(const CSGObject_t * csgo);


/**
 * Returns the value of the "ordinal" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure whose ordinal is sought.
 *
 * @return the value of the "ordinal" attribute of this CSGObject_t as a
 * integer.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_getOrdinal(const CSGObject_t * csgo);


/**
 * Predicate returning @c 1 (true) if this CSGObject_t's "id" attribute is set.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return @c 1 (true) if this CSGObject_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_isSetId(const CSGObject_t * csgo);


/**
 * Predicate returning @c 1 (true) if this CSGObject_t's "name" attribute is
 * set.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return @c 1 (true) if this CSGObject_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_isSetName(const CSGObject_t * csgo);


/**
 * Predicate returning @c 1 (true) if this CSGObject_t's "domainType" attribute
 * is set.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return @c 1 (true) if this CSGObject_t's "domainType" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_isSetDomainType(const CSGObject_t * csgo);


/**
 * Predicate returning @c 1 (true) if this CSGObject_t's "ordinal" attribute is
 * set.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return @c 1 (true) if this CSGObject_t's "ordinal" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_isSetOrdinal(const CSGObject_t * csgo);


/**
 * Sets the value of the "id" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling CSGObject_unsetId().
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_setId(CSGObject_t * csgo, const char * id);


/**
 * Sets the value of the "name" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling CSGObject_unsetName().
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_setName(CSGObject_t * csgo, const char * name);


/**
 * Sets the value of the "domainType" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @param domainType const char * value of the "domainType" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_setDomainType(CSGObject_t * csgo, const char * domainType);


/**
 * Sets the value of the "ordinal" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @param ordinal int value of the "ordinal" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_setOrdinal(CSGObject_t * csgo, int ordinal);


/**
 * Unsets the value of the "id" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_unsetId(CSGObject_t * csgo);


/**
 * Unsets the value of the "name" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_unsetName(CSGObject_t * csgo);


/**
 * Unsets the value of the "domainType" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_unsetDomainType(CSGObject_t * csgo);


/**
 * Unsets the value of the "ordinal" attribute of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_unsetOrdinal(CSGObject_t * csgo);


/**
 * Returns the value of the "csgNode" element of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure whose csgNode is sought.
 *
 * @return the value of the "csgNode" element of this CSGObject_t as a
 * CSGNode*.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
const CSGNode_t*
CSGObject_getCSGNode(const CSGObject_t * csgo);


/**
 * Predicate returning @c 1 (true) if this CSGObject_t's "csgNode" element is
 * set.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return @c 1 (true) if this CSGObject_t's "csgNode" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_isSetCSGNode(const CSGObject_t * csgo);


/**
 * Sets the value of the "csgNode" element of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @param csgNode CSGNode_t* value of the "csgNode" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_setCSGNode(CSGObject_t * csgo, const CSGNode_t* csgNode);


/**
 * Creates a new CSGPrimitive_t object, adds it to this CSGObject_t object and
 * returns the CSGPrimitive_t object created.
 *
 * @param csgo the CSGObject_t structure to which the CSGPrimitive_t should be
 * added.
 *
 * @return a new CSGPrimitive_t object instance.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
CSGPrimitive_t*
CSGObject_createCSGPrimitive(CSGObject_t* csgo);


/**
 * Creates a new CSGTranslation_t object, adds it to this CSGObject_t object
 * and returns the CSGTranslation_t object created.
 *
 * @param csgo the CSGObject_t structure to which the CSGTranslation_t should
 * be added.
 *
 * @return a new CSGTranslation_t object instance.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
CSGTranslation_t*
CSGObject_createCSGTranslation(CSGObject_t* csgo);


/**
 * Creates a new CSGRotation_t object, adds it to this CSGObject_t object and
 * returns the CSGRotation_t object created.
 *
 * @param csgo the CSGObject_t structure to which the CSGRotation_t should be
 * added.
 *
 * @return a new CSGRotation_t object instance.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
CSGRotation_t*
CSGObject_createCSGRotation(CSGObject_t* csgo);


/**
 * Creates a new CSGScale_t object, adds it to this CSGObject_t object and
 * returns the CSGScale_t object created.
 *
 * @param csgo the CSGObject_t structure to which the CSGScale_t should be
 * added.
 *
 * @return a new CSGScale_t object instance.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
CSGScale_t*
CSGObject_createCSGScale(CSGObject_t* csgo);


/**
 * Creates a new CSGHomogeneousTransformation_t object, adds it to this
 * CSGObject_t object and returns the CSGHomogeneousTransformation_t object
 * created.
 *
 * @param csgo the CSGObject_t structure to which the
 * CSGHomogeneousTransformation_t should be added.
 *
 * @return a new CSGHomogeneousTransformation_t object instance.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t*
CSGObject_createCSGHomogeneousTransformation(CSGObject_t* csgo);


/**
 * Creates a new CSGSetOperator_t object, adds it to this CSGObject_t object
 * and returns the CSGSetOperator_t object created.
 *
 * @param csgo the CSGObject_t structure to which the CSGSetOperator_t should
 * be added.
 *
 * @return a new CSGSetOperator_t object instance.
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
CSGSetOperator_t*
CSGObject_createCSGSetOperator(CSGObject_t* csgo);


/**
 * Unsets the value of the "csgNode" element of this CSGObject_t.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_unsetCSGNode(CSGObject_t * csgo);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGObject_t object have been set.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CSGObject_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the CSGObject_t object are:
 * @li "id"
 * @li "domainType"
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_hasRequiredAttributes(const CSGObject_t * csgo);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * CSGObject_t object have been set.
 *
 * @param csgo the CSGObject_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * CSGObject_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the CSGObject_t object are:
 * @li "csgNode"
 *
 * @memberof CSGObject_t
 */
LIBSBML_EXTERN
int
CSGObject_hasRequiredElements(const CSGObject_t * csgo);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CSGObject_H__ */


