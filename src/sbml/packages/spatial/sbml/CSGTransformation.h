/**
 * @file CSGTransformation.h
 * @brief Definition of the CSGTransformation class.
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
 * @class CSGTransformation
 * @sbmlbrief{spatial} TODO:Definition of the CSGTransformation class.
 */


#ifndef CSGTransformation_H__
#define CSGTransformation_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/CSGNode.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class CSGTranslation;
class CSGRotation;
class CSGScale;
class CSGHomogeneousTransformation;

class LIBSBML_EXTERN CSGTransformation : public CSGNode
{
protected:

  /** @cond doxygenLibsbmlInternal */

  CSGNode* mCSGNode;
  std::string mElementName;

  /** @endcond */

public:

  /**
   * Creates a new CSGTransformation using the given SBML Level, Version and
   * &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * CSGTransformation.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CSGTransformation.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CSGTransformation.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGTransformation(unsigned int level = SpatialExtension::getDefaultLevel(),
                    unsigned int version =
                      SpatialExtension::getDefaultVersion(),
                    unsigned int pkgVersion =
                      SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGTransformation using the given SpatialPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGTransformation(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CSGTransformation.
   *
   * @param orig the CSGTransformation instance to copy.
   */
  CSGTransformation(const CSGTransformation& orig);


  /**
   * Assignment operator for CSGTransformation.
   *
   * @param rhs the CSGTransformation object whose values are to be used as the
   * basis of the assignment.
   */
  CSGTransformation& operator=(const CSGTransformation& rhs);


  /**
   * Creates and returns a deep copy of this CSGTransformation object.
   *
   * @return a (deep) copy of this CSGTransformation object.
   */
  virtual CSGTransformation* clone() const;


  /**
   * Destructor for CSGTransformation.
   */
  virtual ~CSGTransformation();


  /**
   * Returns the value of the "csgNode" element of this CSGTransformation.
   *
   * @return the value of the "csgNode" element of this CSGTransformation as a
   * CSGNode*.
   */
  const CSGNode* getCSGNode() const;


  /**
   * Returns the value of the "csgNode" element of this CSGTransformation.
   *
   * @return the value of the "csgNode" element of this CSGTransformation as a
   * CSGNode*.
   */
  CSGNode* getCSGNode();


  /**
   * Predicate returning @c true if this CSGTransformation's "csgNode" element
   * is set.
   *
   * @return @c true if this CSGTransformation's "csgNode" element has been
   * set, otherwise @c false is returned.
   */
  bool isSetCSGNode() const;


  /**
   * Sets the value of the "csgNode" element of this CSGTransformation.
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
   * Creates a new CSGPrimitive object, adds it to this CSGTransformation
   * object and returns the CSGPrimitive object created.
   *
   * @return a new CSGPrimitive object instance.
   */
  CSGPrimitive* createCSGPrimitive();


  /**
   * Creates a new CSGTranslation object, adds it to this CSGTransformation
   * object and returns the CSGTranslation object created.
   *
   * @return a new CSGTranslation object instance.
   */
  CSGTranslation* createCSGTranslation();


  /**
   * Creates a new CSGRotation object, adds it to this CSGTransformation object
   * and returns the CSGRotation object created.
   *
   * @return a new CSGRotation object instance.
   */
  CSGRotation* createCSGRotation();


  /**
   * Creates a new CSGScale object, adds it to this CSGTransformation object
   * and returns the CSGScale object created.
   *
   * @return a new CSGScale object instance.
   */
  CSGScale* createCSGScale();


  /**
   * Creates a new CSGHomogeneousTransformation object, adds it to this
   * CSGTransformation object and returns the CSGHomogeneousTransformation
   * object created.
   *
   * @return a new CSGHomogeneousTransformation object instance.
   */
  CSGHomogeneousTransformation* createCSGHomogeneousTransformation();


  /**
   * Creates a new CSGSetOperator object, adds it to this CSGTransformation
   * object and returns the CSGSetOperator object created.
   *
   * @return a new CSGSetOperator object instance.
   */
  CSGSetOperator* createCSGSetOperator();


  /**
   * Unsets the value of the "csgNode" element of this CSGTransformation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetCSGNode();


  /**
   * Predicate returning @c true if this abstract "CSGTransformation" is of
   * type CSGTranslation
   *
   * @return @c true if this abstract "CSGTransformation" is of type
   * CSGTranslation, @c false otherwise
   */
  virtual bool isCSGTranslation() const;


  /**
   * Predicate returning @c true if this abstract "CSGTransformation" is of
   * type CSGRotation
   *
   * @return @c true if this abstract "CSGTransformation" is of type
   * CSGRotation, @c false otherwise
   */
  virtual bool isCSGRotation() const;


  /**
   * Predicate returning @c true if this abstract "CSGTransformation" is of
   * type CSGScale
   *
   * @return @c true if this abstract "CSGTransformation" is of type CSGScale,
   * @c false otherwise
   */
  virtual bool isCSGScale() const;


  /**
   * Predicate returning @c true if this abstract "CSGTransformation" is of
   * type CSGHomogeneousTransformation
   *
   * @return @c true if this abstract "CSGTransformation" is of type
   * CSGHomogeneousTransformation, @c false otherwise
   */
  virtual bool isCSGHomogeneousTransformation() const;


  /**
   * Returns the XML element name of this CSGTransformation object.
   *
   * For CSGTransformation, the XML element name is always
   * @c "csgTransformation".
   *
   * @return the name of this element, i.e. @c "csgTransformation".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this CSGTransformation object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this CSGTransformation object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_CSGTRANSFORMATION, SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CSGTransformation object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CSGTransformation have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * CSGTransformation object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * CSGTransformation have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the CSGTransformation object are:
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
   * Gets the value of the "attributeName" attribute of this CSGTransformation.
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
   * Gets the value of the "attributeName" attribute of this CSGTransformation.
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
   * Gets the value of the "attributeName" attribute of this CSGTransformation.
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
   * Gets the value of the "attributeName" attribute of this CSGTransformation.
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
   * Gets the value of the "attributeName" attribute of this CSGTransformation.
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
   * Predicate returning @c true if this CSGTransformation's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CSGTransformation's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this CSGTransformation.
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
   * Sets the value of the "attributeName" attribute of this CSGTransformation.
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
   * Sets the value of the "attributeName" attribute of this CSGTransformation.
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
   * Sets the value of the "attributeName" attribute of this CSGTransformation.
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
   * Sets the value of the "attributeName" attribute of this CSGTransformation.
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
   * CSGTransformation.
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
   * Creates and returns an new "elementName" object in this CSGTransformation.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this CSGTransformation.
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
   * CSGTransformation.
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
   * Returns the number of "elementName" in this CSGTransformation.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this CSGTransformation.
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
 * Creates a new CSGTransformation_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * CSGTransformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CSGTransformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CSGTransformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
CSGTransformation_t *
CSGTransformation_create(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this CSGTransformation_t object.
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @return a (deep) copy of this CSGTransformation_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
CSGTransformation_t*
CSGTransformation_clone(const CSGTransformation_t* csgt);


/**
 * Frees this CSGTransformation_t object.
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
void
CSGTransformation_free(CSGTransformation_t* csgt);


/**
 * Returns the value of the "csgNode" element of this CSGTransformation_t.
 *
 * @param csgt the CSGTransformation_t structure whose csgNode is sought.
 *
 * @return the value of the "csgNode" element of this CSGTransformation_t as a
 * CSGNode*.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
const CSGNode_t*
CSGTransformation_getCSGNode(const CSGTransformation_t * csgt);


/**
 * Predicate returning @c 1 (true) if this CSGTransformation_t's "csgNode"
 * element is set.
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @return @c 1 (true) if this CSGTransformation_t's "csgNode" element has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_isSetCSGNode(const CSGTransformation_t * csgt);


/**
 * Sets the value of the "csgNode" element of this CSGTransformation_t.
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @param csgNode CSGNode_t* value of the "csgNode" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_setCSGNode(CSGTransformation_t * csgt,
                             const CSGNode_t* csgNode);


/**
 * Creates a new CSGPrimitive_t object, adds it to this CSGTransformation_t
 * object and returns the CSGPrimitive_t object created.
 *
 * @param csgt the CSGTransformation_t structure to which the CSGPrimitive_t
 * should be added.
 *
 * @return a new CSGPrimitive_t object instance.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
CSGPrimitive_t*
CSGTransformation_createCSGPrimitive(CSGTransformation_t* csgt);


/**
 * Creates a new CSGTranslation_t object, adds it to this CSGTransformation_t
 * object and returns the CSGTranslation_t object created.
 *
 * @param csgt the CSGTransformation_t structure to which the CSGTranslation_t
 * should be added.
 *
 * @return a new CSGTranslation_t object instance.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
CSGTranslation_t*
CSGTransformation_createCSGTranslation(CSGTransformation_t* csgt);


/**
 * Creates a new CSGRotation_t object, adds it to this CSGTransformation_t
 * object and returns the CSGRotation_t object created.
 *
 * @param csgt the CSGTransformation_t structure to which the CSGRotation_t
 * should be added.
 *
 * @return a new CSGRotation_t object instance.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
CSGRotation_t*
CSGTransformation_createCSGRotation(CSGTransformation_t* csgt);


/**
 * Creates a new CSGScale_t object, adds it to this CSGTransformation_t object
 * and returns the CSGScale_t object created.
 *
 * @param csgt the CSGTransformation_t structure to which the CSGScale_t should
 * be added.
 *
 * @return a new CSGScale_t object instance.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
CSGScale_t*
CSGTransformation_createCSGScale(CSGTransformation_t* csgt);


/**
 * Creates a new CSGHomogeneousTransformation_t object, adds it to this
 * CSGTransformation_t object and returns the CSGHomogeneousTransformation_t
 * object created.
 *
 * @param csgt the CSGTransformation_t structure to which the
 * CSGHomogeneousTransformation_t should be added.
 *
 * @return a new CSGHomogeneousTransformation_t object instance.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t*
CSGTransformation_createCSGHomogeneousTransformation(CSGTransformation_t*
  csgt);


/**
 * Creates a new CSGSetOperator_t object, adds it to this CSGTransformation_t
 * object and returns the CSGSetOperator_t object created.
 *
 * @param csgt the CSGTransformation_t structure to which the CSGSetOperator_t
 * should be added.
 *
 * @return a new CSGSetOperator_t object instance.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
CSGSetOperator_t*
CSGTransformation_createCSGSetOperator(CSGTransformation_t* csgt);


/**
 * Unsets the value of the "csgNode" element of this CSGTransformation_t.
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_unsetCSGNode(CSGTransformation_t * csgt);


/**
 * Predicate returning @c 1 if this CSGTransformation_t is of type
 * CSGTranslation_t
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @return @c 1 if this CSGTransformation_t is of type CSGTranslation_t, @c 0
 * otherwise
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_isCSGTranslation(const CSGTransformation_t * csgt);


/**
 * Predicate returning @c 1 if this CSGTransformation_t is of type
 * CSGRotation_t
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @return @c 1 if this CSGTransformation_t is of type CSGRotation_t, @c 0
 * otherwise
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_isCSGRotation(const CSGTransformation_t * csgt);


/**
 * Predicate returning @c 1 if this CSGTransformation_t is of type CSGScale_t
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @return @c 1 if this CSGTransformation_t is of type CSGScale_t, @c 0
 * otherwise
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_isCSGScale(const CSGTransformation_t * csgt);


/**
 * Predicate returning @c 1 if this CSGTransformation_t is of type
 * CSGHomogeneousTransformation_t
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @return @c 1 if this CSGTransformation_t is of type
 * CSGHomogeneousTransformation_t, @c 0 otherwise
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_isCSGHomogeneousTransformation(const CSGTransformation_t *
  csgt);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGTransformation_t object have been set.
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CSGTransformation_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_hasRequiredAttributes(const CSGTransformation_t * csgt);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * CSGTransformation_t object have been set.
 *
 * @param csgt the CSGTransformation_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * CSGTransformation_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the CSGTransformation_t object are:
 *
 * @memberof CSGTransformation_t
 */
LIBSBML_EXTERN
int
CSGTransformation_hasRequiredElements(const CSGTransformation_t * csgt);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CSGTransformation_H__ */


