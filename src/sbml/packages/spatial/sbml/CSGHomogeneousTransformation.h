/**
 * @file CSGHomogeneousTransformation.h
 * @brief Definition of the CSGHomogeneousTransformation class.
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
 * @class CSGHomogeneousTransformation
 * @sbmlbrief{spatial} TODO:Definition of the CSGHomogeneousTransformation
 * class.
 */


#ifndef CSGHomogeneousTransformation_H__
#define CSGHomogeneousTransformation_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/spatial/sbml/CSGTransformation.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>
#include <sbml/packages/spatial/sbml/TransformationComponent.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CSGHomogeneousTransformation : public CSGTransformation
{
protected:

  /** @cond doxygenLibsbmlInternal */

  TransformationComponent* mForwardTransformation;

  /** @endcond */

public:

  /**
   * Creates a new CSGHomogeneousTransformation using the given SBML Level,
   * Version and &ldquo;spatial&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * CSGHomogeneousTransformation.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * CSGHomogeneousTransformation.
   *
   * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
   * this CSGHomogeneousTransformation.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGHomogeneousTransformation(
                               unsigned int level =
                                 SpatialExtension::getDefaultLevel(),
                               unsigned int version =
                                 SpatialExtension::getDefaultVersion(),
                               unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new CSGHomogeneousTransformation using the given
   * SpatialPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param spatialns the SpatialPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  CSGHomogeneousTransformation(SpatialPkgNamespaces *spatialns);


  /**
   * Copy constructor for CSGHomogeneousTransformation.
   *
   * @param orig the CSGHomogeneousTransformation instance to copy.
   */
  CSGHomogeneousTransformation(const CSGHomogeneousTransformation& orig);


  /**
   * Assignment operator for CSGHomogeneousTransformation.
   *
   * @param rhs the CSGHomogeneousTransformation object whose values are to be
   * used as the basis of the assignment.
   */
  CSGHomogeneousTransformation& operator=(const CSGHomogeneousTransformation&
    rhs);


  /**
   * Creates and returns a deep copy of this CSGHomogeneousTransformation
   * object.
   *
   * @return a (deep) copy of this CSGHomogeneousTransformation object.
   */
  virtual CSGHomogeneousTransformation* clone() const;


  /**
   * Destructor for CSGHomogeneousTransformation.
   */
  virtual ~CSGHomogeneousTransformation();


  /**
   * Returns the value of the "forwardTransformation" element of this
   * CSGHomogeneousTransformation.
   *
   * @return the value of the "forwardTransformation" element of this
   * CSGHomogeneousTransformation as a TransformationComponent*.
   */
  const TransformationComponent* getForwardTransformation() const;


  /**
   * Returns the value of the "forwardTransformation" element of this
   * CSGHomogeneousTransformation.
   *
   * @return the value of the "forwardTransformation" element of this
   * CSGHomogeneousTransformation as a TransformationComponent*.
   */
  TransformationComponent* getForwardTransformation();


  /**
   * Predicate returning @c true if this CSGHomogeneousTransformation's
   * "forwardTransformation" element is set.
   *
   * @return @c true if this CSGHomogeneousTransformation's
   * "forwardTransformation" element has been set, otherwise @c false is
   * returned.
   */
  bool isSetForwardTransformation() const;


  /**
   * Sets the value of the "forwardTransformation" element of this
   * CSGHomogeneousTransformation.
   *
   * @param forwardTransformation TransformationComponent* value of the
   * "forwardTransformation" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setForwardTransformation(const TransformationComponent*
    forwardTransformation);


  /**
   * Creates a new TransformationComponent object, adds it to this
   * CSGHomogeneousTransformation object and returns the
   * TransformationComponent object created.
   *
   * @return a new TransformationComponent object instance.
   */
  TransformationComponent* createForwardTransformation();


  /**
   * Unsets the value of the "forwardTransformation" element of this
   * CSGHomogeneousTransformation.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetForwardTransformation();


  /**
   * Returns the XML element name of this CSGHomogeneousTransformation object.
   *
   * For CSGHomogeneousTransformation, the XML element name is always
   * @c "csgHomogeneousTransformation".
   *
   * @return the name of this element, i.e. @c "csgHomogeneousTransformation".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this CSGHomogeneousTransformation
   * object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION,
   * SBMLSpatialTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * CSGHomogeneousTransformation object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * CSGHomogeneousTransformation have been set, otherwise @c false is
   * returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * CSGHomogeneousTransformation object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * CSGHomogeneousTransformation have been set, otherwise @c false is
   * returned.
   *
   *
   * @note The required elements for the CSGHomogeneousTransformation object
   * are:
   * @li "forwardTransformation"
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
   * CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
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
   * Predicate returning @c true if this CSGHomogeneousTransformation's
   * attribute "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this CSGHomogeneousTransformation's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
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
   * Returns the number of "elementName" in this CSGHomogeneousTransformation.
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
   * CSGHomogeneousTransformation.
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
 * Creates a new CSGHomogeneousTransformation_t using the given SBML Level,
 * Version and &ldquo;spatial&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * CSGHomogeneousTransformation_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * CSGHomogeneousTransformation_t.
 *
 * @param pkgVersion an unsigned int, the SBML Spatial Version to assign to
 * this CSGHomogeneousTransformation_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t *
CSGHomogeneousTransformation_create(unsigned int level,
                                    unsigned int version,
                                    unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this CSGHomogeneousTransformation_t
 * object.
 *
 * @param csght the CSGHomogeneousTransformation_t structure.
 *
 * @return a (deep) copy of this CSGHomogeneousTransformation_t object.
 *
 * @copydetails doc_returned_owned_pointer
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
CSGHomogeneousTransformation_t*
CSGHomogeneousTransformation_clone(const CSGHomogeneousTransformation_t*
  csght);


/**
 * Frees this CSGHomogeneousTransformation_t object.
 *
 * @param csght the CSGHomogeneousTransformation_t structure.
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
void
CSGHomogeneousTransformation_free(CSGHomogeneousTransformation_t* csght);


/**
 * Returns the value of the "forwardTransformation" element of this
 * CSGHomogeneousTransformation_t.
 *
 * @param csght the CSGHomogeneousTransformation_t structure whose
 * forwardTransformation is sought.
 *
 * @return the value of the "forwardTransformation" element of this
 * CSGHomogeneousTransformation_t as a TransformationComponent*.
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
const TransformationComponent_t*
CSGHomogeneousTransformation_getForwardTransformation(const
  CSGHomogeneousTransformation_t * csght);


/**
 * Predicate returning @c 1 (true) if this CSGHomogeneousTransformation_t's
 * "forwardTransformation" element is set.
 *
 * @param csght the CSGHomogeneousTransformation_t structure.
 *
 * @return @c 1 (true) if this CSGHomogeneousTransformation_t's
 * "forwardTransformation" element has been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_isSetForwardTransformation(const
  CSGHomogeneousTransformation_t * csght);


/**
 * Sets the value of the "forwardTransformation" element of this
 * CSGHomogeneousTransformation_t.
 *
 * @param csght the CSGHomogeneousTransformation_t structure.
 *
 * @param forwardTransformation TransformationComponent_t* value of the
 * "forwardTransformation" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_setForwardTransformation(
                                                      CSGHomogeneousTransformation_t
                                                        * csght,
                                                      const TransformationComponent_t*
                                                        forwardTransformation);


/**
 * Creates a new TransformationComponent_t object, adds it to this
 * CSGHomogeneousTransformation_t object and returns the
 * TransformationComponent_t object created.
 *
 * @param csght the CSGHomogeneousTransformation_t structure to which the
 * TransformationComponent_t should be added.
 *
 * @return a new TransformationComponent_t object instance.
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
TransformationComponent_t*
CSGHomogeneousTransformation_createForwardTransformation(CSGHomogeneousTransformation_t*
  csght);


/**
 * Unsets the value of the "forwardTransformation" element of this
 * CSGHomogeneousTransformation_t.
 *
 * @param csght the CSGHomogeneousTransformation_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_unsetForwardTransformation(CSGHomogeneousTransformation_t
  * csght);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGHomogeneousTransformation_t object have been set.
 *
 * @param csght the CSGHomogeneousTransformation_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * CSGHomogeneousTransformation_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_hasRequiredAttributes(const
  CSGHomogeneousTransformation_t * csght);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * CSGHomogeneousTransformation_t object have been set.
 *
 * @param csght the CSGHomogeneousTransformation_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * CSGHomogeneousTransformation_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required elements for the CSGHomogeneousTransformation_t object
 * are:
 * @li "forwardTransformation"
 *
 * @memberof CSGHomogeneousTransformation_t
 */
LIBSBML_EXTERN
int
CSGHomogeneousTransformation_hasRequiredElements(const
  CSGHomogeneousTransformation_t * csght);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !CSGHomogeneousTransformation_H__ */


