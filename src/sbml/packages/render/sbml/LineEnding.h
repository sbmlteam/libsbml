/**
 * @file    LineEnding.h
 * @brief   class representing line endings, e.g. arrow heads
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2011-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright 2010 Ralph Gauges
 *     Group for the modeling of biological processes 
 *     University of Heidelberg
 *     Im Neuenheimer Feld 267
 *     69120 Heidelberg
 *     Germany
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class LineEnding
 * @sbmlbrief{render} Decoration element for the start and/or end of curves.
 *
 * LineEndings in the SBML Level&nbsp;3 Render package are used to apply
 * certain decorations to the end of curves. Since many curves in layout
 * diagrams use the same decoration for the beginnings and start of a line,
 * it would be highly redundant to encode those decorations with each
 * line. Therefore, LineEnding objects can be defined which are then applied
 * to the beginning or the ends of several curve objects.
 *
 * A LineEnding contains an id by which it can be referenced from curve
 * styles, it contains a visual representation of the decoration in the form
 * of a render extension Group object and it has some attributes that define
 * the viewport and how the LineEnding is to be applied to a curve.
 *
 * A LineEnding object is only valid if it has an id, a viewport that has an
 * area which is not 0 and a valid group object.
 */

#ifndef LineEnding_H__
#define LineEnding_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>
#include <sbml/util/ElementFilter.h>

#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/GraphicalPrimitive2D.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/RenderGroup.h>
#include <sbml/packages/render/sbml/ListOfDrawables.h>
#include <sbml/packages/layout/sbml/BoundingBox.h>
#include <sbml/xml/XMLNode.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN LineEnding : public GraphicalPrimitive2D
{
protected:

  /** @cond doxygenLibsbmlInternal */

  bool mEnableRotationalMapping;
  bool mIsSetEnableRotationalMapping;
  RenderGroup* mGroup;
  BoundingBox* mBoundingBox;

  /** @endcond */

public:

  /**
   * Creates a new LineEnding using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this LineEnding.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * LineEnding.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this LineEnding.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  LineEnding(unsigned int level = RenderExtension::getDefaultLevel(),
             unsigned int version = RenderExtension::getDefaultVersion(),
             unsigned int pkgVersion =
               RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new LineEnding using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  LineEnding(RenderPkgNamespaces *renderns);


  /**
   * Creates a new LineEnding object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * LineEnding object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the LineEnding
   * object to be instantiated.
   *
   * @param l2version The version of SBML Level&nbsp;2.
   */
  LineEnding(const XMLNode& node, unsigned int l2version);


#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a LineEnding with an empty group object,
   * and a viewport with a size of 0.
   * The id is set to the given value.
   * In order to get a valid object, the group object has to be valid,
   * the group object has to have descendants other than groups and
   * the viewport has to have a positive size.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id The id for the LineEnding.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  LineEnding(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED

  /**
   * Copy constructor for LineEnding.
   *
   * @param orig the LineEnding instance to copy.
   */
  LineEnding(const LineEnding& orig);


  /**
   * Assignment operator for LineEnding.
   *
   * @param rhs the LineEnding object whose values are to be used as the basis
   * of the assignment.
   */
  LineEnding& operator=(const LineEnding& rhs);


  /**
   * Creates and returns a deep copy of this LineEnding object.
   *
   * @return a (deep) copy of this LineEnding object.
   */
  virtual LineEnding* clone() const;


  /**
   * Destructor for LineEnding.
   */
  virtual ~LineEnding();


  /**
   * Returns the value of the "id" attribute of this LineEnding.
   *
   * @return the value of the "id" attribute of this LineEnding as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "enableRotationalMapping" attribute of this
   * LineEnding.
   *
   * @return the value of the "enableRotationalMapping" attribute of this
   * LineEnding as a boolean.
   */
  bool getEnableRotationalMapping() const;


  /**
   * Returns whether rotational mapping is enabled or not.
   *
   * @return bool value that specifies if rotational mapping is 
   * enabled for the LineEnding or not.
   */
  bool getIsEnabledRotationalMapping() const;

  /**
   * Predicate returning @c true if this LineEnding's "id" attribute is set.
   *
   * @return @c true if this LineEnding's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this LineEnding's "enableRotationalMapping"
   * attribute is set.
   *
   * @return @c true if this LineEnding's "enableRotationalMapping" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetEnableRotationalMapping() const;


  /**
   * Sets the value of the "id" attribute of this LineEnding.
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
   * Sets the value of the "enableRotationalMapping" attribute of this
   * LineEnding.
   *
   * @param enableRotationalMapping bool value of the "enableRotationalMapping"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setEnableRotationalMapping(bool enableRotationalMapping);


  /**
   * Unsets the value of the "id" attribute of this LineEnding.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "enableRotationalMapping" attribute of this
   * LineEnding.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetEnableRotationalMapping();


  /**
   * Returns the value of the "group" element of this LineEnding.
   *
   * @return the value of the "group" element of this LineEnding as a
   * RenderGroup.
   */
  const RenderGroup* getGroup() const;


  /**
   * Returns the value of the "group" element of this LineEnding.
   *
   * @return the value of the "group" element of this LineEnding as a
   * RenderGroup.
   */
  RenderGroup* getGroup();


  /**
   * Returns the value of the "boundingBox" element of this LineEnding.
   *
   * @return the value of the "boundingBox" element of this LineEnding as a
   * BoundingBox.
   */
  const BoundingBox* getBoundingBox() const;


  /**
   * Returns the value of the "boundingBox" element of this LineEnding.
   *
   * @return the value of the "boundingBox" element of this LineEnding as a
   * BoundingBox.
   */
  BoundingBox* getBoundingBox();


  /**
   * Predicate returning @c true if this LineEnding's "group" element is set.
   *
   * @return @c true if this LineEnding's "group" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetGroup() const;


  /**
   * Predicate returning @c true if this LineEnding's "boundingBox" element is
   * set.
   *
   * @return @c true if this LineEnding's "boundingBox" element has been set,
   * otherwise @c false is returned.
   */
  bool isSetBoundingBox() const;


  /**
   * Sets the value of the "group" element of this LineEnding.
   *
   * @param group RenderGroup value of the "group" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setGroup(const RenderGroup* group);


  /**
   * Sets the value of the "boundingBox" element of this LineEnding.
   *
   * @param boundingBox BoundingBox value of the "boundingBox" element to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setBoundingBox(const BoundingBox* boundingBox);


  /**
   * Creates a new RenderGroup object, adds it to this LineEnding object and
   * returns the RenderGroup object created.
   *
   * @return a new RenderGroup object instance.
   */
  RenderGroup* createGroup();


  /**
   * Creates a new BoundingBox object, adds it to this LineEnding object and
   * returns the BoundingBox object created.
   *
   * @return a new BoundingBox object instance.
   */
  BoundingBox* createBoundingBox();


  /**
   * Unsets the value of the "group" element of this LineEnding.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetGroup();


  /**
   * Unsets the value of the "boundingBox" element of this LineEnding.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetBoundingBox();


  /**
   * Returns the XML element name of this LineEnding object.
   *
   * For LineEnding, the XML element name is always @c "lineEnding".
   *
   * @return the name of this element, i.e. @c "lineEnding".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this LineEnding object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_LINEENDING, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * LineEnding object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * LineEnding have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the LineEnding object are:
   * @li "id"
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements for this
   * LineEnding object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * LineEnding have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the LineEnding object are:
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




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this LineEnding.
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
   * Returns the value of the "attributeName" attribute of this LineEnding.
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
   * Returns the value of the "attributeName" attribute of this LineEnding.
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
   * Returns the value of the "attributeName" attribute of this LineEnding.
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
   * Returns the value of the "attributeName" attribute of this LineEnding.
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
   * Predicate returning @c true if this LineEnding's attribute "attributeName"
   * is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this LineEnding's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this LineEnding.
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
   * Sets the value of the "attributeName" attribute of this LineEnding.
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
   * Sets the value of the "attributeName" attribute of this LineEnding.
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
   * Sets the value of the "attributeName" attribute of this LineEnding.
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
   * Sets the value of the "attributeName" attribute of this LineEnding.
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
   * Unsets the value of the "attributeName" attribute of this LineEnding.
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
   * Creates and returns an new "elementName" object in this LineEnding.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this LineEnding.
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
   * LineEnding.
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
   * Returns the number of "elementName" in this LineEnding.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this LineEnding.
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


  /**
   * Creates an XMLNode object from this LineEnding object.
   *
   * @return the XMLNode with the XML representation for the 
   * LineEnding object.
   */
  XMLNode toXML() const;


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
 * Creates a new LineEnding_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this LineEnding_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * LineEnding_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * LineEnding_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
LineEnding_t *
LineEnding_create(unsigned int level,
                  unsigned int version,
                  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this LineEnding_t object.
 *
 * @param le the LineEnding_t structure.
 *
 * @return a (deep) copy of this LineEnding_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
LineEnding_t*
LineEnding_clone(const LineEnding_t* le);


/**
 * Frees this LineEnding_t object.
 *
 * @param le the LineEnding_t structure.
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
void
LineEnding_free(LineEnding_t* le);


/**
 * Returns the value of the "id" attribute of this LineEnding_t.
 *
 * @param le the LineEnding_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this LineEnding_t as a pointer to
 * a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
char *
LineEnding_getId(const LineEnding_t * le);


/**
 * Returns the value of the "enableRotationalMapping" attribute of this
 * LineEnding_t.
 *
 * @param le the LineEnding_t structure whose enableRotationalMapping is
 * sought.
 *
 * @return the value of the "enableRotationalMapping" attribute of this
 * LineEnding_t as a boolean.
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_getEnableRotationalMapping(const LineEnding_t * le);


/**
 * Predicate returning @c 1 (true) if this LineEnding_t's "id" attribute is
 * set.
 *
 * @param le the LineEnding_t structure.
 *
 * @return @c 1 (true) if this LineEnding_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_isSetId(const LineEnding_t * le);


/**
 * Predicate returning @c 1 (true) if this LineEnding_t's
 * "enableRotationalMapping" attribute is set.
 *
 * @param le the LineEnding_t structure.
 *
 * @return @c 1 (true) if this LineEnding_t's "enableRotationalMapping"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_isSetEnableRotationalMapping(const LineEnding_t * le);


/**
 * Sets the value of the "id" attribute of this LineEnding_t.
 *
 * @param le the LineEnding_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling LineEnding_unsetId().
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_setId(LineEnding_t * le, const char * id);


/**
 * Sets the value of the "enableRotationalMapping" attribute of this
 * LineEnding_t.
 *
 * @param le the LineEnding_t structure.
 *
 * @param enableRotationalMapping int value of the "enableRotationalMapping"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_setEnableRotationalMapping(LineEnding_t * le,
                                      int enableRotationalMapping);


/**
 * Unsets the value of the "id" attribute of this LineEnding_t.
 *
 * @param le the LineEnding_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_unsetId(LineEnding_t * le);


/**
 * Unsets the value of the "enableRotationalMapping" attribute of this
 * LineEnding_t.
 *
 * @param le the LineEnding_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_unsetEnableRotationalMapping(LineEnding_t * le);


/**
 * Returns the value of the "group" element of this LineEnding_t.
 *
 * @param le the LineEnding_t structure whose group is sought.
 *
 * @return the value of the "group" element of this LineEnding_t as a
 * RenderGroup.
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
const RenderGroup_t*
LineEnding_getGroup(const LineEnding_t * le);


/**
 * Returns the value of the "boundingBox" element of this LineEnding_t.
 *
 * @param le the LineEnding_t structure whose boundingBox is sought.
 *
 * @return the value of the "boundingBox" element of this LineEnding_t as a
 * BoundingBox.
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
const BoundingBox_t*
LineEnding_getBoundingBox(const LineEnding_t * le);


/**
 * Predicate returning @c 1 (true) if this LineEnding_t's "group" element is
 * set.
 *
 * @param le the LineEnding_t structure.
 *
 * @return @c 1 (true) if this LineEnding_t's "group" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_isSetGroup(const LineEnding_t * le);


/**
 * Predicate returning @c 1 (true) if this LineEnding_t's "boundingBox" element
 * is set.
 *
 * @param le the LineEnding_t structure.
 *
 * @return @c 1 (true) if this LineEnding_t's "boundingBox" element has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_isSetBoundingBox(const LineEnding_t * le);


/**
 * Sets the value of the "group" element of this LineEnding_t.
 *
 * @param le the LineEnding_t structure.
 *
 * @param group RenderGroup_t value of the "group" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_setGroup(LineEnding_t * le, const RenderGroup_t* group);


/**
 * Sets the value of the "boundingBox" element of this LineEnding_t.
 *
 * @param le the LineEnding_t structure.
 *
 * @param boundingBox BoundingBox_t value of the "boundingBox" element to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_setBoundingBox(LineEnding_t * le,
                          const BoundingBox_t* boundingBox);


/**
 * Creates a new RenderGroup_t object, adds it to this LineEnding_t object and
 * returns the RenderGroup_t object created.
 *
 * @param le the LineEnding_t structure to which the RenderGroup_t should be
 * added.
 *
 * @return a new RenderGroup_t object instance.
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
RenderGroup_t*
LineEnding_createGroup(LineEnding_t* le);


/**
 * Creates a new BoundingBox_t object, adds it to this LineEnding_t object and
 * returns the BoundingBox_t object created.
 *
 * @param le the LineEnding_t structure to which the BoundingBox_t should be
 * added.
 *
 * @return a new BoundingBox_t object instance.
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
BoundingBox_t*
LineEnding_createBoundingBox(LineEnding_t* le);


/**
 * Unsets the value of the "group" element of this LineEnding_t.
 *
 * @param le the LineEnding_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_unsetGroup(LineEnding_t * le);


/**
 * Unsets the value of the "boundingBox" element of this LineEnding_t.
 *
 * @param le the LineEnding_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_unsetBoundingBox(LineEnding_t * le);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * LineEnding_t object have been set.
 *
 * @param le the LineEnding_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * LineEnding_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the LineEnding_t object are:
 * @li "id"
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_hasRequiredAttributes(const LineEnding_t * le);


/**
 * Predicate returning @c 1 (true) if all the required elements for this
 * LineEnding_t object have been set.
 *
 * @param le the LineEnding_t structure.
 *
 * @return @c 1 (true) to indicate that all the required elements of this
 * LineEnding_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required elements for the LineEnding_t object are:
 *
 * @memberof LineEnding_t
 */
LIBSBML_EXTERN
int
LineEnding_hasRequiredElements(const LineEnding_t * le);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !LineEnding_H__ */


