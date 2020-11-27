/**
 * @file    Style.h
 * @brief Definition of the Style class.
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
 * @class Style
 * @sbmlbrief{render} Abstract base class for local and global styles.
 *
 * Local and global styles in the SBML Level&nbsp;3 Render package have many
 * attributes and methods in common.  These have been implemented in the
 * abstract base class Style.
 *
 * A style is a graphical representation for certain layout objects. The
 * assignment of styles to individual layout objects can either be done
 * through layout object ids (local styles only), layout object types
 * (SPECIES, COMPARTMENT, etc.) or layout object roles.
 */

#ifndef Style_H__
#define Style_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>

#include <sbml/xml/XMLNode.h>

#ifdef __cplusplus

#include <set>
#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/RenderGroup.h>
#include <sbml/packages/render/sbml/ListOfDrawables.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class GlobalStyle;
class LocalStyle;

class LIBSBML_EXTERN Style : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::set<std::string> mRoleList;
  std::set<std::string> mTypeList;
  RenderGroup mGroup;

  /** @endcond */

protected:
  /** @cond doxygenLibsbmlInternal */
  /**
   * Creates a new Style using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Style.
   *
   * @param version an unsigned int, the SBML Version to assign to this Style.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this Style.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Style(unsigned int level = RenderExtension::getDefaultLevel(),
        unsigned int version = RenderExtension::getDefaultVersion(),
        unsigned int pkgVersion = RenderExtension::getDefaultPackageVersion());
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Creates a new Style using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Style(RenderPkgNamespaces *renderns);
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Creates a new Style object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * Style object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the Style
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  Style(const XMLNode& node, unsigned int l2version);
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
#ifndef OMIT_DEPRECATED
  /**
   * Constructor which creates a Style with an empty group
   * and empty role and type list.
   * The group has to be filled before the * object is valid.
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Style(RenderPkgNamespaces* renderns, const std::string& id);
#endif // OMIT_DEPRECATED
  /** @endcond */

public:

  /**
   * Copy constructor for Style.
   *
   * @param orig the Style instance to copy.
   */
  Style(const Style& orig);


  /**
   * Assignment operator for Style.
   *
   * @param rhs the Style object whose values are to be used as the basis of
   * the assignment.
   */
  Style& operator=(const Style& rhs);


  /**
   * Creates and returns a deep copy of this Style object.
   *
   * @return a (deep) copy of this Style object.
   */
  virtual Style* clone() const;


  /**
   * Destructor for Style.
   */
  virtual ~Style();


  /**
   * Returns the value of the "id" attribute of this Style.
   *
   * @return the value of the "id" attribute of this Style as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns the value of the "name" attribute of this Style.
   *
   * @return the value of the "name" attribute of this Style as a string.
   */
  virtual const std::string& getName() const;



  /**
   * Predicate returning @c true if this Style's "id" attribute is set.
   *
   * @return @c true if this Style's "id" attribute has been set, otherwise
   * @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Predicate returning @c true if this Style's "name" attribute is set.
   *
   * @return @c true if this Style's "name" attribute has been set, otherwise
   * @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "id" attribute of this Style.
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
   * Sets the value of the "name" attribute of this Style.
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
   * Unsets the value of the "id" attribute of this Style.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Unsets the value of the "name" attribute of this Style.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "roleList" attribute of this Style.
   *
   * @return the value of the "roleList" attribute of this Style as a string.
   */
  std::set<std::string>& getRoleList();


  /**
   * Returns a const reference to the role list.
   *
   * @return const reference to the role list.
   */
  const std::set<std::string>& getRoleList() const;


  /**
   * Returns the number of ids in the role list.
   *
   * @return the number of roles in the role list. 
   */
  unsigned int getNumRoles() const;


  /**
   * Checks whether a given role is in the role list.
   *
   * @param role role string to check for in the role list.
   */
  bool isInRoleList(const std::string& role) const;


  /**
   * Adds an id to the role list.
   *
   * @param role New role to be added to the role list.
   */
  int addRole(const std::string& role);


  /** 
   * @return the string of all roles
   */
  std::string createRoleString() const;


  /**
   * Removes the given role from the role list.
   *
   * @param role role string to be removed from the role list.
   */
  int removeRole(const std::string& role);


  /**
   * Sets the complete role list to a copy of the given list.
   *
   * @param roleList New list of role strings to be set on the style.
   */
  int setRoleList(const std::set<std::string>& roleList);

  /**
   * Returns the type list.
   *
   * @return const reference to the type list.
   *
   * @copydetails doc_render_style_type
   */
  const std::set<std::string>& getTypeList() const;

  /**
   * Returns the value of the "typeList" attribute of this Style.
   *
   * @return the value of the "typeList" attribute of this Style as a string.
   *
   * @copydetails doc_render_style_type
   */
  std::set<std::string>& getTypeList();


  /**
   * Returns the number of types in the type list.
   *
   * @return number of types in type list.
   */
  unsigned int getNumTypes() const;


  /**
   * Checks whether a given type string is in the type list.
   *
   * @param type string to be searched for in the type list
   *
   * @return @c true or @c false depending on whether the given string was
   * found in the type list.
   *
   * @copydetails doc_render_style_type
   */
  bool isInTypeList(const std::string& type) const;


  /**
   * Adds a type string to the type list.
   *
   * @param type new type string to be added to the type list
   */
  int addType(const std::string& type);


  /** 
   * @return the string of all types
   */
  std::string createTypeString() const;


  /**
   * Removes a type string from the type list.
   *
   * @param type type string to be removed from the type list.
   */
  int removeType(const std::string& type);


  /**
   * Sets the complete type list to a copy of the given list.
   *
   * @param typeList the list of types to be set for the style.
   *
   * @copydetails doc_render_style_type
   */
  int setTypeList(const std::set<std::string>& typeList);


  /**
   * Returns the value of the "group" element of this Style.
   *
   * @return the value of the "group" element of this Style as a RenderGroup.
   */
  const RenderGroup* getGroup() const;


  /**
   * Returns the value of the "group" element of this Style.
   *
   * @return the value of the "group" element of this Style as a RenderGroup.
   */
  RenderGroup* getGroup();


  /**
   * Predicate returning @c true if this Style's "group" element is set.
   *
   * @return @c true if this Style's "group" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetGroup() const;


  /**
   * Sets the value of the "group" element of this Style.
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
   * Creates a new RenderGroup object, adds it to this Style object and returns
   * the RenderGroup object created.
   *
   * @return a new RenderGroup object instance.
   */
  RenderGroup* createGroup();


  /**
   * Unsets the value of the "group" element of this Style.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetGroup();


  /**
   * Predicate returning @c true if this abstract Style is of type
   * GlobalStyle
   *
   * @return @c true if this abstract Style is of type GlobalStyle, @c false
   * otherwise
   */
  virtual bool isGlobalStyle() const;


  /**
   * Predicate returning @c true if this abstract Style is of type LocalStyle
   *
   * @return @c true if this abstract Style is of type LocalStyle, @c false
   * otherwise
   */
  virtual bool isLocalStyle() const;



  /**
   * Returns the XML element name of this Style object.
   *
   * For Style, the XML element name is always @c "style".
   *
   * @return the name of this element, i.e. @c "style".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Style object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_STYLE_BASE, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this Style
   * object have been set.
   *
   * @return @c true to indicate that all the required attributes of this Style
   * have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;



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
   * Returns the value of the "attributeName" attribute of this Style.
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
   * Returns the value of the "attributeName" attribute of this Style.
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
   * Returns the value of the "attributeName" attribute of this Style.
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
   * Returns the value of the "attributeName" attribute of this Style.
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
   * Returns the value of the "attributeName" attribute of this Style.
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
   * Predicate returning @c true if this Style's attribute "attributeName" is
   * set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this Style's attribute "attributeName" has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this Style.
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
   * Sets the value of the "attributeName" attribute of this Style.
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
   * Sets the value of the "attributeName" attribute of this Style.
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
   * Sets the value of the "attributeName" attribute of this Style.
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
   * Sets the value of the "attributeName" attribute of this Style.
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
   * Unsets the value of the "attributeName" attribute of this Style.
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
   * Creates and returns an new "elementName" object in this Style.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this Style.
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
   * Style.
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
   * Returns the number of "elementName" in this Style.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this Style.
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
   * Creates an XMLNode object from this Style object.
   *
   * @return the XMLNode with the XML representation for the 
   * Style object.
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


  /** @cond doxygenLibsbmlInternal */
  /**
   * Parse the list of roles into the role list.
   */
  void readListOfRoles(const XMLAttributes& attr);
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Parse the list of types into the type list.
   */
  void readListOfTypes(const XMLAttributes& attr);
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Adds the typeList attribute to an XMLAttributes object.
   */
  void addListOfRoles(XMLAttributes& attr) const;
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Adds the roleList attribute to an XMLAttributes object.
   */
  void addListOfTypes(XMLAttributes& attr) const;
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Writes the type list to an XML stream.
   */
  void writeTypeList(XMLOutputStream& stream) const;
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Writes the role list to an XML stream.
   */
  void writeRolesList(XMLOutputStream& stream) const;
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Devides a string into individual elements and stores them in the given set.
   */
  static void readIntoSet(const std::string& s,std::set<std::string>& set);
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Concatenates individual elements from a set to a string.
   * The string is returned.
   */
  static std::string createStringFromSet(const std::set<std::string>& set);
  /** @endcond */

};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new GlobalStyle (Style_t) using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Style_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this Style_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Style_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
Style_t *
Style_createGlobalStyle(unsigned int level,
                        unsigned int version,
                        unsigned int pkgVersion);


/**
 * Creates a new LocalStyle (Style_t) using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Style_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this Style_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Style_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
Style_t *
Style_createLocalStyle(unsigned int level,
                       unsigned int version,
                       unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Style_t object.
 *
 * @param s the Style_t structure.
 *
 * @return a (deep) copy of this Style_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
Style_t*
Style_clone(const Style_t* s);


/**
 * Frees this Style_t object.
 *
 * @param s the Style_t structure.
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
void
Style_free(Style_t* s);


/**
 * Returns the value of the "id" attribute of this Style_t.
 *
 * @param s the Style_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this Style_t as a pointer to a
 * string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
char *
Style_getId(const Style_t * s);


/**
 * Returns the value of the "name" attribute of this Style_t.
 *
 * @param s the Style_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this Style_t as a pointer to a
 * string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
char *
Style_getName(const Style_t * s);

//render FIX_ME
///**
// * Returns the value of the "roleList" attribute of this Style_t.
// *
// * @param s the Style_t structure whose roleList is sought.
// *
// * @return the value of the "roleList" attribute of this Style_t as a pointer
// * to a string.
// *
// * @copydetails doc_warning_returns_owned_char
// *
// * @memberof Style_t
// */
//LIBSBML_EXTERN
//char *
//Style_getRoleList(const Style_t * s);
//
//
///**
// * Returns the value of the "typeList" attribute of this Style_t.
// *
// * @param s the Style_t structure whose typeList is sought.
// *
// * @return the value of the "typeList" attribute of this Style_t as a pointer
// * to a string.
// *
// * @copydetails doc_warning_returns_owned_char
// *
// * @memberof Style_t
// */
//LIBSBML_EXTERN
//char *
//Style_getTypeList(const Style_t * s);
//
//
/**
 * Predicate returning @c 1 (true) if this Style_t's "id" attribute is set.
 *
 * @param s the Style_t structure.
 *
 * @return @c 1 (true) if this Style_t's "id" attribute has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_isSetId(const Style_t * s);


/**
 * Predicate returning @c 1 (true) if this Style_t's "name" attribute is set.
 *
 * @param s the Style_t structure.
 *
 * @return @c 1 (true) if this Style_t's "name" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_isSetName(const Style_t * s);


/**
 * Sets the value of the "id" attribute of this Style_t.
 *
 * @param s the Style_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling Style_unsetId().
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_setId(Style_t * s, const char * id);


/**
 * Sets the value of the "name" attribute of this Style_t.
 *
 * @param s the Style_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling Style_unsetName().
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_setName(Style_t * s, const char * name);


/**
 * Sets the value of the "roleList" attribute of this Style_t.
 *
 * @param s the Style_t structure.
 *
 * @param roleList const char * value of the "roleList" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p roleList = @c NULL or an empty string is
 * equivalent to calling Style_unsetRoleList().
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_setRoleList(Style_t * s, const char * roleList);


/**
 * Sets the value of the "typeList" attribute of this Style_t.
 *
 * @param s the Style_t structure.
 *
 * @param typeList const char * value of the "typeList" attribute to be set.
 *
 * @copydetails doc_render_style_type
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p typeList = @c NULL or an empty string is
 * equivalent to calling Style_unsetTypeList().
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_setTypeList(Style_t * s, const char * typeList);


/**
 * Unsets the value of the "id" attribute of this Style_t.
 *
 * @param s the Style_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_unsetId(Style_t * s);


/**
 * Unsets the value of the "name" attribute of this Style_t.
 *
 * @param s the Style_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_unsetName(Style_t * s);


/**
 * Unsets the value of the "roleList" attribute of this Style_t.
 *
 * @param s the Style_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_unsetRoleList(Style_t * s);


/**
 * Unsets the value of the "typeList" attribute of this Style_t.
 *
 * @param s the Style_t structure.
 *
 * @copydetails doc_render_style_type
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_unsetTypeList(Style_t * s);


/**
 * Returns the value of the "group" element of this Style_t.
 *
 * @param s the Style_t structure whose group is sought.
 *
 * @return the value of the "group" element of this Style_t as a RenderGroup.
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
const RenderGroup_t*
Style_getGroup(const Style_t * s);


/**
 * Predicate returning @c 1 (true) if this Style_t's "group" element is set.
 *
 * @param s the Style_t structure.
 *
 * @return @c 1 (true) if this Style_t's "group" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_isSetGroup(const Style_t * s);


/**
 * Sets the value of the "group" element of this Style_t.
 *
 * @param s the Style_t structure.
 *
 * @param group RenderGroup_t value of the "group" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_setGroup(Style_t * s, const RenderGroup_t* group);


/**
 * Creates a new RenderGroup_t object, adds it to this Style_t object and
 * returns the RenderGroup_t object created.
 *
 * @param s the Style_t structure to which the RenderGroup_t should be added.
 *
 * @return a new RenderGroup_t object instance.
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
RenderGroup_t*
Style_createGroup(Style_t* s);


/**
 * Unsets the value of the "group" element of this Style_t.
 *
 * @param s the Style_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_unsetGroup(Style_t * s);


/**
 * Predicate returning @c 1 if this Style_t is of type GlobalStyle_t
 *
 * @param s the Style_t structure.
 *
 * @return @c 1 if this Style_t is of type GlobalStyle_t, @c 0 otherwise
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_isGlobalStyle(const Style_t * s);


/**
 * Predicate returning @c 1 if this Style_t is of type LocalStyle_t
 *
 * @param s the Style_t structure.
 *
 * @return @c 1 if this Style_t is of type LocalStyle_t, @c 0 otherwise
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_isLocalStyle(const Style_t * s);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Style_t object have been set.
 *
 * @param s the Style_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * Style_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof Style_t
 */
LIBSBML_EXTERN
int
Style_hasRequiredAttributes(const Style_t * s);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !Style_H__ */


