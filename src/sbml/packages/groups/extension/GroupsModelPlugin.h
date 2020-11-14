/**
 * @file GroupsModelPlugin.h
 * @brief Definition of the GroupsModelPlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
 * @class GroupsModelPlugin
 * @sbmlbrief{groups} Extension of Model.
 */


#ifndef GroupsModelPlugin_H__
#define GroupsModelPlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/groups/sbml/ListOfGroups.h>
#include <sbml/packages/groups/sbml/Group.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN GroupsModelPlugin : public SBasePlugin
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfGroups mGroups;

  /** @endcond */

public:

  /**
   * Creates a new GroupsModelPlugin using the given URI, prefix and package
   * namespace.
   *
   * @param uri a string, representing the URI of the SBML Level&nbsp;3 package
   * implemented by this libSBML package extension.
   *
   * @param prefix a string, the XML namespace prefix being used for this
   * package.
   *
   * @param groupsns a pointer to the namesspaces object (GroupsPkgNamespaces)
   * for this package.
   *
   * @copydetails doc_what_are_xmlnamespaces
   *
   * @copydetails doc_what_are_sbmlnamespaces
   */
  GroupsModelPlugin(const std::string& uri,
                    const std::string& prefix,
                    GroupsPkgNamespaces* groupsns);


  /**
   * Copy constructor for GroupsModelPlugin.
   *
   * @param orig the GroupsModelPlugin instance to copy.
   */
  GroupsModelPlugin(const GroupsModelPlugin& orig);


  /**
   * Assignment operator for GroupsModelPlugin.
   *
   * @param rhs the GroupsModelPlugin object whose values are to be used as the
   * basis of the assignment.
   */
  GroupsModelPlugin& operator=(const GroupsModelPlugin& rhs);


  /**
   * Creates and returns a deep copy of this GroupsModelPlugin object.
   *
   * @return a (deep) copy of this GroupsModelPlugin object.
   */
  virtual GroupsModelPlugin* clone() const;


  /**
   * Destructor for GroupsModelPlugin.
   */
  virtual ~GroupsModelPlugin();


  /**
   * Returns the ListOfGroups from this GroupsModelPlugin.
   *
   * @return the ListOfGroups from this GroupsModelPlugin.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGroup(const Group* object)
   * @see createGroup()
   * @see getGroup(const std::string& sid)
   * @see getGroup(unsigned int n)
   * @see getNumGroups()
   * @see removeGroup(const std::string& sid)
   * @see removeGroup(unsigned int n)
   */
  const ListOfGroups* getListOfGroups() const;


  /**
   * Returns the ListOfGroups from this GroupsModelPlugin.
   *
   * @return the ListOfGroups from this GroupsModelPlugin.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGroup(const Group* object)
   * @see createGroup()
   * @see getGroup(const std::string& sid)
   * @see getGroup(unsigned int n)
   * @see getNumGroups()
   * @see removeGroup(const std::string& sid)
   * @see removeGroup(unsigned int n)
   */
  ListOfGroups* getListOfGroups();


  /**
   * Get a Group from the GroupsModelPlugin.
   *
   * @param n an unsigned int representing the index of the Group to retrieve.
   *
   * @return the nth Group in the ListOfGroups within this GroupsModelPlugin or
   * @c NULL if no such object exists..
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGroup(const Group* object)
   * @see createGroup()
   * @see getGroup(const std::string& sid)
   * @see getNumGroups()
   * @see removeGroup(const std::string& sid)
   * @see removeGroup(unsigned int n)
   */
  Group* getGroup(unsigned int n);


  /**
   * Get a Group from the GroupsModelPlugin.
   *
   * @param n an unsigned int representing the index of the Group to retrieve.
   *
   * @return the nth Group in the ListOfGroups within this GroupsModelPlugin or
   * @c NULL if no such object exists..
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGroup(const Group* object)
   * @see createGroup()
   * @see getGroup(const std::string& sid)
   * @see getNumGroups()
   * @see removeGroup(const std::string& sid)
   * @see removeGroup(unsigned int n)
   */
  const Group* getGroup(unsigned int n) const;


  /**
   * Get a Group from the GroupsModelPlugin based on its identifier.
   *
   * @param sid a string representing the identifier of the Group to retrieve.
   *
   * @return the Group in the ListOfGroups within this GroupsModelPlugin with
   * the given @p sid or @c NULL if no such Group exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGroup(const Group* object)
   * @see createGroup()
   * @see getGroup(unsigned int n)
   * @see getNumGroups()
   * @see removeGroup(const std::string& sid)
   * @see removeGroup(unsigned int n)
   */
  Group* getGroup(const std::string& sid);


  /**
   * Get a Group from the GroupsModelPlugin based on its identifier.
   *
   * @param sid a string representing the identifier of the Group to retrieve.
   *
   * @return the Group in the ListOfGroups within this GroupsModelPlugin with
   * the given @p sid or @c NULL if no such Group exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGroup(const Group* object)
   * @see createGroup()
   * @see getGroup(unsigned int n)
   * @see getNumGroups()
   * @see removeGroup(const std::string& sid)
   * @see removeGroup(unsigned int n)
   */
  const Group* getGroup(const std::string& sid) const;


  /**
   * Adds a copy of the given Group to this GroupsModelPlugin.
   *
   * @param g the Group object to add.
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
   * @see createGroup()
   * @see getGroup(const std::string& sid)
   * @see getGroup(unsigned int n)
   * @see getNumGroups()
   * @see removeGroup(const std::string& sid)
   * @see removeGroup(unsigned int n)
   */
  int addGroup(const Group* g);


  /**
   * Get the number of Group objects in this GroupsModelPlugin.
   *
   * @return the number of Group objects in this GroupsModelPlugin.
   *
   * @see addGroup(const Group* object)
   * @see createGroup()
   * @see getGroup(const std::string& sid)
   * @see getGroup(unsigned int n)
   * @see removeGroup(const std::string& sid)
   * @see removeGroup(unsigned int n)
   */
  unsigned int getNumGroups() const;


  /**
   * Creates a new Group object, adds it to this GroupsModelPlugin object and
   * returns the Group object created.
   *
   * @return a new Group object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addGroup(const Group* object)
   * @see getGroup(const std::string& sid)
   * @see getGroup(unsigned int n)
   * @see getNumGroups()
   * @see removeGroup(const std::string& sid)
   * @see removeGroup(unsigned int n)
   */
  Group* createGroup();


  /**
   * Removes the nth Group from this GroupsModelPlugin and returns a pointer to
   * it.
   *
   * @param n an unsigned int representing the index of the Group to remove.
   *
   * @return a pointer to the nth Group in this GroupsModelPlugin.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGroup(const Group* object)
   * @see createGroup()
   * @see getGroup(const std::string& sid)
   * @see getGroup(unsigned int n)
   * @see getNumGroups()
   * @see removeGroup(const std::string& sid)
   */
  Group* removeGroup(unsigned int n);


  /**
   * Removes the Group from this GroupsModelPlugin based on its identifier and
   * returns a pointer to it.
   *
   * @param sid a string representing the identifier of the Group to remove.
   *
   * @return the Group in this GroupsModelPlugin based on the identifier or
   * NULL if no such Group exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addGroup(const Group* object)
   * @see createGroup()
   * @see getGroup(const std::string& sid)
   * @see getGroup(unsigned int n)
   * @see getNumGroups()
   * @see removeGroup(unsigned int n)
   */
  Group* removeGroup(const std::string& sid);



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
   * Connects to parent element
   */
  virtual void connectToParent(SBase* base);

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
   * Returns the value of the "attributeName" attribute of this GroupsModelPlugin.
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
   * Returns the value of the "attributeName" attribute of this GroupsModelPlugin.
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
   * Returns the value of the "attributeName" attribute of this GroupsModelPlugin.
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
   * Returns the value of the "attributeName" attribute of this GroupsModelPlugin.
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
   * Returns the value of the "attributeName" attribute of this GroupsModelPlugin.
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
   * Predicate returning @c true if this GroupsModelPlugin's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this GroupsModelPlugin's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this GroupsModelPlugin.
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
   * Sets the value of the "attributeName" attribute of this GroupsModelPlugin.
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
   * Sets the value of the "attributeName" attribute of this GroupsModelPlugin.
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
   * Sets the value of the "attributeName" attribute of this GroupsModelPlugin.
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
   * Sets the value of the "attributeName" attribute of this GroupsModelPlugin.
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
   * GroupsModelPlugin.
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
   * Creates and returns an new "elementName" object in this GroupsModelPlugin.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this GroupsModelPlugin.
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
   * GroupsModelPlugin.
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
   * Returns the number of "elementName" in this GroupsModelPlugin.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this GroupsModelPlugin.
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



  /** @cond doxygenLibsbmlInternal */

  /**
   * Append items from model (used in comp flattening)
   *
   * @param model a pointer to a model object.
   *
   */
  int appendFrom(const Model* model);

  /** @endcond */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new object from the next XMLToken on the XMLInputStream
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */


public:

  /**
   * For nested groups (Member objects that reference a ListOfMembers 
   * object), SBO terms, Notes, and Annotation from the
   * parent ListOfMembers applies to the child.  This function
   * copies any information from any of those three things to all 
   * child ListOfMembers, and if that information is not already 
   * set.  After calling
   * this function, it is sufficient to check any ListOfMembers
   * to see if its SBO term, Notes, or Annotation is set, without
   * further checking to see if that element was nested in another
   * Group.
   */
  virtual void copyInformationToNestedLists();


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Returns a ListOf_t * containing Group_t objects from this
 * GroupsModelPlugin_t.
 *
 * @param gmp the GroupsModelPlugin_t structure whose ListOfGroups is sought.
 *
 * @return the ListOfGroups from this GroupsModelPlugin_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see GroupsModelPlugin_addGroup()
 * @see GroupsModelPlugin_createGroup()
 * @see GroupsModelPlugin_getGroupById()
 * @see GroupsModelPlugin_getGroup()
 * @see GroupsModelPlugin_getNumGroups()
 * @see GroupsModelPlugin_removeGroupById()
 * @see GroupsModelPlugin_removeGroup()
 *
 * @memberof GroupsModelPlugin_t
 */
LIBSBML_EXTERN
ListOf_t*
GroupsModelPlugin_getListOfGroups(GroupsModelPlugin_t* gmp);


/**
 * Get a Group_t from the GroupsModelPlugin_t.
 *
 * @param gmp the GroupsModelPlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the Group_t to retrieve.
 *
 * @return the nth Group_t in the ListOfGroups within this GroupsModelPlugin or
 * @c NULL if no such object exists..
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof GroupsModelPlugin_t
 */
LIBSBML_EXTERN
Group_t*
GroupsModelPlugin_getGroup(GroupsModelPlugin_t* gmp, unsigned int n);


/**
 * Get a Group_t from the GroupsModelPlugin_t based on its identifier.
 *
 * @param gmp the GroupsModelPlugin_t structure to search.
 *
 * @param sid a string representing the identifier of the Group_t to retrieve.
 *
 * @return the Group_t in the ListOfGroups within this GroupsModelPlugin with
 * the given @p sid or @c NULL if no such Group_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof GroupsModelPlugin_t
 */
LIBSBML_EXTERN
Group_t*
GroupsModelPlugin_getGroupById(GroupsModelPlugin_t* gmp, const char *sid);


/**
 * Adds a copy of the given Group_t to this GroupsModelPlugin_t.
 *
 * @param gmp the GroupsModelPlugin_t structure to which the Group_t should be
 * added.
 *
 * @param g the Group_t object to add.
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
 * @memberof GroupsModelPlugin_t
 */
LIBSBML_EXTERN
int
GroupsModelPlugin_addGroup(GroupsModelPlugin_t* gmp, const Group_t* g);


/**
 * Get the number of Group_t objects in this GroupsModelPlugin_t.
 *
 * @param gmp the GroupsModelPlugin_t structure to query.
 *
 * @return the number of Group_t objects in this GroupsModelPlugin_t.
 *
 * @memberof GroupsModelPlugin_t
 */
LIBSBML_EXTERN
unsigned int
GroupsModelPlugin_getNumGroups(GroupsModelPlugin_t* gmp);


/**
 * Creates a new Group_t object, adds it to this GroupsModelPlugin_t object and
 * returns the Group_t object created.
 *
 * @param gmp the GroupsModelPlugin_t structure to which the Group_t should be
 * added.
 *
 * @return a new Group_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof GroupsModelPlugin_t
 */
LIBSBML_EXTERN
Group_t*
GroupsModelPlugin_createGroup(GroupsModelPlugin_t* gmp);


/**
 * Removes the nth Group_t from this GroupsModelPlugin_t and returns a pointer
 * to it.
 *
 * @param gmp the GroupsModelPlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the Group_t to remove.
 *
 * @return a pointer to the nth Group_t in this GroupsModelPlugin_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GroupsModelPlugin_t
 */
LIBSBML_EXTERN
Group_t*
GroupsModelPlugin_removeGroup(GroupsModelPlugin_t* gmp, unsigned int n);


/**
 * Removes the Group_t from this GroupsModelPlugin_t based on its identifier
 * and returns a pointer to it.
 *
 * @param gmp the GroupsModelPlugin_t structure to search.
 *
 * @param sid a string representing the identifier of the Group_t to remove.
 *
 * @return the Group_t in this GroupsModelPlugin_t based on the identifier or
 * NULL if no such Group_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof GroupsModelPlugin_t
 */
LIBSBML_EXTERN
Group_t*
GroupsModelPlugin_removeGroupById(GroupsModelPlugin_t* gmp, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !GroupsModelPlugin_H__ */


