/**
 * @file DistribSBasePlugin.h
 * @brief Definition of the DistribSBasePlugin class.
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
 * @class DistribSBasePlugin
 * @sbmlbrief{distrib} Extension of SBase.
 */


#ifndef DistribSBasePlugin_H__
#define DistribSBasePlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/distrib/sbml/ListOfUncertainties.h>
#include <sbml/packages/distrib/sbml/Uncertainty.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribSBasePlugin : public SBasePlugin
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfUncertainties mUncertainties;

  /** @endcond */

public:

  /**
   * Creates a new DistribSBasePlugin using the given URI, prefix and package
   * namespace.
   *
   * @param uri a string, representing the URI of the SBML Level&nbsp;3 package
   * implemented by this libSBML package extension.
   *
   * @param prefix a string, the XML namespace prefix being used for this
   * package.
   *
   * @param distribns a pointer to the namesspaces object
   * (DistribPkgNamespaces) for this package.
   *
   * @copydetails doc_what_are_xmlnamespaces
   *
   * @copydetails doc_what_are_sbmlnamespaces
   */
  DistribSBasePlugin(const std::string& uri,
                     const std::string& prefix,
                     DistribPkgNamespaces* distribns);


  /**
   * Copy constructor for DistribSBasePlugin.
   *
   * @param orig the DistribSBasePlugin instance to copy.
   */
  DistribSBasePlugin(const DistribSBasePlugin& orig);


  /**
   * Assignment operator for DistribSBasePlugin.
   *
   * @param rhs the DistribSBasePlugin object whose values are to be used as
   * the basis of the assignment.
   */
  DistribSBasePlugin& operator=(const DistribSBasePlugin& rhs);


  /**
   * Creates and returns a deep copy of this DistribSBasePlugin object.
   *
   * @return a (deep) copy of this DistribSBasePlugin object.
   */
  virtual DistribSBasePlugin* clone() const;


  /**
   * Destructor for DistribSBasePlugin.
   */
  virtual ~DistribSBasePlugin();


  /**
   * Returns the ListOfUncertainties from this DistribSBasePlugin.
   *
   * @return the ListOfUncertainties from this DistribSBasePlugin.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see getUncertainty(const std::string& sid)
   * @see getUncertainty(unsigned int n)
   * @see getNumUncertainties()
   * @see removeUncertainty(const std::string& sid)
   * @see removeUncertainty(unsigned int n)
   */
  const ListOfUncertainties* getListOfUncertainties() const;


  /**
   * Returns the ListOfUncertainties from this DistribSBasePlugin.
   *
   * @return the ListOfUncertainties from this DistribSBasePlugin.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see getUncertainty(const std::string& sid)
   * @see getUncertainty(unsigned int n)
   * @see getNumUncertainties()
   * @see removeUncertainty(const std::string& sid)
   * @see removeUncertainty(unsigned int n)
   */
  ListOfUncertainties* getListOfUncertainties();


  /**
   * Get an Uncertainty from the DistribSBasePlugin.
   *
   * @param n an unsigned int representing the index of the Uncertainty to
   * retrieve.
   *
   * @return the nth Uncertainty in the ListOfUncertainties within this
   * DistribSBasePlugin or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see getUncertainty(const std::string& sid)
   * @see getNumUncertainties()
   * @see removeUncertainty(const std::string& sid)
   * @see removeUncertainty(unsigned int n)
   */
  Uncertainty* getUncertainty(unsigned int n);


  /**
   * Get an Uncertainty from the DistribSBasePlugin.
   *
   * @param n an unsigned int representing the index of the Uncertainty to
   * retrieve.
   *
   * @return the nth Uncertainty in the ListOfUncertainties within this
   * DistribSBasePlugin or @c NULL if no such object exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see getUncertainty(const std::string& sid)
   * @see getNumUncertainties()
   * @see removeUncertainty(const std::string& sid)
   * @see removeUncertainty(unsigned int n)
   */
  const Uncertainty* getUncertainty(unsigned int n) const;


  /**
   * Adds a copy of the given Uncertainty to this DistribSBasePlugin.
   *
   * @param u the Uncertainty object to add.
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
   * @see createUncertainty()
   * @see getUncertainty(const std::string& sid)
   * @see getUncertainty(unsigned int n)
   * @see getNumUncertainties()
   * @see removeUncertainty(const std::string& sid)
   * @see removeUncertainty(unsigned int n)
   */
  int addUncertainty(const Uncertainty* u);


  /**
   * Get the number of Uncertainty objects in this DistribSBasePlugin.
   *
   * @return the number of Uncertainty objects in this DistribSBasePlugin.
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see getUncertainty(const std::string& sid)
   * @see getUncertainty(unsigned int n)
   * @see removeUncertainty(const std::string& sid)
   * @see removeUncertainty(unsigned int n)
   */
  unsigned int getNumUncertainties() const;


  /**
   * Creates a new Uncertainty object, adds it to this DistribSBasePlugin
   * object and returns the Uncertainty object created.
   *
   * @return a new Uncertainty object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see getUncertainty(const std::string& sid)
   * @see getUncertainty(unsigned int n)
   * @see getNumUncertainties()
   * @see removeUncertainty(const std::string& sid)
   * @see removeUncertainty(unsigned int n)
   */
  Uncertainty* createUncertainty();


  /**
   * Removes the nth Uncertainty from this DistribSBasePlugin and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the Uncertainty to
   * remove.
   *
   * @return a pointer to the nth Uncertainty in this DistribSBasePlugin.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addUncertainty(const Uncertainty* object)
   * @see createUncertainty()
   * @see getUncertainty(const std::string& sid)
   * @see getUncertainty(unsigned int n)
   * @see getNumUncertainties()
   * @see removeUncertainty(const std::string& sid)
   */
  Uncertainty* removeUncertainty(unsigned int n);



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
   * Gets the value of the "attributeName" attribute of this
   * DistribSBasePlugin.
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
   * DistribSBasePlugin.
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
   * DistribSBasePlugin.
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
   * DistribSBasePlugin.
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
   * DistribSBasePlugin.
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
   * Predicate returning @c true if this DistribSBasePlugin's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribSBasePlugin's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribSBasePlugin.
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
   * DistribSBasePlugin.
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
   * DistribSBasePlugin.
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
   * DistribSBasePlugin.
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
   * DistribSBasePlugin.
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
   * DistribSBasePlugin.
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
   * DistribSBasePlugin.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribSBasePlugin.
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
   * DistribSBasePlugin.
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
   * Returns the number of "elementName" in this DistribSBasePlugin.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this DistribSBasePlugin.
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


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Returns a ListOf_t * containing Uncertainty_t objects from this
 * DistribSBasePlugin_t.
 *
 * @param dsbp the DistribSBasePlugin_t structure whose ListOfUncertainties is
 * sought.
 *
 * @return the ListOfUncertainties from this DistribSBasePlugin_t as a ListOf_t
 * *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see DistribSBasePlugin_addUncertainty()
 * @see DistribSBasePlugin_createUncertainty()
 * @see DistribSBasePlugin_getUncertaintyById()
 * @see DistribSBasePlugin_getUncertainty()
 * @see DistribSBasePlugin_getNumUncertainties()
 * @see DistribSBasePlugin_removeUncertaintyById()
 * @see DistribSBasePlugin_removeUncertainty()
 *
 * @memberof DistribSBasePlugin_t
 */
LIBSBML_EXTERN
ListOf_t*
DistribSBasePlugin_getListOfUncertainties(DistribSBasePlugin_t* dsbp);


/**
 * Get an Uncertainty_t from the DistribSBasePlugin_t.
 *
 * @param dsbp the DistribSBasePlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the Uncertainty_t to
 * retrieve.
 *
 * @return the nth Uncertainty_t in the ListOfUncertainties within this
 * DistribSBasePlugin or @c NULL if no such object exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribSBasePlugin_t
 */
LIBSBML_EXTERN
Uncertainty_t*
DistribSBasePlugin_getUncertainty(DistribSBasePlugin_t* dsbp, unsigned int n);


/**
 * Adds a copy of the given Uncertainty_t to this DistribSBasePlugin_t.
 *
 * @param dsbp the DistribSBasePlugin_t structure to which the Uncertainty_t
 * should be added.
 *
 * @param u the Uncertainty_t object to add.
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
 * @memberof DistribSBasePlugin_t
 */
LIBSBML_EXTERN
int
DistribSBasePlugin_addUncertainty(DistribSBasePlugin_t* dsbp,
                                  const Uncertainty_t* u);


/**
 * Get the number of Uncertainty_t objects in this DistribSBasePlugin_t.
 *
 * @param dsbp the DistribSBasePlugin_t structure to query.
 *
 * @return the number of Uncertainty_t objects in this DistribSBasePlugin_t.
 *
 * @memberof DistribSBasePlugin_t
 */
LIBSBML_EXTERN
unsigned int
DistribSBasePlugin_getNumUncertainties(DistribSBasePlugin_t* dsbp);


/**
 * Creates a new Uncertainty_t object, adds it to this DistribSBasePlugin_t
 * object and returns the Uncertainty_t object created.
 *
 * @param dsbp the DistribSBasePlugin_t structure to which the Uncertainty_t
 * should be added.
 *
 * @return a new Uncertainty_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof DistribSBasePlugin_t
 */
LIBSBML_EXTERN
Uncertainty_t*
DistribSBasePlugin_createUncertainty(DistribSBasePlugin_t* dsbp);


/**
 * Removes the nth Uncertainty_t from this DistribSBasePlugin_t and returns a
 * pointer to it.
 *
 * @param dsbp the DistribSBasePlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the Uncertainty_t to
 * remove.
 *
 * @return a pointer to the nth Uncertainty_t in this DistribSBasePlugin_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof DistribSBasePlugin_t
 */
LIBSBML_EXTERN
Uncertainty_t*
DistribSBasePlugin_removeUncertainty(DistribSBasePlugin_t* dsbp,
                                     unsigned int n);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribSBasePlugin_H__ */


