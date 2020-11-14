/**
 * @file ArraysSBasePlugin.h
 * @brief Definition of the ArraysSBasePlugin class.
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
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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
 * @class ArraysSBasePlugin
 * @sbmlbrief{arrays} Extension of SBase.
 */


#ifndef ArraysSBasePlugin_H__
#define ArraysSBasePlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/arrays/sbml/ListOfIndices.h>
#include <sbml/packages/arrays/sbml/Index.h>
#include <sbml/packages/arrays/sbml/ListOfDimensions.h>
#include <sbml/packages/arrays/sbml/Dimension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ArraysSBasePlugin : public SBasePlugin
{
protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfIndices mIndices;
  ListOfDimensions mDimensions;

  /** @endcond */

public:

  /**
   * Creates a new ArraysSBasePlugin using the given uri, prefix and package
   * namespace.
   *
   * @param uri a string, representing the uri of the package.
   *
   * @param prefix a string, the prefix to be used.
   *
   * @param arraysns a pointer to the ArraysPkgNamespaces object to be used.
   */
  ArraysSBasePlugin(const std::string& uri,
                    const std::string& prefix,
                    ArraysPkgNamespaces* arraysns);


  /**
   * Copy constructor for ArraysSBasePlugin.
   *
   * @param orig the ArraysSBasePlugin instance to copy.
   */
  ArraysSBasePlugin(const ArraysSBasePlugin& orig);


  /**
   * Assignment operator for ArraysSBasePlugin.
   *
   * @param rhs the ArraysSBasePlugin object whose values are to be used as the
   * basis of the assignment.
   */
  ArraysSBasePlugin& operator=(const ArraysSBasePlugin& rhs);


  /**
   * Creates and returns a deep copy of this ArraysSBasePlugin object.
   *
   * @return a (deep) copy of this ArraysSBasePlugin object.
   */
  virtual ArraysSBasePlugin* clone() const;


  /**
   * Destructor for ArraysSBasePlugin.
   */
  virtual ~ArraysSBasePlugin();


  /**
   * Returns the ListOfIndices from this ArraysSBasePlugin.
   *
   * @return the ListOfIndices from this ArraysSBasePlugin.
   */
  const ListOfIndices* getListOfIndices() const;


  /**
   * Returns the ListOfIndices from this ArraysSBasePlugin.
   *
   * @return the ListOfIndices from this ArraysSBasePlugin.
   */
  ListOfIndices* getListOfIndices();


  /**
   * Get an Index from the ArraysSBasePlugin.
   *
   * @param n an unsigned int representing the index of the Index to retrieve.
   *
   * @return the nth Index in the ListOfIndices within this ArraysSBasePlugin.
   *
   * @see getNumIndices()
   */
  Index* getIndex(unsigned int n);


  /**
   * Get an Index from the ArraysSBasePlugin.
   *
   * @param n an unsigned int representing the index of the Index to retrieve.
   *
   * @return the nth Index in the ListOfIndices within this ArraysSBasePlugin.
   *
   * @see getNumIndices()
   */
  const Index* getIndex(unsigned int n) const;


  /**
  * Get a Index from the ArraysSBasePlugin based on the arrayDimension to which it
  * refers.
  *
  * @param arrayDimension an unsigned int representing the arrayDimension attribute of the Index
  * object to retrieve.
  *
  * @return the first Index in this ArraysSBasePlugin based on the given
  * arrayDimension attribute or NULL if no such Index exists.
  */
  const Index* getIndexByArrayDimension(unsigned int arrayDimension) const;


  /**
  * Get a Index from the ArraysSBasePlugin based on the arrayIndex to which it
  * refers.
  *
  * @param arrayDimension an unsigned int representing the arrayDimension attribute of the Index
  * object to retrieve.
  *
  * @return the first Index in this ArraysSBasePlugin based on the given
  * arrayDimension attribute or NULL if no such Index exists.
  */
  Index* getIndexByArrayDimension(unsigned int arrayDimension);


  /**
   * Adds a copy of the given Index to this ArraysSBasePlugin.
   *
   * @param i the Index object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createIndex()
   */
  int addIndex(const Index* i);


  /**
   * Get the number of Index objects in this ArraysSBasePlugin.
   *
   * @return the number of Index objects in this ArraysSBasePlugin.
   */
  unsigned int getNumIndices() const;


  /**
   * Creates a new Index object, adds it to this ArraysSBasePlugin object and
   * returns the Index object created.
   *
   * @return a new Index object instance.
   *
   * @see addIndex(const Index* i)
   */
  Index* createIndex();


  /**
   * Removes the nth Index from this ArraysSBasePlugin and returns a pointer to
   * it.
   *
   * @param n an unsigned int representing the index of the Index to remove.
   *
   * @return a pointer to the nth Index in this ArraysSBasePlugin.
   *
   * @see getNumIndices
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  Index* removeIndex(unsigned int n);


  /**
   * Returns the ListOfDimensions from this ArraysSBasePlugin.
   *
   * @return the ListOfDimensions from this ArraysSBasePlugin.
   */
  const ListOfDimensions* getListOfDimensions() const;


  /**
   * Returns the ListOfDimensions from this ArraysSBasePlugin.
   *
   * @return the ListOfDimensions from this ArraysSBasePlugin.
   */
  ListOfDimensions* getListOfDimensions();


  /**
   * Get a Dimension from the ArraysSBasePlugin.
   *
   * @param n an unsigned int representing the index of the Dimension to
   * retrieve.
   *
   * @return the nth Dimension in the ListOfDimensions within this
   * ArraysSBasePlugin.
   *
   * @see getNumDimensions()
   */
  Dimension* getDimension(unsigned int n);


  /**
   * Get a Dimension from the ArraysSBasePlugin.
   *
   * @param n an unsigned int representing the index of the Dimension to
   * retrieve.
   *
   * @return the nth Dimension in the ListOfDimensions within this
   * ArraysSBasePlugin.
   *
   * @see getNumDimensions()
   */
  const Dimension* getDimension(unsigned int n) const;


  /**
   * Get a Dimension from the ArraysSBasePlugin based on its identifier.
   *
   * @param sid a string representing the identifier of the Dimension to
   * retrieve.
   *
   * @return the Dimension in the ListOfDimensions within this
   * ArraysSBasePlugin with the given id or NULL if no such Dimension exists.
   *
   * @see getDimension(unsigned int n)
   * @see getNumDimensions()
   */
  Dimension* getDimension(const std::string& sid);


  /**
   * Get a Dimension from the ArraysSBasePlugin based on its identifier.
   *
   * @param sid a string representing the identifier of the Dimension to
   * retrieve.
   *
   * @return the Dimension in the ListOfDimensions within this
   * ArraysSBasePlugin with the given id or NULL if no such Dimension exists.
   *
   * @see getDimension(unsigned int n)
   * @see getNumDimensions()
   */
  const Dimension* getDimension(const std::string& sid) const;


  /**
   * Get a Dimension from the ArraysSBasePlugin based on the Size to which it
   * refers.
   *
   * @param sid a string representing the size attribute of the Dimension
   * object to retrieve.
   *
   * @return the first Dimension in this ArraysSBasePlugin based on the given
   * size attribute or NULL if no such Dimension exists.
   */
  const Dimension* getDimensionBySize(const std::string& sid) const;


  /**
   * Get a Dimension from the ArraysSBasePlugin based on the Size to which it
   * refers.
   *
   * @param sid a string representing the size attribute of the Dimension
   * object to retrieve.
   *
   * @return the first Dimension in this ArraysSBasePlugin based on the given
   * size attribute or NULL if no such Dimension exists.
   */
  Dimension* getDimensionBySize(const std::string& sid);


  /**
  * Get a Dimension from the ArraysSBasePlugin based on the arrayDimension to which it
  * refers.
  *
  * @param arrayDimension an unsigned int representing the arrayDimension attribute of the Dimension
  * object to retrieve.
  *
  * @return the first Dimension in this ArraysSBasePlugin based on the given
  * arrayDimension attribute or NULL if no such Dimension exists.
  */
  const Dimension* getDimensionByArrayDimension(unsigned int arrayDimension) const;


  /**
  * Get a Dimension from the ArraysSBasePlugin based on the arrayDimension to which it
  * refers.
  *
  * @param arrayDimension an unsigned int representing the arrayDimension attribute of the Dimension
  * object to retrieve.
  *
  * @return the first Dimension in this ArraysSBasePlugin based on the given
  * arrayDimension attribute or NULL if no such Dimension exists.
  */
  Dimension* getDimensionByArrayDimension(unsigned int arrayDimension);


  /**
   * Adds a copy of the given Dimension to this ArraysSBasePlugin.
   *
   * @param d the Dimension object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createDimension()
   */
  int addDimension(const Dimension* d);


  /**
   * Get the number of Dimension objects in this ArraysSBasePlugin.
   *
   * @return the number of Dimension objects in this ArraysSBasePlugin.
   */
  unsigned int getNumDimensions() const;


  /**
   * Creates a new Dimension object, adds it to this ArraysSBasePlugin object
   * and returns the Dimension object created.
   *
   * @return a new Dimension object instance.
   *
   * @see addDimension(const Dimension* d)
   */
  Dimension* createDimension();


  /**
   * Removes the nth Dimension from this ArraysSBasePlugin and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the Dimension to
   * remove.
   *
   * @return a pointer to the nth Dimension in this ArraysSBasePlugin.
   *
   * @see getNumDimensions
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  Dimension* removeDimension(unsigned int n);


  /**
   * Removes the Dimension from this ArraysSBasePlugin based on its identifier
   * and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the Dimension to
   * remove.
   *
   * @return the Dimension in this ArraysSBasePlugin based on the identifier or
   * NULL if no such Dimension exists.
   *
   * @note the caller owns the returned object and is responsible for deleting
   * it.
   */
  Dimension* removeDimension(const std::string& sid);


  /**
   * Predicate returning @c true if all the required elements for this
   * ArraysSBasePlugin object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * ArraysSBasePlugin have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the ArraysSBasePlugin object are:
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


  /**
   * Get the total number of elements from the ArraysSBasePlugin.
   *
   * @return the total number of elements in this array.
   */
  std::vector<unsigned int> getNumArrayElements() const;


  /**
  * Get the number of elements from the ArraysSBasePlugin for the given
  * arrayDimension.
  *
  * @param arrayDimension an unsigned int specifying the arrayDimension to be queried.
  *
  * @return the number of elements in this array in the given dimension.
  */
  unsigned int getNumElementsInDimension(unsigned int arrayDimension) const;

  /**/
  unsigned int getNumImpliedDimensions() const;

  SBase* getParent() const;

  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this ArraysSBasePlugin.
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
   * Gets the value of the "attributeName" attribute of this ArraysSBasePlugin.
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
   * Gets the value of the "attributeName" attribute of this ArraysSBasePlugin.
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
   * Gets the value of the "attributeName" attribute of this ArraysSBasePlugin.
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
   * Gets the value of the "attributeName" attribute of this ArraysSBasePlugin.
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
   * Predicate returning @c true if this ArraysSBasePlugin's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this ArraysSBasePlugin's attribute "attributeName" has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this ArraysSBasePlugin.
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
   * Sets the value of the "attributeName" attribute of this ArraysSBasePlugin.
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
   * Sets the value of the "attributeName" attribute of this ArraysSBasePlugin.
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
   * Sets the value of the "attributeName" attribute of this ArraysSBasePlugin.
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
   * Sets the value of the "attributeName" attribute of this ArraysSBasePlugin.
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
   * ArraysSBasePlugin.
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
   * Creates and returns an new "elementName" object in this ArraysSBasePlugin.
   *
   * @param elementName, the name of the element to create.
   *
   * pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the number of "elementName" in this ArraysSBasePlugin.
   *
   * @param elementName, the name of the element to get number of.
   *
   * unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this ArraysSBasePlugin.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @param index, unsigned int the index of the object to retrieve.
   *
   * pointer to the object.
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
   * @return a pointer to the SBase element with the given @p id.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element that has the given @p metaid, or @c NULL
   * if no such object is found.
   *
   * @param metaid a string representing the metaid attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p metaid.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * filter, an ElementFilter that may impose restrictions on the objects to be
   * retrieved.
   *
   * @return a List* pointer of pointers to all SBase child objects with any
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




#endif /* !ArraysSBasePlugin_H__ */


