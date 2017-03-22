/**
 * @file:   MultiCompartmentPlugin.h
 * @brief:  Implementation of the MultiCompartmentPlugin class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#ifndef MultiCompartmentPlugin_H__
#define MultiCompartmentPlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <string>

#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/multi/sbml/CompartmentReference.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN MultiCompartmentPlugin : public SBasePlugin
{
public:

  /**
   * Creates a new MultiCompartmentPlugin
   */
  MultiCompartmentPlugin(const std::string& uri, const std::string& prefix, 
                                 MultiPkgNamespaces* multins);


  /**
   * Copy constructor for MultiCompartmentPlugin.
   *
   * @param orig; the MultiCompartmentPlugin instance to copy.
   */
  MultiCompartmentPlugin(const MultiCompartmentPlugin& orig);


   /**
   * Assignment operator for MultiCompartmentPlugin.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  MultiCompartmentPlugin& operator=(const MultiCompartmentPlugin& rhs);


   /**
   * Creates and returns a deep copy of this MultiCompartmentPlugin object.
   *
   * @return a (deep) copy of this MultiCompartmentPlugin object.
   */
  virtual MultiCompartmentPlugin* clone () const;


   /**
   * Destructor for MultiCompartmentPlugin.
   */
  virtual ~MultiCompartmentPlugin();


   //---------------------------------------------------------------
  //
  // overridden virtual functions for read/write/check
  //
  //---------------------------------------------------------------

  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses must override this method to create, store, and then
   * return an SBML object corresponding to the next XMLToken in the
   * XMLInputStream if they have their specific elements.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses must override this method to write out their contained
   * SBML objects as XML elements if they have their specific elements.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


  /**
   * Checks if this plugin object has all the required elements.
   *
   * Subclasses must override this method 
   * if they have their specific elements.
   *
   * @return true if this plugin object has all the required elements
   * otherwise false will be returned.
   */
  virtual bool hasRequiredElements () const;


  //---------------------------------------------------------------


  //---------------------------------------------------------------
  //
  // Functions for interacting with the members of the plugin
  //
  //---------------------------------------------------------------


   /**
   * Returns the value of the "compartmentType" attribute of this CompPlugin.
   *
   * @return the value of the "compartmentType" attribute of this CompPlugin as a string.
   */
  virtual const std::string& getCompartmentType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CompPlugin's "compartmentType" attribute has been set.
   *
   * @return @c true if this CompPlugin's "compartmentType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCompartmentType() const;


  /**
   * Sets the value of the "compartmentType" attribute of this CompPlugin.
   *
   * @param compartmentType; const std::string& value of the "compartmentType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCompartmentType(const std::string& compartmentType);


  /**
   * Unsets the value of the "compartmentType" attribute of this CompPlugin.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCompartmentType();


  /**
   * Returns the value of the "isType" attribute of this CompPlugin.
   *
   * @return the value of the "isType" attribute of this CompPlugin as a boolean.
   */
  virtual bool getIsType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * CompPlugin's "isType" attribute has been set.
   *
   * @return @c true if this CompPlugin's "isType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetIsType() const;


  /**
   * Sets the value of the "isType" attribute of this CompPlugin.
   *
   * @param isType; bool value of the "isType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setIsType(bool isType);


  /**
   * Unsets the value of the "isType" attribute of this CompPlugin.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetIsType();



  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the ListOfCompartmentReferences in this plugin object.
   *
   * @return ListOfCompartmentReferences object in this plugin object.
   */
  const ListOfCompartmentReferences* getListOfCompartmentReferences () const;


  /**
   * Returns the ListOfCompartmentReferences in this plugin object.
   *
   * @return ListOfCompartmentReferences object in this plugin object.
   */
  ListOfCompartmentReferences* getListOfCompartmentReferences ();


  /**
   * Returns the CompartmentReference object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the CompartmentReference to get
   *
   * @return the nth CompartmentReference in the ListOfCompartmentReferences
   */
  const CompartmentReference* getCompartmentReference(unsigned int n) const;


  /**
   * Returns the CompartmentReference object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the CompartmentReference to get
   *
   * @return the nth CompartmentReference in the ListOfCompartmentReferences
   */
  CompartmentReference* getCompartmentReference(unsigned int n);


  /**
   * Returns the CompartmentReference object based on its identifier.
   *
   * @param sid a string representing the id of the CompartmentReference to get
   *
   * @return CompartmentReference in the ListOfCompartmentReferences with the given id
   * or NULL if no such CompartmentReference exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  const CompartmentReference* getCompartmentReference(const std::string& sid) const;


  /**
   * Returns the CompartmentReference object based on its identifier.
   *
   * @param sid a string representing the id of the CompartmentReference to get
   *
   * @return CompartmentReference in the ListOfCompartmentReferences with the given id
   * or NULL if no such CompartmentReference exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  CompartmentReference* getCompartmentReference(const std::string& sid);


  /**
   * Adds a copy of the given CompartmentReference to the ListOfCompartmentReferences in this plugin object.
   *
   * @param compartmentReference the compartmentReference to be added.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int addCompartmentReference (const CompartmentReference* compartmentReference);


  /**
   * Creates a new CompartmentReference object and adds it to the ListOfCompartmentReferences in this plugin object.
   *
   * @return the newly created CompartmentReference object.
   */
  CompartmentReference* createCompartmentReference ();


  /**
   * Removes the nth CompartmentReference object from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param n the index of the CompartmentReference to remove
   *
   * @return the CompartmentReference object removed 
   * or NULL index was out of range.
   */
  CompartmentReference* removeCompartmentReference(unsigned int n);


  /**
   * Removes the CompartmentReference object with the given id from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param sid a string representing the id of the CompartmentReference to remove
   *
   * @return the CompartmentReference object removed 
   * or NULL if no such CompartmentReference exists.
   */
  CompartmentReference* removeCompartmentReference(const std::string& sid);


  /**
   * Returns the number of CompartmentReference objects in this plugin object.
   *
   * @return the number of CompartmentReference objects in this plugin object.
   */
  unsigned int getNumCompartmentReferences () const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  virtual void connectToParent (SBase* sbase);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  virtual bool accept (SBMLVisitor& v) const;

  /** @endcond doxygenLibsbmlInternal */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */

  /** @cond doxygenLibsbmlInternal */


  ListOfCompartmentReferences mListOfCompartmentReferences;

  std::string   mCompartmentType;
  bool          mIsType;
  bool          mIsSetIsType;

  /** @endcond doxygenLibsbmlInternal */


};




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* MultiCompartmentPlugin_H__ */


