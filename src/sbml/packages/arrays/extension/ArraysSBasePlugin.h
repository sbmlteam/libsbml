/**
 * @file:   ArraysSBasePlugin.h
 * @brief:  Implementation of the ArraysSBasePlugin class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
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


#ifndef ArraysSBasePlugin_H__
#define ArraysSBasePlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/arrays/sbml/Index.h>
#include <sbml/packages/arrays/sbml/Dimension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ArraysSBasePlugin : public SBasePlugin
{
public:

  /**
   * Creates a new ArraysSBasePlugin
   */
  ArraysSBasePlugin(const std::string& uri, const std::string& prefix, 
                                 ArraysPkgNamespaces* arraysns);


  /**
   * Copy constructor for ArraysSBasePlugin.
   *
   * @param orig; the ArraysSBasePlugin instance to copy.
   */
  ArraysSBasePlugin(const ArraysSBasePlugin& orig);


   /**
   * Assignment operator for ArraysSBasePlugin.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  ArraysSBasePlugin& operator=(const ArraysSBasePlugin& rhs);


   /**
   * Creates and returns a deep copy of this ArraysSBasePlugin object.
   *
   * @return a (deep) copy of this ArraysSBasePlugin object.
   */
  virtual ArraysSBasePlugin* clone () const;


   /**
   * Destructor for ArraysSBasePlugin.
   */
  virtual ~ArraysSBasePlugin();


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
   * Returns a List of all child SBase objects, including those nested to an
   * arbitary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the ListOfIndices in this plugin object.
   *
   * @return ListOfIndices object in this plugin object.
   */
  const ListOfIndices* getListOfIndices () const;


  /**
   * Returns the ListOfIndices in this plugin object.
   *
   * @return ListOfIndices object in this plugin object.
   */
  ListOfIndices* getListOfIndices ();


  /**
   * Returns the Index object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the Index to get
   *
   * @return the nth Index in the ListOfIndices
   */
  const Index* getIndex(unsigned int n) const;


  /**
   * Returns the Index object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the Index to get
   *
   * @return the nth Index in the ListOfIndices
   */
  Index* getIndex(unsigned int n);


  /**
   * Returns the Index object based on its identifier.
   *
   * @param sid a string representing the id of the Index to get
   *
   * @return Index in the ListOfIndices with the given id
   * or NULL if no such Index exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  const Index* getIndex(const std::string& sid) const;


  /**
   * Returns the Index object based on its identifier.
   *
   * @param sid a string representing the id of the Index to get
   *
   * @return Index in the ListOfIndices with the given id
   * or NULL if no such Index exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  Index* getIndex(const std::string& sid);


  /**
   * Adds a copy of the given Index to the ListOfIndices in this plugin object.
   *
   * @param index the index to be added.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int addIndex (const Index* index);


  /**
   * Creates a new Index object and adds it to the ListOfIndices in this plugin object.
   *
   * @return the newly created Index object.
   */
  Index* createIndex ();


  /**
   * Removes the nth Index object from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param n the index of the Index to remove
   *
   * @return the Index object removed 
   * or NULL index was out of range.
   */
  Index* removeIndex(unsigned int n);


  /**
   * Removes the Index object with the given id from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param sid a string representing the id of the Index to remove
   *
   * @return the Index object removed 
   * or NULL if no such Index exists.
   */
  Index* removeIndex(const std::string& sid);


  /**
   * Returns the number of Index objects in this plugin object.
   *
   * @return the number of Index objects in this plugin object.
   */
  unsigned int getNumIndexs () const;


  /**
   * Returns the ListOfDimensions in this plugin object.
   *
   * @return ListOfDimensions object in this plugin object.
   */
  const ListOfDimensions* getListOfDimensions () const;


  /**
   * Returns the ListOfDimensions in this plugin object.
   *
   * @return ListOfDimensions object in this plugin object.
   */
  ListOfDimensions* getListOfDimensions ();


  /**
   * Returns the Dimension object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the Dimension to get
   *
   * @return the nth Dimension in the ListOfDimensions
   */
  const Dimension* getDimension(unsigned int n) const;


  /**
   * Returns the Dimension object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the Dimension to get
   *
   * @return the nth Dimension in the ListOfDimensions
   */
  Dimension* getDimension(unsigned int n);


  /**
   * Returns the Dimension object based on its identifier.
   *
   * @param sid a string representing the id of the Dimension to get
   *
   * @return Dimension in the ListOfDimensions with the given id
   * or NULL if no such Dimension exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  const Dimension* getDimension(const std::string& sid) const;


  /**
   * Returns the Dimension object based on its identifier.
   *
   * @param sid a string representing the id of the Dimension to get
   *
   * @return Dimension in the ListOfDimensions with the given id
   * or NULL if no such Dimension exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  Dimension* getDimension(const std::string& sid);


  /**
   * Adds a copy of the given Dimension to the ListOfDimensions in this plugin object.
   *
   * @param dimension the dimension to be added.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int addDimension (const Dimension* dimension);


  /**
   * Creates a new Dimension object and adds it to the ListOfDimensions in this plugin object.
   *
   * @return the newly created Dimension object.
   */
  Dimension* createDimension ();


  /**
   * Removes the nth Dimension object from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param n the index of the Dimension to remove
   *
   * @return the Dimension object removed 
   * or NULL index was out of range.
   */
  Dimension* removeDimension(unsigned int n);


  /**
   * Removes the Dimension object with the given id from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param sid a string representing the id of the Dimension to remove
   *
   * @return the Dimension object removed 
   * or NULL if no such Dimension exists.
   */
  Dimension* removeDimension(const std::string& sid);


  /**
   * Returns the number of Dimension objects in this plugin object.
   *
   * @return the number of Dimension objects in this plugin object.
   */
  unsigned int getNumDimensions () const;


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

  ListOfIndices mIndexs;
  ListOfDimensions mDimensions;

  /** @endcond doxygenLibsbmlInternal */


};




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* ArraysSBasePlugin_H__ */


