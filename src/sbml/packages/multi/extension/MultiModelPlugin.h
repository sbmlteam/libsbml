/**
 * @file:   MultiModelPlugin.h
 * @brief:  Implementation of the MultiModelPlugin class
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


#ifndef MultiModelPlugin_H__
#define MultiModelPlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/multi/sbml/MultiSpeciesType.h>
#include <sbml/packages/multi/sbml/BindingSiteSpeciesType.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN MultiModelPlugin : public SBasePlugin
{
public:

  /**
   * Creates a new MultiModelPlugin
   */
  MultiModelPlugin(const std::string& uri, const std::string& prefix, 
                                 MultiPkgNamespaces* multins);


  /**
   * Copy constructor for MultiModelPlugin.
   *
   * @param orig; the MultiModelPlugin instance to copy.
   */
  MultiModelPlugin(const MultiModelPlugin& orig);


   /**
   * Assignment operator for MultiModelPlugin.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  MultiModelPlugin& operator=(const MultiModelPlugin& rhs);


   /**
   * Creates and returns a deep copy of this MultiModelPlugin object.
   *
   * @return a (deep) copy of this MultiModelPlugin object.
   */
  virtual MultiModelPlugin* clone () const;


   /**
   * Destructor for MultiModelPlugin.
   */
  virtual ~MultiModelPlugin();


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
   * Returns the ListOfMultiSpeciesTypes in this plugin object.
   *
   * @return ListOfMultiSpeciesTypes object in this plugin object.
   */
  const ListOfMultiSpeciesTypes* getListOfMultiSpeciesTypes () const;


  /**
   * Returns the ListOfMultiSpeciesTypes in this plugin object.
   *
   * @return ListOfMultiSpeciesTypes object in this plugin object.
   */
  ListOfMultiSpeciesTypes* getListOfMultiSpeciesTypes ();


  /**
   * Returns the MultiSpeciesType object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the MultiSpeciesType to get
   *
   * @return the nth MultiSpeciesType in the ListOfMultiSpeciesTypes
   */
  const MultiSpeciesType* getMultiSpeciesType(unsigned int n) const;


  /**
   * Returns the MultiSpeciesType object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the MultiSpeciesType to get
   *
   * @return the nth MultiSpeciesType in the ListOfMultiSpeciesTypes
   */
  MultiSpeciesType* getMultiSpeciesType(unsigned int n);


  /**
   * Returns the MultiSpeciesType object based on its identifier.
   *
   * @param sid a string representing the id of the MultiSpeciesType to get
   *
   * @return MultiSpeciesType in the ListOfMultiSpeciesTypes with the given id
   * or NULL if no such MultiSpeciesType exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  const MultiSpeciesType* getMultiSpeciesType(const std::string& sid) const;


  /**
   * Returns the MultiSpeciesType object based on its identifier.
   *
   * @param sid a string representing the id of the MultiSpeciesType to get
   *
   * @return MultiSpeciesType in the ListOfMultiSpeciesTypes with the given id
   * or NULL if no such MultiSpeciesType exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  MultiSpeciesType* getMultiSpeciesType(const std::string& sid);


  /**
   * Adds a copy of the given MultiSpeciesType to the ListOfMultiSpeciesTypes in this plugin object.
   *
   * @param multiSpeciesType the multiSpeciesType to be added.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int addMultiSpeciesType (const MultiSpeciesType* multiSpeciesType);


  /**
   * Creates a new MultiSpeciesType object and adds it to the ListOfMultiSpeciesTypes in this plugin object.
   *
   * @return the newly created MultiSpeciesType object.
   */
  MultiSpeciesType* createMultiSpeciesType ();


  /**
   * Creates a new BindingSiteSpeciesType object and adds it to the ListOfMultiSpeciesTypes in this plugin object.
   *
   * @return the newly created BindingSiteSpeciesType object.
   */
  BindingSiteSpeciesType* createBindingSiteSpeciesType ();


  /**
   * Removes the nth MultiSpeciesType object from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param n the index of the MultiSpeciesType to remove
   *
   * @return the MultiSpeciesType object removed 
   * or NULL index was out of range.
   */
  MultiSpeciesType* removeMultiSpeciesType(unsigned int n);


  /**
   * Removes the MultiSpeciesType object with the given id from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param sid a string representing the id of the MultiSpeciesType to remove
   *
   * @return the MultiSpeciesType object removed 
   * or NULL if no such MultiSpeciesType exists.
   */
  MultiSpeciesType* removeMultiSpeciesType(const std::string& sid);


  /**
   * Returns the number of MultiSpeciesType objects in this plugin object.
   *
   * @return the number of MultiSpeciesType objects in this plugin object.
   */
  unsigned int getNumMultiSpeciesTypes () const;

  /**
   * Creates a new IntraSpeciesReaction object and adds it to the ListOfReactions.
   *
   * @return the newly created IntraSpeciesReaction object.
   */
  IntraSpeciesReaction* createIntraSpeciesReaction ();


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

  ListOfMultiSpeciesTypes mListOfMultiSpeciesTypes;

  /** @endcond doxygenLibsbmlInternal */


};




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* MultiModelPlugin_H__ */


