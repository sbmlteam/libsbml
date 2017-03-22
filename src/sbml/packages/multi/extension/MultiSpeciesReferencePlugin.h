/**
 * @file:   MultiSpeciesReferencePlugin.h
 * @brief:  Implementation of the MultiSpeciesReferencePlugin class
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


#ifndef MultiSpeciesReferencePlugin_H__
#define MultiSpeciesReferencePlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <sbml/packages/multi/extension/MultiSimpleSpeciesReferencePlugin.h>
#include <sbml/packages/multi/sbml/SpeciesTypeComponentMapInProduct.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN MultiSpeciesReferencePlugin : public MultiSimpleSpeciesReferencePlugin
{
public:

  /**
   * Creates a new MultiSpeciesReferencePlugin
   */
  MultiSpeciesReferencePlugin(const std::string& uri, const std::string& prefix, 
                                 MultiPkgNamespaces* multins);


  /**
   * Copy constructor for MultiSpeciesReferencePlugin.
   *
   * @param orig; the MultiSpeciesReferencePlugin instance to copy.
   */
  MultiSpeciesReferencePlugin(const MultiSpeciesReferencePlugin& orig);


   /**
   * Assignment operator for MultiSpeciesReferencePlugin.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  MultiSpeciesReferencePlugin& operator=(const MultiSpeciesReferencePlugin& rhs);


   /**
   * Creates and returns a deep copy of this MultiSpeciesReferencePlugin object.
   *
   * @return a (deep) copy of this MultiSpeciesReferencePlugin object.
   */
  virtual MultiSpeciesReferencePlugin* clone () const;


   /**
   * Destructor for MultiSpeciesReferencePlugin.
   */
  virtual ~MultiSpeciesReferencePlugin();


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
   * Returns the ListOfSpeciesTypeComponentMapInProducts in this plugin object.
   *
   * @return ListOfSpeciesTypeComponentMapInProducts object in this plugin object.
   */
  const ListOfSpeciesTypeComponentMapInProducts* getListOfSpeciesTypeComponentMapInProducts () const;


  /**
   * Returns the ListOfSpeciesTypeComponentMapInProducts in this plugin object.
   *
   * @return ListOfSpeciesTypeComponentMapInProducts object in this plugin object.
   */
  ListOfSpeciesTypeComponentMapInProducts* getListOfSpeciesTypeComponentMapInProducts ();


  /**
   * Returns the SpeciesTypeComponentMapInProduct object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the SpeciesTypeComponentMapInProduct to get
   *
   * @return the nth SpeciesTypeComponentMapInProduct in the ListOfSpeciesTypeComponentMapInProducts
   */
  const SpeciesTypeComponentMapInProduct* getSpeciesTypeComponentMapInProduct(unsigned int n) const;


  /**
   * Returns the SpeciesTypeComponentMapInProduct object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the SpeciesTypeComponentMapInProduct to get
   *
   * @return the nth SpeciesTypeComponentMapInProduct in the ListOfSpeciesTypeComponentMapInProducts
   */
  SpeciesTypeComponentMapInProduct* getSpeciesTypeComponentMapInProduct(unsigned int n);


  /**
   * Returns the SpeciesTypeComponentMapInProduct object based on its identifier.
   *
   * @param sid a string representing the id of the SpeciesTypeComponentMapInProduct to get
   *
   * @return SpeciesTypeComponentMapInProduct in the ListOfSpeciesTypeComponentMapInProducts with the given id
   * or NULL if no such SpeciesTypeComponentMapInProduct exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  const SpeciesTypeComponentMapInProduct* getSpeciesTypeComponentMapInProduct(const std::string& sid) const;


  /**
   * Returns the SpeciesTypeComponentMapInProduct object based on its identifier.
   *
   * @param sid a string representing the id of the SpeciesTypeComponentMapInProduct to get
   *
   * @return SpeciesTypeComponentMapInProduct in the ListOfSpeciesTypeComponentMapInProducts with the given id
   * or NULL if no such SpeciesTypeComponentMapInProduct exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  SpeciesTypeComponentMapInProduct* getSpeciesTypeComponentMapInProduct(const std::string& sid);


  /**
   * Adds a copy of the given SpeciesTypeComponentMapInProduct to the ListOfSpeciesTypeComponentMapInProducts in this plugin object.
   *
   * @param speciesTypeComponentMapInProduct the speciesTypeComponentMapInProduct to be added.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int addSpeciesTypeComponentMapInProduct (const SpeciesTypeComponentMapInProduct* speciesTypeComponentMapInProduct);


  /**
   * Creates a new SpeciesTypeComponentMapInProduct object and adds it to the ListOfSpeciesTypeComponentMapInProducts in this plugin object.
   *
   * @return the newly created SpeciesTypeComponentMapInProduct object.
   */
  SpeciesTypeComponentMapInProduct* createSpeciesTypeComponentMapInProduct ();


  /**
   * Removes the nth SpeciesTypeComponentMapInProduct object from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param n the index of the SpeciesTypeComponentMapInProduct to remove
   *
   * @return the SpeciesTypeComponentMapInProduct object removed 
   * or NULL index was out of range.
   */
  SpeciesTypeComponentMapInProduct* removeSpeciesTypeComponentMapInProduct(unsigned int n);


  /**
   * Removes the SpeciesTypeComponentMapInProduct object with the given id from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param sid a string representing the id of the SpeciesTypeComponentMapInProduct to remove
   *
   * @return the SpeciesTypeComponentMapInProduct object removed 
   * or NULL if no such SpeciesTypeComponentMapInProduct exists.
   */
  SpeciesTypeComponentMapInProduct* removeSpeciesTypeComponentMapInProduct(const std::string& sid);


  /**
   * Returns the number of SpeciesTypeComponentMapInProduct objects in this plugin object.
   *
   * @return the number of SpeciesTypeComponentMapInProduct objects in this plugin object.
   */
  unsigned int getNumSpeciesTypeComponentMapInProducts () const;


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

  ListOfSpeciesTypeComponentMapInProducts mSpeciesTypeComponentMapInProducts;

  /** @endcond doxygenLibsbmlInternal */


};




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* MultiSpeciesReferencePlugin_H__ */


