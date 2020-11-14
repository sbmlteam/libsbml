/**
 * @file:   MultiModelPlugin.h
 * @brief:  Implementation of the MultiModelPlugin class
 * @author: SBMLTeam
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
 *
 * @class MultiModelPlugin
 * @sbmlbrief{multi} Extension of Model.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * The MultiModelPlugin object is used to extend the standard SBML Model
 * object to allow a ListOfSpeciesTypes child.
 */


#ifndef MultiModelPlugin_H__
#define MultiModelPlugin_H__


#include <sbml/common/extern.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/multi/sbml/MultiSpeciesType.h>
#include <sbml/packages/multi/sbml/BindingSiteSpeciesType.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN MultiModelPlugin : public SBasePlugin
{
public:

  /**
   * Creates a new MultiModelPlugin object using the given parameters.
   *
   * @copydetails doc_what_are_xmlnamespaces
   *
   * @copydetails doc_what_are_sbmlnamespaces
   *
   * @param uri the URI of the SBML Level&nbsp;3 package implemented by
   * this libSBML package extension.
   *
   * @param prefix the XML namespace prefix being used for the package.
   *
   * @param multins the namespaces object for the package.
   */
  MultiModelPlugin(const std::string& uri, const std::string& prefix, 
                                 MultiPkgNamespaces* multins);


  /**
   * Copy constructor for MultiModelPlugin.
   *
   * @param orig the MultiModelPlugin instance to copy.
   */
  MultiModelPlugin(const MultiModelPlugin& orig);


  /**
   * Assignment operator for MultiModelPlugin.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment.
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
   * XMLInputStream or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses must override this method to write out their contained
   * SBML objects as XML elements if they have their specific elements.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /**
   * Returns @c true if this object has all the required elements.
   *
   * @return @c true if this object has all the elements required by the
   * package specification; otherwise, @c false will be returned.
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
   * @param filter a pointer to an ElementFilter, which causes the function 
   * to return only elements that match a particular set of constraints.  
   * If NULL (the default), the function will return all child objects.
   *
   * @return a List of pointers to all child objects.
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
   * If the index @p n is invalid, @c NULL is returned.
   */
  const MultiSpeciesType* getMultiSpeciesType(unsigned int n) const;


  /**
   * Returns the MultiSpeciesType object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the MultiSpeciesType to get
   *
   * @return the nth MultiSpeciesType in the ListOfMultiSpeciesTypes
   * If the index @p n is invalid, @c NULL is returned.
   */
  MultiSpeciesType* getMultiSpeciesType(unsigned int n);


  /**
   * Returns the MultiSpeciesType object based on its identifier.
   *
   * @param sid a string representing the id of the MultiSpeciesType to get
   *
   * @return MultiSpeciesType in the ListOfMultiSpeciesTypes with the given id
   * or @c NULL if no such MultiSpeciesType exists.
   */
  const MultiSpeciesType* getMultiSpeciesType(const std::string& sid) const;


  /**
   * Returns the MultiSpeciesType object based on its identifier.
   *
   * @param sid a string representing the id of the MultiSpeciesType to get
   *
   * @return MultiSpeciesType in the ListOfMultiSpeciesTypes with the given id
   * or @c NULL if no such MultiSpeciesType exists.
   */
  MultiSpeciesType* getMultiSpeciesType(const std::string& sid);


  /**
   * Adds a copy of the given MultiSpeciesType to the ListOfMultiSpeciesTypes in this plugin object.
   *
   * @param multiSpeciesType the multiSpeciesType to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
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
   * or @c NULL index was out of range.
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
   * or @c NULL if no such MultiSpeciesType exists.
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


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  virtual void connectToParent (SBase* sbase);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  virtual bool accept (SBMLVisitor& v) const;

  /** @endcond */


protected:

  /** @cond doxygenLibsbmlInternal */

  ListOfMultiSpeciesTypes mListOfMultiSpeciesTypes;

  /** @endcond */


};




LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Returns a ListOf_t * containing MultiSpeciesType_t objects from this
 * MultiModelPlugin_t.
 *
 * @param mmp the MultiModelPlugin_t structure whose ListOfMultiSpeciesTypes is
 * sought.
 *
 * @return the ListOfMultiSpeciesTypes from this MultiModelPlugin_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see MultiModelPlugin_addMultiSpeciesType()
 * @see MultiModelPlugin_createMultiSpeciesType()
 * @see MultiModelPlugin_getMultiSpeciesTypeById()
 * @see MultiModelPlugin_getMultiSpeciesType()
 * @see MultiModelPlugin_getNumMultiSpeciesTypes()
 * @see MultiModelPlugin_removeMultiSpeciesTypeById()
 * @see MultiModelPlugin_removeMultiSpeciesType()
 *
 * @memberof MultiModelPlugin_t
 */
LIBSBML_EXTERN
ListOf_t*
MultiModelPlugin_getListOfMultiSpeciesTypes(MultiModelPlugin_t* mmp);


/**
 * Get a MultiSpeciesType_t from the MultiModelPlugin_t.
 *
 * @param mmp the MultiModelPlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the MultiSpeciesType_t to
 * retrieve.
 *
 * @return the nth MultiSpeciesType_t in the ListOfMultiSpeciesTypes within
 * this MultiModelPlugin.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiModelPlugin_t
 */
LIBSBML_EXTERN
MultiSpeciesType_t*
MultiModelPlugin_getMultiSpeciesType(MultiModelPlugin_t* mmp, unsigned int n);


/**
 * Get a MultiSpeciesType_t from the MultiModelPlugin_t based on its
 * identifier.
 *
 * @param mmp the MultiModelPlugin_t structure to search.
 *
 * @param sid a string representing the identifier of the MultiSpeciesType_t to
 * retrieve.
 *
 * @return the MultiSpeciesType_t in the ListOfMultiSpeciesTypes within this
 * MultiModelPlugin with the given @p sid or @c NULL if no such
 * MultiSpeciesType_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiModelPlugin_t
 */
LIBSBML_EXTERN
MultiSpeciesType_t*
MultiModelPlugin_getMultiSpeciesTypeById(MultiModelPlugin_t* mmp,
  const char *sid);


/**
 * Adds a copy of the given MultiSpeciesType_t to this MultiModelPlugin_t.
 *
 * @param mmp the MultiModelPlugin_t structure to which the MultiSpeciesType_t
 * should be added.
 *
 * @param mst the MultiSpeciesType_t object to add.
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
 * @memberof MultiModelPlugin_t
 */
LIBSBML_EXTERN
int
MultiModelPlugin_addMultiSpeciesType(MultiModelPlugin_t* mmp,
  const MultiSpeciesType_t* mst);


/**
 * Get the number of MultiSpeciesType_t objects in this MultiModelPlugin_t.
 *
 * @param mmp the MultiModelPlugin_t structure to query.
 *
 * @return the number of MultiSpeciesType_t objects in this MultiModelPlugin_t.
 *
 * @memberof MultiModelPlugin_t
 */
LIBSBML_EXTERN
unsigned int
MultiModelPlugin_getNumMultiSpeciesTypes(MultiModelPlugin_t* mmp);


/**
 * Creates a new MultiSpeciesType_t object, adds it to this MultiModelPlugin_t
 * object and returns the MultiSpeciesType_t object created.
 *
 * @param mmp the MultiModelPlugin_t structure to which the MultiSpeciesType_t
 * should be added.
 *
 * @return a new MultiSpeciesType_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiModelPlugin_t
 */
LIBSBML_EXTERN
MultiSpeciesType_t*
MultiModelPlugin_createMultiSpeciesType(MultiModelPlugin_t* mmp);


/**
 * Removes the nth MultiSpeciesType_t from this MultiModelPlugin_t and returns
 * a pointer to it.
 *
 * @param mmp the MultiModelPlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the MultiSpeciesType_t to
 * remove.
 *
 * @return a pointer to the nth MultiSpeciesType_t in this MultiModelPlugin_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiModelPlugin_t
 */
LIBSBML_EXTERN
MultiSpeciesType_t*
MultiModelPlugin_removeMultiSpeciesType(MultiModelPlugin_t* mmp,
  unsigned int n);


/**
 * Removes the MultiSpeciesType_t from this MultiModelPlugin_t based on its
 * identifier and returns a pointer to it.
 *
 * @param mmp the MultiModelPlugin_t structure to search.
 *
 * @param sid a string representing the identifier of the MultiSpeciesType_t to
 * remove.
 *
 * @return the MultiSpeciesType_t in this MultiModelPlugin_t based on the
 * identifier or NULL if no such MultiSpeciesType_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiModelPlugin_t
 */
LIBSBML_EXTERN
MultiSpeciesType_t*
MultiModelPlugin_removeMultiSpeciesTypeById(MultiModelPlugin_t* mmp,
  const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !MultiModelPlugin_H__ */




