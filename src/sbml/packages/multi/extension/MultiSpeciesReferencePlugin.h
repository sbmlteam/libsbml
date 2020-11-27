/**
 * @file:   MultiSpeciesReferencePlugin.h
 * @brief:  Implementation of the MultiSpeciesReferencePlugin class
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
 * @class MultiSpeciesReferencePlugin
 * @sbmlbrief{multi} Extension of SpeciesReference for the "multi" package.
 *
 * The MultiSpeciesReferencePlugin class inherits from the
 * MultiSimpleSpeciesReferencePlugin class, and extends the SpeciesReference
 * class to establish component mappings between the reactant species and the
 * product species when the mappings cannot be inferred from the ids of the
 * SpeciesTypeInstance objects. A MultiSpeciesReferencePlugin object defines
 * an optional ListOfSpeciesTypeComponentMapInProducts child. Only a reaction
 * product can contain the ListOfSpeciesTypeComponentMapInProducts child and
 * it is not necessary to store the mappings again in the reactants.
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
   * Creates a new MultiSpeciesReferencePlugin object.
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
  MultiSpeciesReferencePlugin(const std::string& uri, const std::string& prefix,
                              MultiPkgNamespaces* multins);


  /**
   * Copy constructor for MultiSpeciesReferencePlugin.
   *
   * @param orig the MultiSpeciesReferencePlugin instance to copy.
   */
  MultiSpeciesReferencePlugin(const MultiSpeciesReferencePlugin& orig);


  /**
   * Assignment operator for MultiSpeciesReferencePlugin.
   *
   * @param rhs the object whose values are used as the basis
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
   * Returns the ListOfSpeciesTypeComponentMapInProducts object.
   *
   * @return ListOfSpeciesTypeComponentMapInProducts object within this
   * "multi" SpeciesReference object.
   */
  const ListOfSpeciesTypeComponentMapInProducts* getListOfSpeciesTypeComponentMapInProducts () const;


  /**
   * Returns the ListOfSpeciesTypeComponentMapInProducts object.
   *
   * @return ListOfSpeciesTypeComponentMapInProducts object within this
   * "multi" SpeciesReference object.
   */
  ListOfSpeciesTypeComponentMapInProducts* getListOfSpeciesTypeComponentMapInProducts ();


  /**
   * Returns the nth SpeciesTypeComponentMapInProduct object.
   *
   * @param n the index number of the SpeciesTypeComponentMapInProduct to get.
   *
   * @return the nth SpeciesTypeComponentMapInProduct in the
   * ListOfSpeciesTypeComponentMapInProducts.  If the index is invalid, NULL
   * is returned.
   */
  const SpeciesTypeComponentMapInProduct* getSpeciesTypeComponentMapInProduct(unsigned int n) const;


  /**
   * Returns the nth SpeciesTypeComponentMapInProduct object.
   *
   * @param n the index number of the SpeciesTypeComponentMapInProduct to get.
   *
   * @return the nth SpeciesTypeComponentMapInProduct in the
   * ListOfSpeciesTypeComponentMapInProducts. If the index is invalid, NULL
   * is returned.
   */
  SpeciesTypeComponentMapInProduct* getSpeciesTypeComponentMapInProduct(unsigned int n);


  /**
   * Returns the SpeciesTypeComponentMapInProduct object with the given
   * identifier @p sid.
   *
   * @param sid a string representing the id of the
   * SpeciesTypeComponentMapInProduct to get.
   *
   * @return the SpeciesTypeComponentMapInProduct object within the
   * ListOfSpeciesTypeComponentMapInProducts with the given id, or @c NULL if no
   * such SpeciesTypeComponentMapInProduct exists.
   */
  const SpeciesTypeComponentMapInProduct* getSpeciesTypeComponentMapInProduct(const std::string& sid) const;


  /**
   * Returns the SpeciesTypeComponentMapInProduct object with the given
   * identifier @p sid.
   *
   * @param sid a string representing the id of the
   * SpeciesTypeComponentMapInProduct to get.
   *
   * @return the SpeciesTypeComponentMapInProduct object within the
   * ListOfSpeciesTypeComponentMapInProducts with the given id, or @c NULL if no
   * such SpeciesTypeComponentMapInProduct exists.
   */
  SpeciesTypeComponentMapInProduct* getSpeciesTypeComponentMapInProduct(const std::string& sid);


  /**
   * Adds a copy of the given SpeciesTypeComponentMapInProduct to the
   * ListOfSpeciesTypeComponentMapInProducts.
   *
   * @param speciesTypeComponentMapInProduct the
   * speciesTypeComponentMapInProduct to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int addSpeciesTypeComponentMapInProduct (const SpeciesTypeComponentMapInProduct* speciesTypeComponentMapInProduct);


  /**
   * Creates a new SpeciesTypeComponentMapInProduct object and adds it to the
   * ListOfSpeciesTypeComponentMapInProducts.
   *
   * @return the newly created SpeciesTypeComponentMapInProduct object.  Note
   * that the caller owns the returned object and is responsible for deleting
   * it.
   */
  SpeciesTypeComponentMapInProduct* createSpeciesTypeComponentMapInProduct ();


  /**
   * Removes the nth SpeciesTypeComponentMapInProduct object and returns a
   * pointer to it.
   *
   * @param n the index of the SpeciesTypeComponentMapInProduct to remove.
   *
   * @return the SpeciesTypeComponentMapInProduct object removed or @c NULL
   * index was out of range.  Note that the caller owns the returned object
   * and is responsible for deleting it.
   */
  SpeciesTypeComponentMapInProduct* removeSpeciesTypeComponentMapInProduct(unsigned int n);


  /**
   * Removes the SpeciesTypeComponentMapInProduct object with the given id
   * and returns a pointer to it.
   *
   * @param sid a string representing the id of the
   * SpeciesTypeComponentMapInProduct to remove.
   *
   * @return the SpeciesTypeComponentMapInProduct object removed or @c NULL if
   * no such SpeciesTypeComponentMapInProduct exists.  Note that the caller
   * owns the returned object and is responsible for deleting it.
   */
  SpeciesTypeComponentMapInProduct* removeSpeciesTypeComponentMapInProduct(const std::string& sid);


  /**
   * Returns the number of SpeciesTypeComponentMapInProduct objects.
   *
   * @return the number of SpeciesTypeComponentMapInProduct objects.
   */
  unsigned int getNumSpeciesTypeComponentMapInProducts () const;


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

  ListOfSpeciesTypeComponentMapInProducts mSpeciesTypeComponentMapInProducts;

  /** @endcond */

};




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */


#ifndef SWIG


LIBSBML_CPP_NAMESPACE_BEGIN


BEGIN_C_DECLS


/**
 * Returns a ListOf_t * containing SpeciesTypeComponentMapInProduct_t objects
 * from this MultiSpeciesReferencePlugin_t.
 *
 * @param msrp the MultiSpeciesReferencePlugin_t structure whose
 * ListOfSpeciesTypeComponentMapsInProduct is sought.
 *
 * @return the ListOfSpeciesTypeComponentMapsInProduct from this
 * MultiSpeciesReferencePlugin_t as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see MultiSpeciesReferencePlugin_addSpeciesTypeComponentMapInProduct()
 * @see MultiSpeciesReferencePlugin_createSpeciesTypeComponentMapInProduct()
 * @see MultiSpeciesReferencePlugin_getSpeciesTypeComponentMapInProductById()
 * @see MultiSpeciesReferencePlugin_getSpeciesTypeComponentMapInProduct()
 * @see MultiSpeciesReferencePlugin_getNumSpeciesTypeComponentMapInProducts()
 * @see
 * MultiSpeciesReferencePlugin_removeSpeciesTypeComponentMapInProductById()
 * @see MultiSpeciesReferencePlugin_removeSpeciesTypeComponentMapInProduct()
 *
 * @memberof MultiSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesReferencePlugin_getListOfSpeciesTypeComponentMapInProducts(MultiSpeciesReferencePlugin_t*
  msrp);


/**
 * Get a SpeciesTypeComponentMapInProduct_t from the
 * MultiSpeciesReferencePlugin_t.
 *
 * @param msrp the MultiSpeciesReferencePlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * SpeciesTypeComponentMapInProduct_t to retrieve.
 *
 * @return the nth SpeciesTypeComponentMapInProduct_t in the
 * ListOfSpeciesTypeComponentMapsInProduct within this
 * MultiSpeciesReferencePlugin.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t*
MultiSpeciesReferencePlugin_getSpeciesTypeComponentMapInProduct(
  MultiSpeciesReferencePlugin_t*
  msrp,
  unsigned int
  n);


/**
 * Get a SpeciesTypeComponentMapInProduct_t from the
 * MultiSpeciesReferencePlugin_t based on its identifier.
 *
 * @param msrp the MultiSpeciesReferencePlugin_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * SpeciesTypeComponentMapInProduct_t to retrieve.
 *
 * @return the SpeciesTypeComponentMapInProduct_t in the
 * ListOfSpeciesTypeComponentMapsInProduct within this
 * MultiSpeciesReferencePlugin with the given @p sid or @c NULL if no such
 * SpeciesTypeComponentMapInProduct_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t*
MultiSpeciesReferencePlugin_getSpeciesTypeComponentMapInProductById(
  MultiSpeciesReferencePlugin_t* msrp, const char *sid);


/**
 * Adds a copy of the given SpeciesTypeComponentMapInProduct_t to this
 * MultiSpeciesReferencePlugin_t.
 *
 * @param msrp the MultiSpeciesReferencePlugin_t structure to which the
 * SpeciesTypeComponentMapInProduct_t should be added.
 *
 * @param stcmip the SpeciesTypeComponentMapInProduct_t object to add.
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
 * @memberof MultiSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
int
MultiSpeciesReferencePlugin_addSpeciesTypeComponentMapInProduct(
  MultiSpeciesReferencePlugin_t* msrp, const SpeciesTypeComponentMapInProduct_t* stcmip);


/**
 * Get the number of SpeciesTypeComponentMapInProduct_t objects in this
 * MultiSpeciesReferencePlugin_t.
 *
 * @param msrp the MultiSpeciesReferencePlugin_t structure to query.
 *
 * @return the number of SpeciesTypeComponentMapInProduct_t objects in this
 * MultiSpeciesReferencePlugin_t.
 *
 * @memberof MultiSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesReferencePlugin_getNumSpeciesTypeComponentMapInProducts(MultiSpeciesReferencePlugin_t*
  msrp);


/**
 * Creates a new SpeciesTypeComponentMapInProduct_t object, adds it to this
 * MultiSpeciesReferencePlugin_t object and returns the
 * SpeciesTypeComponentMapInProduct_t object created.
 *
 * @param msrp the MultiSpeciesReferencePlugin_t structure to which the
 * SpeciesTypeComponentMapInProduct_t should be added.
 *
 * @return a new SpeciesTypeComponentMapInProduct_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t*
MultiSpeciesReferencePlugin_createSpeciesTypeComponentMapInProduct(MultiSpeciesReferencePlugin_t*
  msrp);


/**
 * Removes the nth SpeciesTypeComponentMapInProduct_t from this
 * MultiSpeciesReferencePlugin_t and returns a pointer to it.
 *
 * @param msrp the MultiSpeciesReferencePlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * SpeciesTypeComponentMapInProduct_t to remove.
 *
 * @return a pointer to the nth SpeciesTypeComponentMapInProduct_t in this
 * MultiSpeciesReferencePlugin_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t*
MultiSpeciesReferencePlugin_removeSpeciesTypeComponentMapInProduct(
  MultiSpeciesReferencePlugin_t* msrp, unsigned int n);


/**
 * Removes the SpeciesTypeComponentMapInProduct_t from this
 * MultiSpeciesReferencePlugin_t based on its identifier and returns a pointer
 * to it.
 *
 * @param msrp the MultiSpeciesReferencePlugin_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * SpeciesTypeComponentMapInProduct_t to remove.
 *
 * @return the SpeciesTypeComponentMapInProduct_t in this
 * MultiSpeciesReferencePlugin_t based on the identifier or NULL if no such
 * SpeciesTypeComponentMapInProduct_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentMapInProduct_t*
MultiSpeciesReferencePlugin_removeSpeciesTypeComponentMapInProductById(
  MultiSpeciesReferencePlugin_t* msrp, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !MultiSpeciesReferencePlugin_H__ */
