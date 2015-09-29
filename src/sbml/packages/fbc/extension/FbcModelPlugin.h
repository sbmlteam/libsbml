/**
 * @file    FbcModelPlugin.h
 * @brief   Definition of FbcModelPlugin, the plugin class of
 *          fbc package for the Model element.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2015 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class FbcModelPlugin
 * @sbmlbrief{fbc} Extension of Model.
 */


#ifndef FbcModelPlugin_H__
#define FbcModelPlugin_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>

#ifdef __cplusplus

#include <sbml/SBMLErrorLog.h>
#include <sbml/Model.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/sbml/FluxBound.h>
#include <sbml/packages/fbc/sbml/Objective.h>
#include <sbml/packages/fbc/sbml/GeneAssociation.h>
#include <sbml/packages/fbc/sbml/GeneProduct.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN FbcModelPlugin : public SBasePlugin
{
public:

  /**
   * Creates a new FbcModelPlugin
   */
  FbcModelPlugin(const std::string& uri, const std::string& prefix, 
                                 FbcPkgNamespaces* fbcns);


  /**
   * Copy constructor for FbcModelPlugin.
   *
   * @param orig; the FbcModelPlugin instance to copy.
   */
  FbcModelPlugin(const FbcModelPlugin& orig);


   /**
   * Assignment operator for FbcModelPlugin.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  FbcModelPlugin& operator=(const FbcModelPlugin& rhs);


   /**
   * Creates and returns a deep copy of this FbcModelPlugin object.
   *
   * @return a (deep) copy of this FbcModelPlugin object.
   */
  virtual FbcModelPlugin* clone () const;


   /**
   * Destructor for FbcModelPlugin.
   */
  virtual ~FbcModelPlugin();


   //---------------------------------------------------------------
  //
  // overridden virtual functions for reading/writing/checking 
  // elements
  //
  // --------------------------------------------------------

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
  int appendFrom(const Model* model);
  
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
  /** @cond doxygenLibsbmlInternal */
  /**
   * Parses Gene Annotation Extension 
   */
  virtual bool readOtherXML (SBase* parentObject, XMLInputStream& stream);
  /** @endcond */


  //---------------------------------------------------------------


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


  //---------------------------------------------------------------
  //
  // Functions for interacting with the members of the plugin
  //
  //---------------------------------------------------------------

  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the value of the "strict" attribute of this FbcModelPlugin.
   *
   * @return the value of the "strict" attribute of this FbcModelPlugin as a boolean.
   */
  virtual bool getStrict() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * FbcModelPlugin's "strict" attribute has been set.
   *
   * @return @c true if this FbcModelPlugin's "strict" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetStrict() const;


  /**
   * Sets the value of the "strict" attribute of this FbcModelPlugin.
   *
   * @param strict; bool value of the "strict" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setStrict(bool strict);


  /**
   * Unsets the value of the "strict" attribute of this FbcModelPlugin.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetStrict();

 
  /**
   * Returns the ListOfFluxBounds in this plugin object.
   *
   * @return ListOfFluxBounds object in this plugin object.
   */
  const ListOfFluxBounds* getListOfFluxBounds () const;


  /**
   * Returns the ListOfFluxBounds in this plugin object.
   *
   * @return ListOfFluxBounds object in this plugin object.
   */
  ListOfFluxBounds* getListOfFluxBounds ();


  /**
   * Returns the FluxBound object that belongs to the given index. If the
   * index is invalid, @c NULL is returned.
   *
   * @param n the index number of the FluxBound to get.
   *
   * @return the nth FluxBound in the ListOfFluxBounds.
   */
  const FluxBound* getFluxBound (unsigned int n) const;


  /**
   * Returns the FluxBound object that belongs to the given index. If the
   * index is invalid, @c NULL is returned.
   *
   * @param n the index number of the FluxBound to get.
   *
   * @return the nth FluxBound in the ListOfFluxBounds.
   */
  FluxBound* getFluxBound (unsigned int n);


  /**
   * Returns the FluxBound object based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the FluxBound to get.
   * 
   * @return FluxBound in the ListOfFluxBounds with the given @p sid
   * or NULL if no such FluxBound exists.
   *
   * @see getFluxBound(unsigned int n)
   * @see getListOfFluxBounds()
   */
  FluxBound* getFluxBound (const std::string& sid);


  /**
   * Returns the FluxBound object based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the FluxBound to get.
   * 
   * @return FluxBound in the ListOfFluxBounds with the given @p sid 
   * or NULL if no such FluxBound exists.
   *
   * @see getFluxBound(unsigned int n)
   * @see getListOfFluxBounds()
   */
  const FluxBound* getFluxBound (const std::string& sid) const;

  /** 
   * 
   * @param reaction the id of an reaction to find fluxBounds for
   * 
   * @returns a listOfFluxBounds for the given reaction id
   */
  ListOfFluxBounds *  getFluxBoundsForReaction(const std::string& reaction) const;

  /**
   * Adds a copy of the given FluxBound object to the list of FluxBounds.
   *
   * @param bound the FluxBound object to be added to the list of FluxBounds.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */ 
  int addFluxBound (const FluxBound* bound);


  /**
   * Creates a new FluxBound object and adds it to the list of FluxBound objects
   * and returns it.
   *
   * @return a newly created FluxBound object
   */
  FluxBound* createFluxBound();


  /**
   * Removes the nth FluxBound object from this plugin object and
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   *  deleting it.
   *
   * @param n the index of the FluxBound object to remove
   *
   * @return the FluxBound object removed.  As mentioned above, the 
   * caller owns the returned object. @c NULL is returned if the 
   * given index is out of range.
   */
  FluxBound* removeFluxBound (unsigned int n);


  /**
   * Removes the FluxBound object with the given @p sid attribute from 
   * this plugin object and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param sid the id attribute of the FluxBound object to remove
   *
   * @return the FluxBound object removed.  As mentioned above, the 
   * caller owns the returned object. @c NULL is returned if the 
   * given index is out of range.
   */
  FluxBound* removeFluxBound (const std::string& sid);


  /**
   * Returns the number of FluxBound object in this plugin object.
   *
   * @return the number of FluxBound object in this plugin object.
   */
  unsigned int getNumFluxBounds() const;


  /**
   * Returns the  "ListOfObjectives" in this FbcModelPlugin object.
   *
   * @return the "ListOfObjectives" attribute of this FbcModelPlugin.
   */
  const ListOfObjectives* getListOfObjectives() const;


  /**
   * Returns the  "ListOfObjectives" in this FbcModelPlugin object.
   *
   * @return the "ListOfObjectives" attribute of this FbcModelPlugin.
   */
  ListOfObjectives* getListOfObjectives();


  /**
   * Get a Objective from the ListOfObjectives.
   *
   * @param n the index number of the Objective to get.
   *
   * @return the nth Objective in the ListOfObjectives within this FbcModelPlugin.
   *
   * @see getNumObjectives()
   */
  Objective* getObjective(unsigned int n);


  /**
   * Get a Objective from the ListOfObjectives.
   *
   * @param n the index number of the Objective to get.
   *
   * @return the nth Objective in the ListOfObjectives within this FbcModelPlugin.
   *
   * @see getNumObjectives()
   */
  const Objective* getObjective(unsigned int n) const;


  /**
   * Get a Objective from the ListOfObjectives
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Objective to get.
   *
   * @return the Objective in the ListOfObjectives
   * with the given id or NULL if no such
   * Objective exists.
   *
   * @see getObjective(unsigned int n)
   *
   * @see getNumObjectives()
   */
  Objective* getObjective(const std::string& sid);


  /**
   * Get a Objective from the ListOfObjectives
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the Objective to get.
   *
   * @return the Objective in the ListOfObjectives
   * with the given id or NULL if no such
   * Objective exists.
   *
   * @see getObjective(unsigned int n)
   *
   * @see getNumObjectives()
   */
  const Objective* getObjective(const std::string& sid) const;


  /**
   * Adds a copy the given "Objective" to this FbcModelPlugin.
   *
   * @param o; the Objective object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addObjective(const Objective* o);


  /**
   * Get the number of Objective objects in this FbcModelPlugin.
   *
   * @return the number of Objective objects in this FbcModelPlugin
   */
  unsigned int getNumObjectives() const;


  /**
   * Creates a new Objective object, adds it to this FbcModelPlugins
   * ListOfObjectives and returns the Objective object created. 
   *
   * @return a new Objective object instance
   *
   * @see addObjective(const Objective* o)
   */
  Objective* createObjective();


  /**
   * Removes the nth Objective from the ListOfObjectives within this FbcModelPlugin.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the Objective to remove.
   *
   * @see getNumObjectives()
   */
  Objective* removeObjective(unsigned int n);


  /**
   * Removes the Objective with the given identifier from the ListOfObjectives within this FbcModelPlugin
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the Objective to remove.
   *
   * @return the Objective removed. As mentioned above, the caller owns the
   * returned item.
   */
  Objective* removeObjective(const std::string& sid);


  /** 
   * Returns the current active objective. 
   */
  Objective* getActiveObjective();

  /** 
   * Returns the current active objective. 
   */
  const Objective *getActiveObjective() const;
  
  /** 
   * Sets the id of the active objective.
   */
  int setActiveObjectiveId(const std::string& objectiveId);

  /** 
   * returns the id of the current active objective.
   */  
  std::string getActiveObjectiveId() const;
  
  /** 
   * Unsets the active objective.
   */  
  void unsetActiveObjectiveId();
  /**
   * Returns the  "ListOfGeneProducts" in this FbcModelPlugin object.
   *
   * @return the "ListOfGeneProducts" attribute of this FbcModelPlugin.
   */
  const ListOfGeneProducts* getListOfGeneProducts() const;


  /**
   * Returns the  "ListOfGeneProducts" in this FbcModelPlugin object.
   *
   * @return the "ListOfGeneProducts" attribute of this FbcModelPlugin.
   */
  ListOfGeneProducts* getListOfGeneProducts();


  /**
   * Get a GeneProduct from the ListOfGeneProducts.
   *
   * @param n the index number of the GeneProduct to get.
   *
   * @return the nth GeneProduct in the ListOfGeneProducts within this FbcModelPlugin.
   *
   * @see getNumGeneProducts()
   */
  GeneProduct* getGeneProduct(unsigned int n);


  /**
   * Get a GeneProduct from the ListOfGeneProducts.
   *
   * @param n the index number of the GeneProduct to get.
   *
   * @return the nth GeneProduct in the ListOfGeneProducts within this FbcModelPlugin.
   *
   * @see getNumGeneProducts()
   */
  const GeneProduct* getGeneProduct(unsigned int n) const;


  /**
   * Get a GeneProduct from the ListOfGeneProducts
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the GeneProduct to get.
   *
   * @return the GeneProduct in the ListOfGeneProducts
   * with the given id or NULL if no such
   * GeneProduct exists.
   *
   * @see getGeneProduct(unsigned int n)
   *
   * @see getNumGeneProducts()
   */
  GeneProduct* getGeneProduct(const std::string& sid);

  /**
  * Get a GeneProduct from the ListOfGeneProducts
  * based on its label.
  *
  * @param label a string representing the label
  * of the GeneProduct to get.
  *
  * @return the GeneProduct in the ListOfGeneProducts
  * with the given label or NULL if no such
  * GeneProduct exists.
  *
  * @see getGeneProduct(unsigned int n)
  *
  * @see getNumGeneProducts()
  */
  GeneProduct* getGeneProductByLabel(const std::string& label);


  /**
   * Get a GeneProduct from the ListOfGeneProducts
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the GeneProduct to get.
   *
   * @return the GeneProduct in the ListOfGeneProducts
   * with the given id or NULL if no such
   * GeneProduct exists.
   *
   * @see getGeneProduct(unsigned int n)
   *
   * @see getNumGeneProducts()
   */
  const GeneProduct* getGeneProduct(const std::string& sid) const;


  /**
   * Adds a copy the given "GeneProduct" to this FbcModelPlugin.
   *
   * @param gp; the GeneProduct object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addGeneProduct(const GeneProduct* gp);


  /**
   * Get the number of GeneProduct objects in this FbcModelPlugin.
   *
   * @return the number of GeneProduct objects in this FbcModelPlugin
   */
  unsigned int getNumGeneProducts() const;


  /**
   * Creates a new GeneProduct object, adds it to this FbcModelPlugins
   * ListOfGeneProducts and returns the GeneProduct object created. 
   *
   * @return a new GeneProduct object instance
   *
   * @see addGeneProduct(const GeneProduct* gp)
   */
  GeneProduct* createGeneProduct();


  /**
   * Removes the nth GeneProduct from the ListOfGeneProducts within this FbcModelPlugin.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the GeneProduct to remove.
   *
   * @see getNumGeneProducts()
   */
  GeneProduct* removeGeneProduct(unsigned int n);


  /**
   * Removes the GeneProduct with the given identifier from the ListOfGeneProducts within this FbcModelPlugin
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the GeneProduct to remove.
   *
   * @return the GeneProduct removed. As mentioned above, the caller owns the
   * returned item.
   */
  GeneProduct* removeGeneProduct(const std::string& sid);

  /**
   * Returns the ListOfObjectives in this plugin object.
   *
   * @return ListOfObjectives object in this plugin object.
   */
  const ListOfGeneAssociations* getListOfGeneAssociations () const;

  /**
   * Returns the ListOfGeneAssociations in this plugin object.
   *
   * @return ListOfGeneAssociations object in this plugin object.
   */
  ListOfGeneAssociations* getListOfGeneAssociations ();

  
  /**
   * Returns the GeneAssociation object that belongs to the given index. If the
   * index is invalid, @c NULL is returned.
   *
   * @param n the index number of the GeneAssociation to get.
   *
   * @return the nth GeneAssociation in the ListOfGeneAssociations.
   */
  const GeneAssociation* getGeneAssociation (unsigned int n) const;


  /**
   * Returns the GeneAssociation object that belongs to the given index. If the
   * index is invalid, @c NULL is returned.
   *
   * @param n the index number of the GeneAssociation to get.
   *
   * @return the nth GeneAssociation in the ListOfGeneAssociations.
   */
  GeneAssociation* getGeneAssociation (unsigned int n);


  /**
   * Returns the GeneAssociation object based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the GeneAssociation to get.
   * 
   * @return GeneAssociation in the ListOfGeneAssociations with the given @p sid
   * or NULL if no such GeneAssociation exists.
   *
   * @see getGeneAssociation(unsigned int n)
   * @see getListOfGeneAssociations()
   */
  GeneAssociation* getGeneAssociation (const std::string& sid);


  /**
   * Returns the GeneAssociation object based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the GeneAssociation to get.
   * 
   * @return GeneAssociation in the ListOfGeneAssociations with the given @p sid 
   * or NULL if no such GeneAssociation exists.
   *
   * @see getGeneAssociation(unsigned int n)
   * @see getListOfGeneAssociations()
   */
  const GeneAssociation* getGeneAssociation (const std::string& sid) const;


  /**
   * Adds a copy of the given GeneAssociation object to the list of GeneAssociations.
   *
   * @param association the GeneAssociation object to be added to the list of GeneAssociations.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */ 
  int addGeneAssociation (const GeneAssociation* association);


  /**
   * Creates a new GeneAssociation object and adds it to the list of GeneAssociation objects
   * and returns it.
   *
   * @return a newly created GeneAssociation object
   */
  GeneAssociation* createGeneAssociation();


  /**
   * Removes the nth GeneAssociation object from this plugin object and
   * returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   *  deleting it.
   *
   * @param n the index of the GeneAssociation object to remove
   *
   * @return the GeneAssociation object removed.  As mentioned above, the 
   * caller owns the returned object. @c NULL is returned if the 
   * given index is out of range.
   */
  GeneAssociation* removeGeneAssociation (unsigned int n);


  /**
   * Removes the GeneAssociation object with the given @p sid attribute from 
   * this plugin object and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param sid the id attribute of the GeneAssociation object to remove
   *
   * @return the GeneAssociation object removed.  As mentioned above, the 
   * caller owns the returned object. @c NULL is returned if the 
   * given index is out of range.
   */
  GeneAssociation* removeGeneAssociation (const std::string& sid);


  /**
   * Returns the number of GeneAssociation object in this plugin object.
   *
   * @return the number of GeneAssociation object in this plugin object.
   */
  int getNumGeneAssociations() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */
  /**
  * Sets the *parent* of this SBML object to child SBML objects (if any).
  * (Creates a child-parent relationship by the parent)
  *
  * @see setSBMLDocument
  * @see enablePackageInternal
  */
  virtual void connectToChild();
  /** @endcond */


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
   * Parse L2 annotation if supported
   *
   */
  virtual void parseAnnotation(SBase *parentObject, XMLNode *annotation);
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */

  bool          mStrict;
  bool          mIsSetStrict;
  ListOfObjectives   mObjectives;
  ListOfGeneProducts   mGeneProducts;
  ListOfFluxBounds mBounds;
  ListOfGeneAssociations mAssociations;

  /** @endcond doxygenLibsbmlInternal */


};




LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Appends a copy of the given FluxBound_t structure to the given FbcModelPlugin_t
 * structure.
 *
 * @param fmp the FbcModelPlugin_t structure to which the FluxBound_t should be
 * added
 *
 * @param fb a FluxBound_t structure to add
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof FbcModelPlugin_t
 */
LIBSBML_EXTERN
int
FbcModelPlugin_addFluxBound(SBasePlugin_t * fmp, FluxBound_t * fb);


/**
 * Return a specific FluxBound_t structure of the given FbcModelPlugin_t.
 *
 * @param fmp the FbcModelPlugin_t structure to use
 *
 * @param n an integer, the index of the FluxBound_t structure to return
 * 
 * @return the nth FluxBound_t of the given FbcModelPlugin_t, or @c NULL if no such FluxBound_t exists.
 *
 * @memberof FbcModelPlugin_t
 */
LIBSBML_EXTERN
FluxBound_t *
FbcModelPlugin_getFluxBound(SBasePlugin_t * fmp, unsigned int n);


/**
 * Returns the number of EventAssignment_t structures attached to the given
 * FbcModelPlugin_t.
 *
 * @param fmp the FbcModelPlugin_t structure to use
 * 
 * @return the number of EventAssignment_t structures in the given FbcModelPlugin_t.
 *
 * @memberof FbcModelPlugin_t
 */
LIBSBML_EXTERN
unsigned int
FbcModelPlugin_getNumFluxBounds(SBasePlugin_t * fmp);


/**
 * Appends a copy of the given Objective_t structure to the given FbcModelPlugin_t
 * structure.
 *
 * @param fmp the FbcModelPlugin_t structure to which the Objective_t should be
 * added
 *
 * @param obj an Objective_t structure to add
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 *
 * @memberof FbcModelPlugin_t
 */
LIBSBML_EXTERN
int
FbcModelPlugin_addObjective(SBasePlugin_t * fmp, Objective_t * obj);


/**
 * Return a specific Objective_t structure of the given FbcModelPlugin_t.
 *
 * @param fmp the FbcModelPlugin_t structure to use
 *
 * @param n an integer, the index of the Objective_t structure to return
 * 
 * @return the nth Objective_t of the given FbcModelPlugin_t, or @c NULL if no such Objective_t exists.
 *
 * @memberof FbcModelPlugin_t
 */
LIBSBML_EXTERN
Objective_t *
FbcModelPlugin_getObjective(SBasePlugin_t * fmp, unsigned int n);


/**
 * Returns the number of Objective_t structures attached to the given
 * FbcModelPlugin_t.
 *
 * @param fmp the FbcModelPlugin_t structure to use
 * 
 * @return the number of Objective_t structures in the given FbcModelPlugin_t.
 *
 * @memberof FbcModelPlugin_t
 */
LIBSBML_EXTERN
unsigned int
FbcModelPlugin_getNumObjectives(SBasePlugin_t * fmp);


/**
 * Takes a FbcModelPlugin_t structure and returns the id of the current activeObjective.
 *
 * @param fmp the FbcModelPlugin_t whose id of the current activeObjective is sought.
 *
 * @return the id of the current activeObjective of the given FbcModelPlugin_t, as a pointer to a string.
 *
 * @memberof FbcModelPlugin_t
 */
LIBSBML_EXTERN
char *
FbcModelPlugin_getActiveObjectiveId(SBasePlugin_t * fmp);


/**
 * Sets the activeObjective of the given FbcModelPlugin_t to a copy of @p activeObjective.
 *
 * @param fmp the FbcModelPlugin_t structure to set
 * @param activeObjective the activeObjective to assign to the given FbcModelPlugin_t's "activeObjective" attribute.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 *
 * @note Using this function with the name set to NULL is equivalent to
 * unsetting the "activeObjective" attribute.
 *
 * @memberof FbcModelPlugin_t
 */
LIBSBML_EXTERN
int
FbcModelPlugin_setActiveObjectiveId(SBasePlugin_t * fmp, const char * activeObjective);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif /* FbcModelPlugin_H__ */


