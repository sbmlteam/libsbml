/**
 * @file:   MultiSpeciesPlugin.h
 * @brief:  Implementation of the MultiSpeciesPlugin class
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
 * @class MultiSpeciesPlugin
 * @sbmlbrief{multi} Extension of Species for the "multi" package.
 *
 * The MultiSpeciesPlugin class extends the Species class to have a new
 * attribute "speciesType", and two extra optional ListOfOutwardBindingSites
 * and ListOfSpeciesFeatures children. A species may have a
 * ListOfOutwardBindingSites child and/or a ListOfSpeciesFeatures child only
 * when its speciesType attribute has been defined.  The relationship among
 * the elements of a ListOfOutwardBindingSites or a ListOfSpeciesFeatures is
 * "and".
 */

#ifndef MultiSpeciesPlugin_H__
#define MultiSpeciesPlugin_H__


#include <sbml/common/extern.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/multi/sbml/OutwardBindingSite.h>
#include <sbml/packages/multi/sbml/SpeciesFeature.h>
#include <sbml/packages/multi/sbml/SubListOfSpeciesFeatures.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN MultiSpeciesPlugin : public SBasePlugin
{
public:

  /**
   * Creates a new MultiSpeciesPlugin object.
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
  MultiSpeciesPlugin(const std::string& uri, const std::string& prefix, 
                     MultiPkgNamespaces* multins);


  /**
   * Copy constructor for MultiSpeciesPlugin.
   *
   * @param orig the MultiSpeciesPlugin instance to copy.
   */
  MultiSpeciesPlugin(const MultiSpeciesPlugin& orig);


  /**
   * Assignment operator for MultiSpeciesPlugin.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  MultiSpeciesPlugin& operator=(const MultiSpeciesPlugin& rhs);


  /**
   * Creates and returns a deep copy of this MultiSpeciesPlugin object.
   *
   * @return a (deep) copy of this MultiSpeciesPlugin object.
   */
  virtual MultiSpeciesPlugin* clone () const;


  /**
   * Destructor for MultiSpeciesPlugin.
   */
  virtual ~MultiSpeciesPlugin();


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
   * Returns the value of the "speciesType" attribute of this "multi" Species.
   *
   * @return the value of the "speciesType" attribute.
   */
  virtual const std::string& getSpeciesType() const;


  /**
   * Returns @c true if the "speciesType" attribute of this "multi" Species
   * has been set.
   *
   * @return @c true if this SpeciesPlugin's "speciesType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetSpeciesType() const;


  /**
   * Sets the value of the "speciesType" attribute on this "multi" Species.
   *
   * @param speciesType the new value for the attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setSpeciesType(const std::string& speciesType);


  /**
   * Unsets the value of the "speciesType" attribute on this "multi" Species.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetSpeciesType();



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
   * Returns the ListOfOutwardBindingSites of this "multi" Species.
   *
   * @return the ListOfOutwardBindingSites object.
   */
  const ListOfOutwardBindingSites* getListOfOutwardBindingSites () const;


  /**
   * Returns the ListOfOutwardBindingSites of this "multi" Species.
   *
   * @return the ListOfOutwardBindingSites object.
   */
  ListOfOutwardBindingSites* getListOfOutwardBindingSites ();


  /**
   * Returns the nth OutwardBindingSite object.
   *
   * @param n the index number of the OutwardBindingSite to get
   *
   * @return the nth OutwardBindingSite in the ListOfOutwardBindingSites.  If
   * the index is invalid, NULL is returned.
   */
  const OutwardBindingSite* getOutwardBindingSite(unsigned int n) const;


  /**
   * Returns the nth OutwardBindingSite object.
   *
   * @param n the index number of the OutwardBindingSite to get
   *
   * @return the nth OutwardBindingSite in the ListOfOutwardBindingSites.  If
   * the index is invalid, NULL is returned.
   */
  OutwardBindingSite* getOutwardBindingSite(unsigned int n);


  /**
   * Returns the OutwardBindingSite object with the given identifier @p sid.
   *
   * @param sid a string representing the id of the OutwardBindingSite to get.
   *
   * @return OutwardBindingSite in the ListOfOutwardBindingSites with the given id
   * or @c NULL if no such OutwardBindingSite exists.
   */
  const OutwardBindingSite* getOutwardBindingSite(const std::string& sid) const;


  /**
   * Returns the OutwardBindingSite object with the given identifier @p sid.
   *
   * @param sid a string representing the id of the OutwardBindingSite to get.
   *
   * @return OutwardBindingSite in the ListOfOutwardBindingSites with the given id
   * or @c NULL if no such OutwardBindingSite exists.
   */
  OutwardBindingSite* getOutwardBindingSite(const std::string& sid);


  /**
   * Adds a copy of the given OutwardBindingSite to the
   * ListOfOutwardBindingSites.
   *
   * @param outwardBindingSite the outwardBindingSite to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int addOutwardBindingSite (const OutwardBindingSite* outwardBindingSite);


  /**
   * Creates a new OutwardBindingSite object and adds it to the
   * ListOfOutwardBindingSites.
   *
   * @return the newly created OutwardBindingSite object.
   */
  OutwardBindingSite* createOutwardBindingSite ();


  /**
   * Removes the nth OutwardBindingSite object and returns a pointer to it.
   *
   * @param n the index of the OutwardBindingSite to remove.
   *
   * @return the OutwardBindingSite object removed or @c NULL index was out of
   * range.  Note that the caller owns the returned object and is responsible
   * for deleting it.
   */
  OutwardBindingSite* removeOutwardBindingSite(unsigned int n);


  /**
   * Removes the OutwardBindingSite object with the given id @p sid and
   * returns a pointer to it.
   *
   * @param sid a string representing the id of the OutwardBindingSite to remove.
   *
   * @return the OutwardBindingSite object removed or @c NULL index was out of
   * range.  Note that the caller owns the returned object and is responsible
   * for deleting it.
   */
  OutwardBindingSite* removeOutwardBindingSite(const std::string& sid);


  /**
   * Returns the number of OutwardBindingSite objects of this "multi" Species.
   *
   * @return the number of OutwardBindingSite objects in this plugin object.
   */
  unsigned int getNumOutwardBindingSites () const;


  /**
   * Returns the ListOfSpeciesFeatures of this "multi" Species.
   *
   * @return ListOfSpeciesFeatures object in this plugin object.
   */
  const ListOfSpeciesFeatures* getListOfSpeciesFeatures () const;


  /**
   * Returns the ListOfSpeciesFeatures of this "multi" Species.
   *
   * @return ListOfSpeciesFeatures object in this plugin object.
   */
  ListOfSpeciesFeatures* getListOfSpeciesFeatures ();


  /**
   * Returns the nth SpeciesFeature object.
   *
   * @param n the index number of the SpeciesFeature to get.
   *
   * @return the nth SpeciesFeature in the ListOfSpeciesFeatures.  If the
   * index is invalid, NULL is returned.
   */
  const SpeciesFeature* getSpeciesFeature(unsigned int n) const;


  /**
   * Returns the nth SpeciesFeature object.
   *
   * @param n the index number of the SpeciesFeature to get.
   *
   * @return the nth SpeciesFeature in the ListOfSpeciesFeatures.  If the
   * index is invalid, NULL is returned.
   */
  SpeciesFeature* getSpeciesFeature(unsigned int n);


  /**
   * Returns the SpeciesFeature object with the given identifier @p sid.
   *
   * @param sid a string representing the id of the SpeciesFeature to get
   *
   * @return SpeciesFeature in the ListOfSpeciesFeatures with the given id @p
   * sid, or @c NULL if no such SpeciesFeature exists.
   */
  const SpeciesFeature* getSpeciesFeature(const std::string& sid) const;


  /**
   * Returns the SpeciesFeature object with the given identifier @p sid.
   *
   * @param sid a string representing the id of the SpeciesFeature to get
   *
   * @return SpeciesFeature in the ListOfSpeciesFeatures with the given id @p
   * sid, or @c NULL if no such SpeciesFeature exists.
   */
  SpeciesFeature* getSpeciesFeature(const std::string& sid);


  /**
   * Adds a copy of the given SpeciesFeature to the ListOfSpeciesFeatures of
   * this "multi" Species.
   *
   * @param speciesFeature the SpeciesFeature to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int addSpeciesFeature (const SpeciesFeature* speciesFeature);


  /**
   * Creates a new SpeciesFeature object and adds it to the
   * ListOfSpeciesFeatures of this "multi" Species.
   *
   * @return the newly created SpeciesFeature object.
   */
  SpeciesFeature* createSpeciesFeature ();


  /**
   * Returns the nth SubListOfSpeciesFeatures object.
   *
   * @param n the index number of the SubListOfSpeciesFeatures to get
   *
   * @return the nth SubListOfSpeciesFeatures in the ListOfSpeciesFeatures.
   * If the index is invalid, NULL is returned.
   */
  const SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(unsigned int n) const;


  /**
   * Returns the nth SubListOfSpeciesFeatures object.
   *
   * @param n the index number of the SubListOfSpeciesFeatures to get
   *
   * @return the nth SubListOfSpeciesFeatures in the ListOfSpeciesFeatures.
   * If the index is invalid, NULL is returned.
   */
  SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(unsigned int n);


  /**
   * Returns the SubListOfSpeciesFeatures object with the given identifier @p sid.
   *
   * @param sid the id of the SubListOfSpeciesFeatures to get.
   *
   * @return SubListOfSpeciesFeatures in the ListOfSpeciesFeatures with the
   * given id @p sid, or @c NULL if no such SubListOfSpeciesFeatures exists.
   */
  const SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(const std::string& sid) const;


  /**
   * Returns the SubListOfSpeciesFeatures object with the given identifier @p sid.
   *
   * @param sid the id of the SubListOfSpeciesFeatures to get.
   *
   * @return SubListOfSpeciesFeatures in the ListOfSpeciesFeatures with the
   * given id @p sid, or @c NULL if no such SubListOfSpeciesFeatures exists.
   */
  SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(const std::string& sid);


  /**
   * Adds a copy of the given SubListOfSpeciesFeatures to the
   * ListOfSpeciesFeatures.
   *
   * @param subListOfSpeciesFeatures the SubListOfSpeciesFeatures to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int addSubListOfSpeciesFeatures (SubListOfSpeciesFeatures* subListOfSpeciesFeatures);


  /**
   * Creates a new SubListOfSpeciesFeatures object and adds it to the
   * SubListOfSpeciesFeatures.
   *
   * @return the newly created SubListOfSpeciesFeatures object.
   */
  SubListOfSpeciesFeatures* createSubListOfSpeciesFeatures ();


  /**
   * Removes the nth SpeciesFeature object and returns a pointer to it.
   *
   * @param n the index of the SpeciesFeature to remove.
   *
   * @return the SpeciesFeature object removed or @c NULL index was out of
   * range.  Note that the caller owns the returned object and is responsible
   * for deleting it.
   */
  SpeciesFeature* removeSpeciesFeature(unsigned int n);


  /**
   * Removes the SpeciesFeature object with the given identifier @p sid.
   *
   * @param sid a string representing the id of the SpeciesFeature to get.
   *
   * @return the SpeciesFeature object removed or @c NULL index was out of
   * range.  Note that the caller owns the returned object and is responsible
   * for deleting it.
   */
  SpeciesFeature* removeSpeciesFeature(const std::string& sid);


  /**
   * Returns the number of SpeciesFeature objects of this "multi" Species.
   *
   * @return the number of SpeciesFeature objects in this plugin object.
   */
  unsigned int getNumSpeciesFeatures () const;


  /**
   * Returns the number of SubListOfSpeciesFeatures objects of this "multi" Species.
   *
   * @return the number of SubListOfSpeciesFeatures objects in this plugin object.
   */
  unsigned int getNumSubListOfSpeciesFeatures () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  virtual void connectToParent (SBase* sbase);

  virtual void connectToChild();
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
  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  ListOfOutwardBindingSites mOutwardBindingSites;
  ListOfSpeciesFeatures mSpeciesFeatures;

  std::string   mSpeciesType;
  /** @endcond */

};




LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Returns a ListOf_t * containing SpeciesFeature_t objects from this
 * MultiSpeciesPlugin_t.
 *
 * @param msp the MultiSpeciesPlugin_t structure whose ListOfSpeciesFeatures is
 * sought.
 *
 * @return the ListOfSpeciesFeatures from this MultiSpeciesPlugin_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see MultiSpeciesPlugin_addSpeciesFeature()
 * @see MultiSpeciesPlugin_createSpeciesFeature()
 * @see MultiSpeciesPlugin_getSpeciesFeatureById()
 * @see MultiSpeciesPlugin_getSpeciesFeature()
 * @see MultiSpeciesPlugin_getNumSpeciesFeatures()
 * @see MultiSpeciesPlugin_removeSpeciesFeatureById()
 * @see MultiSpeciesPlugin_removeSpeciesFeature()
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesPlugin_getListOfSpeciesFeatures(MultiSpeciesPlugin_t* msp);


/**
 * Get a SpeciesFeature_t from the MultiSpeciesPlugin_t.
 *
 * @param msp the MultiSpeciesPlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the SpeciesFeature_t to
 * retrieve.
 *
 * @return the nth SpeciesFeature_t in the ListOfSpeciesFeatures within this
 * MultiSpeciesPlugin.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
SpeciesFeature_t*
MultiSpeciesPlugin_getSpeciesFeature(MultiSpeciesPlugin_t* msp,
  unsigned int n);


/**
 * Get a SpeciesFeature_t from the MultiSpeciesPlugin_t based on its
 * identifier.
 *
 * @param msp the MultiSpeciesPlugin_t structure to search.
 *
 * @param sid a string representing the identifier of the SpeciesFeature_t to
 * retrieve.
 *
 * @return the SpeciesFeature_t in the ListOfSpeciesFeatures within this
 * MultiSpeciesPlugin with the given @p sid or @c NULL if no such
 * SpeciesFeature_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
SpeciesFeature_t*
MultiSpeciesPlugin_getSpeciesFeatureById(MultiSpeciesPlugin_t* msp,
  const char *sid);


/**
 * Adds a copy of the given SpeciesFeature_t to this MultiSpeciesPlugin_t.
 *
 * @param msp the MultiSpeciesPlugin_t structure to which the SpeciesFeature_t
 * should be added.
 *
 * @param sf the SpeciesFeature_t object to add.
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
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
MultiSpeciesPlugin_addSpeciesFeature(MultiSpeciesPlugin_t* msp,
  const SpeciesFeature_t* sf);


/**
 * Get the number of SpeciesFeature_t objects in this MultiSpeciesPlugin_t.
 *
 * @param msp the MultiSpeciesPlugin_t structure to query.
 *
 * @return the number of SpeciesFeature_t objects in this MultiSpeciesPlugin_t.
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesPlugin_getNumSpeciesFeatures(MultiSpeciesPlugin_t* msp);


/**
 * Creates a new SpeciesFeature_t object, adds it to this MultiSpeciesPlugin_t
 * object and returns the SpeciesFeature_t object created.
 *
 * @param msp the MultiSpeciesPlugin_t structure to which the SpeciesFeature_t
 * should be added.
 *
 * @return a new SpeciesFeature_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
SpeciesFeature_t*
MultiSpeciesPlugin_createSpeciesFeature(MultiSpeciesPlugin_t* msp);


/**
 * Removes the nth SpeciesFeature_t from this MultiSpeciesPlugin_t and returns
 * a pointer to it.
 *
 * @param msp the MultiSpeciesPlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the SpeciesFeature_t to
 * remove.
 *
 * @return a pointer to the nth SpeciesFeature_t in this MultiSpeciesPlugin_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
SpeciesFeature_t*
MultiSpeciesPlugin_removeSpeciesFeature(MultiSpeciesPlugin_t* msp,
  unsigned int n);


/**
 * Removes the SpeciesFeature_t from this MultiSpeciesPlugin_t based on its
 * identifier and returns a pointer to it.
 *
 * @param msp the MultiSpeciesPlugin_t structure to search.
 *
 * @param sid a string representing the identifier of the SpeciesFeature_t to
 * remove.
 *
 * @return the SpeciesFeature_t in this MultiSpeciesPlugin_t based on the
 * identifier or NULL if no such SpeciesFeature_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
SpeciesFeature_t*
MultiSpeciesPlugin_removeSpeciesFeatureById(MultiSpeciesPlugin_t* msp,
  const char* sid);


/**
 * Returns a ListOf_t * containing OutwardBindingSite_t objects from this
 * MultiSpeciesPlugin_t.
 *
 * @param msp the MultiSpeciesPlugin_t structure whose
 * ListOfOutwardBindingSites is sought.
 *
 * @return the ListOfOutwardBindingSites from this MultiSpeciesPlugin_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see MultiSpeciesPlugin_addOutwardBindingSite()
 * @see MultiSpeciesPlugin_createOutwardBindingSite()
 * @see MultiSpeciesPlugin_getOutwardBindingSiteById()
 * @see MultiSpeciesPlugin_getOutwardBindingSite()
 * @see MultiSpeciesPlugin_getNumOutwardBindingSites()
 * @see MultiSpeciesPlugin_removeOutwardBindingSiteById()
 * @see MultiSpeciesPlugin_removeOutwardBindingSite()
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesPlugin_getListOfOutwardBindingSites(MultiSpeciesPlugin_t* msp);


/**
 * Get an OutwardBindingSite_t from the MultiSpeciesPlugin_t.
 *
 * @param msp the MultiSpeciesPlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the OutwardBindingSite_t
 * to retrieve.
 *
 * @return the nth OutwardBindingSite_t in the ListOfOutwardBindingSites within
 * this MultiSpeciesPlugin.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
OutwardBindingSite_t*
MultiSpeciesPlugin_getOutwardBindingSite(MultiSpeciesPlugin_t* msp,
  unsigned int n);


/**
 * Get an OutwardBindingSite_t from the MultiSpeciesPlugin_t based on its
 * identifier.
 *
 * @param msp the MultiSpeciesPlugin_t structure to search.
 *
 * @param sid a string representing the identifier of the OutwardBindingSite_t
 * to retrieve.
 *
 * @return the OutwardBindingSite_t in the ListOfOutwardBindingSites within
 * this MultiSpeciesPlugin with the given @p sid or @c NULL if no such
 * OutwardBindingSite_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
OutwardBindingSite_t*
MultiSpeciesPlugin_getOutwardBindingSiteById(MultiSpeciesPlugin_t* msp,
  const char *sid);


/**
 * Adds a copy of the given OutwardBindingSite_t to this MultiSpeciesPlugin_t.
 *
 * @param msp the MultiSpeciesPlugin_t structure to which the
 * OutwardBindingSite_t should be added.
 *
 * @param obs the OutwardBindingSite_t object to add.
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
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
MultiSpeciesPlugin_addOutwardBindingSite(MultiSpeciesPlugin_t* msp,
  const OutwardBindingSite_t* obs);


/**
 * Get the number of OutwardBindingSite_t objects in this MultiSpeciesPlugin_t.
 *
 * @param msp the MultiSpeciesPlugin_t structure to query.
 *
 * @return the number of OutwardBindingSite_t objects in this
 * MultiSpeciesPlugin_t.
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesPlugin_getNumOutwardBindingSites(MultiSpeciesPlugin_t* msp);


/**
 * Creates a new OutwardBindingSite_t object, adds it to this
 * MultiSpeciesPlugin_t object and returns the OutwardBindingSite_t object
 * created.
 *
 * @param msp the MultiSpeciesPlugin_t structure to which the
 * OutwardBindingSite_t should be added.
 *
 * @return a new OutwardBindingSite_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
OutwardBindingSite_t*
MultiSpeciesPlugin_createOutwardBindingSite(MultiSpeciesPlugin_t* msp);


/**
 * Removes the nth OutwardBindingSite_t from this MultiSpeciesPlugin_t and
 * returns a pointer to it.
 *
 * @param msp the MultiSpeciesPlugin_t structure to search.
 *
 * @param n an unsigned int representing the index of the OutwardBindingSite_t
 * to remove.
 *
 * @return a pointer to the nth OutwardBindingSite_t in this
 * MultiSpeciesPlugin_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
OutwardBindingSite_t*
MultiSpeciesPlugin_removeOutwardBindingSite(MultiSpeciesPlugin_t* msp,
  unsigned int n);


/**
 * Removes the OutwardBindingSite_t from this MultiSpeciesPlugin_t based on its
 * identifier and returns a pointer to it.
 *
 * @param msp the MultiSpeciesPlugin_t structure to search.
 *
 * @param sid a string representing the identifier of the OutwardBindingSite_t
 * to remove.
 *
 * @return the OutwardBindingSite_t in this MultiSpeciesPlugin_t based on the
 * identifier or NULL if no such OutwardBindingSite_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
OutwardBindingSite_t*
MultiSpeciesPlugin_removeOutwardBindingSiteById(MultiSpeciesPlugin_t* msp,
  const char* sid);


/**
 * Returns the value of the "speciesType" attribute of this
 * MultiSpeciesPlugin_t.
 *
 * @param msp the MultiSpeciesPlugin_t structure whose speciesType is sought.
 *
 * @return the value of the "speciesType" attribute of this
 * MultiSpeciesPlugin_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
char *
MultiSpeciesPlugin_getSpeciesType(const MultiSpeciesPlugin_t * msp);


/**
 * Predicate returning @c 1 (true) if this MultiSpeciesPlugin_t's "speciesType"
 * attribute is set.
 *
 * @param msp the MultiSpeciesPlugin_t structure.
 *
 * @return @c 1 (true) if this MultiSpeciesPlugin_t's "speciesType" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
MultiSpeciesPlugin_isSetSpeciesType(const MultiSpeciesPlugin_t * msp);


/**
 * Sets the value of the "speciesType" attribute of this MultiSpeciesPlugin_t.
 *
 * @param msp the MultiSpeciesPlugin_t structure.
 *
 * @param speciesType const char * value of the "speciesType" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
MultiSpeciesPlugin_setSpeciesType(MultiSpeciesPlugin_t * msp,
  const char * speciesType);


/**
 * Unsets the value of the "speciesType" attribute of this
 * MultiSpeciesPlugin_t.
 *
 * @param msp the MultiSpeciesPlugin_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof MultiSpeciesPlugin_t
 */
LIBSBML_EXTERN
int
MultiSpeciesPlugin_unsetSpeciesType(MultiSpeciesPlugin_t * msp);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !MultiSpeciesPlugin_H__ */


