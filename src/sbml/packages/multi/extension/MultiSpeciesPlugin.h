/**
 * @file:   MultiSpeciesPlugin.h
 * @brief:  Implementation of the MultiSpeciesPlugin class
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


#ifndef MultiSpeciesPlugin_H__
#define MultiSpeciesPlugin_H__


#include <sbml/common/extern.h>


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
   * Creates a new MultiSpeciesPlugin
   */
  MultiSpeciesPlugin(const std::string& uri, const std::string& prefix, 
                                 MultiPkgNamespaces* multins);


  /**
   * Copy constructor for MultiSpeciesPlugin.
   *
   * @param orig; the MultiSpeciesPlugin instance to copy.
   */
  MultiSpeciesPlugin(const MultiSpeciesPlugin& orig);


   /**
   * Assignment operator for MultiSpeciesPlugin.
   *
   * @param rhs; the object whose values are used as the basis
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
   * Returns the value of the "speciesType" attribute of this SpeciesPlugin.
   *
   * @return the value of the "speciesType" attribute of this SpeciesPlugin as a string.
   */
  virtual const std::string& getSpeciesType() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SpeciesPlugin's "speciesType" attribute has been set.
   *
   * @return @c true if this SpeciesPlugin's "speciesType" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetSpeciesType() const;


  /**
   * Sets the value of the "speciesType" attribute of this SpeciesPlugin.
   *
   * @param speciesType; const std::string& value of the "speciesType" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setSpeciesType(const std::string& speciesType);


  /**
   * Unsets the value of the "speciesType" attribute of this SpeciesPlugin.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSpeciesType();



  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the ListOfOutwardBindingSites in this plugin object.
   *
   * @return ListOfOutwardBindingSites object in this plugin object.
   */
  const ListOfOutwardBindingSites* getListOfOutwardBindingSites () const;


  /**
   * Returns the ListOfOutwardBindingSites in this plugin object.
   *
   * @return ListOfOutwardBindingSites object in this plugin object.
   */
  ListOfOutwardBindingSites* getListOfOutwardBindingSites ();


  /**
   * Returns the OutwardBindingSite object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the OutwardBindingSite to get
   *
   * @return the nth OutwardBindingSite in the ListOfOutwardBindingSites
   */
  const OutwardBindingSite* getOutwardBindingSite(unsigned int n) const;


  /**
   * Returns the OutwardBindingSite object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the OutwardBindingSite to get
   *
   * @return the nth OutwardBindingSite in the ListOfOutwardBindingSites
   */
  OutwardBindingSite* getOutwardBindingSite(unsigned int n);


  /**
   * Returns the OutwardBindingSite object based on its identifier.
   *
   * @param sid a string representing the id of the OutwardBindingSite to get
   *
   * @return OutwardBindingSite in the ListOfOutwardBindingSites with the given id
   * or NULL if no such OutwardBindingSite exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  const OutwardBindingSite* getOutwardBindingSite(const std::string& sid) const;


  /**
   * Returns the OutwardBindingSite object based on its identifier.
   *
   * @param sid a string representing the id of the OutwardBindingSite to get
   *
   * @return OutwardBindingSite in the ListOfOutwardBindingSites with the given id
   * or NULL if no such OutwardBindingSite exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  OutwardBindingSite* getOutwardBindingSite(const std::string& sid);


  /**
   * Adds a copy of the given OutwardBindingSite to the ListOfOutwardBindingSites in this plugin object.
   *
   * @param outwardBindingSite the outwardBindingSite to be added.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int addOutwardBindingSite (const OutwardBindingSite* outwardBindingSite);


  /**
   * Creates a new OutwardBindingSite object and adds it to the ListOfOutwardBindingSites in this plugin object.
   *
   * @return the newly created OutwardBindingSite object.
   */
  OutwardBindingSite* createOutwardBindingSite ();


  /**
   * Removes the nth OutwardBindingSite object from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param n the index of the OutwardBindingSite to remove
   *
   * @return the OutwardBindingSite object removed 
   * or NULL index was out of range.
   */
  OutwardBindingSite* removeOutwardBindingSite(unsigned int n);


  /**
   * Removes the OutwardBindingSite object with the given id from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param sid a string representing the id of the OutwardBindingSite to remove
   *
   * @return the OutwardBindingSite object removed 
   * or NULL if no such OutwardBindingSite exists.
   */
  OutwardBindingSite* removeOutwardBindingSite(const std::string& sid);


  /**
   * Returns the number of OutwardBindingSite objects in this plugin object.
   *
   * @return the number of OutwardBindingSite objects in this plugin object.
   */
  unsigned int getNumOutwardBindingSites () const;


  /**
   * Returns the ListOfSpeciesFeatures in this plugin object.
   *
   * @return ListOfSpeciesFeatures object in this plugin object.
   */
  const ListOfSpeciesFeatures* getListOfSpeciesFeatures () const;


  /**
   * Returns the ListOfSpeciesFeatures in this plugin object.
   *
   * @return ListOfSpeciesFeatures object in this plugin object.
   */
  ListOfSpeciesFeatures* getListOfSpeciesFeatures ();


  /**
   * Returns the SpeciesFeature object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the SpeciesFeature to get
   *
   * @return the nth SpeciesFeature in the ListOfSpeciesFeatures
   */
  const SpeciesFeature* getSpeciesFeature(unsigned int n) const;


  /**
   * Returns the SpeciesFeature object that belongs to the given index. If the 
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the SpeciesFeature to get
   *
   * @return the nth SpeciesFeature in the ListOfSpeciesFeatures
   */
  SpeciesFeature* getSpeciesFeature(unsigned int n);


  /**
   * Returns the SpeciesFeature object based on its identifier.
   *
   * @param sid a string representing the id of the SpeciesFeature to get
   *
   * @return SpeciesFeature in the ListOfSpeciesFeatures with the given id
   * or NULL if no such SpeciesFeature exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  const SpeciesFeature* getSpeciesFeature(const std::string& sid) const;


  /**
   * Returns the SpeciesFeature object based on its identifier.
   *
   * @param sid a string representing the id of the SpeciesFeature to get
   *
   * @return SpeciesFeature in the ListOfSpeciesFeatures with the given id
   * or NULL if no such SpeciesFeature exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  SpeciesFeature* getSpeciesFeature(const std::string& sid);


  /**
   * Adds a copy of the given SpeciesFeature to the ListOfSpeciesFeatures in this plugin object.
   *
   * @param speciesFeature the speciesFeature to be added.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int addSpeciesFeature (const SpeciesFeature* speciesFeature);


  /**
   * Creates a new SpeciesFeature object and adds it to the ListOfSpeciesFeatures in this plugin object.
   *
   * @return the newly created SpeciesFeature object.
   */
  SpeciesFeature* createSpeciesFeature ();


  /**
   * Returns the SubListOfSpeciesFeatures object that belongs to the given index. If the
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the SubListOfSpeciesFeatures to get
   *
   * @return the nth SubListOfSpeciesFeatures in the ListOfSpeciesFeatures
   */
  const SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(unsigned int n) const;


  /**
   * Returns the SubListOfSpeciesFeatures object that belongs to the given index. If the
   * index is invalid, NULL is returned.
   *
   * @param n the index number of the SubListOfSpeciesFeatures to get
   *
   * @return the nth SubListOfSpeciesFeatures in the ListOfSpeciesFeatures
   */
  SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(unsigned int n);


  /**
   * Returns the SubListOfSpeciesFeatures object based on its identifier.
   *
   * @param sid a string representing the id of the SubListOfSpeciesFeatures to get
   *
   * @return SubListOfSpeciesFeatures in the ListOfSpeciesFeatures with the given id
   * or NULL if no such SubListOfSpeciesFeatures exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  const SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(const std::string& sid) const;


  /**
   * Returns the SubListOfSpeciesFeatures object based on its identifier.
   *
   * @param sid a string representing the id of the SubListOfSpeciesFeatures to get
   *
   * @return SubListOfSpeciesFeatures in the ListOfSpeciesFeatures with the given id
   * or NULL if no such SubListOfSpeciesFeatures exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(const std::string& sid);


  /**
   * Adds a copy of the given SubListOfSpeciesFeatures to the ListOfSpeciesFeatures in this plugin object.
   *
   * @param SubListOfSpeciesFeatures the SubListOfSpeciesFeatures to be added.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int addSubListOfSpeciesFeatures (SubListOfSpeciesFeatures* subListOfSpeciesFeatures);


  /**
   * Creates a new SubListOfSpeciesFeatures object and
   * adds it to the SubListOfSpeciesFeatures in this plugin object.
   *
   * @return the newly created SubListOfSpeciesFeatures object.
   */
  SubListOfSpeciesFeatures* createSubListOfSpeciesFeatures ();


  /**
   * Removes the nth SpeciesFeature object from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param n the index of the SpeciesFeature to remove
   *
   * @return the SpeciesFeature object removed 
   * or NULL index was out of range.
   */
  SpeciesFeature* removeSpeciesFeature(unsigned int n);


  /**
   * Removes the SpeciesFeature object with the given id from this plugin object
   * and returns a pointer to it.
   *
   * The caller owns the returned object and is responsible for
   * deleting it.
   *
   * @param sid a string representing the id of the SpeciesFeature to remove
   *
   * @return the SpeciesFeature object removed 
   * or NULL if no such SpeciesFeature exists.
   */
  SpeciesFeature* removeSpeciesFeature(const std::string& sid);


  /**
   * Returns the number of SpeciesFeature objects in this plugin object.
   *
   * @return the number of SpeciesFeature objects in this plugin object.
   */
  unsigned int getNumSpeciesFeatures () const;

  /**
   * Returns the number of SubListOfSpeciesFeatures objects in this plugin object.
   *
   * @return the number of SubListOfSpeciesFeatures objects in this plugin object.
   */
  unsigned int getNumSubListOfSpeciesFeatures () const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  virtual void connectToParent (SBase* sbase);

  virtual void connectToChild();


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

  ListOfOutwardBindingSites mOutwardBindingSites;
  ListOfSpeciesFeatures mSpeciesFeatures;

  std::string   mSpeciesType;

  /** @endcond doxygenLibsbmlInternal */

};




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* MultiSpeciesPlugin_H__ */


