/**
 * @file:   MultiSpeciesType.h
 * @brief:  Implementation of the MultiSpeciesType class
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


#ifndef MultiSpeciesType_H__
#define MultiSpeciesType_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>

#include <sbml/packages/multi/sbml/SpeciesFeatureType.h>
#include <sbml/packages/multi/sbml/SpeciesTypeInstance.h>
#include <sbml/packages/multi/sbml/SpeciesTypeComponentIndex.h>
#include <sbml/packages/multi/sbml/InSpeciesTypeBond.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN MultiSpeciesType : public SBase
{

protected:

////  std::string                           mId;
////  std::string                           mName;
  std::string                           mCompartment;
  ListOfSpeciesFeatureTypes             mListOfSpeciesFeatureTypes;
  ListOfSpeciesTypeInstances            mListOfSpeciesTypeInstances;
  ListOfSpeciesTypeComponentIndexes     mListOfSpeciesTypeComponentIndexes;
  ListOfInSpeciesTypeBonds              mListOfInSpeciesTypeBonds;


public:

  /**
   * Creates a new MultiSpeciesType with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this MultiSpeciesType
   *
   * @param version an unsigned int, the SBML Version to assign to this MultiSpeciesType
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this MultiSpeciesType
   */
  MultiSpeciesType(unsigned int level      = MultiExtension::getDefaultLevel(),
                   unsigned int version    = MultiExtension::getDefaultVersion(),
                   unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new MultiSpeciesType with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  MultiSpeciesType(MultiPkgNamespaces* multins);


   /**
   * Copy constructor for MultiSpeciesType.
   *
   * @param orig; the MultiSpeciesType instance to copy.
   */
  MultiSpeciesType(const MultiSpeciesType& orig);


   /**
   * Assignment operator for MultiSpeciesType.
   *
   * @param rhs; the object whose values are used as the basis
   * of the assignment
   */
  MultiSpeciesType& operator=(const MultiSpeciesType& rhs);


   /**
   * Creates and returns a deep copy of this MultiSpeciesType object.
   *
   * @return a (deep) copy of this MultiSpeciesType object.
   */
  virtual MultiSpeciesType* clone () const;


   /**
   * Destructor for MultiSpeciesType.
   */
  virtual ~MultiSpeciesType();


   /**
   * Returns the value of the "id" attribute of this MultiSpeciesType.
   *
   * @return the value of the "id" attribute of this MultiSpeciesType as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * MultiSpeciesType's "id" attribute has been set.
   *
   * @return @c true if this MultiSpeciesType's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this MultiSpeciesType.
   *
   * @param id; const std::string& value of the "id" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this MultiSpeciesType.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this MultiSpeciesType.
   *
   * @return the value of the "name" attribute of this MultiSpeciesType as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * MultiSpeciesType's "name" attribute has been set.
   *
   * @return @c true if this MultiSpeciesType's "name" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this MultiSpeciesType.
   *
   * @param name; const std::string& value of the "name" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this MultiSpeciesType.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetName();




  /**
   * Returns the value of the "compartment" attribute of this MultiSpeciesType.
   *
   * @return the value of the "compartment" attribute of this MultiSpeciesType as a string.
   */
  virtual const std::string& getCompartment() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * MultiSpeciesType's "compartment" attribute has been set.
   *
   * @return @c true if this MultiSpeciesType's "compartment" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetCompartment() const;


  /**
   * Sets the value of the "compartment" attribute of this MultiSpeciesType.
   *
   * @param compartment; const std::string& value of the "compartment" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCompartment(const std::string& compartment);


  /**
   * Unsets the value of the "compartment" attribute of this MultiSpeciesType.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCompartment();


  /**
   * Returns the  "ListOfSpeciesFeatureTypes" in this MultiSpeciesType object.
   *
   * @return the "ListOfSpeciesFeatureTypes" attribute of this MultiSpeciesType.
   */
  const ListOfSpeciesFeatureTypes* getListOfSpeciesFeatureTypes() const;


  /**
   * Returns the  "ListOfSpeciesFeatureTypes" in this MultiSpeciesType object.
   *
   * @return the "ListOfSpeciesFeatureTypes" attribute of this MultiSpeciesType.
   */
  ListOfSpeciesFeatureTypes* getListOfSpeciesFeatureTypes();


  /**
   * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes.
   *
   * @param n the index number of the SpeciesFeatureType to get.
   *
   * @return the nth SpeciesFeatureType in the ListOfSpeciesFeatureTypes within this MultiSpeciesType.
   *
   * @see getNumSpeciesFeatureTypes()
   */
  SpeciesFeatureType* getSpeciesFeatureType(unsigned int n);


  /**
   * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes.
   *
   * @param n the index number of the SpeciesFeatureType to get.
   *
   * @return the nth SpeciesFeatureType in the ListOfSpeciesFeatureTypes within this MultiSpeciesType.
   *
   * @see getNumSpeciesFeatureTypes()
   */
  const SpeciesFeatureType* getSpeciesFeatureType(unsigned int n) const;


  /**
   * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesFeatureType to get.
   *
   * @return the SpeciesFeatureType in the ListOfSpeciesFeatureTypes
   * with the given id or NULL if no such
   * SpeciesFeatureType exists.
   *
   * @see getSpeciesFeatureType(unsigned int n)
   *
   * @see getNumSpeciesFeatureTypes()
   */
  SpeciesFeatureType* getSpeciesFeatureType(const std::string& sid);


  /**
   * Get a SpeciesFeatureType from the ListOfSpeciesFeatureTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesFeatureType to get.
   *
   * @return the SpeciesFeatureType in the ListOfSpeciesFeatureTypes
   * with the given id or NULL if no such
   * SpeciesFeatureType exists.
   *
   * @see getSpeciesFeatureType(unsigned int n)
   *
   * @see getNumSpeciesFeatureTypes()
   */
  const SpeciesFeatureType* getSpeciesFeatureType(const std::string& sid) const;


  /**
   * Adds a copy the given "SpeciesFeatureType" to this MultiSpeciesType.
   *
   * @param sft; the SpeciesFeatureType object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addSpeciesFeatureType(const SpeciesFeatureType* sft);


  /**
   * Get the number of SpeciesFeatureType objects in this MultiSpeciesType.
   *
   * @return the number of SpeciesFeatureType objects in this MultiSpeciesType
   */
  unsigned int getNumSpeciesFeatureTypes() const;


  /**
   * Creates a new SpeciesFeatureType object, adds it to this MultiSpeciesTypes
   * ListOfSpeciesFeatureTypes and returns the SpeciesFeatureType object created. 
   *
   * @return a new SpeciesFeatureType object instance
   *
   * @see addSpeciesFeatureType(const SpeciesFeatureType* sft)
   */
  SpeciesFeatureType* createSpeciesFeatureType();


  /**
   * Removes the nth SpeciesFeatureType from the ListOfSpeciesFeatureTypes within this MultiSpeciesType.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SpeciesFeatureType to remove.
   *
   * @see getNumSpeciesFeatureTypes()
   */
  SpeciesFeatureType* removeSpeciesFeatureType(unsigned int n);


  /**
   * Removes the SpeciesFeatureType with the given identifier from the ListOfSpeciesFeatureTypes within this MultiSpeciesType
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SpeciesFeatureType to remove.
   *
   * @return the SpeciesFeatureType removed. As mentioned above, the caller owns the
   * returned item.
   */
  SpeciesFeatureType* removeSpeciesFeatureType(const std::string& sid);


  /**
   * Returns the  "ListOfSpeciesTypeInstances" in this MultiSpeciesType object.
   *
   * @return the "ListOfSpeciesTypeInstances" attribute of this MultiSpeciesType.
   */
  const ListOfSpeciesTypeInstances* getListOfSpeciesTypeInstances() const;


  /**
   * Returns the  "ListOfSpeciesTypeInstances" in this MultiSpeciesType object.
   *
   * @return the "ListOfSpeciesTypeInstances" attribute of this MultiSpeciesType.
   */
  ListOfSpeciesTypeInstances* getListOfSpeciesTypeInstances();


  /**
   * Get a SpeciesTypeInstance from the ListOfSpeciesTypeInstances.
   *
   * @param n the index number of the SpeciesTypeInstance to get.
   *
   * @return the nth SpeciesTypeInstance in the ListOfSpeciesTypeInstances within this MultiSpeciesType.
   *
   * @see getNumSpeciesTypeInstances()
   */
  SpeciesTypeInstance* getSpeciesTypeInstance(unsigned int n);


  /**
   * Get a SpeciesTypeInstance from the ListOfSpeciesTypeInstances.
   *
   * @param n the index number of the SpeciesTypeInstance to get.
   *
   * @return the nth SpeciesTypeInstance in the ListOfSpeciesTypeInstances within this MultiSpeciesType.
   *
   * @see getNumSpeciesTypeInstances()
   */
  const SpeciesTypeInstance* getSpeciesTypeInstance(unsigned int n) const;


  /**
   * Get a SpeciesTypeInstance from the ListOfSpeciesTypeInstances
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeInstance to get.
   *
   * @return the SpeciesTypeInstance in the ListOfSpeciesTypeInstances
   * with the given id or NULL if no such
   * SpeciesTypeInstance exists.
   *
   * @see getSpeciesTypeInstance(unsigned int n)
   *
   * @see getNumSpeciesTypeInstances()
   */
  SpeciesTypeInstance* getSpeciesTypeInstance(const std::string& sid);


  /**
   * Get a SpeciesTypeInstance from the ListOfSpeciesTypeInstances
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeInstance to get.
   *
   * @return the SpeciesTypeInstance in the ListOfSpeciesTypeInstances
   * with the given id or NULL if no such
   * SpeciesTypeInstance exists.
   *
   * @see getSpeciesTypeInstance(unsigned int n)
   *
   * @see getNumSpeciesTypeInstances()
   */
  const SpeciesTypeInstance* getSpeciesTypeInstance(const std::string& sid) const;


  /**
   * Adds a copy the given "SpeciesTypeInstance" to this MultiSpeciesType.
   *
   * @param sti; the SpeciesTypeInstance object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addSpeciesTypeInstance(const SpeciesTypeInstance* sti);


  /**
   * Get the number of SpeciesTypeInstance objects in this MultiSpeciesType.
   *
   * @return the number of SpeciesTypeInstance objects in this MultiSpeciesType
   */
  unsigned int getNumSpeciesTypeInstances() const;


  /**
   * Creates a new SpeciesTypeInstance object, adds it to this MultiSpeciesTypes
   * ListOfSpeciesTypeInstances and returns the SpeciesTypeInstance object created. 
   *
   * @return a new SpeciesTypeInstance object instance
   *
   * @see addSpeciesTypeInstance(const SpeciesTypeInstance* sti)
   */
  SpeciesTypeInstance* createSpeciesTypeInstance();


  /**
   * Removes the nth SpeciesTypeInstance from the ListOfSpeciesTypeInstances within this MultiSpeciesType.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SpeciesTypeInstance to remove.
   *
   * @see getNumSpeciesTypeInstances()
   */
  SpeciesTypeInstance* removeSpeciesTypeInstance(unsigned int n);


  /**
   * Removes the SpeciesTypeInstance with the given identifier from the ListOfSpeciesTypeInstances within this MultiSpeciesType
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SpeciesTypeInstance to remove.
   *
   * @return the SpeciesTypeInstance removed. As mentioned above, the caller owns the
   * returned item.
   */
  SpeciesTypeInstance* removeSpeciesTypeInstance(const std::string& sid);


  /**
   * Returns the  "ListOfSpeciesTypeComponentIndexes" in this MultiSpeciesType object.
   *
   * @return the "ListOfSpeciesTypeComponentIndexes" attribute of this MultiSpeciesType.
   */
  const ListOfSpeciesTypeComponentIndexes* getListOfSpeciesTypeComponentIndexes() const;


  /**
   * Returns the  "ListOfSpeciesTypeComponentIndexes" in this MultiSpeciesType object.
   *
   * @return the "ListOfSpeciesTypeComponentIndexes" attribute of this MultiSpeciesType.
   */
  ListOfSpeciesTypeComponentIndexes* getListOfSpeciesTypeComponentIndexes();


  /**
   * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes.
   *
   * @param n the index number of the SpeciesTypeComponentIndex to get.
   *
   * @return the nth SpeciesTypeComponentIndex in the ListOfSpeciesTypeComponentIndexes within this MultiSpeciesType.
   *
   * @see getNumSpeciesTypeComponentIndexes()
   */
  SpeciesTypeComponentIndex* getSpeciesTypeComponentIndex(unsigned int n);


  /**
   * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes.
   *
   * @param n the index number of the SpeciesTypeComponentIndex to get.
   *
   * @return the nth SpeciesTypeComponentIndex in the ListOfSpeciesTypeComponentIndexes within this MultiSpeciesType.
   *
   * @see getNumSpeciesTypeComponentIndexes()
   */
  const SpeciesTypeComponentIndex* getSpeciesTypeComponentIndex(unsigned int n) const;


  /**
   * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeComponentIndex to get.
   *
   * @return the SpeciesTypeComponentIndex in the ListOfSpeciesTypeComponentIndexes
   * with the given id or NULL if no such
   * SpeciesTypeComponentIndex exists.
   *
   * @see getSpeciesTypeComponentIndex(unsigned int n)
   *
   * @see getNumSpeciesTypeComponentIndexes()
   */
  SpeciesTypeComponentIndex* getSpeciesTypeComponentIndex(const std::string& sid);


  /**
   * Get a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeComponentIndex to get.
   *
   * @return the SpeciesTypeComponentIndex in the ListOfSpeciesTypeComponentIndexes
   * with the given id or NULL if no such
   * SpeciesTypeComponentIndex exists.
   *
   * @see getSpeciesTypeComponentIndex(unsigned int n)
   *
   * @see getNumSpeciesTypeComponentIndexes()
   */
  const SpeciesTypeComponentIndex* getSpeciesTypeComponentIndex(const std::string& sid) const;


  /**
   * Adds a copy the given "SpeciesTypeComponentIndex" to this MultiSpeciesType.
   *
   * @param stci; the SpeciesTypeComponentIndex object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addSpeciesTypeComponentIndex(const SpeciesTypeComponentIndex* stci);


  /**
   * Get the number of SpeciesTypeComponentIndex objects in this MultiSpeciesType.
   *
   * @return the number of SpeciesTypeComponentIndex objects in this MultiSpeciesType
   */
  unsigned int getNumSpeciesTypeComponentIndexes() const;


  /**
   * Creates a new SpeciesTypeComponentIndex object, adds it to this MultiSpeciesTypes
   * ListOfSpeciesTypeComponentIndexes and returns the SpeciesTypeComponentIndex object created. 
   *
   * @return a new SpeciesTypeComponentIndex object instance
   *
   * @see addSpeciesTypeComponentIndex(const SpeciesTypeComponentIndex* stci)
   */
  SpeciesTypeComponentIndex* createSpeciesTypeComponentIndex();


  /**
   * Removes the nth SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes within this MultiSpeciesType.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SpeciesTypeComponentIndex to remove.
   *
   * @see getNumSpeciesTypeComponentIndexes()
   */
  SpeciesTypeComponentIndex* removeSpeciesTypeComponentIndex(unsigned int n);


  /**
   * Removes the SpeciesTypeComponentIndex with the given identifier from the ListOfSpeciesTypeComponentIndexes within this MultiSpeciesType
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SpeciesTypeComponentIndex to remove.
   *
   * @return the SpeciesTypeComponentIndex removed. As mentioned above, the caller owns the
   * returned item.
   */
  SpeciesTypeComponentIndex* removeSpeciesTypeComponentIndex(const std::string& sid);


  /**
   * Returns the  "ListOfInSpeciesTypeBonds" in this MultiSpeciesType object.
   *
   * @return the "ListOfInSpeciesTypeBonds" attribute of this MultiSpeciesType.
   */
  const ListOfInSpeciesTypeBonds* getListOfInSpeciesTypeBonds() const;


  /**
   * Returns the  "ListOfInSpeciesTypeBonds" in this MultiSpeciesType object.
   *
   * @return the "ListOfInSpeciesTypeBonds" attribute of this MultiSpeciesType.
   */
  ListOfInSpeciesTypeBonds* getListOfInSpeciesTypeBonds();


  /**
   * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds.
   *
   * @param n the index number of the InSpeciesTypeBond to get.
   *
   * @return the nth InSpeciesTypeBond in the ListOfInSpeciesTypeBonds within this MultiSpeciesType.
   *
   * @see getNumInSpeciesTypeBonds()
   */
  InSpeciesTypeBond* getInSpeciesTypeBond(unsigned int n);


  /**
   * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds.
   *
   * @param n the index number of the InSpeciesTypeBond to get.
   *
   * @return the nth InSpeciesTypeBond in the ListOfInSpeciesTypeBonds within this MultiSpeciesType.
   *
   * @see getNumInSpeciesTypeBonds()
   */
  const InSpeciesTypeBond* getInSpeciesTypeBond(unsigned int n) const;


  /**
   * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the InSpeciesTypeBond to get.
   *
   * @return the InSpeciesTypeBond in the ListOfInSpeciesTypeBonds
   * with the given id or NULL if no such
   * InSpeciesTypeBond exists.
   *
   * @see getInSpeciesTypeBond(unsigned int n)
   *
   * @see getNumInSpeciesTypeBonds()
   */
  InSpeciesTypeBond* getInSpeciesTypeBond(const std::string& sid);


  /**
   * Get a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the InSpeciesTypeBond to get.
   *
   * @return the InSpeciesTypeBond in the ListOfInSpeciesTypeBonds
   * with the given id or NULL if no such
   * InSpeciesTypeBond exists.
   *
   * @see getInSpeciesTypeBond(unsigned int n)
   *
   * @see getNumInSpeciesTypeBonds()
   */
  const InSpeciesTypeBond* getInSpeciesTypeBond(const std::string& sid) const;


  /**
   * Adds a copy the given "InSpeciesTypeBond" to this MultiSpeciesType.
   *
   * @param istb; the InSpeciesTypeBond object to add
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  int addInSpeciesTypeBond(const InSpeciesTypeBond* istb);


  /**
   * Get the number of InSpeciesTypeBond objects in this MultiSpeciesType.
   *
   * @return the number of InSpeciesTypeBond objects in this MultiSpeciesType
   */
  unsigned int getNumInSpeciesTypeBonds() const;


  /**
   * Creates a new InSpeciesTypeBond object, adds it to this MultiSpeciesTypes
   * ListOfInSpeciesTypeBonds and returns the InSpeciesTypeBond object created. 
   *
   * @return a new InSpeciesTypeBond object instance
   *
   * @see addInSpeciesTypeBond(const InSpeciesTypeBond* istb)
   */
  InSpeciesTypeBond* createInSpeciesTypeBond();


  /**
   * Removes the nth InSpeciesTypeBond from the ListOfInSpeciesTypeBonds within this MultiSpeciesType.
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the InSpeciesTypeBond to remove.
   *
   * @see getNumInSpeciesTypeBonds()
   */
  InSpeciesTypeBond* removeInSpeciesTypeBond(unsigned int n);


  /**
   * Removes the InSpeciesTypeBond with the given identifier from the ListOfInSpeciesTypeBonds within this MultiSpeciesType
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the InSpeciesTypeBond to remove.
   *
   * @return the InSpeciesTypeBond removed. As mentioned above, the caller owns the
   * returned item.
   */
  InSpeciesTypeBond* removeInSpeciesTypeBond(const std::string& sid);


  /**
   * Renames all the @c SIdRef attributes on this element, including any
   * found in MathML content (if such exists).
   *
   * This method works by looking at all attributes and (if appropriate)
   * mathematical formulas, comparing the identifiers to the value of @p
   * oldid.  If any matches are found, the matching identifiers are replaced
   * with @p newid.  The method does @em not descend into child elements.
   *
   * @param oldid the old identifier
   * @param newid the new identifier
   */
   virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitary depth.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object, which for MultiSpeciesType, is
   * always @c "multiSpeciesType".
   *
   * @return the name of this element, i.e. @c "multiSpeciesType".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;


  /**
   * Predicate returning @c true if all the required attributes
   * for this MultiSpeciesType object have been set.
   *
   * @note The required attributes for a MultiSpeciesType object are:
   * @li "id"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Predicate returning @c true if all the required elements
   * for this MultiSpeciesType object have been set.
   *
   * @note The required elements for a MultiSpeciesType object are:
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to child elements.
   */
  virtual void connectToChild ();


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);


  /** @endcond doxygenLibsbmlInternal */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * return the SBML object corresponding to next XMLToken.
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


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



};

class LIBSBML_EXTERN ListOfMultiSpeciesTypes : public ListOf
{

public:

  /**
   * Creates a new ListOfMultiSpeciesTypes with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this ListOfMultiSpeciesTypes
   *
   * @param version an unsigned int, the SBML Version to assign to this ListOfMultiSpeciesTypes
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this ListOfMultiSpeciesTypes
   */
  ListOfMultiSpeciesTypes(unsigned int level      = MultiExtension::getDefaultLevel(),
                          unsigned int version    = MultiExtension::getDefaultVersion(),
                          unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfMultiSpeciesTypes with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  ListOfMultiSpeciesTypes(MultiPkgNamespaces* multins);


   /**
   * Creates and returns a deep copy of this ListOfMultiSpeciesTypes object.
   *
   * @return a (deep) copy of this ListOfMultiSpeciesTypes object.
   */
  virtual ListOfMultiSpeciesTypes* clone () const;


   /**
   * Get a MultiSpeciesType from the ListOfMultiSpeciesTypes.
   *
   * @param n the index number of the MultiSpeciesType to get.
   *
   * @return the nth MultiSpeciesType in this ListOfMultiSpeciesTypes.
   *
   * @see size()
   */
  virtual MultiSpeciesType* get(unsigned int n);


  /**
   * Get a MultiSpeciesType from the ListOfMultiSpeciesTypes.
   *
   * @param n the index number of the MultiSpeciesType to get.
   *
   * @return the nth MultiSpeciesType in this ListOfMultiSpeciesTypes.
   *
   * @see size()
   */
  virtual const MultiSpeciesType* get(unsigned int n) const;


  /**
   * Get a MultiSpeciesType from the ListOfMultiSpeciesTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the MultiSpeciesType to get.
   *
   * @return MultiSpeciesType in this ListOfMultiSpeciesTypes
   * with the given id or NULL if no such
   * MultiSpeciesType exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual MultiSpeciesType* get(const std::string& sid);


  /**
   * Get a MultiSpeciesType from the ListOfMultiSpeciesTypes
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the MultiSpeciesType to get.
   *
   * @return MultiSpeciesType in this ListOfMultiSpeciesTypes
   * with the given id or NULL if no such
   * MultiSpeciesType exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const MultiSpeciesType* get(const std::string& sid) const;


  /**
   * Removes the nth MultiSpeciesType from this ListOfMultiSpeciesTypes
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the MultiSpeciesType to remove.
   *
   * @see size()
   */
  virtual MultiSpeciesType* remove(unsigned int n);


  /**
   * Removes the MultiSpeciesType from this ListOfMultiSpeciesTypes with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the MultiSpeciesType to remove.
   *
   * @return the MultiSpeciesType removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual MultiSpeciesType* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object, which for ListOfMultiSpeciesTypes, is
   * always @c "listOfMultiSpeciesTypes".
   *
   * @return the name of this element, i.e. @c "listOfMultiSpeciesTypes".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;


  /**
   * Returns the libSBML type code for the SBML objects
   * contained in this ListOf object
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for the objects in this ListOf instance, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getItemTypeCode () const;


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new MultiSpeciesType in this ListOfMultiSpeciesTypes
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Multi package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */


  /**
   * Override the virtual function in the parent ListOf class. Returns true
   * if the item is an object of MultiSpeciesType or BindingSiteSpeciesType.
   */
  virtual bool isValidTypeForList(SBase * item);


};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
MultiSpeciesType_t *
MultiSpeciesType_create(unsigned int level, unsigned int version,
                        unsigned int pkgVersion);


LIBSBML_EXTERN
void
MultiSpeciesType_free(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
MultiSpeciesType_t *
MultiSpeciesType_clone(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
char *
MultiSpeciesType_getId(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
char *
MultiSpeciesType_getName(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
char *
MultiSpeciesType_getCompartment(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
int
MultiSpeciesType_isSetId(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
int
MultiSpeciesType_isSetName(MultiSpeciesType_t * mst);




LIBSBML_EXTERN
int
MultiSpeciesType_isSetCompartment(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
int
MultiSpeciesType_setId(MultiSpeciesType_t * mst, const char * id);


LIBSBML_EXTERN
int
MultiSpeciesType_setName(MultiSpeciesType_t * mst, const char * name);




LIBSBML_EXTERN
int
MultiSpeciesType_setCompartment(MultiSpeciesType_t * mst, const char * compartment);


LIBSBML_EXTERN
int
MultiSpeciesType_unsetId(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
int
MultiSpeciesType_unsetName(MultiSpeciesType_t * mst);




LIBSBML_EXTERN
int
MultiSpeciesType_unsetCompartment(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
int
MultiSpeciesType_addSpeciesFeatureType(MultiSpeciesType_t * mst, SpeciesFeatureType_t * sft);


LIBSBML_EXTERN
SpeciesFeatureType_t *
MultiSpeciesType_createSpeciesFeatureType(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
ListOf_t *
MultiSpeciesType_getListOfSpeciesFeatureTypes(MultiSpeciesType_t * mst) ;


LIBSBML_EXTERN
SpeciesFeatureType_t *
MultiSpeciesType_getSpeciesFeatureType(MultiSpeciesType_t * mst, unsigned int n);


LIBSBML_EXTERN
SpeciesFeatureType_t *
MultiSpeciesType_getSpeciesFeatureTypeById(MultiSpeciesType_t * mst, const char * sid);


LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumSpeciesFeatureTypes(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
SpeciesFeatureType_t *
MultiSpeciesType_removeSpeciesFeatureType(MultiSpeciesType_t * mst, unsigned int n);


LIBSBML_EXTERN
SpeciesFeatureType_t *
MultiSpeciesType_removeSpeciesFeatureTypeById(MultiSpeciesType_t * mst, const char * sid);


LIBSBML_EXTERN
int
MultiSpeciesType_addSpeciesTypeInstance(MultiSpeciesType_t * mst, SpeciesTypeInstance_t * sti);


LIBSBML_EXTERN
SpeciesTypeInstance_t *
MultiSpeciesType_createSpeciesTypeInstance(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
ListOf_t *
MultiSpeciesType_getListOfSpeciesTypeInstances(MultiSpeciesType_t * mst) ;


LIBSBML_EXTERN
SpeciesTypeInstance_t *
MultiSpeciesType_getSpeciesTypeInstance(MultiSpeciesType_t * mst, unsigned int n);


LIBSBML_EXTERN
SpeciesTypeInstance_t *
MultiSpeciesType_getSpeciesTypeInstanceById(MultiSpeciesType_t * mst, const char * sid);


LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumSpeciesTypeInstances(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
SpeciesTypeInstance_t *
MultiSpeciesType_removeSpeciesTypeInstance(MultiSpeciesType_t * mst, unsigned int n);


LIBSBML_EXTERN
SpeciesTypeInstance_t *
MultiSpeciesType_removeSpeciesTypeInstanceById(MultiSpeciesType_t * mst, const char * sid);


LIBSBML_EXTERN
int
MultiSpeciesType_addSpeciesTypeComponentIndex(MultiSpeciesType_t * mst, SpeciesTypeComponentIndex_t * stci);


LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
MultiSpeciesType_createSpeciesTypeComponentIndex(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
ListOf_t *
MultiSpeciesType_getListOfSpeciesTypeComponentIndexes(MultiSpeciesType_t * mst) ;


LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
MultiSpeciesType_getSpeciesTypeComponentIndex(MultiSpeciesType_t * mst, unsigned int n);


LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
MultiSpeciesType_getSpeciesTypeComponentIndexById(MultiSpeciesType_t * mst, const char * sid);


LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumSpeciesTypeComponentIndexes(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
MultiSpeciesType_removeSpeciesTypeComponentIndex(MultiSpeciesType_t * mst, unsigned int n);


LIBSBML_EXTERN
SpeciesTypeComponentIndex_t *
MultiSpeciesType_removeSpeciesTypeComponentIndexById(MultiSpeciesType_t * mst, const char * sid);


LIBSBML_EXTERN
int
MultiSpeciesType_addInSpeciesTypeBond(MultiSpeciesType_t * mst, InSpeciesTypeBond_t * istb);


LIBSBML_EXTERN
InSpeciesTypeBond_t *
MultiSpeciesType_createInSpeciesTypeBond(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
ListOf_t *
MultiSpeciesType_getListOfInSpeciesTypeBonds(MultiSpeciesType_t * mst) ;


LIBSBML_EXTERN
InSpeciesTypeBond_t *
MultiSpeciesType_getInSpeciesTypeBond(MultiSpeciesType_t * mst, unsigned int n);


LIBSBML_EXTERN
InSpeciesTypeBond_t *
MultiSpeciesType_getInSpeciesTypeBondById(MultiSpeciesType_t * mst, const char * sid);


LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumInSpeciesTypeBonds(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
InSpeciesTypeBond_t *
MultiSpeciesType_removeInSpeciesTypeBond(MultiSpeciesType_t * mst, unsigned int n);


LIBSBML_EXTERN
InSpeciesTypeBond_t *
MultiSpeciesType_removeInSpeciesTypeBondById(MultiSpeciesType_t * mst, const char * sid);


LIBSBML_EXTERN
int
MultiSpeciesType_hasRequiredAttributes(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
int
MultiSpeciesType_hasRequiredElements(MultiSpeciesType_t * mst);


LIBSBML_EXTERN
MultiSpeciesType_t *
ListOfMultiSpeciesTypes_getById(ListOf_t * lo, const char * sid);


LIBSBML_EXTERN
MultiSpeciesType_t *
ListOfMultiSpeciesTypes_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  MultiSpeciesType_H__  */

