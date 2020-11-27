/**
 * @file:   MultiSpeciesType.h
 * @brief:  Implementation of the MultiSpeciesType class
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
 * @class MultiSpeciesType
 * @sbmlbrief{multi} A type of Species in SBML Level&nbsp;3 "multi"
 *
 * The MultiSpeciesType class (defined simply as @c SpeciesType in the SBML
 * Level&nbsp;3 "multi" specification, but called MultiSpeciesType here to
 * distinguish it from the SpeciesType class defined in SBML Level&nbsp;2),
 * is a child of the extended Model object (via the MultiModelPlugin class).
 * It defines "id" and "name" attributes, an optional "compartment" attribute
 * for indicating which Compartment the referencing Species is in, and four
 * optional lists for child SpeciesFeatureType, SpeciesTypeInstance,
 * SpeciesTypeComponentIndex, and InSpeciesTypeBond objects.  Together those
 * children define the species type.  The ListOfSpeciesTypeInstances
 * subobject provides a way to define multicomponents which are instances of
 * other MultiSpeciesType objects. The ListOfSpeciesFeatureTypes subobject
 * and its SpeciesFeatureType children set up a framework for the referencing
 * species or the instances of MultiSpeciesType objects to be able to have
 * multistates. The ListOfSpeciesTypeComponentIndexes subobject provides a
 * flexible way to reference any component in a MultiSpeciesType.  The
 * ListOfInSpeciesTypeBonds subobject and its InSpeciesTypeBond children
 * provides a way to define bonds within a MultiSpeciesType.
 *
 * @class ListOfMultiSpeciesTypes
 * @sbmlbrief{multi} A list of MultiSpeciesType objects.
 *
 * The ListOfMultiSpeciesTypes is a container for MultiSpeciesType objects.
 *
 * @copydetails doc_what_is_listof
 *
 * @see MultiSpeciesType
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
  /** @cond doxygenLibsbmlInternal */

  ////  std::string                           mId;
  ////  std::string                           mName;
  std::string                           mCompartment;
  ListOfSpeciesFeatureTypes             mListOfSpeciesFeatureTypes;
  ListOfSpeciesTypeInstances            mListOfSpeciesTypeInstances;
  ListOfSpeciesTypeComponentIndexes     mListOfSpeciesTypeComponentIndexes;
  ListOfInSpeciesTypeBonds              mListOfInSpeciesTypeBonds;

  /** @endcond */


public:

  /**
   * Creates a new MultiSpeciesType object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  MultiSpeciesType(unsigned int level      = MultiExtension::getDefaultLevel(),
                   unsigned int version    = MultiExtension::getDefaultVersion(),
                   unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new MultiSpeciesType with the given MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  MultiSpeciesType(MultiPkgNamespaces* multins);


   /**
   * Copy constructor for MultiSpeciesType.
   *
   * @param orig the MultiSpeciesType instance to copy.
   */
  MultiSpeciesType(const MultiSpeciesType& orig);


   /**
   * Assignment operator for MultiSpeciesType.
   *
   * @param rhs the object whose values are used as the basis
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
   * @return the value of the "id" attribute of this MultiSpeciesType as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns @c true if whether this MultiSpeciesType's "id" attribute has
   * been set.
   *
   * @return @c true if this MultiSpeciesType's "id" attribute has been set;
   * otherwise, @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this MultiSpeciesType.
   *
   * @param id const std::string& value of the "id" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this MultiSpeciesType.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this MultiSpeciesType.
   *
   * @return the value of the "name" attribute of this MultiSpeciesType as a
   * string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns @c true if this MultiSpeciesType's "name" attribute has been
   * set.
   *
   * @return @c true if this MultiSpeciesType's "name" attribute has been
   * set; otherwise, @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this MultiSpeciesType.
   *
   * @param name const std::string& value of the "name" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this MultiSpeciesType.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "compartment" attribute of this
   * MultiSpeciesType.
   *
   * @return the value of the "compartment" attribute of this
   * MultiSpeciesType as a string.
   */
  virtual const std::string& getCompartment() const;


  /**
   * Returns @c true if this MultiSpeciesType's "compartment" attribute has
   * been set.
   *
   * @return @c true if this MultiSpeciesType's "compartment" attribute has
   * been set; otherwise, @c false is returned.
   */
  virtual bool isSetCompartment() const;


  /**
   * Sets the value of the "compartment" attribute of this MultiSpeciesType.
   *
   * @param compartment the new value for the attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setCompartment(const std::string& compartment);


  /**
   * Unsets the value of the "compartment" attribute of this MultiSpeciesType.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetCompartment();


  /**
   * Returns the ListOfSpeciesFeatureTypes in this MultiSpeciesType object.
   *
   * @return the ListOfSpeciesFeatureTypes child of this
   * MultiSpeciesType.
   */
  const ListOfSpeciesFeatureTypes* getListOfSpeciesFeatureTypes() const;


  /**
   * Returns the ListOfSpeciesFeatureTypes in this MultiSpeciesType object.
   *
   * @return the ListOfSpeciesFeatureTypes child of this
   * MultiSpeciesType.
   */
  ListOfSpeciesFeatureTypes* getListOfSpeciesFeatureTypes();


  /**
   * Returns the nth SpeciesFeatureType object from the
   * ListOfSpeciesFeatureTypes.
   *
   * @param n the index number of the SpeciesFeatureType to get.
   *
   * @return the nth SpeciesFeatureType in the ListOfSpeciesFeatureTypes
   * within this MultiSpeciesType.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see getNumSpeciesFeatureTypes()
   */
  SpeciesFeatureType* getSpeciesFeatureType(unsigned int n);


  /**
   * Returns the nth SpeciesFeatureType object from the
   * ListOfSpeciesFeatureTypes.
   *
   * @param n the index number of the SpeciesFeatureType to get.
   *
   * @return the nth SpeciesFeatureType in the ListOfSpeciesFeatureTypes
   * within this MultiSpeciesType.
   * If the index @p n is invalid, @c NULL is returned.
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
   * @return the SpeciesFeatureType in the ListOfSpeciesFeatureTypes with the
   * given id, or @c NULL if no such SpeciesFeatureType exists.
   *
   * @see getSpeciesFeatureType(unsigned int n)
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
   * @return the SpeciesFeatureType in the ListOfSpeciesFeatureTypes with the
   * given id, or @c NULL if no such SpeciesFeatureType exists.
   *
   * @see getSpeciesFeatureType(unsigned int n)
   * @see getNumSpeciesFeatureTypes()
   */
  const SpeciesFeatureType* getSpeciesFeatureType(const std::string& sid) const;


  /**
   * Adds a copy the given "SpeciesFeatureType" to this MultiSpeciesType.
   *
   * @param sft the SpeciesFeatureType object to add
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  int addSpeciesFeatureType(const SpeciesFeatureType* sft);


  /**
   * Get the number of SpeciesFeatureType objects in this MultiSpeciesType.
   *
   * @return the number of SpeciesFeatureType objects in this
   * MultiSpeciesType
   */
  unsigned int getNumSpeciesFeatureTypes() const;


  /**
   * Creates a new SpeciesFeatureType object and adds it to this
   * MultiSpeciesTypes ListOfSpeciesFeatureTypes.
   *
   * @return the newly created SpeciesFeatureType object instance.
   *
   * @see addSpeciesFeatureType(const SpeciesFeatureType* sft)
   */
  SpeciesFeatureType* createSpeciesFeatureType();


  /**
   * Removes the nth SpeciesFeatureType from the ListOfSpeciesFeatureTypes
   * within this MultiSpeciesType object.
   *
   * @param n the index of the SpeciesFeatureType to remove.
   *
   * @return the SpeciesFeatureType object removed, or @c NULL if the given
   * index @p n is out of range.  Note that the caller owns the returned
   * object and is responsible for deleting it.
   *
   * @see getNumSpeciesFeatureTypes()
   */
  SpeciesFeatureType* removeSpeciesFeatureType(unsigned int n);


  /**
   * Removes the SpeciesFeatureType with the given identifier from the
   * ListOfSpeciesFeatureTypes object.
   *
   * @param sid the identifier of the SpeciesFeatureType to remove.
   *
   * @return the SpeciesFeatureType removed, or @c NULL if none have the
   * identifier @p sid.  Note that the caller owns the returned item and is
   * responsible for deleting it.
   */
  SpeciesFeatureType* removeSpeciesFeatureType(const std::string& sid);


  /**
   * Returns the ListOfSpeciesTypeInstances in this MultiSpeciesType
   * object.
   *
   * @return the ListOfSpeciesTypeInstances child of this
   * MultiSpeciesType.
   */
  const ListOfSpeciesTypeInstances* getListOfSpeciesTypeInstances() const;


  /**
   * Returns the ListOfSpeciesTypeInstances in this MultiSpeciesType
   * object.
   *
   * @return the ListOfSpeciesTypeInstances child of this
   * MultiSpeciesType.
   */
  ListOfSpeciesTypeInstances* getListOfSpeciesTypeInstances();


  /**
   * Get the nth SpeciesTypeInstance object from the
   * ListOfSpeciesTypeInstances.
   *
   * @param n the index number of the SpeciesTypeInstance to get.
   *
   * @return the nth SpeciesTypeInstance object in the
   * ListOfSpeciesTypeInstances, or @c NULL if the given index is out of range.
   *
   * @see getNumSpeciesTypeInstances()
   */
  SpeciesTypeInstance* getSpeciesTypeInstance(unsigned int n);


  /**
   * Get the nth SpeciesTypeInstance object from the
   * ListOfSpeciesTypeInstances.
   *
   * @param n the index number of the SpeciesTypeInstance to get.
   *
   * @return the nth SpeciesTypeInstance object in the
   * ListOfSpeciesTypeInstances, or @c NULL if the given index is out of range.
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
   * @return the SpeciesTypeInstance in the ListOfSpeciesTypeInstances with
   * the given id, or @c NULL if no such SpeciesTypeInstance exists.
   *
   * @see getSpeciesTypeInstance(unsigned int n)
   * @see getNumSpeciesTypeInstances()
   */
  SpeciesTypeInstance* getSpeciesTypeInstance(const std::string& sid);


  /**
   * Get a SpeciesTypeInstance from the ListOfSpeciesTypeInstances based on
   * its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesTypeInstance to get.
   *
   * @return the SpeciesTypeInstance in the ListOfSpeciesTypeInstances with
   * the given id, or @c NULL if no such SpeciesTypeInstance exists.
   *
   * @see getSpeciesTypeInstance(unsigned int n)
   * @see getNumSpeciesTypeInstances()
   */
  const SpeciesTypeInstance* getSpeciesTypeInstance(const std::string& sid) const;


  /**
   * Adds a copy the given "SpeciesTypeInstance" to this MultiSpeciesType.
   *
   * @param sti the SpeciesTypeInstance object to add
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  int addSpeciesTypeInstance(const SpeciesTypeInstance* sti);


  /**
   * Get the number of SpeciesTypeInstance objects in this MultiSpeciesType.
   *
   * @return the number of SpeciesTypeInstance objects in this MultiSpeciesType
   */
  unsigned int getNumSpeciesTypeInstances() const;


  /**
   * Creates a new SpeciesTypeInstance object and adds it to this
   * MultiSpeciesTypes ListOfSpeciesTypeInstances.
   *
   * @return a new SpeciesTypeInstance object instance.
   *
   * @see addSpeciesTypeInstance(const SpeciesTypeInstance* sti)
   */
  SpeciesTypeInstance* createSpeciesTypeInstance();


  /**
   * Removes the nth SpeciesTypeInstance from the ListOfSpeciesTypeInstances
   * within this MultiSpeciesType.
   *
   * @param n the index of the SpeciesTypeInstance to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see getNumSpeciesTypeInstances()
   */
  SpeciesTypeInstance* removeSpeciesTypeInstance(unsigned int n);


  /**
   * Removes the SpeciesTypeInstance with the given identifier from the
   * ListOfSpeciesTypeInstances within this MultiSpeciesType.
   *
   * @param sid the identifier of the SpeciesTypeInstance to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   */
  SpeciesTypeInstance* removeSpeciesTypeInstance(const std::string& sid);


  /**
   * Returns the ListOfSpeciesTypeComponentIndexes object.
   *
   * @return the ListOfSpeciesTypeComponentIndexes object in this
   * MultiSpeciesType object.
   */
  const ListOfSpeciesTypeComponentIndexes* getListOfSpeciesTypeComponentIndexes() const;


  /**
   * Returns the ListOfSpeciesTypeComponentIndexes object.
   *
   * @return the ListOfSpeciesTypeComponentIndexes object in this
   * MultiSpeciesType object.
   */
  ListOfSpeciesTypeComponentIndexes* getListOfSpeciesTypeComponentIndexes();


  /**
   * Get the nth SpeciesTypeComponentIndex object from the
   * ListOfSpeciesTypeComponentIndexes.
   *
   * @param n the index number of the SpeciesTypeComponentIndex to get from
   * the ListOfSpeciesTypeComponentIndexes.
   *
   * @return the nth object in the ListOfSpeciesTypeComponentIndexes, or @c NULL
   * if the index @p n is out of range.
   *
   * @see getNumSpeciesTypeComponentIndexes()
   */
  SpeciesTypeComponentIndex* getSpeciesTypeComponentIndex(unsigned int n);


  /**
   * Get the nth SpeciesTypeComponentIndex object from the
   * ListOfSpeciesTypeComponentIndexes.
   *
   * @param n the index number of the SpeciesTypeComponentIndex to get from
   * the ListOfSpeciesTypeComponentIndexes.
   *
   * @return the nth object in the ListOfSpeciesTypeComponentIndexes, or @c NULL
   * if the index @p n is out of range.
   *
   * @see getNumSpeciesTypeComponentIndexes()
   */
  const SpeciesTypeComponentIndex* getSpeciesTypeComponentIndex(unsigned int n) const;


  /**
   * Get a SpeciesTypeComponentIndex object based on its identifier.
   *
   * @param sid a string representing the identifier of the
   * SpeciesTypeComponentIndex to get from the
   * ListOfSpeciesTypeComponentIndexes.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see getSpeciesTypeComponentIndex(unsigned int n)
   * @see getNumSpeciesTypeComponentIndexes()
   */
  SpeciesTypeComponentIndex* getSpeciesTypeComponentIndex(const std::string& sid);


  /**
   * Get a SpeciesTypeComponentIndex object based on its identifier.
   *
   * @param sid a string representing the identifier of the
   * SpeciesTypeComponentIndex to get from the
   * ListOfSpeciesTypeComponentIndexes.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see getSpeciesTypeComponentIndex(unsigned int n)
   * @see getNumSpeciesTypeComponentIndexes()
   */
  const SpeciesTypeComponentIndex* getSpeciesTypeComponentIndex(const std::string& sid) const;


  /**
   * Adds a copy of the given SpeciesTypeComponentIndex object to this
   * MultiSpeciesType.
   *
   * @param stci the SpeciesTypeComponentIndex object to add
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  int addSpeciesTypeComponentIndex(const SpeciesTypeComponentIndex* stci);


  /**
   * Get the number of SpeciesTypeComponentIndex objects in this
   * MultiSpeciesType.
   *
   * @return the number of SpeciesTypeComponentIndex objects in the
   * ListOfSpeciesTypeComponentIndexes object within this MultiSpeciesType
   * object.
   */
  unsigned int getNumSpeciesTypeComponentIndexes() const;


  /**
   * Creates a new SpeciesTypeComponentIndex object and adds it to the
   * ListOfSpeciesTypeComponentIndexes.
   *
   * @return a new SpeciesTypeComponentIndex object instance to add to the
   * ListOfSpeciesTypeComponentIndexes object within this MultiSpeciesType
   * object.
   *
   * @see addSpeciesTypeComponentIndex(const SpeciesTypeComponentIndex* stci)
   */
  SpeciesTypeComponentIndex* createSpeciesTypeComponentIndex();


  /**
   * Removes the nth SpeciesTypeComponentIndex object from the
   * ListOfSpeciesTypeComponentIndexes.
   *
   * @param n the index of the SpeciesTypeComponentIndex to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see getNumSpeciesTypeComponentIndexes()
   */
  SpeciesTypeComponentIndex* removeSpeciesTypeComponentIndex(unsigned int n);


  /**
   * Removes the SpeciesTypeComponentIndex object with the given identifier
   * @p sid.
   *
   * @param sid the identifier to search for.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see getNumSpeciesTypeComponentIndexes()
   */
  SpeciesTypeComponentIndex* removeSpeciesTypeComponentIndex(const std::string& sid);


  /**
   * Returns the ListOfInSpeciesTypeBonds object.
   *
   * @return the ListOfInSpeciesTypeBonds child of this MultiSpeciesType.
   */
  const ListOfInSpeciesTypeBonds* getListOfInSpeciesTypeBonds() const;


  /**
   * Returns the ListOfInSpeciesTypeBonds object.
   *
   * @return the ListOfInSpeciesTypeBonds child of this MultiSpeciesType.
   */
  ListOfInSpeciesTypeBonds* getListOfInSpeciesTypeBonds();


  /**
   * Get the nth InSpeciesTypeBond object from the ListOfInSpeciesTypeBonds.
   *
   * @param n the index number of the InSpeciesTypeBond to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see getNumInSpeciesTypeBonds()
   */
  InSpeciesTypeBond* getInSpeciesTypeBond(unsigned int n);


  /**
   * Get the nth InSpeciesTypeBond object from the ListOfInSpeciesTypeBonds.
   *
   * @param n the index number of the InSpeciesTypeBond to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see getNumInSpeciesTypeBonds()
   */
  const InSpeciesTypeBond* getInSpeciesTypeBond(unsigned int n) const;


  /**
   * Get the InSpeciesTypeBond object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the InSpeciesTypeBond
   * to get from this ListOfInSpeciesTypeBonds.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see getInSpeciesTypeBond(unsigned int n)
   * @see getNumInSpeciesTypeBonds()
   */
  InSpeciesTypeBond* getInSpeciesTypeBond(const std::string& sid);


  /**
   * Get the InSpeciesTypeBond object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the InSpeciesTypeBond
   * to get from this ListOfInSpeciesTypeBonds.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see getInSpeciesTypeBond(unsigned int n)
   * @see getNumInSpeciesTypeBonds()
   */
  const InSpeciesTypeBond* getInSpeciesTypeBond(const std::string& sid) const;


  /**
   * Adds a copy the given InSpeciesTypeBond object to this MultiSpeciesType.
   *
   * @param istb the InSpeciesTypeBond object to add
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  int addInSpeciesTypeBond(const InSpeciesTypeBond* istb);


  /**
   * Get the number of InSpeciesTypeBond objects in the
   * ListOfInSpeciesTypeBonds.
   *
   * @return the number of InSpeciesTypeBond objects in the
   * ListOfInSpeciesTypeBonds within this MultiSpeciesType object.
   */
  unsigned int getNumInSpeciesTypeBonds() const;


  /**
   * Creates a new InSpeciesTypeBond object and adds it to the
   * ListOfInSpeciesTypeBonds.
   *
   * @return a new InSpeciesTypeBond object instance.
   *
   * @see addInSpeciesTypeBond(const InSpeciesTypeBond* istb)
   */
  InSpeciesTypeBond* createInSpeciesTypeBond();


  /**
   * Removes the nth InSpeciesTypeBond object from the
   * ListOfInSpeciesTypeBonds.  and returns a pointer to it.
   *
   * @param n the index of the InSpeciesTypeBond to remove.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see getNumInSpeciesTypeBonds()
   */
  InSpeciesTypeBond* removeInSpeciesTypeBond(unsigned int n);


  /**
   * Removes the InSpeciesTypeBond with the given identifier @p sid.
   *
   * @param sid the identifier of the InSpeciesTypeBond to remove from
   * the ListOfInSpeciesTypeBonds within this MultiSpeciesType object.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
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
   * @param filter a pointer to an ElementFilter, which causes the function
   * to return only elements that match a particular set of constraints.
   * If NULL (the default), the function will return all child objects.
   *
   * @return a List of pointers to all child objects.
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
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_MULTI_BINDING_SITE_SPECIES_TYPE, SBMLMultiTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


  /**
   * Returns @c true if this object has all the required attributes.
   *
   * @note The required attributes for a MultiSpeciesType object are:
   * @li "id"
   *
   * @return @c true if this object has all the elements required by the
   * package specification; otherwise, @c false will be returned.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Returns @c true if this object has all the required elements.
   *
   * @return @c true if this object has all the elements required by the
   * package specification; otherwise, @c false will be returned.
   */
  virtual bool hasRequiredElements() const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Connects to child elements.
   */
  virtual void connectToChild ();

  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);
  /** @endcond */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * return the SBML object corresponding to next XMLToken.
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond */


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



};

class LIBSBML_EXTERN ListOfMultiSpeciesTypes : public ListOf
{

public:

  /**
   * Creates a new ListOfMultiSpeciesTypes object
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfMultiSpeciesTypes(unsigned int level      = MultiExtension::getDefaultLevel(),
                          unsigned int version    = MultiExtension::getDefaultVersion(),
                          unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfMultiSpeciesTypes with the given MultiPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfMultiSpeciesTypes(MultiPkgNamespaces* multins);


  /**
   * Creates and returns a deep copy of this ListOfMultiSpeciesTypes object.
   *
   * @return a (deep) copy of this ListOfMultiSpeciesTypes object.
   */
  virtual ListOfMultiSpeciesTypes* clone () const;


  /**
   * Get the nth MultiSpeciesType from the ListOfMultiSpeciesTypes.
   *
   * @param n the index number of the MultiSpeciesType to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual MultiSpeciesType* get(unsigned int n);


  /**
   * Get the nth MultiSpeciesType from the ListOfMultiSpeciesTypes.
   *
   * @param n the index number of the MultiSpeciesType to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual const MultiSpeciesType* get(unsigned int n) const;


  /**
   * Get the MultiSpeciesType with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the MultiSpeciesType
   * to get from this ListOfMultiSpeciesTypes.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual MultiSpeciesType* get(const std::string& sid);


  /**
   * Get the MultiSpeciesType with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the MultiSpeciesType
   * to get from this ListOfMultiSpeciesTypes.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const MultiSpeciesType* get(const std::string& sid) const;


  /**
   * Removes the nth MultiSpeciesType from this ListOfMultiSpeciesTypes.
   *
   * @param n the index of the MultiSpeciesType to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see size()
   */
  virtual MultiSpeciesType* remove(unsigned int n);


  /**
   * Removes the MultiSpeciesType with the given identifier @p sid.
   *
   * @param sid the identifier of the MultiSpeciesType to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   */
  virtual MultiSpeciesType* remove(const std::string& sid);


  /**
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "listOfMultiSpeciesTypes".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_MULTI_BINDING_SITE_SPECIES_TYPE, SBMLMultiTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


  /**
   * Returns the libSBML type code for the objects contained in this ListOf
   * (i.e., Compartment objects, if the list is non-empty).
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for the objects contained in this ListOf
   * instance: @sbmlconstant{SBML_COMPARTMENT, SBMLTypeCode_t} (default).
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode () const;


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new MultiSpeciesType in this ListOfMultiSpeciesTypes
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Multi package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond */


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

/**
 * Creates a new BindingSiteSpeciesType (MultiSpeciesType_t) using the given
 * SBML Level, Version and &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * MultiSpeciesType_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * MultiSpeciesType_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * MultiSpeciesType_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
MultiSpeciesType_t *
MultiSpeciesType_create(unsigned int level, unsigned int version,
                        unsigned int pkgVersion);


/**
 * Frees this MultiSpeciesType_t object.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
void
MultiSpeciesType_free(MultiSpeciesType_t * mst);


/**
 * Creates and returns a deep copy of this MultiSpeciesType_t object.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @return a (deep) copy of this MultiSpeciesType_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
MultiSpeciesType_t *
MultiSpeciesType_clone(MultiSpeciesType_t * mst);


/**
 * Returns the value of the "id" attribute of this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this MultiSpeciesType_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
char *
MultiSpeciesType_getId(const MultiSpeciesType_t * mst);


/**
 * Returns the value of the "name" attribute of this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this MultiSpeciesType_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
char *
MultiSpeciesType_getName(const MultiSpeciesType_t * mst);


/**
 * Returns the value of the "compartment" attribute of this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure whose compartment is sought.
 *
 * @return the value of the "compartment" attribute of this MultiSpeciesType_t
 * as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
char *
MultiSpeciesType_getCompartment(const MultiSpeciesType_t * mst);


/**
 * Predicate returning @c 1 (true) if this MultiSpeciesType_t's "id" attribute
 * is set.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @return @c 1 (true) if this MultiSpeciesType_t's "id" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_isSetId(const MultiSpeciesType_t * mst);


/**
 * Predicate returning @c 1 (true) if this MultiSpeciesType_t's "name"
 * attribute is set.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @return @c 1 (true) if this MultiSpeciesType_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_isSetName(const MultiSpeciesType_t * mst);


/**
 * Predicate returning @c 1 (true) if this MultiSpeciesType_t's "compartment"
 * attribute is set.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @return @c 1 (true) if this MultiSpeciesType_t's "compartment" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_isSetCompartment(const MultiSpeciesType_t * mst);


/**
 * Sets the value of the "id" attribute of this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling MultiSpeciesType_unsetId().
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_setId(MultiSpeciesType_t * mst, const char * id);


/**
 * Sets the value of the "name" attribute of this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling MultiSpeciesType_unsetName().
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_setName(MultiSpeciesType_t * mst, const char * name);


/**
 * Sets the value of the "compartment" attribute of this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @param compartment const char * value of the "compartment" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_setCompartment(MultiSpeciesType_t * mst, const char * compartment);


/**
 * Unsets the value of the "id" attribute of this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_unsetId(MultiSpeciesType_t * mst);


/**
 * Unsets the value of the "name" attribute of this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_unsetName(MultiSpeciesType_t * mst);


/**
 * Unsets the value of the "compartment" attribute of this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_unsetCompartment(MultiSpeciesType_t * mst);


/**
 * Returns a ListOf_t * containing SpeciesFeatureType_t objects from this
 * MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure whose ListOfSpeciesFeatureTypes
 * is sought.
 *
 * @return the ListOfSpeciesFeatureTypes from this MultiSpeciesType_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see MultiSpeciesType_addSpeciesFeatureType()
 * @see MultiSpeciesType_createSpeciesFeatureType()
 * @see MultiSpeciesType_getSpeciesFeatureTypeById()
 * @see MultiSpeciesType_getSpeciesFeatureType()
 * @see MultiSpeciesType_getNumSpeciesFeatureTypes()
 * @see MultiSpeciesType_removeSpeciesFeatureTypeById()
 * @see MultiSpeciesType_removeSpeciesFeatureType()
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesType_getListOfSpeciesFeatureTypes(MultiSpeciesType_t* mst);


/**
 * Get a SpeciesFeatureType_t from the MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param n an unsigned int representing the index of the SpeciesFeatureType_t
 * to retrieve.
 *
 * @return the nth SpeciesFeatureType_t in the ListOfSpeciesFeatureTypes within
 * this MultiSpeciesType.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesFeatureType_t*
MultiSpeciesType_getSpeciesFeatureType(MultiSpeciesType_t* mst,
                                       unsigned int n);


/**
 * Get a SpeciesFeatureType_t from the MultiSpeciesType_t based on its
 * identifier.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param sid a string representing the identifier of the SpeciesFeatureType_t
 * to retrieve.
 *
 * @return the SpeciesFeatureType_t in the ListOfSpeciesFeatureTypes within
 * this MultiSpeciesType with the given @p sid or @c NULL if no such
 * SpeciesFeatureType_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesFeatureType_t*
MultiSpeciesType_getSpeciesFeatureTypeById(MultiSpeciesType_t* mst,
                                           const char *sid);


/**
 * Adds a copy of the given SpeciesFeatureType_t to this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to which the
 * SpeciesFeatureType_t should be added.
 *
 * @param sft the SpeciesFeatureType_t object to add.
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
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_addSpeciesFeatureType(MultiSpeciesType_t* mst,
                                       const SpeciesFeatureType_t* sft);


/**
 * Get the number of SpeciesFeatureType_t objects in this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to query.
 *
 * @return the number of SpeciesFeatureType_t objects in this
 * MultiSpeciesType_t.
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumSpeciesFeatureTypes(MultiSpeciesType_t* mst);


/**
 * Creates a new SpeciesFeatureType_t object, adds it to this
 * MultiSpeciesType_t object and returns the SpeciesFeatureType_t object
 * created.
 *
 * @param mst the MultiSpeciesType_t structure to which the
 * SpeciesFeatureType_t should be added.
 *
 * @return a new SpeciesFeatureType_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesFeatureType_t*
MultiSpeciesType_createSpeciesFeatureType(MultiSpeciesType_t* mst);


/**
 * Removes the nth SpeciesFeatureType_t from this MultiSpeciesType_t and
 * returns a pointer to it.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param n an unsigned int representing the index of the SpeciesFeatureType_t
 * to remove.
 *
 * @return a pointer to the nth SpeciesFeatureType_t in this
 * MultiSpeciesType_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesFeatureType_t*
MultiSpeciesType_removeSpeciesFeatureType(MultiSpeciesType_t* mst,
                                          unsigned int n);


/**
 * Removes the SpeciesFeatureType_t from this MultiSpeciesType_t based on its
 * identifier and returns a pointer to it.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param sid a string representing the identifier of the SpeciesFeatureType_t
 * to remove.
 *
 * @return the SpeciesFeatureType_t in this MultiSpeciesType_t based on the
 * identifier or NULL if no such SpeciesFeatureType_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesFeatureType_t*
MultiSpeciesType_removeSpeciesFeatureTypeById(MultiSpeciesType_t* mst,
                                              const char* sid);


/**
 * Returns a ListOf_t * containing SpeciesTypeInstance_t objects from this
 * MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure whose ListOfSpeciesTypeInstances
 * is sought.
 *
 * @return the ListOfSpeciesTypeInstances from this MultiSpeciesType_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see MultiSpeciesType_addSpeciesTypeInstance()
 * @see MultiSpeciesType_createSpeciesTypeInstance()
 * @see MultiSpeciesType_getSpeciesTypeInstanceById()
 * @see MultiSpeciesType_getSpeciesTypeInstance()
 * @see MultiSpeciesType_getNumSpeciesTypeInstances()
 * @see MultiSpeciesType_removeSpeciesTypeInstanceById()
 * @see MultiSpeciesType_removeSpeciesTypeInstance()
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesType_getListOfSpeciesTypeInstances(MultiSpeciesType_t* mst);


/**
 * Get a SpeciesTypeInstance_t from the MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param n an unsigned int representing the index of the SpeciesTypeInstance_t
 * to retrieve.
 *
 * @return the nth SpeciesTypeInstance_t in the ListOfSpeciesTypeInstances
 * within this MultiSpeciesType.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t*
MultiSpeciesType_getSpeciesTypeInstance(MultiSpeciesType_t* mst,
                                        unsigned int n);


/**
 * Get a SpeciesTypeInstance_t from the MultiSpeciesType_t based on its
 * identifier.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param sid a string representing the identifier of the SpeciesTypeInstance_t
 * to retrieve.
 *
 * @return the SpeciesTypeInstance_t in the ListOfSpeciesTypeInstances within
 * this MultiSpeciesType with the given @p sid or @c NULL if no such
 * SpeciesTypeInstance_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t*
MultiSpeciesType_getSpeciesTypeInstanceById(MultiSpeciesType_t* mst,
                                            const char *sid);


/**
 * Adds a copy of the given SpeciesTypeInstance_t to this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to which the
 * SpeciesTypeInstance_t should be added.
 *
 * @param sti the SpeciesTypeInstance_t object to add.
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
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_addSpeciesTypeInstance(MultiSpeciesType_t* mst,
                                        const SpeciesTypeInstance_t* sti);


/**
 * Get the number of SpeciesTypeInstance_t objects in this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to query.
 *
 * @return the number of SpeciesTypeInstance_t objects in this
 * MultiSpeciesType_t.
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumSpeciesTypeInstances(MultiSpeciesType_t* mst);


/**
 * Creates a new SpeciesTypeInstance_t object, adds it to this
 * MultiSpeciesType_t object and returns the SpeciesTypeInstance_t object
 * created.
 *
 * @param mst the MultiSpeciesType_t structure to which the
 * SpeciesTypeInstance_t should be added.
 *
 * @return a new SpeciesTypeInstance_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t*
MultiSpeciesType_createSpeciesTypeInstance(MultiSpeciesType_t* mst);


/**
 * Removes the nth SpeciesTypeInstance_t from this MultiSpeciesType_t and
 * returns a pointer to it.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param n an unsigned int representing the index of the SpeciesTypeInstance_t
 * to remove.
 *
 * @return a pointer to the nth SpeciesTypeInstance_t in this
 * MultiSpeciesType_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t*
MultiSpeciesType_removeSpeciesTypeInstance(MultiSpeciesType_t* mst,
                                           unsigned int n);


/**
 * Removes the SpeciesTypeInstance_t from this MultiSpeciesType_t based on its
 * identifier and returns a pointer to it.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param sid a string representing the identifier of the SpeciesTypeInstance_t
 * to remove.
 *
 * @return the SpeciesTypeInstance_t in this MultiSpeciesType_t based on the
 * identifier or NULL if no such SpeciesTypeInstance_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t*
MultiSpeciesType_removeSpeciesTypeInstanceById(MultiSpeciesType_t* mst,
                                               const char* sid);


/**
 * Returns a ListOf_t * containing SpeciesTypeComponentIndex_t objects from
 * this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure whose
 * ListOfSpeciesTypeComponentIndexes is sought.
 *
 * @return the ListOfSpeciesTypeComponentIndexes from this MultiSpeciesType_t
 * as a ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see MultiSpeciesType_addSpeciesTypeComponentIndex()
 * @see MultiSpeciesType_createSpeciesTypeComponentIndex()
 * @see MultiSpeciesType_getSpeciesTypeComponentIndexById()
 * @see MultiSpeciesType_getSpeciesTypeComponentIndex()
 * @see MultiSpeciesType_getNumSpeciesTypeComponentIndexes()
 * @see MultiSpeciesType_removeSpeciesTypeComponentIndexById()
 * @see MultiSpeciesType_removeSpeciesTypeComponentIndex()
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesType_getListOfSpeciesTypeComponentIndexes(MultiSpeciesType_t* mst);


/**
 * Get a SpeciesTypeComponentIndex_t from the MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * SpeciesTypeComponentIndex_t to retrieve.
 *
 * @return the nth SpeciesTypeComponentIndex_t in the
 * ListOfSpeciesTypeComponentIndexes within this MultiSpeciesType.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t*
MultiSpeciesType_getSpeciesTypeComponentIndex(MultiSpeciesType_t* mst,
                                              unsigned int n);


/**
 * Get a SpeciesTypeComponentIndex_t from the MultiSpeciesType_t based on its
 * identifier.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * SpeciesTypeComponentIndex_t to retrieve.
 *
 * @return the SpeciesTypeComponentIndex_t in the
 * ListOfSpeciesTypeComponentIndexes within this MultiSpeciesType with the
 * given @p sid or @c NULL if no such SpeciesTypeComponentIndex_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t*
MultiSpeciesType_getSpeciesTypeComponentIndexById(MultiSpeciesType_t* mst,
                                                  const char *sid);


/**
 * Adds a copy of the given SpeciesTypeComponentIndex_t to this
 * MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to which the
 * SpeciesTypeComponentIndex_t should be added.
 *
 * @param stci the SpeciesTypeComponentIndex_t object to add.
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
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_addSpeciesTypeComponentIndex(MultiSpeciesType_t* mst,
                                              const
                                                SpeciesTypeComponentIndex_t*
                                                  stci);


/**
 * Get the number of SpeciesTypeComponentIndex_t objects in this
 * MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to query.
 *
 * @return the number of SpeciesTypeComponentIndex_t objects in this
 * MultiSpeciesType_t.
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumSpeciesTypeComponentIndexes(MultiSpeciesType_t* mst);


/**
 * Creates a new SpeciesTypeComponentIndex_t object, adds it to this
 * MultiSpeciesType_t object and returns the SpeciesTypeComponentIndex_t object
 * created.
 *
 * @param mst the MultiSpeciesType_t structure to which the
 * SpeciesTypeComponentIndex_t should be added.
 *
 * @return a new SpeciesTypeComponentIndex_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t*
MultiSpeciesType_createSpeciesTypeComponentIndex(MultiSpeciesType_t* mst);


/**
 * Removes the nth SpeciesTypeComponentIndex_t from this MultiSpeciesType_t and
 * returns a pointer to it.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param n an unsigned int representing the index of the
 * SpeciesTypeComponentIndex_t to remove.
 *
 * @return a pointer to the nth SpeciesTypeComponentIndex_t in this
 * MultiSpeciesType_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t*
MultiSpeciesType_removeSpeciesTypeComponentIndex(MultiSpeciesType_t* mst,
                                                 unsigned int n);


/**
 * Removes the SpeciesTypeComponentIndex_t from this MultiSpeciesType_t based
 * on its identifier and returns a pointer to it.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param sid a string representing the identifier of the
 * SpeciesTypeComponentIndex_t to remove.
 *
 * @return the SpeciesTypeComponentIndex_t in this MultiSpeciesType_t based on
 * the identifier or NULL if no such SpeciesTypeComponentIndex_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t*
MultiSpeciesType_removeSpeciesTypeComponentIndexById(MultiSpeciesType_t* mst,
                                                     const char* sid);


/**
 * Returns a ListOf_t * containing InSpeciesTypeBond_t objects from this
 * MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure whose ListOfInSpeciesTypeBonds
 * is sought.
 *
 * @return the ListOfInSpeciesTypeBonds from this MultiSpeciesType_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see MultiSpeciesType_addInSpeciesTypeBond()
 * @see MultiSpeciesType_createInSpeciesTypeBond()
 * @see MultiSpeciesType_getInSpeciesTypeBondById()
 * @see MultiSpeciesType_getInSpeciesTypeBond()
 * @see MultiSpeciesType_getNumInSpeciesTypeBonds()
 * @see MultiSpeciesType_removeInSpeciesTypeBondById()
 * @see MultiSpeciesType_removeInSpeciesTypeBond()
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesType_getListOfInSpeciesTypeBonds(MultiSpeciesType_t* mst);


/**
 * Get an InSpeciesTypeBond_t from the MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param n an unsigned int representing the index of the InSpeciesTypeBond_t
 * to retrieve.
 *
 * @return the nth InSpeciesTypeBond_t in the ListOfInSpeciesTypeBonds within
 * this MultiSpeciesType.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t*
MultiSpeciesType_getInSpeciesTypeBond(MultiSpeciesType_t* mst,
                                      unsigned int n);


/**
 * Get an InSpeciesTypeBond_t from the MultiSpeciesType_t based on its
 * identifier.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param sid a string representing the identifier of the InSpeciesTypeBond_t
 * to retrieve.
 *
 * @return the InSpeciesTypeBond_t in the ListOfInSpeciesTypeBonds within this
 * MultiSpeciesType with the given @p sid or @c NULL if no such
 * InSpeciesTypeBond_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t*
MultiSpeciesType_getInSpeciesTypeBondById(MultiSpeciesType_t* mst,
                                          const char *sid);


/**
 * Adds a copy of the given InSpeciesTypeBond_t to this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to which the InSpeciesTypeBond_t
 * should be added.
 *
 * @param istb the InSpeciesTypeBond_t object to add.
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
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_addInSpeciesTypeBond(MultiSpeciesType_t* mst,
                                      const InSpeciesTypeBond_t* istb);


/**
 * Get the number of InSpeciesTypeBond_t objects in this MultiSpeciesType_t.
 *
 * @param mst the MultiSpeciesType_t structure to query.
 *
 * @return the number of InSpeciesTypeBond_t objects in this
 * MultiSpeciesType_t.
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumInSpeciesTypeBonds(MultiSpeciesType_t* mst);


/**
 * Creates a new InSpeciesTypeBond_t object, adds it to this MultiSpeciesType_t
 * object and returns the InSpeciesTypeBond_t object created.
 *
 * @param mst the MultiSpeciesType_t structure to which the InSpeciesTypeBond_t
 * should be added.
 *
 * @return a new InSpeciesTypeBond_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t*
MultiSpeciesType_createInSpeciesTypeBond(MultiSpeciesType_t* mst);


/**
 * Removes the nth InSpeciesTypeBond_t from this MultiSpeciesType_t and returns
 * a pointer to it.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param n an unsigned int representing the index of the InSpeciesTypeBond_t
 * to remove.
 *
 * @return a pointer to the nth InSpeciesTypeBond_t in this MultiSpeciesType_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t*
MultiSpeciesType_removeInSpeciesTypeBond(MultiSpeciesType_t* mst,
                                         unsigned int n);


/**
 * Removes the InSpeciesTypeBond_t from this MultiSpeciesType_t based on its
 * identifier and returns a pointer to it.
 *
 * @param mst the MultiSpeciesType_t structure to search.
 *
 * @param sid a string representing the identifier of the InSpeciesTypeBond_t
 * to remove.
 *
 * @return the InSpeciesTypeBond_t in this MultiSpeciesType_t based on the
 * identifier or NULL if no such InSpeciesTypeBond_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t*
MultiSpeciesType_removeInSpeciesTypeBondById(MultiSpeciesType_t* mst,
                                             const char* sid);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * MultiSpeciesType_t object have been set.
 *
 * @param mst the MultiSpeciesType_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * MultiSpeciesType_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the MultiSpeciesType_t object are:
 * @li "id"
 *
 * @memberof MultiSpeciesType_t
 */
LIBSBML_EXTERN
int
MultiSpeciesType_hasRequiredAttributes(MultiSpeciesType_t * mst);


/**
 * Return the structure indicated by the given @p sid.
 *
 * @param lo the ListOf_t structure to use.
 *
 * @param sid a string matching the "id" attribute of the element sought.
 *
 * @return the structure for the given variable, or @c NULL if no such
 * object exists in the list.
 *
 * @memberof ListOfMultiSpeciesTypes_t
 */
LIBSBML_EXTERN
MultiSpeciesType_t *
ListOfMultiSpeciesTypes_getById(ListOf_t * lo, const char * sid);


/**
 * Removes the structure with the given @p sid
 * from the given list and returns a pointer to it.
 *
 * The caller owns the returned structure and is responsible for deleting it.
 *
 * @param lo the ListOf_t structure.
 * @param sid a string matching the "id" attribute of the element sought.
 *
 * @return the structure removed.  As mentioned above, the
 * caller owns the returned structure. @c NULL is returned if no
 * structure with the "id" attribute exists in the given list.
 *
 * @memberof ListOfMultiSpeciesTypes_t
 */
LIBSBML_EXTERN
MultiSpeciesType_t *
ListOfMultiSpeciesTypes_removeById(ListOf_t * lo, const char * sid);




END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  MultiSpeciesType_H__  */

