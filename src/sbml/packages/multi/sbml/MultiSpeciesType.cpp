/**
 * @file:   MultiSpeciesType.cpp
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
 */


#include <sbml/packages/multi/sbml/MultiSpeciesType.h>
#include <sbml/packages/multi/sbml/BindingSiteSpeciesType.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>

#include <sbml/util/ElementFilter.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new MultiSpeciesType with the given level, version, and package version.
 */
MultiSpeciesType::MultiSpeciesType (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
////   ,mId ("")
////   ,mName ("")
   ,mCompartment ("")
   ,mListOfSpeciesFeatureTypes (level, version, pkgVersion)
   ,mListOfSpeciesTypeInstances (level, version, pkgVersion)
   ,mListOfSpeciesTypeComponentIndexes (level, version, pkgVersion)
   ,mListOfInSpeciesTypeBonds (level, version, pkgVersion)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new MultiSpeciesType with the given MultiPkgNamespaces object.
 */
MultiSpeciesType::MultiSpeciesType (MultiPkgNamespaces* multins)
  : SBase(multins)
////   ,mId ("")
////   ,mName ("")
   ,mCompartment ("")
   ,mListOfSpeciesFeatureTypes (multins)
   ,mListOfSpeciesTypeInstances (multins)
   ,mListOfSpeciesTypeComponentIndexes (multins)
   ,mListOfInSpeciesTypeBonds (multins)
{
  // set the element namespace of this object
  setElementNamespace(multins->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(multins);
}


/*
 * Copy constructor for MultiSpeciesType.
 */
MultiSpeciesType::MultiSpeciesType (const MultiSpeciesType& orig)
  : SBase(orig)
//  , mId  ( orig.mId)
//  , mName  ( orig.mName)
  , mCompartment  ( orig.mCompartment)
  ,mListOfSpeciesFeatureTypes (orig.mListOfSpeciesFeatureTypes)
  ,mListOfSpeciesTypeInstances (orig.mListOfSpeciesTypeInstances)
   ,mListOfSpeciesTypeComponentIndexes (orig.mListOfSpeciesTypeComponentIndexes)
   ,mListOfInSpeciesTypeBonds (orig.mListOfInSpeciesTypeBonds)
{
  // connect to child objects
  connectToChild();
}


/*
 * Assignment for MultiSpeciesType.
 */
MultiSpeciesType&
MultiSpeciesType::operator=(const MultiSpeciesType& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    mCompartment  = rhs.mCompartment;
    mListOfSpeciesFeatureTypes  = rhs.mListOfSpeciesFeatureTypes;
    mListOfSpeciesTypeInstances  = rhs.mListOfSpeciesTypeInstances;
    mListOfSpeciesTypeComponentIndexes  = rhs.mListOfSpeciesTypeComponentIndexes;
    mListOfInSpeciesTypeBonds  = rhs.mListOfInSpeciesTypeBonds;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for MultiSpeciesType.
 */
MultiSpeciesType*
MultiSpeciesType::clone () const
{
  return new MultiSpeciesType(*this);
}


/*
 * Destructor for MultiSpeciesType.
 */
MultiSpeciesType::~MultiSpeciesType ()
{
}


/*
 * Returns the value of the "id" attribute of this MultiSpeciesType.
 */
const std::string&
MultiSpeciesType::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this MultiSpeciesType.
 */
const std::string&
MultiSpeciesType::getName() const
{
  return mName;
}




/*
 * Returns the value of the "compartment" attribute of this MultiSpeciesType.
 */
const std::string&
MultiSpeciesType::getCompartment() const
{
  return mCompartment;
}


/*
 * Returns true/false if id is set.
 */
bool
MultiSpeciesType::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
MultiSpeciesType::isSetName() const
{
  return (mName.empty() == false);
}




/*
 * Returns true/false if compartment is set.
 */
bool
MultiSpeciesType::isSetCompartment() const
{
  return (mCompartment.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
MultiSpeciesType::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
MultiSpeciesType::setName(const std::string& name)
{
  mName = name;
  return LIBSBML_OPERATION_SUCCESS;
}




/*
 * Sets compartment and returns value indicating success.
 */
int
MultiSpeciesType::setCompartment(const std::string& compartment)
{
  if (!(SyntaxChecker::isValidInternalSId(compartment)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCompartment = compartment;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
MultiSpeciesType::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets name and returns value indicating success.
 */
int
MultiSpeciesType::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}




/*
 * Unsets compartment and returns value indicating success.
 */
int
MultiSpeciesType::unsetCompartment()
{
  mCompartment.erase();

  if (mCompartment.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the  "ListOfSpeciesFeatureTypes" in this MultiSpeciesType object.
 */
const ListOfSpeciesFeatureTypes*
MultiSpeciesType::getListOfSpeciesFeatureTypes() const
{
  return &mListOfSpeciesFeatureTypes;
}


/*
 * Returns the  "ListOfSpeciesFeatureTypes" in this MultiSpeciesType object.
 */
ListOfSpeciesFeatureTypes*
MultiSpeciesType::getListOfSpeciesFeatureTypes()
{
  return &mListOfSpeciesFeatureTypes;
}


/*
 * Removes the nth SpeciesFeatureType from the ListOfSpeciesFeatureTypes.
 */
SpeciesFeatureType*
MultiSpeciesType::removeSpeciesFeatureType(unsigned int n)
{
  return mListOfSpeciesFeatureTypes.remove(n);
}


/*
 * Removes the a SpeciesFeatureType with given id from the ListOfSpeciesFeatureTypes.
 */
SpeciesFeatureType*
MultiSpeciesType::removeSpeciesFeatureType(const std::string& sid)
{
  return mListOfSpeciesFeatureTypes.remove(sid);
}


/*
 * Return the nth SpeciesFeatureType in the ListOfSpeciesFeatureTypes within this MultiSpeciesType.
 */
SpeciesFeatureType*
MultiSpeciesType::getSpeciesFeatureType(unsigned int n)
{
  return mListOfSpeciesFeatureTypes.get(n);
}


/*
 * Return the nth SpeciesFeatureType in the ListOfSpeciesFeatureTypes within this MultiSpeciesType.
 */
const SpeciesFeatureType*
MultiSpeciesType::getSpeciesFeatureType(unsigned int n) const
{
  return mListOfSpeciesFeatureTypes.get(n);
}


/*
 * Return a SpeciesFeatureType from the ListOfSpeciesFeatureTypes by id.
 */
SpeciesFeatureType*
MultiSpeciesType::getSpeciesFeatureType(const std::string& sid)
{
  return mListOfSpeciesFeatureTypes.get(sid);
}


/*
 * Return a SpeciesFeatureType from the ListOfSpeciesFeatureTypes by id.
 */
const SpeciesFeatureType*
MultiSpeciesType::getSpeciesFeatureType(const std::string& sid) const
{
  return mListOfSpeciesFeatureTypes.get(sid);
}


/*
 * Adds a copy the given "SpeciesFeatureType" to this MultiSpeciesType.
 */
int
MultiSpeciesType::addSpeciesFeatureType(const SpeciesFeatureType* sft)
{
  if (sft == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (sft->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sft->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sft->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(sft)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mListOfSpeciesFeatureTypes.append(sft);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of SpeciesFeatureType objects in this MultiSpeciesType.
 */
unsigned int
MultiSpeciesType::getNumSpeciesFeatureTypes() const
{
  return mListOfSpeciesFeatureTypes.size();
}


/*
 * Creates a new SpeciesFeatureType object, adds it to this MultiSpeciesTypes
 */
SpeciesFeatureType*
MultiSpeciesType::createSpeciesFeatureType()
{
  SpeciesFeatureType* sft = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    sft = new SpeciesFeatureType(multins);
    delete multins;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(sft != NULL)
  {
    mListOfSpeciesFeatureTypes.appendAndOwn(sft);
  }

  return sft;
}


/*
 * Returns the  "ListOfSpeciesTypeInstances" in this MultiSpeciesType object.
 */
const ListOfSpeciesTypeInstances*
MultiSpeciesType::getListOfSpeciesTypeInstances() const
{
  return &mListOfSpeciesTypeInstances;
}


/*
 * Returns the  "ListOfSpeciesTypeInstances" in this MultiSpeciesType object.
 */
ListOfSpeciesTypeInstances*
MultiSpeciesType::getListOfSpeciesTypeInstances()
{
  return &mListOfSpeciesTypeInstances;
}


/*
 * Removes the nth SpeciesTypeInstance from the ListOfSpeciesTypeInstances.
 */
SpeciesTypeInstance*
MultiSpeciesType::removeSpeciesTypeInstance(unsigned int n)
{
  return mListOfSpeciesTypeInstances.remove(n);
}


/*
 * Removes the a SpeciesTypeInstance with given id from the ListOfSpeciesTypeInstances.
 */
SpeciesTypeInstance*
MultiSpeciesType::removeSpeciesTypeInstance(const std::string& sid)
{
  return mListOfSpeciesTypeInstances.remove(sid);
}


/*
 * Return the nth SpeciesTypeInstance in the ListOfSpeciesTypeInstances within this MultiSpeciesType.
 */
SpeciesTypeInstance*
MultiSpeciesType::getSpeciesTypeInstance(unsigned int n)
{
  return mListOfSpeciesTypeInstances.get(n);
}


/*
 * Return the nth SpeciesTypeInstance in the ListOfSpeciesTypeInstances within this MultiSpeciesType.
 */
const SpeciesTypeInstance*
MultiSpeciesType::getSpeciesTypeInstance(unsigned int n) const
{
  return mListOfSpeciesTypeInstances.get(n);
}


/*
 * Return a SpeciesTypeInstance from the ListOfSpeciesTypeInstances by id.
 */
SpeciesTypeInstance*
MultiSpeciesType::getSpeciesTypeInstance(const std::string& sid)
{
  return mListOfSpeciesTypeInstances.get(sid);
}


/*
 * Return a SpeciesTypeInstance from the ListOfSpeciesTypeInstances by id.
 */
const SpeciesTypeInstance*
MultiSpeciesType::getSpeciesTypeInstance(const std::string& sid) const
{
  return mListOfSpeciesTypeInstances.get(sid);
}


/*
 * Adds a copy the given "SpeciesTypeInstance" to this MultiSpeciesType.
 */
int
MultiSpeciesType::addSpeciesTypeInstance(const SpeciesTypeInstance* sti)
{
  if (sti == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (sti->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sti->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sti->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(sti)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mListOfSpeciesTypeInstances.append(sti);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of SpeciesTypeInstance objects in this MultiSpeciesType.
 */
unsigned int
MultiSpeciesType::getNumSpeciesTypeInstances() const
{
  return mListOfSpeciesTypeInstances.size();
}


/*
 * Creates a new SpeciesTypeInstance object, adds it to this MultiSpeciesTypes
 */
SpeciesTypeInstance*
MultiSpeciesType::createSpeciesTypeInstance()
{
  SpeciesTypeInstance* sti = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    sti = new SpeciesTypeInstance(multins);
    delete multins;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(sti != NULL)
  {
    mListOfSpeciesTypeInstances.appendAndOwn(sti);
  }

  return sti;
}


/*
 * Returns the  "ListOfSpeciesTypeComponentIndexes" in this MultiSpeciesType object.
 */
const ListOfSpeciesTypeComponentIndexes*
MultiSpeciesType::getListOfSpeciesTypeComponentIndexes() const
{
  return &mListOfSpeciesTypeComponentIndexes;
}


/*
 * Returns the  "ListOfSpeciesTypeComponentIndexes" in this MultiSpeciesType object.
 */
ListOfSpeciesTypeComponentIndexes*
MultiSpeciesType::getListOfSpeciesTypeComponentIndexes()
{
  return &mListOfSpeciesTypeComponentIndexes;
}


/*
 * Removes the nth SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes.
 */
SpeciesTypeComponentIndex*
MultiSpeciesType::removeSpeciesTypeComponentIndex(unsigned int n)
{
  return mListOfSpeciesTypeComponentIndexes.remove(n);
}


/*
 * Removes the a SpeciesTypeComponentIndex with given id from the ListOfSpeciesTypeComponentIndexes.
 */
SpeciesTypeComponentIndex*
MultiSpeciesType::removeSpeciesTypeComponentIndex(const std::string& sid)
{
  return mListOfSpeciesTypeComponentIndexes.remove(sid);
}


/*
 * Return the nth SpeciesTypeComponentIndex in the ListOfSpeciesTypeComponentIndexes within this MultiSpeciesType.
 */
SpeciesTypeComponentIndex*
MultiSpeciesType::getSpeciesTypeComponentIndex(unsigned int n)
{
  return mListOfSpeciesTypeComponentIndexes.get(n);
}


/*
 * Return the nth SpeciesTypeComponentIndex in the ListOfSpeciesTypeComponentIndexes within this MultiSpeciesType.
 */
const SpeciesTypeComponentIndex*
MultiSpeciesType::getSpeciesTypeComponentIndex(unsigned int n) const
{
  return mListOfSpeciesTypeComponentIndexes.get(n);
}


/*
 * Return a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes by id.
 */
SpeciesTypeComponentIndex*
MultiSpeciesType::getSpeciesTypeComponentIndex(const std::string& sid)
{
  return mListOfSpeciesTypeComponentIndexes.get(sid);
}


/*
 * Return a SpeciesTypeComponentIndex from the ListOfSpeciesTypeComponentIndexes by id.
 */
const SpeciesTypeComponentIndex*
MultiSpeciesType::getSpeciesTypeComponentIndex(const std::string& sid) const
{
  return mListOfSpeciesTypeComponentIndexes.get(sid);
}


/*
 * Adds a copy the given "SpeciesTypeComponentIndex" to this MultiSpeciesType.
 */
int
MultiSpeciesType::addSpeciesTypeComponentIndex(const SpeciesTypeComponentIndex* stci)
{
  if (stci == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (stci->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != stci->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != stci->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(stci)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mListOfSpeciesTypeComponentIndexes.append(stci);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of SpeciesTypeComponentIndex objects in this MultiSpeciesType.
 */
unsigned int
MultiSpeciesType::getNumSpeciesTypeComponentIndexes() const
{
  return mListOfSpeciesTypeComponentIndexes.size();
}


/*
 * Creates a new SpeciesTypeComponentIndex object, adds it to this MultiSpeciesTypes
 */
SpeciesTypeComponentIndex*
MultiSpeciesType::createSpeciesTypeComponentIndex()
{
  SpeciesTypeComponentIndex* stci = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    stci = new SpeciesTypeComponentIndex(multins);
    delete multins;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(stci != NULL)
  {
    mListOfSpeciesTypeComponentIndexes.appendAndOwn(stci);
  }

  return stci;
}


/*
 * Returns the  "ListOfInSpeciesTypeBonds" in this MultiSpeciesType object.
 */
const ListOfInSpeciesTypeBonds*
MultiSpeciesType::getListOfInSpeciesTypeBonds() const
{
  return &mListOfInSpeciesTypeBonds;
}


/*
 * Returns the  "ListOfInSpeciesTypeBonds" in this MultiSpeciesType object.
 */
ListOfInSpeciesTypeBonds*
MultiSpeciesType::getListOfInSpeciesTypeBonds()
{
  return &mListOfInSpeciesTypeBonds;
}


/*
 * Removes the nth InSpeciesTypeBond from the ListOfInSpeciesTypeBonds.
 */
InSpeciesTypeBond*
MultiSpeciesType::removeInSpeciesTypeBond(unsigned int n)
{
  return mListOfInSpeciesTypeBonds.remove(n);
}


/*
 * Removes the a InSpeciesTypeBond with given id from the ListOfInSpeciesTypeBonds.
 */
InSpeciesTypeBond*
MultiSpeciesType::removeInSpeciesTypeBond(const std::string& sid)
{
  return mListOfInSpeciesTypeBonds.remove(sid);
}


/*
 * Return the nth InSpeciesTypeBond in the ListOfInSpeciesTypeBonds within this MultiSpeciesType.
 */
InSpeciesTypeBond*
MultiSpeciesType::getInSpeciesTypeBond(unsigned int n)
{
  return mListOfInSpeciesTypeBonds.get(n);
}


/*
 * Return the nth InSpeciesTypeBond in the ListOfInSpeciesTypeBonds within this MultiSpeciesType.
 */
const InSpeciesTypeBond*
MultiSpeciesType::getInSpeciesTypeBond(unsigned int n) const
{
  return mListOfInSpeciesTypeBonds.get(n);
}


/*
 * Return a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds by id.
 */
InSpeciesTypeBond*
MultiSpeciesType::getInSpeciesTypeBond(const std::string& sid)
{
  return mListOfInSpeciesTypeBonds.get(sid);
}


/*
 * Return a InSpeciesTypeBond from the ListOfInSpeciesTypeBonds by id.
 */
const InSpeciesTypeBond*
MultiSpeciesType::getInSpeciesTypeBond(const std::string& sid) const
{
  return mListOfInSpeciesTypeBonds.get(sid);
}


/*
 * Adds a copy the given "InSpeciesTypeBond" to this MultiSpeciesType.
 */
int
MultiSpeciesType::addInSpeciesTypeBond(const InSpeciesTypeBond* istb)
{
  if (istb == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (istb->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != istb->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != istb->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(istb)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mListOfInSpeciesTypeBonds.append(istb);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of InSpeciesTypeBond objects in this MultiSpeciesType.
 */
unsigned int
MultiSpeciesType::getNumInSpeciesTypeBonds() const
{
  return mListOfInSpeciesTypeBonds.size();
}


/*
 * Creates a new InSpeciesTypeBond object, adds it to this MultiSpeciesTypes
 */
InSpeciesTypeBond*
MultiSpeciesType::createInSpeciesTypeBond()
{
  InSpeciesTypeBond* istb = NULL;

  try
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    istb = new InSpeciesTypeBond(multins);
    delete multins;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(istb != NULL)
  {
    mListOfInSpeciesTypeBonds.appendAndOwn(istb);
  }

  return istb;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
MultiSpeciesType::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetCompartment() == true && mCompartment == oldid)
  {
    setCompartment(newid);
  }

}


List*
MultiSpeciesType::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mListOfSpeciesFeatureTypes, filter);
  ADD_FILTERED_LIST(ret, sublist, mListOfSpeciesTypeInstances, filter);
  ADD_FILTERED_LIST(ret, sublist, mListOfSpeciesTypeComponentIndexes, filter);
  ADD_FILTERED_LIST(ret, sublist, mListOfInSpeciesTypeBonds, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
MultiSpeciesType::getElementName () const
{
  static const string name = "speciesType";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
MultiSpeciesType::getTypeCode () const
{
  return SBML_MULTI_SPECIES_TYPE;
}


/*
 * check if all the required attributes are set
 */
bool
MultiSpeciesType::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;


  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
MultiSpeciesType::hasRequiredElements () const
{
  bool allPresent = true;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
MultiSpeciesType::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (getNumSpeciesFeatureTypes() > 0)
  {
    mListOfSpeciesFeatureTypes.write(stream);
  }

  if (getNumSpeciesTypeInstances() > 0)
  {
    mListOfSpeciesTypeInstances.write(stream);
  }

  if (getNumSpeciesTypeComponentIndexes() > 0)
  {
    mListOfSpeciesTypeComponentIndexes.write(stream);
  }

  if (getNumInSpeciesTypeBonds() > 0)
  {
    mListOfInSpeciesTypeBonds.write(stream);
  }

  SBase::writeExtensionElements(stream);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
MultiSpeciesType::accept (SBMLVisitor& v) const
{
  v.visit(*this);

  // SpeciesFeatureType
  for(unsigned int i = 0; i < getNumSpeciesFeatureTypes(); i++)
  {
    getSpeciesFeatureType(i)->accept(v);
  }

  // SpeciesTypeInstance
  for(unsigned int i = 0; i < getNumSpeciesTypeInstances(); i++)
  {
    getSpeciesTypeInstance(i)->accept(v);
  }

  // SpeciesTypeComponentIndex
  for(unsigned int i = 0; i < getNumSpeciesTypeComponentIndexes(); i++)
  {
    getSpeciesTypeComponentIndex(i)->accept(v);
  }

  // InSpeciesTypeBond
  for(unsigned int i = 0; i < getNumInSpeciesTypeBonds(); i++)
  {
    getInSpeciesTypeBond(i)->accept(v);
  }

  return true;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
MultiSpeciesType::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  mListOfSpeciesFeatureTypes.setSBMLDocument(d);
  mListOfSpeciesTypeInstances.setSBMLDocument(d);
  mListOfSpeciesTypeComponentIndexes.setSBMLDocument(d);
  mListOfInSpeciesTypeBonds.setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
MultiSpeciesType::connectToChild()
{
  mListOfSpeciesFeatureTypes.connectToParent(this);
  mListOfSpeciesTypeInstances.connectToParent(this);
  mListOfSpeciesTypeComponentIndexes.connectToParent(this);
  mListOfInSpeciesTypeBonds.connectToParent(this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
MultiSpeciesType::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mListOfSpeciesFeatureTypes.enablePackageInternal(pkgURI, pkgPrefix, flag);
  mListOfSpeciesTypeInstances.enablePackageInternal(pkgURI, pkgPrefix, flag);
  mListOfSpeciesTypeComponentIndexes.enablePackageInternal(pkgURI, pkgPrefix, flag);
  mListOfInSpeciesTypeBonds.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
MultiSpeciesType::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  const std::string& name = stream.peek().getName();
  const XMLNamespaces& xmlns = stream.peek().getNamespaces();
  std::string prefix(stream.peek().getPrefix());

  const std::string& targetPrefix =
      (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : getPrefix();

  if (prefix == targetPrefix)
    {
      MULTI_CREATE_NS(multins, getSBMLNamespaces());
      if (!targetPrefix.empty()) {
          prefix += ":";
      }

      if (name == "listOfSpeciesFeatureTypes")
        {
          if (mListOfSpeciesFeatureTypes.size() > 0)
            {
              getErrorLog()->logPackageError("multi", MultiLofSpeFtrTyps_onlyOne,
                  getPackageVersion(), getLevel(), getVersion(),
                  "<" + prefix + "speciesType> may only have one <" + prefix
                      + "listOfSpeciesFeatureTypes>",
                      stream.peek().getLine(),
                      stream.peek().getColumn());
            }
          else {
              object = &mListOfSpeciesFeatureTypes;

              if (targetPrefix.empty() == true)
                {
                  mListOfSpeciesFeatureTypes.getSBMLDocument()->enableDefaultNS(mURI,
                      true);
                }
          }
        }
      else if (name == "listOfSpeciesTypeInstances")
        {
          if (mListOfSpeciesTypeInstances.size() > 0)
            {
              getErrorLog()->logPackageError("multi", MultiLofSptInss_onlyOne,
                  getPackageVersion(), getLevel(), getVersion(),
                  "<" + prefix + "speciesType> may only have one <" + prefix
                      + "listOfSpeciesTypeInstances>",
                      stream.peek().getLine(),
                      stream.peek().getColumn());
            }
          else {
              object = &mListOfSpeciesTypeInstances;

              if (targetPrefix.empty() == true)
                {
                  mListOfSpeciesTypeInstances.getSBMLDocument()->enableDefaultNS(mURI,
                      true);
                }
          }
        }
      else if (name == "listOfSpeciesTypeComponentIndexes")
        {
          if (mListOfSpeciesTypeComponentIndexes.size() > 0)
            {
              getErrorLog()->logPackageError("multi", MultiLofSptCpoInds_onlyOne,
                  getPackageVersion(), getLevel(), getVersion(),
                  "<" + prefix + "speciesType> may only have one <" + prefix
                      + "listOfSpeciesTypeComponentIndexes>",
                      stream.peek().getLine(),
                      stream.peek().getColumn());
            }
          else {
              object = &mListOfSpeciesTypeComponentIndexes;

              if (targetPrefix.empty() == true)
                {
                  mListOfSpeciesTypeComponentIndexes.getSBMLDocument()->enableDefaultNS(mURI,
                      true);
                }
          }
        }
      else if (name == "listOfInSpeciesTypeBonds")
        {
          if (mListOfInSpeciesTypeBonds.size() > 0)
            {
              getErrorLog()->logPackageError("multi", MultiLofInSptBnds_onlyOne,
                  getPackageVersion(), getLevel(), getVersion(),
                  "<" + prefix + "speciesType> may only have one <" + prefix
                      + "listOfInSpeciesTypeBonds>",
                      stream.peek().getLine(),
                      stream.peek().getColumn());
            }
          else {
              object = &mListOfInSpeciesTypeBonds;

              if (targetPrefix.empty() == true)
                {
                  mListOfInSpeciesTypeBonds.getSBMLDocument()->enableDefaultNS(mURI,
                      true);
                }
          }
          object = &mListOfInSpeciesTypeBonds;
        }

      delete multins;
    }

  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
MultiSpeciesType::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
  attributes.add("compartment");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
MultiSpeciesType::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfMultiSpeciesTypes - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfMultiSpeciesTypes*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("multi", MultiLofStps_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiLofStps_AllowedAtts,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details,
                  getLine(), getColumn());
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("multi", MultiSpt_AllowedMultiAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, 
                       getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("multi", MultiSpt_AllowedCoreAtts,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, 
                       getLine(), getColumn());
      }
    }
  }

  bool assigned = false;

  //
  // id SId  ( use = "required" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<MultiSpeciesType>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
        std::string details = "The syntax of the attribute id='" + mId + "' does not conform.";
        getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                   getPackageVersion(), sbmlLevel, sbmlVersion, details,
                   getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Multi attribute 'id' is missing.";
    getErrorLog()->logPackageError("multi", MultiSpt_AllowedMultiAtts,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // name string   ( use = "optional" )
  //
  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    // check string is not empty

    if (mName.empty() == true)
    {
      logEmptyString(mName, getLevel(), getVersion(), "<MultiSpeciesType>");
    }
  }

  //
  // compartment SIdRef   ( use = "optional" )
  //
  assigned = attributes.readInto("compartment", mCompartment);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mCompartment.empty() == true)
    {
      logEmptyString(mCompartment, getLevel(), getVersion(), "<MultiSpeciesType>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mCompartment) == false && getErrorLog() != NULL)
    {
      std::string details = "The syntax of the attribute compartment='" + mCompartment + "' does not conform.";
      getErrorLog()->logPackageError("multi", MultiInvSIdSyn,
                 getPackageVersion(), sbmlLevel, sbmlVersion, details,
                 getLine(), getColumn());

    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
MultiSpeciesType::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);


  if (isSetCompartment() == true)
    stream.writeAttribute("compartment", getPrefix(), mCompartment);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfMultiSpeciesTypes::ListOfMultiSpeciesTypes(unsigned int level, 
                          unsigned int version, 
                          unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new MultiPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfMultiSpeciesTypes::ListOfMultiSpeciesTypes(MultiPkgNamespaces* multins)
  : ListOf(multins)
{
  setElementNamespace(multins->getURI());
}


/*
 * Returns a deep copy of this ListOfMultiSpeciesTypes 
 */
ListOfMultiSpeciesTypes* 
ListOfMultiSpeciesTypes::clone () const
 {
  return new ListOfMultiSpeciesTypes(*this);
}


/*
 * Get a MultiSpeciesType from the ListOfMultiSpeciesTypes by index.
 */
MultiSpeciesType*
ListOfMultiSpeciesTypes::get(unsigned int n)
{
  return static_cast<MultiSpeciesType*>(ListOf::get(n));
}


/*
 * Get a MultiSpeciesType from the ListOfMultiSpeciesTypes by index.
 */
const MultiSpeciesType*
ListOfMultiSpeciesTypes::get(unsigned int n) const
{
  return static_cast<const MultiSpeciesType*>(ListOf::get(n));
}


/*
 * Get a MultiSpeciesType from the ListOfMultiSpeciesTypes by id.
 */
MultiSpeciesType*
ListOfMultiSpeciesTypes::get(const std::string& sid)
{
  return const_cast<MultiSpeciesType*>(
    static_cast<const ListOfMultiSpeciesTypes&>(*this).get(sid));
}


/*
 * Get a MultiSpeciesType from the ListOfMultiSpeciesTypes by id.
 */
const MultiSpeciesType*
ListOfMultiSpeciesTypes::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<MultiSpeciesType>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <MultiSpeciesType*> (*result);
}


/*
 * Removes the nth MultiSpeciesType from this ListOfMultiSpeciesTypes
 */
MultiSpeciesType*
ListOfMultiSpeciesTypes::remove(unsigned int n)
{
  return static_cast<MultiSpeciesType*>(ListOf::remove(n));
}


/*
 * Removes the MultiSpeciesType from this ListOfMultiSpeciesTypes with the given identifier
 */
MultiSpeciesType*
ListOfMultiSpeciesTypes::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<MultiSpeciesType>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <MultiSpeciesType*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfMultiSpeciesTypes::getElementName () const
{
  static const string name = "listOfSpeciesTypes";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfMultiSpeciesTypes::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfMultiSpeciesTypes::getItemTypeCode () const
{
  return SBML_MULTI_SPECIES_TYPE;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new MultiSpeciesType in this ListOfMultiSpeciesTypes
 */
SBase*
ListOfMultiSpeciesTypes::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "speciesType")
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    object = new MultiSpeciesType(multins);
    appendAndOwn(object);
    delete multins;
  }
  else if (name == "bindingSiteSpeciesType")
  {
    MULTI_CREATE_NS(multins, getSBMLNamespaces());
    object = new BindingSiteSpeciesType(multins);
    appendAndOwn(object);
    delete multins;
  }

  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Multi package.
 */
void
ListOfMultiSpeciesTypes::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(MultiExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(MultiExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}

/*
 * Return true only when the item is an object of MultiSpeciesType or
 * BindingSiteSpeciesType
 */
bool
ListOfMultiSpeciesTypes::isValidTypeForList(SBase * item)
{
  return item->getTypeCode() == SBML_MULTI_SPECIES_TYPE ||
      item->getTypeCode() == SBML_MULTI_BINDING_SITE_SPECIES_TYPE;
}



  /** @endcond */


LIBSBML_EXTERN
MultiSpeciesType_t *
MultiSpeciesType_create(unsigned int level, unsigned int version,
                        unsigned int pkgVersion)
{
  return new MultiSpeciesType(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
MultiSpeciesType_free(MultiSpeciesType_t * mst)
{
  if (mst != NULL)
    delete mst;
}


LIBSBML_EXTERN
MultiSpeciesType_t *
MultiSpeciesType_clone(MultiSpeciesType_t * mst)
{
  if (mst != NULL)
  {
    return static_cast<MultiSpeciesType_t*>(mst->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
char *
MultiSpeciesType_getId(MultiSpeciesType_t * mst)
{
  if (mst == NULL)
    return NULL;

  return mst->getId().empty() ? NULL : safe_strdup(mst->getId().c_str());
}


LIBSBML_EXTERN
char *
MultiSpeciesType_getName(MultiSpeciesType_t * mst)
{
  if (mst == NULL)
    return NULL;

  return mst->getName().empty() ? NULL : safe_strdup(mst->getName().c_str());
}




LIBSBML_EXTERN
char *
MultiSpeciesType_getCompartment(MultiSpeciesType_t * mst)
{
  if (mst == NULL)
    return NULL;

  return mst->getCompartment().empty() ? NULL : safe_strdup(mst->getCompartment().c_str());
}


LIBSBML_EXTERN
int
MultiSpeciesType_isSetId(MultiSpeciesType_t * mst)
{
  return (mst != NULL) ? static_cast<int>(mst->isSetId()) : 0;
}


LIBSBML_EXTERN
int
MultiSpeciesType_isSetName(MultiSpeciesType_t * mst)
{
  return (mst != NULL) ? static_cast<int>(mst->isSetName()) : 0;
}




LIBSBML_EXTERN
int
MultiSpeciesType_isSetCompartment(MultiSpeciesType_t * mst)
{
  return (mst != NULL) ? static_cast<int>(mst->isSetCompartment()) : 0;
}


LIBSBML_EXTERN
int
MultiSpeciesType_setId(MultiSpeciesType_t * mst, const char * id)
{
  return (mst != NULL) ? mst->setId(id) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
MultiSpeciesType_setName(MultiSpeciesType_t * mst, const char * name)
{
  return (mst != NULL) ? mst->setName(name) : LIBSBML_INVALID_OBJECT;
}




LIBSBML_EXTERN
int
MultiSpeciesType_setCompartment(MultiSpeciesType_t * mst, const char * compartment)
{
  return (mst != NULL) ? mst->setCompartment(compartment) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
MultiSpeciesType_unsetId(MultiSpeciesType_t * mst)
{
  return (mst != NULL) ? mst->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
MultiSpeciesType_unsetName(MultiSpeciesType_t * mst)
{
  return (mst != NULL) ? mst->unsetName() : LIBSBML_INVALID_OBJECT;
}




LIBSBML_EXTERN
int
MultiSpeciesType_unsetCompartment(MultiSpeciesType_t * mst)
{
  return (mst != NULL) ? mst->unsetCompartment() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing SpeciesFeatureType_t objects from this
 * MultiSpeciesType_t.
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesType_getListOfSpeciesFeatureTypes(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->getListOfSpeciesFeatureTypes() : NULL;
}


/*
 * Get a SpeciesFeatureType_t from the MultiSpeciesType_t.
 */
LIBSBML_EXTERN
SpeciesFeatureType_t*
MultiSpeciesType_getSpeciesFeatureType(MultiSpeciesType_t* mst,
                                       unsigned int n)
{
  return (mst != NULL) ? mst->getSpeciesFeatureType(n) : NULL;
}


/*
 * Get a SpeciesFeatureType_t from the MultiSpeciesType_t based on its
 * identifier.
 */
LIBSBML_EXTERN
SpeciesFeatureType_t*
MultiSpeciesType_getSpeciesFeatureTypeById(MultiSpeciesType_t* mst,
                                           const char *sid)
{
  return (mst != NULL && sid != NULL) ? mst->getSpeciesFeatureType(sid) : NULL;
}


/*
 * Adds a copy of the given SpeciesFeatureType_t to this MultiSpeciesType_t.
 */
LIBSBML_EXTERN
int
MultiSpeciesType_addSpeciesFeatureType(MultiSpeciesType_t* mst,
                                       const SpeciesFeatureType_t* sft)
{
  return (mst != NULL) ? mst->addSpeciesFeatureType(sft) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of SpeciesFeatureType_t objects in this MultiSpeciesType_t.
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumSpeciesFeatureTypes(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->getNumSpeciesFeatureTypes() : SBML_INT_MAX;
}


/*
 * Creates a new SpeciesFeatureType_t object, adds it to this
 * MultiSpeciesType_t object and returns the SpeciesFeatureType_t object
 * created.
 */
LIBSBML_EXTERN
SpeciesFeatureType_t*
MultiSpeciesType_createSpeciesFeatureType(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->createSpeciesFeatureType() : NULL;
}


/*
 * Removes the nth SpeciesFeatureType_t from this MultiSpeciesType_t and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesFeatureType_t*
MultiSpeciesType_removeSpeciesFeatureType(MultiSpeciesType_t* mst,
                                          unsigned int n)
{
  return (mst != NULL) ? mst->removeSpeciesFeatureType(n) : NULL;
}


/*
 * Removes the SpeciesFeatureType_t from this MultiSpeciesType_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesFeatureType_t*
MultiSpeciesType_removeSpeciesFeatureTypeById(MultiSpeciesType_t* mst,
                                              const char* sid)
{
  return (mst != NULL && sid != NULL) ? mst->removeSpeciesFeatureType(sid) :
    NULL;
}


/*
 * Returns a ListOf_t * containing SpeciesTypeInstance_t objects from this
 * MultiSpeciesType_t.
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesType_getListOfSpeciesTypeInstances(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->getListOfSpeciesTypeInstances() : NULL;
}


/*
 * Get a SpeciesTypeInstance_t from the MultiSpeciesType_t.
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t*
MultiSpeciesType_getSpeciesTypeInstance(MultiSpeciesType_t* mst,
                                        unsigned int n)
{
  return (mst != NULL) ? mst->getSpeciesTypeInstance(n) : NULL;
}


/*
 * Get a SpeciesTypeInstance_t from the MultiSpeciesType_t based on its
 * identifier.
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t*
MultiSpeciesType_getSpeciesTypeInstanceById(MultiSpeciesType_t* mst,
                                            const char *sid)
{
  return (mst != NULL && sid != NULL) ? mst->getSpeciesTypeInstance(sid) :
    NULL;
}


/*
 * Adds a copy of the given SpeciesTypeInstance_t to this MultiSpeciesType_t.
 */
LIBSBML_EXTERN
int
MultiSpeciesType_addSpeciesTypeInstance(MultiSpeciesType_t* mst,
                                        const SpeciesTypeInstance_t* sti)
{
  return (mst != NULL) ? mst->addSpeciesTypeInstance(sti) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of SpeciesTypeInstance_t objects in this MultiSpeciesType_t.
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumSpeciesTypeInstances(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->getNumSpeciesTypeInstances() : SBML_INT_MAX;
}


/*
 * Creates a new SpeciesTypeInstance_t object, adds it to this
 * MultiSpeciesType_t object and returns the SpeciesTypeInstance_t object
 * created.
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t*
MultiSpeciesType_createSpeciesTypeInstance(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->createSpeciesTypeInstance() : NULL;
}


/*
 * Removes the nth SpeciesTypeInstance_t from this MultiSpeciesType_t and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t*
MultiSpeciesType_removeSpeciesTypeInstance(MultiSpeciesType_t* mst,
                                           unsigned int n)
{
  return (mst != NULL) ? mst->removeSpeciesTypeInstance(n) : NULL;
}


/*
 * Removes the SpeciesTypeInstance_t from this MultiSpeciesType_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesTypeInstance_t*
MultiSpeciesType_removeSpeciesTypeInstanceById(MultiSpeciesType_t* mst,
                                               const char* sid)
{
  return (mst != NULL && sid != NULL) ? mst->removeSpeciesTypeInstance(sid) :
    NULL;
}


/*
 * Returns a ListOf_t * containing SpeciesTypeComponentIndex_t objects from
 * this MultiSpeciesType_t.
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesType_getListOfSpeciesTypeComponentIndexes(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->getListOfSpeciesTypeComponentIndexes() : NULL;
}


/*
 * Get a SpeciesTypeComponentIndex_t from the MultiSpeciesType_t.
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t*
MultiSpeciesType_getSpeciesTypeComponentIndex(MultiSpeciesType_t* mst,
                                              unsigned int n)
{
  return (mst != NULL) ? mst->getSpeciesTypeComponentIndex(n) : NULL;
}


/*
 * Get a SpeciesTypeComponentIndex_t from the MultiSpeciesType_t based on its
 * identifier.
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t*
MultiSpeciesType_getSpeciesTypeComponentIndexById(MultiSpeciesType_t* mst,
                                                  const char *sid)
{
  return (mst != NULL && sid != NULL) ? mst->getSpeciesTypeComponentIndex(sid)
    : NULL;
}


/*
 * Adds a copy of the given SpeciesTypeComponentIndex_t to this
 * MultiSpeciesType_t.
 */
LIBSBML_EXTERN
int
MultiSpeciesType_addSpeciesTypeComponentIndex(MultiSpeciesType_t* mst,
                                              const
                                                SpeciesTypeComponentIndex_t*
                                                  stci)
{
  return (mst != NULL) ? mst->addSpeciesTypeComponentIndex(stci) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of SpeciesTypeComponentIndex_t objects in this
 * MultiSpeciesType_t.
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumSpeciesTypeComponentIndexes(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->getNumSpeciesTypeComponentIndexes() :
    SBML_INT_MAX;
}


/*
 * Creates a new SpeciesTypeComponentIndex_t object, adds it to this
 * MultiSpeciesType_t object and returns the SpeciesTypeComponentIndex_t object
 * created.
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t*
MultiSpeciesType_createSpeciesTypeComponentIndex(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->createSpeciesTypeComponentIndex() : NULL;
}


/*
 * Removes the nth SpeciesTypeComponentIndex_t from this MultiSpeciesType_t and
 * returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t*
MultiSpeciesType_removeSpeciesTypeComponentIndex(MultiSpeciesType_t* mst,
                                                 unsigned int n)
{
  return (mst != NULL) ? mst->removeSpeciesTypeComponentIndex(n) : NULL;
}


/*
 * Removes the SpeciesTypeComponentIndex_t from this MultiSpeciesType_t based
 * on its identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesTypeComponentIndex_t*
MultiSpeciesType_removeSpeciesTypeComponentIndexById(MultiSpeciesType_t* mst,
                                                     const char* sid)
{
  return (mst != NULL && sid != NULL) ?
    mst->removeSpeciesTypeComponentIndex(sid) : NULL;
}


/*
 * Returns a ListOf_t * containing InSpeciesTypeBond_t objects from this
 * MultiSpeciesType_t.
 */
LIBSBML_EXTERN
ListOf_t*
MultiSpeciesType_getListOfInSpeciesTypeBonds(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->getListOfInSpeciesTypeBonds() : NULL;
}


/*
 * Get an InSpeciesTypeBond_t from the MultiSpeciesType_t.
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t*
MultiSpeciesType_getInSpeciesTypeBond(MultiSpeciesType_t* mst, unsigned int n)
{
  return (mst != NULL) ? mst->getInSpeciesTypeBond(n) : NULL;
}


/*
 * Get an InSpeciesTypeBond_t from the MultiSpeciesType_t based on its
 * identifier.
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t*
MultiSpeciesType_getInSpeciesTypeBondById(MultiSpeciesType_t* mst,
                                          const char *sid)
{
  return (mst != NULL && sid != NULL) ? mst->getInSpeciesTypeBond(sid) : NULL;
}


/*
 * Adds a copy of the given InSpeciesTypeBond_t to this MultiSpeciesType_t.
 */
LIBSBML_EXTERN
int
MultiSpeciesType_addInSpeciesTypeBond(MultiSpeciesType_t* mst,
                                      const InSpeciesTypeBond_t* istb)
{
  return (mst != NULL) ? mst->addInSpeciesTypeBond(istb) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of InSpeciesTypeBond_t objects in this MultiSpeciesType_t.
 */
LIBSBML_EXTERN
unsigned int
MultiSpeciesType_getNumInSpeciesTypeBonds(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->getNumInSpeciesTypeBonds() : SBML_INT_MAX;
}


/*
 * Creates a new InSpeciesTypeBond_t object, adds it to this MultiSpeciesType_t
 * object and returns the InSpeciesTypeBond_t object created.
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t*
MultiSpeciesType_createInSpeciesTypeBond(MultiSpeciesType_t* mst)
{
  return (mst != NULL) ? mst->createInSpeciesTypeBond() : NULL;
}


/*
 * Removes the nth InSpeciesTypeBond_t from this MultiSpeciesType_t and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t*
MultiSpeciesType_removeInSpeciesTypeBond(MultiSpeciesType_t* mst,
                                         unsigned int n)
{
  return (mst != NULL) ? mst->removeInSpeciesTypeBond(n) : NULL;
}


/*
 * Removes the InSpeciesTypeBond_t from this MultiSpeciesType_t based on its
 * identifier and returns a pointer to it.
 */
LIBSBML_EXTERN
InSpeciesTypeBond_t*
MultiSpeciesType_removeInSpeciesTypeBondById(MultiSpeciesType_t* mst,
                                             const char* sid)
{
  return (mst != NULL && sid != NULL) ? mst->removeInSpeciesTypeBond(sid) :
    NULL;
}


LIBSBML_EXTERN
int
MultiSpeciesType_hasRequiredAttributes(MultiSpeciesType_t * mst)
{
  return (mst != NULL) ? static_cast<int>(mst->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
MultiSpeciesType_t *
ListOfMultiSpeciesTypes_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfMultiSpeciesTypes *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
MultiSpeciesType_t *
ListOfMultiSpeciesTypes_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfMultiSpeciesTypes *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END

#endif /*__cplusplus */


