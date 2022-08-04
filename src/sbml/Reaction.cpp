/**
 * @file    Reaction.cpp
 * @brief   Implementations of Reaction and ListOfReactions.
 * @author  Ben Bornstein
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
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
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
 * ---------------------------------------------------------------------- -->*/

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SpeciesReference.h>
#include <sbml/SimpleSpeciesReference.h>
#include <sbml/ModifierSpeciesReference.h>
#include <sbml/KineticLaw.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLError.h>
#include <sbml/Model.h>
#include <sbml/Reaction.h>

#include <sbml/util/ElementFilter.h>

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

/**
 * Used by getReactant(species), getProduct(species), and
 * getModifier(species).
 */
static SBase*
GetSpeciesRef (ListOf& items, const string& species)
{
  // TODO: Maybe ListOf should return begin and end iterators to the
  // underlying container.  Then this loop could be rewritten with
  // a find_if() algorithm.

  unsigned int size = items.size();

  for (unsigned int n = 0; n < size; ++n)
  {
    SpeciesReference* sr = static_cast<SpeciesReference*>( items.get(n) );
    if (sr->getSpecies() == species) return sr;
    else if (sr->getId() == species) return sr;
  }

  return NULL;
}


/**
 * Simply calls non-const version above.
 */
static const SBase*
GetSpeciesRef (const ListOf& items, const std::string& species)
{
  return GetSpeciesRef(const_cast<ListOf&>(items), species);
}



Reaction::Reaction (unsigned int level, unsigned int version) :
   SBase ( level, version )
 , mReactants (level, version)
 , mProducts  (level, version)
 , mModifiers (level, version)
 , mKineticLaw( NULL        )
 , mReversible( true     )
 , mFast      ( false    )
 , mIsSetFast ( false    )
 , mCompartment ( "" )
 , mIsSetReversible (false)
 , mExplicitlySetReversible (false)
 , mExplicitlySetFast (false)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();

  mReactants.setType( ListOfSpeciesReferences::Reactant );
  mProducts .setType( ListOfSpeciesReferences::Product  );
  mModifiers.setType( ListOfSpeciesReferences::Modifier );
  // before level 3 reversible and fast was set by default
  if (level < 3)
  {
    mIsSetReversible = true;
  }

  connectToChild();
}


Reaction::Reaction (SBMLNamespaces * sbmlns) :
   SBase      ( sbmlns   )
 , mReactants (sbmlns)
 , mProducts  (sbmlns)
 , mModifiers (sbmlns)
 , mKineticLaw( NULL        )
 , mReversible( true     )
 , mFast      ( false    )
 , mIsSetFast ( false    )
 , mCompartment ( "" )
 , mIsSetReversible (false)
 , mExplicitlySetReversible (false)
 , mExplicitlySetFast (false)
{
  if (!hasValidLevelVersionNamespaceCombination())
  {
    throw SBMLConstructorException(getElementName(), sbmlns);
  }

  mReactants.setType( ListOfSpeciesReferences::Reactant );
  mProducts .setType( ListOfSpeciesReferences::Product  );
  mModifiers.setType( ListOfSpeciesReferences::Modifier );

  // before level 3 reversible and fast was set by default
  if (sbmlns->getLevel() < 3)
  {
    mIsSetReversible = true;
  }
  connectToChild();
  loadPlugins(sbmlns);
}


/*
 * Destroys this Reaction.
 */
Reaction::~Reaction ()
{
  delete mKineticLaw;
}


/*
 * Copy constructor. Creates a copy of this Reaction.
 */
Reaction::Reaction (const Reaction& orig)
  : SBase      ( orig )
  , mReactants ( orig.mReactants  )
  , mProducts  ( orig.mProducts   )
  , mModifiers ( orig.mModifiers  )
  , mKineticLaw( NULL    )
  , mReversible( orig.mReversible )
  , mFast      ( orig.mFast       )
  , mIsSetFast ( orig.mIsSetFast  )
  , mCompartment ( orig.mCompartment )
  , mIsSetReversible ( orig.mIsSetReversible )
  , mExplicitlySetReversible ( orig.mExplicitlySetReversible )
  , mExplicitlySetFast       ( orig.mExplicitlySetFast )
{  
  if (orig.mKineticLaw != NULL)
  {
    mKineticLaw = static_cast<KineticLaw*>( orig.mKineticLaw->clone() );
  }
  connectToChild();
}


/*
 * Assignment operator.
 */
Reaction& Reaction::operator=(const Reaction& rhs)
{
  if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
    mReversible = rhs.mReversible ;
    mFast       = rhs.mFast       ;
    mIsSetFast  = rhs.mIsSetFast  ;
    mReactants  = rhs.mReactants  ;
    mProducts   = rhs.mProducts   ;
    mModifiers  = rhs.mModifiers  ;
    mCompartment = rhs.mCompartment;
    mIsSetReversible = rhs.mIsSetReversible;
    mExplicitlySetReversible = rhs.mExplicitlySetReversible;
    mExplicitlySetFast = rhs.mExplicitlySetFast;

    delete mKineticLaw;
    if (rhs.mKineticLaw != NULL)
    {
      mKineticLaw = static_cast<KineticLaw*>( rhs.mKineticLaw->clone() );
    }
    else
    {
      mKineticLaw = NULL;
    }
  }

  connectToChild();

  return *this;
}


/** @cond doxygenLibsbmlInternal */
bool
Reaction::accept (SBMLVisitor& v) const
{
  bool result = v.visit(*this);

  mReactants.accept(v);
  mProducts .accept(v);
  mModifiers.accept(v);

  if (mKineticLaw != NULL) mKineticLaw->accept(v);

  v.leave(*this);

  return result;
}
/** @endcond */


/*
 * @return a (deep) copy of this Reaction.
 */
Reaction*
Reaction::clone () const
{
  return new Reaction(*this);
}


SBase*
Reaction::getElementBySId(const std::string& id)
{
  if (id.empty()) return NULL;
  if (mReactants.getId() == id) return &mReactants;
  if (mProducts.getId() == id) return &mProducts;
  if (mModifiers.getId() == id) return &mModifiers;
  if (mKineticLaw && mKineticLaw->getId() == id) return mKineticLaw;

  SBase* obj = mReactants.getElementBySId(id);
  if (obj != NULL) return obj;
  obj = mProducts.getElementBySId(id);
  if (obj != NULL) return obj;
  obj = mModifiers.getElementBySId(id);
  if (obj != NULL) return obj;
  if (mKineticLaw != NULL) {
    obj = mKineticLaw->getElementBySId(id);
    if (obj != NULL) return obj;
  }
  return getElementFromPluginsBySId(id);
}


SBase*
Reaction::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty()) return NULL;
  if (mReactants.getMetaId() == metaid) return &mReactants;
  if (mProducts.getMetaId() == metaid) return &mProducts;
  if (mModifiers.getMetaId() == metaid) return &mModifiers;
  if (mKineticLaw && mKineticLaw->getMetaId() == metaid) return mKineticLaw;

  SBase* obj = mReactants.getElementByMetaId(metaid);
  if (obj != NULL) return obj;
  obj = mProducts.getElementByMetaId(metaid);
  if (obj != NULL) return obj;
  obj = mModifiers.getElementByMetaId(metaid);
  if (obj != NULL) return obj;
  if (mKineticLaw != NULL) {
    obj = mKineticLaw->getElementByMetaId(metaid);
    if (obj != NULL) return obj;
  }
  return getElementFromPluginsByMetaId(metaid);
}


List*
Reaction::getAllElements(ElementFilter *filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mKineticLaw, filter);  
  
  ADD_FILTERED_LIST(ret, sublist, mReactants, filter);  
  ADD_FILTERED_LIST(ret, sublist, mProducts, filter);  
  ADD_FILTERED_LIST(ret, sublist, mModifiers, filter);  

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}

void
Reaction::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (mCompartment == oldid) {
    setCompartment(newid);
  }
}

/*
 * Initializes the fields of this Reaction to their defaults:
 *
 *   - reversible = true
 *   - fast       = false  (L1 only)
 */
void
Reaction::initDefaults ()
{
  setReversible(true);
  // not explicilty set
  mExplicitlySetReversible = false;

  //
  // Set fast explicitly and make sure mIsSetFast is false.  This preserves
  // backward compatibility with L1 where fast defaulted to false and such
  // Reaction.isSetFast() was not available.  E.g.:
  //
  //   Level 1                          Level 2
  //   ---------------------------      -------------------------------
  //   Reaction r;                      Reaction r;
  //   r.getFast()   == false;          r.getFast()   == false, but
  //   r.isSetFast() == N/A             r.isSetFast() == false
  //
  // but for L3 acknowledge that fast has been set
  mFast      = false;
  mIsSetFast = false;
  if (getLevel() == 3)
  {
    setFast(false);
  }

  mExplicitlySetFast = false;
}


/*
 * @return the id of this SBML object.
 */
const string&
Reaction::getId () const
{
  return mId;
}


/*
 * @return the name of this SBML object.
 */
const string&
Reaction::getName () const
{
  return (getLevel() == 1) ? mId : mName;
}


/*
 * @return the KineticLaw of this Reaction.
 */
const KineticLaw*
Reaction::getKineticLaw () const
{
  return mKineticLaw;
}


/*
 * @return the KineticLaw of this Reaction.
 */
KineticLaw*
Reaction::getKineticLaw ()
{
  return mKineticLaw;
}


/*
 * @return the reversible status of this Reaction.
 */
bool
Reaction::getReversible () const
{
  return mReversible;
}


/*
 * @return the fast status of this Reaction.
 */
bool
Reaction::getFast () const
{
  return mFast;
}


/*
 * @return the compartment of this SBML object.
 */
const string&
Reaction::getCompartment () const
{
  return mCompartment;
}


/*
 * @return @c true if the id of this SBML object is set, false
 * otherwise.
 */
bool
Reaction::isSetId () const
{
  return (mId.empty() == false);
}


/*
 * @return @c true if the name of this SBML object is set, false
 * otherwise.
 */
bool
Reaction::isSetName () const
{
  return (getLevel() == 1) ? (mId.empty() == false) : 
                            (mName.empty() == false);
}


/*
 * @return @c true if the KineticLaw of this Reaction is set, false
 * otherwise.
 */
bool
Reaction::isSetKineticLaw () const
{
  return (mKineticLaw != NULL);
}


/*
 * @return @c true if the fast status of this Reaction is set, false
 * otherwise.
 *
 * In L1, fast is optional with a default of false, which means it is
 * effectively always set.  In L2, however, fast is optional with no
 * default value, so it may or may not be set to a specific value.
 */
bool
Reaction::isSetFast () const
{
  return mIsSetFast;
}


/*
 * @return @c true if the compartment of this SBML object is set, false
 * otherwise.
 */
bool
Reaction::isSetCompartment () const
{
  return (mCompartment.empty() == false);
}


/*
 * @return @c true if the fast status of this Reaction is set, false
 * otherwise.
 */
bool
Reaction::isSetReversible () const
{
  return mIsSetReversible;
}


/*
 * Sets the id of this SBML object to a copy of @p sid.
 */
int
Reaction::setId (const std::string& sid)
{
  if (!(SyntaxChecker::isValidInternalSId(sid)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mId = sid;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the name of this SBML object to a copy of name.
 */
int
Reaction::setName (const std::string& name)
{
  /* if this is setting an L2 name the type is string
   * whereas if it is setting an L1 name its type is SId
   */
  if (getLevel() == 1)
  {
    if (!(SyntaxChecker::isValidInternalSId(name)))
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
      mId = name;
      return LIBSBML_OPERATION_SUCCESS;
    }
  }
  else
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the KineticLaw of this Reaction to a copy of the given KineticLaw.
 */
int
Reaction::setKineticLaw (const KineticLaw* kl)
{
  int returnValue = checkCompatibility(static_cast<const SBase *>(kl));
  
  if (returnValue == LIBSBML_OPERATION_FAILED && kl == NULL)
  {
    delete mKineticLaw;
    mKineticLaw = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (returnValue != LIBSBML_OPERATION_SUCCESS)
  {
    return returnValue;
  }
  
  if (mKineticLaw == kl)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mKineticLaw;
    mKineticLaw = static_cast<KineticLaw*>( kl->clone() );

    if (mKineticLaw != NULL) mKineticLaw->connectToParent(this);
    
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the reversible status of this Reaction to value.
 */
int
Reaction::setReversible (bool value)
{
  mReversible = value;
  mIsSetReversible = true;
  mExplicitlySetReversible = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the fast status of this Reaction to value.
 */
int
Reaction::setFast (bool value)
{
  if (getLevel() == 3 && getVersion() > 1)
  {
    mFast = false;
    mIsSetFast = false;
    mExplicitlySetFast = false;
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else
  {
    mFast      = value;
    mIsSetFast = true;
    mExplicitlySetFast = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the compartment of this SBML object to a copy of @p sid.
 */
int
Reaction::setCompartment (const std::string& sid)
{
  if (getLevel() < 3)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }

  if (!(SyntaxChecker::isValidInternalSId(sid)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCompartment = sid;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets the name of this SBML object.
 */
int
Reaction::unsetName ()
{
  if (getLevel() == 1) 
  {
    mId.erase();
  }
  else 
  {
    mName.erase();
  }

  if (getLevel() == 1 && mId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (mName.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the KineticLaw of this Reaction.
 */
int
Reaction::unsetKineticLaw ()
{
  delete mKineticLaw;
  mKineticLaw = NULL;

  if (mKineticLaw == NULL) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the fast status of this Reaction.
 *
 * In L1, fast is optional with a default of false, which means it is
 * effectively always set.  In L2, however, fast is optional with no
 * default value, so it may or may not be set to a specific value.
 */
int
Reaction::unsetFast ()
{
  mIsSetFast = false;

  if (getLevel() == 3 && getVersion() > 1)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }

  if (!mIsSetFast)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the compartment of this SBML object.
 */
int
Reaction::unsetCompartment ()
{
  if (getLevel() < 3) 
  {
    mCompartment.erase();
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }

  mCompartment.erase();

  if (mCompartment.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the reversible status of this Reaction.
 */
int
Reaction::unsetReversible ()
{
  if (getLevel() < 3)
  {
    // reset default
    mReversible = true;
    mIsSetReversible = true;
    mExplicitlySetReversible = false;
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else
  {
    mIsSetReversible = false;
    mExplicitlySetReversible = false;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Adds a copy of the given reactant (SpeciesReference) to this Reaction.
 */
int
Reaction::addReactant (const SpeciesReference* sr)
{
  if (sr == NULL)
    return LIBSBML_OPERATION_FAILED;

  int returnValue = checkCompatibility(static_cast<const SBase *>(sr));
  if (returnValue != LIBSBML_OPERATION_SUCCESS)
  {
    return returnValue;
  }
  else if (sr->isSetId() 
       && (getListOfReactants()->get(sr->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mReactants.append(sr);
  }
}

int Reaction::addReactant(
    const Species *species,
    double stoichiometry,
    const std::string id,
    bool constant)
{
  if (species == NULL)
    return LIBSBML_INVALID_OBJECT;

  if (!species->isSetId())
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  if (!id.empty() && getListOfReactants()->get(id) != NULL)
    {
      // an object with this id already exists
      return LIBSBML_DUPLICATE_OBJECT_ID;
    }

  SpeciesReference* ref = createReactant();
  if (!id.empty())
    ref->setId(id);

  ref->setStoichiometry(stoichiometry);
  ref->setSpecies(species->getId());
  ref->setConstant(constant);

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Adds a copy of the given product (SpeciesReference) to this Reaction.
 */
int
Reaction::addProduct (const SpeciesReference* sr)
{
  if (sr == NULL)
    return LIBSBML_OPERATION_FAILED;

  int returnValue = checkCompatibility(static_cast<const SBase *>(sr));
  if (returnValue != LIBSBML_OPERATION_SUCCESS)
  {
    return returnValue;
  }
  else if (sr->isSetId() 
       && (getListOfProducts()->get(sr->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mProducts.append(sr);
  }
}

int
Reaction::addProduct(
    const Species *species,
    double stoichiometry,
    const std::string id,
    bool constant)
{
  if (species == NULL)
    return LIBSBML_INVALID_OBJECT;

  if (!species->isSetId())
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  if (!id.empty() && getListOfProducts()->get(id) != NULL)
    {
      // an object with this id already exists
      return LIBSBML_DUPLICATE_OBJECT_ID;
    }

  SpeciesReference* ref = createProduct();
  if (!id.empty())
    ref->setId(id);

  if (stoichiometry == stoichiometry)
    ref->setStoichiometry(stoichiometry);

  ref->setSpecies(species->getId());
  ref->setConstant(constant);

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Adds a copy of the given modifier (ModifierSpeciesReference) to this
 * Reaction.
 */
int
Reaction::addModifier (const ModifierSpeciesReference* msr)
{
  if (msr == NULL)
    return LIBSBML_OPERATION_FAILED;

  int returnValue = checkCompatibility(static_cast<const SBase *>(msr));
  if (returnValue != LIBSBML_OPERATION_SUCCESS)
  {
    return returnValue;
  }
  else if (msr->isSetId() 
       && (getListOfModifiers()->get(msr->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mModifiers.append(msr);
  }
}

int
Reaction::addModifier(
    const Species *species,
    const std::string id)
{
  if (species == NULL)
    return LIBSBML_INVALID_OBJECT;

  if (!species->isSetId())
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  if (!id.empty() && getListOfModifiers()->get(id) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }

  ModifierSpeciesReference* ref = createModifier();
  if (!id.empty())
    ref->setId(id);

  ref->setSpecies(species->getId());

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Creates a new SpeciesReference, adds it to this Reaction's list of
 * reactants and returns it.
 */
SpeciesReference*
Reaction::createReactant ()
{
  SpeciesReference* sr = NULL;

  try
  {
    sr = new SpeciesReference(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
    return NULL;
  }
  
  if (sr != NULL) mReactants.appendAndOwn(sr);

  return sr;
}


/*
 * Creates a new SpeciesReference, adds it to this Reaction's list of
 * products and returns it.
 */
SpeciesReference*
Reaction::createProduct ()
{
  SpeciesReference* sr = NULL;

  try
  {
    sr = new SpeciesReference(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
    return NULL;
  }

  if (sr) mProducts.appendAndOwn(sr);

  return sr;
}


/*
 * Creates a new ModifierSpeciesReference, adds it to this Reaction's
 * list of modifiers and returns it.
 */
ModifierSpeciesReference*
Reaction::createModifier ()
{
  ModifierSpeciesReference* sr = NULL;

  try
  {
    sr = new ModifierSpeciesReference(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
    return NULL;
  }
  
  if (sr != NULL) mModifiers.appendAndOwn(sr);

  return sr;
}


/*
 * Creates a new KineticLaw for this Reaction and returns it.  If this
 * Reaction had a previous KineticLaw, it will be destroyed.
 */
KineticLaw*
Reaction::createKineticLaw ()
{
  delete mKineticLaw;
  mKineticLaw = NULL;

  try
  {
    mKineticLaw = new KineticLaw(getSBMLNamespaces());
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * so do nothing
     */
    return NULL;
  }

  if (mKineticLaw != NULL)
  {
    mKineticLaw->connectToParent(this);
  }

  return mKineticLaw;
}


/*
 * @return the list of Reactants for this Reaction.
 */
const ListOfSpeciesReferences*
Reaction::getListOfReactants () const
{
  return &mReactants;
}


/*
 * @return the list of Reactants for this Reaction.
 */
ListOfSpeciesReferences*
Reaction::getListOfReactants ()
{
  return &mReactants;
}


/*
 * @return the list of Products for this Reaction.
 */
const ListOfSpeciesReferences*
Reaction::getListOfProducts () const
{
  return &mProducts;
}


/*
 * @return the list of Products for this Reaction.
 */
ListOfSpeciesReferences*
Reaction::getListOfProducts ()
{
  return &mProducts;
}


/*
 * @return the list of Modifiers for this Reaction.
 */
const ListOfSpeciesReferences*
Reaction::getListOfModifiers () const
{
  return &mModifiers;
}


/*
 * @return the list of Modifiers for this Reaction.
 */
ListOfSpeciesReferences*
Reaction::getListOfModifiers ()
{
  return &mModifiers;
}


/*
 * @return the nth reactant (SpeciesReference) of this Reaction.
 */
const SpeciesReference*
Reaction::getReactant (unsigned int n) const
{
  return static_cast<const SpeciesReference*>( mReactants.get(n) );
}


/*
 * @return the nth reactant (SpeciesReference) of this Reaction.
 */
SpeciesReference*
Reaction::getReactant (unsigned int n)
{
  return static_cast<SpeciesReference*>( mReactants.get(n) );
}


/*
 * @return the reactant (SpeciesReference) in this Reaction with the given
 * species or @c NULL if no such reactant exists.
 */
const SpeciesReference*
Reaction::getReactant (const std::string& species) const
{
  return
    static_cast<const SpeciesReference*>( GetSpeciesRef(mReactants, species) );
}


/*
 * @return the reactant (SpeciesReference) in this Reaction with the given
 * species or @c NULL if no such reactant exists.
 */
SpeciesReference*
Reaction::getReactant (const std::string& species)
{
  return static_cast<SpeciesReference*>( GetSpeciesRef(mReactants, species) );
}


/*
 * @return the nth product (SpeciesReference) of this Reaction.
 */
const SpeciesReference*
Reaction::getProduct (unsigned int n) const
{
  return static_cast<const SpeciesReference*>( mProducts.get(n) );
}


/*
 * @return the nth product (SpeciesReference) of this Reaction.
 */
SpeciesReference*
Reaction::getProduct (unsigned int n)
{
  return static_cast<SpeciesReference*>( mProducts.get(n) );
}


/*
 * @return the product (SpeciesReference) in this Reaction with the given
 * species or @c NULL if no such product exists.
 */
const SpeciesReference*
Reaction::getProduct (const std::string& species) const
{
  return
    static_cast<const SpeciesReference*>( GetSpeciesRef(mProducts, species) );
}


/*
 * @return the product (SpeciesReference) in this Reaction with the given
 * species or @c NULL if no such product exists.
 */
SpeciesReference*
Reaction::getProduct (const std::string& species)
{
  return static_cast<SpeciesReference*>( GetSpeciesRef(mProducts, species) );
}


/*
 * @return the nth modifier (ModifierSpeciesReference) of this Reaction.
 */
const ModifierSpeciesReference*
Reaction::getModifier (unsigned int n) const
{
  return static_cast<const ModifierSpeciesReference*>( mModifiers.get(n) );
}


/*
 * @return the nth modifier (ModifierSpeciesReference) of this Reaction.
 */
ModifierSpeciesReference*
Reaction::getModifier (unsigned int n)
{
  return static_cast<ModifierSpeciesReference*>( mModifiers.get(n) );
}


/*
 * @return the modifier (ModifierSpeciesReference) in this Reaction with
 * the given species or @c NULL if no such modifier exists.
 */
const ModifierSpeciesReference*
Reaction::getModifier (const std::string& species) const
{
  return static_cast<const ModifierSpeciesReference*>
  (
    GetSpeciesRef(mModifiers, species)
  );
}


/*
 * @return the modifier (ModifierSpeciesReference) in this Reaction with
 * the given species or @c NULL if no such modifier exists.
 */
ModifierSpeciesReference*
Reaction::getModifier (const std::string& species)
{
  return static_cast<ModifierSpeciesReference*>
  (
    GetSpeciesRef(mModifiers, species)
  );
}


/*
 * @return the number of reactants (SpeciesReferences) in this Reaction.
 */
unsigned int
Reaction::getNumReactants () const
{
  return mReactants.size();
}


/*
 * @return the number of products (SpeciesReferences) in this Reaction.
 */
unsigned int
Reaction::getNumProducts () const
{
  return mProducts.size();
}


/*
 * @return the number of modifiers (ModifierSpeciesReferences) in this
 * Reaction.
 */
unsigned int
Reaction::getNumModifiers () const
{
  return mModifiers.size();
}


/**
 * Removes the nth reactant species (SpeciesReference object) in the list of 
 * reactants in this Reaction and returns a pointer to it.
 */
SpeciesReference* 
Reaction::removeReactant (unsigned int n)
{
  return static_cast<SpeciesReference*>(mReactants.remove(n));
}


/**
 * Removes the reactant species (SpeciesReference object) having the given  
 * "species" attribute in this Reaction and returns a pointer to it.
 */
SpeciesReference* 
Reaction::removeReactant (const std::string& species)
{
  unsigned int size = mReactants.size();

  for (unsigned int n = 0; n < size; ++n)
  {
    SpeciesReference* sr = static_cast<SpeciesReference*>( mReactants.get(n) );
    if (sr->getSpecies() == species) 
      return static_cast<SpeciesReference*>(mReactants.remove(n));
  }
  return NULL;
}


/**
 * Removes the nth product species (SpeciesReference object) in the list of 
 * products in this Reaction and returns a pointer to it.
 */
SpeciesReference* 
Reaction::removeProduct (unsigned int n)
{
  return static_cast<SpeciesReference*>(mProducts.remove(n));
}


/**
 * Removes the product species (SpeciesReference object) having the given  
 * "species" attribute in this Reaction and returns a pointer to it.
 */
SpeciesReference* 
Reaction::removeProduct (const std::string& species)
{
  unsigned int size = mProducts.size();

  for (unsigned int n = 0; n < size; ++n)
  {
    SpeciesReference* sr = static_cast<SpeciesReference*>( mProducts.get(n) );
    if (sr->getSpecies() == species) 
      return static_cast<SpeciesReference*>(mProducts.remove(n));
  }
  return NULL;
}


/**
 * Removes the nth modifier species (ModifierSpeciesReference object) in 
 * the list of  modifiers in this Reaction and returns a pointer to it.
 */
ModifierSpeciesReference* 
Reaction::removeModifier (unsigned int n)
{
  return static_cast<ModifierSpeciesReference*>(mModifiers.remove(n));
}


/**
 * Removes the modifier species (ModifierSpeciesReference object) having 
 * the given "species" attribute in this Reaction and returns a pointer to it.
 */
ModifierSpeciesReference* 
Reaction::removeModifier (const std::string& species)
{
  unsigned int size = mModifiers.size();

  for (unsigned int n = 0; n < size; ++n)
  {
    SpeciesReference* sr = static_cast<SpeciesReference*>( mModifiers.get(n) );
    if (sr->getSpecies() == species) 
      return static_cast<ModifierSpeciesReference*>(mModifiers.remove(n));
  }
  return NULL;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Reaction::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);

  mReactants.setSBMLDocument(d);
  mProducts .setSBMLDocument(d);
  mModifiers.setSBMLDocument(d);

  if (mKineticLaw != NULL) mKineticLaw->setSBMLDocument(d);
}


/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
  */
void
Reaction::connectToChild()
{
  SBase::connectToChild();
  mReactants.connectToParent(this);
  mProducts .connectToParent(this);
  mModifiers.connectToParent(this);

  if (mKineticLaw) mKineticLaw->connectToParent(this);
}


/**
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePackage function)
 */
void 
Reaction::enablePackageInternal(const std::string& pkgURI, 
                                const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mReactants.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mProducts.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mModifiers.enablePackageInternal(pkgURI,pkgPrefix,flag);

  if (mKineticLaw) mKineticLaw->enablePackageInternal(pkgURI,pkgPrefix,flag);
}


void
Reaction::updateSBMLNamespace(const std::string& pkg, unsigned int level,
  unsigned int version)
{
  SBase::updateSBMLNamespace(pkg, level, version);

  mReactants.updateSBMLNamespace(pkg, level, version);
  mProducts.updateSBMLNamespace(pkg, level, version);
  mModifiers.updateSBMLNamespace(pkg, level, version);

  if (mKineticLaw) mKineticLaw->updateSBMLNamespace(pkg, level, version);
}
/** @endcond */


/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
Reaction::getTypeCode () const
{
  return SBML_REACTION;
}


/*
 * @return the name of this element ie "reaction".
 */
const string&
Reaction::getElementName () const
{
  static const string name = "reaction";
  return name;
}


bool 
Reaction::hasRequiredAttributes() const
{
  bool allPresent = true;

  /* required attributes for reaction: 
  * @li id (name in L1)
  * @li fast (in L3V1 only)
  * @li reversible (in L3 only)
  */

  if (!isSetId())
    allPresent = false;

  if (getLevel() > 2 && !isSetReversible())
    allPresent = false;

  if (getLevel() == 3  && getVersion() == 1 && !isSetFast())
    allPresent = false;

  return allPresent;
}


/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or @c NULL if the token was not recognized.
 */
SBase*
Reaction::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = NULL;


  if (name == "listOfReactants")
  {
    if (mReactants.size() != 0)
    {
      if (getLevel() < 3)
        logError(NotSchemaConformant);
      else
        logError(OneSubElementPerReaction, getLevel(), getVersion());
    }
    mReactants.setExplicitlyListed();
    object = &mReactants;
  }
  else if (name == "listOfProducts")
  {
    if (mProducts.size() != 0)
    {
      if (getLevel() < 3)
        logError(NotSchemaConformant);
      else
        logError(OneSubElementPerReaction, getLevel(), getVersion());
    }
    mProducts.setExplicitlyListed();
    object = &mProducts;
  }
  else if (name == "listOfModifiers")
  {
    if (getLevel() == 1)
    {
      return NULL;
    }

    if (mModifiers.size() != 0)
    {
      if (getLevel() < 3)
        logError(NotSchemaConformant);
      else
        logError(OneSubElementPerReaction, getLevel(), getVersion());
    }
    mModifiers.setExplicitlyListed();
    object = &mModifiers;
  }
  else if (name == "kineticLaw")
  {
    if (mKineticLaw != NULL)
    {
      if (getLevel() < 3)
        logError(NotSchemaConformant);
      else
        logError(OneSubElementPerReaction, getLevel(), getVersion());
    }
    delete mKineticLaw;

    try
    {
      mKineticLaw = new KineticLaw(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      mKineticLaw = new KineticLaw(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      mKineticLaw = new KineticLaw(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }

    object      = mKineticLaw;
  }

  return object;
}
/** @endcond */







/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Reaction.
 */
int
Reaction::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "fast")
  {
    value = getFast();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "reversible")
  {
    value = getReversible();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Reaction.
 */
int
Reaction::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Reaction.
 */
int
Reaction::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Reaction.
 */
int
Reaction::getAttribute(const std::string& attributeName,
                       unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Reaction.
 */
int
Reaction::getAttribute(const std::string& attributeName,
                       std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "compartment")
  {
    value = getCompartment();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Reaction's attribute "attributeName" is
 * set.
 */
bool
Reaction::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "fast")
  {
    value = isSetFast();
  }
  else if (attributeName == "reversible")
  {
    value = isSetReversible();
  }
  else if (attributeName == "compartment")
  {
    value = isSetCompartment();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Reaction.
 */
int
Reaction::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "fast")
  {
    return_value = setFast(value);
  }
  else if (attributeName == "reversible")
  {
    return_value = setReversible(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Reaction.
 */
int
Reaction::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Reaction.
 */
int
Reaction::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Reaction.
 */
int
Reaction::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Reaction.
 */
int
Reaction::setAttribute(const std::string& attributeName,
                       const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "compartment")
  {
    return_value = setCompartment(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Reaction.
 */
int
Reaction::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "fast")
  {
    value = unsetFast();
  }
  else if (attributeName == "reversible")
  {
    value = unsetReversible();
  }
  else if (attributeName == "compartment")
  {
    value = unsetCompartment();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this Reaction.
 */
SBase*
Reaction::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "kineticLaw")
  {
    return createKineticLaw();
  }
  else if (elementName == "product")
  {
    return createProduct();
  }
  else if (elementName == "reactant")
  {
    return createReactant();
  }
  else if (elementName == "modifier")
  {
    return createModifier();
  }

  return obj;
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */

/*
 * Adds an new "elementName" object in this Reaction.
 */
int
Reaction::addChildObject(const std::string& elementName, const SBase* element)
{
  if (elementName == "kineticLaw" && element->getTypeCode() == SBML_KINETIC_LAW)
  {
    return setKineticLaw((const KineticLaw*)(element));
  }
  else if (elementName == "reactant" && element->getTypeCode() == SBML_SPECIES_REFERENCE)
  {
    return addReactant((const SpeciesReference*)(element));
  }
  else if (elementName == "product" && element->getTypeCode() == SBML_SPECIES_REFERENCE)
  {
    return addProduct((const SpeciesReference*)(element));
  }
  else if (elementName == "modifier" && element->getTypeCode() == SBML_MODIFIER_SPECIES_REFERENCE)
  {
    return addModifier((const ModifierSpeciesReference*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Adds an new "elementName" object in this Reaction.
 */
SBase*
Reaction::removeChildObject(const std::string& elementName, const std::string& id)
{

  if (elementName == "kineticLaw")
  {
    unsetKineticLaw(); // already deletes the kineticLaw, so nothing to do after
  }
  else if (elementName == "reactant")
  {
    return removeReactant(id);
  }
  else if (elementName == "product")
  {
    return removeProduct(id);
  }
  else if (elementName == "modifier")
  {
    return removeModifier(id);
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this Reaction.
 */
unsigned int
Reaction::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "kineticLaw")
  {
    if (isSetKineticLaw())
    {
      return 1;
    }
  }
  else if (elementName == "reactant")
  {
    return getNumReactants();
  }
  else if (elementName == "product")
  {
    return getNumProducts();
  }
  else if (elementName == "modifier")
  {
    return getNumModifiers();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this Reaction.
 */
SBase*
Reaction::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "kineticLaw")
  {
    return getKineticLaw();
  }
  else if (elementName == "reactant")
  {
    return getReactant(index);
  }
  else if (elementName == "product")
  {
    return getProduct(index);
  }
  else if (elementName == "modifier")
  {
    return getModifier(index);
  }

  return obj;
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */
/**
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
Reaction::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  switch (level)
  {
  case 1:
    attributes.add("name");
    attributes.add("reversible");
    attributes.add("fast");
    break;
  case 2:
    attributes.add("name");
    attributes.add("reversible");
    attributes.add("fast");
    attributes.add("id");
    if (version == 2)
    {
      attributes.add("sboTerm");
    }

    break;
  case 3:
    attributes.add("reversible");
    attributes.add("compartment");
    if (version == 1)
    {
      attributes.add("name");
      attributes.add("id");
      attributes.add("fast");
    }
    break;
  default:
    // assumes l3v2
    attributes.add("reversible");
    attributes.add("compartment");
    break;
  }

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parent's implementation of this method as well.
 */
void
Reaction::readAttributes (const XMLAttributes& attributes,
                          const ExpectedAttributes& expectedAttributes)
{
  const unsigned int level   = getLevel  ();

  SBase::readAttributes(attributes, expectedAttributes);

  switch (level)
  {
  case 1:
    readL1Attributes(attributes);
    break;
  case 2:
    readL2Attributes(attributes);
    break;
  case 3:
  default:
    readL3Attributes(attributes);
    break;
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parent's implementation of this method as well.
 */
void
Reaction::readL1Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // name: SName  { use="required" }  (L1v1, L1v2)
  //
  bool assigned = attributes.readInto("name", mId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mId.size() == 0)
  {
    logEmptyString("name", level, version, "<reaction>");
  }
  if (!SyntaxChecker::isValidInternalSId(mId)) 
    logError(InvalidIdSyntax, level, version, "The id '" + mId + "' does not conform to the syntax.");

  //
  // reversible: boolean  { use="optional"  default="true" }
  // (L1v1, L1v2, L2v1->)
  //
  mExplicitlySetReversible = attributes.readInto("reversible", mReversible, getErrorLog(), false, getLine(), getColumn());

  //
  // fast: boolean  { use="optional" default="false" }  (L1v1, L1v2)
  // fast: boolean  { use="optional" }                  (L2v1 ->)
  //
  mIsSetFast = attributes.readInto("fast", mFast, getErrorLog(), false, getLine(), getColumn());
  mExplicitlySetFast = mIsSetFast;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parent's implementation of this method as well.
 */
void
Reaction::readL2Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  //   id: SId    { use="required" }  (L2v1 ->)
  //
  bool assigned = attributes.readInto("id", mId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mId.size() == 0)
  {
    logEmptyString("id", level, version, "<reaction>");
  }
  if (!SyntaxChecker::isValidInternalSId(mId)) 
    logError(InvalidIdSyntax, level, version, "The id '" + mId + "' does not conform to the syntax.");

  //
  // reversible: boolean  { use="optional"  default="true" }
  // (L1v1, L1v2, L2v1->)
  // reversible: boolean  { use="required"} (L3v1->)
  //
  mExplicitlySetReversible = attributes.readInto("reversible", mReversible, getErrorLog(), false, getLine(), getColumn());

  //
  // fast: boolean  { use="optional" }                  (L2v1 ->)
  //
  mIsSetFast = attributes.readInto("fast", mFast, getErrorLog(), false, getLine(), getColumn());
  mExplicitlySetFast = mIsSetFast;

  //
  // name: string  { use="optional" }  (L2v1 ->)
  //
  attributes.readInto("name", mName, getErrorLog(), false, getLine(), getColumn());
  
  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2 ->)
  //
  if (version == 2) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version,
        getLine(), getColumn());

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parent's implementation of this method as well.
 */
void
Reaction::readL3Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  //   id: SId    { use="required" }  (L2v1 ->)
  //
  bool assigned;
  // for l3v2 sbase will read this as generically optional
  // we want to log errors relating to the specific object
  if (version == 1)
  {
    assigned = attributes.readInto("id", mId, getErrorLog(), false, getLine(), getColumn());
    if (!assigned)
    {
      logError(AllowedAttributesOnReaction, level, version, 
        "The required attribute 'id' is missing.");
    }
    if (assigned && mId.size() == 0)
    {
      logEmptyString("id", level, version, "<reaction>");
    }
    if (!SyntaxChecker::isValidInternalSId(mId)) 
      logError(InvalidIdSyntax, level, version, "The id '" + mId + "' does not conform to the syntax.");
  }
  else
  {
    // need to check that id was present
    // it has already been read and checked for syntax/emptyness
    if (attributes.hasAttribute("id") == false)
    {
      logError(AllowedAttributesOnReaction, level, version, 
        "The required attribute 'id' is missing.");
    }
  }

  string elplusid = "<reaction>";
  if (!mId.empty()) {
    elplusid += " with the id '" + mId + "'";
  }
  //
  // reversible: boolean  { use="required"} (L3v1->)
  //
  mIsSetReversible = attributes.readInto("reversible", 
                   mReversible, getErrorLog(), false, getLine(), getColumn());
  mExplicitlySetReversible = mIsSetReversible;

  if (!mIsSetReversible)
  {
    logError(AllowedAttributesOnReaction, level, version, 
                "The required attribute 'reversible' is missing from the "
                + elplusid + ".");
  }

  //
  // fast: boolean  { use="required" }                  (L3v1 only)
  //
  if (version == 1)
  {
    mIsSetFast = attributes.readInto("fast", mFast, getErrorLog(), 
                                                false, getLine(), getColumn());
    mExplicitlySetFast = mIsSetFast;

    if (!mIsSetFast)
    {
      logError(AllowedAttributesOnReaction, level, version, 
        "The required attribute 'fast' is missing from the "
                  + elplusid + ".");
    }
  }

  //
  // name: string  { use="optional" }  (L2v1 ->)
  //
  // for l3v2 sbase will read this
  if (version == 1)
  {
    attributes.readInto("name", mName, getErrorLog(), false, 
                                       getLine(), getColumn());
  }
   
  //
  // compartment: string { use="optional" } (L3v1 -> )
  //
  assigned = attributes.readInto("compartment", mCompartment, getErrorLog(), false, getLine(), getColumn());
  if (assigned && mCompartment.size() == 0)
  {
    logEmptyString("compartment", level, version, "<reaction>");
  }
  if (!SyntaxChecker::isValidInternalSId(mCompartment)) 
    logError(InvalidIdSyntax, getLevel(), getVersion(), 
      "The " + elplusid + " has a 'compartment' with a value of '" + mCompartment 
      + "' which does not conform .");
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parent's implementation
 * of this method as well.
 */
void
Reaction::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2->)
  //
  // sboTerm for L2V3 or later is written in SBase::writeAttributes()
  //
  if ( (level == 2) && (version == 2) )
  {
    SBO::writeTerm(stream, mSBOTerm);
  }

  // for L3V2 and above SBase will write this out
  if (level < 3 || (level == 3 && version == 1))
  {
    //
    // name: SName   { use="required" }  (L1v1, L1v2)
    //   id: SId     { use="required" }  (L2v1, L2v2)
    //
    const string id = (level == 1) ? "name" : "id";
    stream.writeAttribute(id, mId);

    //
    // name: string  { use="optional" }  (L2v1->)
    //
    if (level > 1) stream.writeAttribute("name", mName);
  }

  //
  // reversible: boolean  { use="optional"  default="true" }
  // (L1v1, L1v2, L2v1-> )
  // reversible: boolean  { use="required"} (L3v1->)
  //
  if (level < 3)
  {
    if (mReversible != true || isExplicitlySetReversible()) 
      stream.writeAttribute("reversible", mReversible);
  }
  else
  {
    // in L3 only write it out if it has been set
    if (isSetReversible())
      stream.writeAttribute("reversible", mReversible);
  }

  //
  // fast: boolean  { use="optional" default="false" }  (L1v1, L1v2, L2v1 ->)
  // fast: boolean  { use="required" }                  (L3v1-> )
  //
  if (level < 3)
  {
    if (mIsSetFast)
    {
      if (isExplicitlySetFast() || level != 1 || mFast != false) 
        stream.writeAttribute("fast", mFast);
    }
  }
  else
  {
    // in L3 only write it out if it has been set
    if (version == 1 && isSetFast())
      stream.writeAttribute("fast", mFast);
  }

  if (level > 2)
  {
    stream.writeAttribute("compartment", mCompartment);
  }

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parent's
 * implementation of this method as well.
 */
void
Reaction::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  const unsigned int level = getLevel();

  if (getLevel() == 3 && getVersion() > 1)
  {
    if (mReactants.hasOptionalElements() == true ||
        mReactants.hasOptionalAttributes() == true ||
        mReactants.isExplicitlyListed())
    {
      mReactants.write(stream);
    }

    if (mProducts.hasOptionalElements() == true ||
        mProducts.hasOptionalAttributes() == true ||
        mProducts.isExplicitlyListed())
    {
      mProducts.write(stream);
    }

    if (mModifiers.hasOptionalElements() == true ||
        mModifiers.hasOptionalAttributes() == true ||
        mModifiers.isExplicitlyListed())
    {
      mModifiers.write(stream);
    }
  }
  else
  {
    // use original code
    if (getNumReactants () > 0) mReactants.write(stream);
    if (getNumProducts  () > 0) mProducts .write(stream);

    if (level > 1 && getNumModifiers () > 0) mModifiers.write(stream);
  }

  if (mKineticLaw != NULL) mKineticLaw->write(stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}
/** @endcond */


/*
 * Creates a new ListOfReactions items.
 */
ListOfReactions::ListOfReactions (unsigned int level, unsigned int version)
  : ListOf(level,version)
{
}


/*
 * Creates a new ListOfReactions items.
 */
ListOfReactions::ListOfReactions (SBMLNamespaces* sbmlns)
  : ListOf(sbmlns)
{
  loadPlugins(sbmlns);
}


/*
 * @return a (deep) copy of this ListOfReactions.
 */
ListOfReactions*
ListOfReactions::clone () const
{
  return new ListOfReactions(*this);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfReactions::getItemTypeCode () const
{
  return SBML_REACTION;
}


/*
 * @return the name of this element ie "listOfReactions".
 */
const string&
ListOfReactions::getElementName () const
{
  static const string name = "listOfReactions";
  return name;
}


/* return nth item in list */
Reaction *
ListOfReactions::get(unsigned int n)
{
  return static_cast<Reaction*>(ListOf::get(n));
}


/* return nth item in list */
const Reaction *
ListOfReactions::get(unsigned int n) const
{
  return static_cast<const Reaction*>(ListOf::get(n));
}


/* return item by id */
Reaction*
ListOfReactions::get (const std::string& sid)
{
  return const_cast<Reaction*>( 
    static_cast<const ListOfReactions&>(*this).get(sid) );
}


/* return item by id */
const Reaction*
ListOfReactions::get (const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Reaction>(sid) );
  return (result == mItems.end()) ? NULL : static_cast <Reaction*> (*result);
}


/* Removes the nth item from this list */
Reaction*
ListOfReactions::remove (unsigned int n)
{
   return static_cast<Reaction*>(ListOf::remove(n));
}


/* Removes item in this list by id */
Reaction*
ListOfReactions::remove (const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Reaction>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }


  return static_cast <Reaction*> (item);
}


/** @cond doxygenLibsbmlInternal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfReactions::getElementPosition () const
{
  return 11;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or @c NULL if the token was not recognized.
 */
SBase*
ListOfReactions::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = NULL;

  if (name == "reaction")
  {
    try
    {
      object = new Reaction(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      object = new Reaction(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      object = new Reaction(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    
    if (object != NULL) mItems.push_back(object);
  }

  return object;
}
/** @endcond */

#endif /* __cplusplus */


/** @cond doxygenIgnored */
LIBSBML_EXTERN
Reaction_t *
Reaction_create (unsigned int level, unsigned int version)
{
  try
  {
    Reaction* obj = new Reaction(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


LIBSBML_EXTERN
Reaction_t *
Reaction_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    Reaction* obj = new Reaction(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}

LIBSBML_EXTERN
void
Reaction_free (Reaction_t *r)
{
  delete r;
}


LIBSBML_EXTERN
Reaction_t *
Reaction_clone (const Reaction_t *r)
{
  return (r != NULL) ? static_cast<Reaction*>( r->clone() ) : NULL;
}


LIBSBML_EXTERN
void
Reaction_initDefaults (Reaction_t *r)
{
  if (r != NULL)
    r->initDefaults();
}


LIBSBML_EXTERN
const XMLNamespaces_t *
Reaction_getNamespaces(Reaction_t *r)
{
  return (r != NULL) ? r->getNamespaces() : NULL;
}

LIBSBML_EXTERN
const char *
Reaction_getId (const Reaction_t *r)
{
  return (r != NULL && r->isSetId()) ? r->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
Reaction_getName (const Reaction_t *r)
{
  return (r != NULL && r->isSetName()) ? r->getName().c_str() : NULL;
}


LIBSBML_EXTERN
KineticLaw_t *
Reaction_getKineticLaw (Reaction_t *r)
{
  return (r != NULL) ? r->getKineticLaw() : NULL;
}


LIBSBML_EXTERN
int
Reaction_getReversible (const Reaction_t *r)
{
  return (r != NULL) ? static_cast<int>( r->getReversible() ) : 0;
}


LIBSBML_EXTERN
int
Reaction_getFast (const Reaction_t *r)
{
  return (r != NULL) ? static_cast<int>( r->getFast() ) : 0;
}


LIBSBML_EXTERN
const char *
Reaction_getCompartment (const Reaction_t *r)
{
  return (r != NULL && r->isSetCompartment()) ? 
                       r->getCompartment().c_str() : NULL;
}


LIBSBML_EXTERN
int
Reaction_isSetId (const Reaction_t *r)
{
  return (r != NULL) ? static_cast<int>( r->isSetId() ) : 0;
}


LIBSBML_EXTERN
int
Reaction_isSetName (const Reaction_t *r)
{
  return (r != NULL) ? static_cast<int>( r->isSetName() ) : 0;
}


LIBSBML_EXTERN
int
Reaction_isSetKineticLaw (const Reaction_t *r)
{
  return (r != NULL) ? static_cast<int>( r->isSetKineticLaw() ) : 0;
}


LIBSBML_EXTERN
int
Reaction_isSetFast (const Reaction_t *r)
{
  return (r != NULL) ? static_cast<int>( r->isSetFast() ) : 0;
}


LIBSBML_EXTERN
int
Reaction_isSetCompartment (const Reaction_t *r)
{
  return (r != NULL) ? static_cast<int>( r->isSetCompartment() ) : 0;
}


LIBSBML_EXTERN
int
Reaction_isSetReversible (const Reaction_t *r)
{
  return (r != NULL) ? static_cast<int>( r->isSetReversible() ) : 0;
}


LIBSBML_EXTERN
int
Reaction_setId (Reaction_t *r, const char *sid)
{
  if (r != NULL)
    return (sid == NULL) ? r->setId("") : r->setId(sid);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_setName (Reaction_t *r, const char *name)
{
  if (r != NULL)
    return (name == NULL) ? r->unsetName() : r->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_setKineticLaw (Reaction_t *r, const KineticLaw_t *kl)
{
  if (r != NULL)
    return (kl == NULL) ? r->unsetKineticLaw() : r->setKineticLaw(kl);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_setReversible (Reaction_t *r, int value)
{
  if (r != NULL)
    return r->setReversible( static_cast<bool>(value) );
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_setFast (Reaction_t *r, int value)
{
  if (r != NULL)
    return r->setFast( static_cast<bool>(value) );
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_setCompartment (Reaction_t *r, const char *compartment)
{
  if (r != NULL)
    return (compartment == NULL) ? r->unsetCompartment() : 
                                 r->setCompartment(compartment);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_unsetName (Reaction_t *r)
{
  if (r != NULL)
    return r->unsetName();
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_unsetCompartment (Reaction_t *r)
{
  if (r != NULL)
    return r->unsetCompartment();
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_unsetKineticLaw (Reaction_t *r)
{
  if (r != NULL)
    return r->unsetKineticLaw();
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_unsetFast (Reaction_t *r)
{
  if (r != NULL)
    return r->unsetFast();
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_unsetReversible (Reaction_t *r)
{
  if (r != NULL)
    return r->unsetReversible();
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_hasRequiredAttributes(Reaction_t *r)
{
  return (r != NULL) ? static_cast<int>(r->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
Reaction_addReactant (Reaction_t *r, const SpeciesReference_t *sr)
{
  if (r != NULL)
    return r->addReactant( static_cast<const SpeciesReference*>(sr) );
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_addReactantBySpecies (Reaction_t *r, const Species_t *s, 
                               double stoichiometry, const char *id,
                               int constant)
{
  if (r != NULL)
    return r->addReactant( static_cast<const Species*>(s), stoichiometry, id, 
                            constant);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_addProduct (Reaction_t *r, const SpeciesReference_t *sr)
{
  if (r != NULL)
    return r->addProduct( static_cast<const SpeciesReference*>(sr) );
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_addProductBySpecies (Reaction_t *r, const Species_t *s, 
                               double stoichiometry, const char *id,
                               int constant)
{
  if (r != NULL)
    return r->addProduct( static_cast<const Species*>(s), stoichiometry, id, 
                            constant);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_addModifier (Reaction_t *r, const SpeciesReference_t *msr)
{
  if (r != NULL)
  {
    if (msr == NULL || msr->isModifier())
    {
      return r->addModifier(static_cast<const ModifierSpeciesReference*>(msr) );
    }
    else
    {
      return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
  }
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Reaction_addModifierBySpecies (Reaction_t *r, const Species_t *s, 
                               const char *id)
{
  if (r != NULL)
    return r->addModifier( static_cast<const Species*>(s), id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_createReactant (Reaction_t *r)
{
  return (r != NULL) ? r->createReactant() : NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_createProduct (Reaction_t *r)
{
  return (r != NULL) ? r->createProduct() : NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_createModifier (Reaction_t *r)
{
  return (r != NULL) ? r->createModifier() : NULL;
}


LIBSBML_EXTERN
KineticLaw_t *
Reaction_createKineticLaw (Reaction_t *r)
{
  return (r != NULL) ? r->createKineticLaw() : NULL;
}


LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfReactants (Reaction_t *r)
{
  return (r != NULL) ? r->getListOfReactants() : NULL;
}


LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfProducts (Reaction_t *r)
{
  return (r != NULL) ? r->getListOfProducts() : NULL;
}


LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfModifiers (Reaction_t *r)
{
  return (r != NULL) ? r->getListOfModifiers() : NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getReactant (Reaction_t *r, unsigned int n)
{
  return (r != NULL) ? r->getReactant(n) : NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getReactantBySpecies (Reaction_t *r, const char *species)
{
  return (r != NULL && species != NULL) ? r->getReactant(species) : NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getProduct (Reaction_t *r, unsigned int n)
{
  return (r != NULL) ? r->getProduct(n) : NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getProductBySpecies (Reaction_t *r, const char *species)
{
  return (r != NULL && species != NULL) ? r->getProduct(species) : NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getModifier (Reaction_t *r, unsigned int n)
{
  return (r != NULL) ? r->getModifier(n) : NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getModifierBySpecies (Reaction_t *r, const char *species)
{
  return (r != NULL &&  species != NULL) ? r->getModifier(species) : NULL;
}

LIBSBML_EXTERN
unsigned int
Reaction_getNumReactants (const Reaction_t *r)
{
  return (r != NULL) ? r->getNumReactants() : SBML_INT_MAX;
}


LIBSBML_EXTERN
unsigned int
Reaction_getNumProducts (const Reaction_t *r)
{
  return (r != NULL) ? r->getNumProducts() : SBML_INT_MAX;
}


LIBSBML_EXTERN
unsigned int
Reaction_getNumModifiers (const Reaction_t *r)
{
  return (r != NULL) ? r->getNumModifiers() : SBML_INT_MAX;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeReactant (Reaction_t *r, unsigned int n)
{
  return (r != NULL) ? r->removeReactant(n) : NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeReactantBySpecies (Reaction_t *r, const char *species)
{
  if (r != NULL)
    return (species != NULL) ? r->removeReactant(species) : NULL;
  else
    return NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeProduct (Reaction_t *r, unsigned int n)
{
  return (r != NULL) ? r->removeProduct(n) : NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeProductBySpecies (Reaction_t *r, const char *species)
{
  if (r != NULL)
    return (species != NULL) ? r->removeProduct(species) : NULL;
  else
    return NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeModifier (Reaction_t *r, unsigned int n)
{
  return (r != NULL) ? r->removeModifier(n) : NULL;
}


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeModifierBySpecies (Reaction_t *r, const char *species)
{
  if (r != NULL)
    return (species != NULL) ? r->removeModifier(species) : NULL;
  else
    return NULL;
}


LIBSBML_EXTERN
Reaction_t *
ListOfReactions_getById (ListOf_t *lo, const char *sid)
{
  if (lo != NULL)
    return (sid != NULL) ? 
      static_cast <ListOfReactions *> (lo)->get(sid) : NULL;
  else
    return NULL;
}


LIBSBML_EXTERN
Reaction_t *
ListOfReactions_removeById (ListOf_t *lo, const char *sid)
{
  if (lo != NULL)
    return (sid != NULL) ? 
      static_cast <ListOfReactions *> (lo)->remove(sid) : NULL;
  else
    return NULL;
}
/** @endcond */
LIBSBML_CPP_NAMESPACE_END
