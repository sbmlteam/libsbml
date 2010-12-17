/**
 * @file    Reaction.cpp
 * @brief   Implementations of Reaction and ListOfReactions.
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SpeciesReference.h>
#include <sbml/KineticLaw.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLError.h>
#include <sbml/Model.h>
#include <sbml/Reaction.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

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

  return 0;
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
 , mId        ( ""       )
 , mName      ( ""       )
 , mKineticLaw( 0        )
 , mReversible( true     )
 , mFast      ( false    )
 , mIsSetFast ( false    )
 , mCompartment ( "" )
 , mIsSetReversible (false)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();

  mReactants.setType( ListOfSpeciesReferences::Reactant );
  mProducts .setType( ListOfSpeciesReferences::Product  );
  mModifiers.setType( ListOfSpeciesReferences::Modifier );
  // before level 3 reversible and fast was set by default
  if (level < 3)
  {
    /* this changes existing behaviour as isSetFast already existed in L2 */
//    mIsSetFast = true;
    mIsSetReversible = true;
  }
}


Reaction::Reaction (SBMLNamespaces * sbmlns) :
   SBase      ( sbmlns   )
 , mId        ( ""       )
 , mName      ( ""       )
 , mKineticLaw( 0        )
 , mReversible( true     )
 , mFast      ( false    )
 , mIsSetFast ( false    )
 , mCompartment ( "" )
 , mIsSetReversible (false)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();

  mReactants.setType( ListOfSpeciesReferences::Reactant );
  mProducts .setType( ListOfSpeciesReferences::Product  );
  mModifiers.setType( ListOfSpeciesReferences::Modifier );
  // before level 3 reversible and fast was set by default
  if (sbmlns->getLevel() < 3)
  {
    /* this changes existing behaviour as isSetFast already existed in L2 */
//    mIsSetFast = true;
    mIsSetReversible = true;
  }
}


/** @cond doxygen-libsbml-internal */

/* constructor for validators */
Reaction::Reaction() :
  SBase()
{
}

/** @endcond */
                          
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
Reaction::Reaction (const Reaction& orig) :
    SBase      ( orig             )
  , mId        ( orig.mId         )  
  , mName      ( orig.mName       )
  , mReactants ( orig.mReactants  )
  , mProducts  ( orig.mProducts   )
  , mModifiers ( orig.mModifiers  )
  , mKineticLaw( 0                )
  , mReversible( orig.mReversible )
  , mFast      ( orig.mFast       )
  , mIsSetFast ( orig.mIsSetFast  )
  , mCompartment ( orig.mCompartment )
  , mIsSetReversible (orig.mIsSetReversible )
{
  /* since a reaction has children we need to re-establish the
   * parentage of these children
   */
  if (orig.getNumReactants() > 0)
  {
    mReactants.setParentSBMLObject(this);
  }
  if (orig.getNumProducts() > 0)
  {
    mProducts.setParentSBMLObject(this);
  }
  if (orig.getNumModifiers() > 0)
  {
    mModifiers.setParentSBMLObject(this);
  }

  if (orig.mKineticLaw)
  {
    mKineticLaw = static_cast<KineticLaw*>( orig.mKineticLaw->clone() );
    mKineticLaw->setParentSBMLObject(this);
  }
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
    mId = rhs.mId;
    mName = rhs.mName;
    mCompartment = rhs.mCompartment;
    mIsSetReversible = rhs.mIsSetReversible;

    if (rhs.getNumReactants() > 0)
    {
      mReactants.setParentSBMLObject(this);
    }
    if (rhs.getNumProducts() > 0)
    {
      mProducts.setParentSBMLObject(this);
    }
    if (rhs.getNumModifiers() > 0)
    {
      mModifiers.setParentSBMLObject(this);
    }

    delete mKineticLaw;
    if (rhs.mKineticLaw)
    {
      mKineticLaw = static_cast<KineticLaw*>( rhs.mKineticLaw->clone() );
      mKineticLaw->setParentSBMLObject(this);
    }
    else
    {
      mKineticLaw = 0;
    }
  }

  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next
 * Reaction (if available).
 */
bool
Reaction::accept (SBMLVisitor& v) const
{
  bool result = v.visit(*this);

  mReactants.accept(v);
  mProducts .accept(v);
  mModifiers.accept(v);

  if (mKineticLaw) mKineticLaw->accept(v);

  v.leave(*this);

  return result;
}


/*
 * @return a (deep) copy of this Reaction.
 */
Reaction*
Reaction::clone () const
{
  return new Reaction(*this);
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
  //// level 3 has no defaults
  //if (getLevel() < 3)
  //{
    setReversible(true);

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
  //}
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
 * @return true if the id of this SBML object has been set, false
 * otherwise.
 */
bool
Reaction::isSetId () const
{
  return (mId.empty() == false);
}


/*
 * @return true if the name of this SBML object has been set, false
 * otherwise.
 */
bool
Reaction::isSetName () const
{
  return (getLevel() == 1) ? (mId.empty() == false) : 
                            (mName.empty() == false);
}


/*
 * @return true if the KineticLaw of this Reaction has been set, false
 * otherwise.
 */
bool
Reaction::isSetKineticLaw () const
{
  return (mKineticLaw != 0);
}


/*
 * @return true if the fast status of this Reaction has been set, false
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
 * @return true if the compartment of this SBML object has been set, false
 * otherwise.
 */
bool
Reaction::isSetCompartment () const
{
  return (mCompartment.empty() == false);
}


/*
 * @return true if the fast status of this Reaction has been set, false
 * otherwise.
 */
bool
Reaction::isSetReversible () const
{
  return mIsSetReversible;
}


/*
 * Sets the id of this SBML object to a copy of sid.
 */
int
Reaction::setId (const std::string& sid)
{
  /* since the setId function has been used as an
   * alias for setName we cant require it to only
   * be used on a L2 model
   */
/*  if (getLevel() == 1)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
*/
  if (&(sid) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidSBMLSId(sid)))
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
  if (&(name) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (getLevel() == 1)
  {
    if (!(SyntaxChecker::isValidSBMLSId(name)))
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
  if (mKineticLaw == kl)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (kl == NULL)
  {
    delete mKineticLaw;
    mKineticLaw = 0;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (getLevel() != kl->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != kl->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else
  {
    delete mKineticLaw;
    mKineticLaw = static_cast<KineticLaw*>( kl->clone() );

    if (mKineticLaw) mKineticLaw->setSBMLDocument(mSBML);
    if (mKineticLaw) mKineticLaw->setParentSBMLObject(this);
    
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
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the fast status of this Reaction to value.
 */
int
Reaction::setFast (bool value)
{
  mFast      = value;
  mIsSetFast = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the compartment of this SBML object to a copy of sid.
 */
int
Reaction::setCompartment (const std::string& sid)
{
  if (&(sid) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (getLevel() < 3)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }

  if (!(SyntaxChecker::isValidSBMLSId(sid)))
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
  mKineticLaw = 0;

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
 * Adds a copy of the given reactant (SpeciesReference) to this Reaction.
 */
int
Reaction::addReactant (const SpeciesReference* sr)
{
  if (sr == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(sr->hasRequiredAttributes()) || !(sr->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sr->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sr->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (sr->isSetId() 
       && (getListOfReactants()->get(sr->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    /* if the ListOf is empty it doesnt know its parent */
    if (mReactants.size() == 0)
    {
      mReactants.setSBMLDocument(this->getSBMLDocument());
      mReactants.setParentSBMLObject(this);
    }

    mReactants.append(sr);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Adds a copy of the given product (SpeciesReference) to this Reaction.
 */
int
Reaction::addProduct (const SpeciesReference* sr)
{
  if (sr == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(sr->hasRequiredAttributes()) || !(sr->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sr->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sr->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (sr->isSetId() 
       && (getListOfProducts()->get(sr->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    /* if the ListOf is empty it doesnt know its parent */
    if (mProducts.size() == 0)
    {
      mProducts.setSBMLDocument(this->getSBMLDocument());
      mProducts.setParentSBMLObject(this);
    }

    mProducts.append(sr);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Adds a copy of the given modifier (ModifierSpeciesReference) to this
 * Reaction.
 */
int
Reaction::addModifier (const ModifierSpeciesReference* msr)
{
  if (msr == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (!(msr->hasRequiredAttributes()) || !(msr->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (msr->getLevel() < 2)
  {
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else if (getLevel() != msr->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != msr->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (msr->isSetId() 
       && (getListOfModifiers()->get(msr->getId())) != NULL)
  {
    // an object with this id already exists
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    /* if the ListOf is empty it doesnt know its parent */
    if (mModifiers.size() == 0)
    {
      mModifiers.setSBMLDocument(this->getSBMLDocument());
      mModifiers.setParentSBMLObject(this);
    }

    mModifiers.append(msr);

    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Creates a new SpeciesReference, adds it to this Reaction's list of
 * reactants and returns it.
 */
SpeciesReference*
Reaction::createReactant ()
{
  SpeciesReference* sr = 0;

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
  }
  
  /* if the ListOf is empty it doesnt know its parent */
  if (mReactants.size() == 0)
  {
    mReactants.setSBMLDocument(this->getSBMLDocument());
    mReactants.setParentSBMLObject(this);
  }

  if (sr) mReactants.appendAndOwn(sr);

  return sr;
}


/*
 * Creates a new SpeciesReference, adds it to this Reaction's list of
 * products and returns it.
 */
SpeciesReference*
Reaction::createProduct ()
{
  SpeciesReference* sr = 0;

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
  }

  /* if the ListOf is empty it doesnt know its parent */
  if (mProducts.size() == 0)
  {
    mProducts.setSBMLDocument(this->getSBMLDocument());
    mProducts.setParentSBMLObject(this);
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
  ModifierSpeciesReference* sr = 0;

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
  }
  
  /* if the ListOf is empty it doesnt know its parent */
  if (mModifiers.size() == 0)
  {
    mModifiers.setSBMLDocument(this->getSBMLDocument());
    mModifiers.setParentSBMLObject(this);
  }
  
  if (sr) mModifiers.appendAndOwn(sr);

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
  mKineticLaw = 0;

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
  }

  if (mKineticLaw)
  {
    mKineticLaw->setSBMLDocument(mSBML);
    mKineticLaw->setParentSBMLObject(this);
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
 * species or NULL if no such reactant exists.
 */
const SpeciesReference*
Reaction::getReactant (const std::string& species) const
{
  return
    static_cast<const SpeciesReference*>( GetSpeciesRef(mReactants, species) );
}


/*
 * @return the reactant (SpeciesReference) in this Reaction with the given
 * species or NULL if no such reactant exists.
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
 * species or NULL if no such product exists.
 */
const SpeciesReference*
Reaction::getProduct (const std::string& species) const
{
  return
    static_cast<const SpeciesReference*>( GetSpeciesRef(mProducts, species) );
}


/*
 * @return the product (SpeciesReference) in this Reaction with the given
 * species or NULL if no such product exists.
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
 * the given species or NULL if no such modifier exists.
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
 * the given species or NULL if no such modifier exists.
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
  return 0;
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
  return 0;
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
  return 0;
}


/** @cond doxygen-libsbml-internal */

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Reaction::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;

  mReactants.setSBMLDocument(d);
  mProducts .setSBMLDocument(d);
  mModifiers.setSBMLDocument(d);

  if (mKineticLaw) mKineticLaw->setSBMLDocument(d);
}


/**
  * Sets the parent SBML object of this SBML object.
  *
  * @param sb the SBML object to use
  */
void 
Reaction::setParentSBMLObject (SBase* sb)
{
  mParentSBMLObject = sb;
}
/** @endcond */


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
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
  * @li fast (in L3 only)
  * @li reversible (in L3 only)
  */

  if (!isSetId())
    allPresent = false;

  if (getLevel() > 2 && !isSetFast())
    allPresent = false;

  if (getLevel() > 2 && !isSetReversible())
    allPresent = false;

  return allPresent;
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
Reaction::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "listOfReactants")
  {
    if (mReactants.size() != 0)
    {
      if (getLevel() < 3)
        logError(NotSchemaConformant);
      else
        logError(OneSubElementPerReaction, getLevel(), getVersion());
    }
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
    object = &mModifiers;
  }
  else if (name == "kineticLaw")
  {
    if (mKineticLaw)
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


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Reaction::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
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


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Reaction::readL1Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("reversible");
  expectedAttributes.push_back("fast");

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    std::string prefix = attributes.getPrefix(i);
    // only check attributes in the sbml namespace   
    if (prefix.empty() || prefix == "sbml")
    {
      if (std::find(begin, end, name) == end)
      {
        logUnknownAttribute(name, level, version, "<reaction>");
      }
    }
  }

  //
  // name: SName  { use="required" }  (L1v1, L1v2)
  //
  bool assigned = attributes.readInto("name", mId, getErrorLog(), true);
  if (assigned && mId.size() == 0)
  {
    logEmptyString("name", level, version, "<reaction>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

  //
  // reversible: boolean  { use="optional"  default="true" }
  // (L1v1, L1v2, L2v1->)
  //
  attributes.readInto("reversible", mReversible);

  //
  // fast: boolean  { use="optional" default="false" }  (L1v1, L1v2)
  // fast: boolean  { use="optional" }                  (L2v1 ->)
  //
  mIsSetFast = attributes.readInto("fast", mFast);
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Reaction::readL2Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("reversible");
  expectedAttributes.push_back("fast");
  expectedAttributes.push_back("metaid");
  expectedAttributes.push_back("id");

  if (version > 1)
  {
    expectedAttributes.push_back("sboTerm");
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    std::string prefix = attributes.getPrefix(i);
    // only check attributes in the sbml namespace   
    if (prefix.empty() || prefix == "sbml")
    {
      if (std::find(begin, end, name) == end)
      {
        logUnknownAttribute(name, level, version, "<reaction>");
      }
    }
  }

  //
  //   id: SId    { use="required" }  (L2v1 ->)
  //
  bool assigned = attributes.readInto("id", mId, getErrorLog(), true);
  if (assigned && mId.size() == 0)
  {
    logEmptyString("id", level, version, "<reaction>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

  //
  // reversible: boolean  { use="optional"  default="true" }
  // (L1v1, L1v2, L2v1->)
  // reversible: boolean  { use="required"} (L3v1->)
  //
  attributes.readInto("reversible", mReversible);

  //
  // fast: boolean  { use="optional" }                  (L2v1 ->)
  //
  mIsSetFast = attributes.readInto("fast", mFast);

  //
  // name: string  { use="optional" }  (L2v1 ->)
  //
  attributes.readInto("name", mName);
  
  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2 ->)
  //
  if (version > 1) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);

}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Reaction::readL3Attributes (const XMLAttributes& attributes)
{
  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("reversible");
  expectedAttributes.push_back("fast");
  expectedAttributes.push_back("metaid");
  expectedAttributes.push_back("id");
  expectedAttributes.push_back("sboTerm");
  expectedAttributes.push_back("compartment");

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    std::string prefix = attributes.getPrefix(i);
    // only check attributes in the sbml namespace   
    if (prefix.empty() || prefix == "sbml")
    {
      if (std::find(begin, end, name) == end)
      {
        logUnknownAttribute(name, level, version, "<reaction>");
      }
    }
  }

  //
  //   id: SId    { use="required" }  (L2v1 ->)
  //
  bool assigned = attributes.readInto("id", mId, getErrorLog());
  if (!assigned)
  {
    getErrorLog()->logError(AllowedAttributesOnReaction, level, version);
  }
  if (assigned && mId.size() == 0)
  {
    logEmptyString("id", level, version, "<reaction>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mId)) logError(InvalidIdSyntax);

  //
  // reversible: boolean  { use="required"} (L3v1->)
  //
  mIsSetReversible = attributes.readInto("reversible", mReversible);
  if (!mIsSetReversible)
  {
    getErrorLog()->logError(AllowedAttributesOnReaction, level, version);
  }

  //
  // fast: boolean  { use="required" }                  (L3v1 ->)
  //
  mIsSetFast = attributes.readInto("fast", mFast);
  if (!mIsSetFast)
  {
    getErrorLog()->logError(AllowedAttributesOnReaction, level, version);
  }

  //
  // name: string  { use="optional" }  (L2v1 ->)
  //
  attributes.readInto("name", mName);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2 ->)
  //
  mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);

  //
  // compartment: string { use="optional" } (L3v1 -> )
  //
  assigned = attributes.readInto("compartment", mCompartment);
  if (assigned && mCompartment.size() == 0)
  {
    logEmptyString("compartment", level, version, "<reaction>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mCompartment)) logError(InvalidIdSyntax);
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Reaction::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

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

  //
  // reversible: boolean  { use="optional"  default="true" }
  // (L1v1, L1v2, L2v1-> )
  // reversible: boolean  { use="required"} (L3v1->)
  //
  if (level < 3)
  {
    if (mReversible != true) 
      stream.writeAttribute("reversible", mReversible);
  }
  else
  {
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
      if (level != 1 || mFast != false) stream.writeAttribute("fast", mFast);
    }
  }
  else
  {
    stream.writeAttribute("fast", mFast);
  }

  if (level > 2)
  {
    stream.writeAttribute("compartment", mCompartment);
  }

  if (level > 1)
  {
    //
    // sboTerm: SBOTerm { use="optional" }  (L2v2->)
    //
    if (!(level == 2 && version == 1)) 
      SBO::writeTerm(stream, mSBOTerm);
  }
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Reaction::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  const unsigned int level = getLevel();

  if (getNumReactants () > 0) mReactants.write(stream);
  if (getNumProducts  () > 0) mProducts .write(stream);

  if (level > 1 && getNumModifiers () > 0) mModifiers.write(stream);

  if (mKineticLaw) mKineticLaw->write(stream);
}
/** @endcond */


/*
 * @return a (deep) copy of this ListOfReactions.
 */
ListOfReactions*
ListOfReactions::clone () const
{
  return new ListOfReactions(*this);
}


/*
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
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


/**
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqR : public unary_function<SBase*, bool>
{
  const string& id;

  IdEqR (const string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <Reaction *> (sb)->getId() == id; }
};


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

  result = find_if( mItems.begin(), mItems.end(), IdEqR(sid) );
  return (result == mItems.end()) ? 0 : static_cast <Reaction*> (*result);
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
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEqR(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <Reaction*> (item);
}


/** @cond doxygen-libsbml-internal */
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


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfReactions::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;

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
    
    if (object) mItems.push_back(object);
  }

  return object;
}
/** @endcond */


/**
 * Creates a new Reaction_t structure using the given SBML @p level
 * and @p version values.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Reaction
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Reaction
 *
 * @return a pointer to the newly created Reaction_t structure.
 *
 * @note Once a Reaction has been added to an SBMLDocument, the @p
 * level and @p version for the document @em override those used to create
 * the Reaction.  Despite this, the ability to supply the values at
 * creation time is an important aid to creating valid SBML.  Knowledge of
 * the intended SBML Level and Version  determine whether it is valid to
 * assign a particular value to an attribute, or whether it is valid to add
 * an object to an existing SBMLDocument.
 */
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


/**
 * Creates a new Reaction_t structure using the given
 * SBMLNamespaces_t structure.
 *
 * @param sbmlns SBMLNamespaces, a pointer to an SBMLNamespaces structure
 * to assign to this Reaction
 *
 * @return a pointer to the newly created Reaction_t structure.
 *
 * @note Once a Reaction has been added to an SBMLDocument, the
 * @p sbmlns namespaces for the document @em override those used to create
 * the Reaction.  Despite this, the ability to supply the values at creation 
 * time is an important aid to creating valid SBML.  Knowledge of the intended 
 * SBML Level and Version determine whether it is valid to assign a particular 
 * value to an attribute, or whether it is valid to add an object to an 
 * existing SBMLDocument.
 */
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

/**
 * Frees the given Reaction.
 */
LIBSBML_EXTERN
void
Reaction_free (Reaction_t *r)
{
  delete r;
}


/**
 * @return a (deep) copy of this Reaction.
 */
LIBSBML_EXTERN
Reaction_t *
Reaction_clone (const Reaction_t *r)
{
  return static_cast<Reaction*>( r->clone() );
}


/**
 * Initializes the fields of this Reaction to their defaults:
 *
 *   - reversible = 1 (true)
 *   - fast       = 0 (false)  (L1 only)
 */
LIBSBML_EXTERN
void
Reaction_initDefaults (Reaction_t *r)
{
  r->initDefaults();
}


/**
 * Returns a list of XMLNamespaces_t associated with this Reaction_t
 * structure.
 *
 * @param r the Reaction_t structure
 * 
 * @return pointer to the XMLNamespaces_t structure associated with 
 * this SBML object
 */
LIBSBML_EXTERN
const XMLNamespaces_t *
Reaction_getNamespaces(Reaction_t *r)
{
  return r->getNamespaces();
}

/**
 * @return the id of this Reaction.
 */
LIBSBML_EXTERN
const char *
Reaction_getId (const Reaction_t *r)
{
  return r->isSetId() ? r->getId().c_str() : NULL;
}


/**
 * @return the name of this Reaction.
 */
LIBSBML_EXTERN
const char *
Reaction_getName (const Reaction_t *r)
{
  return r->isSetName() ? r->getName().c_str() : NULL;
}


/**
 * @return the KineticLaw of this Reaction.
 */
LIBSBML_EXTERN
KineticLaw_t *
Reaction_getKineticLaw (Reaction_t *r)
{
  return r->getKineticLaw();
}


/**
 * @return the reversible status of this Reaction.
 */
LIBSBML_EXTERN
int
Reaction_getReversible (const Reaction_t *r)
{
  return static_cast<int>( r->getReversible() );
}


/**
 * @return the fast status of this Reaction.
 */
LIBSBML_EXTERN
int
Reaction_getFast (const Reaction_t *r)
{
  return static_cast<int>( r->getFast() );
}


/**
 * @return the compartment of this Reaction.
 */
LIBSBML_EXTERN
const char *
Reaction_getCompartment (const Reaction_t *r)
{
  return r->isSetCompartment() ? r->getCompartment().c_str() : NULL;
}


/**
 * @return true (non-zero) if the id of this Reaction has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Reaction_isSetId (const Reaction_t *r)
{
  return static_cast<int>( r->isSetId() );
}


/**
 * @return true (non-zero) if the name of this Reaction has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Reaction_isSetName (const Reaction_t *r)
{
  return static_cast<int>( r->isSetName() );
}


/**
 * @return true (non-zero) if the KineticLaw of this Reaction has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Reaction_isSetKineticLaw (const Reaction_t *r)
{
  return static_cast<int>( r->isSetKineticLaw() );
}


/**
 * @return true (non-zero) if the fast status of this Reaction has been set,
 * false (0) otherwise.
 *
 * In L1, fast is optional with a default of false, which means it is
 * effectively always set.  In L2, however, fast is optional with no
 * default value, so it may or may not be set to a specific value.
 */
LIBSBML_EXTERN
int
Reaction_isSetFast (const Reaction_t *r)
{
  return static_cast<int>( r->isSetFast() );
}


/**
 * @return true (non-zero) if the KineticLaw of this Reaction has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Reaction_isSetCompartment (const Reaction_t *r)
{
  return static_cast<int>( r->isSetCompartment() );
}


/**
 * @return true (non-zero) if the reversible attribute of this Reaction has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Reaction_isSetReversible (const Reaction_t *r)
{
  return static_cast<int>( r->isSetReversible() );
}


/**
 * Sets the id of this Reaction to a copy of sid.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 *
 * @note Using this function with an id of NULL is equivalent to
 * unsetting the "id" attribute.
 */
LIBSBML_EXTERN
int
Reaction_setId (Reaction_t *r, const char *sid)
{
  return (sid == NULL) ? r->setId("") : r->setId(sid);
}


/**
 * Sets the name of this Reaction to a copy of name.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 *
 * @note Using this function with the name set to NULL is equivalent to
 * unsetting the "name" attribute.
 */
LIBSBML_EXTERN
int
Reaction_setName (Reaction_t *r, const char *name)
{
  return (name == NULL) ? r->unsetName() : r->setName(name);
}


/**
 * Sets the KineticLaw of this Reaction to a copy of the given KineticLaw.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 */
LIBSBML_EXTERN
int
Reaction_setKineticLaw (Reaction_t *r, const KineticLaw_t *kl)
{
  return (kl == NULL) ? r->unsetKineticLaw() : r->setKineticLaw(kl);
}


/**
 * Sets the reversible status of this Reaction to value (boolean).
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
Reaction_setReversible (Reaction_t *r, int value)
{
  return r->setReversible( static_cast<bool>(value) );
}


/**
 * Sets the fast status of this Reaction to value (boolean).
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
Reaction_setFast (Reaction_t *r, int value)
{
  return r->setFast( static_cast<bool>(value) );
}


/**
 * Sets the compartment of this Reaction to a copy of compartment.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 *
 * @note Using this function with the compartment set to NULL is equivalent to
 * unsetting the "compartment" attribute.
 */
LIBSBML_EXTERN
int
Reaction_setCompartment (Reaction_t *r, const char *compartment)
{
  return (compartment == NULL) ? r->unsetCompartment() : 
                                 r->setCompartment(compartment);
}


/**
 * Unsets the name of this Reaction.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Reaction_unsetName (Reaction_t *r)
{
  return r->unsetName();
}


/**
 * Unsets the compartment of this Reaction.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Reaction_unsetCompartment (Reaction_t *r)
{
  return r->unsetCompartment();
}


/**
 * Unsets the KineticLaw of this Reaction.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Reaction_unsetKineticLaw (Reaction_t *r)
{
  return r->unsetKineticLaw();
}


/**
 * Unsets the fast status of this Reation.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_OPERATION_FAILED
 *
 * In L1, fast is optional with a default of false, which means it is
 * effectively always set.  In L2, however, fast is optional with no
 * default value, so it may or may not be set to a specific value.
 */
LIBSBML_EXTERN
int
Reaction_unsetFast (Reaction_t *r)
{
  return r->unsetFast();
}


/**
  * Predicate returning @c true or @c false depending on whether
  * all the required attributes for this Reaction object
  * have been set.
  *
 * @param r the Reaction_t structure to check.
 *
  * @note The required attributes for a Reaction object are:
  * @li id (name in L1)
  * @li fast (in L3 only)
  * @li reversible (in L3 only)
  *
  * @return a true if all the required
  * attributes for this object have been defined, false otherwise.
  */
LIBSBML_EXTERN
int
Reaction_hasRequiredAttributes(Reaction_t *r)
{
  return static_cast<int>(r->hasRequiredAttributes());
}


/**
 * Adds a copy of the given reactant (SpeciesReference) to this Reaction.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Reaction_addReactant (Reaction_t *r, const SpeciesReference_t *sr)
{
  return r->addReactant( static_cast<const SpeciesReference*>(sr) );
}


/**
 * Adds a copy of the given product (SpeciesReference) to this Reaction.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Reaction_addProduct (Reaction_t *r, const SpeciesReference_t *sr)
{
  return r->addProduct( static_cast<const SpeciesReference*>(sr) );
}


/**
 * Adds a copy of the given modifier (ModifierSpeciesReference) to this
 * Reaction.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 * @li LIBSBML_OPERATION_FAILED
 */
LIBSBML_EXTERN
int
Reaction_addModifier (Reaction_t *r, const SpeciesReference_t *msr)
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


/**
 * Creates a new SpeciesReference, adds it to this Reaction's list of
 * reactants and returns it.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_createReactant (Reaction_t *r)
{
  return r->createReactant();
}


/**
 * Creates a new SpeciesReference, adds it to this Reaction's list of
 * products and returns it.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_createProduct (Reaction_t *r)
{
  return r->createProduct();
}


/**
 * Creates a new SpeciesReference, adds it to this Reaction's list of
 * modifiers and returns it.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_createModifier (Reaction_t *r)
{
  return r->createModifier();
}


/**
 * Creates a new KineticLaw for this Reaction and returns it.  If this
 * Reaction had a previous KineticLaw, it will be destroyed.
 */
LIBSBML_EXTERN
KineticLaw_t *
Reaction_createKineticLaw (Reaction_t *r)
{
  return r->createKineticLaw();
}


/**
 * @return the list of Reactants for this Reaction.
 */
LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfReactants (Reaction_t *r)
{
  return r->getListOfReactants();
}


/**
 * @return the list of Products for this Reaction.
 */
LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfProducts (Reaction_t *r)
{
  return r->getListOfProducts();
}


/**
 * @return the list of Modifiers for this Reaction.
 */
LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfModifiers (Reaction_t *r)
{
  return r->getListOfModifiers();
}


/**
 * @return the nth reactant (SpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getReactant (Reaction_t *r, unsigned int n)
{
  return r->getReactant(n);
}


/**
 * @return the reactant (SpeciesReference) in this Reaction with the given
 * species or NULL if no such reactant exists.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getReactantBySpecies (Reaction_t *r, const char *species)
{
  return (species != NULL) ? r->getReactant(species) : NULL;
}


/**
 * @return the nth product (SpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getProduct (Reaction_t *r, unsigned int n)
{
  return r->getProduct(n);
}


/**
 * @return the product (SpeciesReference) in this Reaction with the given
 * species or NULL if no such product exists.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getProductBySpecies (Reaction_t *r, const char *species)
{
  return (species != NULL) ? r->getProduct(species) : NULL;
}


/**
 * @return the nth modifier (ModifierSpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getModifier (Reaction_t *r, unsigned int n)
{
  return r->getModifier(n);
}


/**
 * @return the modifier (ModifierSpeciesReference) in this Reaction with
 * the given species or NULL if no such modifier exists.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getModifierBySpecies (Reaction_t *r, const char *species)
{
  return (species != NULL) ? r->getModifier(species) : NULL;
}

/**
 * @return the number of reactants (SpeciesReferences) in this Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction_getNumReactants (const Reaction_t *r)
{
  return r->getNumReactants();
}


/**
 * @return the number of products (SpeciesReferences) in this Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction_getNumProducts (const Reaction_t *r)
{
  return r->getNumProducts();
}


/**
 * @return the number of modifiers (ModifierSpeciesReferences) in this
 * Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction_getNumModifiers (const Reaction_t *r)
{
  return r->getNumModifiers();
}


/**
 * Removes the nth reactant SpeciesReference_t object from this 
 * Reaction_t object and returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param r the Reaction_t structure
 * @param n the integer index of the reactant SpeciesReference_t to remove
 *
 * @return the reactant SpeciesReference_t object removed.  As mentioned 
 * above, the caller owns the returned object. NULL is returned if the 
 * given index is out of range.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeReactant (Reaction_t *r, unsigned int n)
{
  if (!r) return 0;
  return r->removeReactant(n);
}


/**
 * Removes the reactant SpeciesReference_t object with the given 
 * "species" attribute from this Reaction_t object and returns a pointer
 * to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param r the Reaction_t structure
 * @param species the "species" attribute of the reactant SpeciesReference_t 
 * to remove
 *
 * @return the reactant SpeciesReference_t object removed.  As mentioned 
 * above, the caller owns the returned object. NULL is returned if no 
 * reactant SpeciesReference_t object with the "species" attribute exists 
 * in this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeReactantBySpecies (Reaction_t *r, const char *species)
{
  if (!r) return 0;
  return r->removeReactant(species);
}


/**
 * Removes the nth product SpeciesReference_t object from this 
 * Reaction_t object and returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param r the Reaction_t structure
 * @param n the integer index of the product SpeciesReference_t to remove
 *
 * @return the product SpeciesReference_t object removed.  As mentioned 
 * above, the caller owns the returned object. NULL is returned if the 
 * given index is out of range.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeProduct (Reaction_t *r, unsigned int n)
{
  if (!r) return 0;
  return r->removeProduct(n);
}


/**
 * Removes the product SpeciesReference_t object with the given 
 * "species" attribute from this Reaction_t object and returns a pointer
 * to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param r the Reaction_t structure
 * @param species the "species" attribute of the product SpeciesReference_t 
 * to remove
 *
 * @return the product SpeciesReference_t object removed.  As mentioned 
 * above, the caller owns the returned object. NULL is returned if no 
 * product SpeciesReference_t object with the "species" attribute exists 
 * in this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeProductBySpecies (Reaction_t *r, const char *species)
{
  if (!r) return 0;
  return r->removeProduct(species);
}


/**
 * Removes the nth modifier SpeciesReference_t object from this 
 * Reaction_t object and returns a pointer to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param r the Reaction_t structure
 * @param n the integer index of the modifier SpeciesReference_t to remove
 *
 * @return the modifier SpeciesReference_t object removed.  As mentioned 
 * above, the caller owns the returned object. NULL is returned if the 
 * given index is out of range.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeModifier (Reaction_t *r, unsigned int n)
{
  if (!r) return 0;
  return r->removeModifier(n);
}


/**
 * Removes the modifier SpeciesReference_t object with the given 
 * "species" attribute from this Reaction_t object and returns a pointer
 * to it.
 *
 * The caller owns the returned object and is responsible for deleting it.
 *
 * @param r the Reaction_t structure
 * @param species the "species" attribute of the modifier SpeciesReference_t 
 * to remove
 *
 * @return the modifier SpeciesReference_t object removed.  As mentioned 
 * above, the caller owns the returned object. NULL is returned if no 
 * modifier SpeciesReference_t object with the "species" attribute exists 
 * in this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_removeModifierBySpecies (Reaction_t *r, const char *species)
{
  if (!r) return 0;
  return r->removeModifier(species);
}


/**
 * @return item in this ListOfReaction with the given id or NULL if no such
 * item exists.
 */
LIBSBML_EXTERN
Reaction_t *
ListOfReactions_getById (ListOf_t *lo, const char *sid)
{
  return (sid != NULL) ? 
    static_cast <ListOfReactions *> (lo)->get(sid) : NULL;
}


/**
 * Removes item in this ListOf items with the given id or NULL if no such
 * item exists.  The caller owns the returned item and is responsible for
 * deleting it.
 */
LIBSBML_EXTERN
Reaction_t *
ListOfReactions_removeById (ListOf_t *lo, const char *sid)
{
  return (sid != NULL) ? 
    static_cast <ListOfReactions *> (lo)->remove(sid) : NULL;
}

LIBSBML_CPP_NAMESPACE_END
