/**
 * @file    Reaction.cpp
 * @brief   Implementations of Reaction and ListOfReactions.
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
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

/** @endcond doxygen-ignored */

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



/*
 * Creates a new Reaction, optionally with its id, KineticLaw, and
 * reversible attributes set.
 */
Reaction::Reaction (const std::string& id, const std::string& name, 
                    const KineticLaw* kl, bool reversible) :
    SBase      ( id , name, -1        )
  , mKineticLaw( 0          )
  , mReversible( reversible )
  , mFast      ( false      )
  , mIsSetFast ( false      )
{
  if (kl) mKineticLaw = static_cast<KineticLaw*>( kl->clone() );

  mReactants.setType( ListOfSpeciesReferences::Reactant );
  mProducts .setType( ListOfSpeciesReferences::Product  );
  mModifiers.setType( ListOfSpeciesReferences::Modifier );
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
Reaction::Reaction (const Reaction& orig) :
    SBase      ( orig             )
  , mReactants ( orig.mReactants  )
  , mProducts  ( orig.mProducts   )
  , mModifiers ( orig.mModifiers  )
  , mKineticLaw( 0                )
  , mReversible( orig.mReversible )
  , mFast      ( orig.mFast       )
  , mIsSetFast ( orig.mIsSetFast  )
{
  if (orig.mKineticLaw)
  {
    mKineticLaw = static_cast<KineticLaw*>( orig.mKineticLaw->clone() );
  }
}


/*
 * Assignment operator.
 */
Reaction& Reaction::operator=(const Reaction& rhs)
{
  this->SBase::operator =(rhs);
 mReversible = rhs.mReversible ;
 mFast       = rhs.mFast       ;
 mIsSetFast  = rhs.mIsSetFast  ;
 mReactants  = rhs.mReactants  ;
 mProducts   = rhs.mProducts   ;
 mModifiers  = rhs.mModifiers  ;
  if (rhs.mKineticLaw)
  {
    mKineticLaw = static_cast<KineticLaw*>( rhs.mKineticLaw->clone() );
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
SBase*
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
  mFast      = false;
  mIsSetFast = false;
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
 * Sets the KineticLaw of this Reaction to a copy of the given KineticLaw.
 */
void
Reaction::setKineticLaw (const KineticLaw* kl)
{
  if (mKineticLaw == kl) return;

  delete mKineticLaw;
  mKineticLaw = (kl != 0) ? static_cast<KineticLaw*>( kl->clone() ) : 0;

  if (mKineticLaw) mKineticLaw->setSBMLDocument(mSBML);
  if (mKineticLaw) mKineticLaw->setParentSBMLObject(this);
}


/*
 * Sets the reversible status of this Reaction to value.
 */
void
Reaction::setReversible (bool value)
{
  mReversible = value;
}


/*
 * Sets the fast status of this Reaction to value.
 */
void
Reaction::setFast (bool value)
{
  mFast      = value;
  mIsSetFast = true;
}


/*
 * Unsets the KineticLaw of this Reaction.
 */
void
Reaction::unsetKineticLaw ()
{
  delete mKineticLaw;
  mKineticLaw = 0;
}


/*
 * Unsets the fast status of this Reaction.
 *
 * In L1, fast is optional with a default of false, which means it is
 * effectively always set.  In L2, however, fast is optional with no
 * default value, so it may or may not be set to a specific value.
 */
void
Reaction::unsetFast ()
{
  mIsSetFast = false;
}


/*
 * Adds a copy of the given reactant (SpeciesReference) to this Reaction.
 */
void
Reaction::addReactant (const SpeciesReference* sr)
{
  /* if the ListOf is empty it doesnt know its parent */
  if (mReactants.size() == 0)
  {
    mReactants.setSBMLDocument(this->getSBMLDocument());
    mReactants.setParentSBMLObject(this);
  }

  mReactants.append(sr);
}


/*
 * Adds a copy of the given product (SpeciesReference) to this Reaction.
 */
void
Reaction::addProduct (const SpeciesReference* sr)
{
  /* if the ListOf is empty it doesnt know its parent */
  if (mProducts.size() == 0)
  {
    mProducts.setSBMLDocument(this->getSBMLDocument());
    mProducts.setParentSBMLObject(this);
  }

  mProducts.append(sr);
}


/*
 * Adds a copy of the given modifier (ModifierSpeciesReference) to this
 * Reaction.
 */
void
Reaction::addModifier (const ModifierSpeciesReference* msr)
{
  /* if the ListOf is empty it doesnt know its parent */
  if (mModifiers.size() == 0)
  {
    mModifiers.setSBMLDocument(this->getSBMLDocument());
    mModifiers.setParentSBMLObject(this);
  }

  mModifiers.append(msr);
}


/*
 * Creates a new SpeciesReference, adds it to this Reaction's list of
 * reactants and returns it.
 */
SpeciesReference*
Reaction::createReactant ()
{
  SpeciesReference* species = new SpeciesReference;
  
  /* if the ListOf is empty it doesnt know its parent */
  if (mReactants.size() == 0)
  {
    mReactants.setSBMLDocument(this->getSBMLDocument());
    mReactants.setParentSBMLObject(this);
  }

  mReactants.appendAndOwn(species);

  return species;
}


/*
 * Creates a new SpeciesReference, adds it to this Reaction's list of
 * products and returns it.
 */
SpeciesReference*
Reaction::createProduct ()
{
  SpeciesReference* species = new SpeciesReference;
  
  /* if the ListOf is empty it doesnt know its parent */
  if (mProducts.size() == 0)
  {
    mProducts.setSBMLDocument(this->getSBMLDocument());
    mProducts.setParentSBMLObject(this);
  }
  
  mProducts.appendAndOwn(species);

  return species;
}


/*
 * Creates a new ModifierSpeciesReference, adds it to this Reaction's
 * list of modifiers and returns it.
 */
ModifierSpeciesReference*
Reaction::createModifier ()
{
  ModifierSpeciesReference* species = new ModifierSpeciesReference;
  
  /* if the ListOf is empty it doesnt know its parent */
  if (mModifiers.size() == 0)
  {
    mModifiers.setSBMLDocument(this->getSBMLDocument());
    mModifiers.setParentSBMLObject(this);
  }
  
  mModifiers.appendAndOwn(species);

  return species;
}


/*
 * Creates a new KineticLaw for this Reaction and returns it.  If this
 * Reaction had a previous KineticLaw, it will be destroyed.
 */
KineticLaw*
Reaction::createKineticLaw ()
{
  delete mKineticLaw;
  mKineticLaw = new KineticLaw;

  mKineticLaw->setSBMLDocument(mSBML);
  mKineticLaw->setParentSBMLObject(this);

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
      logError(NotSchemaConformant);
    }
    object = &mReactants;
  }
  else if (name == "listOfProducts")
  {
    if (mProducts.size() != 0)
    {
      logError(NotSchemaConformant);
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
      logError(NotSchemaConformant);
    }
    object = &mModifiers;
  }
  else if (name == "kineticLaw")
  {
    if (mKineticLaw)
    {
      logError(NotSchemaConformant);
    }
    delete mKineticLaw;

    mKineticLaw = new KineticLaw();
    object      = mKineticLaw;
  }

  return object;
}
/** @endcond doxygen-libsbml-internal */


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
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("name");
  expectedAttributes.push_back("reversible");
  expectedAttributes.push_back("fast");

  if (level == 2)
  {
    expectedAttributes.push_back("metaid");
    expectedAttributes.push_back("id");

    if (version != 1)
    {
      expectedAttributes.push_back("sboTerm");
    }
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      logUnknownAttribute(name, level, version, "<reaction>");
    }
  }

  //
  // name: SName  { use="required" }  (L1v1, L1v2)
  //   id: SId    { use="required" }  (L2v1, L2v2)
  //
  const string id = (level == 1) ? "name" : "id";
  attributes.readInto(id, mId, getErrorLog(), true);
  SBase::checkIdSyntax();

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 2) attributes.readInto("name", mName);

  //
  // reversible: boolean  { use="optional"  default="true" }
  // (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("reversible", mReversible);

  //
  // fast: boolean  { use="optional" default="false" }  (L1v1, L1v2)
  // fast: boolean  { use="optional" }                  (L2v1, L2v2)
  //
  mIsSetFast = attributes.readInto("fast", mFast);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());
}
/** @endcond doxygen-libsbml-internal */


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
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 2) stream.writeAttribute("name", mName);

  //
  // reversible: boolean  { use="optional"  default="true" }
  // (L1v1, L1v2, L2v1, L2v2)
  //
  if (mReversible != true) stream.writeAttribute("reversible", mReversible);

  //
  // fast: boolean  { use="optional" default="false" }  (L1v1, L1v2)
  // fast: boolean  { use="optional" }                  (L2v1, L2v2)
  //
  if (mIsSetFast)
  {
    if (level != 1 || mFast != false) stream.writeAttribute("fast", mFast);
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    SBO::writeTerm(stream, mSBOTerm);
}
/** @endcond doxygen-libsbml-internal */


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

  if (level == 2 && getNumModifiers () > 0) mModifiers.write(stream);

  if (mKineticLaw) mKineticLaw->write(stream);
}
/** @endcond doxygen-libsbml-internal */


/*
 * @return a (deep) copy of this ListOfReactions.
 */
SBase*
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
/** @endcond doxygen-libsbml-internal */


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
    object = new Reaction();
    mItems.push_back(object);
  }

  return object;
}
/** @endcond doxygen-libsbml-internal */



/**
 * Creates a new Reaction and returns a pointer to it.
 */
LIBSBML_EXTERN
Reaction_t *
Reaction_create ()
{
  return new(nothrow) Reaction;
}


/**
 * Creates a new Reaction_t structure with the given identifier and returns a
 * pointer to it.
 *
 * @param sid a string, the identifier to assign to this Reaction_t structure
 * @param name a string, the name to give this Reaction_t structure
 *
 * @return the Reaction_t structure created
 */
LIBSBML_EXTERN
Reaction_t *
Reaction_createWith (const char *sid, const char * name)
{
  return new(nothrow) Reaction(sid ? sid : "", name ? name : "");
}


/**
 * Creates a new Reaction with the given id, KineticLaw, reversible and
 * fast and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 *
 *   Reaction_t *r = Reaction_create();
 *   Reaction_setId(r, sid); Reaction_setKineticLaw(r, kl); ...;
 */
LIBSBML_EXTERN
Reaction_t *
Reaction_createWithKineticLaw ( const char   *sid,
                               const char *name,
                      KineticLaw_t *kl,
                      int          reversible,
                      int          fast )
{
  KineticLaw* k = static_cast<KineticLaw*>(kl);
  Reaction*   r = new(nothrow) Reaction(sid ? sid : "", name ? name : "", k, reversible);


  r->setFast(fast);


  return r;
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
 * Sets the id of this Reaction to a copy of sid.
 */
LIBSBML_EXTERN
void
Reaction_setId (Reaction_t *r, const char *sid)
{
  (sid == NULL) ? r->unsetId() : r->setId(sid);
}


/**
 * Sets the name of this Reaction to a copy of name.
 */
LIBSBML_EXTERN
void
Reaction_setName (Reaction_t *r, const char *name)
{
  (name == NULL) ? r->unsetName() : r->setName(name);
}


/**
 * Sets the KineticLaw of this Reaction to a copy of the given KineticLaw.
 */
LIBSBML_EXTERN
void
Reaction_setKineticLaw (Reaction_t *r, const KineticLaw_t *kl)
{
  (kl == NULL) ? r->unsetKineticLaw() : r->setKineticLaw(kl);
}


/**
 * Sets the reversible status of this Reaction to value (boolean).
 */
LIBSBML_EXTERN
void
Reaction_setReversible (Reaction_t *r, int value)
{
  r->setReversible( static_cast<bool>(value) );
}


/**
 * Sets the fast status of this Reaction to value (boolean).
 */
LIBSBML_EXTERN
void
Reaction_setFast (Reaction_t *r, int value)
{
  r->setFast( static_cast<bool>(value) );
}


/**
 * Unsets the name of this Reaction.
 */
LIBSBML_EXTERN
void
Reaction_unsetName (Reaction_t *r)
{
  r->unsetName();
}


/**
 * Unsets the KineticLaw of this Reaction.
 */
LIBSBML_EXTERN
void
Reaction_unsetKineticLaw (Reaction_t *r)
{
  r->unsetKineticLaw();
}


/**
 * Unsets the fast status of this Reation.
 *
 * In L1, fast is optional with a default of false, which means it is
 * effectively always set.  In L2, however, fast is optional with no
 * default value, so it may or may not be set to a specific value.
 */
LIBSBML_EXTERN
void
Reaction_unsetFast (Reaction_t *r)
{
  r->unsetFast();
}


/**
 * Adds a copy of the given reactant (SpeciesReference) to this Reaction.
 */
LIBSBML_EXTERN
void
Reaction_addReactant (Reaction_t *r, const SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    r->addReactant( static_cast<const SpeciesReference*>(sr) );
  }
}


/**
 * Adds a copy of the given product (SpeciesReference) to this Reaction.
 */
LIBSBML_EXTERN
void
Reaction_addProduct (Reaction_t *r, const SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    r->addProduct( static_cast<const SpeciesReference*>(sr) );
  }
}


/**
 * Adds a copy of the given modifier (ModifierSpeciesReference) to this
 * Reaction.
 */
LIBSBML_EXTERN
void
Reaction_addModifier (Reaction_t *r, const SpeciesReference_t *msr)
{
  if (msr != NULL && msr->isModifier())
  {
    r->addModifier( static_cast<const ModifierSpeciesReference*>(msr) );
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


