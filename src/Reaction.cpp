/**
 * Filename    : Reaction.cpp
 * Description : SBML Reaction
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-25
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/SpeciesReference.hpp"
#include "sbml/ModifierSpeciesReference.hpp"
#include "sbml/KineticLaw.hpp"
#include "sbml/SBMLVisitor.hpp"

#include "sbml/Reaction.h"
#include "sbml/Reaction.hpp"


/**
 * Creates a new Reaction, optionally with its id, KineticLaw, and
 * reversible attributes set.
 */
LIBSBML_EXTERN
Reaction::Reaction (   const std::string&  id
                     , KineticLaw*         kl
                     , bool                reversible ) :
    SBase()
  , id        ( id         )
  , kineticLaw( kl         )
  , reversible( reversible )
{
  init(SBML_REACTION);

  // See comment block in initDefaults() for rationale.
  fast       = false;
  isSet.fast = 0;
}


/**
 * Destroys this Reaction.
 */
LIBSBML_EXTERN
Reaction::~Reaction ()
{
  delete kineticLaw;
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Model's next
 * Reaction (if available).
 */
LIBSBML_EXTERN
bool
Reaction::accept (SBMLVisitor& v) const
{
  unsigned int n;
  bool next, result;


  result = v.visit(*this);

  //
  // Reactant
  //

  getListOfReactants().accept(v, SBML_SPECIES_REFERENCE);

  for (n = 0, next = true; n < getNumReactants() && next; n++)
  {
    next = getReactant(n)->accept(v);
  }

  v.leave(getListOfReactants(), SBML_SPECIES_REFERENCE);


  //
  // Product
  //

  getListOfProducts().accept(v, SBML_SPECIES_REFERENCE);

  for (n = 0, next = true; n < getNumProducts() && next; n++)
  {
    next = getProduct(n)->accept(v);
  }

  v.leave(getListOfProducts(), SBML_SPECIES_REFERENCE);


  //
  // Modifier
  //

  getListOfModifiers().accept(v, SBML_MODIFIER_SPECIES_REFERENCE);

  for (n = 0, next = true; n < getNumModifiers() && next; n++)
  {
    next = getModifier(n)->accept(v);
  }

  v.leave(getListOfModifiers(), SBML_MODIFIER_SPECIES_REFERENCE);

  if (kineticLaw != NULL) kineticLaw->accept(v);

  v.leave(*this);

  return result;
}


/**
 * Initializes the fields of this Reaction to their defaults:
 *
 *   - reversible = true
 *   - fast       = false  (L1 only)
 */
LIBSBML_EXTERN
void
Reaction::initDefaults ()
{
  setReversible(true);

  /**
   * Set fast explicitly and make sure isSet.fast is false.  This preserves
   * backward compatibility with L1 where fast defaulted to false and such
   * Reaction_isSetFast() was not available.  E.g.:
   *
   *   Level 1                          Level 2
   *   ---------------------------      -------------------------------
   *   r = Reaction_create();           r = Reaction_create();
   *   Reaction_getFast(r)   == false;  Reaction_getFast(r)   == false, but
   *   Reaction_isSetFast(r) == N/A     Reaction_isSetFast(r) == 0
   */
  fast       = false;
  isSet.fast = 0;
}


/**
 * @return the id of this Reaction.
 */
LIBSBML_EXTERN
const std::string&
Reaction::getId () const
{
  return id;
}


/**
 * @return the name of this Reaction.
 */
LIBSBML_EXTERN
const std::string&
Reaction::getName () const
{
  return name;
}


/**
 * @return the KineticLaw of this Reaction.
 */
LIBSBML_EXTERN
KineticLaw*
Reaction::getKineticLaw () const
{
  return kineticLaw;
}


/**
 * @return the reversible status of this Reaction.
 */
LIBSBML_EXTERN
bool
Reaction::getReversible () const
{
  return reversible;
}


/**
 * @return the fast status of this Reaction.
 */
LIBSBML_EXTERN
bool
Reaction::getFast () const
{
  return fast;
}


/**
 * @return true if the id of this Reaction has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Reaction::isSetId () const
{
  return ! id.empty();
}


/**
 * @return true if the name of this Reaction has been set, false otherwise.
 *
 * In SBML L1, a Reaction name is required and therefore <b>should always
 * be set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
bool
Reaction::isSetName () const
{
  return ! name.empty();
}


/**
 * @return true if the KineticLaw of this Reaction has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Reaction::isSetKineticLaw () const
{
  return (kineticLaw != NULL);
}


/**
 * @return true if the fast status of this Reation has been set, false
 * otherwise.
 *
 * In L1, fast is optional with a default of false, which means it is
 * effectively always set.  In L2, however, fast is optional with no
 * default value, so it may or may not be set to a specific value.
 */
LIBSBML_EXTERN
bool
Reaction::isSetFast () const
{
  return isSet.fast;
}


/**
 * Moves the id field of this Reaction to its name field (iff name is not
 * already set).  This method is used for converting from L2 to L1.
 */
LIBSBML_EXTERN
void
Reaction::moveIdToName ()
{
  if ( isSetName() ) return;

  setName( getId() );
  setId  ( "" );
}


/**
 * Moves the name field of this Reaction to its id field (iff id is not
 * already set).  This method is used for converting from L1 to L2.
 */
LIBSBML_EXTERN
void
Reaction::moveNameToId ()
{
  if ( isSetId() ) return;

  setId  ( getName() );
  setName( "" );
}


/**
 * Sets the id of this Reaction to a copy of sid.
 */
LIBSBML_EXTERN
void
Reaction::setId (const std::string& sid)
{
  id = sid;
}


/**
 * Sets the name of this Reaction to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Reaction::setName (const std::string& string)
{
  name = string;
}


/**
 * Sets the KineticLaw of this Reaction to the given KineticLaw.
 */
LIBSBML_EXTERN
void
Reaction::setKineticLaw (KineticLaw& kl)
{
  kineticLaw = &kl;
}


/**
 * Sets the reversible status of this Reaction to value.
 */
LIBSBML_EXTERN
void
Reaction::setReversible (bool value)
{
  reversible = value;
}


/**
 * Sets the fast status of this Reaction to value.
 */
LIBSBML_EXTERN
void
Reaction::setFast (bool value)
{
  fast       = value;
  isSet.fast = 1;
}


/**
 * Adds the given reactant (SpeciesReference) to this Reaction.
 */
LIBSBML_EXTERN
void
Reaction::addReactant (SpeciesReference& sr)
{
  reactant.append(&sr);
}


/**
 * Adds the given product (SpeciesReference) to this Reaction.
 */
LIBSBML_EXTERN
void
Reaction::addProduct (SpeciesReference& sr)
{
  product.append(&sr);
}


/**
 * Adds the given modifier (ModifierSpeciesReference) to this Reaction.
 */
LIBSBML_EXTERN
void
Reaction::addModifier (ModifierSpeciesReference& msr)
{
  modifier.append(&msr);
}


/**
 * @return the list of Reactants for this Reaction.
 */
LIBSBML_EXTERN
ListOf&
Reaction::getListOfReactants ()
{
  return reactant;
}


/**
 * @return the list of Reactants for this Reaction.
 */
LIBSBML_EXTERN
const ListOf&
Reaction::getListOfReactants () const
{
  return reactant;
}


/**
 * @return the list of Products for this Reaction.
 */
LIBSBML_EXTERN
ListOf&
Reaction::getListOfProducts ()
{
  return product;
}


/**
 * @return the list of Products for this Reaction.
 */
LIBSBML_EXTERN
const ListOf&
Reaction::getListOfProducts () const
{
  return product;
}


/**
 * @return the list of Modifiers for this Reaction.
 */
LIBSBML_EXTERN
ListOf&
Reaction::getListOfModifiers ()
{
  return modifier;
}


/**
 * @return the list of Modifiers for this Reaction.
 */
LIBSBML_EXTERN
const ListOf&
Reaction::getListOfModifiers () const
{
  return modifier;
}


/**
 * @return the nth reactant (SpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference*
Reaction::getReactant (unsigned int n) const
{
  return static_cast<SpeciesReference*>( reactant.get(n) );
}


/**
 * @return the reactant (SpeciesReference) in this Reaction with the given
 * id or NULL if no such reactant exists.
 */
LIBSBML_EXTERN
SpeciesReference*
Reaction::getReactant (const std::string& sid) const
{
  void* x =
    reactant.find(sid.c_str(), (ListItemComparator) SimpleSpeciesReferenceCmp);


  return static_cast<SpeciesReference*>(x);
}


/**
 * @return the nth product (SpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference*
Reaction::getProduct (unsigned int n) const
{
  return static_cast<SpeciesReference*>( product.get(n) );
}


/**
 * @return the product (SpeciesReference) in this Reaction with the given
 * id or NULL if no such product exists.
 */
LIBSBML_EXTERN
SpeciesReference*
Reaction::getProduct (const std::string& sid) const
{
  void* x =
    product.find(sid.c_str(), (ListItemComparator) SimpleSpeciesReferenceCmp);


  return static_cast<SpeciesReference*>(x);
}


/**
 * @return the nth modifier (ModifierSpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
ModifierSpeciesReference*
Reaction::getModifier (unsigned int n) const
{
  return static_cast<ModifierSpeciesReference*>( modifier.get(n) );
}


/**
 * @return the modifier (ModifierSpeciesReference) in this Reaction with
 * the given id or NULL if no such modifier exists.
 */
LIBSBML_EXTERN
ModifierSpeciesReference*
Reaction::getModifier (const std::string& sid) const
{
  void* x =
    modifier.find(sid.c_str(), (ListItemComparator) SimpleSpeciesReferenceCmp);


  return static_cast<ModifierSpeciesReference*>(x);
}


/**
 * @return the number of reactants (SpeciesReferences) in this Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction::getNumReactants () const
{
  return reactant.getNumItems();
}


/**
 * @return the number of products (SpeciesReferences) in this Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction::getNumProducts () const
{
  return product.getNumItems();
}


/**
 * @return the number of modifiers (ModifierSpeciesReferences) in this
 * Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction::getNumModifiers () const
{
  return modifier.getNumItems();
}


/**
 * Unsets the name of this Reaction.
 *
 * In SBML L1, a Reaction name is required and therefore <b>should always
 * be set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Reaction::unsetName ()
{
  name.erase();
}


/**
 * Unsets the KineticLaw of this Reaction.
 */
LIBSBML_EXTERN
void
Reaction::unsetKineticLaw ()
{
  kineticLaw = NULL;
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
Reaction::unsetFast ()
{
  isSet.fast = 0;
}




/**
 * Creates a new Reaction and returns a pointer to it.
 */
LIBSBML_EXTERN
Reaction_t *
Reaction_create (void)
{
  return new(std::nothrow) Reaction;
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
Reaction_createWith ( const char   *sid,
                      KineticLaw_t *kl,
                      int          reversible,
                      int          fast )
{
  KineticLaw* k = static_cast<KineticLaw*>(kl);
  Reaction*   r = new(std::nothrow) Reaction(sid ? sid : "", k, reversible);


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
  delete static_cast<Reaction*>(r);
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
  static_cast<Reaction*>(r)->initDefaults();
}


/**
 * @return the id of this Reaction.
 */
LIBSBML_EXTERN
const char *
Reaction_getId (const Reaction_t *r)
{
  const Reaction* x = static_cast<const Reaction*>(r);


  return x->isSetId() ? x->getId().c_str() : NULL;
}


/**
 * @return the name of this Reaction.
 */
LIBSBML_EXTERN
const char *
Reaction_getName (const Reaction_t *r)
{
  const Reaction* x = static_cast<const Reaction*>(r);


  return x->isSetName() ? x->getName().c_str() : NULL;
}


/**
 * @return the KineticLaw of this Reaction.
 */
LIBSBML_EXTERN
KineticLaw_t *
Reaction_getKineticLaw (const Reaction_t *r)
{
  return static_cast<const Reaction*>(r)->getKineticLaw();
}


/**
 * @return the reversible status of this Reaction.
 */
LIBSBML_EXTERN
int
Reaction_getReversible (const Reaction_t *r)
{
  return (int) static_cast<const Reaction*>(r)->getReversible();
}


/**
 * @return the fast status of this Reaction.
 */
LIBSBML_EXTERN
int
Reaction_getFast (const Reaction_t *r)
{
  return (int) static_cast<const Reaction*>(r)->getFast();
}


/**
 * @return 1 if the id of this Reaction has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Reaction_isSetId (const Reaction_t *r)
{
  return (int) static_cast<const Reaction*>(r)->isSetId();
}


/**
 * @return 1 if the name of this Reaction has been set, 0 otherwise.
 *
 * In SBML L1, a Reaction name is required and therefore <b>should always
 * be set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
int
Reaction_isSetName (const Reaction_t *r)
{
  return (int) static_cast<const Reaction*>(r)->isSetName();
}


/**
 * @return 1 if the KineticLaw of this Reaction has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Reaction_isSetKineticLaw (const Reaction_t *r)
{
  return (int) static_cast<const Reaction*>(r)->isSetKineticLaw();
}


/**
 * @return 1 if the fast status of this Reation has been set, 0 otherwise.
 *
 * In L1, fast is optional with a default of false, which means it is
 * effectively always set.  In L2, however, fast is optional with no
 * default value, so it may or may not be set to a specific value.
 */
LIBSBML_EXTERN
int
Reaction_isSetFast (const Reaction_t *r)
{
  return (int) static_cast<const Reaction*>(r)->isSetFast();
}


/**
 * Moves the id field of this Reaction to its name field (iff name is not
 * already set).  This method is used for converting from L2 to L1.
 */
LIBSBML_EXTERN
void
Reaction_moveIdToName (Reaction_t *r)
{
  static_cast<Reaction*>(r)->moveIdToName();
}


/**
 * Moves the name field of this Reaction to its id field (iff id is not
 * already set).  This method is used for converting from L1 to L2.
 */
LIBSBML_EXTERN
void
Reaction_moveNameToId (Reaction_t *r)
{
  static_cast<Reaction*>(r)->moveNameToId();
}


/**
 * Sets the id of this Reaction to a copy of sid.
 */
LIBSBML_EXTERN
void
Reaction_setId (Reaction_t *r, const char *sid)
{
  static_cast<Reaction*>(r)->setId(sid ? sid : "");
}


/**
 * Sets the name of this Reaction to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Reaction_setName (Reaction_t *r, const char *string)
{
  if (string == NULL)
  {
    static_cast<Reaction*>(r)->unsetName();
  }
  else
  {
    static_cast<Reaction*>(r)->setName(string);
  }
}


/**
 * Sets the KineticLaw of this Reaction to the given KineticLaw.
 */
LIBSBML_EXTERN
void
Reaction_setKineticLaw (Reaction_t *r, KineticLaw_t *kl)
{
  if (kl == NULL)
  {
    static_cast<Reaction*>(r)->unsetKineticLaw();
  }
  else
  {
    KineticLaw* x = static_cast<KineticLaw*>(kl);
    static_cast<Reaction*>(r)->setKineticLaw(*x);
  }
}


/**
 * Sets the reversible status of this Reaction to value (boolean).
 */
LIBSBML_EXTERN
void
Reaction_setReversible (Reaction_t *r, int value)
{
  static_cast<Reaction*>(r)->setReversible((bool) value);
}


/**
 * Sets the fast status of this Reaction to value (boolean).
 */
LIBSBML_EXTERN
void
Reaction_setFast (Reaction_t *r, int value)
{
  static_cast<Reaction*>(r)->setFast((bool) value);
}


/**
 * Adds the given reactant (SpeciesReference) to this Reaction.
 */
LIBSBML_EXTERN
void
Reaction_addReactant (Reaction_t *r, SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    SpeciesReference* x = static_cast<SpeciesReference*>(sr);
    static_cast<Reaction*>(r)->addReactant(*x);
  }
}


/**
 * Adds the given product (SpeciesReference) to this Reaction.
 */
LIBSBML_EXTERN
void
Reaction_addProduct (Reaction_t *r, SpeciesReference_t *sr)
{
  if (sr != NULL)
  {
    SpeciesReference* x = static_cast<SpeciesReference*>(sr);
    static_cast<Reaction*>(r)->addProduct(*x);
  }
}


/**
 * Adds the given modifier (ModifierSpeciesReference) to this Reaction.
 */
LIBSBML_EXTERN
void
Reaction_addModifier (Reaction_t *r, ModifierSpeciesReference_t *msr)
{
  if (msr != NULL)
  {
    ModifierSpeciesReference* x = static_cast<ModifierSpeciesReference*>(msr);
    static_cast<Reaction*>(r)->addModifier(*x);
  }
}


/**
 * @return the list of Reactants for this Reaction.
 */
LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfReactants (Reaction_t *r)
{
  return (ListOf_t *) &
  static_cast<Reaction*>(r)->getListOfReactants();
}


/**
 * @return the list of Products for this Reaction.
 */
LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfProducts (Reaction_t *r)
{
  return (ListOf_t *) &
  static_cast<Reaction*>(r)->getListOfProducts();
}


/**
 * @return the list of Modifiers for this Reaction.
 */
LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfModifiers (Reaction_t *r)
{
  return (ListOf_t *) &
  static_cast<Reaction*>(r)->getListOfModifiers();
}


/**
 * @return the nth reactant (SpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getReactant (const Reaction_t *r, unsigned int n)
{
  return static_cast<const Reaction*>(r)->getReactant(n);
}


/**
 * @return the reactant (SpeciesReference) in this Reaction with the given
 * id or NULL if no such reactant exists.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getReactantById (const Reaction_t *r, const char *sid)
{
  if (sid == NULL) return NULL;


  return static_cast<const Reaction*>(r)->getReactant(sid);
}


/**
 * @return the nth product (SpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getProduct (const Reaction_t *r, unsigned int n)
{
  return static_cast<const Reaction*>(r)->getProduct(n);
}


/**
 * @return the product (SpeciesReference) in this Reaction with the given
 * id or NULL if no such product exists.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getProductById (const Reaction_t *r, const char *sid)
{
  if (sid == NULL) return NULL;


  return static_cast<const Reaction*>(r)->getProduct(sid);
}


/**
 * @return the nth modifier (ModifierSpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
ModifierSpeciesReference_t *
Reaction_getModifier (const Reaction_t *r, unsigned int n)
{
  return static_cast<const Reaction*>(r)->getModifier(n);
}


/**
 * @return the modifier (ModifierSpeciesReference) in this Reaction with
 * the given id or NULL if no such modifier exists.
 */
LIBSBML_EXTERN
ModifierSpeciesReference_t *
Reaction_getModifierById (const Reaction_t *r, const char *sid)
{
  if (sid == NULL) return NULL;


  return static_cast<const Reaction*>(r)->getModifier(sid);
}


/**
 * @return the number of reactants (SpeciesReferences) in this Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction_getNumReactants (const Reaction_t *r)
{
  return static_cast<const Reaction*>(r)->getNumReactants();
}


/**
 * @return the number of products (SpeciesReferences) in this Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction_getNumProducts (const Reaction_t *r)
{
  return static_cast<const Reaction*>(r)->getNumProducts();
}


/**
 * @return the number of modifiers (ModifierSpeciesReferences) in this
 * Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction_getNumModifiers (const Reaction_t *r)
{
  return static_cast<const Reaction*>(r)->getNumModifiers();
}


/**
 * Unsets the name of this Reaction.  This is equivalent to:
 * safe_free(r->name); r->name = NULL;
 *
 * In SBML L1, a Reaction name is required and therefore <b>should always
 * be set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Reaction_unsetName (Reaction_t *r)
{
  static_cast<Reaction*>(r)->unsetName();
}


/**
 * Unsets the KineticLaw of this Reaction.  This is equivalent to:
 * r->kineticLaw = NULL;
 */
LIBSBML_EXTERN
void
Reaction_unsetKineticLaw (Reaction_t *r)
{
  static_cast<Reaction*>(r)->unsetKineticLaw();
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
  static_cast<Reaction*>(r)->unsetFast();
}


/**
 * The ReactionIdCmp function compares the string sid to r->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than r->id.
 * Returns -1 if either sid or r->id is NULL.
 */
LIBSBML_EXTERN
int
ReactionIdCmp (const char *sid, const Reaction_t *r)
{
  int result = -1;


  if (sid != NULL && Reaction_isSetId(r))
  {
    result = strcmp(sid, Reaction_getId(r));
  }

  return result;
}
