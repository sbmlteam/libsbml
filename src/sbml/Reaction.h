/**
 * Filename    : Reaction.h
 * Description : SBML Reaction
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef Reaction_h
#define Reaction_h


#include "extern.h"

#include "ListOf.h"
#include "SBase.h"
#include "KineticLaw.h"
#include "ModifierSpeciesReference.h"
#include "SpeciesReference.h"


BEGIN_C_DECLS


typedef struct
{
  SBASE_FIELDS;
  char         *id;
  char         *name;
  ListOf_t     *reactant;
  ListOf_t     *product;
  ListOf_t     *modifier;
  KineticLaw_t *kineticLaw;
  int           reversible;
  int           fast;

  struct
  {
    unsigned int fast:1;
  }
  isSet;

} Reaction_t;


/**
 * Creates a new Reaction and returns a pointer to it.
 */
LIBSBML_EXTERN
Reaction_t *
Reaction_create (void);

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
                      int          fast );

/**
 * Frees the given Reaction.
 */
LIBSBML_EXTERN
void
Reaction_free (Reaction_t *r);

/**
 * Initializes the fields of this Reaction to their defaults:
 *
 *   - reversible = 1 (true)
 *   - fast       = 0 (false)  (L1 only)
 */
LIBSBML_EXTERN
void
Reaction_initDefaults (Reaction_t *r);


/**
 * @return the id of this Reaction.
 */
LIBSBML_EXTERN
const char *
Reaction_getId (const Reaction_t *r);

/**
 * @return the name of this Reaction.
 */
LIBSBML_EXTERN
const char *
Reaction_getName (const Reaction_t *r);

/**
 * @return the KineticLaw of this Reaction.
 */
LIBSBML_EXTERN
KineticLaw_t *
Reaction_getKineticLaw (const Reaction_t *r);

/**
 * @return the reversible status of this Reaction.
 */
LIBSBML_EXTERN
int
Reaction_getReversible (const Reaction_t *r);

/**
 * @return the fast status of this Reaction.
 */
LIBSBML_EXTERN
int
Reaction_getFast (const Reaction_t *r);


/**
 * @return 1 if the id of this Reaction has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Reaction_isSetId (const Reaction_t *r);

/**
 * @return 1 if the name of this Reaction has been set, 0 otherwise.
 *
 * In SBML L1, a Reaction name is required and therefore <b>should always
 * be set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
int
Reaction_isSetName (const Reaction_t *r);

/**
 * @return 1 if the KineticLaw of this Reaction has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Reaction_isSetKineticLaw (const Reaction_t *r);

/**
 * @return 1 if the fast status of this Reation has been set, 0 otherwise.
 *
 * In L1, fast is optional with a default of false, which means it is
 * effectively always set.  In L2, however, fast is optional with no
 * default value, so it may or may not be set to a specific value.
 */
LIBSBML_EXTERN
int
Reaction_isSetFast (const Reaction_t *r);


/**
 * Sets the id of this Reaction to a copy of sid.
 */
LIBSBML_EXTERN
void
Reaction_setId (Reaction_t *r, const char *sid);

/**
 * Sets the name of this Reaction to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Reaction_setName (Reaction_t *r, const char *string);

/**
 * Sets the KineticLaw of this Reaction to the given KineticLaw.
 */
LIBSBML_EXTERN
void
Reaction_setKineticLaw (Reaction_t *r, KineticLaw_t *kl);

/**
 * Sets the reversible status of this Reaction to value (boolean).
 */
LIBSBML_EXTERN
void
Reaction_setReversible (Reaction_t *r, int value);

/**
 * Sets the fast status of this Reaction to value (boolean).
 */
LIBSBML_EXTERN
void
Reaction_setFast (Reaction_t *r, int value);


/**
 * @return the list of Reactants for this Reaction.
 */
LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfReactants (const Reaction_t *r);

/**
 * @return the list of Products for this Reaction.
 */
LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfProducts (const Reaction_t *r);

/**
 * @return the list of Modifiers for this Reaction.
 */
LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfModifiers (const Reaction_t *r);


/**
 * Adds the given reactant (SpeciesReference) to this Reaction.
 */
LIBSBML_EXTERN
void
Reaction_addReactant (Reaction_t *r, SpeciesReference_t *sr);

/**
 * Adds the given product (SpeciesReference) to this Reaction.
 */
LIBSBML_EXTERN
void
Reaction_addProduct (Reaction_t *r, SpeciesReference_t *sr);

/**
 * Adds the given modifier (ModifierSpeciesReference) to this Reaction.
 */
LIBSBML_EXTERN
void
Reaction_addModifier (Reaction_t *r, ModifierSpeciesReference_t *msr);


/**
 * @return the nth reactant (SpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getReactant (const Reaction_t *r, unsigned int n);

/**
 * @return the reactant (SpeciesReference) in this Reaction with the given
 * id or NULL if no such reactant exists.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getReactantById (const Reaction_t *r, const char *sid);

/**
 * @return the nth product (SpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getProduct (const Reaction_t *r, unsigned int n);

/**
 * @return the product (SpeciesReference) in this Reaction with the given
 * id or NULL if no such product exists.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getProductById (const Reaction_t *r, const char *sid);

/**
 * @return the nth modifier (ModifierSpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
ModifierSpeciesReference_t *
Reaction_getModifier (const Reaction_t *r, unsigned int n);

/**
 * @return the modifier (ModifierSpeciesReference) in this Reaction with
 * the given id or NULL if no such modifier exists.
 */
LIBSBML_EXTERN
ModifierSpeciesReference_t *
Reaction_getModifierById (const Reaction_t *r, const char *sid);


/**
 * @return the number of reactants (SpeciesReferences) in this Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction_getNumReactants (const Reaction_t *r);

/**
 * @return the number of products (SpeciesReferences) in this Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction_getNumProducts (const Reaction_t *r);

/**
 * @return the number of modifiers (ModifierSpeciesReferences) in this
 * Reaction.
 */
LIBSBML_EXTERN
unsigned int
Reaction_getNumModifiers (const Reaction_t *r);


/**
 * Unsets the name of this Reaction.  This is equivalent to:
 * safe_free(r->name); r->name = NULL;
 *
 * In SBML L1, a Reaction name is required and therefore <b>should always
 * be set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Reaction_unsetName (Reaction_t *r);

/**
 * Unsets the KineticLaw of this Reaction.  This is equivalent to:
 * r->kineticLaw = NULL;
 */
LIBSBML_EXTERN
void
Reaction_unsetKineticLaw (Reaction_t *r);

/**
 * Unsets the fast status of this Reation.
 *
 * In L1, fast is optional with a default of false, which means it is
 * effectively always set.  In L2, however, fast is optional with no
 * default value, so it may or may not be set to a specific value.
 */
LIBSBML_EXTERN
void
Reaction_unsetFast (Reaction_t *r);


END_C_DECLS


#endif  /** Reaction_h **/
