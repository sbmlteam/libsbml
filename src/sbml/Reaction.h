/**
 * Filename    : Reaction.h
 * Description : SBML Reaction
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
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


#include "common.h"
#include "List.h"
#include "SBase.h"
#include "KineticLaw.h"
#include "SpeciesReference.h"


BEGIN_C_DECLS


typedef struct
{
  SBASE_FIELDS;
  char         *name;
  List_t       *reactant;
  List_t       *product;
  KineticLaw_t *kineticLaw;
  int           reversible;
  int           fast;
} Reaction_t;


/**
 * Creates a new Reaction and returns a pointer to it.
 */
LIBSBML_EXTERN
Reaction_t *
Reaction_create (void);

/**
 * Creates a new Reaction with the given name, KineticLaw, reversible and
 * fast and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 *
 *   Reaction_t *r = Reaction_create();
 *   Reaction_setName(r, name); Reaction_setKineticLaw(r, kl); ...;
 */
LIBSBML_EXTERN
Reaction_t *
Reaction_createWith ( const char   *name,
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
 *   - fast       = 0 (false)
 */
LIBSBML_EXTERN
void
Reaction_initDefaults (Reaction_t *r);


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
 * @return 1 if the name of this Reaction has been set, 0 otherwise.
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
 * Sets the name of this Reaction to a copy of sname.
 */
LIBSBML_EXTERN
void
Reaction_setName (Reaction_t *r, const char *sname);

/**
 * Sets the KineticLaw of this Reaction to the given KineticLaw and marks
 * the field as set.
 */
LIBSBML_EXTERN
void
Reaction_setKineticLaw (Reaction_t *r, KineticLaw_t *kl);

/**
 * Sets the reversible status of this Reaction to value (boolean) and marks
 * the field as set.
 */
LIBSBML_EXTERN
void
Reaction_setReversible (Reaction_t *r, int value);

/**
 * Sets the fast status of this Reaction to value (boolean) and marks the
 * field as set.
 */
LIBSBML_EXTERN
void
Reaction_setFast (Reaction_t *r, int value);


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
 * @return the nth reactant (SpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getReactant (const Reaction_t *r, unsigned int n);

/**
 * @return the nth product (SpeciesReference) of this Reaction.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getProduct (const Reaction_t *r, unsigned int n);

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
 * Unsets the name of this Reaction.  This is equivalent to:
 * safe_free(r->name); r->name = NULL;
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


END_C_DECLS


#endif  /** Reaction_h **/
