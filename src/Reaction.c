/**
 * Filename    : Reaction.c
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


#include "sbml/common.h"
#include "sbml/Reaction.h"


/**
 * Creates a new Reaction and returns a pointer to it.
 */
Reaction_t *
Reaction_create (void)
{
  Reaction_t *r;


  r = (Reaction_t *) safe_calloc(1, sizeof(Reaction_t));

  SBase_init((SBase_t *) r, SBML_REACTION);
  Reaction_initDefaults(r);

  r->reactant = (List_t *) List_create();
  r->product  = (List_t *) List_create();

  return r;
}


/**
 * Creates a new Reaction with the given name, KineticLaw, reversible and
 * fast and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 *
 *   Reaction_t *r = Reaction_create();
 *   Reaction_setName(r, name); r->kineticLaw = kl; ...;
 */
Reaction_t *
Reaction_createWith ( const char   *name,
                      KineticLaw_t *kl,
                      int          reversible,
                      int          fast )
{
  Reaction_t *r = Reaction_create();


  Reaction_setName(r, name);

  r->kineticLaw = kl;
  r->reversible = reversible;
  r->fast       = fast;

  return r;
}


/**
 * Frees the given Reaction.
 */
void
Reaction_free (Reaction_t *r)
{
  if (r == NULL) return;

  SBase_clear((SBase_t *) r);

  List_freeItems(r->reactant, SpeciesReference_free, SpeciesReference_t);
  List_free(r->reactant);

  List_freeItems(r->product , SpeciesReference_free, SpeciesReference_t);
  List_free(r->product);

  KineticLaw_free(r->kineticLaw);

  safe_free(r->name);
  safe_free(r);
}


/**
 * Initializes the fields of this Reaction to their defaults:
 *
 *   - reversible = 1 (true)
 *   - fast       = 0 (false)
 */
void
Reaction_initDefaults (Reaction_t *r)
{
  r->reversible = 1;
  r->fast       = 0;
}


/**
 * Sets the name field of this Reaction to a copy of sname.
 */
void
Reaction_setName(Reaction_t *r, const char *sname)
{
  if (r->name != NULL)
  {
    safe_free(r->name);
  }

  r->name = (sname == NULL) ? NULL : safe_strdup(sname);
}


/**
 * Adds the given reactant (SpeciesReference) to this Reaction.
 */
void
Reaction_addReactant(Reaction_t *r, SpeciesReference_t *sr)
{
  List_add(r->reactant, sr);
}


/**
 * Adds the given product (SpeciesReference) to this Reaction.
 */
void
Reaction_addProduct(Reaction_t *r, SpeciesReference_t *sr)
{
  List_add(r->product, sr);
}


/**
 * @return the nth reactant (SpeciesReference) of this Reaction.
 */
SpeciesReference_t *
Reaction_getReactant(Reaction_t *r, unsigned int n)
{
  return (SpeciesReference_t *) List_get(r->reactant, n);
}


/**
 * @return the nth product (SpeciesReference) of this Reaction.
 */
SpeciesReference_t *
Reaction_getProduct(Reaction_t *r, unsigned int n)
{
  return (SpeciesReference_t *) List_get(r->product, n);
}


/**
 * @return the number of reactants (SpeciesReferences) in this Reaction.
 */
unsigned int
Reaction_getNumReactants(const Reaction_t *r)
{
  return List_size(r->reactant);
}


/**
 * @return the number of products (SpeciesReferences) in this Reaction.
 */
unsigned int
Reaction_getNumProducts(const Reaction_t *r)
{
  return List_size(r->product);
}
