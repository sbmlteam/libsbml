/**
 * Filename    : SBMLConvert.c
 * Description : Converts SBML L1 objects to SBML L2 objects.
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-07-27
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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


#include "sbml/common.h"

#include "sbml/SBMLTypes.h"
#include "sbml/SBMLTypeCodes.h"

#include "sbml/SBMLConvert.h"


/**
 * Converts the given SBase object and any of its subordinate objects from
 * SBML L1 to L2.  This function delegates, based on SBMLTypeCode, to
 * SBML_convertNameToId() and others.
 */
void
LIBSBML_EXTERN
SBML_convertToL2 (SBase_t *sb)
{
  SBMLDocument_t *d;
  Model_t        *m;
  KineticLaw_t   *kl;
  ListOf_t       *lo;

  unsigned int n;
  unsigned int size;


  if (sb == NULL) return;

  switch (sb->typecode)
  {
    case SBML_DOCUMENT:
      d          = (SBMLDocument_t *) sb;
      d->level   = 2;
      d->version = 1;
      SBML_convertToL2( (SBase_t *) d->model );
      break;

    case SBML_LIST_OF:
      lo   = (ListOf_t *) sb;
      size = ListOf_getNumItems(lo);

      for (n = 0; n < size; n++)
      {
        SBML_convertToL2( (SBase_t *) ListOf_get(lo, n) );
      }
      break;

    case SBML_MODEL:
      m = (Model_t *) sb;
      SBML_convertNameToId(sb);
      SBML_convertToL2( (SBase_t *) m->unitDefinition );
      SBML_convertToL2( (SBase_t *) m->compartment    );
      SBML_convertToL2( (SBase_t *) m->species        );
      SBML_convertToL2( (SBase_t *) m->parameter      );
      SBML_convertReactionsInModelToL2(m);
      break;

    case SBML_UNIT_DEFINITION:
    case SBML_COMPARTMENT:
    case SBML_SPECIES:
    case SBML_PARAMETER:
      SBML_convertNameToId(sb);
      break;

    case SBML_KINETIC_LAW:
      kl = (KineticLaw_t *) sb;
      SBML_convertToL2( (SBase_t *) kl->parameter );
      break;

    default:
      break;
  }
}


/**
 * Moves the name field of the given SBase object to its Id field if and
 * only if the name field is not set.  SBase may be any L1 object that has
 * a name: Model, UnitDefinition, Species, Parameter or Reaction.
 */
void
LIBSBML_EXTERN
SBML_convertNameToId (SBase_t *sb)
{
  Model_t          *m;
  UnitDefinition_t *ud;
  Compartment_t    *c;
  Species_t        *s;
  Parameter_t      *p;
  Reaction_t       *r;


  if (sb == NULL) return;

  switch (sb->typecode)
  {
    case SBML_MODEL:
      m = (Model_t *) sb;
      if (m->id == NULL)
      {
        m->id   = m->name;
        m->name = NULL;
      }
      break;

    case SBML_UNIT_DEFINITION:
      ud = (UnitDefinition_t *) sb;
      if (ud->id == NULL)
      {
        ud->id   = ud->name;
        ud->name = NULL;
      }
      break;

    case SBML_COMPARTMENT:
      c = (Compartment_t *) sb;
      if (c->id == NULL)
      {
        c->id   = c->name;
        c->name = NULL;
      }
      break;

    case SBML_SPECIES:
      s = (Species_t *) sb;
      if (s->id == NULL)
      {
        s->id   = s->name;
        s->name = NULL;
      }
      break;

    case SBML_PARAMETER:
      p = (Parameter_t *) sb;
      if (p->id == NULL)
      {
        p->id   = p->name;
        p->name = NULL;
      }
      break;

    case SBML_REACTION:
      r = (Reaction_t *) sb;
      if (r->id == NULL)
      {
        r->id   = r->name;
        r->name = NULL;
      }
      break;

    default:
      break;
  }
}


/**
 * Converts the list of Reactions in this Model from SBML L1 to L2.
 *
 * Conversion involves:
 *
 *   - Converting Reaction name to Reaction id (via SBML_convertNameToId())
 *
 *   - Converting the subordinate KineticLaw (and its Parameters) to L2
 *     (via SBML_convertToL2()), and
 *
 *   - Adding modifiers (ModifierSpeciesReference) to this Reaction as
 *     appropriate (via SBML_addModifiersToReaction()).
 */
void
SBML_convertReactionsInModelToL2 (Model_t *m)
{
  unsigned int  numReactions = Model_getNumReactions(m);
  ListOf_t     *reactions    = Model_getListOfReactions(m);

  unsigned int  n;
  Reaction_t   *r;


  for (n = 0; n < numReactions; n++)
  {
    r = (Reaction_t *) ListOf_get(reactions, n);

    SBML_convertNameToId( (SBase_t *) r );
    SBML_convertToL2    ( (SBase_t *) r->kineticLaw );

    SBML_addModifiersToReaction(r, m);
  }
}


/**
 * Adds modifiers (ModifierSpeciesReferences) to the given Reaction.
 *
 * A Model is needed for context to determine the set of allowable Species
 * (see criterion 1 below).
 *
 * For each symbol in the Reaction's KineticLaw, that symbol is a modifier
 * iff:
 *
 *   1. It is defined as a Species in the Model
 *   2. It is not a Reactant or Product in this Reaction.
 */
LIBSBML_EXTERN
void
SBML_addModifiersToReaction (Reaction_t *r, const Model_t *m)
{
  const char *id;

  unsigned int size;
  unsigned int n;

  const ASTNode_t *node;
  List_t          *names;

  KineticLaw_t *kl = Reaction_getKineticLaw(r);


  /**
   * If the Reaction does not have a KineticLaw or the KineticLaw does not
   * have a formula, there is nothing to be done.
   */
  if ( kl == NULL ) return;
  if ( !KineticLaw_isSetMath(kl) && !KineticLaw_isSetFormula(kl) ) return;

  /**
   * Ensure the KineticLaw has an AST math expression by deriving it from
   * the infix formula string if nescessary.
   */
  if ( !KineticLaw_isSetMath(kl) )
  {
    KineticLaw_setMath(kl, SBML_parseFormula( KineticLaw_getFormula(kl) ));
  }

  /**
   * Get a list of AST_NAMEs (symbols) used in the KineticLaw.
   */
  node  = KineticLaw_getMath(kl);
  names = ASTNode_getListOfNodes(node, (ASTNodePredicate) ASTNode_isName);
  size  = List_size(names);

  /**
   * NOTE: The C 'continue' keyword immediately aborts the current loop
   * iteration and continues with the next one (n++).  It is used below as
   * an alternative to several levels of nested if statements.
   *
   * For each symbol, add it as a Reaction modifier iff:
   */
  for (n = 0; n < size; n++)
  {
    node = (ASTNode_t *) List_get(names, n);
    id   = ASTNode_getName(node);

    /** 1. It is an AST_NAME (not AST_NAME_TIME nor AST_NAME_DELAY), and **/
    if (ASTNode_getType(node) != AST_NAME) continue;

    /** 2. It refers to a Species in this Model, and **/
    if (id == NULL || Model_getSpeciesById(m, id) == NULL) continue;

    /** 3. It is not a Reactant, Product, or (already) a Modifier **/
    if (Reaction_getReactantById(r, id) != NULL) continue;
    if (Reaction_getProductById (r, id) != NULL) continue;
    if (Reaction_getModifierById(r, id) != NULL) continue;

    Reaction_addModifier(r, ModifierSpeciesReference_createWith(id));
  }

  List_free(names);
}
