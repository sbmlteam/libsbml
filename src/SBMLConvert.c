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
 * SBML_convertNameToId().
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
      SBML_convertToL2( (SBase_t *) m->reaction       );
      break;

    case SBML_UNIT_DEFINITION:
    case SBML_COMPARTMENT:
    case SBML_SPECIES:
    case SBML_PARAMETER:
    case SBML_REACTION:
      SBML_convertNameToId(sb);
      break;

    case SBML_KINETIC_LAW:
      kl = (KineticLaw_t *) sb;
      SBML_convertToL2( (SBase_t *) (kl->parameter) );
      break;

    default:
      break;
  }
}


/**
 * Converts the formula of this SBase object to the equivalent ASTNode math
 * expression if and only if the formula field is set and the math field is
 * not set.  After the conversion, the formula field is cleared.  SBase may
 * be a KineticLaw or Rule.
 */
void
SBML_convertFormulaToMath (SBase_t *sb)
{
  KineticLaw_t *kl;
  Rule_t       *r;


  switch (sb->typecode)
  {
    case SBML_KINETIC_LAW:
      kl = (KineticLaw_t *) sb;
      if (kl->math == NULL && kl->formula != NULL)
      {
        kl->math    = SBML_parseFormula(kl->formula);
        kl->formula = NULL;
      }
      break;

    case SBML_ALGEBRAIC_RULE:
    case SBML_ASSIGNMENT_RULE:
      r = (Rule_t *) sb;
      if (r->math == NULL && r->formula != NULL)
      {
        r->math    = SBML_parseFormula(r->formula);
        r->formula = NULL;
      }
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
 * Converts the SBase object from an SBML Level 1 Rule to an SBML Level 2
 * Rule.
 *
 * The address of the SBase pointer (**sb) is required as L1
 * AssignmentRules of RULE_TYPE_RATE will be copied to a newly created
 * RateRule in L2.  Also, ParameterRules are always copied to a new
 * AssignmentRule or RateRule, regardless of their RuleType.  This is done
 * to keep rule sizes in memory consistent.  ParameterRules are the only
 * AssignmentRules with an extra field, units.
 */
void
SBML_convertRuleToL2 (SBase_t **sb)
{
  RateRule_t       *rr;
  AssignmentRule_t *ar;
  ParameterRule_t  *pr;


  switch ( (*sb)->typecode )
  {
    case SBML_ALGEBRAIC_RULE:
      SBML_convertFormulaToMath(*sb);
      break;

    case SBML_SPECIES_CONCENTRATION_RULE:
    case SBML_COMPARTMENT_VOLUME_RULE:
      ar = (AssignmentRule_t *) *sb;

      if (ar->type == RULE_TYPE_SCALAR)
      {
        (*sb)->typecode = SBML_ASSIGNMENT_RULE;
      }
      else if (ar->type == RULE_TYPE_RATE)
      {
        rr           = RateRule_create();
        rr->formula  = ar->formula;
        rr->math     = ar->math;
        rr->variable = ar->variable;

        safe_free(ar);
        *sb = (SBase_t *) rr;
      }

      SBML_convertFormulaToMath(*sb);
      break;
  
    case SBML_PARAMETER_RULE:
      pr = (ParameterRule_t *) *sb;

      if (pr->type == RULE_TYPE_SCALAR)
      {
        ar           = AssignmentRule_create();
        ar->formula  = pr->formula;
        ar->math     = pr->math;
        ar->variable = pr->variable;

        safe_free(pr->units);
        safe_free(pr);

        *sb = (SBase_t *) ar;
      }
      else if (pr->type == RULE_TYPE_RATE)
      {
        rr           = RateRule_create();
        rr->formula  = pr->formula;
        rr->math     = pr->math;
        rr->variable = pr->variable;

        safe_free(pr->units);
        safe_free(pr);
        *sb = (SBase_t *) rr;
      }

      SBML_convertFormulaToMath(*sb);
      break;

    default:
      break;
  }
}
