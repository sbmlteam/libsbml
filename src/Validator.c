/**
 * \file    Validator.c
 * \brief   Holds a set of rules for validating an SBML document
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and
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


#include "common.h"


#include "ListOf.h"
#include "Model.h"
#include "Validator.h"


/**
 * Creates a new Validator and returns a pointer to it.
 */
LIBSBML_EXTERN
Validator_t *
Validator_create (void)
{
  Validator_t *v;


  v       = (Validator_t *) safe_calloc(1, sizeof(Validator_t));
  v->rule = (List_t *)      List_create();

  return v;
}


/**
 * Creates a new Validator with the default ValidationRule set and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
Validator_t *
Validator_createDefault (void)
{
  Validator_t *v = Validator_create();


  Validator_addDefaultRules(v);

  return v;
}


/**
 * Creates a new ValidatorPair and returns a pointer to it.
 */
ValidatorPair_t *
ValidatorPair_create (ValidationRule rule, SBMLTypeCode_t type)
{
  ValidatorPair_t *pair;


  pair = (ValidatorPair_t *) safe_malloc( sizeof(ValidatorPair_t) );

  pair->rule = rule;
  pair->type = type;

  return pair;
}


/**
 * Frees the given Validator.
 */
LIBSBML_EXTERN
void
Validator_free (Validator_t *v)
{
  if (v == NULL) return;

  List_freeItems(v->rule, ValidatorPair_free, ValidatorPair_t);
  List_free(v->rule);

  safe_free(v);
}


/**
 * Frees the given ValidatorPair.
 */
void
ValidatorPair_free (ValidatorPair_t *pair)
{
  if (pair == NULL) return;

  safe_free(pair);
}


/**
 * Adds the given ValidationRule to this validator.  When the Validator is
 * run, the ValidationRule will be called once for each SBML object with
 * the given SBMLTypeCode.
 */
LIBSBML_EXTERN
void
Validator_addRule (Validator_t *v, ValidationRule rule, SBMLTypeCode_t type)
{
  List_add( v->rule, ValidatorPair_create(rule, type) );
}


/**
 * @return the number of ValidationRules in this Validator.
 */
LIBSBML_EXTERN
unsigned int
Validator_getNumRules (const Validator_t *v)
{
  return List_size(v->rule);
}


/**
 * @return the nth ValidationRule of this Validator.
 */
LIBSBML_EXTERN
ValidationRule
Validator_getRule (const Validator_t *v, unsigned int n)
{
  return (ValidationRule) List_get(v->rule, n);
}


/**
 * @return a List of ValidationRules for the SBML object of the given
 * type.
 *
 * The caller owns the returned list (but not its constituent items) and is
 * responsible for freeing it with List_free().
 */
List_t *
Validator_getRulesOfType (const Validator_t *v, SBMLTypeCode_t type)
{
  ValidatorPair_t *pair;

  unsigned int n;
  unsigned int size = List_size(v->rule);

  List_t *result = List_create();


  for (n = 0; n < size; n++)
  {
    pair = (ValidatorPair_t *) List_get(v->rule, n);

    if (pair->type == type)
    {
      List_add(result, (void *) pair->rule);
    }
  }

  return result;
}


/**
 * Validates the given SBML document using the ValidationRules of this
 * Validator.  If messages is not NULL, a ParseMessage error message may be
 * logged for each ValidationRule that failed.
 *
 * @return the number of failed ValidationRules.
 */
LIBSBML_EXTERN
unsigned int
Validator_validate ( const Validator_t    *v,
                     const SBMLDocument_t *d,
                     List_t               *messages )
{
  unsigned int count = 0;


  count += Validator_runRules( v, SBML_FUNCTION_DEFINITION, d, messages );
  count += Validator_runRules( v, SBML_UNIT_DEFINITION    , d, messages );
  count += Validator_runRules( v, SBML_COMPARTMENT        , d, messages );
  count += Validator_runRules( v, SBML_SPECIES            , d, messages );
  count += Validator_runRules( v, SBML_PARAMETER          , d, messages );
  count += Validator_runRules( v, SBML_REACTION           , d, messages );
  count += Validator_runRules( v, SBML_EVENT              , d, messages );
  /* this next line actually picks up all rules, not just assignment rules */
  count += Validator_runRules( v, SBML_ASSIGNMENT_RULE    , d, messages );

  return count;
}


/**
 * Runs all ValidationRules for a given type of SBML object on all SBML
 * objects of that type in the SBML document.  If messages is not NULL, a
 * ParseMessage error message may be logged for each ValidationRule that
 * failed.
 *
 * This function is used internally by Validator_validate().
 *
 * @return the number of failed ValidationRules.
 */
unsigned int
Validator_runRules (   const Validator_t    * v
                     , SBMLTypeCode_t         type
                     , const SBMLDocument_t * d
                     , List_t               * messages )
{
  ValidationRule rule;

  SBase_t    *obj;
  List_t     *rules;
  ListOf_t   *objects;

  unsigned int count;
  unsigned int osize;
  unsigned int rsize;
  unsigned int on;
  unsigned int rn;

  Model_t *m = SBMLDocument_getModel((SBMLDocument_t*) d);


  count   = 0;
  objects = Model_getListOfByTypecode(m, type);

  if (objects != NULL)
  {
    osize = ListOf_getNumItems(objects);
    rules = Validator_getRulesOfType(v, type);
    rsize = List_size(rules);

    for (rn = 0; rn < rsize; rn++)
    {
      rule = (ValidationRule) List_get(rules, rn);

      for (on = 0; on < osize; on++)
      {
        obj    = (SBase_t *) ListOf_get(objects, on);
        count += !rule(obj, (SBMLDocument_t *) d, messages);
      }
    }

    List_free(rules);
  }

  return count;
}
