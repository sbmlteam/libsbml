/**
 * Filename    : Validator.h
 * Description : Holds a set of rules for validating an SBML document
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2004-03-25
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


#ifndef Validator_h
#define Validator_h


#include "extern.h"

#include "List.h"
#include "SBase.h"
#include "SBMLDocument.h"
#include "SBMLTypeCodes.h"


BEGIN_C_DECLS


/**
 * ValidationRule
 *
 * A ValidationRule is a function that performs a single check on an aspect
 * of an SBML document.
 *
 * A ValidationRule takes as parameters the SBML object to be checked
 * (SBase_t *), the SBMLDocument itself (for context, if necessary), and a
 * list of ParseMessages to which it may append an error message. It
 * returns true (1) if the check passed and false (0) otherwise.
 *
 * If the list of messages is NULL, the ValidationRule should not generate
 * an error message.
 *
 * ValidationRules are added to a Validator with the Validator_addRule()
 * function.  When a rule is added, it is associated with an SBMLTypeCode,
 * indicating the type of SBML object it validates.  When a Validator is
 * run, the rule will be called once for each SBML object of the given
 * type.
 *
 * @see Validator_addRule()
 */
typedef unsigned int
        (*ValidationRule) ( const SBase_t *obj, const SBMLDocument_t *d,
                            List_t *messages );


/**
 * ValidatorPair
 *
 * Pairs a ValidationRule with the type of SBML object on which it can
 * operate.
 */
typedef struct
{
  ValidationRule rule;
  SBMLTypeCode_t type;
} ValidatorPair_t;


/**
 * Validator
 *
 * A Validator is a list of ValidatorPairs.
 */
typedef struct
{
  List_t *rule;
} Validator_t;


/**
 * Creates a new Validator and returns a pointer to it.
 */
LIBSBML_EXTERN
Validator_t *
Validator_create (void);

/**
 * Creates a new Validator with the default ValidationRule set and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
Validator_t *
Validator_createDefault (void);

/**
 * Creates a new ValidatorPair and returns a pointer to it.
 */
ValidatorPair_t *
ValidatorPair_create (ValidationRule rule, SBMLTypeCode_t type);

/**
 * Frees the given Validator.
 */
LIBSBML_EXTERN
void
Validator_free (Validator_t *v);

/**
 * Frees the given ValidatorPair.
 */
void
ValidatorPair_free (ValidatorPair_t *pair);


/**
 * Adds the given ValidationRule to this validator.  When the Validator is
 * run, the ValidationRule will be called once for each SBML object with
 * the given SBMLTypeCode.
 */
LIBSBML_EXTERN
void
Validator_addRule (Validator_t *v, ValidationRule rule, SBMLTypeCode_t type);

/**
 * Adds the default ValidationRule set to this Validator.
 */
void
Validator_addDefaultRules (Validator_t *v);


/**
 * @return the number of ValidationRules in this Validator.
 */
LIBSBML_EXTERN
unsigned int
Validator_getNumRules (const Validator_t *v);

/**
 * @return the nth ValidationRule of this Validator.
 */
LIBSBML_EXTERN
ValidationRule
Validator_getRule (const Validator_t *v, unsigned int n);

/**
 * @return a List of ValidationRules for the SBML object of the given
 * type.
 *
 * The caller owns the returned list (but not its constituent items) and is
 * responsible for freeing it with List_free().
 */
List_t *
Validator_getRulesOfType (const Validator_t *v, SBMLTypeCode_t type);


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
                     List_t               *messages );

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
                     , List_t               * messages );


END_C_DECLS


#endif  /** Validator_h **/
