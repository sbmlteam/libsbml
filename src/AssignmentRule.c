/**
 * Filename    : AssignmentRule.c
 * Description : SBML AssignmentRule
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-26
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


#include "sbml/Rule.h"
#include "sbml/AssignmentRule.h"


/**
 * Creates a new AssignmentRule and returns a pointer to it.
 *
 * In L1 AssignmentRule is an abstract class.  It exists soley to provide
 * fields to its subclasess: CompartmentVolumeRule, ParameterRule and
 * SpeciesConcentrationRule.
 *
 * In L2 the three subclasses are gone and AssigmentRule is concrete;
 * i.e. it may be created, used and destroyed directly.
 */
LIBSBML_EXTERN
AssignmentRule_t *
AssignmentRule_create (void)
{
  AssignmentRule_t *ar;


  ar = (AssignmentRule_t *) safe_calloc(1, sizeof(AssignmentRule_t));
  AssignmentRule_init(ar, SBML_ASSIGNMENT_RULE);

  return ar;
}


/**
 * Creates a new AssignmentRule with the given variable and math and
 * returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   ar = AssignmentRule_create();
 *   AssignmentRule_setVariable(ar, variable);
 *   Rule_setMath((Rule_t *) ar, math);
 */
LIBSBML_EXTERN
AssignmentRule_t *
AssignmentRule_createWith (const char *variable, ASTNode_t *math)
{
  AssignmentRule_t *ar = AssignmentRule_create();


  AssignmentRule_setVariable( ar, variable );
  Rule_setMath( (Rule_t *) ar, math );

  return ar;
}


/**
 * Frees the given AssignmentRule.
 */
LIBSBML_EXTERN
void
AssignmentRule_free (AssignmentRule_t *ar)
{
  if (ar == NULL) return;


  AssignmentRule_clear(ar);
  safe_free(ar);
}


/**
 * The function is kept for backward compatibility with the SBML L1 API.
 *
 * Clears (frees) ASSIGNMENT_RULE_FIELDS of this AssignmentRule "subclass".
 * This function also calls its "parent", Rule_clear().
 */
void
AssignmentRule_clear (AssignmentRule_t *ar)
{
  if (ar == NULL) return;


  safe_free(ar->variable);
  Rule_clear((Rule_t *) ar);
}


/**
 * The function is kept for backward compatibility with the SBML L1 API.
 *
 * In L1 AssignmentRule "objects" are abstract, i.e. they are not created.
 * Rather, specific "subclasses" are created (e.g. ParameterRule) and their
 * ASSIGNMENT_RULE_FIELDS are initialized with this function.  The type of
 * the specific "subclass" is indicated by the given SBMLTypeCode.
 *
 * This function also calls its "parent", Rule_init().
 */
void
AssignmentRule_init (AssignmentRule_t *ar, SBMLTypeCode_t tc)
{
  Rule_init((Rule_t *) ar, tc);
  AssignmentRule_initDefaults(ar);
}


/**
 * The function is kept for backward compatibility with the SBML L1 API.
 *
 * Initializes the fields of this AssignmentRule to their defaults:
 *
 *   - type = RULE_TYPE_SCALAR
 */
LIBSBML_EXTERN
void
AssignmentRule_initDefaults (AssignmentRule_t *ar)
{
  AssignmentRule_setType(ar, RULE_TYPE_SCALAR);
}


/**
 * @return the type for this AssignmentRule.
 */
LIBSBML_EXTERN
RuleType_t
AssignmentRule_getType (const AssignmentRule_t *ar)
{
  return ar->type;
}


/**
 * @return the variable for this AssignmentRule.
 */
LIBSBML_EXTERN
const char *
AssignmentRule_getVariable (const AssignmentRule_t *ar)
{
  return ar->variable;
}


/**
 * @return 1 if the variable of this AssignmentRule has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
AssignmentRule_isSetVariable (const AssignmentRule_t *ar)
{
  return (ar->variable != NULL);
}


/**
 * Sets the type of this Rule to the given RuleType.
 */
LIBSBML_EXTERN
void
AssignmentRule_setType (AssignmentRule_t *ar, RuleType_t rt)
{
  ar->type = rt;
}


/**
 * Sets the variable of this AssignmentRule to a copy of sid.
 */
LIBSBML_EXTERN
void
AssignmentRule_setVariable (AssignmentRule_t *ar, const char *sid)
{
  if (ar->variable == sid) return;


  if (ar->variable != NULL)
  {
    safe_free(ar->variable);
  }

  ar->variable = (sid == NULL) ? NULL : safe_strdup(sid);
}
