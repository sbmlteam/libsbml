/**
 * Filename    : AssignmentRule.c
 * Description : SBML AssignmentRule
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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


#include "sbml/Rule.h"
#include "sbml/AssignmentRule.h"


/**
 * AssignmentRule "objects" are abstract, i.e. they are not created.
 * Rather, specific "subclasses" are created (e.g. ParameterRule) and their
 * ASSIGNMENT_RULE_FIELDS are initialized with this function.  The type of
 * the specific "subclass" is indicated by the given SBMLTypeCode.
 *
 * This function also calls its "parent", Rule_init().
 */
LIBSBML_EXTERN
void
AssignmentRule_init (AssignmentRule_t *ar, SBMLTypeCode_t tc)
{
  Rule_init((Rule_t *) ar, tc);
  AssignmentRule_initDefaults(ar);
}


/**
 * Clears (frees) ASSIGNMENT_RULE_FIELDS of this AssignmentRule "subclass".
 * This function also calls its "parent", Rule_clear().
 */
LIBSBML_EXTERN
void
AssignmentRule_clear (AssignmentRule_t *ar)
{
  if (ar == NULL) return;

  Rule_clear((Rule_t *) ar);
}


/**
 * Initializes the fields of this AssignmentRule to their defaults:
 *
 *   - type = RULE_TYPE_SCALAR
 */
LIBSBML_EXTERN
void
AssignmentRule_initDefaults (AssignmentRule_t *ar)
{
  ar->type = RULE_TYPE_SCALAR;
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
 * Sets the type of this Rule to the given RuleType.
 */
LIBSBML_EXTERN
void
AssignmentRule_setType (AssignmentRule_t *ar, RuleType_t rt)
{
  ar->type = rt;
}
