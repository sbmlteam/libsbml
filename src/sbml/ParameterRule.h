/**
 * Filename    : ParameterRule.h
 * Description : SBML ParameterRule
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


#ifndef ParameterRule_h
#define ParameterRule_h


#include "common.h"
#include "SBase.h"
#include "Rule.h"
#include "AssignmentRule.h"


BEGIN_C_DECLS


typedef struct
{
  SBASE_FIELDS;
  RULE_FIELDS;
  ASSIGNMENT_RULE_FIELDS;
  char *name;
  char *units;
} ParameterRule_t;


/**
 * Creates a new ParameterRule and returns a pointer to it.
 */
LIBSBML_EXTERN
ParameterRule_t *
ParameterRule_create (void);

/**
 * Creates a new ParameterRule with the given formula, type, and name and
 * and returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   ParameterRule_t *pr = ParameterRule_create();
 *   Rule_setFormula((Rule_t *) pr, formula); scr->type = type; ...;
 */
LIBSBML_EXTERN
ParameterRule_t *
ParameterRule_createWith ( const char *formula,
                           RuleType_t type,
                           const char *name );

/**
 * Frees the given ParameterRule.
 */
LIBSBML_EXTERN
void
ParameterRule_free (ParameterRule_t *pr);


/**
 * @return the (Parameter) name for this ParameterRule.
 */
LIBSBML_EXTERN
const char *
ParameterRule_getName (const ParameterRule_t *pr);

/**
 * @return the units for this ParameterRule.
 */
LIBSBML_EXTERN
const char *
ParameterRule_getUnits (const ParameterRule_t *pr);


/**
 * @return 1 if the (Parameter) name for this ParameterRule has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
ParameterRule_isSetName (const ParameterRule_t *pr);

/**
 * @return 1 if the units for this ParameterRule has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
ParameterRule_isSetUnits (const ParameterRule_t *pr);


/**
 * Sets the (Parameter) name for this ParameterRule to a copy of sname.
 */
LIBSBML_EXTERN
void
ParameterRule_setName (ParameterRule_t *pr, const char *sname);

/**
 * Sets the units for this ParameterRule to a copy of sname.
 */
LIBSBML_EXTERN
void
ParameterRule_setUnits (ParameterRule_t *pr, const char *sname);


/**
 * Unsets the units for this ParameterRule.  This is equivalent to:
 * safe_free(pr->units); pr->units = NULL;
 */
LIBSBML_EXTERN
void
ParameterRule_unsetUnits (ParameterRule_t *pr);


END_C_DECLS


#endif  /** ParameterRule_h **/
