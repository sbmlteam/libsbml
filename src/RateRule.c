/**
 * Filename    : RateRule.c
 * Description : SBML RateRule
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-40-29
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
#include "sbml/RateRule.h"


/**
 * Creates a new RateRule and returns a pointer to it.
 */
LIBSBML_EXTERN
RateRule_t *
RateRule_create (void)
{
  RateRule_t *rr = (RateRule_t *) safe_calloc(1, sizeof(RateRule_t));


  Rule_init( (Rule_t *) rr, SBML_RATE_RULE );

  return rr;
}


/**
 * Creates a new RateRule with the given variable and math and returns a
 * pointer to it.  This convenience function is functionally equivalent to:
 *
 *   rr = RateRule_create();
 *   RateRule_setVariable(rr, variable);
 *   Rule_setMath((Rule_t *) rr, math);
 */
LIBSBML_EXTERN
RateRule_t *
RateRule_createWith (const char *variable, ASTNode_t *math)
{
  RateRule_t *rr = RateRule_create();


  RateRule_setVariable( rr, variable );
  Rule_setMath( (Rule_t *) rr, math );

  return rr;
}


/**
 * Frees the given RateRule.
 */
LIBSBML_EXTERN
void
RateRule_free (RateRule_t *rr)
{
  if (rr == NULL) return;


  Rule_clear( (Rule_t *) rr );

  safe_free(rr->variable);
  safe_free(rr);
}


/**
 * @return the variable for this RateRule.
 */
LIBSBML_EXTERN
const char *
RateRule_getVariable (const RateRule_t *rr)
{
  return rr->variable;
}


/**
 * @return 1 if the variable of this RateRule has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
RateRule_isSetVariable (const RateRule_t *rr)
{
  return (rr->variable != NULL);
}


/**
 * Sets the variable of this RateRule to a copy of sid.
 */
LIBSBML_EXTERN
void
RateRule_setVariable (RateRule_t *rr, const char *sid)
{
  if (rr->variable == sid) return;


  if (rr->variable != NULL)
  {
    safe_free(rr->variable);
  }

  rr->variable = (sid == NULL) ? NULL : safe_strdup(sid);
}
