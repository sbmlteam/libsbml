/**
 * Filename    : CompartmentVolumeRule.c
 * Description : SBML CompartmentVolumeRule
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


#include "sbml/AssignmentRule.h"
#include "sbml/CompartmentVolumeRule.h"


/**
 * Creates a new CompartmentVolumeRule and returns a pointer to it.
 */
LIBSBML_EXTERN
CompartmentVolumeRule_t *
CompartmentVolumeRule_create (void)
{
  CompartmentVolumeRule_t *cvr;


  cvr = (CompartmentVolumeRule_t *)
        safe_calloc(1, sizeof(CompartmentVolumeRule_t));

  AssignmentRule_init((AssignmentRule_t *) cvr, SBML_COMPARTMENT_VOLUME_RULE);

  return cvr;
}


/**
 * Creates a new CompartmentVolumeRule with the given formula, type and
 * compartment and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 *
 *   CompartmentVolumeRule_t *cvr = CompartmentVolumeRule_create();
 *   Rule_setFormula((Rule_t *) cvr, formula);
 *   AssignmentRule_setType((AssignmentRule_t *) cvr, type);
 *   ...;
 */
LIBSBML_EXTERN
CompartmentVolumeRule_t *
CompartmentVolumeRule_createWith ( const char *formula,
                                   RuleType_t type,
                                   const char *compartment )
{
  CompartmentVolumeRule_t *cvr = CompartmentVolumeRule_create();


  Rule_setFormula((Rule_t *) cvr, formula);
  CompartmentVolumeRule_setCompartment(cvr, compartment);

  cvr->type = type;

  return cvr;
}


/**
 * Frees the given CompartmentVolumeRule.
 */
LIBSBML_EXTERN
void
CompartmentVolumeRule_free (CompartmentVolumeRule_t *cvr)
{
  if (cvr == NULL) return;

  AssignmentRule_clear((AssignmentRule_t *) cvr);

  safe_free(cvr->compartment);
  safe_free(cvr);
}


/**
 * @return the compartment of this CompartmentVolumeRule.
 */
LIBSBML_EXTERN
const char *
CompartmentVolumeRule_getCompartment (const CompartmentVolumeRule_t *cvr)
{
  return cvr->compartment;
}


/**
 * @return 1 if the compartment of this CompartmentVolumeRule has been set,
 * 0 otherwise.
 */
LIBSBML_EXTERN
int
CompartmentVolumeRule_isSetCompartment (const CompartmentVolumeRule_t *cvr)
{
  return (cvr->compartment != NULL);
}


/**
 * Sets the compartment field of this CompartmentVolumeRule to a copy of
 * sname.
 */
LIBSBML_EXTERN
void
CompartmentVolumeRule_setCompartment ( CompartmentVolumeRule_t *cvr,
                                       const char *sname )
{
  if (cvr->compartment != NULL)
  {
    safe_free(cvr->compartment);
  }

  cvr->compartment = (sname == NULL) ? NULL : safe_strdup(sname);
}
