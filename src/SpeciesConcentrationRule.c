/**
 * Filename    : SpeciesConcentrationRule.c
 * Description : SBML SpeciesConcentrationRule
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
#include "sbml/SpeciesConcentrationRule.h"


/**
 * Creates a new SpeciesConcentrationRule and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesConcentrationRule_t *
SpeciesConcentrationRule_create (void)
{
  SpeciesConcentrationRule_t *scr;


  scr = (SpeciesConcentrationRule_t *)
        safe_calloc(1, sizeof(SpeciesConcentrationRule_t));

  AssignmentRule_init( (AssignmentRule_t *) scr,
                       SBML_SPECIES_CONCENTRATION_RULE );

  return scr;
}


/**
 * Creates a new SpeciesConcentrationRule with the given formula, type and
 * species and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 *
 *   SpeciesConcentrationRule_t *scr = SpeciesConcentrationRule_create();
 *   Rule_setFormula((Rule_t *) scr, formula); scr->type = type; ...;
 */
LIBSBML_EXTERN
SpeciesConcentrationRule_t *
SpeciesConcentrationRule_createWith ( const char *formula,
                                      RuleType_t type,
                                      const char *species )
{
  SpeciesConcentrationRule_t *scr = SpeciesConcentrationRule_create();


  Rule_setFormula((Rule_t *) scr, formula);
  SpeciesConcentrationRule_setSpecies(scr, species);

  scr->type = type;

  return scr;
}


/**
 * Frees the given SpeciesConcentrationRule.
 */
LIBSBML_EXTERN
void
SpeciesConcentrationRule_free (SpeciesConcentrationRule_t *scr)
{
  if (scr == NULL) return;

  AssignmentRule_clear((AssignmentRule_t *) scr);

  safe_free(scr->species);
  safe_free(scr);
}


/**
 * @return the species of this SpeciesConcentrationRule.
 */
LIBSBML_EXTERN
const char *
SpeciesConcentrationRule_getSpecies (const SpeciesConcentrationRule_t *scr)
{
  return scr->species;
}


/**
 * @return 1 if the species of this SpeciesConcentrationRule has been set,
 * 0 otherwise.
 */
LIBSBML_EXTERN
int
SpeciesConcentrationRule_isSetSpecies (const SpeciesConcentrationRule_t *scr)
{
  return (scr->species != NULL);
}


/**
 * Sets the species of this SpeciesConcentrationRule to a copy of sname.
 */
LIBSBML_EXTERN
void
SpeciesConcentrationRule_setSpecies ( SpeciesConcentrationRule_t *scr,
                                      const char *sname )
{
  if (scr->species != NULL)
  {
    safe_free(scr->species);
  }

  scr->species = (sname == NULL) ? NULL : safe_strdup(sname);
}
