/**
 * Filename    : SpeciesReference.c
 * Description : SBML SpeciesReference
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-11-22
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


#include "sbml/common.h"
#include "sbml/SpeciesReference.h"


/**
 * Creates a new SpeciesReference and returns a pointer to it.
 */
SpeciesReference_t *
SpeciesReference_create (void)
{
  SpeciesReference_t *sr;


  sr = (SpeciesReference_t *) safe_calloc(1, sizeof(SpeciesReference_t));  

  SBase_init((SBase_t *) sr, SBML_SPECIES_REFERENCE);
  SpeciesReference_initDefaults(sr);

  return sr;
}


/**
 * Creates a new SpeciesReference with the given species, stoichiometry and
 * denominator and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 *
 *   SpeciesReference_t *r = SpeciesReference_create();
 *   SpeciesReference_setSpecies(r, species);
 *   r->stoichiometry = stoichiometry;
 *   ...;
 */
SpeciesReference_t *
SpeciesReference_createWith ( const char *species,
                              int        stoichiometry,
                              int        denominator )
{
  SpeciesReference_t *sr = SpeciesReference_create();


  SpeciesReference_setSpecies(sr, species);

  sr->stoichiometry = stoichiometry;
  sr->denominator   = denominator;

  return sr;
}


/**
 * Frees the given SpeciesReference.
 */
void
SpeciesReference_free (SpeciesReference_t *sr)
{
  if (sr == NULL) return;

  SBase_clear((SBase_t *) sr);

  safe_free(sr->species);
  safe_free(sr);
}


/**
 * Initializes the fields of this SpeciesReference to their defaults:
 *
 *   - stoichiometry = 1
 *   - denominator   = 1
 */
void
SpeciesReference_initDefaults (SpeciesReference_t *sr)
{
  sr->stoichiometry = 1;
  sr->denominator   = 1;
}


/**
 * Sets the species field of this SpeciesReference to a copy of sname.
 */
void
SpeciesReference_setSpecies (SpeciesReference_t *sr, const char *sname)
{
  if (sr->species != NULL)
  {
    safe_free(sr->species);
  }

  sr->species = (sname == NULL) ? NULL : safe_strdup(sname);
}
