/**
 * Filename    : SpeciesReference.h
 * Description : SBML SpeciesReference
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-11-25
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


#ifndef SpeciesReference_h
#define SpeciesReference_h


#include "SBase.h"


#ifdef __cplusplus
extern "C" {
#endif


typedef struct
{
  SBASE_FIELDS;
  char   *species;
  int     stoichiometry;
  int     denominator;
} SpeciesReference_t;


/**
 * Creates a new SpeciesReference and returns a pointer to it.
 */
SpeciesReference_t *
SpeciesReference_create (void);

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
                              int        denominator );

/**
 * Frees the given SpeciesReference.
 */
void
SpeciesReference_free (SpeciesReference_t *sr);

/**
 * Initializes the fields of this SpeciesReference to their defaults:
 *
 *   - stoichiometry = 1
 *   - denominator   = 1
 */
void
SpeciesReference_initDefaults (SpeciesReference_t *sr);

/**
 * Sets the species field of this SpeciesReference to a copy of sname.
 */
void
SpeciesReference_setSpecies (SpeciesReference_t *sr, const char *sname);


#ifdef __cplusplus
}
#endif


#endif  /** SpeciesReference_h **/
