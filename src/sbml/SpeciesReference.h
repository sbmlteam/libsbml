/**
 * Filename    : SpeciesReference.h
 * Description : SBML SpeciesReference
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
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


#ifndef SpeciesReference_h
#define SpeciesReference_h


#include "common.h"
#include "ASTNode.h"
#include "SBase.h"
#include "SimpleSpeciesReference.h"


BEGIN_C_DECLS


typedef struct
{
  SBASE_FIELDS;
  SIMPLE_SPECIES_REFERENCE_FIELDS;
  double    stoichiometry;
  int       denominator;
  ASTNode_t *stoichiometryMath;
} SpeciesReference_t;


/**
 * Creates a new SpeciesReference and returns a pointer to it.
 */
LIBSBML_EXTERN
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
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createWith ( const char *species,
                              double     stoichiometry,
                              int        denominator );

/**
 * Frees the given SpeciesReference.
 */
LIBSBML_EXTERN
void
SpeciesReference_free (SpeciesReference_t *sr);

/**
 * Initializes the fields of this SpeciesReference to their defaults:
 *
 *   - stoichiometry = 1
 *   - denominator   = 1
 */
LIBSBML_EXTERN
void
SpeciesReference_initDefaults (SpeciesReference_t *sr);


/**
 * @return the species of this SpeciesReference.
 */
LIBSBML_EXTERN
const char *
SpeciesReference_getSpecies (const SpeciesReference_t *sr);

/**
 * @return the stoichiometry of this SpeciesReference.
 */
LIBSBML_EXTERN
double
SpeciesReference_getStoichiometry (const SpeciesReference_t *sr);

/**
 * @return the stoichiometryMath of this SpeciesReference.
 */
LIBSBML_EXTERN
const ASTNode_t *
SpeciesReference_getStoichiometryMath (const SpeciesReference_t *sr);

/**
 * @return the denominator of this SpeciesReference.
 */
LIBSBML_EXTERN
int
SpeciesReference_getDenominator (const SpeciesReference_t *sr);


/**
 * @return 1 if the species of this SpeciesReference has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetSpecies (const SpeciesReference_t *sr);

/**
 * @return 1 if the stoichiometryMath of this SpeciesReference has been
 * set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetStoichiometryMath (const SpeciesReference_t *sr);


/**
 * Sets the species of this SpeciesReference to a copy of sname.
 */
LIBSBML_EXTERN
void
SpeciesReference_setSpecies (SpeciesReference_t *sr, const char *sname);

/**
 * Sets the stoichiometry of this SpeciesReference to value.
 */
LIBSBML_EXTERN
void
SpeciesReference_setStoichiometry (SpeciesReference_t *sr, double value);

/**
 * Sets the stoichiometryMath of this SpeciesReference to the given
 * ASTNode.
 *
 * The node <b>is not copied</b> and this SpeciesReference <b>takes
 * ownership</b> of it; i.e. subsequent calls to this function or a call to
 * SpeciesReference_free() will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
SpeciesReference_setStoichiometryMath (SpeciesReference_t *sr, ASTNode_t *math);

/**
 * Sets the denominator of this SpeciesReference to value.
 */
LIBSBML_EXTERN
void
SpeciesReference_setDenominator (SpeciesReference_t *sr, int value);


END_C_DECLS


#endif  /** SpeciesReference_h **/
