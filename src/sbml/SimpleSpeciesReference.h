/**
 * Filename    : SimpleSpeciesReference.h
 * Description : SBML SimpleSpeciesReference
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-04-29
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


#ifndef SimpleSpeciesReference_h
#define SimpleSpeciesReference_h


#include "common.h"
#include "ASTNode.h"
#include "SBase.h"


BEGIN_C_DECLS


/**
 * As shown below, put SIMPLE_SPECIES_REFERENCE_FIELDS as the *second* item
 * (SBASE_FIELDS is first) of any struct which "is a(n)" SBML
 * SimpleSpeciesReference object.
 */
#define SIMPLE_SPECIES_REFERENCE_FIELDS  char *species


typedef struct
{
  SBASE_FIELDS;
  SIMPLE_SPECIES_REFERENCE_FIELDS;
} SimpleSpeciesReference_t;


/**
 * SimpleSpeciesReference "objects" are abstract, i.e. they are not
 * created.  Rather, specific "subclasses" are created
 * (e.g. SpeciesReference, ModifierSpeciesReference) and their
 * SIMPLE_SPECIES_REFERENCE_FIELDS are initialized with this function.  The
 * type of the specific "subclass" is indicated by the given SBMLTypeCode.
 *
 * This function also calls its "parent", SBase_init().
 */
void
SimpleSpeciesReference_init (SimpleSpeciesReference_t *ssr, SBMLTypeCode_t tc);

/**
 * Clears (frees) SIMPLE_SPECIES_REFERENCE_FIELDS of this
 * SimpleSpeciesReference "subclass".  This function also calls its
 * "parent", SBase_clear().
 */
void
SimpleSpeciesReference_clear (SimpleSpeciesReference_t *ssr);


/**
 * @return the species for this SimpleSpeciesReference.
 */
LIBSBML_EXTERN
const char *
SimpleSpeciesReference_getSpecies (const SimpleSpeciesReference_t *ssr);

/**
 * @return 1 if the species for this SimpleSpeciesReference has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
SimpleSpeciesReference_isSetSpecies (const SimpleSpeciesReference_t *ssr);

/**
 * Sets the species of this SimpleSpeciesReference to a copy of sid.
 */
LIBSBML_EXTERN
void
SimpleSpeciesReference_setSpecies ( SimpleSpeciesReference_t *ssr,
                                    const char *sid );


END_C_DECLS


#endif  /** SimpleSpeciesReference_h **/
