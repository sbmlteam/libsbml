/**
 * Filename    : Compartment.h
 * Description : SBML Compartment
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-11-13
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


#ifndef Compartment_h
#define Compartment_h


#include "SBase.h"


#ifdef __cplusplus
extern "C" {
#endif


typedef struct
{
  SBASE_FIELDS;
  char   *name;
  double  volume;
  char   *units;
  char   *outside;
} Compartment_t;


/**
 * Creates a new Compartment and returns a pointer to it.
 */
Compartment_t *
Compartment_create (void);

/**
 * Creates a new Compartment with the given name, volume, units and outside
 * and returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   Compartment_t *c = Compartment_create();
 *   Compartment_setName(c, name); c->volume = volume; ... ;
 */
Compartment_t *
Compartment_createWith ( const char *name,  double volume,
                         const char *units, const char *outside );

/**
 * Frees the given Compartment.
 */
void
Compartment_free (Compartment_t *c);

/**
 * Initializes the fields of this Compartment to their defaults:
 *
 *   - volume = 1.0
 */
void
Compartment_initDefaults (Compartment_t *c);

/**
 * Sets the name field of this Compartment to a copy of sname.
 */
void
Compartment_setName (Compartment_t *c, const char *sname);

/**
 * Sets the units field of this Compartment to a copy of sname.
 */
void
Compartment_setUnits (Compartment_t *c, const char *sname);

/**
 * Sets the outside field of this Compartment to a copy of sname.
 */
void
Compartment_setOutside (Compartment_t *c, const char *sname);


#ifdef __cplusplus
}
#endif


#endif  /** Compartment_h **/
