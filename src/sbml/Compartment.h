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


#include "common.h"
#include "SBase.h"


BEGIN_C_DECLS


typedef struct
{
  SBASE_FIELDS;

  char   *name;
  double  volume;
  char   *units;
  char   *outside;

  struct
  {
    unsigned int volume:1;
  } isSet;

} Compartment_t;


/**
 * Creates a new Compartment and returns a pointer to it.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_create (void);

/**
 * Creates a new Compartment with the given name, volume, units and outside
 * and returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   Compartment_t *c = Compartment_create();
 *   Compartment_setName(c, name); Compartment_setVolume(c, volume); ... ;
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_createWith ( const char *name,  double volume,
                         const char *units, const char *outside );

/**
 * Frees the given Compartment.
 */
LIBSBML_EXTERN
void
Compartment_free (Compartment_t *c);

/**
 * Initializes the fields of this Compartment to their defaults:
 *
 *   - volume = 1.0
 */
LIBSBML_EXTERN
void
Compartment_initDefaults (Compartment_t *c);


/**
 * @return the name of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getName (const Compartment_t *c);

/**
 * @return the volume of this Compartment.
 */
LIBSBML_EXTERN
double
Compartment_getVolume (const Compartment_t *c);

/**
 * @return the units of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getUnits (const Compartment_t *c);

/**
 * @return the outside of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getOutside (const Compartment_t *c);


/**
 * @return 1 if the name of this Compartment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetName (const Compartment_t *c);

/**
 * @return 1 if the volume of this Compartment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetVolume (const Compartment_t *c);

/**
 * @return 1 if the units of this Compartment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetUnits (const Compartment_t *c);

/**
 * @return 1 if the outside of this Compartment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetOutside (const Compartment_t *c);


/**
 * Sets the name of this Compartment to a copy of sname.
 */
LIBSBML_EXTERN
void
Compartment_setName (Compartment_t *c, const char *sname);

/**
 * Sets the volume of this Compartment to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Compartment_setVolume (Compartment_t *c, double value);

/**
 * Sets the units of this Compartment to a copy of sname.
 */
LIBSBML_EXTERN
void
Compartment_setUnits (Compartment_t *c, const char *sname);

/**
 * Sets the outside of this Compartment to a copy of sname.
 */
LIBSBML_EXTERN
void
Compartment_setOutside (Compartment_t *c, const char *sname);


/**
 * Marks the volume of this Compartment as unset.
 */
LIBSBML_EXTERN
void
Compartment_unsetVolume (Compartment_t *c);


END_C_DECLS


#endif  /** Compartment_h **/
