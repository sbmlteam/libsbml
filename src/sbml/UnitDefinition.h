/**
 * Filename    : UnitDefinition.h
 * Description : SBML UnitDefinition
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


#ifndef UnitDefinition_h
#define UnitDefinition_h


#include "List.h"
#include "SBase.h"
#include "Unit.h"


#ifdef __cplusplus
extern "C" {
#endif


typedef struct
{
  SBASE_FIELDS;
  char   *name;
  List_t *unit;
} UnitDefinition_t;


/**
 * Creates a new UnitDefinition and returns a pointer to it.
 */
UnitDefinition_t *
UnitDefinition_create (void);

/**
 * Creates a new UnitDefinition with the given name and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   UnitDefinition_setName(UnitDefinition_create(), sname);
 */
UnitDefinition_t *
UnitDefinition_createWith (const char *sname);

/**
 * Frees the given UnitDefinition.
 */
void
UnitDefinition_free (UnitDefinition_t *ud);

/**
 * Sets the name field of this UnitDefinition to a copy of sname.
 */
void
UnitDefinition_setName(UnitDefinition_t *ud, const char *sname);

/**
 * Adds the given Unit to this UnitDefinition.
 */
void
UnitDefinition_addUnit(UnitDefinition_t *ud, Unit_t *u);

/**
 * @return the nth Unit of this UnitDefinition.
 */
Unit_t *
UnitDefinition_getUnit(const UnitDefinition_t *ud, unsigned int n);

/**
 * @return the number of Units in this UnitDefinition.
 */
unsigned int
UnitDefinition_getNumUnits(const UnitDefinition_t *ud);


#ifdef __cplusplus
}
#endif


#endif  /** UnitDefinition_h **/
