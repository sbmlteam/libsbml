/**
 * Filename    : Unit.c
 * Description : SBML Unit
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
#include "sbml/Unit.h"


/**
 * Creates a new Unit and returns a pointer to it.
 */
Unit_t *
Unit_create (void)
{
  Unit_t *u;


  u = (Unit_t *) safe_calloc(1, sizeof(Unit_t));
 
  SBase_init((SBase_t *) u, SBML_UNIT);
  Unit_initDefaults(u);

  u->kind = UNIT_KIND_INVALID;

  return u;
}


/**
 * Creates a new Unit with the given kind, exponent and scale and returns a
 * pointer to it.  This convenience function is functionally equivalent to:
 *
 *   Unit_t *u = Unit_create();
 *   u->kind = kind; u->exponent = exponent; ... ;
 */
Unit_t *
Unit_createWith(UnitKind_t kind, int exponent, int scale)
{
  Unit_t *u = Unit_create();


  u->kind     = kind;
  u->exponent = exponent;
  u->scale    = scale;

  return u;
}


/**
 * Frees the given Unit.
 */
void
Unit_free (Unit_t *u)
{
  if (u == NULL) return;

  SBase_clear((SBase_t *) u);
  safe_free(u);
}


/**
 * Initializes the fields of this Unit to their defaults:
 *
 *   - exponent = 1
 *   - scale    = 0
 */
void
Unit_initDefaults (Unit_t *u)
{
  u->exponent = 1;
  u->scale    = 0;
}
