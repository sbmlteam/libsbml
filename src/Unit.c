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


#include "sbml/Unit.h"


/**
 * Creates a new Unit and returns a pointer to it.
 */
LIBSBML_EXTERN
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
 *   Unit_setKind(kind); Unit_setExponent(exponent); ...;
 */
LIBSBML_EXTERN
Unit_t *
Unit_createWith (UnitKind_t kind, int exponent, int scale)
{
  Unit_t *u = Unit_create();


  Unit_setKind    ( u, kind     );
  Unit_setExponent( u, exponent );
  Unit_setScale   ( u, scale    );

  return u;
}


/**
 * Frees the given Unit.
 */
LIBSBML_EXTERN
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
LIBSBML_EXTERN
void
Unit_initDefaults (Unit_t *u)
{
  Unit_setExponent( u, 1 );
  Unit_setScale   ( u, 0 );
}


/**
 * @return the kind of this Unit.
 */
LIBSBML_EXTERN
UnitKind_t
Unit_getKind (const Unit_t *u)
{
  return u->kind;
}


/**
 * @return the exponent of this Unit.
 */
LIBSBML_EXTERN
int
Unit_getExponent (const Unit_t *u)
{
  return u->exponent;
}


/**
 * @return the scale of this Unit.
 */
LIBSBML_EXTERN
int
Unit_getScale (const Unit_t *u)
{
  return u->scale;
}


/**
 * @return 1 if the kind of this Unit has been set, 0 otherwise.
 */
LIBSBML_EXTERN
UnitKind_t
Unit_isSetKind (const Unit_t *u)
{
  return (u->kind != UNIT_KIND_INVALID);
}


/**
 * @return 1 if the exponent of this Unit has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSetExponent (const Unit_t *u)
{
  return u->isSet.exponent;
}


/**
 * @return 1 if the scale of this Unit has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSetScale (const Unit_t *u)
{
  return u->isSet.scale;
}


/**
 * Sets the kind of this Unit to the given UnitKind.
 */
LIBSBML_EXTERN
void
Unit_setKind (Unit_t *u, UnitKind_t kind)
{
  u->kind = kind;
}


/**
 * Sets the exponent field of this Unit to value and marks the field as
 * set.
 */
LIBSBML_EXTERN
int
Unit_setExponent (Unit_t *u, int value)
{
  u->exponent       = value;
  u->isSet.exponent = 1;
}


/**
 * Sets the scale field of this Unit to value and marks the field as set.
 */
LIBSBML_EXTERN
int
Unit_setScale (Unit_t *u, int value)
{
  u->scale       = value;
  u->isSet.scale = 1;
}


/**
 * Marks the exponent field of this Unit as unset.
 */
LIBSBML_EXTERN
int
Unit_unsetExponent (Unit_t *u)
{
  u->isSet.exponent = 0;
}


/**
 * Marks the scale field of this Unit as unset.
 */
LIBSBML_EXTERN
int
Unit_unsetScale (Unit_t *u)
{
  u->isSet.scale = 0;
}
