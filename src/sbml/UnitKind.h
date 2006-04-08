/**
 * \file    UnitKind.h
 * \brief   SBML UnitKind enumeration
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#ifndef UnitKind_h
#define UnitKind_h


#include <sbml/common/extern.h>


BEGIN_C_DECLS


typedef enum
{
    UNIT_KIND_AMPERE
  , UNIT_KIND_BECQUEREL
  , UNIT_KIND_CANDELA
  , UNIT_KIND_CELSIUS
  , UNIT_KIND_COULOMB
  , UNIT_KIND_DIMENSIONLESS
  , UNIT_KIND_FARAD
  , UNIT_KIND_GRAM
  , UNIT_KIND_GRAY
  , UNIT_KIND_HENRY
  , UNIT_KIND_HERTZ
  , UNIT_KIND_ITEM
  , UNIT_KIND_JOULE
  , UNIT_KIND_KATAL
  , UNIT_KIND_KELVIN
  , UNIT_KIND_KILOGRAM
  , UNIT_KIND_LITER
  , UNIT_KIND_LITRE
  , UNIT_KIND_LUMEN
  , UNIT_KIND_LUX
  , UNIT_KIND_METER
  , UNIT_KIND_METRE
  , UNIT_KIND_MOLE
  , UNIT_KIND_NEWTON
  , UNIT_KIND_OHM
  , UNIT_KIND_PASCAL
  , UNIT_KIND_RADIAN
  , UNIT_KIND_SECOND
  , UNIT_KIND_SIEMENS
  , UNIT_KIND_SIEVERT
  , UNIT_KIND_STERADIAN
  , UNIT_KIND_TESLA
  , UNIT_KIND_VOLT
  , UNIT_KIND_WATT
  , UNIT_KIND_WEBER
  , UNIT_KIND_INVALID
} UnitKind_t;


/**
 * Tests for logical equality between two UnitKinds.  This function behaves
 * exactly like C's == operator, except for the following two cases:
 *
 *   - UNIT_KIND_LITER == UNIT_KIND_LITRE
 *   - UNIT_KIND_METER == UNIT_KIND_METRE
 *
 * where C would yield false (since each of the above is a distinct
 * enumeration value), UnitKind_equals(...) yields true.
 *
 * @return true (!0) if uk1 is logically equivalent to uk2, false (0) otherwise.
 */
LIBSBML_EXTERN
int
UnitKind_equals (UnitKind_t uk1, UnitKind_t uk2);

/**
 * Returns the UnitKind with the given name (case-insensitive).
 */
LIBSBML_EXTERN
UnitKind_t
UnitKind_forName (const char *name);

/**
 * Returns the name of the given UnitKind.  The caller does not own the
 * returned string and is therefore not allowed to modify it.
 */
LIBSBML_EXTERN
const char *
UnitKind_toString (UnitKind_t uk);

/**
 * Returns nonzero if string is the name of a valid unitKind.
 */
LIBSBML_EXTERN
int
UnitKind_isValidUnitKindString (const char *string);


END_C_DECLS


#endif  /** UnitKind_h **/
