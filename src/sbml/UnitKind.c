/**
 * \file    UnitKind.c
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


#include <sbml/common/common.h>
#include "UnitKind.h"


const char* UNIT_KIND_STRINGS[] =
{
    "ampere"
  , "becquerel"
  , "candela"
  , "Celsius"
  , "coulomb"
  , "dimensionless"
  , "farad"
  , "gram"
  , "gray"
  , "henry"
  , "hertz"
  , "item"
  , "joule"
  , "katal"
  , "kelvin"
  , "kilogram"
  , "liter"
  , "litre"
  , "lumen"
  , "lux"
  , "meter"
  , "metre"
  , "mole"
  , "newton"
  , "ohm"
  , "pascal"
  , "radian"
  , "second"
  , "siemens"
  , "sievert"
  , "steradian"
  , "tesla"
  , "volt"
  , "watt"
  , "weber"
  , "(Invalid UnitKind)"
};


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
 * @return true (!0) if uk1 is logically equivalent to uk2, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
UnitKind_equals (UnitKind_t uk1, UnitKind_t uk2)
{
  return
    (uk1 == uk2) ||
    ( (uk1 == UNIT_KIND_LITER) && (uk2 == UNIT_KIND_LITRE) ) ||
    ( (uk1 == UNIT_KIND_LITRE) && (uk2 == UNIT_KIND_LITER) ) ||
    ( (uk1 == UNIT_KIND_METER) && (uk2 == UNIT_KIND_METRE) ) ||
    ( (uk1 == UNIT_KIND_METRE) && (uk2 == UNIT_KIND_METER) );
}


/**
 * Returns the UnitKind with the given name (case-insensitive).
 */
LIBSBML_EXTERN
UnitKind_t
UnitKind_forName (const char *name)
{
  const UnitKind_t lo = UNIT_KIND_AMPERE;
  const UnitKind_t hi = UNIT_KIND_WEBER;


  return util_bsearchStringsI(UNIT_KIND_STRINGS, name, lo, hi);
}


/**
 * Returns the name of the given UnitKind.  The caller does not own the
 * returned string and is therefore not allowed to modify it.
 */
LIBSBML_EXTERN
const char *
UnitKind_toString (UnitKind_t uk)
{
  if ( (uk < UNIT_KIND_AMPERE) || (uk > UNIT_KIND_INVALID) )
  {
    uk = UNIT_KIND_INVALID;
  }

  return UNIT_KIND_STRINGS[uk];
}


/**
 * Returns nonzero if string is the name of a valid unitKind.
 */
LIBSBML_EXTERN
int
UnitKind_isValidUnitKindString (const char *string)
{
  return UnitKind_forName(string) != UNIT_KIND_INVALID;
}
