/**
 * @file    UnitKind.c
 * @brief   SBML UnitKind enumeration
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/common/common.h>
#include <sbml/UnitKind.h>


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
 * Tests for logical equality between two given UnitKind_t values.
 *
 * This function behaves exactly like C's <tt>==</tt> operator, except for
 * the following two cases:
 *
 *   - UNIT_KIND_LITER == UNIT_KIND_LITRE
 *   - UNIT_KIND_METER == UNIT_KIND_METRE
 *
 * where C would yield false (since each of the above is a distinct
 * enumeration value), UnitKind_equals() returns true.
 *
 * @param uk1 a UnitKind_t enumeration value 
 * @param uk2 the UnitKind_t enumeration value to compare to @p uk1
 *
 * @return nonzero (for true) if uk1 is logically equivalent to uk2,
 * zero (0) otherwise.
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
 * Converts a string to its corresponding UnitKind_t enumeration value.
 *
 * @param name a string, the name of a predefined base unit in SBML
 * 
 * @return a value from UnitKind_t corresponding to the given name
 * (determined in a case-insensitive manner).
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
 * Converts a UnitKind_t enumeration value to a text string equivalent.
 *
 * @param uk the UnitKind_t value to convert
 *
 * @return the name of the given UnitKind.
 *
 * @note The string returned is a static data value.  caller does not own
 * the returned string and is therefore not allowed to modify it.
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
 * Predicate for testing whether a given string corresponds to a
 * predefined UnitKind_t enumeration value.
 *
 * @return nonzero (for true) if string is the name of a valid
 * UnitKind_t enumeration value, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
UnitKind_isValidUnitKindString (const char *string, unsigned int level, unsigned int version)
{
  UnitKind_t uk = UnitKind_forName(string);
  if (level == 1)
  {
    return uk != UNIT_KIND_INVALID;
  }
  else
  {
    if (uk == UNIT_KIND_METER || uk == UNIT_KIND_LITER)
      return 0;
    else if (version > 1 && uk == UNIT_KIND_CELSIUS)
      return 0;
    else
      return uk != UNIT_KIND_INVALID;
  }
}
