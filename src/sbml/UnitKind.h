/**
 * @file    UnitKind.h
 * @brief   Definition of %SBML's UnitKind enumeration
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @var typedef enum UnitKind_t
 * @brief Enumeration of predefined SBML base units
 *
 * For more information, please refer to the class documentation for Unit.
 * 
 * @see UnitDefinition_t
 * @see Unit_t
 */

#ifndef UnitKind_h
#define UnitKind_h


#include <sbml/common/extern.h>

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * @var typedef enum UnitKind_t
 */
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
 * Tests for logical equality between two given UnitKind_t values.
 *
 * This function behaves exactly like C's <tt>==</tt> operator, except for
 * the following two cases:
 * 
 * @li UNIT_KIND_LITER <code>==</code> UNIT_KIND_LITRE
 * @li UNIT_KIND_METER <code>==</code> UNIT_KIND_METRE
 *
 * C equality comparison would yield false (because each of the above is a
 * distinct enumeration value), but UnitKind_equals() returns true.
 *
 * @param uk1 a UnitKind_t enumeration value 
 * @param uk2 the UnitKind_t enumeration value to compare to @p uk1
 *
 * @return nonzero (for true) if uk1 is logically equivalent to uk2,
 * zero (0) otherwise.
 *
 * @note For more information about the UnitKind_t enumeration, please
 * refer to the class documentation for Unit.
 */
LIBSBML_EXTERN
int
UnitKind_equals (UnitKind_t uk1, UnitKind_t uk2);


/**
 * Converts a string to its corresponding UnitKind_t enumeration value.
 *
 * @param name a string, the name of a predefined base unit in SBML
 * 
 * @return a value from UnitKind_t corresponding to the given name
 * (determined in a case-insensitive manner).
 *
 * @note For more information about the UnitKind_t enumeration, please
 * refer to the class documentation for Unit.
 */
LIBSBML_EXTERN
UnitKind_t
UnitKind_forName (const char *name);


/**
 * Converts a UnitKind_t enumeration value to a text string equivalent.
 *
 * @param uk the UnitKind_t value to convert
 *
 * @return the name of the given UnitKind.
 *
 * @note The string returned is a static data value.  The caller does not
 * own the returned string and is therefore not allowed to modify it.
 *
 * @note For more information about the UnitKind_t enumeration, please
 * refer to the class documentation for Unit.
 */
LIBSBML_EXTERN
const char *
UnitKind_toString (UnitKind_t uk);


/**
 * Predicate for testing whether a given string corresponds to a
 * predefined UnitKind_t enumeration value.
 *
 * @return nonzero (for true) if string is the name of a valid
 * UnitKind_t enumeration value, zero (0) otherwise.
 *
 * @note For more information about the UnitKind_t enumeration, please
 * refer to the class documentation for Unit.
 */
LIBSBML_EXTERN
int
UnitKind_isValidUnitKindString (const char *string, unsigned int level, unsigned int version);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /** UnitKind_h **/
