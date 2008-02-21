/**
 * @file    UnitKind.h
 * @brief   Definition of %SBML's UnitKind enumeration
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
 *------------------------------------------------------------------------- -->
 *
 * @var typedef enum UnitKind_t
 * @brief Enumeration of predefined SBML base units
 *
 * Please refer to the class documentation for Unit.
 * 
 * @see UnitDefinition_t
 * @see Unit_t
 */

#ifndef UnitKind_h
#define UnitKind_h


#include <sbml/common/extern.h>


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


/*-----------------------------------------------------------------------------
 * See the .c file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


LIBSBML_EXTERN
int
UnitKind_equals (UnitKind_t uk1, UnitKind_t uk2);


LIBSBML_EXTERN
UnitKind_t
UnitKind_forName (const char *name);


LIBSBML_EXTERN
const char *
UnitKind_toString (UnitKind_t uk);


LIBSBML_EXTERN
int
UnitKind_isValidUnitKindString (const char *string, unsigned int level, unsigned int version);


END_C_DECLS


#endif  /** UnitKind_h **/
