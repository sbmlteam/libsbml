/**
 * Filename    : UnitKind.c
 * Description : SBML UnitKind enumeration
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-11-26
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


#include "sbml/UnitKind.h"


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
  UnitKind_t uk  = UNIT_KIND_INVALID;
  UnitKind_t lo  = UNIT_KIND_AMPERE;
  UnitKind_t hi  = UNIT_KIND_WEBER;
  UnitKind_t mid;

  int cond;


  /* Proceed iff name is not NULL and name is not an empty string */
  if (name && *name)
  {
    /* Classic Binary Search */
    while (lo <= hi)
    {
      mid  = (lo + hi) / 2;
      cond = strcmp_insensitive(name, UNIT_KIND_STRINGS[mid]);
      
      if (cond < 0)
      {
        hi = mid - 1;
      }
      else if (cond > 0)
      {
        lo = mid + 1;
      }
      else
      {
        uk = mid;
        break;
      }
    }
  }

  return uk;
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
