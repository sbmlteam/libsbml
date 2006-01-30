/**
 * \file    Utils_UnitDefinition.cpp
 * \brief   Functions acting on a unit definition
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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
 *     Sarah Keating
 *
 *     The SBML Team
 *     Science and Technology Research Institute
 *     University of Hertfordshire
 *     Hatfield, UK
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "Utils_UnitDefinition.h"


//
// FIXME: UNIT_KIND_STRINGS should really be private to UnitKind.c.  Can we
// FIXME: remove the dependence on UNIT_KIND_STRINGS in
// FIXME: orderUnitDefinition()?
//
extern const char** UNIT_KIND_STRINGS;


/** 
 * simplifies the unitDefinition
 */
//LIBSBML_EXTERN
void
simplifyUnitDefinition(UnitDefinition * ud)
{
  unsigned int n, i;
  ListOf & units = ud->getListOfUnits();
  Unit * unit;
  UnitKindList kindsList;
  const char * unitKind;

  for (n = 0; n < ud->getNumUnits(); n++)
  {
    kindsList.append(UnitKind_toString(ud->getUnit(n)->getKind()));
  }
  
  /* if only one unit cannot be simplified any further */
  if (units.getNumItems() > 1)
  {
    if (kindsList.contains("dimensionless"))
    {
      /* if contains a dimensionless unit and any others then 
        dimensionless is unecessary */
      for (n = 0; n < units.getNumItems(); n++)
      {
        unit = (Unit *) units.get(n);
        if (!strcmp(UnitKind_toString(unit->getKind()), "dimensionless"))
        {
          units.remove(n);
          kindsList.removeUnitKind("dimensionless");
        }
      }
    }

    /* if it contains two units with same kind these must be combined */
    for (n = 0; n < units.getNumItems(); n++)
    {
      unit = (Unit *) units.get(n);
      unitKind = UnitKind_toString(unit->getKind());

      /* check that there is only one occurence */
      kindsList.removeUnitKind(unitKind);
      while (kindsList.contains(unitKind)) 
      {
        /* find next occurence and merge */
        for (i = n + 1; i < units.getNumItems(); i++)
        {
          if (!strcmp(UnitKind_toString(((Unit *) units.get(i))->getKind()), unitKind))
          {
            mergeUnits(unit, (Unit *) units.get(i));
            units.remove(i);
            kindsList.removeUnitKind(unitKind);
          }
        }
      }
    }
  }

  /* may have cancelled units - in which case exponent will be 0 */
  for (n = 0; n < units.getNumItems(); n++)
  {
    unit = (Unit *) units.get(n);
    if (unit->getExponent() == 0)
    {
      units.remove(n);
    }
  }
}

/**
 * returns a unitDefinition which is the argument converted to SI units
 */
//LIBSBML_EXTERN
UnitDefinition * 
convertToSI(UnitDefinition * ud)
{
  unsigned int n, p;
  UnitDefinition * newUd = new UnitDefinition();
  UnitDefinition * tempUd;

  newUd = convertUnitToSI(ud->getUnit(0));
  newUd->setId(ud->getId());
  newUd->setName(ud->getName());

  for (n = 1; n < ud->getNumUnits(); n++)
  {
    tempUd = convertUnitToSI(ud->getUnit(n));
    for (p = 0; p < tempUd->getNumUnits(); p++)
    {
      newUd->addUnit(*(tempUd->getUnit(p)));
    }
  }

  simplifyUnitDefinition(newUd);
  return newUd;
}


/** 
  * returns the unitDefinition with unit kinds in alphabetical order
  */
  //LIBSBML_EXTERN
void 
orderUnitDefinition(UnitDefinition *ud)
{
  unsigned int n;
  ListOf & units = ud->getListOfUnits();
  Unit * unit;
  const char * unitKind;
  const UnitKind_t lo = UNIT_KIND_AMPERE;
  const UnitKind_t hi = UNIT_KIND_WEBER;

  int sorted = 0;
  int first, next, prev, count;
  unit = (Unit *) units.get(0);
  unitKind = UnitKind_toString(unit->getKind());

  first = util_bsearchStringsI(UNIT_KIND_STRINGS, unitKind, lo, hi);

  while (sorted < 1)
  {
    prev = first;
    count = 0;
    for (n = 1; n < ud->getNumUnits(); n++)
    {
      unit = (Unit *) units.get(n);
      unitKind = UnitKind_toString(unit->getKind());
      next = util_bsearchStringsI(UNIT_KIND_STRINGS, unitKind, lo, hi);

      if (next < prev)
      {
        units.remove(n);
        units.prepend(unit);
        first = next;
        count = count + 1;
      }
    }
    if (count == 0)
    {
      sorted = 1;
    }
  }
}


/** 
  * returns true if unit definitions are identical
  */
//LIBSBML_EXTERN
int 
areIdentical(UnitDefinition * ud1, UnitDefinition * ud2)
{
  int identical = 0;
  unsigned int n;

  if (ud1->getNumUnits() == ud2->getNumUnits())
  {
    orderUnitDefinition(ud1);
    orderUnitDefinition(ud2);
    
    n = 0;
    while (n < ud1->getNumUnits())
    {
      if (!areIdentical(ud1->getUnit(n), ud2->getUnit(n)))
      {
        break;
      }
      else
      {
        n++;
      }
    }
    if (n == ud1->getNumUnits())
    {
      identical = 1;
    }
  }

  return identical;
}
