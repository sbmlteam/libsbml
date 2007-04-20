/**
 * @file    Utils_UnitDefinition.cpp
 * @brief   Functions acting on a unit definition
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and Japan Science and
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


#include <sbml/units/Utils_UnitDefinition.h>


/** 
 * simplifies the unitDefinition
 */
LIBSBML_EXTERN
void
simplifyUnitDefinition(UnitDefinition * ud)
{
  unsigned int n, i;
  ListOfUnits *  units = ud->getListOfUnits();
  Unit * unit;
  UnitKindList kindsList;
  const char * unitKind;

  for (n = 0; n < ud->getNumUnits(); n++)
  {
    kindsList.append(UnitKind_toString(ud->getUnit(n)->getKind()));
  }
  
  /* if only one unit cannot be simplified any further */
  if (units->size() > 1)
  {
    if (kindsList.contains("dimensionless"))
    {
      /* if contains a dimensionless unit and any others then 
        dimensionless is unecessary */
      for (n = 0; n < units->size(); n++)
      {
        unit = (Unit *) units->get(n);
        if (!strcmp(UnitKind_toString(unit->getKind()), "dimensionless"))
        {
          units->remove(n);
          kindsList.removeUnitKind("dimensionless");
        }
      }
    }

    /* if it contains two units with same kind these must be combined */
    for (n = 0; n < units->size(); n++)
    {
      unit = (Unit *) units->get(n);
      unitKind = UnitKind_toString(unit->getKind());

      /* check that there is only one occurence */
      kindsList.removeUnitKind(unitKind);
      while (kindsList.contains(unitKind)) 
      {
        /* find next occurence and merge */
        for (i = n + 1; i < units->size(); i++)
        {
          if (!strcmp(UnitKind_toString(((Unit *) units->get(i))->getKind()), unitKind))
          {
            mergeUnits(unit, (Unit *) units->get(i));
            units->remove(i);
            kindsList.removeUnitKind(unitKind);
          }
        }
      }
    }
  }

  /* may have cancelled units - in which case exponent will be 0 */
  for (n = 0; n < units->size(); n++)
  {
    unit = (Unit *) units->get(n);
    if (unit->getExponent() == 0)
    {
      units->remove(n);
    }
  }
}

/**
 * returns a unitDefinition which is the argument converted to SI units
 */
LIBSBML_EXTERN
UnitDefinition * 
convertToSI(UnitDefinition * ud)
{
  unsigned int n, p;
  UnitDefinition * newUd = new UnitDefinition();
  UnitDefinition * tempUd;

  newUd->setId(ud->getId());
  newUd->setName(ud->getName());

  for (n = 0; n < ud->getNumUnits(); n++)
  {
    tempUd = convertUnitToSI(ud->getUnit(n));
    for (p = 0; p < tempUd->getNumUnits(); p++)
    {
      newUd->addUnit(tempUd->getUnit(p));
    }
  }

  simplifyUnitDefinition(newUd);
  return newUd;
}

LIBSBML_EXTERN
UnitDefinition * 
convertToSI(const UnitDefinition * ud)
{
  unsigned int n, p;
  UnitDefinition * newUd = new UnitDefinition();
  UnitDefinition * tempUd;

  newUd->setId(ud->getId());
  newUd->setName(ud->getName());

  for (n = 0; n < ud->getNumUnits(); n++)
  {
    tempUd = convertUnitToSI(ud->getUnit(n));
    for (p = 0; p < tempUd->getNumUnits(); p++)
    {
      newUd->addUnit(tempUd->getUnit(p));
    }
  }

  simplifyUnitDefinition(newUd);
  return newUd;
}

/** 
  * returns the unitDefinition with unit kinds in alphabetical order
  */
int compareKinds(const void * u1, const void * u2)
{
  return (*(int*)u1 - *(int*)u2);
}
LIBSBML_EXTERN
void 
orderUnitDefinition(UnitDefinition *ud)
{
  unsigned int n, p;
  ListOfUnits * units = ud->getListOfUnits();
  Unit * unit;
  unsigned int numUnits = units->size();

  int *indexArray = NULL;
  indexArray = new int[units->size()];

  int *initialIndexArray = NULL;
  initialIndexArray = new int[units->size()];

  for (n = 0; n < numUnits; n++)
  {
    indexArray[n] = ((Unit *)units->get(n))->getKind();
    initialIndexArray[n] = ((Unit *)units->get(n))->getKind();
  }

  qsort(indexArray, numUnits, sizeof(int), compareKinds);
 
  /* append units in correct order */
  for (n = 0; n < numUnits; n++)
  {
    for (p = 0; p < numUnits; p++)
    {
      if (indexArray[n] == initialIndexArray[p])
      {
        unit = (Unit *) units->get(p);
        units->append(unit);
        break;
      }
    }
  }

  /* remove originals */
  for (n = 0; n < numUnits; n++)
  {
    units->remove(0);
  }

  delete [] indexArray;
  delete [] initialIndexArray;
}


/** 
  * returns true if unit definitions are identical
  */
LIBSBML_EXTERN
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


/** 
  * returns true if unit definitions are identical
  */
LIBSBML_EXTERN
int 
areIdentical(const UnitDefinition * ud1, const UnitDefinition * ud2)
{
  int identical = 0;
  unsigned int n;

  /* need to order the unitDefinitions so make copies */
  UnitDefinition * ud1Temp = new UnitDefinition();//(UnitDefinition*) ud1->clone();
  UnitDefinition * ud2Temp = new UnitDefinition();//(UnitDefinition*) ud2->clone();

  for ( n = 0; n < ud1->getNumUnits(); n++)
    ud1Temp->addUnit(ud1->getUnit(n));
  for ( n = 0; n < ud2->getNumUnits(); n++)
    ud2Temp->addUnit(ud2->getUnit(n));


  if (ud1->getNumUnits() == ud2->getNumUnits())
  {
    orderUnitDefinition(ud1Temp);
    orderUnitDefinition(ud2Temp);
    
    n = 0;
    while (n < ud1->getNumUnits())
    {
      if (!areIdentical(ud1Temp->getUnit(n), ud2Temp->getUnit(n)))
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


/** 
  * returns true if unit definitions are equivalent
  * i.e. having been converted to SI kinds/offsets are identical
  */
LIBSBML_EXTERN
int 
areEquivalent(const UnitDefinition * ud1, const UnitDefinition * ud2)
{
  int equivalent = 0;
  unsigned int n;

  UnitDefinition * ud1Temp = convertToSI(ud1);
  UnitDefinition * ud2Temp = convertToSI(ud2);

  if (ud1Temp->getNumUnits() == ud2Temp->getNumUnits())
  {
    orderUnitDefinition(ud1Temp);
    orderUnitDefinition(ud2Temp);
    
    n = 0;
    while (n < ud1Temp->getNumUnits())
    {
      if (!areEquivalent(ud1Temp->getUnit(n), ud2Temp->getUnit(n)))
      {
        break;
      }
      else
      {
        n++;
      }
    }
    if (n == ud1Temp->getNumUnits())
    {
      equivalent = 1;
    }
  }

  return equivalent;
}

/** 
  * returns true if unit definitions are equivalent
  * i.e. having been converted to SI kinds/offsets are identical
  */
LIBSBML_EXTERN
int 
areEquivalent(const UnitDefinition * ud1, UnitDefinition * ud2)
{
  int equivalent = 0;
  unsigned int n;

  UnitDefinition * ud1Temp = convertToSI(ud1);
  UnitDefinition * ud2Temp = convertToSI(ud2);

  if (ud1Temp->getNumUnits() == ud2Temp->getNumUnits())
  {
    orderUnitDefinition(ud1Temp);
    orderUnitDefinition(ud2Temp);
    
    n = 0;
    while (n < ud1Temp->getNumUnits())
    {
      if (!areEquivalent(ud1Temp->getUnit(n), ud2Temp->getUnit(n)))
      {
        break;
      }
      else
      {
        n++;
      }
    }
    if (n == ud1Temp->getNumUnits())
    {
      equivalent = 1;
    }
  }

  return equivalent;
}



/** 
 * combines the unitDefinitions 
 */
LIBSBML_EXTERN
void combine(UnitDefinition *ud1, UnitDefinition *ud2)
{
  for (unsigned int n = 0; n < ud2->getNumUnits(); n++)
  {
    ud1->addUnit(ud2->getUnit(n));
  }

  simplifyUnitDefinition(ud1);
}
