/**
 * @file    Utils_UnitDefinition.cpp
 * @brief   Utility functions acting on a UnitDefinition object
 * @author  Sarah Keating
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/units/Utils_UnitDefinition.h>


/** 
 * Simplifies the UnitDefinition so that any Unit occurring
 * within the listOfUnits occurs only once.
 *
 * For example,
 * @n <unitDefinition>
 * @n  <listOfUnits>
 * @n    <unit kind="metre" exponent="1"/>
 * @n    <unit kind="metre" exponent="2"/>
 * @n  </listOfUnits>
 * @n <unitDefinition>
 *
 * simplified would return
 * @n <unitDefinition>
 * @n   <listOfUnits>
 * @n     <unit kind="metre" exponent="3"/>
 * @n   </listOfUnits>
 * @n <unitDefinition>
 *
 * @param ud the UnitDefinition object to be simplified.
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
          if (!strcmp(UnitKind_toString(((Unit *) units->get(i))->getKind()), 
                                                                   unitKind))
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
  unsigned int numUnits = units->size();
  for (n = numUnits; n > 0; n--)
  {
    unit = (Unit *) units->get(n-1);
    if (unit->getExponent() == 0)
    {
      units->remove(n-1);
    }
  }
}


/** @cond doxygen-libsbml-internal */
int compareKinds(const void * u1, const void * u2)
{
  return (*(int*)u1 - *(int*)u2);
}
/** @endcond doxygen-libsbml-internal */

/** 
 * Orders the listOfUnits within the UnitDefinition alphabetically.
 *
 * @param ud the UnitDefinition object to be ordered.
 */
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
 * Returns a UnitDefinition object which is the argument UnitDefinition
 * converted to the SI units.
 *
 * @param ud the UnitDefinition object to convert to SI
 *
 * @return a UnitDefinition object converted to SI units.
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
    delete tempUd;
  }

  simplifyUnitDefinition(newUd);
  return newUd;
}

/**
 * Returns a UnitDefinition object which is the argument UnitDefinition
 * converted to the SI units.
 *
 * @param ud the UnitDefinition object to convert to SI
 *
 * @return a UnitDefinition object converted to SI units.
 */
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
    delete tempUd;
  }

  simplifyUnitDefinition(newUd);
  return newUd;
}


/** 
 * Predicate returning @c true or @c false depending on whether 
 * UnitDefinition objects are identical (all units are identical).
 *
 * @param ud1 the first UnitDefinition object to compare
 * @param ud2 the second UnitDefinition object to compare
 *
 * @return @c true if all the units of ud1 are identical
 * to the units of ud2, @c false otherwise.
 *
 * @note For the purposes of comparison two units can be "identical",
 * i.e. all attributes are an exact match, or "equivalent" i.e. 
 * matching kind and exponent.
 *
 * @see areEquivalent();
 */
LIBSBML_EXTERN
bool 
areIdentical(UnitDefinition * ud1, UnitDefinition * ud2)
{
  bool identical = false;
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
      identical = true;
    }
  }

  return identical;
}


/** 
 * Predicate returning @c true or @c false depending on whether 
 * UnitDefinition objects are identical (all units are identical).
 *
 * @param ud1 the first UnitDefinition object to compare
 * @param ud2 the second UnitDefinition object to compare
 *
 * @return @c true if all the units of ud1 are identical
 * to the units of ud2, @c false otherwise.
 *
 * @note For the purposes of comparison two units can be "identical",
 * i.e. all attributes are an exact match, or "equivalent" i.e. 
 * matching kind and exponent.
 *
 * @see areEquivalent();
 */
LIBSBML_EXTERN
bool 
areIdentical(const UnitDefinition * ud1, const UnitDefinition * ud2)
{
  bool identical = false;
  unsigned int n;

  /* need to order the unitDefinitions so must make copies
   * since the arguments are const
   */
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
      identical = true;
    }
  }

  delete ud1Temp;
  delete ud2Temp;

  return identical;
}


/** 
 * Predicate returning @c true or @c false depending on whether 
 * UnitDefinition objects are equivalent (all units are equivalent).
 *
 * @param ud1 the first UnitDefinition object to compare
 * @param ud2 the second UnitDefinition object to compare
 *
 * @return @c true if all the units of ud1 are equivalent
 * to the units of ud2, @c false otherwise.
 *
 * @note For the purposes of comparison two units can be "identical",
 * i.e. all attributes are an exact match, or "equivalent" i.e. 
 * matching kind and exponent.
 *
 * @see areIdentical();
 */
LIBSBML_EXTERN
bool 
areEquivalent(const UnitDefinition * ud1, const UnitDefinition * ud2)
{
  bool equivalent = false;
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
      equivalent = true;
    }
  }

  delete ud1Temp;
  delete ud2Temp;

  return equivalent;
}

/** 
 * Predicate returning @c true or @c false depending on whether 
 * UnitDefinition objects are equivalent (all units are equivalent).
 *
 * @param ud1 the first UnitDefinition object to compare
 * @param ud2 the second UnitDefinition object to compare
 *
 * @return @c true if all the units of ud1 are equivalent
 * to the units of ud2, @c false otherwise.
 *
 * @note For the purposes of comparison two units can be "identical",
 * i.e. all attributes are an exact match, or "equivalent" i.e. 
 * matching kind and exponent.
 *
 * @see areIdentical();
 */
LIBSBML_EXTERN
bool 
areEquivalent(const UnitDefinition * ud1, UnitDefinition * ud2)
{
  bool equivalent = false;
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
      equivalent = true;
    }
  }

  delete ud1Temp;
  delete ud2Temp;

  return equivalent;
}



/** 
 * Combines two UnitDefinition objects into a single UnitDefinition object
 * which expresses the units of the two objects multiplied.
 *
 * @param ud1 the first UnitDefinition object into which the second is
 * combined
 * @param ud2 the second UnitDefinition object
 */
LIBSBML_EXTERN
void 
combine(UnitDefinition *ud1, UnitDefinition *ud2)
{
  for (unsigned int n = 0; n < ud2->getNumUnits(); n++)
  {
    ud1->addUnit(ud2->getUnit(n));
  }

  simplifyUnitDefinition(ud1);
}

/** 
 * Returns a string that expresses the units symbolised by the UnitDefinition.
 *
 * For example printUnits applied to
 * @n <unitDefinition>
 * @n  <listOfUnits>
 * @n    <unit kind="metre" exponent="1"/>
 * @n    <unit kind="second" exponent="-2"/>
 * @n  </listOfUnits>
 * @n <unitDefinition>
 * @n returns the string 'metre (exponent = 1) second (exponent = -2)'
 *
 * @param ud the UnitDefinition object
 *
 * @return a string expressing the units
 */
LIBSBML_EXTERN
std::string
printUnits(const UnitDefinition * ud)
{
  std::string unitDef;
  for (unsigned int p = 0; p < ud->getNumUnits(); p++)
  {
	  UnitKind_t kind = ud->getUnit(p)->getKind();
	  int exp = ud->getUnit(p)->getExponent();

    char unit[40];
    sprintf(unit, "%s (exponent = %i)", UnitKind_toString(kind), exp);
    unitDef += unit;
    //msg +=  UnitKind_toString(kind);
    //msg += " (exponent = ";
    //msg += exp;
    //msg +=  ")";

	  if (p + 1 < ud->getNumUnits())
	  {
	    unitDef += ", ";
	  }	  
  }

  if (ud->getNumUnits() == 0)
  {
    unitDef = "indeterminable";
  }
  return unitDef;
}

/** @cond doxygen-c-only */

LIBSBML_EXTERN
void 
UnitDefinition_simplifyUnitDefinition(UnitDefinition_t * ud)
{
  simplifyUnitDefinition(static_cast<UnitDefinition*>(ud));
}

LIBSBML_EXTERN
void 
UnitDefinition_orderUnitDefinition(UnitDefinition_t * ud)
{
  orderUnitDefinition(static_cast<UnitDefinition*>(ud));
}

LIBSBML_EXTERN
UnitDefinition_t * 
UnitDefinition_convertToSI(UnitDefinition_t * ud)
{
  return convertToSI(static_cast<UnitDefinition*>(ud));
}

LIBSBML_EXTERN
int 
UnitDefinition_areIdentical(UnitDefinition_t * ud1, UnitDefinition_t * ud2)
{
  return static_cast<int>(areIdentical(static_cast<UnitDefinition*>(ud1),
                                       static_cast<UnitDefinition*>(ud2)));
}

LIBSBML_EXTERN
int 
UnitDefinition_areEquivalent(UnitDefinition_t *ud1 , UnitDefinition_t * ud2)
{
  return static_cast<int>(areEquivalent(static_cast<UnitDefinition*>(ud1),
                                       static_cast<UnitDefinition*>(ud2)));
}

LIBSBML_EXTERN
void 
UnitDefinition_combine(UnitDefinition_t * ud1, UnitDefinition_t * ud2)
{
  combine(static_cast<UnitDefinition*>(ud1),
                                       static_cast<UnitDefinition*>(ud2));
}


LIBSBML_EXTERN
const char *
UnitDefinition_printUnits(UnitDefinition_t * ud)
{
  return printUnits(static_cast<UnitDefinition*>(ud)).c_str();
}

/** @endcond doxygen-c-only */
