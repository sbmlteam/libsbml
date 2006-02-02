/**
 * \file    Utils_Unit.cpp
 * \brief   Functions acting on a unit
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


#include "Utils_Unit.h"


/** 
  * alters the multiplier so that scale = 0
  * eg 1 mm can be expressed as multipier = 1 scale = -3
  * or as multiplier = 0.001 scale = 0
  */
LIBSBML_EXTERN
void 
removeScale(Unit * unit)
{
  double scaleFactor = pow(10.0, unit->getScale());
  double newMultiplier = unit->getMultiplier() * scaleFactor;
  unit->setMultiplier(newMultiplier);
  unit->setScale(0);
}


/** 
  * returns a unit which is the product of the first unit by the second
  * this function applies both units are of the same kind
  */
LIBSBML_EXTERN
void
mergeUnits(Unit * unit1, Unit * unit2)
{
  int newExponent;
  double newMultiplier;

  /* only applies if units have same kind */
  if (strcmp(UnitKind_toString(unit1->getKind()), UnitKind_toString(unit2->getKind())))
    return;

  /* not yet implemented if offsets != 0 */
  if (unit1->getOffset() != 0 || unit2->getOffset() != 0)
    return;

  removeScale(unit1);
  removeScale(unit2);

  newExponent = unit1->getExponent() + unit2->getExponent();

  if (newExponent == 0)
  {
    newMultiplier = 1;
  }
  else
  {
    newMultiplier = pow(pow(unit1->getMultiplier(), unit1->getExponent())*
      pow(unit2->getMultiplier(), unit2->getExponent()), 1/(double)(newExponent));
  }
    
  unit1->setScale(0);
  unit1->setExponent(newExponent);
  unit1->setMultiplier(newMultiplier);
}

/**
 * returns a unitdefinition which is the argument converted to SI units
 */
LIBSBML_EXTERN
UnitDefinition * 
convertUnitToSI(Unit * unit)
{
  UnitKind_t uKind = unit->getKind();
  Unit * newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
  UnitDefinition * ud = new UnitDefinition();

  removeScale(newUnit);

  switch (uKind)
  {
    case UNIT_KIND_AMPERE:
      /* Ampere is the SI unit of current */
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_BECQUEREL:
    case UNIT_KIND_HERTZ:
      /* 1 becquerel = 1 sec^-1 = (0.1 sec)^-1 */
      /* 1 hertz = 1 sec^-1 = (0.1 sec) ^-1*/
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setExponent(newUnit->getExponent()*-1);
      newUnit->setMultiplier(pow(newUnit->getMultiplier(), -1.0)); 
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_CANDELA:
      /* candela is the SI unit of luminous intensity */
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_CELSIUS:
      /* 1 celsius = 1 Kelvin */
      newUnit->setKind(UNIT_KIND_KELVIN);
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_COULOMB:
      /* 1 coulomb = 1 Ampere second */
      newUnit->setKind(UNIT_KIND_AMPERE);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1);
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_DIMENSIONLESS:
    case UNIT_KIND_ITEM:
    case UNIT_KIND_RADIAN:
    case UNIT_KIND_STERADIAN:
      /* all dimensionless */
      newUnit->setKind(UNIT_KIND_DIMENSIONLESS);
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_FARAD:
      /* 1 Farad = 1 m^-2 kg^-1 s^4 A^2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(4*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_GRAM:
      /* 1 gram = 0.001 Kg */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(0.001 * newUnit->getMultiplier());
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_GRAY:
    case UNIT_KIND_SIEVERT:
      /* 1 Gray = 1 m^2 sec^-2 */
      /* 1 Sievert = 1 m^2 sec^-2 */
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_HENRY:
      /* 1 Henry = 1 m^2 kg s^-2 A^-2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(pow(newUnit->getMultiplier(), -1.0/2.0));
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_JOULE:
      /* 1 joule = 1 m^2 kg s^-2 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_KATAL:
      /* 1 katal = 1 mol s^-1 */
      newUnit->setKind(UNIT_KIND_MOLE);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(*newUnit);
     break;

    case UNIT_KIND_KELVIN:
      /* Kelvin is the SI unit of temperature */
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_KILOGRAM:
      /* Kilogram is the SI unit of mass */
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_LITER:
    case UNIT_KIND_LITRE:
      /* 1 litre = 0.001 m^3 = (0.1 m)^3*/ 
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setExponent(newUnit->getExponent()*3);
      newUnit->setMultiplier(pow((newUnit->getMultiplier() * 0.001), 1.0/3.0)); 
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_LUMEN:
      /* 1 lumen = 1 candela*/ 
      newUnit->setKind(UNIT_KIND_CANDELA);
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_LUX:
      /* 1 lux = 1 candela m^-2*/ 
      newUnit->setKind(UNIT_KIND_CANDELA);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_METER:
    case UNIT_KIND_METRE:
      /* metre is the SI unit of length */
      newUnit->setKind(UNIT_KIND_METRE);
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_MOLE:
      /* mole is the SI unit of substance */
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_NEWTON:
      /* 1 newton = 1 m kg s^-2 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_OHM:
      /* 1 ohm = 1 m^2 kg s^-3 A^-2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(pow(newUnit->getMultiplier(), -1.0/2.0));
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_PASCAL:
      /* 1 pascal = 1 m^-1 kg s^-2 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_SECOND:
      /* second is the SI unit of time */
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_SIEMENS:
      /* 1 siemen = 1 m^-2 kg^-1 s^3 A^2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(pow(newUnit->getMultiplier(), 1.0/2.0));
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(3*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_TESLA:
      /* 1 tesla = 1 kg s^-2 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(pow(newUnit->getMultiplier(), -1.0));
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_VOLT:
      /* 1 volt = 1 m^2 kg s^-3 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(pow(newUnit->getMultiplier(), -1.0));
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_WATT:
      /* 1 watt = 1 m^2 kg s^-3 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_WEBER:
      /* 1 weber = 1 m^2 kg s^-2 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(pow(newUnit->getMultiplier(), -1.0));
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier(), unit->getOffset());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(*newUnit);
      break;

    case UNIT_KIND_INVALID:
      break;
  }

  return ud;
}


/** 
  * returns true if units are identical
  */
LIBSBML_EXTERN
int 
areIdentical(Unit * unit1, Unit * unit2)
{
  int identical = 0;

  if (!strcmp(UnitKind_toString(unit1->getKind()), UnitKind_toString(unit2->getKind())))
  {
    if ((unit1->getMultiplier() == unit2->getMultiplier())
      && (unit1->getScale()     == unit2->getScale())
      && (unit1->getOffset()    == unit2->getOffset())
      && (unit1->getExponent()  == unit2->getExponent()))
    {
      identical = 1;
    }
  }

  return identical;
}


/** 
  * returns true if units are equivalent
  * units are equivalent if they have same 
  * kind and same exponent (and same offset)
  */
LIBSBML_EXTERN
int 
areEquivalent(Unit * unit1, Unit * unit2)
{
  int equivalent = 0;

  if (!strcmp(UnitKind_toString(unit1->getKind()), UnitKind_toString(unit2->getKind())))
  {
    if ( (unit1->getOffset()    == unit2->getOffset())
      && (unit1->getExponent()  == unit2->getExponent()))
    {
      equivalent = 1;
    }
  }

  return equivalent;
}


/** 
  * alters the multiplier so that scale = 0
  * eg 1 mm can be expressed as multipier = 1 scale = -3
  * or as multiplier = 0.001 scale = 0
  */
LIBSBML_EXTERN
void Unit_removeScale(Unit_t * unit)
{
  removeScale(static_cast<Unit*>(unit));
}
