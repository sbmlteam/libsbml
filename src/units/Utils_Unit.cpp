/**
 * @file    Utils_Unit.cpp
 * @brief   Utility functions acting on a Unit object
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

#include <sbml/units/Utils_Unit.h>


/** 
 * Manipulates the attributes of the Unit to express the unit with the 
 * value of the scale attribute reduced to zero.
 *
 * For example, 1 mm can be expressed as a Unit with kind="metre"
 * multipier="1" scale="-3" exponent="1". It can also be expressed as
 * a Unit with kind="metre" multiplier="0.001" scale="0" exponent="1".
 *
 * @param unit the Unit object to manipulate.
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
 * Merges two Unit objects with the same kind attribute into
 * a single Unit.
 * 
 * For example 
 * <unit kind="metre" exponent="2"/>
 * <unit kind="metre" exponent="1"/>
 * merge to become
 * <unit kind="metre" exponent="3"/>
 *
 * @param unit1 the first Unit object into which the second is merged
 * @param unit2 the Unit object to merge with the first
 */
LIBSBML_EXTERN
void
mergeUnits(Unit * unit1, Unit * unit2)
{
  int newExponent;
  double newMultiplier;

  /* only applies if units have same kind */
  if (strcmp(UnitKind_toString(unit1->getKind()), 
             UnitKind_toString(unit2->getKind())))
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
      pow(unit2->getMultiplier(), unit2->getExponent()), 
                                                  1/(double)(newExponent));
  }
    
  unit1->setScale(0);
  unit1->setExponent(newExponent);
  unit1->setMultiplier(newMultiplier);
}

/**
 * Returns a UnitDefinition object which contains the argument Unit
 * converted to the appropriate SI unit.
 *
 * @param unit the Unit object to convert to SI
 *
 * @return a UnitDefinition object containing the SI unit.
 */
LIBSBML_EXTERN
UnitDefinition * 
convertUnitToSI(Unit * unit)
{
  UnitKind_t uKind = unit->getKind();
  Unit * newUnit = new Unit(uKind, unit->getExponent(), 
                                     unit->getScale(), unit->getMultiplier());
  UnitDefinition * ud = new UnitDefinition();

  removeScale(newUnit);

  switch (uKind)
  {
    case UNIT_KIND_AMPERE:
      /* Ampere is the SI unit of current */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_BECQUEREL:
    case UNIT_KIND_HERTZ:
      /* 1 becquerel = 1 sec^-1 = (0.1 sec)^-1 */
      /* 1 hertz = 1 sec^-1 = (0.1 sec) ^-1*/
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setExponent(newUnit->getExponent()*-1);
      newUnit->setMultiplier(pow(newUnit->getMultiplier(), -1.0)); 
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_CANDELA:
      /* candela is the SI unit of luminous intensity */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_CELSIUS:
      /* 1 celsius = 1 Kelvin + 273.15*/
      newUnit->setKind(UNIT_KIND_KELVIN);
      newUnit->setOffset(273.15);
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_COULOMB:
      /* 1 coulomb = 1 Ampere second */
      newUnit->setKind(UNIT_KIND_AMPERE);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1);
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_DIMENSIONLESS:
    case UNIT_KIND_ITEM:
    case UNIT_KIND_RADIAN:
    case UNIT_KIND_STERADIAN:
      /* all dimensionless */
      newUnit->setKind(UNIT_KIND_DIMENSIONLESS);
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_FARAD:
      /* 1 Farad = 1 m^-2 kg^-1 s^4 A^2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(4*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_GRAM:
      /* 1 gram = 0.001 Kg */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(0.001 * newUnit->getMultiplier());
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_GRAY:
    case UNIT_KIND_SIEVERT:
      /* 1 Gray = 1 m^2 sec^-2 */
      /* 1 Sievert = 1 m^2 sec^-2 */
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_HENRY:
      /* 1 Henry = 1 m^2 kg s^-2 A^-2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(1.0/sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_JOULE:
      /* 1 joule = 1 m^2 kg s^-2 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_KATAL:
      /* 1 katal = 1 mol s^-1 */
      newUnit->setKind(UNIT_KIND_MOLE);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
     break;

    case UNIT_KIND_KELVIN:
      /* Kelvin is the SI unit of temperature */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_KILOGRAM:
      /* Kilogram is the SI unit of mass */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_LITER:
    case UNIT_KIND_LITRE:
      /* 1 litre = 0.001 m^3 = (0.1 m)^3*/ 
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setExponent(newUnit->getExponent()*3);
      newUnit->setMultiplier(pow((newUnit->getMultiplier() * 0.001), 1.0/3.0)); 
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_LUMEN:
      /* 1 lumen = 1 candela*/ 
      newUnit->setKind(UNIT_KIND_CANDELA);
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_LUX:
      /* 1 lux = 1 candela m^-2*/ 
      newUnit->setKind(UNIT_KIND_CANDELA);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_METER:
    case UNIT_KIND_METRE:
      /* metre is the SI unit of length */
      newUnit->setKind(UNIT_KIND_METRE);
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_MOLE:
      /* mole is the SI unit of substance */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_NEWTON:
      /* 1 newton = 1 m kg s^-2 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_OHM:
      /* 1 ohm = 1 m^2 kg s^-3 A^-2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(1.0/sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_PASCAL:
      /* 1 pascal = 1 m^-1 kg s^-2 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_SECOND:
      /* second is the SI unit of time */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_SIEMENS:
      /* 1 siemen = 1 m^-2 kg^-1 s^3 A^2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(3*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_TESLA:
      /* 1 tesla = 1 kg s^-2 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(1.0/(newUnit->getMultiplier()));
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_VOLT:
      /* 1 volt = 1 m^2 kg s^-3 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(1.0/(newUnit->getMultiplier()));
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_WATT:
      /* 1 watt = 1 m^2 kg s^-3 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_WEBER:
      /* 1 weber = 1 m^2 kg s^-2 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(1.0/(newUnit->getMultiplier()));
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_INVALID:
      break;
  }
  delete newUnit;

  return ud;
}

/**
 * Returns a UnitDefinition object which contains the argument unit
 * converted to the appropriate SI unit.
 *
 * @param unit the Unit object to convert to SI
 *
 * @return a UnitDefinition object containing the SI unit.
 */
LIBSBML_EXTERN
UnitDefinition * 
convertUnitToSI(const Unit * unit)
{
  UnitKind_t uKind = unit->getKind();
  Unit * newUnit = new Unit(uKind, unit->getExponent(), 
                                    unit->getScale(), unit->getMultiplier());
  UnitDefinition * ud = new UnitDefinition();

  removeScale(newUnit);

  switch (uKind)
  {
    case UNIT_KIND_AMPERE:
      /* Ampere is the SI unit of current */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_BECQUEREL:
    case UNIT_KIND_HERTZ:
      /* 1 becquerel = 1 sec^-1 = (0.1 sec)^-1 */
      /* 1 hertz = 1 sec^-1 = (0.1 sec) ^-1*/
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setExponent(newUnit->getExponent()*-1);
      newUnit->setMultiplier(pow(newUnit->getMultiplier(), -1.0)); 
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_CANDELA:
      /* candela is the SI unit of luminous intensity */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_CELSIUS:
      /* 1 celsius = 1 Kelvin + 273.15*/
      newUnit->setKind(UNIT_KIND_KELVIN);
      newUnit->setOffset(273.15);
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_COULOMB:
      /* 1 coulomb = 1 Ampere second */
      newUnit->setKind(UNIT_KIND_AMPERE);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1);
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_DIMENSIONLESS:
    case UNIT_KIND_ITEM:
    case UNIT_KIND_RADIAN:
    case UNIT_KIND_STERADIAN:
      /* all dimensionless */
      newUnit->setKind(UNIT_KIND_DIMENSIONLESS);
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_FARAD:
      /* 1 Farad = 1 m^-2 kg^-1 s^4 A^2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(4*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_GRAM:
      /* 1 gram = 0.001 Kg */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(0.001 * newUnit->getMultiplier());
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_GRAY:
    case UNIT_KIND_SIEVERT:
      /* 1 Gray = 1 m^2 sec^-2 */
      /* 1 Sievert = 1 m^2 sec^-2 */
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_HENRY:
      /* 1 Henry = 1 m^2 kg s^-2 A^-2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(1.0/sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_JOULE:
      /* 1 joule = 1 m^2 kg s^-2 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_KATAL:
      /* 1 katal = 1 mol s^-1 */
      newUnit->setKind(UNIT_KIND_MOLE);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
     break;

    case UNIT_KIND_KELVIN:
      /* Kelvin is the SI unit of temperature */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_KILOGRAM:
      /* Kilogram is the SI unit of mass */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_LITER:
    case UNIT_KIND_LITRE:
      /* 1 litre = 0.001 m^3 = (0.1 m)^3*/ 
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setExponent(newUnit->getExponent()*3);
      newUnit->setMultiplier(pow((newUnit->getMultiplier() * 0.001), 1.0/3.0)); 
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_LUMEN:
      /* 1 lumen = 1 candela*/ 
      newUnit->setKind(UNIT_KIND_CANDELA);
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_LUX:
      /* 1 lux = 1 candela m^-2*/ 
      newUnit->setKind(UNIT_KIND_CANDELA);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_METER:
    case UNIT_KIND_METRE:
      /* metre is the SI unit of length */
      newUnit->setKind(UNIT_KIND_METRE);
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_MOLE:
      /* mole is the SI unit of substance */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_NEWTON:
      /* 1 newton = 1 m kg s^-2 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_OHM:
      /* 1 ohm = 1 m^2 kg s^-3 A^-2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(1.0/sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_PASCAL:
      /* 1 pascal = 1 m^-1 kg s^-2 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_SECOND:
      /* second is the SI unit of time */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_SIEMENS:
      /* 1 siemen = 1 m^-2 kg^-1 s^3 A^2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(sqrt(newUnit->getMultiplier()));
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(3*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_TESLA:
      /* 1 tesla = 1 kg s^-2 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(1.0/(newUnit->getMultiplier()));
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_VOLT:
      /* 1 volt = 1 m^2 kg s^-3 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(1.0/(newUnit->getMultiplier()));
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_WATT:
      /* 1 watt = 1 m^2 kg s^-3 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_WEBER:
      /* 1 weber = 1 m^2 kg s^-2 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      newUnit->setMultiplier(1.0/(newUnit->getMultiplier()));
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_INVALID:
      break;
  }

  delete newUnit;

  return ud;
}

/** 
 * Predicate returning @c true or @c false depending on whether 
 * Unit objects are identical (matching in all attributes).
 *
 * @param unit1 the first Unit object to compare
 * @param unit2 the second Unit object to compare
 *
 * @return @c true if all the attributes of unit1 are identical
 * to the attributes of unit2, @c false otherwise.
 *
 * @note For the purposes of comparison two units can be "identical",
 * i.e. all attributes are an exact match, or "equivalent" i.e. 
 * matching kind and exponent.
 *
 * @see areEquivalent();
 */
LIBSBML_EXTERN
bool 
areIdentical(Unit * unit1, Unit * unit2)
{
  bool identical = false;

  if (!strcmp(UnitKind_toString(unit1->getKind()), 
              UnitKind_toString(unit2->getKind())))
  {
    if ((unit1->getMultiplier() == unit2->getMultiplier())
      && (unit1->getScale()     == unit2->getScale())
      && (unit1->getOffset()    == unit2->getOffset())
      && (unit1->getExponent()  == unit2->getExponent()))
    {
      identical = true;
    }
  }

  return identical;
}


/** 
 * Predicate returning @c true or @c false depending on whether 
 * Unit objects are equivalent (matching kind and exponent).
 *
 * @param unit1 the first Unit object to compare
 * @param unit2 the second Unit object to compare
 *
 * @return @c true if the kind and exponent attributes of unit1 are identical
 * to the kind and exponent attributes of unit2, @c false otherwise.
 *
 * @note For the purposes of comparison two units can be "identical",
 * i.e. all attributes are an exact match, or "equivalent" i.e. 
 * matching kind and exponent.
 *
 * @see areIdentical();
 */
LIBSBML_EXTERN
bool 
areEquivalent(Unit * unit1, Unit * unit2)
{
  bool equivalent = false;

  if (!strcmp(UnitKind_toString(unit1->getKind()), 
              UnitKind_toString(unit2->getKind())))
  {
    if ( (unit1->getOffset()    == unit2->getOffset())
      && (unit1->getExponent()  == unit2->getExponent()))
    {
      equivalent = true;
    }
  }

  return equivalent;
}

/** @cond doxygen-c-only */

LIBSBML_EXTERN
void 
Unit_removeScale(Unit_t * unit)
{
  removeScale(static_cast<Unit*>(unit));
}


LIBSBML_EXTERN
void 
Unit_mergeUnits(Unit_t * unit1, Unit_t * unit2)
{
  mergeUnits(static_cast<Unit*>(unit1), static_cast<Unit*>(unit2));
}

LIBSBML_EXTERN
UnitDefinition_t * 
Unit_convertUnitToSI(Unit_t * unit)
{
  return convertUnitToSI(static_cast<Unit*>(unit));
}

LIBSBML_EXTERN
int 
Unit_areIdentical(Unit_t * unit1, Unit_t * unit2)
{
  return static_cast<int>(areIdentical(static_cast<Unit*>(unit1),
                                       static_cast<Unit*>(unit2)));
}

LIBSBML_EXTERN
int 
Unit_areEquivalent(Unit_t * unit1, Unit_t * unit2)
{
  return static_cast<int>(areEquivalent(static_cast<Unit*>(unit1),
                                       static_cast<Unit*>(unit2)));
}


/** @endcond doxygen-c-only */
