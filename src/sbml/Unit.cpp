/**
 * @file    Unit.cpp
 * @brief   Implementations of Unit and ListOfUnits.
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

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Unit.h>
#include <sbml/UnitDefinition.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/*
 * Creates a new Unit, optionally with its kind, exponent, scale,
 * and multiplier attributes set.
 */
Unit::Unit (   UnitKind_t  kind
             , int         exponent
             , int         scale
             , double      multiplier ) :
    SBase      ()
  , mKind      ( kind       )
  , mExponent  ( exponent   )
  , mScale     ( scale      )
  , mMultiplier( multiplier )
  , mOffset    ( 0.0     )
{
}


/*
 * Creates a new Unit, optionally with its kind (via string), exponent,
 * scale and multiplier attributes set.
 */
Unit::Unit (   const std::string&  kind
             , int            exponent 
             , int            scale
             , double         multiplier ) :
    SBase      ()
  , mKind      ( UnitKind_forName( kind.c_str() ) )
  , mExponent  ( exponent   )
  , mScale     ( scale      )
  , mMultiplier( multiplier )
  , mOffset    ( 0.0     )
{
}


/*
 * Destroys the given Unit.
 */
Unit::~Unit ()
{
}


/*
 * Copy constructor. Creates a copy of this Unit.
 */
Unit::Unit(const Unit& orig) :
    SBase      ( orig             )
  , mKind      ( orig.mKind       )
  , mExponent  ( orig.mExponent   )
  , mScale     ( orig.mScale      )
  , mMultiplier( orig.mMultiplier )
  , mOffset    ( orig.mOffset     )
{
}


/*
 * Assignment operator.
 */
Unit& Unit::operator=(const Unit& rhs)
{
  this->SBase::operator =(rhs);
  mKind       = rhs.mKind       ;
  mExponent   = rhs.mExponent   ;
  mScale      = rhs.mScale      ;
  mMultiplier = rhs.mMultiplier ;
  mOffset     = rhs.mOffset     ;
  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the UnitDefinition's next
 * Unit (if available).
 */
bool
Unit::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/*
 * @return a (deep) copy of this Unit.
 */
SBase*
Unit::clone () const
{
  return new Unit(*this);
}


/*
 * Initializes the fields of this Unit to their defaults:
 *
 *   - exponent   = 1
 *   - scale      = 0
 *   - multiplier = 1.0
 */
void
Unit::initDefaults ()
{
  setExponent  ( 1   );
  setScale     ( 0   );
  setMultiplier( 1.0 );
  setOffset    ( 0.0 );
}


/*
 * @return the kind of this Unit.
 */
UnitKind_t
Unit::getKind () const
{
  return mKind;
}


/*
 * @return the exponent of this Unit.
 */
int
Unit::getExponent () const
{
  return mExponent;
}


/*
 * @return the scale of this Unit.
 */
int
Unit::getScale () const
{
  return mScale;
}


/*
 * @return the multiplier of this Unit.
 */
double
Unit::getMultiplier () const
{
  return mMultiplier;
}


/*
 * @return the offset of this Unit.
 */
double
Unit::getOffset () const
{
  return mOffset;
}


/*
 * @return true if the kind of this Unit is 'ampere', false otherwise.
 */
bool
Unit::isAmpere () const
{
  return (mKind == UNIT_KIND_AMPERE);
}


/*
 * @return true if the kind of this Unit is 'becquerel', false otherwise.
 */
bool
Unit::isBecquerel () const
{
  return (mKind == UNIT_KIND_BECQUEREL);
}


/*
 * @return true if the kind of this Unit is 'candela', false otherwise.
 */
bool
Unit::isCandela () const
{
  return (mKind == UNIT_KIND_CANDELA);
}


/*
 * @return true if the kind of this Unit is 'Celsius', false otherwise.
 */
bool
Unit::isCelsius () const
{
  return (mKind == UNIT_KIND_CELSIUS);
}


/*
 * @return true if the kind of this Unit is 'coulomb', false otherwise.
 */
bool
Unit::isCoulomb () const
{
  return (mKind == UNIT_KIND_COULOMB);
}


/*
 * @return true if the kind of this Unit is 'dimensionless', false
 * otherwise.
 */
bool
Unit::isDimensionless () const
{
  return (mKind == UNIT_KIND_DIMENSIONLESS);
}


/*
 * @return true if the kind of this Unit is 'farad', false otherwise.
 */
bool
Unit::isFarad () const
{
  return (mKind == UNIT_KIND_FARAD);
}


/*
 * @return true if the kind of this Unit is 'gram', false otherwise.
 */
bool
Unit::isGram () const
{
  return (mKind == UNIT_KIND_GRAM);
}


/*
 * @return true if the kind of this Unit is 'gray', false otherwise.
 */
bool
Unit::isGray () const
{
  return (mKind == UNIT_KIND_GRAY);
}


/*
 * @return true if the kind of this Unit is 'henry', false otherwise.
 */
bool
Unit::isHenry () const
{
  return (mKind == UNIT_KIND_HENRY);
}


/*
 * @return true if the kind of this Unit is 'hertz', false otherwise.
 */
bool
Unit::isHertz () const
{
  return (mKind == UNIT_KIND_HERTZ);
}


/*
 * @return true if the kind of this Unit is 'item', false otherwise.
 */
bool
Unit::isItem () const
{
  return (mKind == UNIT_KIND_ITEM);
}


/*
 * @return true if the kind of this Unit is 'joule', false otherwise.
 */
bool
Unit::isJoule () const
{
  return (mKind == UNIT_KIND_JOULE);
}


/*
 * @return true if the kind of this Unit is 'katal', false otherwise.
 */
bool
Unit::isKatal () const
{
  return (mKind == UNIT_KIND_KATAL);
}


/*
 * @return true if the kind of this Unit is 'kelvin', false otherwise.
 */
bool
Unit::isKelvin () const
{
  return (mKind == UNIT_KIND_KELVIN);
}


/*
 * @return true if the kind of this Unit is 'kilogram', false otherwise.
 */
bool
Unit::isKilogram () const
{
  return (mKind == UNIT_KIND_KILOGRAM);
}


/*
 * @return true if the kind of this Unit is 'litre' or 'liter', false
 * otherwise.
 */
bool
Unit::isLitre () const
{
  if (getLevel() == 1)
  {
    return (mKind == UNIT_KIND_LITRE || mKind == UNIT_KIND_LITER);
  }
  else
  {
    return (mKind == UNIT_KIND_LITRE);
  }
}


/*
 * @return true if the kind of this Unit is 'lumen', false otherwise.
 */
bool
Unit::isLumen () const
{
  return (mKind == UNIT_KIND_LUMEN);
}


/*
 * @return true if the kind of this Unit is 'lux', false otherwise.
 */
bool
Unit::isLux () const
{
  return (mKind == UNIT_KIND_LUX);
}


/*
 * @return true if the kind of this Unit is 'metre' or 'meter', false
 * otherwise.
 */
bool
Unit::isMetre () const
{
  if (getLevel() == 1)
  {
    return (mKind == UNIT_KIND_METRE || mKind == UNIT_KIND_METER);
  }
  else
  {
    return (mKind == UNIT_KIND_METRE);
  }
}


/*
 * @return true if the kind of this Unit is 'mole', false otherwise.
 */
bool
Unit::isMole () const
{
  return (mKind == UNIT_KIND_MOLE);
}


/*
 * @return true if the kind of this Unit is 'newton', false otherwise.
 */
bool
Unit::isNewton () const
{
  return (mKind == UNIT_KIND_NEWTON);
}


/*
 * @return true if the kind of this Unit is 'ohm', false otherwise.
 */
bool
Unit::isOhm () const
{
  return (mKind == UNIT_KIND_OHM);
}


/*
 * @return true if the kind of this Unit is 'pascal', false otherwise.
 */
bool
Unit::isPascal () const
{
  return (mKind == UNIT_KIND_PASCAL);
}


/*
 * @return true if the kind of this Unit is 'radian', false otherwise.
 */
bool
Unit::isRadian () const
{
  return (mKind == UNIT_KIND_RADIAN);
}


/*
 * @return true if the kind of this Unit is 'second', false otherwise.
 */
bool
Unit::isSecond () const
{
  return (mKind == UNIT_KIND_SECOND);
}


/*
 * @return true if the kind of this Unit is 'siemens', false otherwise.
 */
bool
Unit::isSiemens () const
{
  return (mKind == UNIT_KIND_SIEMENS);
}


/*
 * @return true if the kind of this Unit is 'sievert', false otherwise.
 */
bool
Unit::isSievert () const
{
  return (mKind == UNIT_KIND_SIEVERT);
}


/*
 * @return true if the kind of this Unit is 'steradian', false otherwise.
 */
bool
Unit::isSteradian () const
{
  return (mKind == UNIT_KIND_STERADIAN);
}


/*
 * @return true if the kind of this Unit is 'tesla', false otherwise.
 */
bool
Unit::isTesla () const
{
  return (mKind == UNIT_KIND_TESLA);
}


/*
 * @return true if the kind of this Unit is 'volt', false otherwise.
 */
bool
Unit::isVolt () const
{
  return (mKind == UNIT_KIND_VOLT);
}


/*
 * @return true if the kind of this Unit is 'watt', false otherwise.
 */
bool
Unit::isWatt () const
{
  return (mKind == UNIT_KIND_WATT);
}


/*
 * @return true if the kind of this Unit is 'weber', false otherwise.
 */
bool
Unit::isWeber () const
{
  return (mKind == UNIT_KIND_WEBER);
}


/*
 * @return true if the kind of this Unit has been set, false otherwise.
 */
bool
Unit::isSetKind () const
{
  return (mKind != UNIT_KIND_INVALID);
}


/*
 * Sets the kind of this Unit to the given UnitKind.
 */
void
Unit::setKind (UnitKind_t kind)
{
  mKind = kind;
}


/*
 * Sets the exponent of this Unit to the given value.
 */
void
Unit::setExponent (int value)
{
  mExponent = value;
}


/*
 * Sets the scale of this Unit to the given value.
 */
void
Unit::setScale (int value)
{
  mScale = value;
}


/*
 * Sets the multiplier of this Unit to the given value.
 */
void
Unit::setMultiplier (double value)
{
  mMultiplier = value;
}


/*
 * Sets the offset of this Unit to the given value.
 */
void
Unit::setOffset (double value)
{
  mOffset = value;
}


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Unit::getTypeCode () const
{
  return SBML_UNIT;
}


/*
 * @return the name of this element ie "unit".
 */
const string&
Unit::getElementName () const
{
  static const string name = "unit";
  return name;
}


/*
 * @return true if name is one of the five SBML built-in Unit names
 * ('substance', 'volume', 'area', 'length' or 'time'), false otherwise.
 */
bool
Unit::isBuiltIn (const std::string& name, unsigned int level)
{
  if (level == 1)
  {
    return
      name == "substance" ||
      name == "volume"    ||
      name == "time";
  }
  else
  {
    return
      name == "substance" ||
      name == "volume"    ||
      name == "area"      ||
      name == "length"    ||
      name == "time";
  }
}

bool
Unit::isUnitKind(const std::string &name, unsigned int level, 
                                         unsigned int version)
{
  if (level == 1)
  {
    return isL1UnitKind(name);
  }
  else if (version == 1)
  {
    return isL2V1UnitKind(name);
  }
  else
  {
    return isL2UnitKind(name);
  }
}


/** @cond doxygen-libsbml-internal */
/*
 * @return true if name is a valid UnitKind.
 */
bool
Unit::isL1UnitKind (const std::string& name)
{
  return (UnitKind_forName( name.c_str() ) != UNIT_KIND_INVALID);
}

bool 
Unit::areIdentical(Unit * unit1, Unit * unit2)
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
bool 
Unit::areEquivalent(Unit * unit1, Unit * unit2)
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
void 
Unit::removeScale(Unit * unit)
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
void
Unit::merge(Unit * unit1, Unit * unit2)
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

  Unit::removeScale(unit1);
  Unit::removeScale(unit2);

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
UnitDefinition * 
Unit::convertToSI(Unit * unit)
{
  UnitKind_t uKind = unit->getKind();
  Unit * newUnit = new Unit(uKind, unit->getExponent(), 
                                     unit->getScale(), unit->getMultiplier());
  UnitDefinition * ud = new UnitDefinition();

  Unit::removeScale(newUnit);

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
UnitDefinition * 
Unit::convertToSI(const Unit * unit)
{
  UnitKind_t uKind = unit->getKind();
  Unit * newUnit = new Unit(uKind, unit->getExponent(), 
                                    unit->getScale(), unit->getMultiplier());
  UnitDefinition * ud = new UnitDefinition();

  Unit::removeScale(newUnit);

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

/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return true if name is a valid UnitKind.
 */
bool
Unit::isL2V1UnitKind (const std::string& name)
{
  if (name == "meter" || name == "liter")
    return false;
  else
    return (UnitKind_forName( name.c_str() ) != UNIT_KIND_INVALID);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return true if name is a valid UnitKind.
 */
bool
Unit::isL2UnitKind (const std::string& name)
{
  if (name == "meter" || name == "liter" || name == "Celsius")
    return false;
  else
    return (UnitKind_forName( name.c_str() ) != UNIT_KIND_INVALID);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Unit::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  expectedAttributes.push_back("kind");
  expectedAttributes.push_back("exponent");
  expectedAttributes.push_back("scale");

  if (level == 2)
  {
    expectedAttributes.push_back("metaid");
    expectedAttributes.push_back("multiplier");

    if (version == 1)
    {
      expectedAttributes.push_back("offset");
    }

    if (version == 3)
    {
      expectedAttributes.push_back("sboTerm");
    }
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      logUnknownAttribute(name, level, version, "<unit>");
    }
  }

  //
  // kind: UnitKind  (L1v1, L1v2, L2v1, L2v2)
  //
  string kind;
  if ( attributes.readInto("kind", kind, getErrorLog(), true) )
  {
    mKind = UnitKind_forName( kind.c_str() );
  }

  //
  // exponent  { use="optional" default="1" }  (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("exponent", mExponent, getErrorLog());

  //
  // scale  { use="optional" default="0" }  (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("scale", mScale, getErrorLog());

  if (level == 2)
  {
    //
    // multiplier  { use="optional" default="1" }  (L2v1, L2v2)
    //
    attributes.readInto("multiplier", mMultiplier, getErrorLog());

    //
    // offset  { use="optional" default="0" }  (L2v1)
    //
    attributes.readInto("offset", mOffset, getErrorLog());

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v2)
    //
    if (version == 3) 
        mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (this->getLevel() == 2 && this->getVersion() == 3) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Unit::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level = getLevel();
  const unsigned int version = getVersion();

  //
  // kind: UnitKind  { use="required" }  (L1v1, L1v2, L2v1, L2v2)
  //
  const string kind = UnitKind_toString(mKind);
  stream.writeAttribute("kind", kind);

  //
  // exponent  { use="optional" default="1" }  (L1v1, L1v2, L2v1, L2v2)
  //
  if (mExponent != 1) stream.writeAttribute("exponent", mExponent);

  //
  // scale  { use="optional" default="0" }  (L1v1, L1v2, L2v1, L2v2)
  //
  if (mScale != 0) stream.writeAttribute("scale", mScale);

  if (level == 2)
  {
    //
    // multiplier  { use="optional" default="1" }  (L2v1, L2v2)
    //
    if (mMultiplier != 1) stream.writeAttribute("multiplier", mMultiplier);

    //
    // offset  { use="optional" default="0" }  (L2v1, L2v2)
    //
    if (version == 1 && mOffset != 0) stream.writeAttribute("offset", mOffset);

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v3)
    //
    if (version == 3) 
        SBO::writeTerm(stream, mSBOTerm);
  }

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v3)
  //
  if (this->getLevel() == 2 && this->getVersion() == 3) 
    SBO::writeTerm(stream, mSBOTerm);
}
/** @endcond doxygen-libsbml-internal */


/*
 * @return a (deep) copy of this ListOfUnits.
 */
SBase*
ListOfUnits::clone () const
{
  return new ListOfUnits(*this);
}


/*
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfUnits::getItemTypeCode () const
{
  return SBML_UNIT;
}


/*
 * @return the name of this element ie "listOfUnits".
 */
const string&
ListOfUnits::getElementName () const
{
  static const string name = "listOfUnits";
  return name;
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its
 * siblings or -1 (default) to indicate the position is not significant.
 */
int
ListOfUnits::getElementPosition () const
{
  return 1;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfUnits::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "unit")
  {
    object = new Unit();
    mItems.push_back(object);
  }

  return object;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */


/**
 * Creates a new, empty Unit_t structure and returns a pointer to it.
 *
 * It is worth emphasizing that the structure returned by this constructor
 * has no attribute values set, and that the "kind" attribute is
 * initialized to UNIT_KIND_INVALID.  Callers must set the value to
 * something appropriate using the Unit::setKind() method.
 *
 * @return a pointer to the new Unit_t structure.
 */
LIBSBML_EXTERN
Unit_t *
Unit_create (void)
{
  return new(nothrow) Unit;
}


/**
 * Creates a new Unit with the given @p kind, @p exponent and @p scale
 * values and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 * @code
 *   Unit_t *u = Unit_create();
 *   Unit_setKind(kind);
 *   Unit_setExponent(exponent);
 *   Unit_setScale(scale);
 * @endcode
 *
 * @param kind a value from the UnitKind_t enumeration naming the base
 * unit serving as the basis of this particular unit definition
 * 
 * @param exponent an integer, the "exponent" attribute of the unit
 * definition 
 * 
 * @param scale an integer, the "scale" attribute of the unit definition
 *
 * @return a pointer to the new Unit_t structure.
 */
LIBSBML_EXTERN
Unit_t *
Unit_createWithKindExponentScale (UnitKind_t kind, int exponent, int scale)
{
  return new(nothrow) Unit(kind, exponent, scale);
}


/**
 * Creates a new Unit with the given @p kind, @p exponent, @p scale and
 * @p multiplier values and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 * @code
 *   Unit_t *u = Unit_create();
 *   Unit_setKind(kind);
 *   Unit_setExponent(exponent);
 *   Unit_setScale(scale);
 *   Unit_setMultiplier(multiplier);
 * @endcode
 *
 * @param kind a value from the UnitKind_t enumeration naming the base
 * unit serving as the basis of this particular unit definition
 * 
 * @param exponent an integer, the "exponent" attribute of the unit
 * definition 
 * 
 * @param scale an integer, the "scale" attribute of the unit definition
 * @param multiplier an integer, the "multiplier" attribute of the unit
 *
 * @return a pointer to the new Unit_t structure.
 */
LIBSBML_EXTERN
Unit_t *
Unit_createWithKindExponentScaleMultiplier (UnitKind_t kind, int exponent, int scale, double multiplier)
{
  return new(nothrow) Unit(kind, exponent, scale, multiplier);
}


/**
 * Frees the given Unit_t structure.
 *
 * @param u the Unit_t structure to be freed.
 */
LIBSBML_EXTERN
void
Unit_free (Unit_t *u)
{
  delete u;
}


/**
 * Creates a deep copy of the given Unit_t structure
 * 
 * @param u the Unit_t structure to be copied
 * 
 * @return a (deep) copy of the given Unit_t structure.
 */
LIBSBML_EXTERN
Unit_t *
Unit_clone (const Unit_t* u)
{
  return static_cast<Unit*>( u->clone() );
}


/**
 * Initializes the attributes of this Unit (except for "kind") to their
 * defaults values.
 *
 * The default values are as follows:
 * 
 * - exponent   = 1
 * - scale      = 0
 * - multiplier = 1.0
 */
LIBSBML_EXTERN
void
Unit_initDefaults (Unit_t *u)
{
  u->initDefaults();
}


/**
 * Returns the "kind" attribute value of the given unit @p u.
 * 
 * @return the value of the "kind" attribute of this Unit as a value from
 * the UnitKind_t enumeration.
 */
LIBSBML_EXTERN
UnitKind_t
Unit_getKind (const Unit_t *u)
{
  return u->getKind();
}


/**
 * Returns the value of the "exponent" attribute of the given Unit_t
 * structure @p u.
 *
 * @param u a Unit_t structure
 *
 * @return the "exponent" value of this Unit_t structure, as an integer.
 */
LIBSBML_EXTERN
int
Unit_getExponent (const Unit_t *u)
{
  return u->getExponent();
}


/**
 * Returns the value of the "scale" attribute of the given Unit_t structure
 * @p u.
 *
 * @param u a Unit_t structure
 *
 * @return the "scale" value of this Unit, as an integer.
 */
LIBSBML_EXTERN
int
Unit_getScale (const Unit_t *u)
{
  return u->getScale();
}


/**
 * Returns the value of the "multiplier" attribute of the given Unit_t
 * structure @p u.
 * 
 * @param u a Unit_t structure
 *
 * @return the "multiplier" value of this Unit, as a double
 */
LIBSBML_EXTERN
double
Unit_getMultiplier (const Unit_t *u)
{
  return u->getMultiplier();
}


/**
 * Returns the value of the "offset" attribute of the given Unit_t
 * structure @p u.
 * 
 * @param u a Unit_t structure
 *
 * @return the "offset" value of this Unit, as a double
 *
 * @warning The "offset" attribute is only available in SBML Level 2
 * Version 1.  This attribute is not present in SBML Level 2 Version 2 or
 * above.  When producing SBML models using these later specifications,
 * Modelers and software need to account for units with offsets explicitly.
 * The %SBML specification document offers a number of suggestions for how
 * to achieve this.  LibSBML functions such as this one related to "offset"
 * are retained for compatibility with earlier versions of SBML Level 2,
 * but their use is strongly discouraged.
 * 
 */
LIBSBML_EXTERN
double
Unit_getOffset (const Unit_t *u)
{
  return u->getOffset();
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c ampere.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "ampere", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isAmpere (const Unit_t *u)
{
  return static_cast<int>( u->isAmpere() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c becquerel.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "becquerel", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isBecquerel (const Unit_t *u)
{
  return static_cast<int>( u->isBecquerel() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c candela.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "candela", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCandela (const Unit_t *u)
{
  return static_cast<int>( u->isCandela() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c Celsius.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "Celsius", zero (0) otherwise.
 *
 * @warning The predefined unit @c Celsius was removed from the list of
 * predefined units in SBML Level 2 Version 3 at the same time that the
 * "offset" attribute was removed from Unit definitions.  LibSBML functions
 * such as this one related to @c Celsius are retained for compatibility
 * with earlier versions of SBML Level 2, but their use is strongly
 * discouraged.
 */
LIBSBML_EXTERN
int
Unit_isCelsius (const Unit_t *u)
{
  return static_cast<int>( u->isCelsius() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c coulomb.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "coulomb", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCoulomb (const Unit_t *u)
{
  return static_cast<int>( u->isCoulomb() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c dimensionless.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "dimensionless", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isDimensionless (const Unit_t *u)
{
  return static_cast<int>( u->isDimensionless() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c farad.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "farad", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isFarad (const Unit_t *u)
{
  return static_cast<int>( u->isFarad() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c gram.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "gram", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGram (const Unit_t *u)
{
  return static_cast<int>( u->isGram() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c gray.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "gray", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGray (const Unit_t *u)
{
  return static_cast<int>( u->isGray() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c henry.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "henry", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHenry (const Unit_t *u)
{
  return static_cast<int>( u->isHenry() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c hertz.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "hertz", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHertz (const Unit_t *u)
{
  return static_cast<int>( u->isHertz() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c item.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "item", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isItem (const Unit_t *u)
{
  return static_cast<int>( u->isItem() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c joule.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "joule", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isJoule (const Unit_t *u)
{
  return static_cast<int>( u->isJoule() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c katal.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "katal", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKatal (const Unit_t *u)
{
  return static_cast<int>( u->isKatal() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c kelvin.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "kelvin", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKelvin (const Unit_t *u)
{
  return static_cast<int>( u->isKelvin() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c kilogram.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "kilogram", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKilogram (const Unit_t *u)
{
  return static_cast<int>( u->isKilogram() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c litre or @c liter.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given Unit_t
 * structure is set to @c "litre" or @c "liter", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLitre (const Unit_t *u)
{
  return static_cast<int>( u->isLitre() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c lumen.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "lumen", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLumen (const Unit_t *u)
{
  return static_cast<int>( u->isLumen() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c lux.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "lux", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLux (const Unit_t *u)
{
  return static_cast<int>( u->isLux() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c metre or @c meter.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given Unit_t
 * structure is set to @c "metre" or @c "meter", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMetre (const Unit_t *u)
{
  return static_cast<int>( u->isMetre() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c mole.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "mole", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMole (const Unit_t *u)
{
  return static_cast<int>( u->isMole() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c newton.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "newton", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isNewton (const Unit_t *u)
{
  return static_cast<int>( u->isNewton() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c ohm.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "ohm", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isOhm (const Unit_t *u)
{
  return static_cast<int>( u->isOhm() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c pascal.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "pascal", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isPascal (const Unit_t *u)
{
  return static_cast<int>( u->isPascal() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c radian.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "radian", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isRadian (const Unit_t *u)
{
  return static_cast<int>( u->isRadian() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c second.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "second", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSecond (const Unit_t *u)
{
  return static_cast<int>( u->isSecond() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c siemens.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "siemens", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSiemens (const Unit_t *u)
{
  return static_cast<int>( u->isSiemens() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c sievert.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "sievert", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSievert (const Unit_t *u)
{
  return static_cast<int>( u->isSievert() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c steradian.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "steradian", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSteradian (const Unit_t *u)
{
  return static_cast<int>( u->isSteradian() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c tesla.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "tesla", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isTesla (const Unit_t *u)
{
  return static_cast<int>( u->isTesla() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c volt.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "volt", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isVolt (const Unit_t *u)
{
  return static_cast<int>( u->isVolt() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c watt.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "watt", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWatt (const Unit_t *u)
{
  return static_cast<int>( u->isWatt() );
}


/**
 * Predicate for testing whether the given Unit_t structure represents a
 * unit of the kind @c weber.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure is set to @c "weber", zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWeber (const Unit_t *u)
{
  return static_cast<int>( u->isWeber() );
}


/**
 * Predicate to test whether the "kind" attribute of the given Unit_t
 * structure @p u has been set.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "kind" attribute of the given
 * Unit_t structure has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSetKind (const Unit_t *u)
{
  return static_cast<int>( u->isSetKind() );
}


/**
 * Sets the kind of the given Unit_t structure @p u to the given
 * UnitKind_t value.
 *
 * @param u the Unit_t structure whose value is to be set
 * @param kind a value from the UnitKind_t enumeration 
 */
LIBSBML_EXTERN
void
Unit_setKind (Unit_t *u, UnitKind_t kind)
{
  u->setKind(kind);
}


/**
 * Sets the "exponent" attribute value of the given Unit_t structure @p u.
 *
 * @param u the Unit_t structure whose value is to be set
 * @param value the integer to which the attribute "exponent" should be set
 */
LIBSBML_EXTERN
void
Unit_setExponent (Unit_t *u, int value)
{
  u->setExponent(value);
}


/**
 * Sets the "scale" attribute value of the given Unit_t structure @p u.
 *
 * @param u the Unit_t structure whose value is to be set
 * @param value the integer to which the attribute "scale" should be set
 */
LIBSBML_EXTERN
void
Unit_setScale (Unit_t *u, int value)
{
  u->setScale(value);
}


/**
 * Sets the "multiplier" attribute value of the given Unit_t structure @p u.
 *
 * @param u the Unit_t structure whose value is to be set
 * @param value the integer to which the attribute "multiplier" should be set
 */
LIBSBML_EXTERN
void
Unit_setMultiplier (Unit_t *u, double value)
{
  u->setMultiplier(value);
}


/**
 * Sets the "offset" attribute value of the given Unit_t structure @p u.
 * 
 * @param u the Unit_t structure whose value is to be set
 * @param value the integer to which the attribute "offset" should be set
 *
 * @warning The "offset" attribute is only available in SBML Level 2
 * Version 1.  This attribute is not present in SBML Level 2 Version 2 or
 * above.  When producing SBML models using these later specifications,
 * Modelers and software need to account for units with offsets explicitly.
 * The %SBML specification document offers a number of suggestions for how
 * to achieve this.  LibSBML functions such as this one related to "offset"
 * are retained for compatibility with earlier versions of SBML Level 2,
 * but their use is strongly discouraged.
 */
LIBSBML_EXTERN
void
Unit_setOffset (Unit_t *u, double value)
{
  u->setOffset(value);
}


/**
 * Predicate to test whether a given string is the name of a built-in SBML
 * unit.
 *
 * @param name a string to be tested against the built-in unit names

 * @return nonzero (for true) if @p name is one of the five SBML
 * built-in Unit names (@c "substance", @c "volume, @c "area", @c "length"
 * or @c "time"), zero (0) otherwise
 *
 * @note: @c "length" and @c "area" were added in Level 2 Version 1
 */
LIBSBML_EXTERN
int
Unit_isBuiltIn (const char *name, unsigned int level)
{
  return Unit::isBuiltIn(name != NULL ? name : "", level);
}


/** @endcond doxygen-c-only */
