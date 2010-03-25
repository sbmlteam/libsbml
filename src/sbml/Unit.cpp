/**
 * @file    Unit.cpp
 * @brief   Implementations of Unit and ListOfUnits.
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

#include <sstream>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */

LIBSBML_CPP_NAMESPACE_BEGIN

Unit::Unit (unsigned int level, unsigned int version) :
   SBase ( level, version )
  , mKind      ( UNIT_KIND_INVALID )
  , mExponent  ( 1   )
  , mExponentDouble  ( 1   )
  , mScale     ( 0      )
  , mMultiplier( 1.0 )
  , mOffset    ( 0.0     )
  , mIsSetExponent    ( false )
  , mIsSetScale       ( false )
  , mIsSetMultiplier  ( false )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();

  // if level 3 values have no defaults
  if (level == 3)
  {
    mExponentDouble = numeric_limits<double>::quiet_NaN();
    mScale = SBML_INT_MAX;//numeric_limits<int>::max();
    mMultiplier = numeric_limits<double>::quiet_NaN();
  }
}


Unit::Unit (SBMLNamespaces * sbmlns) :
   SBase ( sbmlns )
  , mKind      ( UNIT_KIND_INVALID )
  , mExponent  ( 1  )
  , mExponentDouble  ( 1  )
  , mScale     ( 0      )
  , mMultiplier( 1.0 )
  , mOffset    ( 0.0     )
  , mIsSetExponent    ( false )
  , mIsSetScale       ( false )
  , mIsSetMultiplier  ( false )
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();

  // if level 3 values have no defaults
  if (sbmlns->getLevel() == 3)
  {
    mExponentDouble = numeric_limits<double>::quiet_NaN();
    mScale = numeric_limits<int>::max();
    mMultiplier = numeric_limits<double>::quiet_NaN();
  }
}

/** @cond doxygen-libsbml-internal */

/* constructor for validators */
Unit::Unit() :
  SBase()
{
}

/** @endcond doxygen-libsbml-internal */
                          
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
  , mExponentDouble  ( orig.mExponentDouble   )
  , mScale     ( orig.mScale      )
  , mMultiplier( orig.mMultiplier )
  , mOffset    ( orig.mOffset     )
  , mIsSetExponent    ( orig.mIsSetExponent )
  , mIsSetScale       ( orig.mIsSetScale )
  , mIsSetMultiplier  ( orig.mIsSetMultiplier )
{
}


/*
 * Assignment operator.
 */
Unit& Unit::operator=(const Unit& rhs)
{
  if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
    mKind       = rhs.mKind       ;
    mExponent   = rhs.mExponent   ;
    mExponentDouble   = rhs.mExponentDouble   ;
    mScale      = rhs.mScale      ;
    mMultiplier = rhs.mMultiplier ;
    mOffset     = rhs.mOffset     ;
    mIsSetExponent    = rhs.mIsSetExponent;
    mIsSetScale       = rhs.mIsSetScale;
    mIsSetMultiplier  = rhs.mIsSetMultiplier;
  }

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
Unit*
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
  //// level 3 has no defaults
  //if (getLevel() < 3)
  //{
    setExponent  ( 1   );
    setScale     ( 0   );
    setMultiplier( 1.0 );
    setOffset    ( 0.0 );
  //}
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
  if (getLevel() < 3)
  {
    return mExponent;
  }
  else
  {
    if (isSetExponent())
    {
      if (ceil(mExponentDouble) == 
          floor(mExponentDouble))
      {
        return static_cast<int>(mExponentDouble);
      }
      else
      {
        return numeric_limits<int>::quiet_NaN();
      }
    }
    else
    {
      return static_cast<int>(mExponentDouble);
    }
  }
}


double
Unit::getExponentAsDouble () const
{
  if (getLevel() > 2)
    return mExponentDouble;
  else
    return static_cast<double>(mExponent);
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
 * @return true if the kind of this Unit is 'avogadro', false otherwise.
 */
bool
Unit::isAvogadro () const
{
  return (mKind == UNIT_KIND_AVOGADRO);
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
 * @return @c true if the "exponent" attribute of this Unit has been set, 
 * @c false otherwise.
 */
bool 
Unit::isSetExponent () const
{
  return mIsSetExponent;
}

/*
 * @return @c true if the "scale" attribute of this Unit has been set, 
 * @c false otherwise.
 */
bool 
Unit::isSetScale () const
{
  return mIsSetScale;
}


/*
 * @return @c true if the "multiplier" attribute of this Unit has been set, 
 * @c false otherwise.
 */
bool 
Unit::isSetMultiplier () const
{
  return mIsSetMultiplier;
}

  
/*
 * Sets the kind of this Unit to the given UnitKind.
 */
int
Unit::setKind (UnitKind_t kind)
{
  if (!UnitKind_isValidUnitKindString(UnitKind_toString(kind),
                 getLevel(), getVersion()))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mKind = kind;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the exponent of this Unit to the given value.
 */
int
Unit::setExponent (int value)
{
  mExponent = value;
  mExponentDouble = (double) (value);
  mIsSetExponent = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the exponent of this Unit to the given value.
 */
int
Unit::setExponent (double value)
{
  mExponentDouble = value;
  mExponent = (int) (value);
  mIsSetExponent = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the scale of this Unit to the given value.
 */
int
Unit::setScale (int value)
{
  mScale = value;
  mIsSetScale = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the multiplier of this Unit to the given value.
 */
int
Unit::setMultiplier (double value)
{
  if (getLevel() < 2)
  {
    mMultiplier = value;
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else
  {
    mMultiplier = value;
    mIsSetMultiplier = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the offset of this Unit to the given value.
 */
int
Unit::setOffset (double value)
{
  if (!(getLevel() == 2 && getVersion() == 1))
  {
    mOffset = 0;
    return LIBSBML_UNEXPECTED_ATTRIBUTE;
  }
  else
  {
    mOffset = value;
    return LIBSBML_OPERATION_SUCCESS;
  }
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


bool 
Unit::hasRequiredAttributes() const
{
  bool allPresent = true;

  /* required attributes for unit: 
  kind (exp, multiplier scale from L3)*/

  if (!isSetKind())
    allPresent = false;

  if (getLevel() > 2 && !isSetExponent())
    allPresent = false;

  if (getLevel() > 2 && !isSetMultiplier())
    allPresent = false;

  if (getLevel() > 2 && !isSetScale())
    allPresent = false;

  return allPresent;
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
  else if ( level == 2)
  {
    if (version == 1)
    {
      return isL2V1UnitKind(name);
    }
    else
    {
      return isL2UnitKind(name);
    }
  }
  else
  {
    return isL3UnitKind(name);
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

  removeScale(unit1);
  removeScale(unit2);
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
    // if the kind is dimensionless it doesnt matter 
    // what the exponent is
    if (unit1->getKind() != UNIT_KIND_DIMENSIONLESS)
    {
      if ( (unit1->getOffset()    == unit2->getOffset())
        && (unit1->getExponent()  == unit2->getExponent()))
      {
        equivalent = true;
      }
    }
    else
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
int 
Unit::removeScale(Unit * unit)
{
  double scaleFactor = pow(10.0, unit->getScale());
  double newMultiplier = unit->getMultiplier() * scaleFactor;
  /* hack to force multiplier to be double precision */
  std::ostringstream ossMultiplier;
  ossMultiplier.precision(15);
  ossMultiplier << newMultiplier;
  newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);
  unit->setMultiplier(newMultiplier);
  unit->setScale(0);
  return LIBSBML_OPERATION_SUCCESS;
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
 * @param unit1 the first Unit object 
 * @param unit2 the second Unit object to merge with the first
 */
void
Unit::merge(Unit * unit1, Unit * unit2)
{
  double newExponent;
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
    
  /* hack to force multiplier to be double precision */
  std::ostringstream ossMultiplier;
  ossMultiplier.precision(15);
  ossMultiplier << newMultiplier;
  newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);

  unit1->setScale(0);
  unit1->setExponent(newExponent);
  unit1->setMultiplier(newMultiplier);
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
  double newMultiplier;
  std::ostringstream ossMultiplier;
  UnitKind_t uKind = unit->getKind();
  Unit * newUnit = new Unit(unit->getSBMLNamespaces());
  newUnit->setKind(uKind);
  newUnit->setExponent(unit->getExponent());
  newUnit->setScale(unit->getScale());
  newUnit->setMultiplier(unit->getMultiplier());
  UnitDefinition * ud = new UnitDefinition(unit->getSBMLNamespaces());

  Unit::removeScale(newUnit);
  ossMultiplier.precision(15);

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
      /* hack to force multiplier to be double precision */
      newMultiplier = pow(newUnit->getMultiplier(), -1.0);

      ossMultiplier << newMultiplier;
      newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);

      newUnit->setMultiplier(newMultiplier); 
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
 //     newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setExponent(unit->getExponent());
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
      /* hack to force multiplier to be double precision */
      newMultiplier = sqrt(newUnit->getMultiplier());

      ossMultiplier << newMultiplier;
      newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);

      newUnit->setMultiplier(newMultiplier); 
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*unit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*unit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(4*unit->getExponent());  
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
      /* hack to force multiplier to be double precision */
      newMultiplier = sqrt(newUnit->getMultiplier());

      ossMultiplier << newMultiplier;
      newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);

      newUnit->setMultiplier(newMultiplier); 
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
 //     newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*unit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_HENRY:
      /* 1 Henry = 1 m^2 kg s^-2 A^-2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
       /* hack to force multiplier to be double precision */
      newMultiplier = (1.0/sqrt(newUnit->getMultiplier()));

      ossMultiplier << newMultiplier;
      newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);

      newUnit->setMultiplier(newMultiplier); 
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(unit->getExponent());
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*unit->getExponent());  
      ud->addUnit(newUnit);
 //     newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*unit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_JOULE:
      /* 1 joule = 1 m^2 kg s^-2 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*unit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*unit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_KATAL:
      /* 1 katal = 1 mol s^-1 */
      newUnit->setKind(UNIT_KIND_MOLE);
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*unit->getExponent());  
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
      /* hack to force multiplier to be double precision */
      newMultiplier = pow((newUnit->getMultiplier() * 0.001), 1.0/3.0);

      ossMultiplier << newMultiplier;
      newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);

      newUnit->setMultiplier(newMultiplier); 
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
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*unit->getExponent());  
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
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(unit->getExponent());
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*unit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_OHM:
      /* 1 ohm = 1 m^2 kg s^-3 A^-2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      /* hack to force multiplier to be double precision */
      newMultiplier = (1.0/sqrt(newUnit->getMultiplier()));

      ossMultiplier << newMultiplier;
      newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);

      newUnit->setMultiplier(newMultiplier); 
      newUnit->setExponent(-2*newUnit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(unit->getExponent());
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*unit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*unit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_PASCAL:
      /* 1 pascal = 1 m^-1 kg s^-2 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*unit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*unit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_SECOND:
      /* second is the SI unit of time */
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_SIEMENS:
      /* 1 siemen = 1 m^-2 kg^-1 s^3 A^2 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      /* hack to force multiplier to be double precision */
      newMultiplier = sqrt(newUnit->getMultiplier());

      ossMultiplier << newMultiplier;
      newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);

      newUnit->setMultiplier(newMultiplier); 
      newUnit->setExponent(2*newUnit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-1*unit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*unit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(3*unit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_TESLA:
      /* 1 tesla = 1 kg s^-2 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      /* hack to force multiplier to be double precision */
      newMultiplier = (1.0/(newUnit->getMultiplier()));

      ossMultiplier << newMultiplier;
      newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);

      newUnit->setMultiplier(newMultiplier); 
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(unit->getExponent());
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*unit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_VOLT:
      /* 1 volt = 1 m^2 kg s^-3 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      /* hack to force multiplier to be double precision */
      newMultiplier = (1.0/(newUnit->getMultiplier()));

      ossMultiplier << newMultiplier;
      newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);

      newUnit->setMultiplier(newMultiplier); 
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(unit->getExponent());
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*unit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*unit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_WATT:
      /* 1 watt = 1 m^2 kg s^-3 */
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*unit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-3*unit->getExponent());  
      ud->addUnit(newUnit);
      break;

    case UNIT_KIND_WEBER:
      /* 1 weber = 1 m^2 kg s^-2 A^-1 */
      newUnit->setKind(UNIT_KIND_AMPERE);
      /* hack to force multiplier to be double precision */
      newMultiplier = (1.0/(newUnit->getMultiplier()));

      ossMultiplier << newMultiplier;
      newMultiplier = strtod(ossMultiplier.str().c_str(), NULL);

      newUnit->setMultiplier(newMultiplier); 
      newUnit->setExponent(-1*newUnit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_KILOGRAM);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(unit->getExponent());
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_METRE);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(2*unit->getExponent());  
      ud->addUnit(newUnit);
//      newUnit = new Unit(uKind, unit->getExponent(), unit->getScale(), unit->getMultiplier());
      newUnit->setKind(UNIT_KIND_SECOND);
      newUnit->setMultiplier(1.0);
      newUnit->setExponent(-2*unit->getExponent());  
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
  if (name == "meter" 
   || name == "liter"
   || name == "avogadro")
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
  if (name == "meter" 
   || name == "liter" 
   || name == "Celsius"
   || name == "avogadro")

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
Unit::isL3UnitKind (const std::string& name)
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

  if (level > 1)
  {
    expectedAttributes.push_back("metaid");
    expectedAttributes.push_back("multiplier");

    if (level == 2 && version == 1)
    {
      expectedAttributes.push_back("offset");
    }

    if (!(level == 2 && version < 3))
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
  // kind: UnitKind  (L1v1, L1v2, L2v1->)
  //
  string kind;
  if ( attributes.readInto("kind", kind, getErrorLog(), true) )
  {
    mKind = UnitKind_forName( kind.c_str() );
    if (mKind == UNIT_KIND_CELSIUS)
    {
      if (!(level == 1) && !(level == 2 && version == 1))
      {
        SBMLError * err = new SBMLError(CelsiusNoLongerValid);
        logError(NotSchemaConformant, level, version, err->getMessage());
        delete err;
      }
    }
  }

  //
  // exponent  { use="optional" default="1" }  (L1v1, L1v2, L2v1->)
  // exponent  { use="required" }  (L3v1 ->)
  //
  if (level < 3)
  {
    if (attributes.readInto("exponent", mExponent, getErrorLog()))
    {
      mExponentDouble = (double)(mExponent);
      mIsSetExponent = true;
    }
  }
  else
  {
    mIsSetExponent = attributes.readInto("exponent", mExponentDouble, 
                                          getErrorLog(), true);
  }
  //
  // scale  { use="optional" default="0" }  (L1v1, L1v2, L2v1->)
  // scale  { use="required" }  (L3v1->)
  //
  if (level < 3)
  {
    attributes.readInto("scale", mScale, getErrorLog());
  }
  else
  {
    mIsSetScale = attributes.readInto("scale", mScale, getErrorLog(), true);
  }

  if (level > 1)
  {
    //
    // multiplier  { use="optional" default="1" }  (L2v1-> )
    // multiplier  { use="required" }  (L3v1-> )
    //
    if (level < 3)
    {
      attributes.readInto("multiplier", mMultiplier, getErrorLog());
    }
    else
    {
      mIsSetMultiplier = attributes.readInto("multiplier", mMultiplier, 
                                              getErrorLog(), true);
    }
    //
    // offset  { use="optional" default="0" }  (L2v1)
    //
    if (level == 2 && version == 1)
      attributes.readInto("offset", mOffset, getErrorLog());

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v3->)
    //
    if (!(level == 2 && version < 3)) 
        mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);
  }

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
  // kind: UnitKind  { use="required" }  (L1v1, L1v2, L2v1->)
  //
  const string kind = UnitKind_toString(mKind);
  stream.writeAttribute("kind", kind);

  //
  // exponent  { use="optional" default="1" }  (L1v1, L1v2, L2v1->)
  // exponent  { use="required" }  (L3v1 ->)
  //
  if (level < 3)
  {
    int e = static_cast<int>( mExponent );
    if (e != 1) stream.writeAttribute("exponent", e);
  }
  else
  {
    stream.writeAttribute("exponent", mExponentDouble);
  }
 
  //
  // scale  { use="optional" default="0" }  (L1v1, L1v2, L2v1->)
  // scale  { use="required" }  (L3v1->)
  //
  if (level < 3)
  {
    if (mScale != 0) stream.writeAttribute("scale", mScale);
  }
  else
  {
    stream.writeAttribute("scale", mScale);
  }

  if (level > 1)
  {
    //
    // multiplier  { use="optional" default="1" }  (L2v1->)
    // multiplier  { use="required" }  (L3v1-> )
    //
    if (level < 3)
    {
      if (mMultiplier != 1) stream.writeAttribute("multiplier", mMultiplier);
    }
    else
    {
      stream.writeAttribute("multiplier", mMultiplier);
    }
    //
    // offset  { use="optional" default="0" }  (L2v1)
    //
    if (level == 2 && version == 1 && mOffset != 0) 
    {
      stream.writeAttribute("offset", mOffset);
    }

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v3->)
    //
    if (!(level == 2 && version < 3)) 
        SBO::writeTerm(stream, mSBOTerm);
  }

}
/** @endcond doxygen-libsbml-internal */


/*
 * @return a (deep) copy of this ListOfUnits.
 */
ListOfUnits*
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


/* return nth item in list */
Unit *
ListOfUnits::get(unsigned int n)
{
  return static_cast<Unit*>(ListOf::get(n));
}


/* return nth item in list */
const Unit *
ListOfUnits::get(unsigned int n) const
{
  return static_cast<const Unit*>(ListOf::get(n));
}


/* Removes the nth item from this list */
Unit*
ListOfUnits::remove (unsigned int n)
{
   return static_cast<Unit*>(ListOf::remove(n));
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
    try
    {
      object = new Unit(getSBMLNamespaces());
    }
    catch (SBMLConstructorException*)
    {
      object = new Unit(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    catch ( ... )
    {
      object = new Unit(SBMLDocument::getDefaultLevel(),
        SBMLDocument::getDefaultVersion());
    }
    
    if (object) mItems.push_back(object);
  }

  return object;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */

/**
 * Creates a new Unit_t structure using the given SBML @p level
 * and @p version values.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * Unit
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * Unit
 *
 * @return a pointer to the newly created Unit_t structure.
 *
 * @note Once a Unit has been added to an SBMLDocument, the @p
 * level and @p version for the document @em override those used to create
 * the Unit.  Despite this, the ability to supply the values at
 * creation time is an important aid to creating valid SBML.  Knowledge of
 * the intended SBML Level and Version  determine whether it is valid to
 * assign a particular value to an attribute, or whether it is valid to add
 * an object to an existing SBMLDocument.
 */
LIBSBML_EXTERN
Unit_t *
Unit_create (unsigned int level, unsigned int version)
{
  try
  {
    Unit* obj = new Unit(level,version);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
}


/**
 * Creates a new Unit_t structure using the given
 * SBMLNamespaces_t structure.
 *
 * @param sbmlns SBMLNamespaces, a pointer to an SBMLNamespaces structure
 * to assign to this Unit
 *
 * @return a pointer to the newly created Unit_t structure.
 *
 * @note Once a Unit has been added to an SBMLDocument, the
 * @p sbmlns namespaces for the document @em override those used to create
 * the Unit.  Despite this, the ability to supply the values at creation time
 * is an important aid to creating valid SBML.  Knowledge of the intended SBML
 * Level and Version determine whether it is valid to assign a particular value
 * to an attribute, or whether it is valid to add an object to an existing
 * SBMLDocument.
 */
LIBSBML_EXTERN
Unit_t *
Unit_createWithNS (SBMLNamespaces_t* sbmlns)
{
  try
  {
    Unit* obj = new Unit(sbmlns);
    return obj;
  }
  catch (SBMLConstructorException)
  {
    return NULL;
  }
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
 * Returns a list of XMLNamespaces_t associated with this Unit_t
 * structure.
 *
 * @param u the Unit_t structure
 * 
 * @return pointer to the XMLNamespaces_t structure associated with 
 * this SBML object
 */
LIBSBML_EXTERN
const XMLNamespaces_t *
Unit_getNamespaces(Unit_t *u)
{
  return u->getNamespaces();
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
 * Returns the value of the "exponent" attribute of the given Unit_t
 * structure @p u.
 *
 * @param u a Unit_t structure
 *
 * @return the "exponent" value of this Unit_t structure, as an integer.
 */
LIBSBML_EXTERN
double
Unit_getExponentAsDouble (const Unit_t *u)
{
  return u->getExponentAsDouble();
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
 * Predicate to test whether the "exponent" attribute of the given Unit_t
 * structure @p u has been set.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "exponent" attribute of the given
 * Unit_t structure has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSetExponent (const Unit_t *u)
{
  return static_cast<int>( u->isSetExponent() );
}


/**
 * Predicate to test whether the "multiplier" attribute of the given Unit_t
 * structure @p u has been set.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "multiplier" attribute of the given
 * Unit_t structure has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSetMultiplier (const Unit_t *u)
{
  return static_cast<int>( u->isSetMultiplier() );
}


/**
 * Predicate to test whether the "scale" attribute of the given Unit_t
 * structure @p u has been set.
 *
 * @param u the Unit_t structure to query
 * 
 * @return nonzero (for true) if the "scale" attribute of the given
 * Unit_t structure has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSetScale (const Unit_t *u)
{
  return static_cast<int>( u->isSetScale() );
}


/**
 * Sets the kind of the given Unit_t structure @p u to the given
 * UnitKind_t value.
 *
 * @param u the Unit_t structure whose value is to be set
 * @param kind a value from the UnitKind_t enumeration 
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
LIBSBML_EXTERN
int
Unit_setKind (Unit_t *u, UnitKind_t kind)
{
  return u->setKind(kind);
}


/**
 * Sets the "exponent" attribute value of the given Unit_t structure @p u.
 *
 * @param u the Unit_t structure whose value is to be set
 * @param value the integer to which the attribute "exponent" should be set
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
Unit_setExponent (Unit_t *u, int value)
{
  return u->setExponent(value);
}


/**
 * Sets the "exponent" attribute value of the given Unit_t structure @p u.
 *
 * @param u the Unit_t structure whose value is to be set
 * @param value the double to which the attribute "exponent" should be set
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
Unit_setExponentAsDouble (Unit_t *u, double value)
{
  return u->setExponent(value);
}


/**
 * Sets the "scale" attribute value of the given Unit_t structure @p u.
 *
 * @param u the Unit_t structure whose value is to be set
 * @param value the integer to which the attribute "scale" should be set
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 */
LIBSBML_EXTERN
int
Unit_setScale (Unit_t *u, int value)
{
  return u->setScale(value);
}


/**
 * Sets the "multiplier" attribute value of the given Unit_t structure @p u.
 *
 * @param u the Unit_t structure whose value is to be set
 * @param value the integer to which the attribute "multiplier" should be set
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
 */
LIBSBML_EXTERN
int
Unit_setMultiplier (Unit_t *u, double value)
{
  return u->setMultiplier(value);
}


/**
 * Sets the "offset" attribute value of the given Unit_t structure @p u.
 * 
 * @param u the Unit_t structure whose value is to be set
 * @param value the integer to which the attribute "offset" should be set
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_UNEXPECTED_ATTRIBUTE
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
int
Unit_setOffset (Unit_t *u, double value)
{
  return u->setOffset(value);
}


/**
 * Predicate returning @c true or @c false depending on whether
 * all the required attributes for this Unit object
 * have been set.
 *
 * @note The required attributes for a Unit object are:
 * @li kind
 * @li exponent (L3 on)
 * @li multiplier (L3 on)
 * @li scale (L3 on)
 *
 * @return a boolean value indicating whether all the required
 * elements for this object have been defined.
 */
LIBSBML_EXTERN
int
Unit_hasRequiredAttributes(Unit_t *u)
{
  return static_cast <int> (u->hasRequiredAttributes());
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

LIBSBML_EXTERN
int 
Unit_areIdentical(Unit_t * unit1, Unit_t * unit2)
{
  return static_cast<int>(Unit::areIdentical(
    static_cast<Unit*>(unit1), static_cast<Unit*>(unit2)));
}

LIBSBML_EXTERN
int
Unit_areEquivalent(Unit_t * unit1, Unit_t * unit2)
{
  return static_cast<int>(Unit::areEquivalent(
    static_cast<Unit*>(unit1), static_cast<Unit*>(unit2)));
}

LIBSBML_EXTERN
int 
Unit_removeScale(Unit_t * unit)
{
  return Unit::removeScale(static_cast<Unit*>(unit));
}

LIBSBML_EXTERN
void 
Unit_merge(Unit_t * unit1, Unit_t * unit2)
{
  Unit::merge(static_cast<Unit*>(unit1), static_cast<Unit*>(unit2));
}

LIBSBML_EXTERN
UnitDefinition_t * 
Unit_convertToSI(Unit_t * unit)
{
  return static_cast<UnitDefinition_t*>(Unit::convertToSI(
    static_cast<Unit*>(unit)));
}

/** @endcond doxygen-c-only */

LIBSBML_CPP_NAMESPACE_END
