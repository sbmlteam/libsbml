/**
 * \file    Unit.cpp
 * \brief   SBML Unit
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
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


#include "xml/XMLAttributes.h"
#include "xml/XMLInputStream.h"
#include "xml/XMLOutputStream.h"

#include "SBMLVisitor.h"
#include "Unit.h"


using namespace std;


/**
 * Creates a new Unit, optionally with its kind, exponent, scale,
 * multiplier, and offset attributes set.
 */
Unit::Unit (   UnitKind_t  kind
             , int         exponent
             , int         scale
             , double      multiplier
             , double      offset     ) :
    SBase      ()
  , mKind      ( kind       )
  , mExponent  ( exponent   )
  , mScale     ( scale      )
  , mMultiplier( multiplier )
  , mOffset    ( offset     )
{
}


/**
 * Creates a new Unit, optionally with its kind (via string), exponent,
 * scale, multiplier, and offset attributes set.
 */
Unit::Unit (   const string&  kind
             , int            exponent 
             , int            scale
             , double         multiplier
             , double         offset     ) :
    SBase      ()
  , mKind      ( UnitKind_forName( kind.c_str() ) )
  , mExponent  ( exponent   )
  , mScale     ( scale      )
  , mMultiplier( multiplier )
  , mOffset    ( offset     )
{
}


/**
 * Destroys the given Unit.
 */
Unit::~Unit ()
{
}


/**
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


/**
 * @return a (deep) copy of this Unit.
 */
SBase*
Unit::clone () const
{
  return new Unit(*this);
}


/**
 * Initializes the fields of this Unit to their defaults:
 *
 *   - exponent   = 1
 *   - scale      = 0
 *   - multiplier = 1.0
 *   - offset     = 0.0
 */
void
Unit::initDefaults ()
{
  setExponent  ( 1   );
  setScale     ( 0   );
  setMultiplier( 1.0 );
  setOffset    ( 0.0 );
}


/**
 * @return the kind of this Unit.
 */
UnitKind_t
Unit::getKind () const
{
  return mKind;
}


/**
 * @return the exponent of this Unit.
 */
int
Unit::getExponent () const
{
  return mExponent;
}


/**
 * @return the scale of this Unit.
 */
int
Unit::getScale () const
{
  return mScale;
}


/**
 * @return the multiplier of this Unit.
 */
double
Unit::getMultiplier () const
{
  return mMultiplier;
}


/**
 * @return the offset of this Unit.
 */
double
Unit::getOffset () const
{
  return mOffset;
}


/**
 * @return true if the kind of this Unit is 'ampere', false otherwise.
 */
bool
Unit::isAmpere () const
{
  return (mKind == UNIT_KIND_AMPERE);
}


/**
 * @return true if the kind of this Unit is 'becquerel', false otherwise.
 */
bool
Unit::isBecquerel () const
{
  return (mKind == UNIT_KIND_BECQUEREL);
}


/**
 * @return true if the kind of this Unit is 'candela', false otherwise.
 */
bool
Unit::isCandela () const
{
  return (mKind == UNIT_KIND_CANDELA);
}


/**
 * @return true if the kind of this Unit is 'Celsius', false otherwise.
 */
bool
Unit::isCelsius () const
{
  return (mKind == UNIT_KIND_CELSIUS);
}


/**
 * @return true if the kind of this Unit is 'coulomb', false otherwise.
 */
bool
Unit::isCoulomb () const
{
  return (mKind == UNIT_KIND_COULOMB);
}


/**
 * @return true if the kind of this Unit is 'dimensionless', false
 * otherwise.
 */
bool
Unit::isDimensionless () const
{
  return (mKind == UNIT_KIND_DIMENSIONLESS);
}


/**
 * @return true if the kind of this Unit is 'farad', false otherwise.
 */
bool
Unit::isFarad () const
{
  return (mKind == UNIT_KIND_FARAD);
}


/**
 * @return true if the kind of this Unit is 'gram', false otherwise.
 */
bool
Unit::isGram () const
{
  return (mKind == UNIT_KIND_GRAM);
}


/**
 * @return true if the kind of this Unit is 'gray', false otherwise.
 */
bool
Unit::isGray () const
{
  return (mKind == UNIT_KIND_GRAY);
}


/**
 * @return true if the kind of this Unit is 'henry', false otherwise.
 */
bool
Unit::isHenry () const
{
  return (mKind == UNIT_KIND_HENRY);
}


/**
 * @return true if the kind of this Unit is 'hertz', false otherwise.
 */
bool
Unit::isHertz () const
{
  return (mKind == UNIT_KIND_HERTZ);
}


/**
 * @return true if the kind of this Unit is 'item', false otherwise.
 */
bool
Unit::isItem () const
{
  return (mKind == UNIT_KIND_ITEM);
}


/**
 * @return true if the kind of this Unit is 'joule', false otherwise.
 */
bool
Unit::isJoule () const
{
  return (mKind == UNIT_KIND_JOULE);
}


/**
 * @return true if the kind of this Unit is 'katal', false otherwise.
 */
bool
Unit::isKatal () const
{
  return (mKind == UNIT_KIND_KATAL);
}


/**
 * @return true if the kind of this Unit is 'kelvin', false otherwise.
 */
bool
Unit::isKelvin () const
{
  return (mKind == UNIT_KIND_KELVIN);
}


/**
 * @return true if the kind of this Unit is 'kilogram', false otherwise.
 */
bool
Unit::isKilogram () const
{
  return (mKind == UNIT_KIND_KILOGRAM);
}


/**
 * @return true if the kind of this Unit is 'litre' or 'liter', false
 * otherwise.
 */
bool
Unit::isLitre () const
{
  return (mKind == UNIT_KIND_LITRE || mKind == UNIT_KIND_LITER);
}


/**
 * @return true if the kind of this Unit is 'lumen', false otherwise.
 */
bool
Unit::isLumen () const
{
  return (mKind == UNIT_KIND_LUMEN);
}


/**
 * @return true if the kind of this Unit is 'lux', false otherwise.
 */
bool
Unit::isLux () const
{
  return (mKind == UNIT_KIND_LUX);
}


/**
 * @return true if the kind of this Unit is 'metre' or 'meter', false
 * otherwise.
 */
bool
Unit::isMetre () const
{
  return (mKind == UNIT_KIND_METRE || mKind == UNIT_KIND_METER);
}


/**
 * @return true if the kind of this Unit is 'mole', false otherwise.
 */
bool
Unit::isMole () const
{
  return (mKind == UNIT_KIND_MOLE);
}


/**
 * @return true if the kind of this Unit is 'newton', false otherwise.
 */
bool
Unit::isNewton () const
{
  return (mKind == UNIT_KIND_NEWTON);
}


/**
 * @return true if the kind of this Unit is 'ohm', false otherwise.
 */
bool
Unit::isOhm () const
{
  return (mKind == UNIT_KIND_OHM);
}


/**
 * @return true if the kind of this Unit is 'pascal', false otherwise.
 */
bool
Unit::isPascal () const
{
  return (mKind == UNIT_KIND_PASCAL);
}


/**
 * @return true if the kind of this Unit is 'radian', false otherwise.
 */
bool
Unit::isRadian () const
{
  return (mKind == UNIT_KIND_RADIAN);
}


/**
 * @return true if the kind of this Unit is 'second', false otherwise.
 */
bool
Unit::isSecond () const
{
  return (mKind == UNIT_KIND_SECOND);
}


/**
 * @return true if the kind of this Unit is 'siemens', false otherwise.
 */
bool
Unit::isSiemens () const
{
  return (mKind == UNIT_KIND_SIEMENS);
}


/**
 * @return true if the kind of this Unit is 'sievert', false otherwise.
 */
bool
Unit::isSievert () const
{
  return (mKind == UNIT_KIND_SIEVERT);
}


/**
 * @return true if the kind of this Unit is 'steradian', false otherwise.
 */
bool
Unit::isSteradian () const
{
  return (mKind == UNIT_KIND_STERADIAN);
}


/**
 * @return true if the kind of this Unit is 'tesla', false otherwise.
 */
bool
Unit::isTesla () const
{
  return (mKind == UNIT_KIND_TESLA);
}


/**
 * @return true if the kind of this Unit is 'volt', false otherwise.
 */
bool
Unit::isVolt () const
{
  return (mKind == UNIT_KIND_VOLT);
}


/**
 * @return true if the kind of this Unit is 'watt', false otherwise.
 */
bool
Unit::isWatt () const
{
  return (mKind == UNIT_KIND_WATT);
}


/**
 * @return true if the kind of this Unit is 'weber', false otherwise.
 */
bool
Unit::isWeber () const
{
  return (mKind == UNIT_KIND_WEBER);
}


/**
 * @return true if the kind of this Unit has been set, false otherwise.
 */
bool
Unit::isSetKind () const
{
  return (mKind != UNIT_KIND_INVALID);
}


/**
 * Sets the kind of this Unit to the given UnitKind.
 */
void
Unit::setKind (UnitKind_t kind)
{
  mKind = kind;
}


/**
 * Sets the exponent of this Unit to the given value.
 */
void
Unit::setExponent (int value)
{
  mExponent = value;
}


/**
 * Sets the scale of this Unit to the given value.
 */
void
Unit::setScale (int value)
{
  mScale = value;
}


/**
 * Sets the multiplier of this Unit to the given value.
 */
void
Unit::setMultiplier (double value)
{
  mMultiplier = value;
}


/**
 * Sets the offset of this Unit to the given value.
 */
void
Unit::setOffset (double value)
{
  mOffset = value;
}


/**
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


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
Unit::getElementName () const
{
  static const string name = "unit";
  return name;
}


/**
 * @return true if name is one of the five SBML builtin Unit names
 * ('substance', 'volume', 'area', 'length' or 'time'), false otherwise.
 */
bool
Unit::isBuiltIn (const string& name)
{
  return
    name == "substance" ||
    name == "volume"    ||
    name == "area"      ||
    name == "length"    ||
    name == "time";
}


/**
 * @return true if name is a valid UnitKind.
 */
bool
Unit::isUnitKind (const string& name)
{
  return (UnitKind_forName( name.c_str() ) != UNIT_KIND_INVALID);
}


/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Unit::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  //
  // kind: UnitKind  (L1v1, L1v2, L2v1, L2v2)
  //
  string kind;
  if ( attributes.readInto("kind", kind) )
  {
    mKind = UnitKind_forName( kind.c_str() );
  }

  //
  // exponent  { use="optional" default="1" }  (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("exponent", mExponent);

  //
  // scale  { use="optional" default="0" }  (L1v1, L1v2, L2v1, L2v2)
  //
  attributes.readInto("scale", mScale);

  if (getLevel() == 2)
  {
    //
    // multiplier  { use="optional" default="1" }  (L2v1, L2v2)
    //
    attributes.readInto("multiplier", mMultiplier);

    //
    // offset  { use="optional" default="0" }  (L2v1)
    //
    attributes.readInto("offset", mOffset);
  }
}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Unit::writeAttributes (XMLOutputStream& stream)
{
  SBase::writeAttributes(stream);

  //
  // kind: UnitKind  { use="required" }  (L1v1, L1v2, L2v1, L2v2)
  //
  stream.writeAttribute("kind", UnitKind_toString(mKind));

  //
  // exponent  { use="optional" default="1" }  (L1v1, L1v2, L2v1, L2v2)
  //
  if (mExponent != 1) stream.writeAttribute("exponent", mExponent);

  //
  // scale  { use="optional" default="0" }  (L1v1, L1v2, L2v1, L2v2)
  //
  if (mScale != 0) stream.writeAttribute("scale", mScale);

  if (getLevel() == 2)
  {
    //
    // multiplier  { use="optional" default="1" }  (L2v1, L2v2)
    //
    stream.writeAttribute("multiplier", mMultiplier);

    //
    // offset  { use="optional" default="0" }  (L2v1, L2v2)
    //
    stream.writeAttribute("offset", mOffset);
  }
}




/**
 * @return a (deep) copy of this ListOfUnits.
 */
SBase*
ListOfUnits::clone () const
{
  return new ListOfUnits(*this);
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfUnits::getItemTypeCode () const
{
  return SBML_UNIT;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
ListOfUnits::getElementName () const
{
  static const string name = "listOfUnits";
  return name;
}


/**
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




/**
 * Creates a new Unit and returns a pointer to it.
 */
LIBSBML_EXTERN
Unit_t *
Unit_create (void)
{
  return new(nothrow) Unit;
}


/**
 * Creates a new Unit with the given kind, exponent and scale and returns a
 * pointer to it.  This convenience function is functionally equivalent to:
 *
 *   Unit_t *u = Unit_create();
 *   Unit_setKind(kind); Unit_setExponent(exponent); ...;
 */
LIBSBML_EXTERN
Unit_t *
Unit_createWith (UnitKind_t kind, int exponent, int scale)
{
  return new(nothrow) Unit(kind, exponent, scale);
}


/**
 * Frees the given Unit.
 */
LIBSBML_EXTERN
void
Unit_free (Unit_t *u)
{
  delete u;
}


/**
 * Initializes the fields of this Unit to their defaults:
 *
 *   - exponent   = 1
 *   - scale      = 0
 *   - multiplier = 1.0
 *   - offset     = 0.0
 */
LIBSBML_EXTERN
void
Unit_initDefaults (Unit_t *u)
{
  u->initDefaults();
}


/**
 * @return the kind of this Unit.
 */
LIBSBML_EXTERN
UnitKind_t
Unit_getKind (const Unit_t *u)
{
  return u->getKind();
}


/**
 * @return the exponent of this Unit.
 */
LIBSBML_EXTERN
int
Unit_getExponent (const Unit_t *u)
{
  return u->getExponent();
}


/**
 * @return the scale of this Unit.
 */
LIBSBML_EXTERN
int
Unit_getScale (const Unit_t *u)
{
  return u->getScale();
}


/**
 * @return the multiplier of this Unit.
 */
LIBSBML_EXTERN
double
Unit_getMultiplier (const Unit_t *u)
{
  return u->getMultiplier();
}


/**
 * @return the offset of this Unit.
 */
LIBSBML_EXTERN
double
Unit_getOffset (const Unit_t *u)
{
  return u->getOffset();
}


/**
 * @return non-zero if the kind of this Unit is 'ampere', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isAmpere (const Unit_t *u)
{
  return static_cast<int>( u->isAmpere() );
}


/**
 * @return non-zero if the kind of this Unit is 'becquerel', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isBecquerel (const Unit_t *u)
{
  return static_cast<int>( u->isBecquerel() );
}


/**
 * @return non-zero if the kind of this Unit is 'candela', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCandela (const Unit_t *u)
{
  return static_cast<int>( u->isCandela() );
}


/**
 * @return non-zero if the kind of this Unit is 'Celsius', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCelsius (const Unit_t *u)
{
  return static_cast<int>( u->isCelsius() );
}


/**
 * @return non-zero if the kind of this Unit is 'coulomb', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCoulomb (const Unit_t *u)
{
  return static_cast<int>( u->isCoulomb() );
}


/**
 * @return non-zero if the kind of this Unit is 'dimensionless', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isDimensionless (const Unit_t *u)
{
  return static_cast<int>( u->isDimensionless() );
}


/**
 * @return non-zero if the kind of this Unit is 'farad', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isFarad (const Unit_t *u)
{
  return static_cast<int>( u->isFarad() );
}


/**
 * @return non-zero if the kind of this Unit is 'gram', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGram (const Unit_t *u)
{
  return static_cast<int>( u->isGram() );
}


/**
 * @return non-zero if the kind of this Unit is 'gray', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGray (const Unit_t *u)
{
  return static_cast<int>( u->isGray() );
}


/**
 * @return non-zero if the kind of this Unit is 'henry', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHenry (const Unit_t *u)
{
  return static_cast<int>( u->isHenry() );
}


/**
 * @return non-zero if the kind of this Unit is 'hertz', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHertz (const Unit_t *u)
{
  return static_cast<int>( u->isHertz() );
}


/**
 * @return non-zero if the kind of this Unit is 'item', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isItem (const Unit_t *u)
{
  return static_cast<int>( u->isItem() );
}


/**
 * @return non-zero if the kind of this Unit is 'joule', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isJoule (const Unit_t *u)
{
  return static_cast<int>( u->isJoule() );
}


/**
 * @return non-zero if the kind of this Unit is 'katal', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKatal (const Unit_t *u)
{
  return static_cast<int>( u->isKatal() );
}


/**
 * @return non-zero if the kind of this Unit is 'kelvin', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKelvin (const Unit_t *u)
{
  return static_cast<int>( u->isKelvin() );
}


/**
 * @return non-zero if the kind of this Unit is 'kilogram', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKilogram (const Unit_t *u)
{
  return static_cast<int>( u->isKilogram() );
}


/**
 * @return non-zero if the kind of this Unit is 'litre' or 'liter', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLitre (const Unit_t *u)
{
  return static_cast<int>( u->isLitre() );
}


/**
 * @return non-zero if the kind of this Unit is 'lumen', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLumen (const Unit_t *u)
{
  return static_cast<int>( u->isLumen() );
}


/**
 * @return non-zero if the kind of this Unit is 'lux', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLux (const Unit_t *u)
{
  return static_cast<int>( u->isLux() );
}


/**
 * @return non-zero if the kind of this Unit is 'metre' or 'meter', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMetre (const Unit_t *u)
{
  return static_cast<int>( u->isMetre() );
}


/**
 * @return non-zero if the kind of this Unit is 'mole', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMole (const Unit_t *u)
{
  return static_cast<int>( u->isMole() );
}


/**
 * @return non-zero if the kind of this Unit is 'newton', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isNewton (const Unit_t *u)
{
  return static_cast<int>( u->isNewton() );
}


/**
 * @return non-zero if the kind of this Unit is 'ohm', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isOhm (const Unit_t *u)
{
  return static_cast<int>( u->isOhm() );
}


/**
 * @return non-zero if the kind of this Unit is 'pascal', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isPascal (const Unit_t *u)
{
  return static_cast<int>( u->isPascal() );
}


/**
 * @return non-zero if the kind of this Unit is 'radian', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isRadian (const Unit_t *u)
{
  return static_cast<int>( u->isRadian() );
}


/**
 * @return non-zero if the kind of this Unit is 'second', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSecond (const Unit_t *u)
{
  return static_cast<int>( u->isSecond() );
}


/**
 * @return non-zero if the kind of this Unit is 'siemens', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSiemens (const Unit_t *u)
{
  return static_cast<int>( u->isSiemens() );
}


/**
 * @return non-zero if the kind of this Unit is 'sievert', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSievert (const Unit_t *u)
{
  return static_cast<int>( u->isSievert() );
}


/**
 * @return non-zero if the kind of this Unit is 'steradian', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSteradian (const Unit_t *u)
{
  return static_cast<int>( u->isSteradian() );
}


/**
 * @return non-zero if the kind of this Unit is 'tesla', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isTesla (const Unit_t *u)
{
  return static_cast<int>( u->isTesla() );
}


/**
 * @return non-zero if the kind of this Unit is 'volt', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isVolt (const Unit_t *u)
{
  return static_cast<int>( u->isVolt() );
}


/**
 * @return non-zero if the kind of this Unit is 'watt', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWatt (const Unit_t *u)
{
  return static_cast<int>( u->isWatt() );
}


/**
 * @return non-zero if the kind of this Unit is 'weber', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWeber (const Unit_t *u)
{
  return static_cast<int>( u->isWeber() );
}


/**
 * @return non-zero if the kind of this Unit has been set, zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSetKind (const Unit_t *u)
{
  return static_cast<int>( u->isSetKind() );
}


/**
 * Sets the kind of this Unit to the given UnitKind.
 */
LIBSBML_EXTERN
void
Unit_setKind (Unit_t *u, UnitKind_t kind)
{
  u->setKind(kind);
}


/**
 * Sets the exponent of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setExponent (Unit_t *u, int value)
{
  u->setExponent(value);
}


/**
 * Sets the scale of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setScale (Unit_t *u, int value)
{
  u->setScale(value);
}


/**
 * Sets the multiplier of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setMultiplier (Unit_t *u, double value)
{
  u->setMultiplier(value);
}


/**
 * Sets the offset of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setOffset (Unit_t *u, double value)
{
  u->setOffset(value);
}


/**
 * @return non-zero if name is one of the five SBML builtin Unit names
 * ('substance', 'volume', 'area', 'length' or 'time'), zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isBuiltIn (const char *name)
{
  return Unit::isBuiltIn(name != NULL ? name : "");
}
