/**
 * \file    Unit.cpp
 * \brief   SBML Unit
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "util/util.h"

#include "SBMLVisitor.h"
#include "Unit.h"


/**
 * Creates a new Unit, optionally with its kind, exponent, scale,
 * multiplier, and offset attributes set.
 */
LIBSBML_EXTERN
Unit::Unit (   UnitKind_t  kind
             , int         exponent
             , int         scale
             , double      multiplier
             , double      offset     ) :
    SBase     ()
  , kind      ( kind       )
  , exponent  ( exponent   )
  , scale     ( scale      )
  , multiplier( multiplier )
  , offset    ( offset     )
{
  init(SBML_UNIT);
}


/**
 * Creates a new Unit, optionally with its kind (via string), exponent,
 * scale, multiplier, and offset attributes set.
 */
LIBSBML_EXTERN
Unit::Unit (   const std::string&  kind
             , int                 exponent 
             , int                 scale
             , double              multiplier
             , double              offset     ) :
    SBase     ()
  , kind      ( UNIT_KIND_INVALID )
  , exponent  ( exponent   )
  , scale     ( scale      )
  , multiplier( multiplier )
  , offset    ( offset     )
{
  init(SBML_UNIT);

  if ( !kind.empty() )
  {
    setKind(UnitKind_forName( kind.c_str() ));
  }
}


/**
 * Destroys the given Unit.
 */
LIBSBML_EXTERN
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
LIBSBML_EXTERN
bool
Unit::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
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
LIBSBML_EXTERN
UnitKind_t
Unit::getKind () const
{
  return kind;
}


/**
 * @return the exponent of this Unit.
 */
LIBSBML_EXTERN
int
Unit::getExponent () const
{
  return exponent;
}


/**
 * @return the scale of this Unit.
 */
LIBSBML_EXTERN
int
Unit::getScale () const
{
  return scale;
}


/**
 * @return the multiplier of this Unit.
 */
LIBSBML_EXTERN
double
Unit::getMultiplier () const
{
  return multiplier;
}


/**
 * @return the offset of this Unit.
 */
LIBSBML_EXTERN
double
Unit::getOffset () const
{
  return offset;
}


/**
 * @return true if the kind of this Unit is 'ampere', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isAmpere () const
{
  return kind == UNIT_KIND_AMPERE;
}


/**
 * @return true if the kind of this Unit is 'becquerel', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isBecquerel () const
{
  return kind == UNIT_KIND_BECQUEREL;
}


/**
 * @return true if the kind of this Unit is 'candela', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isCandela () const
{
  return kind == UNIT_KIND_CANDELA;
}


/**
 * @return true if the kind of this Unit is 'Celsius', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isCelsius () const
{
  return kind == UNIT_KIND_CELSIUS;
}


/**
 * @return true if the kind of this Unit is 'coulomb', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isCoulomb () const
{
  return kind == UNIT_KIND_COULOMB;
}


/**
 * @return true if the kind of this Unit is 'dimensionless', false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isDimensionless () const
{
  return kind == UNIT_KIND_DIMENSIONLESS;
}


/**
 * @return true if the kind of this Unit is 'farad', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isFarad () const
{
  return kind == UNIT_KIND_FARAD;
}


/**
 * @return true if the kind of this Unit is 'gram', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isGram () const
{
  return kind == UNIT_KIND_GRAM;
}


/**
 * @return true if the kind of this Unit is 'gray', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isGray () const
{
  return kind == UNIT_KIND_GRAY;
}


/**
 * @return true if the kind of this Unit is 'henry', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isHenry () const
{
  return kind == UNIT_KIND_HENRY;
}


/**
 * @return true if the kind of this Unit is 'hertz', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isHertz () const
{
  return kind == UNIT_KIND_HERTZ;
}


/**
 * @return true if the kind of this Unit is 'item', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isItem () const
{
  return kind == UNIT_KIND_ITEM;
}


/**
 * @return true if the kind of this Unit is 'joule', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isJoule () const
{
  return kind == UNIT_KIND_JOULE;
}


/**
 * @return true if the kind of this Unit is 'katal', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isKatal () const
{
  return kind == UNIT_KIND_KATAL;
}


/**
 * @return true if the kind of this Unit is 'kelvin', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isKelvin () const
{
  return kind == UNIT_KIND_KELVIN;
}


/**
 * @return true if the kind of this Unit is 'kilogram', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isKilogram () const
{
  return kind == UNIT_KIND_KILOGRAM;
}


/**
 * @return true if the kind of this Unit is 'litre' or 'liter', false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isLitre () const
{
  return (kind == UNIT_KIND_LITRE || kind == UNIT_KIND_LITER);
}


/**
 * @return true if the kind of this Unit is 'lumen', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isLumen () const
{
  return kind == UNIT_KIND_LUMEN;
}


/**
 * @return true if the kind of this Unit is 'lux', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isLux () const
{
  return kind == UNIT_KIND_LUX;
}


/**
 * @return true if the kind of this Unit is 'metre' or 'meter', false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isMetre () const
{
  return (kind == UNIT_KIND_METRE || kind == UNIT_KIND_METER);
}


/**
 * @return true if the kind of this Unit is 'mole', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isMole () const
{
  return kind == UNIT_KIND_MOLE;
}


/**
 * @return true if the kind of this Unit is 'newton', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isNewton () const
{
  return kind == UNIT_KIND_NEWTON;
}


/**
 * @return true if the kind of this Unit is 'ohm', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isOhm () const
{
  return kind == UNIT_KIND_OHM;
}


/**
 * @return true if the kind of this Unit is 'pascal', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isPascal () const
{
  return kind == UNIT_KIND_PASCAL;
}


/**
 * @return true if the kind of this Unit is 'radian', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isRadian () const
{
  return kind == UNIT_KIND_RADIAN;
}


/**
 * @return true if the kind of this Unit is 'second', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isSecond () const
{
  return kind == UNIT_KIND_SECOND;
}


/**
 * @return true if the kind of this Unit is 'siemens', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isSiemens () const
{
  return kind == UNIT_KIND_SIEMENS;
}


/**
 * @return true if the kind of this Unit is 'sievert', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isSievert () const
{
  return kind == UNIT_KIND_SIEVERT;
}


/**
 * @return true if the kind of this Unit is 'steradian', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isSteradian () const
{
  return kind == UNIT_KIND_STERADIAN;
}


/**
 * @return true if the kind of this Unit is 'tesla', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isTesla () const
{
  return kind == UNIT_KIND_TESLA;
}


/**
 * @return true if the kind of this Unit is 'volt', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isVolt () const
{
  return kind == UNIT_KIND_VOLT;
}


/**
 * @return true if the kind of this Unit is 'watt', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isWatt () const
{
  return kind == UNIT_KIND_WATT;
}


/**
 * @return true if the kind of this Unit is 'weber', false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isWeber () const
{
  return kind == UNIT_KIND_WEBER;
}


/**
 * @return true if the kind of this Unit has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isSetKind () const
{
  return kind != UNIT_KIND_INVALID;
}


/**
 * Sets the kind of this Unit to the given UnitKind.
 */
LIBSBML_EXTERN
void
Unit::setKind (UnitKind_t kind)
{
  this->kind = kind;
}


/**
 * Sets the exponent of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit::setExponent (int value)
{
  exponent = value;
}


/**
 * Sets the scale of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit::setScale (int value)
{
  scale = value;
}


/**
 * Sets the multiplier of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit::setMultiplier (double value)
{
  multiplier = value;
}


/**
 * Sets the offset of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit::setOffset (double value)
{
  offset = value;
}


/**
 * @return true if name is one of the five SBML builtin Unit names
 * ('substance', 'volume', 'area', 'length' or 'time'), false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isBuiltIn (const std::string& name)
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
LIBSBML_EXTERN
bool
Unit::isUnitKind (const std::string& name)
{
  return UnitKind_forName( name.c_str() ) != UNIT_KIND_INVALID;
}




/**
 * Creates a new Unit and returns a pointer to it.
 */
LIBSBML_EXTERN
Unit_t *
Unit_create (void)
{
  return new(std::nothrow) Unit;
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
  return new(std::nothrow) Unit(kind, exponent, scale);
}


/**
 * Frees the given Unit.
 */
LIBSBML_EXTERN
void
Unit_free (Unit_t *u)
{
  delete static_cast<Unit*>(u);
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
  static_cast<Unit*>(u)->initDefaults();
}


/**
 * @return the kind of this Unit.
 */
LIBSBML_EXTERN
UnitKind_t
Unit_getKind (const Unit_t *u)
{
  return static_cast<const Unit*>(u)->getKind();
}


/**
 * @return the exponent of this Unit.
 */
LIBSBML_EXTERN
int
Unit_getExponent (const Unit_t *u)
{
  return static_cast<const Unit*>(u)->getExponent();
}


/**
 * @return the scale of this Unit.
 */
LIBSBML_EXTERN
int
Unit_getScale (const Unit_t *u)
{
  return static_cast<const Unit*>(u)->getScale();
}


/**
 * @return the multiplier of this Unit.
 */
LIBSBML_EXTERN
double
Unit_getMultiplier (const Unit_t *u)
{
  return static_cast<const Unit*>(u)->getMultiplier();
}


/**
 * @return the offset of this Unit.
 */
LIBSBML_EXTERN
double
Unit_getOffset (const Unit_t *u)
{
  return static_cast<const Unit*>(u)->getOffset();
}


/**
 * @return non-zero if the kind of this Unit is 'ampere', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isAmpere (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isAmpere();
}


/**
 * @return non-zero if the kind of this Unit is 'becquerel', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isBecquerel (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isBecquerel();
}


/**
 * @return non-zero if the kind of this Unit is 'candela', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCandela (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isCandela();
}


/**
 * @return non-zero if the kind of this Unit is 'Celsius', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCelsius (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isCelsius();
}


/**
 * @return non-zero if the kind of this Unit is 'coulomb', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCoulomb (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isCoulomb();
}


/**
 * @return non-zero if the kind of this Unit is 'dimensionless', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isDimensionless (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isDimensionless();
}


/**
 * @return non-zero if the kind of this Unit is 'farad', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isFarad (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isFarad();
}


/**
 * @return non-zero if the kind of this Unit is 'gram', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGram (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isGram();
}


/**
 * @return non-zero if the kind of this Unit is 'gray', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGray (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isGray();
}


/**
 * @return non-zero if the kind of this Unit is 'henry', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHenry (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isHenry();
}


/**
 * @return non-zero if the kind of this Unit is 'hertz', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHertz (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isHertz();
}


/**
 * @return non-zero if the kind of this Unit is 'item', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isItem (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isItem();
}


/**
 * @return non-zero if the kind of this Unit is 'joule', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isJoule (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isJoule();
}


/**
 * @return non-zero if the kind of this Unit is 'katal', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKatal (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isKatal();
}


/**
 * @return non-zero if the kind of this Unit is 'kelvin', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKelvin (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isKelvin();
}


/**
 * @return non-zero if the kind of this Unit is 'kilogram', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKilogram (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isKilogram();
}


/**
 * @return non-zero if the kind of this Unit is 'litre' or 'liter', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLitre (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isLitre();
}


/**
 * @return non-zero if the kind of this Unit is 'lumen', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLumen (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isLumen();
}


/**
 * @return non-zero if the kind of this Unit is 'lux', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLux (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isLux();
}


/**
 * @return non-zero if the kind of this Unit is 'metre' or 'meter', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMetre (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isMetre();
}


/**
 * @return non-zero if the kind of this Unit is 'mole', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMole (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isMole();
}


/**
 * @return non-zero if the kind of this Unit is 'newton', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isNewton (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isNewton();
}


/**
 * @return non-zero if the kind of this Unit is 'ohm', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isOhm (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isOhm();
}


/**
 * @return non-zero if the kind of this Unit is 'pascal', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isPascal (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isPascal();
}


/**
 * @return non-zero if the kind of this Unit is 'radian', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isRadian (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isRadian();
}


/**
 * @return non-zero if the kind of this Unit is 'second', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSecond (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isSecond();
}


/**
 * @return non-zero if the kind of this Unit is 'siemens', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSiemens (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isSiemens();
}


/**
 * @return non-zero if the kind of this Unit is 'sievert', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSievert (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isSievert();
}


/**
 * @return non-zero if the kind of this Unit is 'steradian', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSteradian (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isSteradian();
}


/**
 * @return non-zero if the kind of this Unit is 'tesla', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isTesla (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isTesla();
}


/**
 * @return non-zero if the kind of this Unit is 'volt', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isVolt (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isVolt();
}


/**
 * @return non-zero if the kind of this Unit is 'watt', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWatt (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isWatt();
}


/**
 * @return non-zero if the kind of this Unit is 'weber', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWeber (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isWeber();
}


/**
 * @return non-zero if the kind of this Unit has been set, zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSetKind (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isSetKind();
}


/**
 * Sets the kind of this Unit to the given UnitKind.
 */
LIBSBML_EXTERN
void
Unit_setKind (Unit_t *u, UnitKind_t kind)
{
  static_cast<Unit*>(u)->setKind(kind);
}


/**
 * Sets the exponent of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setExponent (Unit_t *u, int value)
{
  static_cast<Unit*>(u)->setExponent(value);
}


/**
 * Sets the scale of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setScale (Unit_t *u, int value)
{
  static_cast<Unit*>(u)->setScale(value);
}


/**
 * Sets the multiplier of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setMultiplier (Unit_t *u, double value)
{
  static_cast<Unit*>(u)->setMultiplier(value);
}


/**
 * Sets the offset of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setOffset (Unit_t *u, double value)
{
  static_cast<Unit*>(u)->setOffset(value);
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
