/**
 * Filename    : Unit.cpp
 * Description : SBML Unit
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-22
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


#include "sbml/util.h"
#include "sbml/Unit.h"
#include "sbml/Unit.hpp"


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
 * @return 1 if the kind of this Unit is 'ampere', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isAmpere (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isAmpere();
}


/**
 * @return 1 if the kind of this Unit is 'becquerel', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isBecquerel (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isBecquerel();
}


/**
 * @return 1 if the kind of this Unit is 'candela', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCandela (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isCandela();
}


/**
 * @return 1 if the kind of this Unit is 'Celsius', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCelsius (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isCelsius();
}


/**
 * @return 1 if the kind of this Unit is 'coulomb', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCoulomb (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isCoulomb();
}


/**
 * @return 1 if the kind of this Unit is 'dimensionless', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isDimensionless (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isDimensionless();
}


/**
 * @return 1 if the kind of this Unit is 'farad', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isFarad (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isFarad();
}


/**
 * @return 1 if the kind of this Unit is 'gram', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGram (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isGram();
}


/**
 * @return 1 if the kind of this Unit is 'gray', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGray (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isGray();
}


/**
 * @return 1 if the kind of this Unit is 'henry', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHenry (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isHenry();
}


/**
 * @return 1 if the kind of this Unit is 'hertz', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHertz (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isHertz();
}


/**
 * @return 1 if the kind of this Unit is 'item', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isItem (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isItem();
}


/**
 * @return 1 if the kind of this Unit is 'joule', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isJoule (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isJoule();
}


/**
 * @return 1 if the kind of this Unit is 'katal', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKatal (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isKatal();
}


/**
 * @return 1 if the kind of this Unit is 'kelvin', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKelvin (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isKelvin();
}


/**
 * @return 1 if the kind of this Unit is 'kilogram', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKilogram (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isKilogram();
}


/**
 * @return 1 if the kind of this Unit is 'litre' or 'liter', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLitre (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isLitre();
}


/**
 * @return 1 if the kind of this Unit is 'lumen', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLumen (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isLumen();
}


/**
 * @return 1 if the kind of this Unit is 'lux', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLux (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isLux();
}


/**
 * @return 1 if the kind of this Unit is 'metre' or 'meter', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMetre (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isMetre();
}


/**
 * @return 1 if the kind of this Unit is 'mole', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMole (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isMole();
}


/**
 * @return 1 if the kind of this Unit is 'newton', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isNewton (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isNewton();
}


/**
 * @return 1 if the kind of this Unit is 'ohm', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isOhm (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isOhm();
}


/**
 * @return 1 if the kind of this Unit is 'pascal', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isPascal (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isPascal();
}


/**
 * @return 1 if the kind of this Unit is 'radian', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isRadian (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isRadian();
}


/**
 * @return 1 if the kind of this Unit is 'second', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSecond (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isSecond();
}


/**
 * @return 1 if the kind of this Unit is 'siemens', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSiemens (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isSiemens();
}


/**
 * @return 1 if the kind of this Unit is 'sievert', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSievert (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isSievert();
}


/**
 * @return 1 if the kind of this Unit is 'steradian', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSteradian (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isSteradian();
}


/**
 * @return 1 if the kind of this Unit is 'tesla', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isTesla (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isTesla();
}


/**
 * @return 1 if the kind of this Unit is 'volt', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isVolt (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isVolt();
}


/**
 * @return 1 if the kind of this Unit is 'watt', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWatt (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isWatt();
}


/**
 * @return 1 if the kind of this Unit is 'weber', 0 otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWeber (const Unit_t *u)
{
  return (int) static_cast<const Unit*>(u)->isWeber();
}


/**
 * @return 1 if the kind of this Unit has been set, 0 otherwise.
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
