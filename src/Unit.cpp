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
 * @return true if the kind of this Unit has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Unit::isSetKind () const
{
  return (kind != UNIT_KIND_INVALID);
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
