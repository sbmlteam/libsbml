/**
 * \file    Unit.h
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


#ifndef Unit_h
#define Unit_h


#include "extern.h"
#include "UnitKind.h"


#ifdef __cplusplus


#include <string>
#include "SBase.h"


class SBMLVisitor;


class Unit : public SBase
{
public:

  /**
   * Creates a new Unit, optionally with its kind, exponent, scale,
   * multiplier, and offset attributes set.
   */
  LIBSBML_EXTERN
  Unit (   UnitKind_t  kind       = UNIT_KIND_INVALID
         , int         exponent   = 1
         , int         scale      = 0
         , double      multiplier = 1.0
         , double      offset     = 0.0 );

  /**
   * Creates a new Unit, optionally with its kind (via string), exponent,
   * scale, multiplier, and offset attributes set.
   */
  LIBSBML_EXTERN
  Unit (   const std::string&  kind
         , int                 exponent   = 1
         , int                 scale      = 0
         , double              multiplier = 1.0
         , double              offset     = 0.0 );

  /**
   * Destroys the given Unit.
   */
  LIBSBML_EXTERN
  virtual ~Unit ();

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the UnitDefinition's
   * next Unit (if available).
   */
  LIBSBML_EXTERN
  bool accept (SBMLVisitor& v) const;

  /**
   * Initializes the fields of this Unit to their defaults:
   *
   *   - exponent   = 1
   *   - scale      = 0
   *   - multiplier = 1.0
   *   - offset     = 0.0
   */
  LIBSBML_EXTERN
  void initDefaults ();

  /**
   * @return the kind of this Unit.
   */
  LIBSBML_EXTERN
  UnitKind_t getKind () const;

  /**
   * @return the exponent of this Unit.
   */
  LIBSBML_EXTERN
  int getExponent () const;

  /**
   * @return the scale of this Unit.
   */
  LIBSBML_EXTERN
  int getScale () const;

  /**
   * @return the multiplier of this Unit.
   */
  LIBSBML_EXTERN
  double getMultiplier () const;

  /**
   * @return the offset of this Unit.
   */
  LIBSBML_EXTERN
  double getOffset () const;


  /**
   * @return true if the kind of this Unit is 'ampere', false otherwise.
   */
  LIBSBML_EXTERN
  bool isAmpere () const;

  /**
   * @return true if the kind of this Unit is 'becquerel', false otherwise.
   */
  LIBSBML_EXTERN
  bool isBecquerel () const;

  /**
   * @return true if the kind of this Unit is 'candela', false otherwise.
   */
  LIBSBML_EXTERN
  bool isCandela () const;

  /**
   * @return true if the kind of this Unit is 'Celsius', false otherwise.
   */
  LIBSBML_EXTERN
  bool isCelsius () const;

  /**
   * @return true if the kind of this Unit is 'coulomb', false otherwise.
   */
  LIBSBML_EXTERN
  bool isCoulomb () const;

  /**
   * @return true if the kind of this Unit is 'dimensionless', false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isDimensionless () const;

  /**
   * @return true if the kind of this Unit is 'farad', false otherwise.
   */
  LIBSBML_EXTERN
  bool isFarad () const;

  /**
   * @return true if the kind of this Unit is 'gram', false otherwise.
   */
  LIBSBML_EXTERN
  bool isGram () const;

  /**
   * @return true if the kind of this Unit is 'gray', false otherwise.
   */
  LIBSBML_EXTERN
  bool isGray () const;

  /**
   * @return true if the kind of this Unit is 'henry', false otherwise.
   */
  LIBSBML_EXTERN
  bool isHenry () const;

  /**
   * @return true if the kind of this Unit is 'hertz', false otherwise.
   */
  LIBSBML_EXTERN
  bool isHertz () const;

  /**
   * @return true if the kind of this Unit is 'item', false otherwise.
   */
  LIBSBML_EXTERN
  bool isItem () const;

  /**
   * @return true if the kind of this Unit is 'joule', false otherwise.
   */
  LIBSBML_EXTERN
  bool isJoule () const;

  /**
   * @return true if the kind of this Unit is 'katal', false otherwise.
   */
  LIBSBML_EXTERN
  bool isKatal () const;

  /**
   * @return true if the kind of this Unit is 'kelvin', false otherwise.
   */
  LIBSBML_EXTERN
  bool isKelvin () const;

  /**
   * @return true if the kind of this Unit is 'kilogram', false otherwise.
   */
  LIBSBML_EXTERN
  bool isKilogram () const;

  /**
   * @return true if the kind of this Unit is 'litre' or 'liter', false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isLitre () const;

  /**
   * @return true if the kind of this Unit is 'lumen', false otherwise.
   */
  LIBSBML_EXTERN
  bool isLumen () const;

  /**
   * @return true if the kind of this Unit is 'lux', false otherwise.
   */
  LIBSBML_EXTERN
  bool isLux () const;

  /**
   * @return true if the kind of this Unit is 'metre' or 'meter', false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isMetre () const;

  /**
   * @return true if the kind of this Unit is 'mole', false otherwise.
   */
  LIBSBML_EXTERN
  bool isMole () const;

  /**
   * @return true if the kind of this Unit is 'newton', false otherwise.
   */
  LIBSBML_EXTERN
  bool isNewton () const;

  /**
   * @return true if the kind of this Unit is 'ohm', false otherwise.
   */
  LIBSBML_EXTERN
  bool isOhm () const;

  /**
   * @return true if the kind of this Unit is 'pascal', false otherwise.
   */
  LIBSBML_EXTERN
  bool isPascal () const;

  /**
   * @return true if the kind of this Unit is 'radian', false otherwise.
   */
  LIBSBML_EXTERN
  bool isRadian () const;

  /**
   * @return true if the kind of this Unit is 'second', false otherwise.
   */
  LIBSBML_EXTERN
  bool isSecond () const;

  /**
   * @return true if the kind of this Unit is 'siemens', false otherwise.
   */
  LIBSBML_EXTERN
  bool isSiemens () const;

  /**
   * @return true if the kind of this Unit is 'sievert', false otherwise.
   */
  LIBSBML_EXTERN
  bool isSievert () const;

  /**
   * @return true if the kind of this Unit is 'steradian', false otherwise.
   */
  LIBSBML_EXTERN
  bool isSteradian () const;

  /**
   * @return true if the kind of this Unit is 'tesla', false otherwise.
   */
  LIBSBML_EXTERN
  bool isTesla () const;

  /**
   * @return true if the kind of this Unit is 'volt', false otherwise.
   */
  LIBSBML_EXTERN
  bool isVolt () const;

  /**
   * @return true if the kind of this Unit is 'watt', false otherwise.
   */
  LIBSBML_EXTERN
  bool isWatt () const;

  /**
   * @return true if the kind of this Unit is 'weber', false otherwise.
   */
  LIBSBML_EXTERN
  bool isWeber () const;


  /**
   * @return true if the kind of this Unit has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetKind () const;

  /**
   * Sets the kind of this Unit to the given UnitKind.
   */
  LIBSBML_EXTERN
  void setKind (UnitKind_t kind);

  /**
   * Sets the exponent of this Unit to the given value.
   */
  LIBSBML_EXTERN
  void setExponent (int value);

  /**
   * Sets the scale of this Unit to the given value.
   */
  LIBSBML_EXTERN
  void setScale (int value);

  /**
   * Sets the multiplier of this Unit to the given value.
   */
  LIBSBML_EXTERN
  void setMultiplier (double value);

  /**
   * Sets the offset of this Unit to the given value.
   */
  LIBSBML_EXTERN
  void setOffset (double value);


  /**
   * @return true if name is one of the five SBML builtin Unit names
   * ('substance', 'volume', 'area', 'length' or 'time'), false otherwise.
   */
  LIBSBML_EXTERN
  static bool isBuiltIn (const std::string& name);

  /**
   * @return true if name is a valid UnitKind.
   */
  LIBSBML_EXTERN
  static bool isUnitKind (const std::string& name);


protected:

  UnitKind_t kind;
  int        exponent;
  int        scale;
  double     multiplier;
  double     offset;  


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "sbmlfwd.h"


/**
 * Creates a new Unit and returns a pointer to it.
 */
LIBSBML_EXTERN
Unit_t *
Unit_create (void);

/**
 * Creates a new Unit with the given kind, exponent and scale and returns a
 * pointer to it.  This convenience function is functionally equivalent to:
 *
 *   Unit_t *u = Unit_create();
 *   Unit_setKind(kind); Unit_setExponent(exponent); ...;
 */
LIBSBML_EXTERN
Unit_t *
Unit_createWith (UnitKind_t kind, int exponent, int scale);

/**
 * Frees the given Unit.
 */
LIBSBML_EXTERN
void
Unit_free (Unit_t *u);

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
Unit_initDefaults (Unit_t *u);


/**
 * @return the kind of this Unit.
 */
LIBSBML_EXTERN
UnitKind_t
Unit_getKind (const Unit_t *u);

/**
 * @return the exponent of this Unit.
 */
LIBSBML_EXTERN
int
Unit_getExponent (const Unit_t *u);

/**
 * @return the scale of this Unit.
 */
LIBSBML_EXTERN
int
Unit_getScale (const Unit_t *u);

/**
 * @return the multiplier of this Unit.
 */
LIBSBML_EXTERN
double
Unit_getMultiplier (const Unit_t *u);

/**
 * @return the offset of this Unit.
 */
LIBSBML_EXTERN
double
Unit_getOffset (const Unit_t *u);


/**
 * @return non-zero if the kind of this Unit is 'ampere', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isAmpere (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'becquerel', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isBecquerel (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'candela', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCandela (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'Celsius', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCelsius (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'coulomb', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCoulomb (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'dimensionless', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isDimensionless (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'farad', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isFarad (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'gram', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGram (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'gray', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGray (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'henry', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHenry (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'hertz', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHertz (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'item', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isItem (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'joule', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isJoule (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'katal', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKatal (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'kelvin', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKelvin (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'kilogram', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKilogram (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'litre' or 'liter', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLitre (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'lumen', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLumen (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'lux', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLux (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'metre' or 'meter', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMetre (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'mole', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMole (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'newton', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isNewton (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'ohm', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isOhm (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'pascal', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isPascal (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'radian', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isRadian (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'second', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSecond (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'siemens', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSiemens (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'sievert', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSievert (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'steradian', zero
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSteradian (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'tesla', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isTesla (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'volt', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isVolt (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'watt', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWatt (const Unit_t *u);

/**
 * @return non-zero if the kind of this Unit is 'weber', zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWeber (const Unit_t *u);


/**
 * @return non-zero if the kind of this Unit has been set, zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSetKind (const Unit_t *u);


/**
 * Sets the kind of this Unit to the given UnitKind.
 */
LIBSBML_EXTERN
void
Unit_setKind (Unit_t *u, UnitKind_t kind);

/**
 * Sets the exponent of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setExponent (Unit_t *u, int value);

/**
 * Sets the scale of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setScale (Unit_t *u, int value);

/**
 * Sets the multiplier of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setMultiplier (Unit_t *u, double value);

/**
 * Sets the offset of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setOffset (Unit_t *u, double value);

/**
 * @return non-zero if name is one of the five SBML builtin Unit names
 * ('substance', 'volume', 'area', 'length' or 'time'), zero otherwise.
 */
LIBSBML_EXTERN
int
Unit_isBuiltIn (const char *name);


END_C_DECLS


#endif  /* !SWIG  */
#endif  /* Unit_h */
