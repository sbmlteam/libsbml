/**
 * Filename    : Unit.hpp
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


#ifndef Unit_hpp
#define Unit_hpp


#include <string>

#include "extern.h"
#include "SBase.hpp"
#include "UnitKind.h"


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


#endif  // Unit_hpp
