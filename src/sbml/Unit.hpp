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
