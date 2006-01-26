/**
 * \file    Utils_Unit.cpp
 * \brief   Functions acting on a unit
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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
 *     Sarah Keating
 *
 *     The SBML Team
 *     Scince and Technology Research Institute
 *     University of Hertfordshire
 *     Hatfield, UK
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "Utils_Unit.h"


/** 
  * alters the multiplier so that scale = 0
  * eg 1 mm can be expressed as multipier = 1 scale = -3
  * or as multiplier = 10^-3 scale = 0
  * leaves exponent alone
  */
void removeScale(Unit * unit)
{
  double scaleFactor = pow(10.0, unit->getScale());
  double newMultiplier = unit->getMultiplier() * scaleFactor;
  unit->setMultiplier(newMultiplier);
  unit->setScale(0);
}


/** 
  * returns a unit which is the product of the first unit by the second
  * this function applies both units are of the same kind
  */
void
mergeUnits(Unit * unit1, Unit * unit2)
{
  int newExponent;
  double newMultiplier;

  /* only applies if units have same kind */
  if (strcmp(UnitKind_toString(unit1->getKind()), UnitKind_toString(unit2->getKind())))
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
      pow(unit2->getMultiplier(), unit2->getExponent()), 1/(double)(newExponent));
  }
    
  unit1->setScale(0);
  unit1->setExponent(newExponent);
  unit1->setMultiplier(newMultiplier);
}
