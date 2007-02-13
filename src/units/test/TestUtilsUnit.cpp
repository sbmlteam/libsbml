/**
 * \file    TestUtilsUnit.c
 * \brief   Utilities on units unit tests (no pun intended)
 * \author  Sarah Keating and Ralph Gauges
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


#include "sbml/common/common.h"

#include "sbml/Unit.h"
#include "sbml/UnitDefinition.h"
#include "sbml/math/ASTNode.h"

#include "../Utils_Unit.h"

#include <check.h>

BEGIN_C_DECLS

START_TEST(test_unit_remove_scale)
{
    Unit * u = new Unit(UNIT_KIND_LITRE, 1, -3);
    
    removeScale(u);

    fail_unless(u->getMultiplier() == 0.001);
    fail_unless(u->getScale() == 0);
    fail_unless(u->getExponent() == 1);
    fail_unless(u->getOffset() == 0.0);
    fail_unless(u->getKind() == UNIT_KIND_LITRE);

    delete u; 
}
END_TEST

START_TEST(test_unit_merge_units)
{
    Unit * u = new Unit(UNIT_KIND_LITRE, 1, -3, 2);
    Unit * u1 = new Unit(UNIT_KIND_LITRE, 2, 0, 2); 
    
    mergeUnits(u, u1);

    fail_unless(u->getMultiplier() == 0.2);
    fail_unless(u->getScale() == 0);
    fail_unless(u->getExponent() == 3);
    fail_unless(u->getOffset() == 0.0);
    fail_unless(u->getKind() == UNIT_KIND_LITRE);

    fail_unless(u1->getMultiplier() == 2);
    fail_unless(u1->getScale() == 0);
    fail_unless(u1->getExponent() == 2);
    fail_unless(u1->getOffset() == 0.0);
    fail_unless(u1->getKind() == UNIT_KIND_LITRE);

    delete u; 
    delete u1;
}
END_TEST

START_TEST(test_unit_convert_SI)
{
    UnitDefinition * ud;

    /* Ampere */
    Unit * u = new Unit(UNIT_KIND_AMPERE, 1, -3, 2);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.002);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);

    /* becquerel */
    /* 1 becquerel = 1 sec^-1 = (0.1 sec)^-1 */
    u->setKind(UNIT_KIND_BECQUEREL);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.5);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == -1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_SECOND);

    /* candela */
    u->setKind(UNIT_KIND_CANDELA);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_CANDELA);

    /* Celsius 
    u->setKind(UNIT_KIND_CELSIUS);
    u->setMultiplier(1);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 1);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 273.15);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_KELVIN);
    */

    /* coulomb */
    /* 1 coulomb = 1 Ampere second */
    u->setKind(UNIT_KIND_COULOMB);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 2);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == 1);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_SECOND);

    /* dimensionless */
    u->setKind(UNIT_KIND_DIMENSIONLESS);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);
    
    /* farad */
    /* 1 Farad = 1 m^-2 kg^-1 s^4 A^2 */
    u->setKind(UNIT_KIND_FARAD);
    u->setMultiplier(1);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 4);

    fail_unless(ud->getUnit(0)->getMultiplier() == 1);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 2);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == -1);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_KILOGRAM);

    fail_unless(ud->getUnit(2)->getMultiplier() == 1);
    fail_unless(ud->getUnit(2)->getScale() == 0);
    fail_unless(ud->getUnit(2)->getExponent() == -2);
    fail_unless(ud->getUnit(2)->getOffset() == 0.0);
    fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(3)->getMultiplier() == 1);
    fail_unless(ud->getUnit(3)->getScale() == 0);
    fail_unless(ud->getUnit(3)->getExponent() == 4);
    fail_unless(ud->getUnit(3)->getOffset() == 0.0);
    fail_unless(ud->getUnit(3)->getKind() == UNIT_KIND_SECOND);

    /* gram */
    /* 1 gram = 0.001 Kg */
    u->setKind(UNIT_KIND_GRAM);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.002);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_KILOGRAM);

    /* gray */
    /* 1 Gray = 1 m^2 sec^-2 */
    u->setKind(UNIT_KIND_GRAY);
    u->setMultiplier(1);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 2);

    fail_unless(ud->getUnit(0)->getMultiplier() == 1);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 2);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == -2);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_SECOND);

    /* henry */
    /* 1 Henry = 1 m^2 kg s^-2 A^-2 */
    u->setKind(UNIT_KIND_HENRY);
    u->setMultiplier(4);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 4);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.5);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == -2);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == 1);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_KILOGRAM);

    fail_unless(ud->getUnit(2)->getMultiplier() == 1);
    fail_unless(ud->getUnit(2)->getScale() == 0);
    fail_unless(ud->getUnit(2)->getExponent() == 2);
    fail_unless(ud->getUnit(2)->getOffset() == 0.0);
    fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(3)->getMultiplier() == 1);
    fail_unless(ud->getUnit(3)->getScale() == 0);
    fail_unless(ud->getUnit(3)->getExponent() == -2);
    fail_unless(ud->getUnit(3)->getOffset() == 0.0);
    fail_unless(ud->getUnit(3)->getKind() == UNIT_KIND_SECOND);

    /* hertz */
    /* 1 hertz = 1 sec^-1 = (0.1 sec)^-1 */
    u->setKind(UNIT_KIND_HERTZ);
    u->setMultiplier(1);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 1);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == -1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_SECOND);

    /* item */
    u->setKind(UNIT_KIND_ITEM);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);
    
    /* joule */
    /* 1 joule = 1 m^2 kg s^-2 */
    u->setKind(UNIT_KIND_JOULE);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 3);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2.0);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_KILOGRAM);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == 2);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(2)->getMultiplier() == 1);
    fail_unless(ud->getUnit(2)->getScale() == 0);
    fail_unless(ud->getUnit(2)->getExponent() == -2);
    fail_unless(ud->getUnit(2)->getOffset() == 0.0);
    fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_SECOND);

    /* katal */
    /* 1 katal = 1 mol s^-1 */
    u->setKind(UNIT_KIND_KATAL);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 2);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2.0);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == -1);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_SECOND);
 
    /* kelvin */
    u->setKind(UNIT_KIND_KELVIN);
    u->setMultiplier(2);
    u->setScale(-3);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.002);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_KELVIN);

    /* kilogram */
    u->setKind(UNIT_KIND_KILOGRAM);
    u->setMultiplier(2);
    u->setScale(-3);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.002);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_KILOGRAM);

    /* litre */
    /* 1 litre = 0.001 m^3 = (0.1 m)^3*/ 
    u->setKind(UNIT_KIND_LITRE);
    u->setMultiplier(8);
    u->setScale(-3);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == pow(0.000008, (1.0/3.0)));
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 3);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_METRE);

     /* litre */
    /* 1 litre = 0.001 m^3 = (0.1 m)^3*/ 
    u->setKind(UNIT_KIND_LITER);
    u->setMultiplier(8);
    u->setScale(-3);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == pow(0.000008, (1.0/3.0)));
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 3);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_METRE);

    /* lumen */
    /* 1 lumen = 1 candela*/ 
    u->setKind(UNIT_KIND_LUMEN);
    u->setMultiplier(2);
    u->setScale(-3);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.002);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_CANDELA);

    /* lux */
    /* 1 lux = 1 candela m^-2*/ 
    u->setKind(UNIT_KIND_LUX);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 2);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2.0);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_CANDELA);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == -2);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_METRE);
 
    /* metre */
    u->setKind(UNIT_KIND_METRE);
    u->setMultiplier(2);
    u->setScale(-3);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.002);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_METRE);

    /* meter */
    u->setKind(UNIT_KIND_METER);
    u->setMultiplier(2);
    u->setScale(-3);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.002);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_METRE);

    /* mole */
    u->setKind(UNIT_KIND_MOLE);
    u->setMultiplier(2);
    u->setScale(-3);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.002);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

    /* newton */
    /* 1 newton = 1 m kg s^-2 */
    u->setKind(UNIT_KIND_NEWTON);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 3);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2.0);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_KILOGRAM);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == 1);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(2)->getMultiplier() == 1);
    fail_unless(ud->getUnit(2)->getScale() == 0);
    fail_unless(ud->getUnit(2)->getExponent() == -2);
    fail_unless(ud->getUnit(2)->getOffset() == 0.0);
    fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_SECOND);

    /* ohm */
    /* 1 ohm = 1 m^2 kg s^-3 A^-2 */
    u->setKind(UNIT_KIND_OHM);
    u->setMultiplier(4);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 4);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.5);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == -2);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == 1);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_KILOGRAM);

    fail_unless(ud->getUnit(2)->getMultiplier() == 1);
    fail_unless(ud->getUnit(2)->getScale() == 0);
    fail_unless(ud->getUnit(2)->getExponent() == 2);
    fail_unless(ud->getUnit(2)->getOffset() == 0.0);
    fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(3)->getMultiplier() == 1);
    fail_unless(ud->getUnit(3)->getScale() == 0);
    fail_unless(ud->getUnit(3)->getExponent() == -3);
    fail_unless(ud->getUnit(3)->getOffset() == 0.0);
    fail_unless(ud->getUnit(3)->getKind() == UNIT_KIND_SECOND);

    /* pascal */
    /* 1 pascal = 1 m^-1 kg s^-2 */
    u->setKind(UNIT_KIND_PASCAL);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 3);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2.0);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_KILOGRAM);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == -1);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(2)->getMultiplier() == 1);
    fail_unless(ud->getUnit(2)->getScale() == 0);
    fail_unless(ud->getUnit(2)->getExponent() == -2);
    fail_unless(ud->getUnit(2)->getOffset() == 0.0);
    fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_SECOND);

    /* radian */
    u->setKind(UNIT_KIND_RADIAN);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);

    /* second */
    u->setKind(UNIT_KIND_SECOND);
    u->setMultiplier(2);
    u->setScale(-3);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.002);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_SECOND);

    /* siemens */
    /* 1 siemen = 1 m^-2 kg^-1 s^3 A^2 */
    u->setKind(UNIT_KIND_SIEMENS);
    u->setMultiplier(1);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 4);

    fail_unless(ud->getUnit(0)->getMultiplier() == 1);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 2);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == -1);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_KILOGRAM);

    fail_unless(ud->getUnit(2)->getMultiplier() == 1);
    fail_unless(ud->getUnit(2)->getScale() == 0);
    fail_unless(ud->getUnit(2)->getExponent() == -2);
    fail_unless(ud->getUnit(2)->getOffset() == 0.0);
    fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(3)->getMultiplier() == 1);
    fail_unless(ud->getUnit(3)->getScale() == 0);
    fail_unless(ud->getUnit(3)->getExponent() == 3);
    fail_unless(ud->getUnit(3)->getOffset() == 0.0);
    fail_unless(ud->getUnit(3)->getKind() == UNIT_KIND_SECOND);

    /* sievert */
    /* 1 Sievert = 1 m^2 sec^-2 */
    u->setKind(UNIT_KIND_SIEVERT);
    u->setMultiplier(1);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 2);

    fail_unless(ud->getUnit(0)->getMultiplier() == 1);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 2);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == -2);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_SECOND);
 
    /* steradian */
    u->setKind(UNIT_KIND_STERADIAN);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 1);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);

    /* tesla */
    /* 1 tesla = 1 kg s^-2 A^-1 */
    u->setKind(UNIT_KIND_TESLA);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 3);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.5);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == -1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == 1);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_KILOGRAM);

    fail_unless(ud->getUnit(2)->getMultiplier() == 1);
    fail_unless(ud->getUnit(2)->getScale() == 0);
    fail_unless(ud->getUnit(2)->getExponent() == -2);
    fail_unless(ud->getUnit(2)->getOffset() == 0.0);
    fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_SECOND);

    /* volt */
    /* 1 volt = 1 m^2 kg s^-3 A^-1 */
    u->setKind(UNIT_KIND_VOLT);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 4);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.5);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == -1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == 1);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_KILOGRAM);

    fail_unless(ud->getUnit(2)->getMultiplier() == 1);
    fail_unless(ud->getUnit(2)->getScale() == 0);
    fail_unless(ud->getUnit(2)->getExponent() == 2);
    fail_unless(ud->getUnit(2)->getOffset() == 0.0);
    fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(3)->getMultiplier() == 1);
    fail_unless(ud->getUnit(3)->getScale() == 0);
    fail_unless(ud->getUnit(3)->getExponent() == -3);
    fail_unless(ud->getUnit(3)->getOffset() == 0.0);
    fail_unless(ud->getUnit(3)->getKind() == UNIT_KIND_SECOND);

    /* watt */
    /* 1 watt = 1 m^2 kg s^-3 */
    u->setKind(UNIT_KIND_WATT);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 3);

    fail_unless(ud->getUnit(0)->getMultiplier() == 2.0);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == 1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_KILOGRAM);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == 2);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(2)->getMultiplier() == 1);
    fail_unless(ud->getUnit(2)->getScale() == 0);
    fail_unless(ud->getUnit(2)->getExponent() == -3);
    fail_unless(ud->getUnit(2)->getOffset() == 0.0);
    fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_SECOND);

    /* weber */
    /* 1 weber = 1 m^2 kg s^-2 A^-1 */
    u->setKind(UNIT_KIND_WEBER);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    ud = convertUnitToSI(u);

    fail_unless(ud->getNumUnits() == 4);

    fail_unless(ud->getUnit(0)->getMultiplier() == 0.5);
    fail_unless(ud->getUnit(0)->getScale() == 0);
    fail_unless(ud->getUnit(0)->getExponent() == -1);
    fail_unless(ud->getUnit(0)->getOffset() == 0.0);
    fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);

    fail_unless(ud->getUnit(1)->getMultiplier() == 1);
    fail_unless(ud->getUnit(1)->getScale() == 0);
    fail_unless(ud->getUnit(1)->getExponent() == 1);
    fail_unless(ud->getUnit(1)->getOffset() == 0.0);
    fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_KILOGRAM);

    fail_unless(ud->getUnit(2)->getMultiplier() == 1);
    fail_unless(ud->getUnit(2)->getScale() == 0);
    fail_unless(ud->getUnit(2)->getExponent() == 2);
    fail_unless(ud->getUnit(2)->getOffset() == 0.0);
    fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_METRE);

    fail_unless(ud->getUnit(3)->getMultiplier() == 1);
    fail_unless(ud->getUnit(3)->getScale() == 0);
    fail_unless(ud->getUnit(3)->getExponent() == -2);
    fail_unless(ud->getUnit(3)->getOffset() == 0.0);
    fail_unless(ud->getUnit(3)->getKind() == UNIT_KIND_SECOND);



    delete u; 
    delete ud;
    
}
END_TEST

START_TEST(test_unit_areIdentical)
{
    Unit * u = new Unit(UNIT_KIND_LITRE, 1, -3);
    Unit * u1 = new Unit(UNIT_KIND_LITRE, 1, -3);
    
    int identical = areIdentical(u, u1);

    fail_unless(identical == 1);
    
    u->setKind(UNIT_KIND_KATAL);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    identical = areIdentical(u, u1);
    
    fail_unless(identical == 0);

    delete u; 
    delete u1;
}
END_TEST

START_TEST(test_unit_areEquivalent)
{
    Unit * u = new Unit(UNIT_KIND_LITRE, 1, 0);
    Unit * u1 = new Unit(UNIT_KIND_LITRE, 1, -3);
    
    int equivalent = areEquivalent(u, u1);

    fail_unless(equivalent == 1);

    u->setKind(UNIT_KIND_MOLE);
    u->setMultiplier(2);
    u->setScale(0);
    u->setExponent(1);
    u->setOffset(0.0);
    
    equivalent = areEquivalent(u, u1);
    
    fail_unless(equivalent == 0);

    delete u; 
    delete u1;
}
END_TEST


Suite *
create_suite_UtilsUnit (void) 
{ 
  Suite *suite = suite_create("UtilsUnit");
  TCase *tcase = tcase_create("UtilsUnit");
 

  tcase_add_test( tcase, test_unit_remove_scale     );
  tcase_add_test( tcase, test_unit_merge_units      );
  tcase_add_test( tcase, test_unit_convert_SI       );
  tcase_add_test( tcase, test_unit_areIdentical     );
  tcase_add_test( tcase, test_unit_areEquivalent    );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
