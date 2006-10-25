/**
 * \file    TestUtilsUnitDefinition.c
 * \brief   Utilities on unitdefinitions unit tests (no pun intended)
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


#include <check.h>
#include "sbml/common/common.h"

#include "sbml/Unit.h"
#include "sbml/UnitDefinition.h"
#include "sbml/math/ASTNode.h"

#include "../Utils_UnitDefinition.h"

BEGIN_C_DECLS

START_TEST(test_unitdefinition_simplify)
{
  UnitDefinition * ud = new UnitDefinition();
  Unit * u = new Unit(UNIT_KIND_METRE);
  Unit * u1 = new Unit(UNIT_KIND_DIMENSIONLESS);
  Unit * u2 = new Unit(UNIT_KIND_METRE, 2);

  /* case with only one unit */
  ud->addUnit(u);

  simplifyUnitDefinition(ud);

  fail_unless(ud->getNumUnits() == 1);
  fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_METRE);

  /* case with one unit plus a dimensionless unit */
  ud->addUnit(u1);

  simplifyUnitDefinition(ud);
  
  fail_unless(ud->getNumUnits() == 1);
  fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_METRE);

  /* case with two units of same kind */
  ud->addUnit(u2);

  simplifyUnitDefinition(ud);
  
  fail_unless(ud->getNumUnits() == 1);
  fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_METRE);
  fail_unless(ud->getUnit(0)->getExponent() == 3);

  /* case with two units of same kind that cancel*/
  u2->setExponent(-3);
  ud->addUnit(u2);

  simplifyUnitDefinition(ud);
  
  fail_unless(ud->getNumUnits() == 0);

  delete u;
  delete u1;
  delete u2;
  delete ud;
 }
END_TEST

START_TEST(test_unitdefinition_order)
{
  UnitDefinition * ud = new UnitDefinition();
  Unit * u = new Unit(UNIT_KIND_METRE);
  Unit * u1 = new Unit(UNIT_KIND_AMPERE);
  Unit * u2 = new Unit(UNIT_KIND_HERTZ);

  ud->addUnit(u);
  ud->addUnit(u1);
  ud->addUnit(u2);

  orderUnitDefinition(ud);

  fail_unless(ud->getNumUnits() == 3);
  fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);
  fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_HERTZ);
  fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_METRE);

  delete ud;
}
END_TEST

START_TEST(test_unitdefinition_convert_SI)
{
  UnitDefinition * ud = new UnitDefinition();
  UnitDefinition * ud1 = new UnitDefinition();
  Unit * u = new Unit(UNIT_KIND_JOULE);
  Unit * u1 = new Unit(UNIT_KIND_NEWTON);
  u1->setExponent(-1);

  ud->addUnit(u);
  ud->addUnit(u1);

  ud1 = convertToSI(ud);

  fail_unless(ud1->getNumUnits() == 1);
  fail_unless(ud1->getUnit(0)->getKind() == UNIT_KIND_METRE);

  
  delete ud;
  delete ud1;
}
END_TEST

START_TEST(test_unitdefinition_areIdentical)
{
  UnitDefinition * ud = new UnitDefinition();
  UnitDefinition * ud1 = new UnitDefinition();
  Unit * u = new Unit(UNIT_KIND_JOULE);
  Unit * u1 = new Unit(UNIT_KIND_NEWTON);
  Unit * u2 = new Unit(UNIT_KIND_METRE);
  
  ud->addUnit(u);
  ud->addUnit(u1);

  ud1->addUnit(u);
  ud1->addUnit(u1);

  int identical = areIdentical(ud, ud1);

  fail_unless(identical == 1);

  ud->addUnit(u2);

  identical = areIdentical(ud, ud1);
 
  fail_unless(identical == 0);

  delete ud; 

}
END_TEST

START_TEST(test_unitdefinition_areEquivalent)
{
  UnitDefinition * ud = new UnitDefinition();
  UnitDefinition * ud1 = new UnitDefinition();
  Unit * u = new Unit(UNIT_KIND_JOULE);
  Unit * u1 = new Unit(UNIT_KIND_NEWTON);
  Unit * u2 = new Unit(UNIT_KIND_METRE);
  u1->setExponent(-1);

  ud->addUnit(u);
  ud->addUnit(u1);

  ud1->addUnit(u2);

  int equivalent = areEquivalent(ud, ud1);

  fail_unless(equivalent == 1);

  ud->addUnit(u2);

  equivalent = areEquivalent(ud, ud1);
 
  fail_unless(equivalent == 0);

  delete ud;
  delete ud1;
}
END_TEST


Suite *
create_suite_UtilsUnitDefinition (void) 
{ 
  Suite *suite = suite_create("UtilsUnitDefinition");
  TCase *tcase = tcase_create("UtilsUnitDefinition");
 

  tcase_add_test( tcase, test_unitdefinition_simplify     );
  tcase_add_test( tcase, test_unitdefinition_order      );
  tcase_add_test( tcase, test_unitdefinition_convert_SI       );
  tcase_add_test( tcase, test_unitdefinition_areIdentical     );
  tcase_add_test( tcase, test_unitdefinition_areEquivalent    );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
