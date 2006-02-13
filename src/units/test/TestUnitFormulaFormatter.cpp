/**
 * \file    TestUnitFormulaFormatter.cpp
 * \brief   UnitFormulaFormatter unit tests
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


#include <check.h>

#include "common/common.h"
#include "common/extern.h"

#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"

#include "sbml/SBMLDocument.h"
#include "sbml/Model.h"

#include "UnitFormulaFormatter.h"

extern char *TestDataDirectory;

static UnitFormulaFormatter *uff;
static Model *m;


BEGIN_C_DECLS


void
UnitFormulaFormatterTest_setup (void)
{
  SBMLDocument     *d = new SBMLDocument();
 
  char *filename = safe_strcat(TestDataDirectory, "rules.xml");


  d = readSBML(filename);
  m = d->getModel();

  uff = new UnitFormulaFormatter(m);

}


void
UnitFormulaFormatterTest_teardown (void)
{
  delete uff;
}

/* put in a test for each possible type of ASTNode
   this will facilitate the transition to ucar library if necessary */
START_TEST (test_UnitFormulaFormatter_getUnitDefinition_unknown)
{
  ASTNode * node = new ASTNode();
  UnitDefinition * ud = new UnitDefinition();

  ud = uff->getUnitDefinition(node);

  fail_unless(ud->getNumUnits() == 0);

  fail_unless(!strcmp(ud->getId().c_str(), "unknown"), NULL);

}
END_TEST

START_TEST (test_UnitFormulaFormatter_getUnitDefinition_boolean)
{
  UnitDefinition * ud = new UnitDefinition();

  ud = uff->getUnitDefinition(m->getRule(1)->getMath());

  fail_unless(ud->getNumUnits() == 1);

  fail_unless(!strcmp(ud->getId().c_str(), "dimless_ret_func"), NULL);

  fail_unless(ud->getUnit(0)->getMultiplier() == 1);
  fail_unless(ud->getUnit(0)->getScale() == 0);
  fail_unless(ud->getUnit(0)->getExponent() == 1);
  fail_unless(ud->getUnit(0)->getOffset() == 0.0);
  fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);

}
END_TEST

START_TEST (test_UnitFormulaFormatter_getUnitDefinition_dimensionless)
{
  UnitDefinition * ud = new UnitDefinition();

  ud = uff->getUnitDefinition(m->getRule(2)->getMath());

  fail_unless(ud->getNumUnits() == 1);

  fail_unless(!strcmp(ud->getId().c_str(), "dimless_ret_func"), NULL);

  fail_unless(ud->getUnit(0)->getMultiplier() == 1);
  fail_unless(ud->getUnit(0)->getScale() == 0);
  fail_unless(ud->getUnit(0)->getExponent() == 1);
  fail_unless(ud->getUnit(0)->getOffset() == 0.0);
  fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);

}
END_TEST

START_TEST (test_UnitFormulaFormatter_getUnitDefinition_invtrig)
{
  UnitDefinition * ud = new UnitDefinition();

  ud = uff->getUnitDefinition(m->getRule(3)->getMath());

  fail_unless(ud->getNumUnits() == 1);

  fail_unless(!strcmp(ud->getId().c_str(), "dimless_ret_func"), NULL);

  fail_unless(ud->getUnit(0)->getMultiplier() == 1);
  fail_unless(ud->getUnit(0)->getScale() == 0);
  fail_unless(ud->getUnit(0)->getExponent() == 1);
  fail_unless(ud->getUnit(0)->getOffset() == 0.0);
  fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);

}
END_TEST

START_TEST (test_UnitFormulaFormatter_getUnitDefinition_plus)
{
  UnitDefinition * ud = new UnitDefinition();

  ud = uff->getUnitDefinition(m->getRule(4)->getMath());

  fail_unless(ud->getNumUnits() == 1);

  fail_unless(!strcmp(ud->getId().c_str(), "species_subs"), NULL);

  fail_unless(ud->getUnit(0)->getMultiplier() == 1);
  fail_unless(ud->getUnit(0)->getScale() == 0);
  fail_unless(ud->getUnit(0)->getExponent() == 1);
  fail_unless(ud->getUnit(0)->getOffset() == 0.0);
  fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

}
END_TEST

Suite *
create_suite_UnitFormulaFormatter (void)
{
  Suite *suite = suite_create("UnitFormulaFormatter");
  TCase *tcase = tcase_create("UnitFormulaFormatter");

  tcase_add_checked_fixture(tcase,
                            UnitFormulaFormatterTest_setup,
                            UnitFormulaFormatterTest_teardown);

  tcase_add_test(tcase, test_UnitFormulaFormatter_getUnitDefinition_unknown );
  tcase_add_test(tcase, test_UnitFormulaFormatter_getUnitDefinition_boolean );
  tcase_add_test(tcase, test_UnitFormulaFormatter_getUnitDefinition_dimensionless );
  tcase_add_test(tcase, test_UnitFormulaFormatter_getUnitDefinition_invtrig );
  tcase_add_test(tcase, test_UnitFormulaFormatter_getUnitDefinition_plus );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
