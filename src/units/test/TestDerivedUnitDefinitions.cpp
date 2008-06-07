/**
 * \file    TestDerivedUnitDefinitions.cpp
 * \brief   unit tests for the getDerivedUnitDefinition function
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <sbml/common/common.h>
#include <sbml/common/extern.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/SBMLTypeCodes.h>

#include <sbml/units/UnitFormulaFormatter.h>

#include <check.h>

extern char *TestDataDirectory;

static Model *m;
static SBMLDocument* d;

/* 
 * tests the results from different model
 * components that have units
 * e.g. compartment; species; parameter
 */


void
DerivedUnitDefinition_setup (void)
{
  d = new SBMLDocument();
 
  char *filename = safe_strcat(TestDataDirectory, "formula.xml");


  d = readSBML(filename);
  m = d->getModel();

}


void
DerivedUnitDefinition_teardown (void)
{
  delete d;
}
CK_CPPSTART

START_TEST (test_DerivedUnitDefinition_compartment)
{
  const UnitDefinition *fud = m->getCompartment(0)
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 1);
  
  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_LITRE);


  fud = m->getCompartment(1)->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 1);
  
  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == -2);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_METRE);
}
END_TEST

START_TEST (test_DerivedUnitDefinition_species)
{
  UnitDefinition *fud = m->getSpecies(0)->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnit(1)->getKind() == UNIT_KIND_LITRE);

  fud = m->getSpecies(1)->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == -2);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fud = m->getSpecies(2)->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == -2);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnit(1)->getKind() == UNIT_KIND_LITRE);

}
END_TEST

START_TEST (test_DerivedUnitDefinition_parameter)
{
  UnitDefinition *fud = m->getParameter(0)->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_METRE);

  fail_unless(fud->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fud = m->getParameter(1)->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

}
END_TEST

START_TEST (test_DerivedUnitDefinition_initialassignment)
{
  UnitDefinition *fud = m->getInitialAssignment(0)->getDerivedUnitDefinition();
  bool undecl = m->getInitialAssignment(0)->containsUndeclaredUnits();

  fail_unless(undecl == 1);

  fail_unless(fud->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == -2);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

}
END_TEST

START_TEST (test_DerivedUnitDefinition_rule)
{
  UnitDefinition *fud = m->getRule(0)->getDerivedUnitDefinition();
  bool undecl = m->getRule(0)->containsUndeclaredUnits();

  fail_unless(undecl == 1);

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);


  fud = m->getRule(1)->getDerivedUnitDefinition();
  undecl = m->getRule(1)->containsUndeclaredUnits();

  fail_unless(undecl == 1);

  fail_unless(fud->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_METRE);

  fail_unless(fud->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fud = m->getRule(2)->getDerivedUnitDefinition();
  undecl = m->getRule(2)->containsUndeclaredUnits();

  fail_unless(undecl == 1);

  fail_unless(fud->getNumUnits() == 1);
  
  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_LITRE);
}
END_TEST

START_TEST (test_DerivedUnitDefinition_reaction)
{
  UnitDefinition *fud = m->getReaction(0)->getKineticLaw()->getDerivedUnitDefinition();
  bool undecl = m->getReaction(0)->getKineticLaw()->containsUndeclaredUnits();

  fail_unless(undecl == 0);

  fail_unless(fud->getNumUnits() == 3);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnit(1)->getKind() == UNIT_KIND_LITRE);

  fail_unless(fud->getUnit(2)->getMultiplier() == 1);
  fail_unless(fud->getUnit(2)->getScale() == 0);
  fail_unless(fud->getUnit(2)->getExponent() == -1);
  fail_unless(fud->getUnit(2)->getOffset() == 0.0);
  fail_unless(fud->getUnit(2)->getKind() == UNIT_KIND_SECOND);

  fud = m->getReaction(0)->getReactant(0)->getStoichiometryMath()->getDerivedUnitDefinition();
  undecl = m->getReaction(0)->getReactant(0)->getStoichiometryMath()->containsUndeclaredUnits();

  fail_unless(undecl == 1);

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);


}
END_TEST


START_TEST (test_DerivedUnitDefinition_event)
{
  UnitDefinition *fud = m->getEvent(0)->getEventAssignment(0)->getDerivedUnitDefinition();
  bool undecl = m->getEvent(0)->getEventAssignment(0)->containsUndeclaredUnits();

  fail_unless(undecl == 1);

  fail_unless(fud->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_METRE);

  fail_unless(fud->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnit(1)->getKind() == UNIT_KIND_SECOND);


}
END_TEST


Suite *
create_suite_DerivedUnitDefinition (void)
{
  Suite *suite = suite_create("DerivedUnitDefinition");
  TCase *tcase = tcase_create("DerivedUnitDefinition");

  tcase_add_checked_fixture(tcase,
                            DerivedUnitDefinition_setup,
                            DerivedUnitDefinition_teardown);

  tcase_add_test(tcase, test_DerivedUnitDefinition_compartment );
  tcase_add_test(tcase, test_DerivedUnitDefinition_species );
  tcase_add_test(tcase, test_DerivedUnitDefinition_parameter );
  tcase_add_test(tcase, test_DerivedUnitDefinition_initialassignment );
  tcase_add_test(tcase, test_DerivedUnitDefinition_rule );
  tcase_add_test(tcase, test_DerivedUnitDefinition_reaction );
  tcase_add_test(tcase, test_DerivedUnitDefinition_event );
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
