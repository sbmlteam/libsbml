/**
 * \file    TestDerivedUnitDefinitions.cpp
 * \brief   unit tests for the getDerivedUnitDefinition function
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/common/common.h>
#include <sbml/common/extern.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/SBMLTypeCodes.h>

#include <sbml/units/UnitFormulaFormatter.h>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

extern char *TestDataDirectory;

static Model *m;
static SBMLDocument* d;

/* 
 * tests the results from different model
 * components that have units
 * e.g. compartment; species; parameter
 */


void
DerivedUnitDefinitionUndefined_setup (void)
{
  char *filename = safe_strcat(TestDataDirectory, "formula_undefined.xml");

  d = readSBML(filename);
  m = d->getModel();

  safe_free(filename);
}


void
DerivedUnitDefinitionUndefined_teardown (void)
{
  delete d;
}
CK_CPPSTART

START_TEST (test_DerivedUnitDefinitionUndefined_p1_plus_2)
{
  const UnitDefinition *fud = m->getInitialAssignment("k1")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 1);
  
  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(m->getInitialAssignment("k1")->containsUndeclaredUnits() == true);

}
END_TEST

START_TEST (test_DerivedUnitDefinitionUndefined_p2_plus_p1_plus_2)
{
  const UnitDefinition *fud = m->getInitialAssignment("k2")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(m->getInitialAssignment("k2")->containsUndeclaredUnits() == true);

}
END_TEST

START_TEST (test_DerivedUnitDefinitionUndefined_p1_plus_p2_plus_2)
{
  const UnitDefinition *fud = m->getInitialAssignment("k3")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(m->getInitialAssignment("k3")->containsUndeclaredUnits() == true);
}
END_TEST

START_TEST (test_DerivedUnitDefinitionUndefined_p1_plus_p2)
{
  const UnitDefinition *fud = m->getInitialAssignment("k4")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(m->getInitialAssignment("k4")->containsUndeclaredUnits() == false);
}
END_TEST

START_TEST (test_DerivedUnitDefinitionUndefined_p2_plus_p1)
{
  const UnitDefinition *fud = m->getInitialAssignment("k5")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(m->getInitialAssignment("k5")->containsUndeclaredUnits() == false);
}
END_TEST

START_TEST (test_DerivedUnitDefinitionUndefined_p_plus_2)
{
  const UnitDefinition *fud = m->getInitialAssignment("k6")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(m->getInitialAssignment("k6")->containsUndeclaredUnits() == true);
}
END_TEST


START_TEST (test_DerivedUnitDefinitionUndefined_p_plus_p2_plus_2)
{
  const UnitDefinition *fud = m->getInitialAssignment("k7")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(m->getInitialAssignment("k7")->containsUndeclaredUnits() == true);
}
END_TEST


START_TEST(test_DerivedUnitDefinitionUndefined_p1_times_2)
{
  const UnitDefinition *fud = m->getInitialAssignment("m1")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(m->getInitialAssignment("m1")->containsUndeclaredUnits() == true);

}
END_TEST

START_TEST(test_DerivedUnitDefinitionUndefined_p2_times_p1_times_2)
{
  const UnitDefinition *fud = m->getInitialAssignment("m2")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 2);
  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_SECOND);

  fail_unless(fud->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnit(1)->getExponent() == 1);
  fail_unless(fud->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnit(1)->getKind() == UNIT_KIND_MOLE);

  fail_unless(m->getInitialAssignment("m2")->containsUndeclaredUnits() == true);

}
END_TEST

START_TEST(test_DerivedUnitDefinitionUndefined_p1_times_p2_times_2)
{
  const UnitDefinition *fud = m->getInitialAssignment("m3")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 2);
  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnit(1)->getExponent() == 1);
  fail_unless(fud->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fail_unless(m->getInitialAssignment("m3")->containsUndeclaredUnits() == true);
}
END_TEST

START_TEST(test_DerivedUnitDefinitionUndefined_p1_times_p2)
{
  const UnitDefinition *fud = m->getInitialAssignment("m4")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 2);
  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnit(1)->getExponent() == 1);
  fail_unless(fud->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fail_unless(m->getInitialAssignment("m4")->containsUndeclaredUnits() == false);
}
END_TEST

START_TEST(test_DerivedUnitDefinitionUndefined_p2_times_p1)
{
  const UnitDefinition *fud = m->getInitialAssignment("m5")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 2);
  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_SECOND);

  fail_unless(fud->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnit(1)->getExponent() == 1);
  fail_unless(fud->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnit(1)->getKind() == UNIT_KIND_MOLE);

  fail_unless(m->getInitialAssignment("m5")->containsUndeclaredUnits() == false);
}
END_TEST

START_TEST(test_DerivedUnitDefinitionUndefined_p_times_2)
{
  const UnitDefinition *fud = m->getInitialAssignment("m6")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(m->getInitialAssignment("m6")->containsUndeclaredUnits() == true);
}
END_TEST


START_TEST(test_DerivedUnitDefinitionUndefined_p_times_p2_times_2)
{
  const UnitDefinition *fud = m->getInitialAssignment("m7")
    ->getDerivedUnitDefinition();


  fail_unless(fud->getNumUnits() == 1);

  fail_unless(fud->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnit(0)->getKind() == UNIT_KIND_SECOND);

  fail_unless(m->getInitialAssignment("m7")->containsUndeclaredUnits() == true);
}
END_TEST


START_TEST(test_DerivedUnitDefinitionUndefined_p1_power_p2)
{
  const UnitDefinition *fud = m->getInitialAssignment("o1")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(m->getInitialAssignment("o1")->containsUndeclaredUnits() == false);
}
END_TEST


START_TEST(test_DerivedUnitDefinitionUndefined_p1_root_p2)
{
  const UnitDefinition *fud = m->getInitialAssignment("o2")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(m->getInitialAssignment("o2")->containsUndeclaredUnits() == false);
}
END_TEST


START_TEST(test_DerivedUnitDefinitionUndefined_p1_root_p2_plus_p3)
{
  const UnitDefinition *fud = m->getInitialAssignment("o3")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(m->getInitialAssignment("o3")->containsUndeclaredUnits() == false);
}
END_TEST


START_TEST(test_DerivedUnitDefinitionUndefined_p1_root_bad_num)
{
  const UnitDefinition *fud = m->getInitialAssignment("o4")
    ->getDerivedUnitDefinition();

  fail_unless(fud->getNumUnits() == 0);

  fail_unless(m->getInitialAssignment("o4")->containsUndeclaredUnits() == false);
}
END_TEST

Suite *
create_suite_DerivedUnitDefinitionUndefined (void)
{
  Suite *suite = suite_create("DerivedUnitDefinitionUndefined");
  TCase *tcase = tcase_create("DerivedUnitDefinitionUndefined");

  tcase_add_checked_fixture(tcase,
                            DerivedUnitDefinitionUndefined_setup,
                            DerivedUnitDefinitionUndefined_teardown);

  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p1_plus_2 );
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p2_plus_p1_plus_2 );
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p1_plus_p2_plus_2 );
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p1_plus_p2 );
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p2_plus_p1 );
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p_plus_2);
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p_plus_p2_plus_2 );
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p1_times_2);
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p2_times_p1_times_2);
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p1_times_p2_times_2);
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p1_times_p2);
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p2_times_p1);
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p_times_2);
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p_times_p2_times_2);
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p1_power_p2);
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p1_root_p2);
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p1_root_p2_plus_p3);
  tcase_add_test(tcase, test_DerivedUnitDefinitionUndefined_p1_root_bad_num);
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
