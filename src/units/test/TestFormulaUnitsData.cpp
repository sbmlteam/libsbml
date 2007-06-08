/**
 * \file    TestFormulaUnitsData.cpp
 * \brief   fomula units data unit tests
 * \author  Ben Bornstein
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
#include <sbml/units/FormulaUnitsData.h>

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
FormulaUnitsData_setup (void)
{
  d = new SBMLDocument();
 
  char *filename = safe_strcat(TestDataDirectory, "formula.xml");


  d = readSBML(filename);
  m = d->getModel();

  m->createListFormulaUnitsData();

}


void
FormulaUnitsData_teardown (void)
{
  delete d;
}
CK_CPPSTART

START_TEST (test_FormulaUnitsData_setters)
{
  FormulaUnitsData *fud = new FormulaUnitsData();
  fud->setId("sarah");
  fud->setTypecode(SBML_PARAMETER);

  fail_unless(!strcmp(fud->getId().c_str(), "sarah"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "Parameter"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 1);

  fud->setContainsParametersWithUndeclaredUnits(1);
  fud->setCanIgnoreUndeclaredUnits(0);

  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  m->addFormulaUnitsData(fud);

  fail_unless(m->getNumFormulaUnitsData() == 17);

//  delete fud;

}
END_TEST

START_TEST (test_FormulaUnitsData_getdefaults)
{
  const FormulaUnitsData *fud = m->getFormulaUnitsData(0);

  fail_unless(!strcmp(fud->getId().c_str(), "subs_per_time"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 1);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 2);
  
  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == -2);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_SECOND);

}
END_TEST


START_TEST (test_FormulaUnitsData_getcompartment)
{
  const FormulaUnitsData *fud = m->getFormulaUnitsData(1);

  fail_unless(!strcmp(fud->getId().c_str(), "cell"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "Compartment"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 1);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);
  
  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_LITRE);

  fail_unless(fud->getPerTimeUnitDefinition()->getNumUnits() == 2);
  
  fail_unless(!strcmp(fud->getPerTimeUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_LITRE);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fud = m->getFormulaUnitsData(2);

  fail_unless(!strcmp(fud->getId().c_str(), "cell1"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "Compartment"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 1);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);
  
  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == -2);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_METRE);

  fail_unless(fud->getPerTimeUnitDefinition()->getNumUnits() == 2);
  
  fail_unless(!strcmp(fud->getPerTimeUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getScale() == -2);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_METRE);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_SECOND);
}
END_TEST

START_TEST (test_FormulaUnitsData_getspecies)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData(3);

  fail_unless(!strcmp(fud->getId().c_str(), "x"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "Species"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 1);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_LITRE);

  fail_unless(fud->getPerTimeUnitDefinition()->getNumUnits() == 3);

  fail_unless(!strcmp(fud->getPerTimeUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_LITRE);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(2)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(2)->getScale() == 0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(2)->getExponent() == -1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(2)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(2)->getKind() == UNIT_KIND_SECOND);

  fud = m->getFormulaUnitsData(4);

  fail_unless(!strcmp(fud->getId().c_str(), "y"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "Species"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 1);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == -2);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getPerTimeUnitDefinition()->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getPerTimeUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getScale() == -2);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fud = m->getFormulaUnitsData(5);

  fail_unless(!strcmp(fud->getId().c_str(), "z2"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "Species"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 1);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == -2);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_LITRE);

  fail_unless(fud->getPerTimeUnitDefinition()->getNumUnits() == 3);

  fail_unless(!strcmp(fud->getPerTimeUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getScale() == -2);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_LITRE);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(2)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(2)->getScale() == 0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(2)->getExponent() == -1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(2)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(2)->getKind() == UNIT_KIND_SECOND);

}
END_TEST

START_TEST (test_FormulaUnitsData_getparameter)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData(6);

  fail_unless(!strcmp(fud->getId().c_str(), "k1"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "Parameter"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_METRE);

  fail_unless(fud->getUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fail_unless(fud->getPerTimeUnitDefinition()->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getPerTimeUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_METRE);

  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getExponent() == -2);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fud = m->getFormulaUnitsData(7);

  fail_unless(!strcmp(fud->getId().c_str(), "k2"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "Parameter"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 0);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

}
END_TEST

START_TEST (test_FormulaUnitsData_getinitialassignment)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData(8);

  fail_unless(!strcmp(fud->getId().c_str(), "z2"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "InitialAssignment"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == -2);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_MOLE);

}
END_TEST

START_TEST (test_FormulaUnitsData_getrule)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData(9);

  fail_unless(!strcmp(fud->getId().c_str(), "x"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "AssignmentRule"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);

  fud = m->getFormulaUnitsData(10);

  fail_unless(!strcmp(fud->getId().c_str(), "y"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "RateRule"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 1);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_METRE);

  fail_unless(fud->getUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fud = m->getFormulaUnitsData(11);

  fail_unless(!strcmp(fud->getId().c_str(), "alg_rule_0"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "AlgebraicRule"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);
  
  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);
  
  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_LITRE);
}
END_TEST

START_TEST (test_FormulaUnitsData_getreaction)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData(12);

  fail_unless(!strcmp(fud->getId().c_str(), "R"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "KineticLaw"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 3);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_LITRE);

  fail_unless(fud->getUnitDefinition()->getUnit(2)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(2)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(2)->getExponent() == -1);
  fail_unless(fud->getUnitDefinition()->getUnit(2)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(2)->getKind() == UNIT_KIND_SECOND);

  fud = m->getFormulaUnitsData(13);

  fail_unless(!strcmp(fud->getId().c_str(), "x"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "SpeciesReference"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);

}
END_TEST


START_TEST (test_FormulaUnitsData_getevent)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData(14);

  fail_unless(!strcmp(fud->getId().c_str(), "e1"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "Event"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);

  fud = m->getFormulaUnitsData(15);

  fail_unless(!strcmp(fud->getId().c_str(), "cell"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "EventAssignment"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 2);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_METRE);

  fail_unless(fud->getUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_SECOND);


}
END_TEST


START_TEST (test_FormulaUnitsData_getById)
{
  const FormulaUnitsData *fud = m->getFormulaUnitsData("R", SBML_KINETIC_LAW);

  fail_unless(!strcmp(fud->getId().c_str(), "R"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "KineticLaw"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 3);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getUnitDefinition()->getUnit(1)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getExponent() == -1);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_LITRE);

  fail_unless(fud->getUnitDefinition()->getUnit(2)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(2)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(2)->getExponent() == -1);
  fail_unless(fud->getUnitDefinition()->getUnit(2)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(2)->getKind() == UNIT_KIND_SECOND);

  fud = m->getFormulaUnitsData("x", SBML_SPECIES_REFERENCE);

  fail_unless(!strcmp(fud->getId().c_str(), "x"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getTypecode()), "SpeciesReference"), NULL);
  fail_unless(fud->getContainsParametersWithUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);

}
END_TEST



Suite *
create_suite_FormulaUnitsData (void)
{
  Suite *suite = suite_create("FormulaUnitData");
  TCase *tcase = tcase_create("FormulaUnitsData");

  tcase_add_checked_fixture(tcase,
                            FormulaUnitsData_setup,
                            FormulaUnitsData_teardown);

  tcase_add_test(tcase, test_FormulaUnitsData_getdefaults );
  tcase_add_test(tcase, test_FormulaUnitsData_getcompartment );
  tcase_add_test(tcase, test_FormulaUnitsData_getspecies );
  tcase_add_test(tcase, test_FormulaUnitsData_getparameter );
  tcase_add_test(tcase, test_FormulaUnitsData_getinitialassignment );
  tcase_add_test(tcase, test_FormulaUnitsData_getrule );
  tcase_add_test(tcase, test_FormulaUnitsData_getreaction );
  tcase_add_test(tcase, test_FormulaUnitsData_getevent );
  tcase_add_test(tcase, test_FormulaUnitsData_getById );
  tcase_add_test(tcase, test_FormulaUnitsData_setters );
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
