/**
 * \file    TestFormulaUnitsDataMap_map.cpp
 * \brief   fomula units data unit tests
 * \author  Ben Bornstein
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
#include <sbml/units/FormulaUnitsData.h>

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
FormulaUnitsDataMap_setup (void)
{
  char *filename = safe_strcat(TestDataDirectory, "formula.xml");

  d = readSBML(filename);
  m = d->getModel();

  m->populateListFormulaUnitsData();

  safe_free(filename);
}


void
FormulaUnitsDataMap_teardown (void)
{
  delete d;
}
CK_CPPSTART

START_TEST (test_FormulaUnitsDataMap_setters)
{
  FormulaUnitsData *fud = new FormulaUnitsData();
  fud->setUnitReferenceId("sarah");
  fud->setComponentTypecode(SBML_PARAMETER);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "sarah"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Parameter"), NULL);
  fail_unless(!fud->getContainsUndeclaredUnits());
  fail_unless(fud->getCanIgnoreUndeclaredUnits());

  fud->setContainsParametersWithUndeclaredUnits(true);
  fud->setCanIgnoreUndeclaredUnits(false);

  fail_unless(fud->getContainsUndeclaredUnits());
  fail_unless(!fud->getCanIgnoreUndeclaredUnits());

  m->addFormulaUnitsData(fud);

  fail_unless(m->getNumFormulaUnitsData() == 30);

  delete fud;

}
END_TEST

START_TEST (test_FormulaUnitsDataMap_getdefaults)
{
  const FormulaUnitsData *fud = m->getFormulaUnitsData("subs_per_time", SBML_UNKNOWN);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "subs_per_time"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
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


START_TEST (test_FormulaUnitsDataMap_getcompartment)
{
  const FormulaUnitsData *fud = m->getFormulaUnitsData("cell", SBML_COMPARTMENT);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "cell"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Compartment"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
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

  fud = m->getFormulaUnitsData("cell1", SBML_COMPARTMENT);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "cell1"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Compartment"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
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

START_TEST (test_FormulaUnitsDataMap_getspecies)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData("x", SBML_SPECIES);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "x"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Species"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
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

  fud = m->getFormulaUnitsData("y", SBML_SPECIES);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "y"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Species"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
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

  fud = m->getFormulaUnitsData("z2", SBML_SPECIES);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "z2"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Species"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
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

  fud = m->getFormulaUnitsData("x1", SBML_SPECIES);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "x1"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Species"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 1);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_MOLE);

  fail_unless(fud->getPerTimeUnitDefinition()->getNumUnits() == 2);

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
  fail_unless(fud->getPerTimeUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fud = m->getFormulaUnitsData("y1", SBML_SPECIES);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "y1"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Species"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
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

}
END_TEST

START_TEST (test_FormulaUnitsDataMap_getparameter)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData("k1", SBML_PARAMETER);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "k1"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Parameter"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
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

  fud = m->getFormulaUnitsData("k2", SBML_PARAMETER);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "k2"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Parameter"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 0);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

}
END_TEST

START_TEST (test_FormulaUnitsDataMap_getinitialassignment)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData("z2", SBML_INITIAL_ASSIGNMENT);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "z2"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "InitialAssignment"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
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

START_TEST (test_FormulaUnitsDataMap_getrule)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData("x", SBML_ASSIGNMENT_RULE);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "x"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "AssignmentRule"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 0);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);


  fud = m->getFormulaUnitsData("cell1", SBML_RATE_RULE);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "cell1"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "RateRule"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
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

  fud = m->getFormulaUnitsData("alg_rule_0", SBML_ALGEBRAIC_RULE);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "alg_rule_0"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "AlgebraicRule"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
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


START_TEST (test_FormulaUnitsDataMap_getreaction)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData("R", SBML_KINETIC_LAW);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "R"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "KineticLaw"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

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
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fud = m->getFormulaUnitsData("x1", SBML_STOICHIOMETRY_MATH);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "x1"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "StoichiometryMath"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 0);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);


}
END_TEST


START_TEST (test_FormulaUnitsDataMap_getlocalparameters)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData("k_1_R", SBML_LOCAL_PARAMETER);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "k_1_R"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "LocalParameter"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_SECOND);

  fud = m->getFormulaUnitsData("k2_R", SBML_LOCAL_PARAMETER);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "k2_R"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "LocalParameter"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == -2);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_METRE);
  
  fud = m->getFormulaUnitsData("k3_R", SBML_LOCAL_PARAMETER);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "k3_R"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "LocalParameter"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 0);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fud = m->getFormulaUnitsData("h_R", SBML_LOCAL_PARAMETER);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "h_R"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "LocalParameter"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 0);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);
}
END_TEST



START_TEST (test_FormulaUnitsDataMap_getevent)
{
  FormulaUnitsData *fud = m->getFormulaUnitsData("event_0", SBML_TRIGGER);
  //trigger units
  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "event_0"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Trigger"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getMultiplier() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getScale() == 0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getExponent() == 1);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getOffset() == 0.0);
  fail_unless(fud->getUnitDefinition()->getUnit(0)->getKind() == UNIT_KIND_DIMENSIONLESS);


  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);




  fud = m->getFormulaUnitsData("event_0", SBML_EVENT);
  // delay units
  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "event_0"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "Event"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 0);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);

  fud = m->getFormulaUnitsData("k2event_0", SBML_EVENT_ASSIGNMENT);
  // event assignment units
  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "k2event_0"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "EventAssignment"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
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


START_TEST (test_FormulaUnitsDataMap_getById)
{
  const FormulaUnitsData *fud = m->getFormulaUnitsData("R", SBML_KINETIC_LAW);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "R"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "KineticLaw"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 0);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

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
  fail_unless(fud->getUnitDefinition()->getUnit(1)->getKind() == UNIT_KIND_SECOND);

  fud = m->getFormulaUnitsData("x1", SBML_STOICHIOMETRY_MATH);

  fail_unless(!strcmp(fud->getUnitReferenceId().c_str(), "x1"), NULL);
  fail_unless(!strcmp(SBMLTypeCode_toString(fud->getComponentTypecode(), "core"), "StoichiometryMath"), NULL);
  fail_unless(fud->getContainsUndeclaredUnits() == 1);
  fail_unless(fud->getCanIgnoreUndeclaredUnits() == 0);

  fail_unless(fud->getUnitDefinition()->getNumUnits() == 0);

  fail_unless(!strcmp(fud->getUnitDefinition()->getId().c_str(), ""), NULL);


}
END_TEST



Suite *
create_suite_FormulaUnitsDataMap (void)
{
  Suite *suite = suite_create("FormulaUnitDataMap");
  TCase *tcase = tcase_create("FormulaUnitsDataMap");

  tcase_add_checked_fixture(tcase,
                            FormulaUnitsDataMap_setup,
                            FormulaUnitsDataMap_teardown);

  tcase_add_test(tcase, test_FormulaUnitsDataMap_getdefaults );
  tcase_add_test(tcase, test_FormulaUnitsDataMap_getcompartment );
  tcase_add_test(tcase, test_FormulaUnitsDataMap_getspecies );
  tcase_add_test(tcase, test_FormulaUnitsDataMap_getparameter );
  tcase_add_test(tcase, test_FormulaUnitsDataMap_getinitialassignment );
  tcase_add_test(tcase, test_FormulaUnitsDataMap_getrule );
  tcase_add_test(tcase, test_FormulaUnitsDataMap_getreaction );
  tcase_add_test(tcase, test_FormulaUnitsDataMap_getlocalparameters );
  tcase_add_test(tcase, test_FormulaUnitsDataMap_getevent );
  tcase_add_test(tcase, test_FormulaUnitsDataMap_getById );
  tcase_add_test(tcase, test_FormulaUnitsDataMap_setters );
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
