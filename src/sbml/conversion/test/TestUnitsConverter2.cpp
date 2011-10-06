/**
 * @file    TestUnitsConverter2.cpp
 * @brief   Tests for unit converter
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <sbml/conversion/SBMLUnitsConverter.h>
#include <sbml/conversion/ConversionProperties.h>



#include <string>
using namespace std;

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


#if WIN32 && !defined(CYGWIN)
int isnan(double x);
int isinf(double x);
int finite(double x);
#ifndef __DBL_EPSILON__ 
#include <float.h>
#define __DBL_EPSILON__ DBL_EPSILON
#endif
#endif


extern char *TestDataDirectory;

static bool
equalDouble (double a, double b)
{
  return (fabs(a-b) < sqrt(__DBL_EPSILON__));
}

START_TEST (test_convert_double_exponent)
{
  SBMLUnitsConverter * units = new SBMLUnitsConverter();
  SBMLDocument *d = new SBMLDocument(3, 1);
  Model * m = d->createModel();
  Parameter *p = m->createParameter();
  p->setId("c");
  p->setValue(3.0);
  p->setConstant(true);
  p->setUnits("my_ud");
  UnitDefinition *ud1 = m->createUnitDefinition();
  ud1->setId("my_ud");
  Unit * u = ud1->createUnit();
  u->initDefaults();
  u->setKind(UNIT_KIND_HENRY);
  u->setMultiplier(1.0);
  u->setExponent(0.5);
  Unit * u1 = ud1->createUnit();
  u1->initDefaults();
  u1->setKind(UNIT_KIND_LITRE);
  u1->setMultiplier(1.0);
  u1->setExponent(0.5);

  units->setDocument(d);

  /* 1 Henry = 1 m^2 kg s^-2 A^-2*/
  /* 1 litre = 0.001 m^3*/
  /* 3 ((H)^0.5) ((l)^0.5) = 0.09.. m^2.5 kg^0.5 s^-1 A^-1 */
  fail_unless (units->convert() == LIBSBML_OPERATION_SUCCESS);
  double ans = 3 * (pow((0.001), 0.5));
  fail_unless (
      equalDouble(d->getModel()->getParameter(0)->getValue(), ans) == true);
  fail_unless (d->getModel()->getParameter(0)->getUnits() == "unitSid_0");

  fail_unless (d->getModel()->getNumUnitDefinitions() == 1);

  UnitDefinition *ud = d->getModel()->getUnitDefinition(0);
  fail_unless(ud->getId() == "unitSid_0");
  fail_unless(ud->getNumUnits() == 4);
  fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);
  fail_unless(ud->getUnit(0)->getScale() == 0);
  fail_unless(equalDouble(ud->getUnit(0)->getMultiplier(), 1.0) == true);
  fail_unless(equalDouble(ud->getUnit(0)->getExponentAsDouble(), -1.0) == true);
  fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_KILOGRAM);
  fail_unless(ud->getUnit(1)->getScale() == 0);
  fail_unless(equalDouble(ud->getUnit(1)->getMultiplier(), 1.0) == true);
  fail_unless(equalDouble(ud->getUnit(1)->getExponentAsDouble(), 0.5) == true);
  fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_METRE);
  fail_unless(ud->getUnit(2)->getScale() == 0);
  fail_unless(equalDouble(ud->getUnit(2)->getMultiplier(), 1.0) == true);
  fail_unless(equalDouble(ud->getUnit(2)->getExponentAsDouble(), 2.5) == true);
  fail_unless(ud->getUnit(3)->getKind() == UNIT_KIND_SECOND);
  fail_unless(ud->getUnit(3)->getScale() == 0);
  fail_unless(equalDouble(ud->getUnit(3)->getMultiplier(), 1.0) == true);
  fail_unless(equalDouble(ud->getUnit(3)->getExponentAsDouble(), -1.0) == true);

  delete units;
  delete d;
}
END_TEST


START_TEST (test_convert_double_exponent1)
{
  SBMLUnitsConverter * units = new SBMLUnitsConverter();
  SBMLDocument *d = new SBMLDocument(3, 1);
  Model * m = d->createModel();
  Parameter *p = m->createParameter();
  p->setId("c");
  p->setValue(3.0);
  p->setConstant(true);
  p->setUnits("my_ud");
  UnitDefinition *ud1 = m->createUnitDefinition();
  ud1->setId("my_ud");
  Unit * u = ud1->createUnit();
  u->initDefaults();
  u->setKind(UNIT_KIND_HENRY);
  u->setMultiplier(0.2);
  u->setExponent(1.5);
  Unit * u1 = ud1->createUnit();
  u1->initDefaults();
  u1->setKind(UNIT_KIND_LITRE);
  u1->setMultiplier(1.0);
  u1->setExponent(0.5);

  units->setDocument(d);

  /* 1 Henry = 1 m^2 kg s^-2 A^-2*/
  /* 1 litre = 0.001 m^3*/
  /* 3 ((0.2H)^1.5) ((l)^0.5) = 0.0848.. m^4.5 kg^1.5 s^-3 A^-3 */
  fail_unless (units->convert() == LIBSBML_OPERATION_SUCCESS);
  double ans = 3 * (pow((0.001), 0.5)) * (pow(0.2, 1.5));
  fail_unless (
      equalDouble(d->getModel()->getParameter(0)->getValue(), ans) == true);
  fail_unless (d->getModel()->getParameter(0)->getUnits() == "unitSid_0");

  fail_unless (d->getModel()->getNumUnitDefinitions() == 1);

  UnitDefinition *ud = d->getModel()->getUnitDefinition(0);
  fail_unless(ud->getId() == "unitSid_0");
  fail_unless(ud->getNumUnits() == 4);
  fail_unless(ud->getUnit(0)->getKind() == UNIT_KIND_AMPERE);
  fail_unless(ud->getUnit(0)->getScale() == 0);
  fail_unless(equalDouble(ud->getUnit(0)->getMultiplier(), 1.0) == true);
  fail_unless(equalDouble(ud->getUnit(0)->getExponentAsDouble(), -3.0) == true);
  fail_unless(ud->getUnit(1)->getKind() == UNIT_KIND_KILOGRAM);
  fail_unless(ud->getUnit(1)->getScale() == 0);
  fail_unless(equalDouble(ud->getUnit(1)->getMultiplier(), 1.0) == true);
  fail_unless(equalDouble(ud->getUnit(1)->getExponentAsDouble(), 1.5) == true);
  fail_unless(ud->getUnit(2)->getKind() == UNIT_KIND_METRE);
  fail_unless(ud->getUnit(2)->getScale() == 0);
  fail_unless(equalDouble(ud->getUnit(2)->getMultiplier(), 1.0) == true);
  fail_unless(equalDouble(ud->getUnit(2)->getExponentAsDouble(), 4.5) == true);
  fail_unless(ud->getUnit(3)->getKind() == UNIT_KIND_SECOND);
  fail_unless(ud->getUnit(3)->getScale() == 0);
  fail_unless(equalDouble(ud->getUnit(3)->getMultiplier(), 1.0) == true);
  fail_unless(equalDouble(ud->getUnit(3)->getExponentAsDouble(), -3.0) == true);

  delete units;
  delete d;
}
END_TEST


START_TEST (test_convert_model_volume)
{
  SBMLUnitsConverter * units = new SBMLUnitsConverter();
  SBMLDocument *d = new SBMLDocument(3, 1);
  Model * m = d->createModel();
  m->setVolumeUnits("litre");
  Compartment *c = m->createCompartment();
  c->setId("c");
  c->setSize(1.0);
  c->setSpatialDimensions(3.0);
  c->setConstant(true);

  fail_unless(m->getNumUnitDefinitions() == 0);

  units->setDocument(d);

  fail_unless (units->convert() == LIBSBML_OPERATION_SUCCESS);
  fail_unless (d->getModel()->getNumUnitDefinitions() == 1);
  fail_unless (
      equalDouble(d->getModel()->getCompartment(0)->getSize(), 0.001) == true);
  fail_unless (d->getModel()->getCompartment(0)->getUnits().empty());
  fail_unless (d->getModel()->getVolumeUnits() == "unitSid_0");

  fail_unless (d->getModel()->getUnitDefinition(0)->getId() == "unitSid_0");
  fail_unless (d->getModel()->getUnitDefinition(0)->getNumUnits() == 1);
  fail_unless (d->getModel()->getUnitDefinition(0)->getUnit(0)->getKind() 
                                                       == UNIT_KIND_METRE);
  fail_unless (d->getModel()->getUnitDefinition(0)->getUnit(0)->getExponent() == 3);
  fail_unless (d->getModel()->getUnitDefinition(0)->getUnit(0)->getMultiplier() == 1);
  fail_unless (d->getModel()->getUnitDefinition(0)->getUnit(0)->getScale() == 0);

  
  delete units;
  delete d;
}
END_TEST


START_TEST (test_convert_model_area)
{
  SBMLUnitsConverter * units = new SBMLUnitsConverter();
  SBMLDocument *d = new SBMLDocument(3, 1);
  Model * m = d->createModel();
  m->setAreaUnits("area");
  Compartment *c = m->createCompartment();
  c->setId("c");
  c->setSize(1.0);
  c->setSpatialDimensions(2.0);
  c->setConstant(true);
  UnitDefinition *ud = m->createUnitDefinition();
  ud->setId("area");
  Unit * u = ud->createUnit();
  u->initDefaults();
  u->setKind(UNIT_KIND_METRE);
  u->setExponent(2);

  units->setDocument(d);

  fail_unless (units->convert() == LIBSBML_OPERATION_SUCCESS);
  fail_unless (d->getModel()->getNumUnitDefinitions() == 1);
  fail_unless (
      equalDouble(d->getModel()->getCompartment(0)->getSize(), 1) == true);
  fail_unless (d->getModel()->getCompartment(0)->getUnits().empty());
  fail_unless (d->getModel()->getAreaUnits() == "area");

  fail_unless (d->getModel()->getUnitDefinition(0)->getId() == "area");
  fail_unless (d->getModel()->getUnitDefinition(0)->getNumUnits() == 1);
  fail_unless (d->getModel()->getUnitDefinition(0)->getUnit(0)->getKind() 
                                                       == UNIT_KIND_METRE);
  fail_unless (d->getModel()->getUnitDefinition(0)->getUnit(0)->getExponent() == 2);
  fail_unless (d->getModel()->getUnitDefinition(0)->getUnit(0)->getMultiplier() == 1);
  fail_unless (d->getModel()->getUnitDefinition(0)->getUnit(0)->getScale() == 0);

  
  delete units;
  delete d;
}
END_TEST


START_TEST (test_convert_model_length)
{
  SBMLUnitsConverter * units = new SBMLUnitsConverter();
  SBMLDocument *d = new SBMLDocument(3, 1);
  Model * m = d->createModel();
  m->setLengthUnits("metre");
  Compartment *c = m->createCompartment();
  c->setId("c");
  c->setSize(1.0);
  c->setSpatialDimensions(1.0);
  c->setConstant(true);

  fail_unless(m->getNumUnitDefinitions() == 0);

  units->setDocument(d);

  fail_unless (units->convert() == LIBSBML_OPERATION_SUCCESS);
  fail_unless (d->getModel()->getNumUnitDefinitions() == 0);
  fail_unless (
      equalDouble(d->getModel()->getCompartment(0)->getSize(), 1) == true);
  fail_unless (d->getModel()->getCompartment(0)->getUnits().empty());
  fail_unless (d->getModel()->getLengthUnits() == "metre");

  delete units;
  delete d;
}
END_TEST


Suite *
create_suite_TestUnitsConverter2 (void)
{ 
  Suite *suite = suite_create("UnitsConverter2");
  TCase *tcase = tcase_create("UnitsConverter2");


  tcase_add_test(tcase, test_convert_double_exponent);
  tcase_add_test(tcase, test_convert_double_exponent1);
  tcase_add_test(tcase, test_convert_model_volume);
  tcase_add_test(tcase, test_convert_model_area);
  tcase_add_test(tcase, test_convert_model_length);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

