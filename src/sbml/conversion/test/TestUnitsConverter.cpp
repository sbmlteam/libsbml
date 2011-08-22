/**
 * @file    TestUnitsConverter.cpp
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



START_TEST (test_setup)
{
  SBMLUnitsConverter * units = new SBMLUnitsConverter();

  fail_unless (units->getDefaultProperties().hasOption("units") == true);
  
  delete units;
}
END_TEST


START_TEST (test_setDocument)
{
  SBMLUnitsConverter * units = new SBMLUnitsConverter();
  SBMLDocument *d = new SBMLDocument(3, 1);
  d->createModel();
  units->setDocument(d);

  fail_unless (units->getDocument() == d);
  
  delete units;
  delete d;
}
END_TEST


START_TEST (test_convertCompartment)
{
  SBMLUnitsConverter * units = new SBMLUnitsConverter();
  SBMLDocument *d = new SBMLDocument(3, 1);
  Model * m = d->createModel();
  Compartment *c = m->createCompartment();
  c->setId("c");
  c->setSize(1.0);
  c->setSpatialDimensions(3.0);
  c->setConstant(true);
  c->setUnits("litre");

  fail_unless(m->getNumUnitDefinitions() == 0);

  units->setDocument(d);

  fail_unless (units->convert() == LIBSBML_OPERATION_SUCCESS);
  fail_unless (d->getModel()->getNumUnitDefinitions() == 1);
  fail_unless (equalDouble(d->getModel()->getCompartment(0)->getSize(), 0.001) == true);
  fail_unless (d->getModel()->getCompartment(0)->getUnits() == "unitSid_0");
  
  delete units;
  delete d;
}
END_TEST


Suite *
create_suite_TestUnitsConverter (void)
{ 
  Suite *suite = suite_create("UnitsConverter");
  TCase *tcase = tcase_create("UnitsConverter");


  tcase_add_test(tcase, test_setup);
  tcase_add_test(tcase, test_setDocument);
  tcase_add_test(tcase, test_convertCompartment);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

