/**
 * @file    TestLevelCompatibility.cpp
 * @brief   Compatability between levels unit tests
 * @author  Sarah Keating
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


#include <check.h>

#include <iostream>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

extern char *TestDataDirectory;



START_TEST (test_LevelCompatibility_unit_fails)
{
  SBMLReader        reader;
  SBMLDocument*     d;

  std::string filename(TestDataDirectory);
  filename += "inconsistent-l2v1-units.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"inconsistent-l2v1-units.xml\") returned a NULL pointer.");
  }

  unsigned int n = d->checkL2v1Compatibility();

  fail_unless( n == 1 );

  fail_unless (d->getError(0)->getErrorId() == StrictUnitsRequiredInL2v1);

  d->getErrorLog()->clearLog();

  n = d->checkL2v1Compatibility(true);

  fail_unless( n == 0 );

  delete d;
}
END_TEST



START_TEST (test_LevelCompatibility_unit_warnings)
{
  SBMLReader        reader;
  SBMLDocument*     d;

  std::string filename(TestDataDirectory);
  filename += "inconsistent-l2v1-units-2.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"inconsistent-l2v1-units-2.xml\") returned a NULL pointer.");
  }

  unsigned int n = d->checkL2v1Compatibility();

  fail_unless( n == 0 );

  d->getErrorLog()->clearLog();

  n = d->checkL2v1Compatibility(true);

  fail_unless( n == 0 );

  delete d;
}
END_TEST


START_TEST (test_LevelCompatibility_unit_fails_l1)
{
  SBMLReader        reader;
  SBMLDocument*     d;

  std::string filename(TestDataDirectory);
  filename += "inconsistent-l2v1-units.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"inconsistent-l2v1-units.xml\") returned a NULL pointer.");
  }

  unsigned int n = d->checkL1Compatibility();

  fail_unless( n == 1 );

  fail_unless (d->getError(0)->getErrorId() == StrictUnitsRequiredInL1);

  d->getErrorLog()->clearLog();

  n = d->checkL1Compatibility(true);

  fail_unless( n == 0 );

  delete d;
}
END_TEST



START_TEST (test_LevelCompatibility_unit_warnings_l1)
{
  SBMLReader        reader;
  SBMLDocument*     d;

  std::string filename(TestDataDirectory);
  filename += "inconsistent-l2v1-units-2.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"inconsistent-l2v1-units-2.xml\") returned a NULL pointer.");
  }

  unsigned int n = d->checkL1Compatibility();

  fail_unless( n == 0 );

  d->getErrorLog()->clearLog();

  n = d->checkL1Compatibility(true);

  fail_unless( n == 0 );

  delete d;
}
END_TEST


START_TEST (test_LevelCompatibility_unit_fails_l2v2)
{
  SBMLReader        reader;
  SBMLDocument*     d;

  std::string filename(TestDataDirectory);
  filename += "inconsistent-l2v1-units.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"inconsistent-l2v1-units.xml\") returned a NULL pointer.");
  }

  unsigned int n = d->checkL2v2Compatibility();

  fail_unless( n == 1 );

  fail_unless (d->getError(0)->getErrorId() == StrictUnitsRequiredInL2v2);

  d->getErrorLog()->clearLog();

  n = d->checkL1Compatibility(true);

  fail_unless( n == 0 );

  delete d;
}
END_TEST



START_TEST (test_LevelCompatibility_unit_fails_l2v3)
{
  SBMLReader        reader;
  SBMLDocument*     d;

  std::string filename(TestDataDirectory);
  filename += "inconsistent-l2v1-units.xml";


  d = reader.readSBML(filename);

  if (d == NULL)
  {
    fail("readSBML(\"inconsistent-l2v1-units.xml\") returned a NULL pointer.");
  }

  unsigned int n = d->checkL2v3Compatibility();

  fail_unless( n == 1 );

  fail_unless (d->getError(0)->getErrorId() == StrictUnitsRequiredInL2v3);

  d->getErrorLog()->clearLog();

  n = d->checkL1Compatibility(true);

  fail_unless( n == 0 );

  delete d;
}
END_TEST




Suite *
create_suite_LevelCompatibility (void)
{
  Suite *suite = suite_create("LevelCompatibility");
  TCase *tcase = tcase_create("LevelCompatibility");


  tcase_add_test(tcase, test_LevelCompatibility_unit_fails);
  tcase_add_test(tcase, test_LevelCompatibility_unit_warnings);
  tcase_add_test(tcase, test_LevelCompatibility_unit_fails_l1);
  tcase_add_test(tcase, test_LevelCompatibility_unit_warnings_l1);
  tcase_add_test(tcase, test_LevelCompatibility_unit_fails_l2v2);
  tcase_add_test(tcase, test_LevelCompatibility_unit_fails_l2v3);


  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
