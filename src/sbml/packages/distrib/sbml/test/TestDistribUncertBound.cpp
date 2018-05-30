/**
* @file    TestWriteDistribExtension.cpp
* @brief   Unit tests of writing DistribExtension
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
*/

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/distrib/common/DistribExtensionTypes.h>

#include <sbml/conversion/ConversionProperties.h>

#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */

CK_CPPSTART
extern char *TestDataDirectory;

//static DistribUncertBound * UB;
//
//
//void
//TestUncertBound_setup(void)
//{
//  try
//  {
//    UB = new DistribUncertBound(3, 1, 1);
//  }
//  catch (...)
//  {
//    fail("Failed to create a DistribExtension object");
//  }
//}
//
//void
//TestUncertBound_teardown(void)
//{
//  delete UB;
//}



static bool
equals(std::string& expected, std::string& actual)
{
  if (expected == actual) return true;

  cout << "\nStrings are not equal:\n";
  cout << "Expected:\n[" << expected << "]\n";
  cout << "Actual:\n[" << actual << "]\n";

  return false;
}




START_TEST(test_UncertBound_write_attributes)
{
  std::string expected = "<uncertBound id=\"ub\" value=\"2.3\" inclusive=\"true\"/>";
  
                DistribUncertBound *UB = new DistribUncertBound(new DistribPkgNamespaces());
  fail_unless(UB->isSetId() == false);
  fail_unless(UB->isSetValue() == false);
  fail_unless(UB->isSetVar() == false);
  fail_unless(UB->isSetInclusive() == false);
  fail_unless(UB->isSetUnits() == false);
  
  UB->setValue(2.3);
  fail_unless(UB->isSetValue() == true);
  fail_unless(util_isEqual(UB->getValue(), 2.3));


  UB->setInclusive(true);
  fail_unless(UB->isSetInclusive() == true);
  fail_unless(UB->getInclusive() == true);

  UB->setId("ub");
  fail_unless(UB->isSetId() == true);
  fail_unless(UB->getId() == "ub");

  ostringstream oss;
  oss << UB->toSBML();

  fail_unless(equals(expected, oss.str()));
}
END_TEST




Suite *
create_suite_test_UncertBound(void)
{
  Suite *suite = suite_create("TestUncertBound");
  TCase *tcase = tcase_create("TestUncertBound");

  //tcase_add_checked_fixture(tcase, TestUncertBound_setup, TestUncertBound_teardown);
  
  tcase_add_test(tcase, test_UncertBound_write_attributes);

  suite_add_tcase(suite, tcase);

  return suite;
}

CK_CPPEND