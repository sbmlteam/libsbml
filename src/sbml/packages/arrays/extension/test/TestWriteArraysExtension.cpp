/**
* @file    TestWriteArraysExtension.cpp
* @brief   Unit tests of writing ArraysExtension
* @author  Akiya Jouraku
*
* $Id: $
* $HeadURL: $
*/

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/arrays/common/ArraysExtensionTypes.h>

#include <sbml/conversion/ConversionProperties.h>

#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */

CK_CPPSTART
extern char *TestDataDirectory;

static string Arrays_XMLNS_L3V1V1;
static ArraysExtension* G;
static ArraysPkgNamespaces* GNS;

void
WriteArraysExtensionTest_setup(void)
{
  try
  {
    G = new ArraysExtension();
    GNS = new ArraysPkgNamespaces();
    Arrays_XMLNS_L3V1V1 = GNS->getURI();
  }
  catch (...)
  {
    fail("Failed to create a ArraysExtension object");
  }
}

void
WriteArraysExtensionTest_teardown(void)
{
  delete G;
  delete GNS;
}



Suite *
create_suite_WriteArraysExtension(void)
{
  Suite *suite = suite_create("WriteArraysExtension");
  TCase *tcase = tcase_create("WriteArraysExtension");

  tcase_add_checked_fixture(tcase, WriteArraysExtensionTest_setup, WriteArraysExtensionTest_teardown);

  suite_add_tcase(suite, tcase);

  return suite;
}

CK_CPPEND