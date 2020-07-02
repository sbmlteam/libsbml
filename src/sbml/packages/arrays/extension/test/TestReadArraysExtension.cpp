/**
 * @file    TestReadArraysExtension.cpp
 * @brief   Unit tests of writing ArraysExtension
 * @author  Fank T. Bergmann
 *
 * $Id: $
 * $HeadURL: $
 */

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/packages/arrays/common/ArraysExtensionTypes.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */

CK_CPPSTART

extern char *TestDataDirectory;

START_TEST(test_ArraysExtension_read_and_validate)
{
  
  const char* files[4] = { "arrays1-defaultns.xml",  "arrays1ns.xml", 
    "arrays2-defaultns.xml", "arrays2ns.xml" };
  
  

  for (int i = 0; i < 4; ++ i)
  {
    std::string fileName = std::string(TestDataDirectory) + std::string("/") + std::string(files[i]);
    SBMLDocument *doc = readSBMLFromFile(fileName.c_str());
    fail_unless(doc != NULL);
    fail_unless(doc->getModel() != NULL);
    doc->checkConsistency();
    delete doc;
  }

}
END_TEST

Suite *
create_suite_ReadArraysExtension(void)
{
  Suite *suite = suite_create("ReadArraysExtension");
  TCase *tcase = tcase_create("ReadArraysExtension");

  tcase_add_test(tcase, test_ArraysExtension_read_and_validate);
  suite_add_tcase(suite, tcase);

  return suite;
}

CK_CPPEND
