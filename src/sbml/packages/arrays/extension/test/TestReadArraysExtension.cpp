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
#include <sbml/SBMLWriter.h>
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


static int
countOccurrences(const std::string& text, const std::string& sub)
{
  int n = 0;
  for (std::size_t pos = text.find(sub); pos != std::string::npos; pos = text.find(sub, pos + sub.size()))
  {
    n++;
  }
  return n;
}


/*
 * Reads an L3V1 file, edits the sbml element to make it L3V2, and checks that
 * the attribute of the package elements is written with no namespace prefix.
 */
static void
checkWrittenUnprefixedAtL3V2(const std::string& file, const std::string& prefix,
                             const std::string& attribute, const std::string& value)
{
  std::string filename = std::string(TestDataDirectory) + "/" + file;
  SBMLDocument* doc = readSBMLFromFile(filename.c_str());
  fail_unless(doc != NULL);
  std::string text = writeSBMLToStdString(doc);
  delete doc;

  const std::string plain = " " + attribute + "=\"" + value + "\"";
  const std::string prefixed = " " + prefix + ":" + attribute + "=\"" + value + "\"";
  fail_unless(countOccurrences(text, prefixed) > 0);
  const int expected = countOccurrences(text, plain) + countOccurrences(text, prefixed);

  std::size_t pos = text.find("level3/version1/core");
  fail_unless(pos != std::string::npos);
  text.replace(pos, 20, "level3/version2/core");
  pos = text.find("version=\"1\"");
  fail_unless(pos != std::string::npos);
  text.replace(pos, 11, "version=\"2\"");

  doc = readSBMLFromString(text.c_str());
  fail_unless(doc->getLevel() == 3);
  fail_unless(doc->getVersion() == 2);
  std::string output = writeSBMLToStdString(doc);
  delete doc;

  fail_unless(output.find(" " + prefix + ":" + attribute + "=\"") == std::string::npos);
  fail_unless(countOccurrences(output, plain) == expected);
}


START_TEST(test_ArraysExtension_write_ids_unprefixed_L3V2)
{
  checkWrittenUnprefixedAtL3V2("arrays1ns.xml", "arrays", "id", "i");
}
END_TEST


START_TEST(test_ArraysExtension_duplicate_id_logged_once)
{
  std::string filename = std::string(TestDataDirectory) + "/arrays1ns.xml";
  SBMLDocument* doc = readSBMLFromFile(filename.c_str());
  fail_unless(doc != NULL);
  std::string text = writeSBMLToStdString(doc);
  delete doc;

  // give the first dimension a plain id that differs from its arrays:id
  const std::string target = " arrays:id=\"i\"";
  const std::size_t pos = text.find(target);
  fail_unless(pos != std::string::npos);
  text.insert(pos + target.size(), " id=\"j\"");

  doc = readSBMLFromString(text.c_str());
  unsigned int duplicates = 0;
  for (unsigned int n = 0; n < doc->getNumErrors(); n++)
  {
    if (doc->getError(n)->getMessage().find("twice with different values") != std::string::npos)
    {
      duplicates++;
    }
  }
  fail_unless(duplicates == 1);
  fail_unless(doc->getNumErrors() == 1);
  delete doc;
}
END_TEST


Suite *
create_suite_ReadArraysExtension(void)
{
  Suite *suite = suite_create("ReadArraysExtension");
  TCase *tcase = tcase_create("ReadArraysExtension");

  tcase_add_test(tcase, test_ArraysExtension_read_and_validate);
  tcase_add_test(tcase, test_ArraysExtension_write_ids_unprefixed_L3V2);
  tcase_add_test(tcase, test_ArraysExtension_duplicate_id_logged_once);
  suite_add_tcase(suite, tcase);

  return suite;
}

CK_CPPEND
