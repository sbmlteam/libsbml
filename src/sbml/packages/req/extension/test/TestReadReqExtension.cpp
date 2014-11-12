/**
 * @file    TestReadReqExtension.cpp
 * @brief   Unit tests of writing ReqExtension 
 * @author  Sarah Keating
 */

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/packages/req/extension/ReqExtension.h>
#include <sbml/packages/req/common/ReqExtensionTypes.h>
#include <sbml/packages/req/validator/ReqSBMLError.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART

extern char *TestDataDirectory;

START_TEST (test_ReqExtension_read_L3V1V1)
{
  char *filename = safe_strcat(TestDataDirectory, "req_example1.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  
  fail_unless(document->getPackageName() == "core");
  
  Model *model = document->getModel();
  
  fail_unless(model != NULL);
  fail_unless(model->getPackageName() == "core");
  fail_unless(document->getNumErrors() == 0);
  
  // get the rule
  AssignmentRule * r = static_cast<AssignmentRule*>(model->getRule(0));
  fail_unless(r != NULL);
  fail_unless(r->getPackageName() == "core");

  // get req plugin
  ReqSBasePlugin* rplugin = static_cast<ReqSBasePlugin*>(r->getPlugin("req"));
  fail_unless(rplugin != NULL);
  
  fail_unless(rplugin->getNumChangedMaths() == 1);
  fail_unless(rplugin->getListOfChangedMaths()->getPackageName() == "req");
  
  ChangedMath* change = rplugin->getChangedMath(0);
  fail_unless(change->getChangedBy() == "http://www.sbml.org/sbml/level3/version1/spatial/version1");
  fail_unless(change->getViableWithoutChange() == false);

  delete document;  
  safe_free(filename);
}
END_TEST


START_TEST (test_ReqExtension_read_L3V1V1_defaultNS)
{
  char *filename = safe_strcat(TestDataDirectory, "req_example1_defaultNS.xml");
  
  SBMLDocument *document = readSBMLFromFile(filename);
  
  fail_unless(document->getPackageName() == "core");
  
  Model *model = document->getModel();
  
  fail_unless(model != NULL);
  fail_unless(model->getPackageName() == "core");
  fail_unless(document->getNumErrors() == 0);
  
  // get the rule
  AssignmentRule * r = static_cast<AssignmentRule*>(model->getRule(0));
  fail_unless(r != NULL);
  fail_unless(r->getPackageName() == "core");

  // get req plugin
  ReqSBasePlugin* rplugin = static_cast<ReqSBasePlugin*>(r->getPlugin("req"));
  fail_unless(rplugin != NULL);
  
  fail_unless(rplugin->getNumChangedMaths() == 1);
  fail_unless(rplugin->getListOfChangedMaths()->getPackageName() == "req");
  
  ChangedMath* change = rplugin->getChangedMath(0);
  fail_unless(change->getChangedBy() == "http://www.sbml.org/sbml/level3/version1/spatial/version1");
  fail_unless(change->getViableWithoutChange() == false);

  delete document;  
  safe_free(filename);

}
END_TEST


START_TEST (test_ReqExtension_read_L3V1V1_unknown_elements)
{
  const char* s1 =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:req=\"http://www.sbml.org/sbml/level3/version1/req/version1\" level=\"3\" version=\"1\" req:required=\"false\">\n"
  "  <model>\n"
  "    <listOfCompartments>\n"
  "      <compartment id=\"cytosol\" constant=\"true\">\n"
  "        <listOfChangedMaths xmlns=\"http://www.sbml.org/sbml/level3/version1/req/version1\">\n"
  "          <changedMath changedBy=\"sarah\" viableWithoutChange=\"true\" unkniwn=\"ss\"/>\n"
  "        </listOfChangedMaths>\n"
  "      </compartment>\n"
  "    </listOfCompartments>\n"
  "  </model>\n"
  "</sbml>\n"
  ;
  
  SBMLDocument *document = readSBMLFromString(s1);
  Model *model = document->getModel();
  
  fail_unless(model != NULL);
  fail_unless(document->getNumErrors() == 1);
  
  delete document;
}
END_TEST

Suite *
create_suite_ReadReqExtension (void)
{
  Suite *suite = suite_create("ReadReqExtension");
  TCase *tcase = tcase_create("ReadReqExtension");
  
  tcase_add_test( tcase, test_ReqExtension_read_L3V1V1);
  tcase_add_test( tcase, test_ReqExtension_read_L3V1V1_defaultNS);
  tcase_add_test( tcase, test_ReqExtension_read_L3V1V1_unknown_elements);
  suite_add_tcase(suite, tcase);
  
  return suite;
}


CK_CPPEND
