/**
* @file    TestWriteReqExtension.cpp
* @brief   Unit tests of writing ReqExtension 
* @author  Sarah Keating
*/

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/req/extension/ReqExtension.h>
#include <sbml/packages/req/common/ReqExtensionTypes.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

  /** @endcond doxygenIgnored */


  CK_CPPSTART
  extern char *TestDataDirectory;

static string REQ_XMLNS_L3V1V1;
static ReqExtension* G; 
static ReqPkgNamespaces* GNS;

void
  WriteReqExtensionTest_setup (void)
{
  try
  {
    G = new ReqExtension();
    GNS = new ReqPkgNamespaces();
    REQ_XMLNS_L3V1V1 = GNS->getURI();
  }
  catch(...)
  {
    fail("Failed to create a ReqExtension object");
  }
}


void
  WriteReqExtensionTest_teardown (void)
{
  delete G;
  delete GNS;
}


START_TEST (test_ReqExtension_create_and_write_L3V1V1)
{
  ReqPkgNamespaces *sbmlns = new ReqPkgNamespaces(3,1,1);

  // create the document

  SBMLDocument document = SBMLDocument(sbmlns);

  // create the Model

  Model* model=document.createModel();

  // create the Compartment


  Compartment* compartment = model->createCompartment();
  compartment->setId("compartment");
  compartment->setConstant(false);
  compartment->setSize(1);

  // create the Rule

  AssignmentRule* rule = model->createAssignmentRule();
  rule->setVariable("compartment");
  ASTNode * math = SBML_parseFormula("3");
  rule->setMath(math);
  delete math;


  // get the required plugin
  ReqSBasePlugin* splugin = static_cast<ReqSBasePlugin*>(rule->getPlugin("req"));

  // tell the plugin that the math is overridden
  ChangedMath* changed = splugin->createChangedMath ();

  changed->setChangedBy("http://www.sbml.org/sbml/level3/version1/spatial/version1");
  changed->setViableWithoutChange(false);

  std::string s1 = writeSBMLToStdString(&document);

  // check clone()

  SBMLDocument document2 = document;
  std::string s2 = writeSBMLToStdString(&document2);
  fail_unless(s1 == s2); 

  // check operator=

  Model m = *(document.getModel());
  document2.setModel(&m);
  s2 = writeSBMLToStdString(&document2);
  fail_unless(s1==s2);
 
  delete sbmlns;

}
END_TEST

  Suite *
  create_suite_WriteReqExtension (void)
{
  Suite *suite = suite_create("WriteReqExtension");
  TCase *tcase = tcase_create("WriteReqExtension");

  tcase_add_checked_fixture(tcase, WriteReqExtensionTest_setup, WriteReqExtensionTest_teardown);	

  tcase_add_test( tcase, test_ReqExtension_create_and_write_L3V1V1);
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
