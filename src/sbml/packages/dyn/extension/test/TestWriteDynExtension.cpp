/**
* @file    TestWriteDynExtension.cpp
* @brief   Unit tests of writing DynExtension 
* @author  Sarah Keating
*/

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/dyn/extension/DynExtension.h>
#include <sbml/packages/dyn/common/DynExtensionTypes.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

  /** @endcond doxygenIgnored */


CK_CPPSTART
extern char *TestDataDirectory;

static string DYN_XMLNS_L3V1V1;
static DynExtension* G; 
static DynPkgNamespaces* GNS;

void
WriteDynExtensionTest_setup (void)
{
  try
  {
    G = new DynExtension();
    GNS = new DynPkgNamespaces();
    DYN_XMLNS_L3V1V1 = GNS->getURI();
  }
  catch(...)
  {
    fail("Failed to create a DynExtension object");
  }
}


void
WriteDynExtensionTest_teardown (void)
{
  delete G;
  delete GNS;
}


START_TEST (test_DynExtension_create_and_write_L3V1V1)
{
  DynPkgNamespaces sbmlns(3,1,1);

  // create the document

  SBMLDocument *document = new SBMLDocument(&sbmlns);

  // mark dyn as required

  document->setPackageRequired("dyn", true);
  
  // create the Model

  Model* model=document->createModel();

  // create the Compartment

  Compartment* compartment = model->createCompartment();
  compartment->setId("compartment");
  compartment->setConstant(false);
  compartment->setSize(1);

  DynCompartmentPlugin* cplugin =
    static_cast<DynCompartmentPlugin*>(compartment->getPlugin("dyn"));
  
  SpatialComponent* component = cplugin->createSpatialComponent();
  component->setSpatialIndex(DYN_SPATIALKIND_CARTESIANX);
  component->setVariable("q1_X");

  component = cplugin->createSpatialComponent();
  component->setSpatialIndex(DYN_SPATIALKIND_CARTESIANY);
  component->setVariable("q1_Y");

  // create the parameters

  Parameter* param = model->createParameter();
  param->initDefaults();
  param->setId("q1_X");
  param->setValue(1);

  param = model->createParameter();
  param->initDefaults();
  param->setId("q1_Y");
  param->setValue(1);


  string s1 = writeSBMLToStdString(document);

  // check clone()

  SBMLDocument* document2 = document->clone();
  string s2 = writeSBMLToStdString(document2);
  fail_unless(s1==s2); 

  // check operator=

  Model *m = document->getModel();
  document2->setModel(m);
  s2 = writeSBMLToStdString(document2);

  fail_unless(s1==s2); 

  delete document2;
  delete document;  
}
END_TEST


Suite *
create_suite_WriteDynExtension (void)
{
  Suite *suite = suite_create("WriteDynExtension");
  TCase *tcase = tcase_create("WriteDynExtension");

  tcase_add_checked_fixture(tcase, WriteDynExtensionTest_setup, WriteDynExtensionTest_teardown);	

  tcase_add_test( tcase, test_DynExtension_create_and_write_L3V1V1);
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
