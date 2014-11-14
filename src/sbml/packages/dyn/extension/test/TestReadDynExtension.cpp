/**
 * @file    TestReadDynExtension.cpp
 * @brief   Unit tests of reading DynExtension 
 * @author  Sarah Keating
 */

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/packages/dyn/extension/DynExtension.h>
#include <sbml/packages/dyn/common/DynExtensionTypes.h>
#include <sbml/packages/dyn/validator/DynSBMLError.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART

extern char *TestDataDirectory;

START_TEST (test_DynExtension_read_L3V1V1)
{
  string filename = string(TestDataDirectory) + "dyn_example1.xml";
  SBMLDocument *document = readSBMLFromFile(filename.c_str());
  
  fail_unless(document->getPackageName() == "core");
  
  Model *model = document->getModel();
  
  fail_unless(model != NULL);
  fail_unless(model->getPackageName() == "core");
  fail_unless(document->getNumErrors() == 0);
  
  // get the first event
  Event * e = model->getEvent(0);
  fail_unless(e != NULL);
  fail_unless(e->getPackageName() == "core");

  // get dyn plugin
  DynEventPlugin* eplugin = static_cast<DynEventPlugin*>(e->getPlugin("dyn"));
  fail_unless(eplugin != NULL);

  fail_unless(eplugin->isSetCboTerm() == true);
  fail_unless(eplugin->getCboTerm() == "http://cbo.biocomplexity.indiana.edu/svn/cbo/trunk/CBO_1_0.owl#CellDeath");

  fail_unless(eplugin->isSetApplyToAll() == true);
  fail_unless(eplugin->getApplyToAll() == true);
  

  // get the second event
  e = model->getEvent(1);
  fail_unless(e != NULL);
  fail_unless(e->getPackageName() == "core");

  // get dyn plugin
  eplugin = static_cast<DynEventPlugin*>(e->getPlugin("dyn"));
  fail_unless(eplugin != NULL);

  fail_unless(eplugin->isSetCboTerm() == true);
  fail_unless(eplugin->getCboTerm() == "http://cbo.biocomplexity.indiana.edu/svn/cbo/trunk/CBO_1_0.owl#CellDevision");

  fail_unless(eplugin->isSetApplyToAll() == true);
  fail_unless(eplugin->getApplyToAll() == true);

  delete document;  
}
END_TEST


START_TEST (test_DynExtension_read_L3V1V1_2)
{
  string filename = string(TestDataDirectory) + "dyn_example2.xml";
  SBMLDocument *document = readSBMLFromFile(filename.c_str());
  
  fail_unless(document->getPackageName() == "core");
  
  Model *model = document->getModel();
  
  fail_unless(model != NULL);
  fail_unless(model->getPackageName() == "core");
  fail_unless(document->getNumErrors() == 0);
  
  // get the first compartment
  Compartment * c = model->getCompartment(0);
  fail_unless(c != NULL);
  fail_unless(c->getPackageName() == "core");

  // get dyn plugin
  DynCompartmentPlugin* eplugin = static_cast<DynCompartmentPlugin*>(c->getPlugin("dyn"));
  fail_unless(eplugin != NULL);

  fail_unless(eplugin->isSetCboTerm() == false);

  fail_unless(eplugin->getNumSpatialComponents() == 2);

  SpatialComponent * sc = eplugin->getSpatialComponent(0);

  fail_unless(sc != NULL);

  fail_unless(sc->getSpatialIndex() == DYN_SPATIALKIND_CARTESIANX);
  fail_unless(sc->getVariable() == "q1_X");
  
  sc = eplugin->getSpatialComponent(1);

  fail_unless(sc != NULL);

  fail_unless(sc->getSpatialIndex() == DYN_SPATIALKIND_CARTESIANY);
  fail_unless(sc->getVariable() == "q1_Y");

  delete document;  
}
END_TEST


START_TEST (test_DynExtension_read_L3V1V1_defaultNS)
{
  string filename = string(TestDataDirectory) + "dyn_example2_defaultNS.xml";
  SBMLDocument *document = readSBMLFromFile(filename.c_str());
  
  fail_unless(document->getPackageName() == "core");
  
  Model *model = document->getModel();
  
  fail_unless(model != NULL);
  fail_unless(model->getPackageName() == "core");
  fail_unless(document->getNumErrors() == 0);
  
  // get the first compartment
  Compartment * c = model->getCompartment(0);
  fail_unless(c != NULL);
  fail_unless(c->getPackageName() == "core");

  // get dyn plugin
  DynCompartmentPlugin* eplugin = static_cast<DynCompartmentPlugin*>(c->getPlugin("dyn"));
  fail_unless(eplugin != NULL);

  fail_unless(eplugin->isSetCboTerm() == false);

  fail_unless(eplugin->getNumSpatialComponents() == 2);

  SpatialComponent * sc = eplugin->getSpatialComponent(0);

  fail_unless(sc != NULL);

  fail_unless(sc->getSpatialIndex() == DYN_SPATIALKIND_CARTESIANX);
  fail_unless(sc->getVariable() == "q1_X");
  
  sc = eplugin->getSpatialComponent(1);

  fail_unless(sc != NULL);

  fail_unless(sc->getSpatialIndex() == DYN_SPATIALKIND_CARTESIANY);
  fail_unless(sc->getVariable() == "q1_Y");

  delete document;  
}
END_TEST


START_TEST (test_DynExtension_read_L3V1V1_unknown_elements)
{
  const char* s1 =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:dyn=\"http://www.sbml.org/sbml/level3/version1/dyn/version1\" level=\"3\" version=\"1\" dyn:required=\"true\">\n"
  "  <model>\n"
  "    <listOfCompartments>\n"
  "      <compartment id=\"cytosol\" constant=\"true\">\n"
  "        <listOfSpatialComponents xmlns=\"http://www.sbml.org/sbml/level3/version1/dyn/version1\">\n"
  "          <spatialComponent spatialIndex=\"cartesianX\" variable=\"q1_X\" unknown=\"ss\"/>\n"
  "        </listOfSpatialComponents>\n"
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
create_suite_ReadDynExtension (void)
{
  Suite *suite = suite_create("ReadDynExtension");
  TCase *tcase = tcase_create("ReadDynExtension");
  
  tcase_add_test( tcase, test_DynExtension_read_L3V1V1);
  tcase_add_test( tcase, test_DynExtension_read_L3V1V1_2);
  tcase_add_test( tcase, test_DynExtension_read_L3V1V1_defaultNS);
  tcase_add_test( tcase, test_DynExtension_read_L3V1V1_unknown_elements);
  suite_add_tcase(suite, tcase);
  
  return suite;
}


CK_CPPEND
