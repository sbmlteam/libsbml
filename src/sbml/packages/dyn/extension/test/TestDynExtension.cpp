/**
 * @file    TestDynExtension.cpp
 * @brief   TestDynExtension unit tests
 * @author  Sarah Keating
 */

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/packages/dyn/extension/DynExtension.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/SBMLReader.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART
extern char *TestDataDirectory;

static DynExtension* G; 
static DynPkgNamespaces* GNS;
static string DYN_XMLNS_L3V1V1;
static string CORE_XMLNS_L2V4;
static string DYN_PACKAGE_NAME;

void
DynExtensionTest_setup (void)
{
  try
  {
    G = new DynExtension();
    GNS = new DynPkgNamespaces();
    DYN_PACKAGE_NAME = G->getName();
    DYN_XMLNS_L3V1V1 = GNS->getURI();
    CORE_XMLNS_L2V4 = SBMLNamespaces::getSBMLNamespaceURI(2, 4);
  }
  catch(...)
  {
    fail("Failed to create a DynExtension object");
  }
}


void
DynExtensionTest_teardown (void)
{
  delete G;
  delete GNS;
}


START_TEST (test_DynExtension_getName)
{
  fail_unless(G->getName() == "dyn");
  fail_unless(G->getName() == DYN_PACKAGE_NAME);
}
END_TEST


START_TEST (test_DynExtension_getURI)
{
  fail_unless(G->getURI(3,1,1) == DYN_XMLNS_L3V1V1);
  fail_unless(G->getURI(2,1,1) == "");
  fail_unless(G->getURI(4,1,1) == "");
}
END_TEST


START_TEST (test_DynExtension_getLevelVersion)
{
  fail_unless(G->getLevel(DYN_XMLNS_L3V1V1) == 3);
  fail_unless(G->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getLevel("")                          == 0);

  fail_unless(G->getVersion(DYN_XMLNS_L3V1V1) == 1);
  fail_unless(G->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getVersion("")                          == 0);

  fail_unless(G->getPackageVersion(DYN_XMLNS_L3V1V1) == 1);
  fail_unless(G->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getPackageVersion("")                          == 0);
}
END_TEST


START_TEST (test_DynExtension_getSBMLExtensionNamespaces)
{
  DynPkgNamespaces *dynns;
  dynns = static_cast<DynPkgNamespaces*>(G->getSBMLExtensionNamespaces(DYN_XMLNS_L3V1V1));

  fail_unless(dynns->getLevel()          == 3);
  fail_unless(dynns->getVersion()        == 1);
  fail_unless(dynns->getPackageVersion() == 1);

  delete dynns;
  dynns = static_cast<DynPkgNamespaces*>(G->getSBMLExtensionNamespaces(""));

  fail_unless(dynns == NULL);
}
END_TEST


START_TEST(test_DynExtension_copy)
{
  DynExtension *g2 = new DynExtension(*G);

  fail_unless(g2->getName() == "dyn");
  fail_unless(g2->getName() == DYN_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == DYN_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(DYN_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(DYN_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(DYN_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_DynExtension_assignment)
{
  DynExtension* g2 = new DynExtension();

  (*g2) = (*G);

  fail_unless(g2->getName() == "dyn");
  fail_unless(g2->getName() == DYN_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == DYN_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(DYN_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(DYN_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(DYN_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_DynExtension_clone)
{
  DynExtension* g2 = G->clone();

  fail_unless(g2->getName() == "dyn");
  fail_unless(g2->getName() == DYN_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == DYN_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(DYN_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(DYN_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(DYN_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_DynExtension_registry)
{
  SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("dyn");

  fail_unless(sbext != NULL);

  fail_unless(sbext->getName() == "dyn");
  fail_unless(sbext->getName() == DYN_PACKAGE_NAME);

  fail_unless(sbext->getURI(3,1,1) == DYN_XMLNS_L3V1V1);
  fail_unless(sbext->getURI(2,1,1) == "");
  fail_unless(sbext->getURI(4,1,1) == "");

  fail_unless(sbext->getLevel(DYN_XMLNS_L3V1V1) == 3);
  fail_unless(sbext->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getLevel("")                          == 0);

  fail_unless(sbext->getVersion(DYN_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getVersion("")                          == 0);

  fail_unless(sbext->getPackageVersion(DYN_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getPackageVersion("")                          == 0);

  delete sbext;
}
END_TEST


START_TEST(test_DynExtension_typecode)
{
  SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("dyn");

  fail_unless(sbext != NULL);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_DYN_ELEMENT), "DynElement") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_DYN_SPATIALCOMPONENT), "SpatialComponent") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_DYN_ELEMENT-1), "(Unknown SBML Dyn Type)") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_DYN_SPATIALCOMPONENT+1), "(Unknown SBML Dyn Type)") == 0);

  delete sbext;
}
END_TEST

START_TEST(test_DynExtension_SBMLtypecode)
{	
	fail_unless(strcmp(SBMLTypeCode_toString(SBML_DYN_ELEMENT     ,"dyn"), "DynElement") == 0);
	fail_unless(strcmp(SBMLTypeCode_toString(SBML_DYN_SPATIALCOMPONENT     ,"dyn"), "SpatialComponent") == 0);
	fail_unless(strcmp(SBMLTypeCode_toString(SBML_DYN_ELEMENT - 1   ,"dyn"), "(Unknown SBML Dyn Type)") == 0);
	fail_unless(strcmp(SBMLTypeCode_toString(SBML_DYN_SPATIALCOMPONENT + 1  ,"dyn"), "(Unknown SBML Dyn Type)") == 0);
}
END_TEST

Suite *
create_suite_DynExtension (void)
{
  Suite *suite = suite_create("DynExtension");
  TCase *tcase = tcase_create("DynExtension");

  tcase_add_checked_fixture(tcase, DynExtensionTest_setup, DynExtensionTest_teardown);
 
  tcase_add_test( tcase, test_DynExtension_getName         );
  tcase_add_test( tcase, test_DynExtension_getURI          );
  tcase_add_test( tcase, test_DynExtension_getLevelVersion );
  tcase_add_test( tcase, test_DynExtension_getSBMLExtensionNamespaces);
  tcase_add_test( tcase, test_DynExtension_copy            );
  tcase_add_test( tcase, test_DynExtension_assignment      );
  tcase_add_test( tcase, test_DynExtension_clone           );
  tcase_add_test( tcase, test_DynExtension_registry        );
  tcase_add_test( tcase, test_DynExtension_typecode        );
  tcase_add_test( tcase, test_DynExtension_SBMLtypecode    );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
