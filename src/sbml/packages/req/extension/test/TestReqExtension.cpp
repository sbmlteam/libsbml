/**
 * @file    TestReqExtension.cpp
 * @brief   TestReqExtension unit tests
 * @author  Sarah Keating
 */

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/packages/req/extension/ReqExtension.h>
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

static ReqExtension* G; 
static ReqPkgNamespaces* GNS;
static string REQ_XMLNS_L3V1V1;
static string CORE_XMLNS_L2V4;
static string REQ_PACKAGE_NAME;

void
ReqExtensionTest_setup (void)
{
  try
  {
    G = new ReqExtension();
    GNS = new ReqPkgNamespaces();
    REQ_PACKAGE_NAME = G->getName();
    REQ_XMLNS_L3V1V1 = GNS->getURI();
    CORE_XMLNS_L2V4 = SBMLNamespaces::getSBMLNamespaceURI(2, 4);
  }
  catch(...)
  {
    fail("Failed to create a ReqExtension object");
  }
}


void
ReqExtensionTest_teardown (void)
{
  delete G;
  delete GNS;
}


START_TEST (test_ReqExtension_getName)
{
  fail_unless(G->getName() == "req");
  fail_unless(G->getName() == REQ_PACKAGE_NAME);
}
END_TEST


START_TEST (test_ReqExtension_getURI)
{
  fail_unless(G->getURI(3,1,1) == REQ_XMLNS_L3V1V1);
  fail_unless(G->getURI(2,1,1) == "");
  fail_unless(G->getURI(4,1,1) == "");
}
END_TEST


START_TEST (test_ReqExtension_getLevelVersion)
{
  fail_unless(G->getLevel(REQ_XMLNS_L3V1V1) == 3);
  fail_unless(G->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getLevel("")                          == 0);

  fail_unless(G->getVersion(REQ_XMLNS_L3V1V1) == 1);
  fail_unless(G->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getVersion("")                          == 0);

  fail_unless(G->getPackageVersion(REQ_XMLNS_L3V1V1) == 1);
  fail_unless(G->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getPackageVersion("")                          == 0);
}
END_TEST


START_TEST (test_ReqExtension_getSBMLExtensionNamespaces)
{
  ReqPkgNamespaces *reqns;
  reqns = static_cast<ReqPkgNamespaces*>(G->getSBMLExtensionNamespaces(REQ_XMLNS_L3V1V1));

  fail_unless(reqns->getLevel()          == 3);
  fail_unless(reqns->getVersion()        == 1);
  fail_unless(reqns->getPackageVersion() == 1);

  delete reqns;

  reqns = static_cast<ReqPkgNamespaces*>(G->getSBMLExtensionNamespaces(""));

  fail_unless(reqns == NULL);
}
END_TEST


START_TEST(test_ReqExtension_copy)
{
  ReqExtension *g2 = new ReqExtension(*G);

  fail_unless(g2->getName() == "req");
  fail_unless(g2->getName() == REQ_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == REQ_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(REQ_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(REQ_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(REQ_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_ReqExtension_assignment)
{
  ReqExtension* g2 = new ReqExtension();

  (*g2) = (*G);

  fail_unless(g2->getName() == "req");
  fail_unless(g2->getName() == REQ_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == REQ_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(REQ_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(REQ_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(REQ_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_ReqExtension_clone)
{
  ReqExtension* g2 = G->clone();

  fail_unless(g2->getName() == "req");
  fail_unless(g2->getName() == REQ_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == REQ_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(REQ_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(REQ_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(REQ_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_ReqExtension_registry)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("req");

  fail_unless(sbext != NULL);

  fail_unless(sbext->getName() == "req");
  fail_unless(sbext->getName() == REQ_PACKAGE_NAME);

  fail_unless(sbext->getURI(3,1,1) == REQ_XMLNS_L3V1V1);
  fail_unless(sbext->getURI(2,1,1) == "");
  fail_unless(sbext->getURI(4,1,1) == "");

  fail_unless(sbext->getLevel(REQ_XMLNS_L3V1V1) == 3);
  fail_unless(sbext->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getLevel("")                          == 0);

  fail_unless(sbext->getVersion(REQ_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getVersion("")                          == 0);

  fail_unless(sbext->getPackageVersion(REQ_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getPackageVersion("")                          == 0);

  delete sbext;
}
END_TEST


START_TEST(test_ReqExtension_typecode)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("req");

  fail_unless(sbext != NULL);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_REQ_CHANGED_MATH), "ChangedMath") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_REQ_CHANGED_MATH-1), "(Unknown SBML Req Type)") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_REQ_CHANGED_MATH+1), "(Unknown SBML Req Type)") == 0);

  delete sbext;
}
END_TEST

START_TEST(test_ReqExtension_SBMLtypecode)
{	
	fail_unless(strcmp(SBMLTypeCode_toString(SBML_REQ_CHANGED_MATH     ,"req"), "ChangedMath") == 0);
	fail_unless(strcmp(SBMLTypeCode_toString(SBML_REQ_CHANGED_MATH - 1   ,"req"), "(Unknown SBML Req Type)") == 0);
	fail_unless(strcmp(SBMLTypeCode_toString(SBML_REQ_CHANGED_MATH + 1  ,"req"), "(Unknown SBML Req Type)") == 0);
}
END_TEST

Suite *
create_suite_ReqExtension (void)
{
  Suite *suite = suite_create("ReqExtension");
  TCase *tcase = tcase_create("ReqExtension");

  tcase_add_checked_fixture(tcase, ReqExtensionTest_setup, ReqExtensionTest_teardown);
 
  tcase_add_test( tcase, test_ReqExtension_getName         );
  tcase_add_test( tcase, test_ReqExtension_getURI          );
  tcase_add_test( tcase, test_ReqExtension_getLevelVersion );
  tcase_add_test( tcase, test_ReqExtension_getSBMLExtensionNamespaces);
  tcase_add_test( tcase, test_ReqExtension_copy            );
  tcase_add_test( tcase, test_ReqExtension_assignment      );
  tcase_add_test( tcase, test_ReqExtension_clone           );
  tcase_add_test( tcase, test_ReqExtension_registry        );
  tcase_add_test( tcase, test_ReqExtension_typecode        );
  tcase_add_test( tcase, test_ReqExtension_SBMLtypecode    );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
