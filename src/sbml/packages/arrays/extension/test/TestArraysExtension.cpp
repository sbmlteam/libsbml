/**
 * @file    TestArraysExtension.cpp
 * @brief   TestArraysExtension unit tests
 * @author  Akiya Jouraku
 *
 * $Id: $
 * $HeadURL: $
 */

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/packages/arrays/common/ArraysExtensionTypes.h>
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

static ArraysExtension* G; 
static ArraysPkgNamespaces* GNS;
static string ARRAYS_XMLNS_L3V1V1;
static string CORE_XMLNS_L2V4;
static string ARRAYS_PACKAGE_NAME;

void
ArraysExtensionTest_setup (void)
{
  try
  {
    G = new ArraysExtension();
    GNS = new ArraysPkgNamespaces();
    ARRAYS_PACKAGE_NAME = G->getName();
    ARRAYS_XMLNS_L3V1V1 = GNS->getURI();
    CORE_XMLNS_L2V4 = SBMLNamespaces::getSBMLNamespaceURI(2, 4);
  }
  catch(...)
  {
    fail("Failed to create a ArraysExtension object");
  }
}


void
ArraysExtensionTest_teardown (void)
{
  delete G;
  delete GNS;
}


START_TEST (test_ArraysExtension_getName)
{
  fail_unless(G->getName() == "arrays");
  fail_unless(G->getName() == ARRAYS_PACKAGE_NAME);
}
END_TEST


START_TEST (test_ArraysExtension_getURI)
{
  fail_unless(G->getURI(3,1,1) == ARRAYS_XMLNS_L3V1V1);
  fail_unless(G->getURI(2,1,1) == "");
  fail_unless(G->getURI(4,1,1) == "");
}
END_TEST


START_TEST (test_ArraysExtension_getLevelVersion)
{
  fail_unless(G->getLevel(ARRAYS_XMLNS_L3V1V1) == 3);
  fail_unless(G->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getLevel("")                          == 0);

  fail_unless(G->getVersion(ARRAYS_XMLNS_L3V1V1) == 1);
  fail_unless(G->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getVersion("")                          == 0);

  fail_unless(G->getPackageVersion(ARRAYS_XMLNS_L3V1V1) == 1);
  fail_unless(G->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getPackageVersion("")                          == 0);
}
END_TEST


START_TEST (test_ArraysExtension_getSBMLExtensionNamespaces)
{
  ArraysPkgNamespaces *Arraysns;
  Arraysns = static_cast<ArraysPkgNamespaces*>(G->getSBMLExtensionNamespaces(ARRAYS_XMLNS_L3V1V1));

  fail_unless(Arraysns->getLevel()          == 3);
  fail_unless(Arraysns->getVersion()        == 1);
  fail_unless(Arraysns->getPackageVersion() == 1);

  delete Arraysns;
  Arraysns = static_cast<ArraysPkgNamespaces*>(G->getSBMLExtensionNamespaces(""));

  fail_unless(Arraysns == NULL);
}
END_TEST


START_TEST(test_ArraysExtension_copy)
{
  ArraysExtension *g2 = new ArraysExtension(*G);

  fail_unless(g2->getName() == "arrays");
  fail_unless(g2->getName() == ARRAYS_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == ARRAYS_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(ARRAYS_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(ARRAYS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(ARRAYS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_ArraysExtension_assignment)
{
  ArraysExtension* g2 = new ArraysExtension();

  (*g2) = (*G);

  fail_unless(g2->getName() == "arrays");
  fail_unless(g2->getName() == ARRAYS_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == ARRAYS_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(ARRAYS_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(ARRAYS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(ARRAYS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_ArraysExtension_clone)
{
  ArraysExtension* g2 = G->clone();

  fail_unless(g2->getName() == "arrays");
  fail_unless(g2->getName() == ARRAYS_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == ARRAYS_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(ARRAYS_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(ARRAYS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(ARRAYS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_ArraysExtension_registry)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("arrays");

  fail_unless(sbext != NULL);

  fail_unless(sbext->getName() == "arrays");
  fail_unless(sbext->getName() == ARRAYS_PACKAGE_NAME);

  fail_unless(sbext->getURI(3,1,1) == ARRAYS_XMLNS_L3V1V1);
  fail_unless(sbext->getURI(2,1,1) == "");
  fail_unless(sbext->getURI(4,1,1) == "");

  fail_unless(sbext->getLevel(ARRAYS_XMLNS_L3V1V1) == 3);
  fail_unless(sbext->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getLevel("")                          == 0);

  fail_unless(sbext->getVersion(ARRAYS_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getVersion("")                          == 0);

  fail_unless(sbext->getPackageVersion(ARRAYS_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getPackageVersion("")                          == 0);

  delete sbext;
}
END_TEST


START_TEST(test_ArraysExtension_typecode)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("arrays");

  fail_unless(sbext != NULL);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_ARRAYS_DIMENSION), "Dimension") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_ARRAYS_INDEX), "Index") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_ARRAYS_DIMENSION +1), "(Unknown SBML Arrays Type)") == 0);

  delete sbext;
}
END_TEST

START_TEST(test_ArraysExtension_SBMLtypecode)
{	
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_ARRAYS_DIMENSION,"arrays"), "Dimension") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_ARRAYS_INDEX,"arrays"), "Index") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_ARRAYS_DIMENSION + 1  ,"arrays"), "(Unknown SBML Arrays Type)") == 0);
}
END_TEST

Suite *
create_suite_ArraysExtension (void)
{
  Suite *suite = suite_create("ArraysExtension");
  TCase *tcase = tcase_create("ArraysExtension");

  tcase_add_checked_fixture(tcase, ArraysExtensionTest_setup, ArraysExtensionTest_teardown);
 
  tcase_add_test( tcase, test_ArraysExtension_getName         );
  tcase_add_test( tcase, test_ArraysExtension_getURI          );
  tcase_add_test( tcase, test_ArraysExtension_getLevelVersion );
  tcase_add_test( tcase, test_ArraysExtension_getSBMLExtensionNamespaces);
  tcase_add_test( tcase, test_ArraysExtension_copy            );
  tcase_add_test( tcase, test_ArraysExtension_assignment      );
  tcase_add_test( tcase, test_ArraysExtension_clone           );
  tcase_add_test( tcase, test_ArraysExtension_registry        );
  tcase_add_test( tcase, test_ArraysExtension_typecode        );
  tcase_add_test( tcase, test_ArraysExtension_SBMLtypecode    );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
