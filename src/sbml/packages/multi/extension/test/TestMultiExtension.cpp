/**
 * @file    TestMultiExtension.cpp
 * @brief   TestMultiExtension unit tests
 * @author  Akiya Jouraku
 *
 * $Id: $
 * $HeadURL: $
 */

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/packages/multi/extension/MultiExtension.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/SBMLReader.h>
#include <sbml/Model.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART
extern char *TestDataDirectory;

static MultiExtension* G; 
static MultiPkgNamespaces* GNS;
static string MULTI_XMLNS_L3V1V1;
static string CORE_XMLNS_L2V4;
static string MULTI_PACKAGE_NAME;

void
MultiExtensionTest_setup (void)
{
  try
  {
    G = new MultiExtension();
    GNS = new MultiPkgNamespaces();
    MULTI_PACKAGE_NAME = G->getName();
    MULTI_XMLNS_L3V1V1 = GNS->getURI();
    CORE_XMLNS_L2V4 = SBMLNamespaces::getSBMLNamespaceURI(2, 4);
  }
  catch(...)
  {
    fail("Failed to create a MultiExtension object");
  }
}


void
MultiExtensionTest_teardown (void)
{
  delete G;
  delete GNS;
}


START_TEST (test_MultiExtension_getName)
{
  fail_unless(G->getName() == "multi");
  fail_unless(G->getName() == MULTI_PACKAGE_NAME);
}
END_TEST


START_TEST (test_MultiExtension_getURI)
{
  fail_unless(G->getURI(3,1,1) == MULTI_XMLNS_L3V1V1);
  fail_unless(G->getURI(2,1,1) == "");
  fail_unless(G->getURI(4,1,1) == "");
}
END_TEST


START_TEST (test_MultiExtension_getLevelVersion)
{
  fail_unless(G->getLevel(MULTI_XMLNS_L3V1V1) == 3);
  fail_unless(G->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getLevel("")                          == 0);

  fail_unless(G->getVersion(MULTI_XMLNS_L3V1V1) == 1);
  fail_unless(G->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getVersion("")                          == 0);

  fail_unless(G->getPackageVersion(MULTI_XMLNS_L3V1V1) == 1);
  fail_unless(G->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getPackageVersion("")                          == 0);
}
END_TEST


START_TEST (test_MultiExtension_getSBMLExtensionNamespaces)
{
  MultiPkgNamespaces *fbcns;
  fbcns = static_cast<MultiPkgNamespaces*>(G->getSBMLExtensionNamespaces(MULTI_XMLNS_L3V1V1));

  fail_unless(fbcns->getLevel()          == 3);
  fail_unless(fbcns->getVersion()        == 1);
  fail_unless(fbcns->getPackageVersion() == 1);

  delete fbcns;
  fbcns = static_cast<MultiPkgNamespaces*>(G->getSBMLExtensionNamespaces(""));

  fail_unless(fbcns == NULL);
}
END_TEST


START_TEST(test_MultiExtension_copy)
{
  MultiExtension *g2 = new MultiExtension(*G);

  fail_unless(g2->getName() == "multi");
  fail_unless(g2->getName() == MULTI_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == MULTI_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(MULTI_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(MULTI_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(MULTI_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_MultiExtension_assignment)
{
  MultiExtension* g2 = new MultiExtension();

  (*g2) = (*G);

  fail_unless(g2->getName() == "multi");
  fail_unless(g2->getName() == MULTI_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == MULTI_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(MULTI_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(MULTI_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(MULTI_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_MultiExtension_clone)
{
  MultiExtension* g2 = G->clone();

  fail_unless(g2->getName() == "multi");
  fail_unless(g2->getName() == MULTI_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == MULTI_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(MULTI_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(MULTI_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(MULTI_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_MultiExtension_registry)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("multi");

  fail_unless(sbext != NULL);

  fail_unless(sbext->getName() == "multi");
  fail_unless(sbext->getName() == MULTI_PACKAGE_NAME);

  fail_unless(sbext->getURI(3,1,1) == MULTI_XMLNS_L3V1V1);
  fail_unless(sbext->getURI(2,1,1) == "");
  fail_unless(sbext->getURI(4,1,1) == "");

  fail_unless(sbext->getLevel(MULTI_XMLNS_L3V1V1) == 3);
  fail_unless(sbext->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getLevel("")                          == 0);

  fail_unless(sbext->getVersion(MULTI_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getVersion("")                          == 0);

  fail_unless(sbext->getPackageVersion(MULTI_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getPackageVersion("")                          == 0);


  // ensure we get the right plugins
  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);
  SBaseExtensionPoint modelExtPoint("core", SBML_MODEL);
  SBaseExtensionPoint compartmentExtPoint("core", SBML_COMPARTMENT);
  SBaseExtensionPoint speciesExtPoint("core", SBML_SPECIES);
  SBaseExtensionPoint simplespeciesreferenceExtPoint("core", SBML_MODIFIER_SPECIES_REFERENCE);
  SBaseExtensionPoint speciesreferenceExtPoint("core", SBML_SPECIES_REFERENCE);
  SBaseExtensionPoint listOfReactionsExtPoint("core", SBML_LIST_OF, "listOfReactions");
  SBaseExtensionPoint listOfSpeciesExtPoint("core", SBML_LIST_OF, "listOfSpecies");

  fail_unless(sbext->getSBasePluginCreator(sbmldocExtPoint) != NULL);
  fail_unless(sbext->getSBasePluginCreator(modelExtPoint) != NULL);
  fail_unless(sbext->getSBasePluginCreator(compartmentExtPoint) != NULL);
  fail_unless(sbext->getSBasePluginCreator(speciesExtPoint) != NULL);
  fail_unless(sbext->getSBasePluginCreator(simplespeciesreferenceExtPoint) != NULL);
  fail_unless(sbext->getSBasePluginCreator(speciesreferenceExtPoint) != NULL);
  fail_unless(sbext->getSBasePluginCreator(listOfReactionsExtPoint) != NULL);
  fail_unless(sbext->getSBasePluginCreator(listOfSpeciesExtPoint) == NULL);

  delete sbext;
}
END_TEST


START_TEST(test_MultiExtension_typecode)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("multi");

  fail_unless(sbext != NULL);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE), "PossibleSpeciesFeatureValue") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_SPECIES_FEATURE_VALUE), "SpeciesFeatureValue") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_COMPARTMENT_REFERENCE), "CompartmentReference") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_SPECIES_TYPE_INSTANCE), "SpeciesTypeInstance") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_IN_SPECIES_TYPE_BOND), "InSpeciesTypeBond") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_OUTWARD_BINDING_SITE), "OutwardBindingSite") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_SPECIES_FEATURE_TYPE), "SpeciesFeatureType") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_SPECIES_TYPE_COMPONENT_INDEX), "SpeciesTypeComponentIndex") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_SPECIES_FEATURE), "SpeciesFeature") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_SPECIES_TYPE_COMPONENT_MAP_IN_PRODUCT), "SpeciesTypeComponentMapInProduct") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_SPECIES_TYPE), "MultiSpeciesType") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_BINDING_SITE_SPECIES_TYPE), "BindingSiteSpeciesType") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_INTRA_SPECIES_REACTION), "IntraSpeciesReaction") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_SUBLIST_OF_SPECIES_FEATURES), "SubListOfSpeciesFeatures") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE-1), "(Unknown SBML Multi Type)") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_MULTI_SUBLIST_OF_SPECIES_FEATURES+1), "(Unknown SBML Multi Type)") == 0);

  delete sbext;
}
END_TEST

START_TEST(test_MultiExtension_SBMLtypecode)
{	
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE     ,"multi"), "PossibleSpeciesFeatureValue") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_SPECIES_FEATURE_VALUE,"multi"), "SpeciesFeatureValue") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_COMPARTMENT_REFERENCE,"multi"), "CompartmentReference") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_SPECIES_TYPE_INSTANCE,"multi"), "SpeciesTypeInstance") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_IN_SPECIES_TYPE_BOND,"multi"), "InSpeciesTypeBond") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_OUTWARD_BINDING_SITE   ,"multi"), "OutwardBindingSite") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_SPECIES_FEATURE_TYPE   ,"multi"), "SpeciesFeatureType") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_SPECIES_TYPE_COMPONENT_INDEX    ,"multi"), "SpeciesTypeComponentIndex") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_SPECIES_FEATURE    ,"multi"), "SpeciesFeature") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_SPECIES_TYPE_COMPONENT_MAP_IN_PRODUCT    ,"multi"), "SpeciesTypeComponentMapInProduct") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_SPECIES_TYPE   ,"multi"), "MultiSpeciesType") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_BINDING_SITE_SPECIES_TYPE    ,"multi"), "BindingSiteSpeciesType") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_INTRA_SPECIES_REACTION   ,"multi"), "IntraSpeciesReaction") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_SUBLIST_OF_SPECIES_FEATURES   ,"multi"), "SubListOfSpeciesFeatures") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE - 1   ,"multi"), "(Unknown SBML Multi Type)") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_MULTI_SUBLIST_OF_SPECIES_FEATURES + 1  ,"multi"), "(Unknown SBML Multi Type)") == 0);
}
END_TEST

START_TEST(test_MultiExtension_read)
{
  char *filename = safe_strcat(TestDataDirectory, "/simmune_Ecad.xml");

  SBMLDocument *doc = readSBMLFromFile(filename);
  
  fail_unless(doc != NULL);

  fail_unless(doc->getModel() != NULL);
  fail_unless(doc->getModel()->getListOfReactions() != NULL);  
  SBasePlugin* plugin = doc->getModel()->getListOfReactions()->getPlugin("multi");
  fail_unless(plugin != NULL);
  
  delete doc;
  safe_free(filename);
}
END_TEST


Suite *
create_suite_MultiExtension(void)
{
  Suite *suite = suite_create("MultiExtension");
  TCase *tcase = tcase_create("MultiExtension");

  tcase_add_checked_fixture(tcase, MultiExtensionTest_setup, MultiExtensionTest_teardown);

  tcase_add_test(tcase, test_MultiExtension_getName);
  tcase_add_test(tcase, test_MultiExtension_getURI);
  tcase_add_test(tcase, test_MultiExtension_getLevelVersion);
  tcase_add_test(tcase, test_MultiExtension_getSBMLExtensionNamespaces);
  tcase_add_test(tcase, test_MultiExtension_copy);
  tcase_add_test(tcase, test_MultiExtension_assignment);
  tcase_add_test(tcase, test_MultiExtension_clone);
  tcase_add_test(tcase, test_MultiExtension_registry);
  tcase_add_test(tcase, test_MultiExtension_typecode);
  tcase_add_test(tcase, test_MultiExtension_SBMLtypecode);
  tcase_add_test(tcase, test_MultiExtension_read);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
