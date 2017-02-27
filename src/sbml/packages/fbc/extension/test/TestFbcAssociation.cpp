/**
 * @file    TestFbcAssociation.cpp
 * @brief   TestFbcAssociation unit tests
 * @author  Akiya Jouraku
 *
 * $Id: $
 * $HeadURL: $
 */

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/packages/fbc/extension/FbcModelPlugin.h>
#include <sbml/packages/fbc/extension/FbcReactionPlugin.h>
#include <sbml/packages/fbc/sbml/FbcAssociation.h>
#include <sbml/packages/fbc/sbml/FbcAnd.h>
#include <sbml/packages/fbc/sbml/FbcOr.h>
#include <sbml/packages/fbc/sbml/GeneProductRef.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/SBMLReader.h>
#include <sbml/SBMLWriter.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART
extern char *TestDataDirectory;

static FbcAssociation* G; 

static bool
equals (const char* expected, const char* actual)
{
  if ( !strcmp(expected, actual) ) return true;

  printf( "\nStrings are not equal:\n"  );
  printf( "Expected:\n[%s]\n", expected );
  printf( "Actual:\n[%s]\n"  , actual   );

  return false;
}

void
FbcAssociationTest_setup (void)
{
  try
  {
    G = new FbcAssociation(3, 1, 2);
  }
  catch(...)
  {
    fail("Failed to create a FbcAssociation object");
  }
}


void
FbcAssociationTest_teardown (void)
{
  delete G;
}


START_TEST(test_FbcAssociation_parseFbcInfixAssociation_product_ref)
{	
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "    <listOfReactants>"
    "    <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "    </listOfReactants>"
    "    </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;
    const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:geneProductRef fbc:geneProduct=\"gp_A\"/>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "    <fbc:listOfGeneProducts>\n"
    "      <fbc:geneProduct fbc:id=\"gp_A\" fbc:label=\"A\"/>\n"
    "    </fbc:listOfGeneProducts>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  std::string infix = "A";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 0);

  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug =  dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless (rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == false);

  GeneProductAssociation* gpa = rplug->createGeneProductAssociation();
  gpa->setId("gg1");
  FbcAssociation * fa = FbcAssociation::parseFbcInfixAssociation(infix, fbc);
  fail_unless(fa->isGeneProductRef() == true);

  gpa->setAssociation(fa);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  fail_unless(fbc->getNumGeneProducts() == 1);

  GeneProduct* gp = fbc->getGeneProduct(0);
  fail_unless (gp->isSetId() == true);
  fail_unless (gp->isSetLabel() == true);
  
  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isGeneProductRef() == true);

  fail_unless(fa_retrieved->toInfix() == infix);
  
  GeneProductRef * gpref = dynamic_cast<GeneProductRef *>(gpa->getAssociation());
  fail_unless(gpref->isSetGeneProduct() == true);

  fail_unless(gpref->getGeneProduct() == gp->getId());

  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete fa;
  delete doc;
}
END_TEST

START_TEST(test_FbcAssociation_parseFbcInfixAssociation_product_ref_existing)
{	
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <fbc:listOfGeneProducts>"
    "        <fbc:geneProduct fbc:id=\"gp_gp\" fbc:label=\"B\"/>"
    "    </fbc:listOfGeneProducts>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "        <listOfReactants>"
    "           <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "        </listOfReactants>"
    "      </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;
    const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:geneProductRef fbc:geneProduct=\"gp_gp\"/>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "    <fbc:listOfGeneProducts>\n"
    "      <fbc:geneProduct fbc:id=\"gp_gp\" fbc:label=\"B\"/>\n"
    "    </fbc:listOfGeneProducts>\n"
    "  </model>\n"
    "</sbml>\n"
    ;
  std::string infix = "B";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 1);

  GeneProduct* gp = fbc->getGeneProduct(0);
  fail_unless (gp->isSetId() == true);
  fail_unless (gp->isSetLabel() == true);

  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug =  dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless (rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == false);

  GeneProductAssociation* gpa = rplug->createGeneProductAssociation();
  gpa->setId("gg1");
  FbcAssociation * fa = FbcAssociation::parseFbcInfixAssociation(infix, fbc);
  fail_unless(fa->isGeneProductRef() == true);

  gpa->setAssociation(fa);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  fail_unless(fbc->getNumGeneProducts() == 1);
   
  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isGeneProductRef() == true);
  
  GeneProductRef * gpref = dynamic_cast<GeneProductRef *>(gpa->getAssociation());
  fail_unless(gpref->isSetGeneProduct() == true);

  fail_unless(gpref->getGeneProduct() == gp->getId());

  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete fa;
  delete doc;
}
END_TEST

START_TEST(test_FbcAssociation_parseFbcInfixAssociation_product_ref_both_existing)
{	
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <fbc:listOfGeneProducts>"
    "        <fbc:geneProduct fbc:id=\"gp_C\" fbc:label=\"C\"/>"
    "    </fbc:listOfGeneProducts>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "        <listOfReactants>"
    "           <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "        </listOfReactants>"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">"
    "            <fbc:geneProductRef fbc:geneProduct=\"gp_C\"/>"
    "        </fbc:geneProductAssociation>"
    "      </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;
    const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:geneProductRef fbc:geneProduct=\"gp_C\"/>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "    <fbc:listOfGeneProducts>\n"
    "      <fbc:geneProduct fbc:id=\"gp_C\" fbc:label=\"C\"/>\n"
    "    </fbc:listOfGeneProducts>\n"
    "  </model>\n"
    "</sbml>\n"
    ;
  std::string infix = "C";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 1);

  GeneProduct* gp = fbc->getGeneProduct(0);
  fail_unless (gp->isSetId() == true);
  fail_unless (gp->isSetLabel() == true);
  
  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug =  dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless (rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == true);

  GeneProductAssociation* gpa = rplug->getGeneProductAssociation();
  fail_unless (gpa != NULL);
  
  FbcAssociation * fa = gpa->getAssociation();
  fail_unless(fa->isGeneProductRef() == true);

  fail_unless(fa->toInfix() == infix);

  //reset the association
  gpa->setAssociation(infix);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  fail_unless(fbc->getNumGeneProducts() == 1);

  gp = fbc->getGeneProduct(0);
  fail_unless (gp->isSetId() == true);
  fail_unless (gp->isSetLabel() == true);
  
  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isGeneProductRef() == true);
  
  GeneProductRef * gpref = dynamic_cast<GeneProductRef *>(gpa->getAssociation());
  fail_unless(gpref->isSetGeneProduct() == true);

  fail_unless(gpref->getGeneProduct() == gp->getId());

  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete doc;
}
END_TEST

START_TEST(test_FbcAssociation_parseFbcInfixAssociation_product_ref_id_label_same)
{	
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <fbc:listOfGeneProducts>"
    "        <fbc:geneProduct fbc:id=\"gp1\" fbc:label=\"gp1\"/>"
    "    </fbc:listOfGeneProducts>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "        <listOfReactants>"
    "           <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "        </listOfReactants>"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">"
    "            <fbc:geneProductRef fbc:geneProduct=\"gp1\"/>"
    "        </fbc:geneProductAssociation>"
    "      </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;

    const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:geneProductRef fbc:geneProduct=\"gp1\"/>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "    <fbc:listOfGeneProducts>\n"
    "      <fbc:geneProduct fbc:id=\"gp1\" fbc:label=\"gp1\"/>\n"
    "    </fbc:listOfGeneProducts>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  std::string infix = "gp1";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 1);

  GeneProduct* gp = fbc->getGeneProduct(0);
  fail_unless (gp->isSetId() == true);
  fail_unless (gp->isSetLabel() == true);
  
  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug =  dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless (rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == true);

  GeneProductAssociation* gpa = rplug->getGeneProductAssociation();
  fail_unless (gpa != NULL);
  
  FbcAssociation * fa = gpa->getAssociation();
  fail_unless(fa->isGeneProductRef() == true);

  fail_unless(fa->toInfix() == infix);

  //reset the association
  gpa->setAssociation(infix);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  fail_unless(fbc->getNumGeneProducts() == 1);

  gp = fbc->getGeneProduct(0);
  fail_unless (gp->isSetId() == true);
  fail_unless (gp->isSetLabel() == true);
  
  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isGeneProductRef() == true);
  
  GeneProductRef * gpref = dynamic_cast<GeneProductRef *>(gpa->getAssociation());
  fail_unless(gpref->isSetGeneProduct() == true);

  fail_unless(gpref->getGeneProduct() == gp->getId());

  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete doc;
}
END_TEST

  START_TEST(test_FbcAssociation_parseFbcInfixAssociation_strange_labels)
{	
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "    <listOfReactants>"
    "    <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "    </listOfReactants>"
    "    </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;
    const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:and>\n"
    "            <fbc:geneProductRef fbc:geneProduct=\"gp_g__DOT__p\"/>\n"
    "            <fbc:geneProductRef fbc:geneProduct=\"gp___ONE__sd\"/>\n"
    "          </fbc:and>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "    <fbc:listOfGeneProducts>\n"
    "      <fbc:geneProduct fbc:id=\"gp_g__DOT__p\" fbc:label=\"g.p\"/>\n"
    "      <fbc:geneProduct fbc:id=\"gp___ONE__sd\" fbc:label=\"1sd\"/>\n"
    "    </fbc:listOfGeneProducts>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  std::string infix = "(g.p and 1sd)";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 0);

  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug =  dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless (rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == false);

  GeneProductAssociation* gpa = rplug->createGeneProductAssociation();
  gpa->setId("gg1");
  FbcAssociation * fa = FbcAssociation::parseFbcInfixAssociation(infix, fbc);
  fail_unless(fa->isFbcAnd() == true);

  gpa->setAssociation(fa);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  fail_unless(fbc->getNumGeneProducts() == 2);

  GeneProduct* gp = fbc->getGeneProduct(0);
  fail_unless (gp->isSetId() == true);
  fail_unless (gp->isSetLabel() == true);
  
  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isFbcAnd() == true);

  std::string out = fa_retrieved->toInfix();

  fail_unless(fa_retrieved->toInfix() == infix);
  
  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete fa;
  delete doc;
}
END_TEST


START_TEST(test_FbcAssociation_parseFbcInfixIdAssociation_product_ref)
{
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "    <listOfReactants>"
    "    <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "    </listOfReactants>"
    "    </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;
  const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:geneProductRef fbc:geneProduct=\"A\"/>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "    <fbc:listOfGeneProducts>\n"
    "      <fbc:geneProduct fbc:id=\"A\" fbc:label=\"A\"/>\n"
    "    </fbc:listOfGeneProducts>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  std::string infix = "A";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 0);

  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug = dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless(rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == false);

  GeneProductAssociation* gpa = rplug->createGeneProductAssociation();
  gpa->setId("gg1");
  FbcAssociation * fa = FbcAssociation::parseFbcInfixAssociation(infix, fbc, true);
  fail_unless(fa->isGeneProductRef() == true);

  gpa->setAssociation(fa);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  fail_unless(fbc->getNumGeneProducts() == 1);

  GeneProduct* gp = fbc->getGeneProduct(0);
  fail_unless(gp->isSetId() == true);
  fail_unless(gp->isSetLabel() == true);

  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isGeneProductRef() == true);

  fail_unless(fa_retrieved->toInfix(true) == infix);

  GeneProductRef * gpref = dynamic_cast<GeneProductRef *>(gpa->getAssociation());
  fail_unless(gpref->isSetGeneProduct() == true);

  fail_unless(gpref->getGeneProduct() == gp->getId());

  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete fa;
  delete doc;
}
END_TEST


START_TEST(test_FbcAssociation_parseFbcInfixIdAssociation_product_ref_existing)
{
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <fbc:listOfGeneProducts>"
    "        <fbc:geneProduct fbc:id=\"gp_gp\" fbc:label=\"B\"/>"
    "    </fbc:listOfGeneProducts>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "        <listOfReactants>"
    "           <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "        </listOfReactants>"
    "      </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;
  const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:geneProductRef fbc:geneProduct=\"gp_gp\"/>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "    <fbc:listOfGeneProducts>\n"
    "      <fbc:geneProduct fbc:id=\"gp_gp\" fbc:label=\"B\"/>\n"
    "    </fbc:listOfGeneProducts>\n"
    "  </model>\n"
    "</sbml>\n"
    ;
  std::string infix = "gp_gp";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 1);

  GeneProduct* gp = fbc->getGeneProduct(0);
  fail_unless(gp->isSetId() == true);
  fail_unless(gp->isSetLabel() == true);

  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug = dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless(rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == false);

  GeneProductAssociation* gpa = rplug->createGeneProductAssociation();
  gpa->setId("gg1");
  FbcAssociation * fa = FbcAssociation::parseFbcInfixAssociation(infix, fbc, true);
  fail_unless(fa->isGeneProductRef() == true);

  gpa->setAssociation(fa);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  fail_unless(fbc->getNumGeneProducts() == 1);

  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isGeneProductRef() == true);
  fail_unless(fa_retrieved->toInfix(true) == infix);

  GeneProductRef * gpref = dynamic_cast<GeneProductRef *>(gpa->getAssociation());
  fail_unless(gpref->isSetGeneProduct() == true);

  fail_unless(gpref->getGeneProduct() == gp->getId());

  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete fa;
  delete doc;
}
END_TEST

START_TEST(test_FbcAssociation_parseFbcInfixIdAssociation_product_ref_both_existing)
{
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <fbc:listOfGeneProducts>"
    "        <fbc:geneProduct fbc:id=\"gp_C\" fbc:label=\"C\"/>"
    "    </fbc:listOfGeneProducts>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "        <listOfReactants>"
    "           <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "        </listOfReactants>"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">"
    "            <fbc:geneProductRef fbc:geneProduct=\"gp_C\"/>"
    "        </fbc:geneProductAssociation>"
    "      </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;
  const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:geneProductRef fbc:geneProduct=\"gp_C\"/>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "    <fbc:listOfGeneProducts>\n"
    "      <fbc:geneProduct fbc:id=\"gp_C\" fbc:label=\"C\"/>\n"
    "    </fbc:listOfGeneProducts>\n"
    "  </model>\n"
    "</sbml>\n"
    ;
  std::string infix = "gp_C";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 1);

  GeneProduct* gp = fbc->getGeneProduct(0);
  fail_unless(gp->isSetId() == true);
  fail_unless(gp->isSetLabel() == true);

  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug = dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless(rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == true);

  GeneProductAssociation* gpa = rplug->getGeneProductAssociation();
  fail_unless(gpa != NULL);

  FbcAssociation * fa = gpa->getAssociation();
  fail_unless(fa->isGeneProductRef() == true);

  fail_unless(fa->toInfix(true) == infix);

  //reset the association
  gpa->setAssociation(infix, true);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  fail_unless(fbc->getNumGeneProducts() == 1);

  gp = fbc->getGeneProduct(0);
  fail_unless(gp->isSetId() == true);
  fail_unless(gp->isSetLabel() == true);

  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isGeneProductRef() == true);

  GeneProductRef * gpref = dynamic_cast<GeneProductRef *>(gpa->getAssociation());
  fail_unless(gpref->isSetGeneProduct() == true);

  fail_unless(gpref->getGeneProduct() == gp->getId());

  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete doc;
}
END_TEST

START_TEST(test_FbcAssociation_parseFbcInfixIdAssociation_product_ref_id_label_same)
{
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <fbc:listOfGeneProducts>"
    "        <fbc:geneProduct fbc:id=\"gp1\" fbc:label=\"gp1\"/>"
    "    </fbc:listOfGeneProducts>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "        <listOfReactants>"
    "           <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "        </listOfReactants>"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">"
    "            <fbc:geneProductRef fbc:geneProduct=\"gp1\"/>"
    "        </fbc:geneProductAssociation>"
    "      </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;

  const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:geneProductRef fbc:geneProduct=\"gp1\"/>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "    <fbc:listOfGeneProducts>\n"
    "      <fbc:geneProduct fbc:id=\"gp1\" fbc:label=\"gp1\"/>\n"
    "    </fbc:listOfGeneProducts>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  std::string infix = "gp1";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 1);

  GeneProduct* gp = fbc->getGeneProduct(0);
  fail_unless(gp->isSetId() == true);
  fail_unless(gp->isSetLabel() == true);

  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug = dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless(rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == true);

  GeneProductAssociation* gpa = rplug->getGeneProductAssociation();
  fail_unless(gpa != NULL);

  FbcAssociation * fa = gpa->getAssociation();
  fail_unless(fa->isGeneProductRef() == true);

  fail_unless(fa->toInfix(true) == infix);

  //reset the association
  gpa->setAssociation(infix, true);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  fail_unless(fbc->getNumGeneProducts() == 1);

  gp = fbc->getGeneProduct(0);
  fail_unless(gp->isSetId() == true);
  fail_unless(gp->isSetLabel() == true);

  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isGeneProductRef() == true);

  GeneProductRef * gpref = dynamic_cast<GeneProductRef *>(gpa->getAssociation());
  fail_unless(gpref->isSetGeneProduct() == true);

  fail_unless(gpref->getGeneProduct() == gp->getId());

  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete doc;
}
END_TEST

START_TEST(test_FbcAssociation_parseFbcInfixIdAssociation_strange_labels)
{
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "    <listOfReactants>"
    "    <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "    </listOfReactants>"
    "    </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;
  const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:and>\n"
    "            <fbc:geneProductRef fbc:geneProduct=\"gp1\"/>\n"
    "            <fbc:geneProductRef fbc:geneProduct=\"gp2\"/>\n"
    "          </fbc:and>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "    <fbc:listOfGeneProducts>\n"
    "      <fbc:geneProduct fbc:id=\"gp1\" fbc:label=\"gp1\"/>\n"
    "      <fbc:geneProduct fbc:id=\"gp2\" fbc:label=\"gp2\"/>\n"
    "    </fbc:listOfGeneProducts>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  std::string infix = "(gp1 and gp2)";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 0);

  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug = dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless(rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == false);

  GeneProductAssociation* gpa = rplug->createGeneProductAssociation();
  gpa->setId("gg1");
  FbcAssociation * fa = FbcAssociation::parseFbcInfixAssociation(infix, fbc, true);
  fail_unless(fa->isFbcAnd() == true);

  gpa->setAssociation(fa);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  fail_unless(fbc->getNumGeneProducts() == 2);

  GeneProduct* gp = fbc->getGeneProduct(0);
  fail_unless(gp->isSetId() == true);
  fail_unless(gp->isSetLabel() == true);

  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isFbcAnd() == true);

  std::string out = fa_retrieved->toInfix();

  fail_unless(fa_retrieved->toInfix(true) == infix);

  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete fa;
  delete doc;
}
END_TEST


START_TEST(test_FbcAssociation_parseFbcInfixAssociation_product_ref_noAdd)
{
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "    <listOfReactants>"
    "    <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "    </listOfReactants>"
    "    </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;
  const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:geneProductRef fbc:geneProduct=\"gp_AA\"/>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  std::string infix = "AA";
  std::string gprefstr = "gp_AA";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 0);

  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug = dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless(rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == false);

  GeneProductAssociation* gpa = rplug->createGeneProductAssociation();
  gpa->setId("gg1");
  FbcAssociation * fa = FbcAssociation::parseFbcInfixAssociation(infix, fbc, false, false);
  fail_unless(fa->isGeneProductRef() == true);

  gpa->setAssociation(fa);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  // check no gene products were added
  fail_unless(fbc->getNumGeneProducts() == 0);

  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isGeneProductRef() == true);

  // here since we have not created the geneProduct we have no access to the
  // label and can only return the peneProductRef field
  fail_unless(fa_retrieved->toInfix() == gprefstr);

  GeneProductRef * gpref = dynamic_cast<GeneProductRef *>(gpa->getAssociation());
  fail_unless(gpref->isSetGeneProduct() == true);

  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete fa;
  delete doc;
}
END_TEST



START_TEST(test_FbcAssociation_parseFbcInfixIdAssociation_product_ref_noAdd)
{
  const char* model1 =
    "<?xml version='1.0' encoding='UTF-8'?>"
    "<sbml xmlns:html='http://www.w3.org/1999/xhtml' xmlns='http://www.sbml.org/sbml/level3/version1/core' xmlns:fbc='http://www.sbml.org/sbml/level3/version1/fbc/version2' level='3' version='1' fbc:required='false'>"
    "  <model id='M' name='E' timeUnits='dimensionless' fbc:strict='false'>"
    "    <listOfCompartments>"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>"
    "    </listOfCompartments>"
    "    <listOfSpecies>"
    "       <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>"
    "    </listOfSpecies>"
    "    <listOfReactions>"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">"
    "    <listOfReactants>"
    "    <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>"
    "    </listOfReactants>"
    "    </reaction>"
    "    </listOfReactions>"
    "  </model>"
    "</sbml>"
    ;
  const char* expected =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns:html=\"http://www.w3.org/1999/xhtml\" xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:fbc=\"http://www.sbml.org/sbml/level3/version1/fbc/version2\" level=\"3\" version=\"1\" fbc:required=\"false\">\n"
    "  <model id=\"M\" name=\"E\" timeUnits=\"dimensionless\" fbc:strict=\"false\">\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"comp1\" spatialDimensions=\"3\" size=\"1\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"S\" compartment=\"comp1\" initialAmount=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\" fbc:charge=\"2\" fbc:chemicalFormula=\"S20\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"R1\" reversible=\"false\" fast=\"false\" fbc:lowerFluxBound=\"low\" fbc:upperFluxBound=\"up\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "        </listOfReactants>\n"
    "        <fbc:geneProductAssociation fbc:id=\"gg1\">\n"
    "          <fbc:geneProductRef fbc:geneProduct=\"BB\"/>\n"
    "        </fbc:geneProductAssociation>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  std::string infix = "BB";

  SBMLDocument* doc = readSBMLFromString(model1);
  fail_unless(doc->getModel() != NULL);
  FbcModelPlugin* fbc = dynamic_cast<FbcModelPlugin*>(doc->getModel()->getPlugin("fbc"));
  fail_unless(fbc != NULL);
  fail_unless(fbc->getNumGeneProducts() == 0);

  Reaction * r = doc->getModel()->getReaction(0);
  fail_unless(r != NULL);
  FbcReactionPlugin * rplug = dynamic_cast<FbcReactionPlugin*>(r->getPlugin("fbc"));
  fail_unless(rplug != NULL);
  fail_unless(rplug->isSetGeneProductAssociation() == false);

  GeneProductAssociation* gpa = rplug->createGeneProductAssociation();
  gpa->setId("gg1");
  FbcAssociation * fa = FbcAssociation::parseFbcInfixAssociation(infix, fbc, true, false);
  fail_unless(fa->isGeneProductRef() == true);

  gpa->setAssociation(fa);
  fail_unless(rplug->isSetGeneProductAssociation() == true);
  // check no gene products were added
  fail_unless(fbc->getNumGeneProducts() == 0);

  FbcAssociation * fa_retrieved = gpa->getAssociation();
  fail_unless(fa_retrieved->isGeneProductRef() == true);

  // here since we have not created the geneProduct we have no access to the
  // label and can only return the peneProductRef field
  fail_unless(fa_retrieved->toInfix(true) == infix);

  GeneProductRef * gpref = dynamic_cast<GeneProductRef *>(gpa->getAssociation());
  fail_unless(gpref->isSetGeneProduct() == true);

  char * char_doc = writeSBMLToString(doc);
  fail_unless(equals(expected, char_doc));

  safe_free((void*)(char_doc));

  delete fa;
  delete doc;
}
END_TEST



Suite *
create_suite_FbcAssociation (void)
{
  Suite *suite = suite_create("FbcAssociation");
  TCase *tcase = tcase_create("FbcAssociation");

  tcase_add_checked_fixture(tcase, FbcAssociationTest_setup, FbcAssociationTest_teardown);
 
  tcase_add_test( tcase, test_FbcAssociation_parseFbcInfixAssociation_product_ref          );
  tcase_add_test( tcase, test_FbcAssociation_parseFbcInfixAssociation_product_ref_existing          );
  tcase_add_test( tcase, test_FbcAssociation_parseFbcInfixAssociation_product_ref_both_existing          );
  tcase_add_test( tcase, test_FbcAssociation_parseFbcInfixAssociation_product_ref_id_label_same          );
  tcase_add_test( tcase, test_FbcAssociation_parseFbcInfixAssociation_strange_labels          );

  tcase_add_test(tcase, test_FbcAssociation_parseFbcInfixIdAssociation_product_ref);
  tcase_add_test( tcase, test_FbcAssociation_parseFbcInfixIdAssociation_product_ref_existing          );
  tcase_add_test( tcase, test_FbcAssociation_parseFbcInfixIdAssociation_product_ref_both_existing          );
  tcase_add_test( tcase, test_FbcAssociation_parseFbcInfixIdAssociation_product_ref_id_label_same          );
  tcase_add_test( tcase, test_FbcAssociation_parseFbcInfixIdAssociation_strange_labels          );
  
  
  tcase_add_test(tcase, test_FbcAssociation_parseFbcInfixAssociation_product_ref_noAdd);
  tcase_add_test(tcase, test_FbcAssociation_parseFbcInfixIdAssociation_product_ref_noAdd);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
