/**
 * @file    TestMultiExtension.cpp
 * @brief   TestMultiExtension unit tests
 * @author  Akiya Jouraku
 *
 * $Id: $
 * $HeadURL: $
 */

#include <sbml/common/common.h>

#include <sbml/packages/multi/extension/MultiSBMLDocumentPlugin.h>
#include <sbml/packages/multi/extension/MultiASTPlugin.h>
#include <sbml/packages/multi/extension/MultiCompartmentPlugin.h>
#include <sbml/conversion/SBMLConverterRegistry.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>
#include <sbml/packages/multi/validator/MultiSBMLErrorTable.h>

#include <string>

#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static bool
equals(const char* expected, const char* actual)
{
  if (!strcmp(expected, actual)) return true;

  printf("\nStrings are not equal:\n");
  printf("Expected:\n[%s]\n", expected);
  printf("Actual:\n[%s]\n", actual);

  return false;
}



extern char *TestDataDirectory;

START_TEST (test_multi_read_ci)
{
  string filename(TestDataDirectory);
  string cfile = filename + "multi_ci_extension.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  fail_unless(doc != NULL);
  Model* model = doc->getModel();
  fail_unless(model != NULL);

  const ASTNode* math = 
    doc->getModel()->getReaction(0)->getKineticLaw()->getMath();

  fail_unless(math != NULL);

  ASTNode* ci_element1 = math->getChild(0);

  fail_unless(ci_element1 != NULL);
  fail_unless(ci_element1->getPlugin("multi") != NULL);

  const MultiASTPlugin* plug1 = static_cast<const MultiASTPlugin*>(
                                       ci_element1->getPlugin("multi"));

  fail_unless(plug1->isSetSpeciesReference());
  fail_unless(plug1->getSpeciesReference() == "r1");
  fail_unless(plug1->isSetRepresentationType() == false);
  fail_unless(plug1->getRepresentationType() == "");
  
  ASTNode* ci_element2 = math->getChild(1);

  fail_unless(ci_element2 != NULL);
  fail_unless(ci_element2->getPlugin("multi") != NULL);

  const MultiASTPlugin* plug2 = static_cast<const MultiASTPlugin*>(
                                       ci_element2->getPlugin("multi"));

  fail_unless(plug2->isSetSpeciesReference() == false);
  fail_unless(plug2->getSpeciesReference() == "");
  fail_unless(plug2->isSetRepresentationType());
  fail_unless(plug2->getRepresentationType() == "sum");
  
  delete doc;
}
END_TEST


START_TEST (test_multi_write_ci)
{
  string filename(TestDataDirectory);
  string cfile = filename + "multi_ci_extension.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  fail_unless(doc != NULL);

  string docStr = writeSBMLToStdString(doc);

  SBMLDocument* docfromString = readSBMLFromString(docStr.c_str());

  // check read and write produced same string
  string secondDocStr = writeSBMLToStdString(docfromString);
  fail_unless(docStr == secondDocStr);

  fail_unless(docfromString != NULL);
  Model* model = docfromString->getModel();
  fail_unless(model != NULL);

  const ASTNode* math = 
    docfromString->getModel()->getReaction(0)->getKineticLaw()->getMath();

  fail_unless(math != NULL);

  ASTNode* ci_element1 = math->getChild(0);

  fail_unless(ci_element1 != NULL);
  fail_unless(ci_element1->getPlugin("multi") != NULL);

  const MultiASTPlugin* plug1 = static_cast<const MultiASTPlugin*>(
                                       ci_element1->getPlugin("multi"));

  fail_unless(plug1->isSetSpeciesReference());
  fail_unless(plug1->getSpeciesReference() == "r1");
  fail_unless(plug1->isSetRepresentationType() == false);
  fail_unless(plug1->getRepresentationType() == "");

  ASTNode* ci_element2 = math->getChild(1);

  fail_unless(ci_element2 != NULL);
  fail_unless(ci_element2->getPlugin("multi") != NULL);

  const MultiASTPlugin* plug2 = static_cast<const MultiASTPlugin*>(
                                       ci_element2->getPlugin("multi"));

  fail_unless(plug2->isSetSpeciesReference() == false);
  fail_unless(plug2->getSpeciesReference() == "");
  fail_unless(plug2->isSetRepresentationType());
  fail_unless(plug2->getRepresentationType() == "sum");
  
  delete docfromString;
  delete doc;
}
END_TEST


START_TEST (test_multi_create_ci)
{
  string filename(TestDataDirectory);
  string cfile = filename + "multi_ci_extension.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());
  std::string origDoc = writeSBMLToStdString(doc);

  SBMLNamespaces sbmlns(3,1,"multi",1);

  // create the document

  SBMLDocument *document = new SBMLDocument(&sbmlns);

  // set the required attribute to true
  MultiSBMLDocumentPlugin * docPlug = 
    static_cast<MultiSBMLDocumentPlugin*>(document->getPlugin("multi"));
  docPlug->setRequired(true);


  // create the Model

  Model* model=document->createModel();

  // create a  compartment

  Compartment * c = model->createCompartment();
  c->setId("membrane");
  c->setConstant(true);

  // set the multi attribute isType via the compartmentPlugin
  MultiCompartmentPlugin * compPlug = 
    static_cast<MultiCompartmentPlugin*>(c->getPlugin("multi"));
  compPlug->setIsType(true);

  // create species
  Species *s = model->createSpecies();
  s->setId("s1");
  s->setCompartment("membrane");
  s->setBoundaryCondition(false);
  s->setHasOnlySubstanceUnits(false);
  s->setConstant(false);

  // create reaction
  Reaction *r = model->createReaction();
  r->setId("r1");
  r->setFast(false);
  r->setReversible(false);

  // createReactant
  SpeciesReference *sr = r->createReactant();
  sr->setId("sr1");
  sr->setSpecies("s1");
  sr->setConstant(false);

  KineticLaw *kl = r->createKineticLaw();

  ASTNode * ci = new ASTNode(AST_NAME);
  ci->setName("s1");
  MultiASTPlugin * astPlugin = static_cast<MultiASTPlugin*>(ci->getPlugin("multi"));
  astPlugin->setSpeciesReference("r1");

  ASTNode * ci1 = new ASTNode(AST_NAME);
  MultiASTPlugin * astPlugin1 = static_cast<MultiASTPlugin*>(ci1->getPlugin("multi"));
  astPlugin1->setRepresentationType("sum");
  ci1->setName("s1");

  ASTNode *math = new ASTNode(AST_TIMES);
  math->addChild(ci);
  math->addChild(ci1);

  // need to add multi ns
  XMLNamespaces* xmlns = document->getSBMLNamespaces()->getNamespaces();
  math->setDeclaredNamespaces(xmlns);

  kl->setMath(math);
  delete math;

  std::string newModel = writeSBMLToStdString(document);

  fail_unless(origDoc == newModel);
 
  delete document;
  
  delete doc;
}
END_TEST

START_TEST(test_multi_ast_plugin)
{
  SBMLNamespaces sbmlns(3, 1, "multi", 1);

  // create the document

  SBMLDocument *document = new SBMLDocument(&sbmlns);

  // set the required attribute to true
  MultiSBMLDocumentPlugin * docPlug =
    static_cast<MultiSBMLDocumentPlugin*>(document->getPlugin("multi"));
  docPlug->setRequired(true);


  // create the Model

  Model* model = document->createModel();

  // create a  compartment

  Compartment * c = model->createCompartment();
  c->setId("membrane");
  c->setConstant(true);

  // set the multi attribute isType via the compartmentPlugin
  MultiCompartmentPlugin * compPlug =
    static_cast<MultiCompartmentPlugin*>(c->getPlugin("multi"));
  compPlug->setIsType(true);

  // create species
  Species *s = model->createSpecies();
  s->setId("s1");
  s->setCompartment("membrane");
  s->setBoundaryCondition(false);
  s->setHasOnlySubstanceUnits(false);
  s->setConstant(false);

  // create reaction
  Reaction *r = model->createReaction();
  r->setId("r1");
  r->setFast(false);
  r->setReversible(false);

  // createReactant
  SpeciesReference *sr = r->createReactant();
  sr->setId("sr1");
  sr->setSpecies("s1");
  sr->setConstant(false);

  KineticLaw *kl = r->createKineticLaw();

  ASTNode * ci = new ASTNode(AST_NAME);
  ci->setName("s1");
  MultiASTPlugin * astPlugin = static_cast<MultiASTPlugin*>(ci->getPlugin("multi"));
  astPlugin->setSpeciesReference("r1");

    fail_unless(ci->getNumPlugins() == 1);


  ASTNode * ci1 = new ASTNode(AST_NAME);
  MultiASTPlugin * astPlugin1 = static_cast<MultiASTPlugin*>(ci1->getPlugin("multi"));
  astPlugin1->setRepresentationType("sum");
  ci1->setName("s1");

  fail_unless(ci1->getNumPlugins() == 1);


  ASTNode *math = new ASTNode(AST_TIMES);
  math->addChild(ci);
  math->addChild(ci1);

  fail_unless(math->getNumPlugins() == 0);

  kl->setMath(math);
  delete math;

  AssignmentRule *ar = model->createAssignmentRule();
  ar->setVariable("p");

  ASTNode * ci2 = new ASTNode(AST_NAME);
  ci2->setName("s1");
 
  fail_unless(ci2->getNumPlugins() == 0);

  ASTNode * ci3 = new ASTNode(AST_NAME);
  ci3->setName("s1");

  fail_unless(ci3->getNumPlugins() == 0);

  ASTNode *math1 = new ASTNode(AST_TIMES);
  math1->addChild(ci2);
  math1->addChild(ci3);

  fail_unless(math1->getNumPlugins() == 0);

  ar->setMath(math1);
  delete math1;

  // check plugins when we retrieve the math

  const ASTNode* node = model->getRule(0)->getMath();
  fail_unless(node->getNumPlugins() == 0);
  fail_unless(node->getNumChildren() == 2);
  fail_unless(node->getChild(0)->getNumPlugins() == 0);
  fail_unless(node->getChild(1)->getNumPlugins() == 0);

  const ASTNode* node1 = model->getReaction(0)->getKineticLaw()->getMath();
  fail_unless(node1->getNumPlugins() == 0);
  fail_unless(node1->getNumChildren() == 2);
  fail_unless(node1->getChild(0)->getNumPlugins() == 1);
  fail_unless(node1->getChild(1)->getNumPlugins() == 1);

  delete document;
}
END_TEST


START_TEST(test_multi_math_ns)
{
  const char * expected = "<kineticLaw>\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\" xmlns:multi=\"http://www.sbml.org/sbml/level3/version1/multi/version1\">\n"
    "    <apply>\n"
    "      <times/>\n"
    "      <ci multi:speciesReference=\"r1\"> s1 </ci>\n"
    "      <ci multi:representationType=\"sum\"> s1 </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</kineticLaw>";
    
    string filename(TestDataDirectory);
  string cfile = filename + "multi_ci_extension.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());
  KineticLaw* kl = doc->getModel()->getReaction(0)->getKineticLaw();
  fail_unless(equals(expected, kl->toSBML()));
}
END_TEST


Suite *
create_suite_MultiAST(void)
{
  TCase *tcase = tcase_create("MultiAST");
  Suite *suite = suite_create("MultiAST");

  tcase_add_test(tcase, test_multi_read_ci);
  tcase_add_test(tcase, test_multi_write_ci);
  tcase_add_test(tcase, test_multi_create_ci);
  tcase_add_test(tcase, test_multi_ast_plugin);
  tcase_add_test(tcase, test_multi_math_ns);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

