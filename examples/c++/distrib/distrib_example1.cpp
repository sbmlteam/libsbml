/**
 * @file    distrib_example1.cpp
 * @brief   Create a distrib example for the spec
 * @author  Lucian Smith
 *
 * <!--------------------------------------------------------------------------
 * This sample program is distributed under a different license than the rest
 * of libSBML.  This program uses the open-source MIT license, as follows:
 *
 * Copyright (c) 2013-2017 by the California Institute of Technology
 * (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
 * and the University of Heidelberg (Germany), with support from the National
 * Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Neither the name of the California Institute of Technology (Caltech), nor
 * of the European Bioinformatics Institute (EMBL-EBI), nor of the University
 * of Heidelberg, nor the names of any contributors, may be used to endorse
 * or promote products derived from this software without specific prior
 * written permission.
 * ------------------------------------------------------------------------ -->
 */

#include <iostream>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/extension/SBaseExtensionPoint.h>
#include <sbml/extension/SBMLDocumentPlugin.h>
#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionRegistry.h>

#include <sbml/packages/distrib/extension/DistribExtension.h>
#include <sbml/packages/distrib/common/DistribExtensionTypes.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

unsigned int coreVersion = 1;
SBMLDocument * setupBasicModel()
{
  SBMLNamespaces sbmlns(3, coreVersion, "distrib", 1);

  // create the document
  SBMLDocument *document = new SBMLDocument(&sbmlns);

  //Create our submodel
  DistribSBMLDocumentPlugin* distdoc
    = static_cast<DistribSBMLDocumentPlugin*>(document->getPlugin("distrib"));
  distdoc->setRequired(true);
  Model* model = document->createModel();
  return document;
}

InitialAssignment* addParamAndIA(Model* model)
{
  Parameter* param = model->createParameter();
  param->setId("P1");
  param->setConstant(true);
  InitialAssignment* ia = model->createInitialAssignment();
  ia->setSymbol("P1");
  return ia;
}

//void setupArguments(DistribDrawFromDistribution* dfd, InitialAssignment* ia, Model* model, vector<string> arguments)
//{
//  string infix = "distribution(";
//  for (size_t i = 0; i < arguments.size(); i++) {
//    string arg = arguments[i];
//    Parameter* param = model->createParameter();
//    param->setConstant(true);
//    param->setId(arg);
//    DistribInput* di = dfd->createDistribInput();
//    di->setId(arg);
//    di->setIndex(i);
//    if (i > 0) {
//      infix += ", ";
//    }
//    infix += arg;
//  }
//  infix += ")";
//  ASTNode* math = SBML_parseL3Formula(infix.c_str());
//  ia->setMath(math);
//  delete math;
//}

//void addNormal(DistribDrawFromDistribution* dfd)
//{
//  DistribNormalDistribution normal(3, coreVersion, 1);
//  DistribUncertValue* mean = normal.createMean();
//  mean->setVar("mean");
//  DistribUncertValue* stddev = normal.createStddev();
//  stddev->setVar("stddev");
//  dfd->setDistribution(&normal);
//}

//void addNormalWithValues(DistribDrawFromDistribution* dfd)
//{
//  DistribNormalDistribution normal(3, coreVersion, 1);
//  DistribUncertValue* mean = normal.createMean();
//  mean->setValue(5.2);
//  DistribUncertValue* stddev = normal.createStddev();
//  stddev->setValue(1.3);
//  dfd->setDistribution(&normal);
//}

void createExample1()
{
  SBMLDocument* doc = setupBasicModel();
  Model* model = doc->getModel();
  InitialAssignment* ia = addParamAndIA(model);
  ASTNode* astn = SBML_parseL3Formula("normal(5.2,1.3)");
  ia->setMath(astn);
  delete astn;

  writeSBMLToFile(doc, "distrib_example1.xml");
  delete doc;
}

void createExample2()
{
  SBMLDocument* doc = setupBasicModel();
  Model* model = doc->getModel();
  InitialAssignment* ia = addParamAndIA(model);
  Parameter* param = model->createParameter();
  param->setConstant("true");
  param->setId("x");
  param->setValue(5.2);
  param = model->createParameter();
  param->setConstant("true");
  param->setId("y");
  param->setValue(1.3);

  ASTNode* astn = SBML_parseL3Formula("normal(x,y)");
  ia->setMath(astn);
  delete astn;

  writeSBMLToFile(doc, "distrib_example2.xml");
  delete doc;
}

void createDistribUncertaintyExample()
{
  SBMLNamespaces sbmlns(3, coreVersion, "distrib", 1);

  // create the document
  SBMLDocument *document = new SBMLDocument(&sbmlns);
  DistribSBMLDocumentPlugin* distdoc
    = static_cast<DistribSBMLDocumentPlugin*>(document->getPlugin("distrib"));
  distdoc->setRequired(true);
  Model* model = document->createModel();
  Species* s1 = model->createSpecies();
  DistribSBasePlugin* dsbp = static_cast<DistribSBasePlugin*>(s1->getPlugin("distrib"));
  Uncertainty* uncertainty = dsbp->createUncertainty();
  UncertParameter* sdev = uncertainty->createUncertParameter();
  sdev->setType("standardDeviation");
  sdev->setValue(0.3);
  s1->setInitialAmount(3.22);
  s1->setId("s1");
  s1->setCompartment("C");
  s1->setHasOnlySubstanceUnits(true);
  s1->setBoundaryCondition(false);
  s1->setConstant(false);
  Compartment* c = model->createCompartment();
  c->setId("C");
  c->setConstant(true);
  c->setSize(1);
  c->setSpatialDimensions(3.0);

  writeSBMLToFile(document, "distrib_example3.xml");
  delete document;
}

void createDistribUncertaintyExample2()
{
  SBMLNamespaces sbmlns(3, coreVersion, "distrib", 1);

  // create the document
  SBMLDocument *document = new SBMLDocument(&sbmlns);
  DistribSBMLDocumentPlugin* distdoc
    = static_cast<DistribSBMLDocumentPlugin*>(document->getPlugin("distrib"));
  distdoc->setRequired(true);
  Model* model = document->createModel();
  Species* s1 = model->createSpecies();
  DistribSBasePlugin* dsbp = static_cast<DistribSBasePlugin*>(s1->getPlugin("distrib"));
  Uncertainty* uncertainty = dsbp->createUncertainty();
  UncertParameter* sdev = uncertainty->createUncertParameter();
  sdev->setType("standardDeviation");
  sdev->setValue(0.3);
  UncertParameter* mean = uncertainty->createUncertParameter();
  mean->setType("mean");
  mean->setValue(3.2);
  UncertParameter* var = uncertainty->createUncertParameter();
  var->setType("variance");
  var->setValue(0.09);
  s1->setInitialAmount(3.22);
  s1->setId("s1");
  s1->setCompartment("C");
  s1->setHasOnlySubstanceUnits(true);
  s1->setBoundaryCondition(false);
  s1->setConstant(false);
  Compartment* c = model->createCompartment();
  c->setId("C");
  c->setConstant(true);
  c->setSize(1);
  c->setSpatialDimensions(3.0);

  writeSBMLToFile(document, "distrib_example4.xml");
  delete document;
}

void createDistribUncertaintyExample3()
{
  SBMLNamespaces sbmlns(3, coreVersion, "distrib", 1);

  // create the document
  SBMLDocument *document = new SBMLDocument(&sbmlns);
  DistribSBMLDocumentPlugin* distdoc
    = static_cast<DistribSBMLDocumentPlugin*>(document->getPlugin("distrib"));
  distdoc->setRequired(true);
  Model* model = document->createModel();
  Parameter* p1 = model->createParameter();
  p1->setId("mu_Z");
  p1->setValue(10);
  p1->setConstant(true);
  p1 = model->createParameter();
  p1->setId("var_Z");
  p1->setValue(0.1);
  p1->setConstant(true);
  p1 = model->createParameter();
  p1->setId("Z");
  p1->setConstant(true);
  DistribSBasePlugin* dsbp = static_cast<DistribSBasePlugin*>(p1->getPlugin("distrib"));
  Uncertainty* uncertainty = dsbp->createUncertainty();
  UncertParameter* dist = uncertainty->createUncertParameter();
  dist->setType("distribution");
  UncertParameter* mean = dist->createUncertParameter();
  mean->setType("standardDeviation");
  mean->setVar("mu_Z");
  UncertParameter* var = dist->createUncertParameter();
  var->setType("variance");
  var->setVar("var_Z");
  ASTNode* astn = SBML_parseL3Formula("normal(mu_Z, var_Z)");
  dist->setMath(astn);
  delete astn;

  writeSBMLToFile(document, "distrib_example5.xml");
  delete document;
}

void createDistribUncertaintyExample4()
{
  SBMLNamespaces sbmlns(3, coreVersion, "distrib", 1);

  // create the document
  SBMLDocument *document = new SBMLDocument(&sbmlns);
  DistribSBMLDocumentPlugin* distdoc
    = static_cast<DistribSBMLDocumentPlugin*>(document->getPlugin("distrib"));
  distdoc->setRequired(true);
  Model* model = document->createModel();
  Species* s1 = model->createSpecies();
  DistribSBasePlugin* dsbp = static_cast<DistribSBasePlugin*>(s1->getPlugin("distrib"));
  Uncertainty* uncertainty = dsbp->createUncertainty();
  UncertParameter* dist = uncertainty->createUncertParameter();
  dist->setType("distribution");
  dist->setName("CategoricalDistribution");
  dist->setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000248"); // Categorical Nonordered
  UncertParameter* p1 = dist->createUncertParameter();
  p1->setType("externalParameter");
  p1->setId("patient1");
  UncertParameter* pp = p1->createUncertParameter();
  pp->setType("externalParameter");
  pp->setVar("probability");
  pp->setValue(0.5);
  pp = p1->createUncertParameter();
  pp->setType("externalParameter");
  pp->setVar("value");
  pp->setValue(1.01);
  p1 = dist->createUncertParameter();
  p1->setType("externalParameter");
  p1->setId("patient2");
  pp = p1->createUncertParameter();
  pp->setType("externalParameter");
  pp->setVar("probability");
  pp->setValue(0.25);
  pp = p1->createUncertParameter();
  pp->setType("externalParameter");
  pp->setVar("value");
  pp->setValue(2.24);  
  p1 = dist->createUncertParameter();
  p1->setType("externalParameter");
  p1->setId("patient3");
  pp = p1->createUncertParameter();
  pp->setType("externalParameter");
  pp->setVar("value");
  pp->setValue(1.72);  

  s1->setId("s1");
  s1->setCompartment("C");
  s1->setHasOnlySubstanceUnits(true);
  s1->setBoundaryCondition(false);
  s1->setConstant(false);
  Compartment* c = model->createCompartment();
  c->setId("C");
  c->setConstant(true);
  c->setSize(1);
  c->setSpatialDimensions(3.0);

  writeSBMLToFile(document, "distrib_example6.xml");
  delete document;
}

//void createUncertainGender()
//{
//  SBMLNamespaces sbmlns(3, coreVersion, "distrib", 1);
//
//  // create the document
//  SBMLDocument *document = new SBMLDocument(&sbmlns);
//  Model* model = document->createModel();
//  DistribSBMLDocumentPlugin* distdoc
//    = static_cast<DistribSBMLDocumentPlugin*>(document->getPlugin("distrib"));
//  distdoc->setRequired(true);
//  Parameter* p1 = model->createParameter();
//  p1->setId("gender");
//  p1->setConstant(false);
//  DistribSBasePlugin* dsbp = static_cast<DistribSBasePlugin*>(p1->getPlugin("distrib"));
//  Uncertainty* uncertainty = dsbp->createUncertainty();
//  Distribution* dist = uncertainty->createDistribution();
//  dist->setName("CategoricalDistribution");
////  dist->setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000248"); // Categorical Nonordered
//  ExternalParameter* p = uncertainty->createExternalParameter();
//  p->setId("male");
//  p->setDefinitionURL("http://purl.obolibrary.org/obo/OBI_0001930"); // categorical value specification
//  ExternalParameter * pp = ((ExternalParameter*)(p))->createExternalParameter();
//  pp->setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000255"); //category probabilities of Categorical-Nonordered-1
//  pp->setVar("probability");
//  pp->setValue(0.5);
//  pp = ((ExternalParameter*)(p))->createExternalParameter();
//  pp->setVar("value");
//  pp->setValue(0);  
//  pp->setDefinitionURL("http://www.probonto.org/ontology#PROB_c0000019"); //random variable
//  p = uncertainty->createExternalParameter();
//  p->setId("female");
//  p->setDefinitionURL("http://purl.obolibrary.org/obo/OBI_0001930"); // categorical value specification
//  pp = ((ExternalParameter*)(p))->createExternalParameter();
//  pp->setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000255"); //category probabilities of Categorical-Nonordered-1
//  pp->setVar("probability");
//  pp->setValue(0.5);
//  pp = ((ExternalParameter*)(p))->createExternalParameter();
//  pp->setVar("value");
//  pp->setValue(1);
//  pp->setDefinitionURL("http://www.probonto.org/ontology#PROB_c0000019"); //random variable
//
//  writeSBMLToFile(document, "distrib_example7.xml");
//  delete document;
//}

void createPkPd()
{
  SBMLNamespaces sbmlns(3, coreVersion, "distrib", 1);
  SBMLDocument * doc = setupBasicModel();
  Model * model = doc->getModel();

  Compartment comp(&sbmlns);
  comp.setId("central");
  comp.setSize(0);
  comp.setConstant(true);
  model->addCompartment(&comp);
  comp.setId("gut");
  model->addCompartment(&comp);

  Species s(&sbmlns);
  s.setId("Qc");
  s.setCompartment("central");
  s.setInitialAmount(1);
  s.setHasOnlySubstanceUnits(true);
  s.setBoundaryCondition(false);
  s.setConstant(false);
  model->addSpecies(&s);
  s.setId("Qg");
  s.setCompartment("gut");
  model->addSpecies(&s);

  Parameter p(&sbmlns);
  p.setId("ka");
  p.setConstant(true);
  model->addParameter(&p);
  p.setId("ke");
  model->addParameter(&p);
  p.setId("Cc");
  p.setConstant(false);
  model->addParameter(&p);
  p.setId("Cc_obs");
  model->addParameter(&p);

  InitialAssignment ia(&sbmlns);
  ia.setSymbol("central");
  ASTNode* astn = SBML_parseL3Formula("lognormal(0.5, 0.1)");
  ia.setMath(astn);
  model->addInitialAssignment(&ia);
  ia.setSymbol("ka");
  model->addInitialAssignment(&ia);
  ia.setSymbol("ke");
  model->addInitialAssignment(&ia);

  AssignmentRule ar(&sbmlns);
  ar.setVariable("Cc");
  delete astn;
  astn = SBML_parseL3Formula("Qc/central");
  ar.setMath(astn);
  model->addRule(&ar);
  ar.setVariable("Cc_obs");
  delete astn;
  astn = SBML_parseL3Formula("Cc + 1");
  ar.setMath(astn);
  model->addRule(&ar);

  Reaction* rxn = model->createReaction();
  rxn->setId("absorption");
  rxn->setReversible(false);
  rxn->setFast(false);
  SpeciesReference* sr = rxn->createReactant();
  sr->setSpecies("Qg");
  sr->setStoichiometry(1);
  sr->setConstant(true);
  sr = rxn->createProduct();
  sr->setSpecies("Qc");
  sr->setStoichiometry(1);
  sr->setConstant(true);
  delete astn;
  astn = SBML_parseL3Formula("ka*Qg");
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(astn);

  rxn = model->createReaction();
  rxn->setId("excretion");
  rxn->setReversible(false);
  rxn->setFast(false);
  sr = rxn->createReactant();
  sr->setSpecies("Qc");
  sr->setStoichiometry(1);
  sr->setConstant(true);
  delete astn;
  astn = SBML_parseL3Formula("(ke*Qc)/central");
  kl = rxn->createKineticLaw();
  kl->setMath(astn);

  writeSBMLToFile(doc, "pkpd.xml");
  delete doc;
}

//void createExternalExponential()
//{
//  SBMLNamespaces sbmlns(3, coreVersion, "distrib", 1);
//
//  // create the document
//  SBMLDocument *document = new SBMLDocument(&sbmlns);
//  DistribSBMLDocumentPlugin* distdoc
//    = static_cast<DistribSBMLDocumentPlugin*>(document->getPlugin("distrib"));
//  distdoc->setRequired(true);
//  Model* model = document->createModel();
//  FunctionDefinition* fd = model->createFunctionDefinition();
//  fd->setId("Exponential2");
//  DistribFunctionDefinitionPlugin* dfdp = static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));
//  DistribDrawFromDistribution* dfd = dfdp->createDistribDrawFromDistribution();
//  DistribInput* input = dfd->createDistribInput();
//  input->setId("beta");
//  input->setIndex(0);
//  DistribExternalDistribution ext(3, coreVersion, 1);
//  ext.setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000353");
//  ext.setName("Exponential 2");
//  DistribExternalParameter* param = ext.createDistribExternalParameter();
//  param->setDefinitionURL("http://www.probonto.org/ontology#PROB_k0000362");
//  param->setName("Beta");
//  param->setVar("beta");
//  dfd->setDistribution(&ext);
//
//  writeSBMLToFile(document, "distrib_example8.xml");
//  delete document;
//}

//void createUserDefined()
//{
//  DistribDrawFromDistribution* dfd = setupBasicModel();
//  SBMLDocument* doc = dfd->getSBMLDocument();
//  Model* model = doc->getModel();
//  InitialAssignment* ia = addParamAndIA(model);
//  addNormal(dfd);
//
//  vector<string> arguments;
//  arguments.push_back("mean");
//  arguments.push_back("stddev");
//  setupArguments(dfd, ia, model, arguments);
//  Parameter* param = model->createParameter();
//  param->setConstant(true);
//  param->setId("V_pop");
//  param->setValue(100);
//  param = model->createParameter();
//  param->setConstant(true);
//  param->setId("V_omega");
//  param->setValue(0.25);
//  ASTNode* astn = SBML_parseL3Formula("normal(V_pop, V_omega)");
//  ia->setMath(astn);
//  param = model->getParameter("P1");
//  param->setId("V");
//  ia->setSymbol("V");
//  delete astn;
//  DistribSBasePlugin* dsbp = static_cast<DistribSBasePlugin*>(ia->getPlugin("distrib"));
//  DistribUncertainty* uncert = dsbp->createDistribUncertainty();
//  DistribUncertStatistics* stats = uncert->createUncertStatistics();
//  DistribUncertValue uv(3, coreVersion, 1);
//  uv.setVar("V_pop");
//  stats->setMean(&uv);
//  uv.setVar("V_omega");
//  stats->setStandardDeviation(&uv);
//
//  dsbp = static_cast<DistribSBasePlugin*>(param->getPlugin("distrib"));
//  dsbp->setDistribUncertainty(uncert);
//
//  model->removeParameter("mean");
//  model->removeParameter("stddev");
//  FunctionDefinition* fd = model->getFunctionDefinition(0);
//  fd->setId("normal");
//  if (coreVersion == 1)
//  {
//    ASTNode* math = SBML_parseL3Formula("lambda(x, y, notanumber)");
//    fd->setMath(math);
//    delete math;
//  }
//  else
//  {
//    fd->setMath(NULL);
//  }
//  writeSBMLToFile(doc, "user-defined.xml");
//  delete doc;
//}

void createConfidenceIntervalEx()
{
  SBMLNamespaces sbmlns(3, coreVersion, "distrib", 1);
  // create the document
  SBMLDocument *document = new SBMLDocument(&sbmlns);
  DistribSBMLDocumentPlugin* distdoc
    = static_cast<DistribSBMLDocumentPlugin*>(document->getPlugin("distrib"));
  distdoc->setRequired(true);
  Model* model = document->createModel();
  Parameter param(&sbmlns);
  param.setConstant(true);
  DistribSBasePlugin* dsbp = static_cast<DistribSBasePlugin*>(param.getPlugin("distrib"));
  Uncertainty* uncert = dsbp->createUncertainty();
  UncertSpan* span = uncert->createUncertSpan();
  param.setId("P1");
  param.setValue(5.13);
  span->setType("confidenceInterval");
  span->setValueLower(5.0);
  span->setValueUpper(5.32);
  model->addParameter(&param);

  param.setId("P2");
  param.setValue(15.0);
  span->setType("confidenceInterval");
  span->setValueLower(10.22);
  span->setValueUpper(15.02);
  model->addParameter(&param);

  param.setId("P3");
  param.setValue(0.003);
  span->setType("confidenceInterval");
  span->setValueLower(-0.001);
  span->setValueUpper(0.0041);
  model->addParameter(&param);

  param.setId("P4");
  param.setValue(.34);
  span->setType("confidenceInterval");
  span->setValueLower(0.22);
  span->setValueUpper(0.51);
  model->addParameter(&param);

  param.setId("P5");
  param.setValue(92);
  span->setType("confidenceInterval");
  span->setValueLower(90);
  span->setValueUpper(99);
  model->addParameter(&param);


  Compartment* c = model->createCompartment();
  c->setId("C");
  c->setConstant(true);
  c->setSize(1);
  c->setSpatialDimensions(3.0);


  writeSBMLToFile(document, "confidence-intervals.xml");
  delete document;
}



int
main (int argc, char* argv[])
{
  coreVersion = 1;
  createExample1();
  createExample2();
  createDistribUncertaintyExample();
  createDistribUncertaintyExample2();
  createDistribUncertaintyExample3();
  createDistribUncertaintyExample4();
  //createUncertainGender();
  createPkPd();
  //createExternalExponential();
  //createUserDefined();
  createConfidenceIntervalEx();
}
