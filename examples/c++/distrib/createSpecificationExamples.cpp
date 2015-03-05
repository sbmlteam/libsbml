/**
 * @file    createSpecificationExamples.cpp
 * @brief   SBML distrib example the produces the models found in the spec.
 * @author  Lucian Smith
 *
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */

#include <sbml/SBMLTypes.h>
#include <sbml/packages/distrib/common/DistribExtensionTypes.h>
#include <string>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

SBMLDocument* createDistribDoc()
{
  SBMLNamespaces sbmlns(3,1,"distrib",1);

  // create the document
  SBMLDocument *document = new SBMLDocument(&sbmlns);

  // set the required attribute to true
  DistribSBMLDocumentPlugin * docPlug = 
    static_cast<DistribSBMLDocumentPlugin*>(document->getPlugin("distrib"));
  docPlug->setRequired(true);


  // create the Model
  Model* model=document->createModel();

  return document;
}
  
string UIntToString(unsigned int in)
{
  stringstream out;
  out << in;
  return out.str();
}

void createExample1()
{
  SBMLDocument* ex1 = createDistribDoc();
  Model* mod = ex1->getModel();
  FunctionDefinition* fd = mod->createFunctionDefinition();
  fd->setId("normal");

  ASTNode * math = SBML_parseFormula("lambda(mean, s, mean)");
  fd->setMath(math);
  delete math;
  //
  // Get a DistribFunctionDefinitionPlugin object plugged in the fd object.
  //
  // The type of the returned value of SBase::getPlugin() 
  // function is SBasePlugin*, and
  // thus the value needs to be casted for the corresponding derived class. 
  //
  DistribFunctionDefinitionPlugin* fdPlugin = 
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  // create a DrawFromDistribution object
  DrawFromDistribution * draw = fdPlugin->createDrawFromDistribution();

  // create the distribInputs
  DistribInput * input1 = draw->createDistribInput();
  input1->setId("avg");
  input1->setIndex(0);

  DistribInput * input2 = draw->createDistribInput();
  input2->setId("sd");
  input2->setIndex(1);

  // create the UncertMLNode object
  UncertMLNode * uncert = UncertMLNode::createDistributionNode
                          ("NormalDistribution", "mean, stddev", "avg,sd");

  draw->setUncertML(uncert);

  Parameter* param = mod->createParameter();
  param->setId("y");
  param = mod->createParameter();
  param->setId("z");
  param->setValue(40);

  InitialAssignment* ia = mod->createInitialAssignment();
  ia->setSymbol("y");
  math = SBML_parseFormula("normal(z,10)");
  ia->setMath(math);
  delete math;

  ex1->checkConsistency();
  writeSBML(ex1,"distrib_spec1.xml");
 
  delete ex1;
}

void createExample2()
{
  SBMLDocument* ex1 = createDistribDoc();
  Model* mod = ex1->getModel();
  FunctionDefinition* fd = mod->createFunctionDefinition();
  fd->setId("rolld4");

  ASTNode * math = SBML_parseFormula("lambda(NaN)");
  fd->setMath(math);
  delete math;
  //
  // Get a DistribFunctionDefinitionPlugin object plugged in the fd object.
  //
  // The type of the returned value of SBase::getPlugin() 
  // function is SBasePlugin*, and
  // thus the value needs to be casted for the corresponding derived class. 
  //
  DistribFunctionDefinitionPlugin* fdPlugin = 
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  // create a DrawFromDistribution object
  DrawFromDistribution * draw = fdPlugin->createDrawFromDistribution();

  // create the UncertMLNode object
  UncertMLNode * uncert = UncertMLNode::createDistributionNode
                          ("CategoricalDistribution", "", "");
  for (unsigned int i = 0; i < 4; i++)
  {
    UncertMLNode * catprob = new UncertMLNode();
    catprob->setElementName("categoryProb");

    UncertMLNode* prob = new UncertMLNode();
    prob->setElementName("prob");
    UncertMLNode* var = new UncertMLNode();
    var->setText("0.25");
    prob->addChild(var);
    catprob->addChild(prob);

    UncertMLNode* name = new UncertMLNode();
    name->setText( UIntToString(i+1));
    catprob->addChild(name);

    uncert->addChild(catprob);
  }
  draw->setUncertML(uncert);

  Parameter* param = mod->createParameter();
  param->setId("y");

  InitialAssignment* ia = mod->createInitialAssignment();
  ia->setSymbol("y");
  math = SBML_parseFormula("rolld4()");
  ia->setMath(math);
  delete math;

  ex1->checkConsistency();
  writeSBML(ex1,"distrib_spec2.xml");
 
  delete ex1;
}


void createExample3()
{
  SBMLDocument* ex1 = createDistribDoc();
  Model* mod = ex1->getModel();
  FunctionDefinition* fd = mod->createFunctionDefinition();
  fd->setId("pickone");

  ASTNode * math = SBML_parseFormula("lambda(A, B, C, D, NaN)");
  fd->setMath(math);
  delete math;
  //
  // Get a DistribFunctionDefinitionPlugin object plugged in the fd object.
  //
  // The type of the returned value of SBase::getPlugin() 
  // function is SBasePlugin*, and
  // thus the value needs to be casted for the corresponding derived class. 
  //
  DistribFunctionDefinitionPlugin* fdPlugin = 
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  // create a DrawFromDistribution object
  DrawFromDistribution * draw = fdPlugin->createDrawFromDistribution();

  // create the distribInputs
  DistribInput * input = draw->createDistribInput();
  input->setId("A");
  input->setIndex(0);

  input = draw->createDistribInput();
  input->setId("B");
  input->setIndex(1);

  input = draw->createDistribInput();
  input->setId("C");
  input->setIndex(2);

  input = draw->createDistribInput();
  input->setId("D");
  input->setIndex(3);

  // create the UncertMLNode object
  vector<string> names;
  names.push_back("A");
  names.push_back("B");
  names.push_back("C");
  names.push_back("D");
  UncertMLNode * uncert = UncertMLNode::createDistributionNode
                          ("CategoricalDistribution", "", "");
  for (unsigned int i = 0; i < 4; i++)
  {
    UncertMLNode * catprob = new UncertMLNode();
    catprob->setElementName("categoryProb");

    UncertMLNode* prob = new UncertMLNode();
    prob->setElementName("prob");
    UncertMLNode* var = new UncertMLNode();
    var->setText("0.25");
    prob->addChild(var);
    catprob->addChild(prob);

    UncertMLNode* name = new UncertMLNode();
    name->setText(names[i]);
    catprob->addChild(name);

    uncert->addChild(catprob);
  }
  draw->setUncertML(uncert);

  Parameter* param = mod->createParameter();
  param->setId("y");

  InitialAssignment* ia = mod->createInitialAssignment();
  ia->setSymbol("y");
  math = SBML_parseFormula("pickone(1, 3, 4, 7)");
  ia->setMath(math);
  delete math;

  ex1->checkConsistency();
  writeSBML(ex1,"distrib_spec3.xml");
 
  delete ex1;
}

void createExample4()
{
  SBMLDocument* ex1 = createDistribDoc();
  Model* mod = ex1->getModel();
  Compartment* c = mod->createCompartment();
  c->setId("C");
  c->setSpatialDimensions(3.0);
  c->setConstant(true);
  c->setSize(1);

  Species* s = mod->createSpecies();
  s->setId("S1");
  s->setHasOnlySubstanceUnits(false);
  s->setInitialAmount(3.22);
  s->setCompartment("C");
  s->setBoundaryCondition(false);
  s->setConstant(false);

  DistribSBasePlugin* distrib = static_cast<DistribSBasePlugin*>(s->getPlugin("distrib"));
  Uncertainty* uncert = distrib->createUncertainty();

  UncertMLNode * stddev = UncertMLNode::createDistributionNodeWithValues
                         ("StandardDeviation", "value", "0.3");

  uncert->setUncertML(stddev);

  ex1->checkConsistency();
  writeSBML(ex1,"distrib_spec4.xml");
 
  delete ex1;
}


void createExample5()
{
  SBMLDocument* ex1 = createDistribDoc();
  Model* mod = ex1->getModel();
  Compartment* c = mod->createCompartment();
  c->setId("C");
  c->setSpatialDimensions(3.0);
  c->setConstant(true);
  c->setSize(1);

  Species* s = mod->createSpecies();
  s->setId("S1");
  s->setHasOnlySubstanceUnits(false);
  s->setInitialAmount(3.22);
  s->setCompartment("C");
  s->setBoundaryCondition(false);
  s->setConstant(false);

  DistribSBasePlugin* distrib = static_cast<DistribSBasePlugin*>(s->getPlugin("distrib"));
  Uncertainty* uncert = distrib->createUncertainty();

  UncertMLNode * statcol = UncertMLNode::createDistributionNode
                          ("StatisticsCollection", "", "");

  UncertMLNode * normal = UncertMLNode::createDistributionNodeWithValues
                          ("NormalDistribution", "mean, variance", "3.2, 0.09");
  statcol->addChild(normal);

  UncertMLNode * stddev = UncertMLNode::createDistributionNodeWithValues
                         ("StandardDeviation", "value", "0.3");
  statcol->addChild(stddev);


  uncert->setUncertML(statcol);

  ex1->checkConsistency();
  writeSBML(ex1,"distrib_spec5.xml");
 
  delete ex1;
}

void createExample6()
{
  SBMLDocument* ex1 = createDistribDoc();
  Model* mod = ex1->getModel();
  Parameter* p = mod->createParameter();
  p->setId("mu_Z");
  p->setValue(10);
  p->setConstant(true);

  p = mod->createParameter();
  p->setId("var_Z");
  p->setValue(0.1);
  p->setConstant(true);

  p = mod->createParameter();
  p->setId("Z");
  p->setConstant(true);

  DistribSBasePlugin* distrib = static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));
  Uncertainty* uncert = distrib->createUncertainty();

  UncertMLNode * normal = UncertMLNode::createDistributionNode
                          ("NormalDistribution", "mean, variance", "mu_Z, var_Z");

  uncert->setUncertML(normal);

  ex1->checkConsistency();
  writeSBML(ex1,"distrib_spec6.xml");
 
  delete ex1;
}


void createPkPd()
{
  SBMLDocument* ex1 = createDistribDoc();
  Model* mod = ex1->getModel();

  FunctionDefinition* fd = mod->createFunctionDefinition();
  fd->setId("logNormal");
  ASTNode * math = SBML_parseFormula("lambda(scale, shape, NaN)");
  fd->setMath(math);
  delete math;
  //
  // Get a DistribFunctionDefinitionPlugin object plugged in the fd object.
  //
  // The type of the returned value of SBase::getPlugin() 
  // function is SBasePlugin*, and
  // thus the value needs to be casted for the corresponding derived class. 
  //
  DistribFunctionDefinitionPlugin* fdPlugin = 
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  // create a DrawFromDistribution object
  DrawFromDistribution * draw = fdPlugin->createDrawFromDistribution();

  // create the distribInputs
  DistribInput * input1 = draw->createDistribInput();
  input1->setId("scale");
  input1->setIndex(0);

  DistribInput * input2 = draw->createDistribInput();
  input2->setId("shape");
  input2->setIndex(1);

  // create the UncertMLNode object
  UncertMLNode * uncert = UncertMLNode::createDistributionNode
                          ("LogNormalDistribution", "logScale, shape", "scale, shape");
  draw->setUncertML(uncert);
  delete uncert;

  Compartment* c = mod->createCompartment();
  c->setId("central");
  c->setConstant(true);
  c->setSize(0);

  c = mod->createCompartment();
  c->setId("gut");
  c->setConstant(true);
  c->setSize(0);

  Species* s = mod->createSpecies();
  s->setId("Qc");
  s->setHasOnlySubstanceUnits(true);
  s->setInitialAmount(1);
  s->setCompartment("central");
  s->setBoundaryCondition(false);
  s->setConstant(false);

  s = mod->createSpecies();
  s->setId("Qg");
  s->setHasOnlySubstanceUnits(true);
  s->setInitialAmount(1);
  s->setCompartment("gut");
  s->setBoundaryCondition(false);
  s->setConstant(false);

  Parameter* p = mod->createParameter();
  p->setId("ka");
  p->setValue(1);
  p->setConstant(true);

  p = mod->createParameter();
  p->setId("ke");
  p->setValue(1);
  p->setConstant(true);

  p = mod->createParameter();
  p->setId("Cc");
  p->setValue(1);
  p->setConstant(false);

  p = mod->createParameter();
  p->setId("Cc_obs");
  p->setValue(1);
  p->setConstant(false);

  InitialAssignment* ia = mod->createInitialAssignment();
  ia->setSymbol("central");
  math = SBML_parseFormula("logNormal(0.5,0.1)");
  ia->setMath(math);
  delete math;

  ia = mod->createInitialAssignment();
  ia->setSymbol("ka");
  math = SBML_parseFormula("logNormal(0.5,0.1)");
  ia->setMath(math);
  delete math;

  ia = mod->createInitialAssignment();
  ia->setSymbol("ke");
  math = SBML_parseFormula("logNormal(0.5,0.1)");
  ia->setMath(math);
  delete math;

  AssignmentRule* ar = mod->createAssignmentRule();
  ar->setVariable("Cc");
  math = SBML_parseFormula("Qc/central");
  ar->setMath(math);
  delete math;

  ar = mod->createAssignmentRule();
  ar->setVariable("Cc_obs");
  math = SBML_parseFormula("Cc+1");
  ar->setMath(math);
  delete math;

  Reaction* rxn = mod->createReaction();
  rxn->setId("absorption");
  rxn->setReversible(false);
  rxn->setFast(false);

  SpeciesReference* sr = mod->createReactant();
  sr->setSpecies("Qg");
  sr->setStoichiometry(1);
  sr->setConstant(true);

  sr = mod->createProduct();
  sr->setSpecies("Qc");
  sr->setStoichiometry(1);
  sr->setConstant(true);

  KineticLaw* kl = mod->createKineticLaw();
  math = SBML_parseFormula("ka*Qg");
  kl->setMath(math);
  delete math;

  rxn = mod->createReaction();
  rxn->setId("excretion");
  rxn->setReversible(false);
  rxn->setFast(false);

  sr = mod->createReactant();
  sr->setSpecies("Qc");
  sr->setStoichiometry(1);
  sr->setConstant(true);

  kl = mod->createKineticLaw();
  math = SBML_parseFormula("(ke*Qc)/central");
  kl->setMath(math);
  delete math;

  ex1->checkConsistency();
  writeSBML(ex1,"pkpd.xml");
 
  delete ex1;
}

void createTruncated()
{
  SBMLDocument* ex1 = createDistribDoc();
  Model* mod = ex1->getModel();

  FunctionDefinition* fd = mod->createFunctionDefinition();
  fd->setId("truncatedNormal");
  ASTNode * math = SBML_parseFormula("lambda(mu, sigma, lower, upper, NaN)");
  fd->setMath(math);
  delete math;
  //
  // Get a DistribFunctionDefinitionPlugin object plugged in the fd object.
  //
  // The type of the returned value of SBase::getPlugin() 
  // function is SBasePlugin*, and
  // thus the value needs to be casted for the corresponding derived class. 
  //
  DistribFunctionDefinitionPlugin* fdPlugin = 
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  // create a DrawFromDistribution object
  DrawFromDistribution * draw = fdPlugin->createDrawFromDistribution();

  // create the distribInputs
  DistribInput * input = draw->createDistribInput();
  input->setId("mu");
  input->setIndex(0);

  input = draw->createDistribInput();
  input->setId("sigma");
  input->setIndex(1);

  input = draw->createDistribInput();
  input->setId("lower");
  input->setIndex(2);

  input = draw->createDistribInput();
  input->setId("upper");
  input->setIndex(3);

  // create the UncertMLNode object
  UncertMLNode * uncert = UncertMLNode::createDistributionNode
                          ("NormalDistribution", "mean, variance, truncationLowerInclusiveBound, truncationUpperInclusiveBound", "mu, sigma, lower, upper");
  draw->setUncertML(uncert);
  delete uncert;

  Parameter* p = mod->createParameter();
  p->setId("V");
  p->setValue(1);
  p->setConstant(true);

  p = mod->createParameter();
  p->setId("V_pop");
  p->setValue(105);
  p->setConstant(true);

  p = mod->createParameter();
  p->setId("V_omega");
  p->setValue(0.70);
  p->setConstant(false);

  p = mod->createParameter();
  p->setId("V_lower");
  p->setValue(15);
  p->setConstant(false);

  p = mod->createParameter();
  p->setId("V_upper");
  p->setValue(150);
  p->setConstant(false);

  InitialAssignment* ia = mod->createInitialAssignment();
  ia->setSymbol("V");
  math = SBML_parseFormula("truncatedNormal(V_pop, V_omega, V_lower, V_upper)");
  ia->setMath(math);
  delete math;

  ex1->checkConsistency();
  writeSBML(ex1,"truncated_distn.xml");
 
  delete ex1;
}


void createMultivariate()
{
  SBMLDocument* ex1 = createDistribDoc();
  Model* mod = ex1->getModel();

  FunctionDefinition* fd = mod->createFunctionDefinition();
  fd->setId("multivariateNormal");
  ASTNode * math = SBML_parseFormula("lambda(meanVec, covarianceM, meanVector)");
  fd->setMath(math);
  delete math;
  //
  // Get a DistribFunctionDefinitionPlugin object plugged in the fd object.
  //
  // The type of the returned value of SBase::getPlugin() 
  // function is SBasePlugin*, and
  // thus the value needs to be casted for the corresponding derived class. 
  //
  DistribFunctionDefinitionPlugin* fdPlugin = 
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  // create a DrawFromDistribution object
  DrawFromDistribution * draw = fdPlugin->createDrawFromDistribution();

  // create the distribInputs
  DistribInput * input = draw->createDistribInput();
  input->setId("meanVec");
  input->setIndex(0);

  input = draw->createDistribInput();
  input->setId("covarianceM");
  input->setIndex(1);

  // create the UncertMLNode object
  UncertMLNode * uncert = UncertMLNode::createDistributionNode
                          ("MultivariateNormalDistribution", "", "");
  UncertMLNode* meanVector = new UncertMLNode();
  meanVector->setElementName("meanVector");
  XMLAttributes attributes = XMLAttributes();
  attributes.add("arrayVar", "meanVec");
  meanVector->setAttributes(attributes);
  uncert->addChild(meanVector);

  UncertMLNode* CM = new UncertMLNode();
  CM->setElementName("covarianceMatrix");
  UncertMLNode* vals = new UncertMLNode();
  vals->setElementName("values");
  attributes = XMLAttributes();
  attributes.add("arrayVar", "covarianceM");
  vals->setAttributes(attributes);
  CM->addChild(vals);
  uncert->addChild(CM);

  draw->setUncertML(uncert);
  delete uncert;

  //NOTE:  the rest of the model was created by hand, since it has arrays stuff in it.
  ex1->checkConsistency();
  writeSBML(ex1,"multivariate_example.xml");
 
  delete ex1;
}





int main(int argc,char** argv)
{
  createExample1();
  createExample2();
  createExample3();
  createExample4();
  createExample5();
  createExample6();
  createPkPd();
  createTruncated();
  createMultivariate();
  return 0;
}
