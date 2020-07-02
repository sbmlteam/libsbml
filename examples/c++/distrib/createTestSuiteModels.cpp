/**
 * @file    createNormalExample.cpp
 * @brief   SBML distrib example
 * @author  Sarah Keating
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

#include <string>
#include <limits>
#include <sbml/SBMLTypes.h>
#include <sbml/packages/distrib/common/DistribExtensionTypes.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

double VALUE_FLAG = 9999999;

SBMLDocument* createBasicSBMLDocument()
{
  SBMLNamespaces sbmlns(3,1,"distrib",1);
  SBMLDocument *document = new SBMLDocument(&sbmlns);
  document->setPackageRequired("distrib", true);
  Model* model = document->createModel();

  //Our species will be the main read-out value in the model
  Species* s = model->createSpecies();
  s->setId("X");
  s->setConstant(false);
  s->setBoundaryCondition(false);
s->setHasOnlySubstanceUnits(true);
s->setCompartment("C");
s->setInitialAmount(0);

//Give it a compartment in which to live
Compartment* c = model->createCompartment();
c->setId("C");
c->setConstant(true);
c->setSize(1);
c->setSpatialDimensions(3.0);

//Our parameter 't' is what triggers an event every second:
Parameter* param = model->createParameter();
param->setId("t");
param->setConstant(false);
param->setValue(0);

//A rate rule to increase t
RateRule* rr = model->createRateRule();
rr->setVariable("t");
ASTNode* astn = SBML_parseL3Formula("1");
rr->setMath(astn);
delete astn;

//An event to constantly reset X to another call from the distribution.  Since this is generic, we don't set the actual math yet.
Event* e = model->createEvent();
e->setId("E0");
e->setUseValuesFromTriggerTime(true);
Trigger* t = e->createTrigger();
t->setPersistent(true);
t->setInitialValue(true);
astn = SBML_parseL3Formula("t >= 0.5");
t->setMath(astn);
delete astn;
EventAssignment* ea = e->createEventAssignment();
ea->setVariable("X");
//leave the math clear for now.
ea = e->createEventAssignment();
ea->setVariable("t");
astn = SBML_parseL3Formula("-0.5");
ea->setMath(astn);
delete astn;

return document;
}

void setupDistrib(Model* model, string distname, vector<string> argvec, string formargs)
{
  string id = argvec[0];
  string args = "";
  for (size_t a = 1; a < argvec.size(); a++) {
    if (a > 1) {
      args += ", ";
    }
    args += argvec[a];
  }

  EventAssignment* ea = model->getEvent(0)->getEventAssignment(0);
  ASTNode* astn = SBML_parseL3Formula((id + "(" + formargs + ")").c_str());
  cout << "Formula: " << id << "(" << formargs << ")" << endl;
  ea->setMath(astn);
  delete astn;
}

bool isnum(char x)
{
  if (x >= '0' && x <= '9') {
    return true;
  }
  return false;
}

void addVariance(Model* model, string formula)
{
  Parameter* variance = model->createParameter();
  variance->setId("variance");
  variance->setConstant(true);
  InitialAssignment* ia = model->createInitialAssignment();
  ia->setSymbol("variance");
  ASTNode* astn = SBML_parseL3Formula(formula.c_str());
  ia->setMath(astn);
  delete astn;
}

void setupDistribWithVals(Model* model, string distname, string id, vector<string> args, vector<double> vals, vector<string> types, string formargs)
{
  assert(args.size() == vals.size() && vals.size() == types.size());
  string calledargs = "";
  string allargs = "";
  vector<string> argvec;
  bool used_formarg = false;
  for (size_t a = 0; a < args.size(); a++) {
    if (a > 0) {
      allargs += ", ";
    }
    if (vals[a] == VALUE_FLAG)
    {
      if (!used_formarg)
      {
        allargs += formargs;
      }
      else
      {
        //Remove the comma
        // these are c++11 - may not work on some linux
        //allargs.pop_back();
        //allargs.pop_back();
        allargs.erase(allargs.size() - 1);
        allargs.erase(allargs.size() - 1);
      }
      used_formarg = true;
    }
    else
    {
      allargs += args[a];
      if (!isnum(args[a][0])) {
        if (args[a] == "variance") {
          stringstream ss;
          ss << vals[a];
          string str = ss.str();
          addVariance(model, str + "^2");
        }
        else {
          Parameter* param = model->createParameter();
          param->setId(args[a]);
          param->setConstant(true);
          param->setValue(vals[a]);
        }
      }
    }
  }

  EventAssignment* ea = model->getEvent(0)->getEventAssignment(0);
  ASTNode* astn = SBML_parseL3Formula((id + "(" + allargs + ")").c_str());
  cout << "Formula: " << id << "(" << allargs << ")" << endl;
  ea->setMath(astn);
  delete astn;
}

//UncertMLNode* createMixtureNode(std::string name, vector<string> weights)
//{
//  UncertMLNode *node = new UncertMLNode();
//  node->setElementName(name);
//
//  XMLAttributes attr = XMLAttributes();
//  /* really the url should be specific to the distribution
//  * but whilst the attribue is required in uncertML it does not require
//  * it to be an exact match
//  */
//  attr.add("definition", "http://www.uncertml.org/distributions");
//  node->setAttributes(attr);
//
//  for (unsigned int i = 0; i < weights.size(); i++)
//  {
//    UncertMLNode * child = new UncertMLNode();
//    child->setElementName("component");
//    XMLAttributes attributes = XMLAttributes();
//    attributes.add("weight", weights[i]);
//    child->setAttributes(attributes);
//    node->addChild(child);
//  }
//
//  return node;
//}
//
//
//void setupMixedDistrib(Model* model, vector<string> distributions, vector<string> weights, string id, vector<vector<string>> argvec, string formargs)
//{
//  assert(distributions.size() == argvec.size()+1);
//  string argstring = "";
//  vector<string> numberedvec;
//  vector<string> straightvec;
//  vector<string> numvec;
//  stringstream str;
//  for (size_t sub=0; sub<argvec.size(); sub++) {
//    vector<string> args = argvec[sub];
//    string numcommas = "";
//    string straightcommas = "";
//    for (size_t a=0; a<args.size(); a++) {
//      if (a>0 || sub>0) {
//        argstring += ", ";
//      }
//      if (a>0) {
//        numcommas += ", ";
//        straightcommas += ", ";
//      }
//      straightcommas += args[a];
//      str.str("");
//      str << args[a] << sub;
//      numberedvec.push_back(str.str());
//      argstring += str.str();
//      numcommas += str.str();
//    }
//    straightvec.push_back(straightcommas);
//    numvec.push_back(numcommas);
//  }
//  FunctionDefinition* fd = addFunctionDefinition(model, argstring);
//  fd->setId(id);
//  DistribFunctionDefinitionPlugin* dfdp = static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));
//  DrawFromDistribution* dfd = dfdp->createDrawFromDistribution();
//  for (size_t a=0; a<numberedvec.size(); a++) {
//    DistribInput* di = dfd->createDistribInput();
//    di->setId(numberedvec[a]);
//    di->setIndex(a);
//  }
//
//  UncertMLNode* mixedRoot = createMixtureNode(distributions[0], weights);
//  for (size_t d=1; d<distributions.size(); d++) {
//    UncertMLNode* dist = UncertMLNode::createDistributionNode(distributions[d], straightvec[d-1], numvec[d-1]);
//    mixedRoot->getChild(d-1)->addChild(dist);
//  }
//  dfd->setUncertML(mixedRoot);
//
//  EventAssignment* ea = model->getEvent(0)->getEventAssignment(0);
//  ASTNode* astn = SBML_parseL3Formula((id + "(" + formargs + ")").c_str());
//  ea->setMath(astn);
//  delete astn;
//}

void CreateStandardDistributions()
{
  int testnum = 39;
  stringstream testid;
  vector<string> args;

  SBMLDocument* document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("normal");
  args.push_back("mean");
  args.push_back("stddev");
  setupDistrib(document->getModel(), "normalDistribution", args, "0, 1.5");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("normal");
  args.push_back("mean");
  args.push_back("variance");
  Model* model = document->getModel();
  setupDistrib(model, "normalDistribution", args, "0, variance");
  addVariance(model, "1.5^2");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("uniform");
  args.push_back("minimum");
  args.push_back("maximum");
  setupDistrib(document->getModel(), "uniformDistribution", args, "0, 1");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("exponential");
  args.push_back("rate");
  setupDistrib(document->getModel(), "exponentialDistribution", args, "1");
  writeSBML(document,testid.str().c_str());
  delete document;

  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("exponential");
  args.push_back("rate");
  setupDistrib(document->getModel(), "exponentialDistribution", args, "0.5");
  writeSBML(document,testid.str().c_str());
  delete document;

  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("gamma");
  args.push_back("shape");
  args.push_back("scale");
  setupDistrib(document->getModel(), "gammaDistribution", args, "1, 2");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("gamma");
  args.push_back("shape");
  args.push_back("scale");
  setupDistrib(document->getModel(), "gammaDistribution", args, "2, 1.1");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("poisson");
  args.push_back("rate");
  setupDistrib(document->getModel(), "poissonDistribution", args, "1.5");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("beta");
  //args.push_back("alpha");
  //args.push_back("beta");
  //setupDistrib(document->getModel(), "betaDistribution", args, "1, 2");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("cauchy");
  args.push_back("location");
  args.push_back("scale");
  setupDistrib(document->getModel(), "cauchyDistribution", args, "1, 2");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("chisquare");
  args.push_back("degreesOfFreedom");
  setupDistrib(document->getModel(), "chiSquareDistribution", args, "2");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("chisquare");
  args.push_back("degreesOfFreedom");
  setupDistrib(document->getModel(), "chiSquareDistribution", args, "3.2");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("rayleigh");
  args.push_back("scale");
  setupDistrib(document->getModel(), "rayleighDistribution", args, "2");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("f_dist");
  //args.push_back("denominator");
  //args.push_back("numerator");
  //setupDistrib(document->getModel(), "fDistribution", args, "4, 6");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("inverse_gamma");
  //args.push_back("shape");
  //args.push_back("scale");
  //setupDistrib(document->getModel(), "inverseGammaDistribution", args, "5, 6");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("laplace");
  args.push_back("location");
  args.push_back("scale");
  setupDistrib(document->getModel(), "laPlaceDistribution", args, "1, 2");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("logNormal");
  args.push_back("logScale");
  args.push_back("shape");
  setupDistrib(document->getModel(), "logNormalDistribution", args, "1, 2");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("logistic");
  //args.push_back("location");
  //args.push_back("scale");
  //setupDistrib(document->getModel(), "logisticDistribution", args, "1, 2");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("pareto");
  //args.push_back("scale");
  //args.push_back("shape");
  //setupDistrib(document->getModel(), "paretoDistribution", args, "2, 10");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("student_t");
  //args.push_back("location");
  //args.push_back("scale");
  //args.push_back("degreesOfFreedom");
  //setupDistrib(document->getModel(), "studentTDistribution", args, "0, 1, 4");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("student_t");
  //args.push_back("location");
  //args.push_back("scale");
  //args.push_back("degreesOfFreedom");
  //setupDistrib(document->getModel(), "studentTDistribution", args, "3, 1, 4");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("student_t");
  //args.push_back("location");
  //args.push_back("scale");
  //args.push_back("degreesOfFreedom");
  //setupDistrib(document->getModel(), "studentTDistribution", args, "3, 2, 4");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("weibull");
  //args.push_back("scale");
  //args.push_back("shape");
  //setupDistrib(document->getModel(), "weibullDistribution", args, "1, 2");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("weibull");
  //args.push_back("scale");
  //args.push_back("shape");
  //setupDistrib(document->getModel(), "weibullDistribution", args, "2, 1");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("weibull");
  //args.push_back("scale");
  //args.push_back("shape");
  //setupDistrib(document->getModel(), "weibullDistribution", args, "2, 0.5");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("binomial");
  args.push_back("numberOfTrials");
  args.push_back("probabilityOfSuccess");
  setupDistrib(document->getModel(), "binomialDistribution", args, "100, 0.2");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  //testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("geometric");
  //args.push_back("probability");
  //setupDistrib(document->getModel(), "geometricDistribution", args, "0.1");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  //testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("hypergeometric");
  //args.push_back("numberOfSuccesses");
  //args.push_back("numberOfTrials");
  //args.push_back("populationSize");
  //setupDistrib(document->getModel(), "hypergeometricDistribution", args, "3, 50, 150");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  //testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("negative_binomial");
  //args.push_back("numberOfFailures");
  //args.push_back("probability");
  //setupDistrib(document->getModel(), "negativeBinomialDistribution", args, "10, 0.2");
  //writeSBML(document,testid.str().c_str());
  //delete document;
 
}

void CreateTruncatedDistributions()
{
  int testnum = 68;
  stringstream testid;
  vector<string> args;

  SBMLDocument* document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("normal");
  args.push_back("mean");
  args.push_back("stddev");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "normalDistribution", args, "0, 1.5, -0.5, infinity");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("normal");
  args.push_back("mean");
  args.push_back("stddev");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "normalDistribution", args, "0, 1.5, -infinity, 0.5");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("normal");
  args.push_back("mean");
  args.push_back("stddev");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "normalDistribution", args, "0, 1.5, 0.5, infinity");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("normal");
  args.push_back("mean");
  args.push_back("stddev");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "normalDistribution", args, "0, 1.5, -infinity, -0.5");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("normal");
  args.push_back("mean");
  args.push_back("stddev");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "normalDistribution", args, "0, 0.5, -0.5, 0.5");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("normal");
  args.push_back("mean");
  args.push_back("variance");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "normalDistribution", args, "0, variance, -0.5, 0.5");
  addVariance(document->getModel(), "0.25^2");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("normal");
  args.push_back("mean");
  args.push_back("stddev");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "normalDistribution", args, "0, 0.5, -0.25, 0.75");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("normal");
  args.push_back("mean");
  args.push_back("stddev");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "normalDistribution", args, "0, 0.5, 0.25, 0.75");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("exponential");
  args.push_back("rate");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "exponentialDistribution", args, "1, 0.25, infinity");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("exponential");
  args.push_back("rate");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "exponentialDistribution", args, "1, -infinity, 0.75");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("exponential");
  args.push_back("rate");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "exponentialDistribution", args, "1, 0.25, 0.75");
  writeSBML(document,testid.str().c_str());
  delete document;
 
}

void CreateUncommonTruncatedDistributions()
{
  int testnum = 1000;
  stringstream testid;
  vector<string> args;
  SBMLDocument* document;

  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("gamma");
  args.push_back("shape");
  args.push_back("scale");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "gammaDistribution", args, "1, 2, 0.5, 10");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("poisson");
  args.push_back("rate");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "poissonDistribution", args, "1.5, 0.25, 0.75");
  writeSBML(document,testid.str().c_str());
  delete document;

  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("beta");
  //args.push_back("alpha");
  //args.push_back("beta");
  //args.push_back("truncationLowerBound");
  //args.push_back("truncationUpperBound");
  //setupDistrib(document->getModel(), "betaDistribution", args, "1, 2, 0.25, 0.75");
  //writeSBML(document,testid.str().c_str());
  //delete document;

  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("cauchy");
  args.push_back("location");
  args.push_back("scale");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "cauchyDistribution", args, "1, 2, 0.25, 0.75");
  writeSBML(document,testid.str().c_str());
  delete document;

  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("chisquare");
  args.push_back("degreesOfFreedom");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "chiSquareDistribution", args, "2, 0.25, 0.75");
  writeSBML(document,testid.str().c_str());
  delete document;

  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("exponential");
  args.push_back("rate");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "exponentialDistribution", args, "2, 0.25, 0.75");
  writeSBML(document,testid.str().c_str());
  delete document;

  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("beta");
  //args.push_back("denominator");
  //args.push_back("numerator");
  //args.push_back("truncationLowerBound");
  //args.push_back("truncationUpperBound");
  //setupDistrib(document->getModel(), "fDistribution", args, "1, 2, 0.25, 0.75");
  //writeSBML(document,testid.str().c_str());
  //delete document;

  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("inverse_gamma");
  //args.push_back("shape");
  //args.push_back("scale");
  //args.push_back("truncationLowerBound");
  //args.push_back("truncationUpperBound");
  //setupDistrib(document->getModel(), "inverseGammaDistribution", args, "1, 2, 0.25, 0.75");
  //writeSBML(document,testid.str().c_str());
  //delete document;

  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("laplace");
  args.push_back("location");
  args.push_back("scale");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "laplaceDistribution", args, "1, 2, 0.25, 0.75");
  writeSBML(document,testid.str().c_str());
  delete document;

  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("lognormal");
  args.push_back("logScale");
  args.push_back("shape");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "logNormalDistribution", args, "1, 2, 0.25, 0.75");
  writeSBML(document,testid.str().c_str());
  delete document;

  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("logistic");
  //args.push_back("location");
  //args.push_back("scale");
  //args.push_back("truncationLowerBound");
  //args.push_back("truncationUpperBound");
  //setupDistrib(document->getModel(), "logisticDistribution", args, "1, 2, 0.25, 0.75");
  //writeSBML(document,testid.str().c_str());
  //delete document;

  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("pareto");
  //args.push_back("scale");
  //args.push_back("shape");
  //args.push_back("truncationLowerBound");
  //args.push_back("truncationUpperBound");
  //setupDistrib(document->getModel(), "paretoDistribution", args, "1, 2, 0.25, 0.75");
  //writeSBML(document,testid.str().c_str());
  //delete document;

  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("student_t");
  //args.push_back("location");
  //args.push_back("scale");
  //args.push_back("degreesOfFreedom");
  //args.push_back("truncationLowerBound");
  //args.push_back("truncationUpperBound");
  //setupDistrib(document->getModel(), "studentTDistribution", args, "1, 2, 4, 0.25, 0.75");
  //writeSBML(document,testid.str().c_str());
  //delete document;

  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("weibull");
  //args.push_back("scale");
  //args.push_back("shape");
  //args.push_back("truncationLowerBound");
  //args.push_back("truncationUpperBound");
  //setupDistrib(document->getModel(), "weibullDistribution", args, "1, 2, 0.25, 0.75");
  //writeSBML(document,testid.str().c_str());
  //delete document;

  document = createBasicSBMLDocument();
  args.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  args.push_back("binomial");
  args.push_back("numberOfTrials");
  args.push_back("probabilityOfSuccess");
  args.push_back("truncationLowerBound");
  args.push_back("truncationUpperBound");
  setupDistrib(document->getModel(), "binomialDistribution", args, "100, 0.2, 50, 75");
  writeSBML(document,testid.str().c_str());
  delete document;

  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("geometric");
  //args.push_back("probability");
  //args.push_back("truncationLowerBound");
  //args.push_back("truncationUpperBound");
  //setupDistrib(document->getModel(), "geometricDistribution", args, "0.1, 25, 75");
  //writeSBML(document,testid.str().c_str());
  //delete document;

  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("hypergeometric");
  //args.push_back("numberOfSuccesses");
  //args.push_back("numberOfTrials");
  //args.push_back("populationSize");
  //args.push_back("truncationLowerBound");
  //args.push_back("truncationUpperBound");
  //setupDistrib(document->getModel(), "hypergeometricDistribution", args, "3, 50, 150, 5, 20");
  //writeSBML(document,testid.str().c_str());
  //delete document;

  //document = createBasicSBMLDocument();
  //args.clear();
  //testid.str("");
  testnum++;
  //testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  //args.push_back("negative_binomial");
  //args.push_back("numberOfFailures");
  //args.push_back("probability");
  //args.push_back("truncationLowerBound");
  //args.push_back("truncationUpperBound");
  //setupDistrib(document->getModel(), "negativeBinomialDistribution", args, "10, 0.2, 25, 75");
  //writeSBML(document,testid.str().c_str());
  //delete document;

}

void CreateDistributionsWithValues()
{
  int testnum = 79;
  stringstream testid;
  string id;
  vector<string> internalargs;
  vector<double> internalvals;
  vector<string> internaltypes;
  SBMLDocument* document;

  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(0);
  internaltypes.push_back("rVal");
  internalargs.push_back("stddev");
  internalvals.push_back(1.5);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(0);
  internaltypes.push_back("rVal");
  internalargs.push_back("variance");
  internalvals.push_back(1.5);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "uniform";
  internalargs.push_back("minimum");
  internalvals.push_back(0);
  internaltypes.push_back("rVal");
  internalargs.push_back("maximum");
  internalvals.push_back(1);
  internaltypes.push_back("rVal");
  setupDistribWithVals(document->getModel(), "uniformDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "exponential";
  internalargs.push_back("rate");
  internalvals.push_back(1);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "exponentialDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;

  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "exponential";
  internalargs.push_back("rate");
  internalvals.push_back(0.5);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "exponentialDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;

  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "gamma";
  internalargs.push_back("shape");
  internalvals.push_back(1);
  internaltypes.push_back("prVal");
  internalargs.push_back("scale");
  internalvals.push_back(2);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "gammaDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "gamma";
  internalargs.push_back("shape");
  internalvals.push_back(2);
  internaltypes.push_back("prVal");
  internalargs.push_back("scale");
  internalvals.push_back(1.1);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "gammaDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "poisson";
  internalargs.push_back("rate");
  internalvals.push_back(1.5);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "poissonDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;
 
}

void CreateDistributionsWithMixedValues()
{
  int testnum = 87;
  stringstream testid;
  string id = "";
  vector<string> internalargs;
  vector<double> internalvals;
  vector<string> internaltypes;

  SBMLDocument* document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  internalargs.push_back("stddev");
  internalvals.push_back(1.5);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "0");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  internalargs.push_back("variance");
  internalvals.push_back(1.5);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "0");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(0);
  internaltypes.push_back("rVal");
  internalargs.push_back("stddev");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "1.5");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(0);
  internaltypes.push_back("rVal");
  internalargs.push_back("variance");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "1.5");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "uniform";
  internalargs.push_back("minimum");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  internalargs.push_back("maximum");
  internalvals.push_back(1);
  internaltypes.push_back("rVal");
  setupDistribWithVals(document->getModel(), "uniformDistribution", id, internalargs, internalvals, internaltypes, "0");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "uniform";
  internalargs.push_back("minimum");
  internalvals.push_back(0);
  internaltypes.push_back("rVal");
  internalargs.push_back("maximum");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  setupDistribWithVals(document->getModel(), "uniformDistribution", id, internalargs, internalvals, internaltypes, "1");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "gamma";
  internalargs.push_back("shape");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  internalargs.push_back("scale");
  internalvals.push_back(2);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "gammaDistribution", id, internalargs, internalvals, internaltypes, "1");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "gamma";
  internalargs.push_back("shape");
  internalvals.push_back(1);
  internaltypes.push_back("prVal");
  internalargs.push_back("scale");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  setupDistribWithVals(document->getModel(), "gammaDistribution", id, internalargs, internalvals, internaltypes, "2");
  writeSBML(document,testid.str().c_str());
  delete document;
 
}

void CreateTruncatedDistributionsWithValues()
{
  int testnum = 95;
  stringstream testid;
  string id = "";
  vector<string> internalargs;
  vector<double> internalvals;
  vector<string> internaltypes;
  SBMLDocument* document;
  
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(0);
  internaltypes.push_back("rVal");
  internalargs.push_back("stddev");
  internalvals.push_back(1.5);
  internaltypes.push_back("prVal");
  internalargs.push_back("truncationUpperBound");
  internalvals.push_back(std::numeric_limits<double>::infinity());
  internaltypes.push_back("rVal");
  internalargs.push_back("truncationLowerBound");
  internalvals.push_back(-0.5);
  internaltypes.push_back("rVal");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  internalargs.push_back("stddev");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  internalargs.push_back("truncationLowerBound");
  internalvals.push_back(-0.5);
  internaltypes.push_back("rVal");
  internalargs.push_back("truncationUpperBound");
  internalvals.push_back(std::numeric_limits<double>::infinity());
  internaltypes.push_back("rVal");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "0,1.5");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(0);
  internaltypes.push_back("rVal");
  internalargs.push_back("stddev");
  internalvals.push_back(1.5);
  internaltypes.push_back("prVal");
  internalargs.push_back("truncationLowerBound");
  internalvals.push_back(-std::numeric_limits<double>::infinity());
  internaltypes.push_back("rVal");
  internalargs.push_back("truncationUpperBound");
  internalvals.push_back(0.5);
  internaltypes.push_back("rVal");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "000" << testnum << "/000" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  internalargs.push_back("stddev");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  internalargs.push_back("truncationLowerBound");
  internalvals.push_back(-std::numeric_limits<double>::infinity());
  internaltypes.push_back("rVal");
  internalargs.push_back("truncationUpperBound");
  internalvals.push_back(0.5);
  internaltypes.push_back("rVal");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "0,1.5");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "00" << testnum << "/00" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(0);
  internaltypes.push_back("rVal");
  internalargs.push_back("stddev");
  internalvals.push_back(0.5);
  internaltypes.push_back("prVal");
  internalargs.push_back("truncationLowerBound");
  internalvals.push_back(-0.5);
  internaltypes.push_back("rVal");
  internalargs.push_back("truncationUpperBound");
  internalvals.push_back(0.5);
  internaltypes.push_back("rVal");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "00" << testnum << "/00" << testnum << "-sbml-l3v1.xml";
  id = "normal";
  internalargs.push_back("mean");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  internalargs.push_back("stddev");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  internalargs.push_back("truncationLowerBound");
  internalvals.push_back(-0.5);
  internaltypes.push_back("rVal");
  internalargs.push_back("truncationUpperBound");
  internalvals.push_back(0.5);
  internaltypes.push_back("rVal");
  setupDistribWithVals(document->getModel(), "normalDistribution", id, internalargs, internalvals, internaltypes, "0, 0.5");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "00" << testnum << "/00" << testnum << "-sbml-l3v1.xml";
  id = "exponential";
  internalargs.push_back("rate");
  internalvals.push_back(1);
  internaltypes.push_back("prVal");
  internalargs.push_back("truncationLowerBound");
  internalvals.push_back(0.25);
  internaltypes.push_back("prVal");
  internalargs.push_back("truncationUpperBound");
  internalvals.push_back(0.75);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "exponentialDistribution", id, internalargs, internalvals, internaltypes, "");
  writeSBML(document,testid.str().c_str());
  delete document;
 
  document = createBasicSBMLDocument();
  internalargs.clear();
  internalvals.clear();
  internaltypes.clear();
  testid.str("");
  testnum++;
  testid << "00" << testnum << "/00" << testnum << "-sbml-l3v1.xml";
  id = "exponential";
  internalargs.push_back("rate");
  internalvals.push_back(VALUE_FLAG);
  internaltypes.push_back("varId");
  internalargs.push_back("truncationLowerBound");
  internalvals.push_back(0.25);
  internaltypes.push_back("prVal");
  internalargs.push_back("truncationUpperBound");
  internalvals.push_back(0.75);
  internaltypes.push_back("prVal");
  setupDistribWithVals(document->getModel(), "exponentialDistribution", id, internalargs, internalvals, internaltypes, "1");
  writeSBML(document,testid.str().c_str());
  delete document;
 
}

int main(int argc,char** argv)
{
  CreateStandardDistributions();
  CreateTruncatedDistributions();
  CreateDistributionsWithValues();
  CreateDistributionsWithMixedValues();
  CreateTruncatedDistributionsWithValues();


  //CreateUncommonTruncatedDistributions();
}