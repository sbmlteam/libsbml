/**
 * @file    example1.cpp
 * @brief   SBML hierarchical composition example
 * @author  Lucian Smith
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <iostream>
#include <sstream>

#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>

#include <sbml/packages/comp/extension/CompExtension.h>
#include <sbml/packages/comp/extension/CompSBasePlugin.h>
#include <sbml/packages/comp/extension/CompSBMLDocumentPlugin.h>
#include <sbml/packages/comp/extension/CompModelPlugin.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLConverter.h>

#ifdef WIN32
#include <conio.h>
#endif

LIBSBML_CPP_NAMESPACE_USE
using namespace std;

static SBMLExtensionRegister<CompExtension> compExtensionRegistry;
void createTest17();
void createTest18();
void createTest19();
void createTest20();
void createTest21();
void createTest22();
void createTest23();
void createTest24();
void createTest25();
void createTest26();
void createTest27();
void createTest28();
void createTest29();
void createTest30();
void createTest31();
void createTest34();
void createTest35();
void createTest36();
void createTest37();
void createTest38();
void createTest39();
void createTest40();
void createTest41();
void createPortSBO();
void createTest42();
void createTest43();
void createTest44();
void createTest45();
void createTest46();
void createTest47();
void createTest48();
void createTest49();
void createTest50();

int main(int argc,char** argv){

  createTest50();
  createTest49();
  createTest48();
  createTest47();
  createTest46();
  createTest45();
  createTest44();
  createTest43();
  createTest42();
  createPortSBO();
  createTest41();
  createTest40();
  createTest39();
  createTest38();
  createTest37();
  createTest36();
  createTest35();
  createTest34();
  createTest31();
  createTest30();
  createTest29();
  createTest28();
  createTest27();
  createTest26();
  createTest25();
  createTest24();
  createTest23();
  createTest22();
  createTest21();
  createTest20();
  createTest19();
  createTest18();
  createTest17();

  int retval = 0;
  int rv;
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  props->addOption("leavePorts", false);
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);


  // create the document
  SBMLDocument *document = new SBMLDocument(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document->getPlugin("comp"));
  compdoc->setRequired(true);

  // create the Model
  Model* model=document->createModel();
  CompModelPlugin* mplugin = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  
  // create a parameter
  Parameter* param = model->createParameter();
  param->setId("param1");
  param->setConstant(true);

  // create a Submodel
  Submodel* submod1 = mplugin->createSubmodel();
  submod1->setId("submod1");
  submod1->setModelRef("Mod1");

  // Convert parameter to the plugin version so we can add the new attributes and replacements to it.
  CompSBasePlugin* splugin = static_cast<CompSBasePlugin*>(param->getPlugin("comp"));

  // Add a replaced element.
  ReplacedElement* rep1 = splugin->createReplacedElement();
  rv = rep1->setSubmodelRef("submod1");
  rv = rep1->setIdRef("subparam1");

  // Create a model definition with a parameter.
  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("Mod1");
  Parameter* m1param1 = moddef1->createParameter();
  m1param1->setId("subparam1");
  m1param1->setConstant(true);

  //Now write it out to a file
  writeSBMLToFile(document,"test1.xml");
  SBMLDocument *flatdoc = document->clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test1_flat.xml");

  //Now, slightly modify it so that the replacing parameter has information the replaced parameter doesn't.
  param->setValue(2.3);
  writeSBMLToFile(document, "test2.xml");
  flatdoc = document->clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test2_flat.xml");

  //Now, modify it again so that the replaced parameter has information the replacing parameter doesn't.
  param->unsetValue();
  m1param1->setValue(5.01);
  writeSBMLToFile(document, "test3.xml");
  flatdoc = document->clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test3_flat.xml");

  //And finally, modify it so that both parameters have set values.
  param->setValue(10.42);
  writeSBMLToFile(document, "test4.xml");
  flatdoc = document->clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test4_flat.xml");

  //Now give the submodel a port and point to that instead.
  CompModelPlugin* moddefplug = static_cast<CompModelPlugin*>(moddef1->getPlugin("comp"));
  Port* port = moddefplug->createPort();
  port->setId("port1");
  port->setIdRef("subparam1");
  rep1->unsetIdRef();
  rep1->setPortRef("port1");
  writeSBMLToFile(document, "test5.xml");
  flatdoc = document->clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test5_flat.xml");


  //Now give the submodel a second parameter that should survive aggregation
  Parameter* m1param2 = moddef1->createParameter();
  m1param2->setId("subparam2");
  m1param2->setConstant(true);
  m1param2->setValue(6);
  writeSBMLToFile(document, "test6.xml");
  flatdoc = document->clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test6_flat.xml");

  //Now give the submodel an initial assignment and a rate rule to the first parameter that should survive aggregation
  InitialAssignment* m1ia = moddef1->createInitialAssignment();
  m1ia->setSymbol("subparam1");
  m1ia->setMath(SBML_parseFormula("subparam2+6"));
  RateRule* m1rr = moddef1->createRateRule();
  m1rr->setVariable("subparam1");
  m1rr->setMath(SBML_parseFormula("8*subparam2+subparam1"));
  param->setConstant(false);
  m1param1->setConstant(false);
  writeSBMLToFile(document, "test7.xml");
  flatdoc = document->clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test7_flat.xml");

  //Give the parent model a parameter that is to be replaced by the second parameter in the submodel.
  Parameter* param2 = model->createParameter();
  param2->setId("param2");
  param2->setMetaId("p2_meta");
  param2->setConstant(false);
  CompSBasePlugin* p2plugin = static_cast<CompSBasePlugin*>(param2->getPlugin("comp"));
  ReplacedBy* rb = p2plugin->createReplacedBy();
  rb->setSubmodelRef("submod1");
  rb->setIdRef("subparam2");
  writeSBMLToFile(document, "test8.xml");
  flatdoc = document->clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test8_flat.xml");

  //OK, back up and make a basic twice-nested model.
  SBMLDocument doc2(&sbmlns);
  CompSBMLDocumentPlugin* compdoc2 = static_cast<CompSBMLDocumentPlugin*>(doc2.getPlugin("comp"));
  compdoc2->setRequired(true);

  // create the Model
  Model* mod2=doc2.createModel();
  CompModelPlugin* mplugin2 = static_cast<CompModelPlugin*>(mod2->getPlugin("comp"));

  //Create some model definitions with some species in 'em (just to change things up).
  ModelDefinition md(&sbmlns);
  CompModelPlugin* mdplugin = static_cast<CompModelPlugin*>(md.getPlugin("comp"));
  Compartment c1(&sbmlns);
  c1.setId("C");
  c1.setConstant(false);
  c1.setSpatialDimensions(3.0);
  c1.setSize(10);
  Species s1(&sbmlns);
  s1.setId("S1");
  s1.setConstant(false);
  s1.setBoundaryCondition(false);
  s1.setInitialAmount(5);
  s1.setHasOnlySubstanceUnits(false);
  s1.setCompartment("C");
  mod2->addCompartment(&c1);
  mod2->addSpecies(&s1);
  c1.setSize(1);
  s1.setInitialAmount(7);
  md.addCompartment(&c1);
  md.addSpecies(&s1);
  md.setId("moddef1");
  compdoc2->addModelDefinition(&md);
  md.setId("moddef2");
  md.getSpecies(0)->setInitialAmount(9);
  Submodel smod(&csbmlns);
  smod.setId("sub1");
  smod.setModelRef("moddef1");
  mdplugin->addSubmodel(&smod);
  compdoc2->addModelDefinition(&md);
  mplugin2->addSubmodel(&smod);
  Submodel smod2(&csbmlns);
  smod2.setId("sub2");
  smod2.setModelRef("moddef2");
  mplugin2->addSubmodel(&smod2);

  //Now write this aggregate model to a file.
  writeSBMLToFile(&doc2, "test9.xml");
  flatdoc = doc2.clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test9_flat.xml");

  //Now set up a connection for the top model to a model two levels down
  Compartment* compartment = mod2->getCompartment(0);
  CompSBasePlugin* compplug = static_cast<CompSBasePlugin*>(compartment->getPlugin("comp"));
  ReplacedElement* re3 = compplug->createReplacedElement();
  re3->setSubmodelRef("sub2");
  re3->setIdRef("sub1");
  SBaseRef* sbr = re3->createSBaseRef();
  sbr->setIdRef("C");

  writeSBMLToFile(&doc2, "test10.xml");
  flatdoc = doc2.clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test10_flat.xml");

  //Now set up a four-deep model to see if aggregation works that far down.
  md.setId("moddef3");
  md.getSpecies(0)->setInitialAmount(11);
  mdplugin->addSubmodel(&smod2);
  compdoc2->addModelDefinition(&md);
  smod2.setId("sub3");
  smod2.setModelRef("moddef3");
  mplugin2->addSubmodel(&smod2);
  
  writeSBMLToFile(&doc2, "test11.xml");
  flatdoc = doc2.clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test11_flat.xml");

  //And, why not?  Let's do another replace on a species that far down.
  Species* sp = mod2->getSpecies("S1");
  CompSBasePlugin* spplug = static_cast<CompSBasePlugin*>(sp->getPlugin("comp"));
  ReplacedElement* re4 = spplug->createReplacedElement();
  re4->setSubmodelRef("sub3");
  re4->setIdRef("sub2");
  SBaseRef* sbr2 = re4->createSBaseRef();
  sbr2->setIdRef("sub1");
  SBaseRef* sbr3 = sbr2->createSBaseRef();
  sbr3->setIdRef("S1");

  writeSBMLToFile(&doc2, "test12.xml");
  flatdoc = doc2.clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test12_flat.xml");

  //And now do a ReplacedBy the other way:
  ReplacedBy* rb2 = spplug->createReplacedBy();
  rb2->setSubmodelRef("sub2");
  rb2->setIdRef("sub1");
  SBaseRef* sbr4 = rb2->createSBaseRef();
  sbr4->setIdRef("S1");
  //And make sure that it's the new one:
  sp = compdoc2->getModelDefinition("moddef1")->getSpecies("S1");
  sp->setInitialConcentration(2.5);

  writeSBMLToFile(&doc2, "test13.xml");
  flatdoc = doc2.clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test13_flat.xml");

  //For Chris:  take a model definition from one document, and make it the official model for a different document.
  ModelDefinition* transfer = compdoc2->getModelDefinition("moddef1");
  SBMLDocument transferdoc(&sbmlns);
  transferdoc.setModel(transfer);
  Model* newmod = transferdoc.getModel();
  //newmod->setElementNamespace("http://www.sbml.org/sbml/level3/version1/core");
  writeSBMLToFile(&transferdoc, "testTransfer.xml");


  //Test having ports on the main model:
  Port* port1 = mplugin2->createPort();
  port1->setIdRef("C");
  port1->setId("C_port");
  
  Port* port2 = mplugin2->createPort();
  port2->setId("C2_port");
  port2->setIdRef("sub3");
  sbr = port2->createSBaseRef();
  sbr->setIdRef("sub2");
  sbr = sbr->createSBaseRef();
  sbr->setIdRef("sub1");
  sbr = sbr->createSBaseRef();
  sbr->setIdRef("C");

  writeSBMLToFile(&doc2, "test14.xml");
  flatdoc = doc2.clone();
  converter->getProperties()->setBoolValue("leavePorts", true);
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test14_flat_ports.xml");
  flatdoc = doc2.clone();
  converter->getProperties()->setBoolValue("leavePorts", false);
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test14_flat.xml");

  //Test chained ReplacedBy elements.
  doc2.setModel(NULL);
  compdoc2->removeModelDefinition(0);
  compdoc2->removeModelDefinition(0);
  compdoc2->removeModelDefinition(0);
  Parameter p5(&sbmlns);
  p5.setConstant(false);
  p5.setId("p8");
  p5.setValue(8);

  ModelDefinition moddef;
  moddef.addParameter(&p5);
  moddef.setId("moddef1");
  compdoc2->addModelDefinition(&moddef);

  moddef.removeParameter(0);
  moddef.setId("moddef2");
  mplugin2 = static_cast<CompModelPlugin*>(moddef.getPlugin("comp"));
  Submodel* submodel = mplugin2->createSubmodel();
  submodel->setId("sub1");
  submodel->setModelRef("moddef1");


  splugin = static_cast<CompSBasePlugin*>(p5.getPlugin("comp"));
  rb = splugin->createReplacedBy();
  rb->setSubmodelRef("sub1");
  rb->setIdRef("p8");
  p5.setId("p4");
  p5.setValue(4);
  moddef.addParameter(&p5);
  compdoc2->addModelDefinition(&moddef);

  model = doc2.createModel();
  mplugin2 = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  submodel = mplugin2->createSubmodel();
  submodel->setId("sub2");
  submodel->setModelRef("moddef2");
  rb->setSubmodelRef("sub2");
  rb->setIdRef("p4");
  p5.setId("p2");
  p5.setValue(2);
  model->addParameter(&p5);


  writeSBMLToFile(&doc2, "test15.xml");
  flatdoc = doc2.clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test15_flat.xml");

  //Now add a different element that is replaced on the way down instead:
  Parameter p6(&sbmlns);
  p6.setConstant(true);
  p6.setValue(16);
  p6.setId("p16");

  ModelDefinition* mdp = compdoc2->getModelDefinition("moddef1");
  mdp->addParameter(&p6);
  mdp = compdoc2->getModelDefinition("moddef2");
  mdp->addParameter(&p6);

  Parameter* pp = mdp->getParameter("p4");
  CompSBasePlugin* comppp = static_cast<CompSBasePlugin*>(pp->getPlugin("comp"));
  re4 = comppp->createReplacedElement();
  re4->setIdRef("p16");
  re4->setSubmodelRef("sub1");

  pp = model->getParameter("p2");
  comppp = static_cast<CompSBasePlugin*>(pp->getPlugin("comp"));
  re4 = comppp->createReplacedElement();
  re4->setIdRef("p16");
  re4->setSubmodelRef("sub2");

  writeSBMLToFile(&doc2, "test16.xml");
  flatdoc = doc2.clone();
  converter->setDocument(flatdoc);
  converter->convert();
  writeSBMLToFile(flatdoc, "test16_flat.xml");



  delete document;
#ifdef WIN32
  if (retval != 0) {
    cout << "(Press any key to exit.)" << endl;
    _getch();
  }
#endif
  return retval;
}

void createTest17() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(p.getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("p80");
  re->setConversionFactor("conv");
  Parameter conv(&sbmlns);
  conv.setConstant(true);
  conv.setId("conv");
  conv.setValue(0.1);
  model->addParameter(&p);
  model->addParameter(&conv);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  
  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("p80");
  ar->setMath(SBML_parseL3Formula("80"));

  writeSBMLToFile(&document, "test17.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test17_flat.xml");
}

void createTest18() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  p.setValue(8);
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(p.getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("p80");
  re->setConversionFactor("conv");
  Parameter conv(&sbmlns);
  conv.setConstant(true);
  conv.setId("conv");
  conv.setValue(0.1);
  model->addParameter(&p);
  model->addParameter(&conv);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  p80->setValue(80);

  Parameter* prel = moddef1->createParameter();
  prel->setConstant(false);
  prel->setId("prel");
  
  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("prel");
  ar->setMath(SBML_parseL3Formula("p80/8"));

  writeSBMLToFile(&document, "test18.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test18_flat.xml");
}

void createTest19() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  p.setValue(8);
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(p.getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("p80");
  re->setConversionFactor("conv");
  Parameter conv(&sbmlns);
  conv.setConstant(true);
  conv.setId("conv");
  conv.setValue(0.1);
  model->addParameter(&p);
  model->addParameter(&conv);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  p80->setValue(80);

  RateRule* rr = moddef1->createRateRule();
  rr->setVariable("p80");
  rr->setMath(SBML_parseL3Formula("4*p80+3"));

  writeSBMLToFile(&document, "test19.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test19_flat.xml");
}

void createTest20() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Compartment* compartment = model->createCompartment();
  compartment->setId("C");
  compartment->setSize(1);
  compartment->setConstant(true);
  Species* s = model->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setId("s8");
  s->setInitialAmount(8);
  s->setHasOnlySubstanceUnits(true);
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(s->getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("s80");
  re->setConversionFactor("conv");
  Parameter conv(&sbmlns);
  conv.setConstant(true);
  conv.setId("conv");
  conv.setValue(0.1);
  model->addParameter(&conv);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  moddef1->addCompartment(compartment);
  Species* s80 = moddef1->createSpecies();
  s80->setConstant(false);
  s80->setBoundaryCondition(false);
  s80->setCompartment("C");
  s80->setId("s80");
  s80->setInitialAmount(80);
  s80->setHasOnlySubstanceUnits(true);

  RateRule* rr = moddef1->createRateRule();
  rr->setVariable("s80");
  rr->setMath(SBML_parseL3Formula("4*s80+3"));

  writeSBMLToFile(&document, "test20.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test20_flat.xml");
}

void createTest21() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Compartment* compartment = model->createCompartment();
  compartment->setId("C8");
  compartment->setSize(8);
  compartment->setConstant(false);
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(compartment->getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("C80");
  re->setConversionFactor("conv");
  Parameter conv(&sbmlns);
  conv.setConstant(true);
  conv.setId("conv");
  conv.setValue(0.1);
  model->addParameter(&conv);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* C80 = moddef1->createCompartment();
  C80->setConstant(false);
  C80->setId("C80");
  C80->setSize(80);

  RateRule* rr = moddef1->createRateRule();
  rr->setVariable("C80");
  rr->setMath(SBML_parseL3Formula("4*C80+3"));

  writeSBMLToFile(&document, "test21.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test21_flat.xml");
}

//A time conversion factor test
void createTest22() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv(&sbmlns);
  conv.setConstant(true);
  conv.setId("timeconv");
  conv.setValue(60);
  model->addParameter(&conv);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setTimeConversionFactor("timeconv");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter tpar(&sbmlns);
  tpar.setConstant(false);
  tpar.setId("t1");
  tpar.setValue(1);
  moddef1->addParameter(&tpar);
  tpar.setId("t2");
  moddef1->addParameter(&tpar);
  tpar.setId("t3");
  moddef1->addParameter(&tpar);
  tpar.setId("t4");
  moddef1->addParameter(&tpar);
  tpar.setId("t5");
  moddef1->addParameter(&tpar);

  CompSBasePlugin* comptpar = static_cast<CompSBasePlugin*>(tpar.getPlugin("comp"));
  ReplacedElement* re = comptpar->createReplacedElement();
  re->setSubmodelRef("sub1");
  tpar.setId("t1");
  re->setIdRef("t1");
  model->addParameter(&tpar);
  tpar.setId("t2");
  re->setIdRef("t2");
  model->addParameter(&tpar);
  tpar.setId("t3");
  re->setIdRef("t3");
  model->addParameter(&tpar);
  tpar.setId("t4");
  re->setIdRef("t4");
  model->addParameter(&tpar);
  tpar.setId("t5");
  re->setIdRef("t5");
  model->addParameter(&tpar);

  RateRule* rr = moddef1->createRateRule();
  rr->setVariable("t1");
  rr->setMath(SBML_parseL3Formula("time/t1+3"));

  InitialAssignment* ia = moddef1->createInitialAssignment();
  ia->setSymbol("t2");
  ia->setMath(SBML_parseL3Formula("time+3"));

  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("t3");
  ar->setMath(SBML_parseL3Formula("delay(t1,3)"));

  AlgebraicRule* alg = moddef1->createAlgebraicRule();
  alg->setMath(SBML_parseL3Formula("t4-delay(t3,time/2)"));

  Event* event = moddef1->createEvent();
  event->setUseValuesFromTriggerTime(true);
  Trigger* trigger = event->createTrigger();
  trigger->setInitialValue(true);
  trigger->setPersistent(true);
  trigger->setMath(SBML_parseL3Formula("time>3"));
  Delay* delay = event->createDelay();
  delay->setMath(SBML_parseL3Formula("1/time"));
  EventAssignment* ea = event->createEventAssignment();
  ea->setVariable("t5");
  ea->setMath(SBML_parseL3Formula("time"));

  Constraint* constraint=moddef1->createConstraint();
  constraint->setMath(SBML_parseL3Formula("time<t4*t5"));

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(.001);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("t3*time/(s1*delay(t5, 2e-1))"));

  model->addCompartment(compartment);
  model->addSpecies(s);
  Compartment* c = model->getCompartment("C");
  CompSBasePlugin* csbp = static_cast<CompSBasePlugin*>(c->getPlugin("comp"));
  ReplacedBy* rb = csbp->createReplacedBy();
  rb->setSubmodelRef("sub1");
  rb->setIdRef("C");
  s = model->getSpecies("s1");
  csbp = static_cast<CompSBasePlugin*>(s->getPlugin("comp"));
  rb = csbp->createReplacedBy();
  rb->setSubmodelRef("sub1");
  rb->setIdRef("s1");

  writeSBMLToFile(&document, "test22.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test22_flat.xml");
}

//Time and extent conversion factors test
void createTest23() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv(&sbmlns);
  conv.setConstant(true);
  conv.setId("timeconv");
  conv.setValue(60);
  model->addParameter(&conv);

  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("extentconv");
  conv2.setValue(1000);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setTimeConversionFactor("timeconv");
  sub1->setExtentConversionFactor("extentconv");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter tpar(&sbmlns);
  tpar.setConstant(false);
  tpar.setId("t1");
  tpar.setValue(1);
  moddef1->addParameter(&tpar);
  tpar.setId("t2");
  moddef1->addParameter(&tpar);
  tpar.setId("t3");
  moddef1->addParameter(&tpar);
  tpar.setId("t4");
  moddef1->addParameter(&tpar);
  tpar.setId("t5");
  moddef1->addParameter(&tpar);

  CompSBasePlugin* comptpar = static_cast<CompSBasePlugin*>(tpar.getPlugin("comp"));
  ReplacedElement* re = comptpar->createReplacedElement();
  re->setSubmodelRef("sub1");
  tpar.setId("t1");
  re->setIdRef("t1");
  model->addParameter(&tpar);
  tpar.setId("t2");
  re->setIdRef("t2");
  model->addParameter(&tpar);
  tpar.setId("t3");
  re->setIdRef("t3");
  model->addParameter(&tpar);
  tpar.setId("t4");
  re->setIdRef("t4");
  model->addParameter(&tpar);
  tpar.setId("t5");
  re->setIdRef("t5");
  model->addParameter(&tpar);

  RateRule* rr = moddef1->createRateRule();
  rr->setVariable("t1");
  rr->setMath(SBML_parseL3Formula("time*t1+3"));

  InitialAssignment* ia = moddef1->createInitialAssignment();
  ia->setSymbol("t2");
  ia->setMath(SBML_parseL3Formula("time+3"));

  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("t3");
  ar->setMath(SBML_parseL3Formula("delay(t1,3)"));

  AlgebraicRule* alg = moddef1->createAlgebraicRule();
  alg->setMath(SBML_parseL3Formula("t4-delay(t3,time)"));

  Event* event = moddef1->createEvent();
  event->setUseValuesFromTriggerTime(true);
  Trigger* trigger = event->createTrigger();
  trigger->setInitialValue(true);
  trigger->setPersistent(true);
  trigger->setMath(SBML_parseL3Formula("time>3"));
  Delay* delay = event->createDelay();
  delay->setMath(SBML_parseL3Formula("1/time"));
  EventAssignment* ea = event->createEventAssignment();
  ea->setVariable("t5");
  ea->setMath(SBML_parseL3Formula("time"));

  Constraint* constraint=moddef1->createConstraint();
  constraint->setMath(SBML_parseL3Formula("time<t4*t5"));

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(1);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("s1*t3*time/delay(t5, 2e4)"));

  writeSBMLToFile(&document, "test23.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test23_flat.xml");
}

//extent conversion factors test
void createTest24() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("extentconv");
  conv2.setValue(1000);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setExtentConversionFactor("extentconv");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(1);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("10"));

  writeSBMLToFile(&document, "test24.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test24_flat.xml");
}

//Test submodel conversion factors with submodels!
//extent conversion factors test
void createTest25() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("extentconv");
  conv2.setValue(1000);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef2");
  sub1->setExtentConversionFactor("extentconv");

  ModelDefinition* moddef2 = compdoc->createModelDefinition();
  moddef2->setId("moddef2");
  CompModelPlugin* compmoddef2 = static_cast<CompModelPlugin*>(moddef2->getPlugin("comp"));
  sub1 = compmoddef2->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(1);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("s1*time/delay(s1, 2e4)"));

  writeSBMLToFile(&document, "test25.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test25_flat.xml");
}

//Test submodel conversion factors with submodels!
//time conversion factors test
void createTest26() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("timeconv");
  conv2.setValue(60);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef2");
  sub1->setTimeConversionFactor("timeconv");

  ModelDefinition* moddef2 = compdoc->createModelDefinition();
  moddef2->setId("moddef2");
  CompModelPlugin* compmoddef2 = static_cast<CompModelPlugin*>(moddef2->getPlugin("comp"));
  sub1 = compmoddef2->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter tpar(&sbmlns);
  tpar.setConstant(false);
  tpar.setId("t1");
  moddef1->addParameter(&tpar);

  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("t1");
  ar->setMath(SBML_parseL3Formula("time+3"));

  writeSBMLToFile(&document, "test26.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test26_flat.xml");
}

//Test submodel conversion factors with submodels!
//double time conversion factors test!
void createTest27() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("timeconv");
  conv2.setValue(60);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef2");
  sub1->setTimeConversionFactor("timeconv");

  ModelDefinition* moddef2 = compdoc->createModelDefinition();
  moddef2->setId("moddef2");
  CompModelPlugin* compmoddef2 = static_cast<CompModelPlugin*>(moddef2->getPlugin("comp"));
  sub1 = compmoddef2->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setTimeConversionFactor("timeconv");
  moddef2->addParameter(&conv2);

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter tpar(&sbmlns);
  tpar.setConstant(false);
  tpar.setId("t1");
  moddef1->addParameter(&tpar);

  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("t1");
  ar->setMath(SBML_parseL3Formula("time+3"));

  writeSBMLToFile(&document, "test27.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test27_flat.xml");
}

//Test submodel conversion factors with submodels!
//extent conversion factors test
void createTest28() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));

  Parameter conv(&sbmlns);
  conv.setConstant(true);
  conv.setId("timeconv");
  conv.setValue(60);
  model->addParameter(&conv);

  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("extentconv");
  conv2.setValue(10);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef3");
  sub1->setTimeConversionFactor("timeconv");
  sub1->setExtentConversionFactor("extentconv");

  ModelDefinition* moddef3 = compdoc->createModelDefinition();
  moddef3->setId("moddef3");
  moddef3->addParameter(&conv);
  moddef3->addParameter(&conv2);
  CompModelPlugin* compmoddef3 = static_cast<CompModelPlugin*>(moddef3->getPlugin("comp"));
  sub1 = compmoddef3->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef2");
  sub1->setTimeConversionFactor("timeconv");
  sub1->setExtentConversionFactor("extentconv");

  ModelDefinition* moddef2 = compdoc->createModelDefinition();
  moddef2->setId("moddef2");
  moddef2->addParameter(&conv);
  moddef2->addParameter(&conv2);
  CompModelPlugin* compmoddef2 = static_cast<CompModelPlugin*>(moddef2->getPlugin("comp"));
  sub1 = compmoddef2->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setTimeConversionFactor("timeconv");
  sub1->setExtentConversionFactor("extentconv");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(1);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("s1*time/delay(s1, 2e4)"));

  writeSBMLToFile(&document, "test28.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test28_flat.xml");
}

//Test the deletion of a rate rule.
void createTest29() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  p.setValue(8);
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(p.getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("p80");
  model->addParameter(&p);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  Deletion* deletion = sub1->createDeletion();
  deletion->setMetaIdRef("p80_raterule");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  p80->setValue(80);

  RateRule* rr = moddef1->createRateRule();
  rr->setVariable("p80");
  rr->setMath(SBML_parseL3Formula("4*p80+3"));
  rr->setMetaId("p80_raterule");

  writeSBMLToFile(&document, "test29.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test29_flat.xml");
}

//A replaced rate rule.
void createTest30() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  p.setValue(8);
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(p.getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("p80");
  model->addParameter(&p);

  RateRule* rr = model->createRateRule();
  rr->setVariable("p8");
  rr->setMath(SBML_parseL3Formula("3"));
  rr->setMetaId("p8_raterule");
  compp = static_cast<CompSBasePlugin*>(rr->getPlugin("comp"));
  re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setMetaIdRef("p80_raterule");

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  p80->setValue(80);

  rr = moddef1->createRateRule();
  rr->setVariable("p80");
  rr->setMath(SBML_parseL3Formula("4*p80+3"));
  rr->setMetaId("p80_raterule");

  writeSBMLToFile(&document, "test30.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test30_flat.xml");
}

//A deleted rate rule, effectively replaced.
void createTest31() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  p.setValue(8);
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(p.getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("p80");
  model->addParameter(&p);

  RateRule* rr = model->createRateRule();
  rr->setVariable("p8");
  rr->setMath(SBML_parseL3Formula("3"));
  rr->setMetaId("p8_raterule");

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  Deletion* deletion = sub1->createDeletion();
  deletion->setMetaIdRef("p80_raterule");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  p80->setValue(80);

  rr = moddef1->createRateRule();
  rr->setVariable("p80");
  rr->setMath(SBML_parseL3Formula("4*p80+3"));
  rr->setMetaId("p80_raterule");

  writeSBMLToFile(&document, "test31.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test31_flat.xml");
}

//Tests 32 and 33 were created by Antimony directly.

//Testing event deletion
void createTest34() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  Deletion* deletion = sub1->createDeletion();
  deletion->setIdRef("E0");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter tpar(&sbmlns);
  tpar.setConstant(false);
  tpar.setId("t1");
  tpar.setValue(1);
  moddef1->addParameter(&tpar);
  tpar.setId("t2");
  moddef1->addParameter(&tpar);

  Event* event = moddef1->createEvent();
  event->setId("E0");
  event->setUseValuesFromTriggerTime(true);
  Trigger* trigger = event->createTrigger();
  trigger->setInitialValue(true);
  trigger->setPersistent(true);
  trigger->setMath(SBML_parseL3Formula("time>3"));
  Delay* delay = event->createDelay();
  delay->setMath(SBML_parseL3Formula("1/time"));
  delay->setMetaId("E0_delay");
  EventAssignment* ea = event->createEventAssignment();
  ea->setVariable("t1");
  ea->setMath(SBML_parseL3Formula("3.3"));
  ea->setMetaId("E0_asnt1");
  ea = event->createEventAssignment();
  ea->setVariable("t2");
  ea->setMath(SBML_parseL3Formula("5.5"));
  ea->setMetaId("E0_asnt2");
  Priority* priority = event->createPriority();
  priority->setMath(SBML_parseL3Formula("10"));
  priority->setMetaId("E0_priority");

  writeSBMLToFile(&document, "test34.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test34_flat.xml");
}

//Testing event delay deletion
void createTest35() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  Deletion* deletion = sub1->createDeletion();
  deletion->setMetaIdRef("E0_delay");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter tpar(&sbmlns);
  tpar.setConstant(false);
  tpar.setId("t1");
  tpar.setValue(1);
  moddef1->addParameter(&tpar);
  tpar.setId("t2");
  moddef1->addParameter(&tpar);

  Event* event = moddef1->createEvent();
  event->setId("E0");
  event->setUseValuesFromTriggerTime(true);
  Trigger* trigger = event->createTrigger();
  trigger->setInitialValue(true);
  trigger->setPersistent(true);
  trigger->setMath(SBML_parseL3Formula("time>3"));
  Delay* delay = event->createDelay();
  delay->setMath(SBML_parseL3Formula("1/time"));
  delay->setMetaId("E0_delay");
  EventAssignment* ea = event->createEventAssignment();
  ea->setVariable("t1");
  ea->setMath(SBML_parseL3Formula("3.3"));
  ea->setMetaId("E0_asnt1");
  ea = event->createEventAssignment();
  ea->setVariable("t2");
  ea->setMath(SBML_parseL3Formula("5.5"));
  ea->setMetaId("E0_asnt2");
  Priority* priority = event->createPriority();
  priority->setMath(SBML_parseL3Formula("10"));
  priority->setMetaId("E0_priority");

  writeSBMLToFile(&document, "test35.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test35_flat.xml");
}

//Testing event priority deletion
void createTest36() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  Deletion* deletion = sub1->createDeletion();
  deletion->setMetaIdRef("E0_priority");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter tpar(&sbmlns);
  tpar.setConstant(false);
  tpar.setId("t1");
  tpar.setValue(1);
  moddef1->addParameter(&tpar);
  tpar.setId("t2");
  moddef1->addParameter(&tpar);

  Event* event = moddef1->createEvent();
  event->setId("E0");
  event->setUseValuesFromTriggerTime(true);
  Trigger* trigger = event->createTrigger();
  trigger->setInitialValue(true);
  trigger->setPersistent(true);
  trigger->setMath(SBML_parseL3Formula("time>3"));
  Delay* delay = event->createDelay();
  delay->setMath(SBML_parseL3Formula("1/time"));
  delay->setMetaId("E0_delay");
  EventAssignment* ea = event->createEventAssignment();
  ea->setVariable("t1");
  ea->setMath(SBML_parseL3Formula("3.3"));
  ea->setMetaId("E0_asnt1");
  ea = event->createEventAssignment();
  ea->setVariable("t2");
  ea->setMath(SBML_parseL3Formula("5.5"));
  ea->setMetaId("E0_asnt2");
  Priority* priority = event->createPriority();
  priority->setMath(SBML_parseL3Formula("10"));
  priority->setMetaId("E0_priority");

  writeSBMLToFile(&document, "test36.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test36_flat.xml");
}

//Testing event assignment deletion
void createTest37() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  Deletion* deletion = sub1->createDeletion();
  deletion->setMetaIdRef("E0_asnt1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter tpar(&sbmlns);
  tpar.setConstant(false);
  tpar.setId("t1");
  tpar.setValue(1);
  moddef1->addParameter(&tpar);
  tpar.setId("t2");
  moddef1->addParameter(&tpar);

  Event* event = moddef1->createEvent();
  event->setId("E0");
  event->setUseValuesFromTriggerTime(true);
  Trigger* trigger = event->createTrigger();
  trigger->setInitialValue(true);
  trigger->setPersistent(true);
  trigger->setMath(SBML_parseL3Formula("time>3"));
  Delay* delay = event->createDelay();
  delay->setMath(SBML_parseL3Formula("1/time"));
  delay->setMetaId("E0_delay");
  EventAssignment* ea = event->createEventAssignment();
  ea->setVariable("t1");
  ea->setMath(SBML_parseL3Formula("3.3"));
  ea->setMetaId("E0_asnt1");
  ea = event->createEventAssignment();
  ea->setVariable("t2");
  ea->setMath(SBML_parseL3Formula("5.5"));
  ea->setMetaId("E0_asnt2");
  Priority* priority = event->createPriority();
  priority->setMath(SBML_parseL3Formula("10"));
  priority->setMetaId("E0_priority");

  writeSBMLToFile(&document, "test37.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test37_flat.xml");
}

//Testing event delay, priority, and assignment deletion
void createTest38() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  Deletion* deletion = sub1->createDeletion();
  deletion->setMetaIdRef("E0_delay");
  deletion = sub1->createDeletion();
  deletion->setMetaIdRef("E0_priority");
  deletion = sub1->createDeletion();
  deletion->setMetaIdRef("E0_asnt2");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter tpar(&sbmlns);
  tpar.setConstant(false);
  tpar.setId("t1");
  tpar.setValue(1);
  moddef1->addParameter(&tpar);
  tpar.setId("t2");
  moddef1->addParameter(&tpar);

  Event* event = moddef1->createEvent();
  event->setId("E0");
  event->setUseValuesFromTriggerTime(true);
  Trigger* trigger = event->createTrigger();
  trigger->setInitialValue(true);
  trigger->setPersistent(true);
  trigger->setMath(SBML_parseL3Formula("time>3"));
  Delay* delay = event->createDelay();
  delay->setMath(SBML_parseL3Formula("1/time"));
  delay->setMetaId("E0_delay");
  EventAssignment* ea = event->createEventAssignment();
  ea->setVariable("t1");
  ea->setMath(SBML_parseL3Formula("3.3"));
  ea->setMetaId("E0_asnt1");
  ea = event->createEventAssignment();
  ea->setVariable("t2");
  ea->setMath(SBML_parseL3Formula("5.5"));
  ea->setMetaId("E0_asnt2");
  Priority* priority = event->createPriority();
  priority->setMath(SBML_parseL3Formula("10"));
  priority->setMetaId("E0_priority");

  writeSBMLToFile(&document, "test38.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test38_flat.xml");
}

//Delete local parameter
void createTest39() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  Deletion* del = sub1->createDeletion();
  del->setMetaIdRef("J0_p1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(.001);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("p1*s1"));
  LocalParameter* lp = kl->createLocalParameter();
  lp->setId("p1");
  lp->setMetaId("J0_p1");
  lp->setConstant(true);
  lp->setValue(10);

  Parameter* param = moddef1->createParameter();
  param->setId("p1");
  param->setConstant(true);
  param->setValue(100);

  writeSBMLToFile(&document, "test39.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test39_flat.xml");
}

//Replace local parameter
void createTest40() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(.001);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("p1*s1"));
  LocalParameter* lp = kl->createLocalParameter();
  lp->setId("p1");
  lp->setMetaId("J0_p1");
  lp->setConstant(true);
  lp->setValue(10);

  Parameter* param = model->createParameter();
  param->setId("new_p1");
  param->setMetaId("new_p1_metaid");
  param->setConstant(true);
  param->setValue(100);
  CompSBasePlugin* csp = static_cast<CompSBasePlugin*>(param->getPlugin("comp"));
  ReplacedElement* re = csp->createReplacedElement();
  re->setMetaIdRef("J0_p1");
  re->setSubmodelRef("sub1");

  writeSBMLToFile(&document, "test40.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test40_flat.xml");
}

//Submodel has a local parameter which doesn't change.
void createTest41() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(.001);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("p1*s1"));
  LocalParameter* lp = kl->createLocalParameter();
  lp->setId("p1");
  lp->setMetaId("J0_p1");
  lp->setConstant(true);
  lp->setValue(10);

  Parameter* param = moddef1->createParameter();
  param->setId("p1");
  param->setMetaId("unused_p1");
  param->setConstant(true);
  param->setValue(100);

  writeSBMLToFile(&document, "test41.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test41_flat.xml");
}

//Reading/writing SBO terms on ports and stuff.
void createPortSBO() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));

  Parameter* param = model->createParameter();
  param->setId("p1");
  param->setConstant(true);

  Port* port = compmod->createPort();
  port->setId("port1");
  port->setIdRef("p1");
  port->setSBOTerm(10);

  writeSBMLToFile(&document, "port_sbo.xml");
}

//Test the deletion of a rate rule via a port.
void createTest42() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  p.setValue(8);
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(p.getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("p80");
  model->addParameter(&p);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  Deletion* deletion = sub1->createDeletion();
  deletion->setPortRef("rr_port");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  p80->setValue(80);

  RateRule* rr = moddef1->createRateRule();
  rr->setVariable("p80");
  rr->setMath(SBML_parseL3Formula("4*p80+3"));
  rr->setMetaId("p80_raterule");

  CompModelPlugin* subcompmod = static_cast<CompModelPlugin*>(moddef1->getPlugin("comp"));
  Port* port = subcompmod->createPort();
  port->setId("rr_port");
  port->setMetaIdRef("p80_raterule");

  writeSBMLToFile(&document, "test42.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test42_flat.xml");
}


//A replaced rate rule that points to a port.
void createTest43() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  p.setValue(8);
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(p.getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("p80");
  model->addParameter(&p);

  RateRule* rr = model->createRateRule();
  rr->setVariable("p8");
  rr->setMath(SBML_parseL3Formula("3"));
  rr->setMetaId("p8_raterule");
  compp = static_cast<CompSBasePlugin*>(rr->getPlugin("comp"));
  re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setPortRef("rr_port");

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  p80->setValue(80);

  rr = moddef1->createRateRule();
  rr->setVariable("p80");
  rr->setMath(SBML_parseL3Formula("4*p80+3"));
  rr->setMetaId("p80_raterule");

  CompModelPlugin* subcompmod = static_cast<CompModelPlugin*>(moddef1->getPlugin("comp"));
  Port* port = subcompmod->createPort();
  port->setId("rr_port");
  port->setMetaIdRef("p80_raterule");

  writeSBMLToFile(&document, "test43.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test43_flat.xml");
}

//extent conversion factor/reaction reference test
void createTest44() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("extentconv");
  conv2.setValue(1000);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setExtentConversionFactor("extentconv");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(1);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("10"));

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  
  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("p80");
  ar->setMath(SBML_parseL3Formula("J0+6"));

  writeSBMLToFile(&document, "test44.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test44_flat.xml");
}

//A time conversion factor/conversion factor for a rate rule test
void createTest45() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv(&sbmlns);
  conv.setConstant(true);
  conv.setId("timeconv");
  conv.setValue(60);
  model->addParameter(&conv);

  conv.setId("paramconv");
  conv.setValue(.01);
  model->addParameter(&conv);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setTimeConversionFactor("timeconv");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Parameter tpar(&sbmlns);
  tpar.setConstant(false);
  tpar.setId("t1");
  tpar.setValue(1);
  moddef1->addParameter(&tpar);

  CompSBasePlugin* comptpar = static_cast<CompSBasePlugin*>(tpar.getPlugin("comp"));
  ReplacedElement* re = comptpar->createReplacedElement();
  re->setConversionFactor("paramconv");
  re->setSubmodelRef("sub1");
  tpar.setId("t1");
  re->setIdRef("t1");
  model->addParameter(&tpar);

  RateRule* rr = moddef1->createRateRule();
  rr->setVariable("t1");
  rr->setMath(SBML_parseL3Formula("time/t1+3"));

  writeSBMLToFile(&document, "test45.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test45_flat.xml");
}

//time conversion factor/reaction reference test
void createTest46() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("timeconv");
  conv2.setValue(60);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setTimeConversionFactor("timeconv");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(1);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("10"));

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  
  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("p80");
  ar->setMath(SBML_parseL3Formula("J0+6"));

  writeSBMLToFile(&document, "test46.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test46_flat.xml");
}

//extent and time conversion factor/reaction reference test
void createTest47() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("extentconv");
  conv2.setValue(1000);
  model->addParameter(&conv2);
  conv2.setId("timeconv");
  conv2.setValue(60);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setExtentConversionFactor("extentconv");
  sub1->setTimeConversionFactor("timeconv");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(1);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("10"));

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  
  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("p80");
  ar->setMath(SBML_parseL3Formula("J0+6"));

  writeSBMLToFile(&document, "test47.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test47_flat.xml");
}

//extent and time conversion factor/reaction with no kinetic law reference test
void createTest48() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("extentconv");
  conv2.setValue(1000);
  model->addParameter(&conv2);
  conv2.setId("timeconv");
  conv2.setValue(60);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setExtentConversionFactor("extentconv");
  sub1->setTimeConversionFactor("timeconv");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(1);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  
  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("p80");
  ar->setMath(SBML_parseL3Formula("J0+6"));

  writeSBMLToFile(&document, "test48.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test48_flat.xml");
}

//extent and time conversion factor/reaction reference test, where the reaction was replaced.
void createTest49() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("extentconv");
  conv2.setValue(1000);
  model->addParameter(&conv2);
  conv2.setId("timeconv");
  conv2.setValue(60);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setExtentConversionFactor("extentconv");
  sub1->setTimeConversionFactor("timeconv");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  model->addCompartment(compartment);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(1);
  model->addSpecies(s);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("10"));
  model->addReaction(rxn);

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  
  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("p80");
  ar->setMath(SBML_parseL3Formula("J0+6"));

  //Now replace the submodel reaction with the top-level reaction.
  rxn = model->getReaction(0);
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(rxn->getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("J0");


  writeSBMLToFile(&document, "test49.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test49_flat.xml");
}

//extent and time conversion factor/reaction reference test, where the reaction was replaced properly, and given its own conversion factor.
void createTest50() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  // create the document
  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model* model = document.createModel();
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Parameter conv2(&sbmlns);
  conv2.setConstant(true);
  conv2.setId("extentconv");
  conv2.setValue(1000);
  model->addParameter(&conv2);
  conv2.setId("timeconv");
  conv2.setValue(60);
  model->addParameter(&conv2);
  conv2.setId("extentpertimeconv");
  conv2.setValue(1000.0/60.0);
  model->addParameter(&conv2);

  Submodel* sub1 = compmod->createSubmodel();
  sub1->setId("sub1");
  sub1->setModelRef("moddef1");
  sub1->setExtentConversionFactor("extentconv");
  sub1->setTimeConversionFactor("timeconv");

  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("moddef1");

  Compartment* compartment = moddef1->createCompartment();
  compartment->setId("C");
  compartment->setConstant(true);
  compartment->setSize(1);
  model->addCompartment(compartment);
  Species* s = moddef1->createSpecies();
  s->setConstant(false);
  s->setBoundaryCondition(false);
  s->setCompartment("C");
  s->setHasOnlySubstanceUnits(true);
  s->setId("s1");
  s->setInitialAmount(1);
  model->addSpecies(s);
  Reaction* rxn = moddef1->createReaction();
  rxn->setReversible(true);
  rxn->setFast(false);
  rxn->setId("J0");
  SpeciesReference* sr = rxn->createProduct();
  sr->setSpecies("s1");
  sr->setConstant(true);
  KineticLaw* kl = rxn->createKineticLaw();
  kl->setMath(SBML_parseL3Formula("10"));
  model->addReaction(rxn);

  Parameter* p80 = moddef1->createParameter();
  p80->setConstant(false);
  p80->setId("p80");
  
  AssignmentRule* ar = moddef1->createAssignmentRule();
  ar->setVariable("p80");
  ar->setMath(SBML_parseL3Formula("J0+6"));

  //Now replace the submodel reaction with the top-level reaction.
  rxn = model->getReaction(0);
  CompSBasePlugin* compp = static_cast<CompSBasePlugin*>(rxn->getPlugin("comp"));
  ReplacedElement* re = compp->createReplacedElement();
  re->setSubmodelRef("sub1");
  re->setIdRef("J0");
  re->setConversionFactor("extentpertimeconv");


  writeSBMLToFile(&document, "test50.xml");
  
  SBMLDocument *flatdoc = document.clone();

  // flattening converter
  ConversionProperties* props = new ConversionProperties();
  props->addOption("flatten comp");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);

  converter->setDocument(flatdoc);
  converter->convert();

  writeSBMLToFile(flatdoc, "test50_flat.xml");
}

