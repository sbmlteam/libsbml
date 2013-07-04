/**
 * @file    spec_example3.cpp
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

#ifdef WIN32
#include <conio.h>
#endif

LIBSBML_CPP_NAMESPACE_USE
using namespace std;

static SBMLExtensionRegister<CompExtension> compExtensionRegistry;

int main(int argc,char** argv){

  int retval = 0;
  SBMLNamespaces sbmlns(3,1,"comp",1);

  // create the document
  SBMLDocument *document = new SBMLDocument(&sbmlns);

  //Define the external model definition
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document->getPlugin("comp"));
  compdoc->setRequired(true);
  ExternalModelDefinition* extmod = compdoc->createExternalModelDefinition();
  extmod->setId("ExtMod1");
  extmod->setSource("enzyme_model.xml");
  extmod->setModelRef("enzyme");

  //Define the 'simple' model
  ModelDefinition* mod1 = compdoc->createModelDefinition();
  mod1->setId("simple");
  Compartment* comp=mod1->createCompartment();
  comp->setSpatialDimensions((unsigned int)3);
  comp->setConstant(true);
  comp->setId("comp");
  comp->setSize(1L);

  Species spec(&sbmlns); //We have to construct it this way because we get the comp plugin from it later.
  spec.setCompartment("comp");
  spec.setHasOnlySubstanceUnits(false);
  spec.setConstant(false);
  spec.setBoundaryCondition(false);
  spec.setId("S");
  spec.setInitialConcentration(5);
  mod1->addSpecies(&spec);
  spec.setId("D");
  spec.setInitialConcentration(10);
  mod1->addSpecies(&spec);

  Reaction rxn(&sbmlns);
  rxn.setReversible(true);
  rxn.setFast(false);
  rxn.setId("J0");

  SpeciesReference sr(&sbmlns);
  sr.setConstant(true);
  sr.setStoichiometry(1);
  sr.setSpecies("S");
  rxn.addReactant(&sr);
  sr.setSpecies("D");
  rxn.addProduct(&sr);

  mod1->addReaction(&rxn);

  CompModelPlugin* mod1plug = static_cast<CompModelPlugin*>(mod1->getPlugin("comp"));
  Port port;
  port.setId("S_port");
  port.setIdRef("S");
  mod1plug->addPort(&port);

  Port* port2 = mod1plug->createPort();
  port2->setId("D_port");
  port2->setIdRef("D");

  port.setId("comp_port");
  port.setIdRef("comp");
  mod1plug->addPort(&port);

  port.setId("J0_port");
  port.setIdRef("J0");
  mod1plug->addPort(&port);

  // create the Model
  Model* model=document->createModel();
  model->setId("complexified");
  
  // Set the submodels
  CompModelPlugin* mplugin = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Submodel* submod1 = mplugin->createSubmodel();
  submod1->setId("A");
  submod1->setModelRef("ExtMod1");
  Submodel* submod2 = mplugin->createSubmodel();
  submod2->setId("B");
  submod2->setModelRef("simple");
  Deletion* del = submod2->createDeletion();
  del->setPortRef("J0_port");

  // Synchronize the compartments
  Compartment* mcomp=model->createCompartment();
  mcomp->setSpatialDimensions((unsigned int)3);
  mcomp->setConstant(true);
  mcomp->setId("comp");
  mcomp->setSize(1L);
  CompSBasePlugin* compartplug = static_cast<CompSBasePlugin*>(mcomp->getPlugin("comp"));
  ReplacedElement re;
  re.setIdRef("comp");
  re.setSubmodelRef("A");
  compartplug->addReplacedElement(&re);
  re.setSubmodelRef("B");
  re.unsetIdRef();
  re.setPortRef("comp_port");
  compartplug->addReplacedElement(&re);

  //Synchronize the species
  spec.setId("S");
  spec.unsetInitialConcentration();
  CompSBasePlugin* specplug = static_cast<CompSBasePlugin*>(spec.getPlugin("comp"));
  ReplacedElement* sre = specplug->createReplacedElement();
  sre->setSubmodelRef("A");
  sre->setIdRef("S");
  ReplacedBy* srb = specplug->createReplacedBy();
  srb->setSubmodelRef("B");
  srb->setPortRef("S_port");
  model->addSpecies(&spec);

  spec.setId("D");
  sre->setIdRef("D");
  srb->setPortRef("D_port");
  model->addSpecies(&spec);

  writeSBMLToFile(document,"eg-ports.xml");
  writeSBMLToFile(document,"spec_example3.xml");
  delete document;
  document = readSBMLFromFile("spec_example3.xml");
  if (document == NULL) {
    cout << "Error reading back in file." << endl;
    retval = -1;
  }
  else {
    document->setConsistencyChecks(LIBSBML_CAT_UNITS_CONSISTENCY, false);
    document->checkConsistency();
    if (document->getErrorLog()->getNumFailsWithSeverity(2) > 0 || document->getErrorLog()->getNumFailsWithSeverity(3) > 0){
      stringstream errorstream;
      document->printErrors(errorstream);
      cout << "Errors encoutered when round-tripping  SBML file: \n" <<  errorstream.str() << endl;
      retval = -1;
    }
    writeSBMLToFile(document, "spec_example3_rt.xml");
    delete document;
  }
#ifdef WIN32
  if (retval != 0) {
    cout << "(Press any key to exit.)" << endl;
    _getch();
  }
#endif
  return retval;
}
