/**
 * @file    spec_example1.cpp
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

  //Create our submodel
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document->getPlugin("comp"));
  compdoc->setRequired(true);
  ModelDefinition* mod1 = compdoc->createModelDefinition();
  mod1->setId("enzyme");
  mod1->setName("enzyme");
  Compartment* comp=mod1->createCompartment();
  comp->setSpatialDimensions((unsigned int)3);
  comp->setConstant(true);
  comp->setId("comp");
  comp->setSize(1L);
  Species spec(&sbmlns);
  spec.setCompartment("comp");
  spec.setHasOnlySubstanceUnits(false);
  spec.setConstant(false);
  spec.setBoundaryCondition(false);
  spec.setId("S");
  mod1->addSpecies(&spec);
  spec.setId("E");
  mod1->addSpecies(&spec);
  spec.setId("D");
  mod1->addSpecies(&spec);
  spec.setId("ES");
  mod1->addSpecies(&spec);
  Reaction rxn(&sbmlns);
  rxn.setReversible(true);
  rxn.setFast(false);
  Reaction rxn2(rxn);
  rxn.setId("J0");
  rxn2.setId("J1");
  SpeciesReference sr(&sbmlns);
  sr.setConstant(true);
  sr.setStoichiometry(1);
  sr.setSpecies("S");
  rxn.addReactant(&sr);
  sr.setSpecies("E");
  rxn.addReactant(&sr);
  rxn2.addProduct(&sr);
  sr.setSpecies("ES");
  rxn.addProduct(&sr);
  rxn2.addReactant(&sr);
  sr.setSpecies("D");
  rxn2.addProduct(&sr);

  mod1->addReaction(&rxn);
  mod1->addReaction(&rxn2);

  // create the Model
  Model* model=document->createModel();
  model->setId("aggregate");
  
  // Create a submodel
  CompModelPlugin* mplugin = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Submodel* submod1 = mplugin->createSubmodel();
  submod1->setId("submod1");
  submod1->setModelRef("enzyme");

  Submodel submod2;
  submod2.setId("submod2");
  submod2.setModelRef("enzyme");
  mplugin->addSubmodel(&submod2);

  writeSBMLToFile(document,"eg-simple-aggregate.xml");
  writeSBMLToFile(document,"enzyme_model.xml");
  delete document;
  document = readSBMLFromFile("enzyme_model.xml");
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
    writeSBMLToFile(document, "enzyme_model_rt.xml");
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
