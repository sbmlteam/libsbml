/**
 * @file    example3.cpp
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
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document->getPlugin("comp"));
  compdoc->setRequired(true);

  // Create units
  Unit moles(&sbmlns);
  moles.setKind(UNIT_KIND_MOLE);
  moles.setExponent(1);
  moles.setScale(0);
  moles.setMultiplier(1);

  Unit millimoles = moles;
  millimoles.setScale(-3);

  Unit perliter(&sbmlns);
  perliter.setKind(UNIT_KIND_LITRE);
  perliter.setExponent(-1);
  perliter.setScale(0);
  perliter.setMultiplier(1);

  Unit permole = moles;
  permole.setExponent(-1);

  UnitDefinition molar(&sbmlns);
  molar.setId("M");
  molar.addUnit(&moles);
  molar.addUnit(&perliter);

  UnitDefinition millimolar(&sbmlns);
  millimolar.setId("mM");
  millimolar.addUnit(&millimoles);
  millimolar.addUnit(&perliter);

  UnitDefinition mMperM(&sbmlns);
  mMperM.setId("mM_per_M");
  mMperM.addUnit(&millimoles);
  mMperM.addUnit(&permole);

  UnitDefinition millimole(&sbmlns);
  millimole.setId("millimole");
  millimole.addUnit(&millimoles);

  // create the Model
  Model* model=document->createModel();
  model->setId("glycolysis_with_ports");

  model->addUnitDefinition(&millimole);
  model->addUnitDefinition(&mMperM);
  
  // create a parameter to be a conversion factor
  Parameter* cf = model->createParameter();
  cf->setId("cf");
  cf->setConstant(true);
  cf->setValue(1000);
  cf->setUnits("mM_per_M");

  //Create a compartment
  Compartment compartment(&sbmlns);
  compartment.setId("cell");
  compartment.setSpatialDimensions(3.0);
  compartment.setSize(1);
  compartment.setConstant(true);
  compartment.setUnits("litre");
  compartment.setSBOTerm(410);

  // Create a species
  Species sp(&sbmlns);
  sp.setId("A");
  sp.setConstant(false);
  sp.setUnits("millimole");
  sp.setCompartment("cell");
  sp.setHasOnlySubstanceUnits(false);
  sp.setBoundaryCondition(false);

  //Create a reaction
  Reaction rxn(&sbmlns);
  rxn.setReversible(false);
  rxn.setFast(false);
  rxn.setId("A_degradation");
  SpeciesReference* sr = rxn.createReactant();
  sr->setSpecies("A");
  sr->setConstant(true);
  sr->setStoichiometry(1);
  KineticLaw* kl = rxn.createKineticLaw();
  ASTNode* astn = SBML_parseFormula("4");
  kl->setMath(astn);

  //Add just the species to the containing model
  model->addCompartment(&compartment);
  model->addSpecies(&sp);

  // Create two model definitions
  ModelDefinition* md1 = compdoc->createModelDefinition();
  md1->setId("mod1");
  md1->addUnitDefinition(&millimolar);
  md1->addCompartment(&compartment);
  md1->addSpecies(&sp);
  md1->addReaction(&rxn);

  ModelDefinition* md2 = compdoc->createModelDefinition();
  md2->setId("mod2");
  md2->addUnitDefinition(&molar);
  md2->addCompartment(&compartment);
  sp.setUnits("mole");
  md2->addSpecies(&sp);
  md2->addReaction(&rxn);

  // Create two submodels
  CompModelPlugin* mplugin = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Submodel* submod1 = mplugin->createSubmodel();
  submod1->setId("m1");
  submod1->setModelRef("mod1");

  Submodel* submod2 = mplugin->createSubmodel();
  submod2->setId("m2");
  submod2->setModelRef("mod2");

  //Now synchronize the species:
  Species* sp1 = model->getSpecies("A");
  CompSBasePlugin* spplug = static_cast<CompSBasePlugin*>(sp1->getPlugin("comp"));
  ReplacedElement re;
  re.setSubmodelRef("m1");
  re.setIdRef("A");
  spplug->addReplacedElement(&re);
  re.setSubmodelRef("m2");
  re.setConversionFactor("cf");
  spplug->addReplacedElement(&re);

  writeSBMLToFile(document,"comp_example3.xml");
  delete document;
  document = readSBMLFromFile("comp_example3.xml");
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
    writeSBMLToFile(document, "comp_example3_rt.xml");
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
