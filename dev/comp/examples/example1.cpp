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

#ifdef WIN32
#include <conio.h>
#endif

LIBSBML_CPP_NAMESPACE_USE
using namespace std;

static SBMLExtensionRegister<CompExtension> compExtensionRegistry;

int main(int argc,char** argv){

  int retval = 0;
  int rv;
  SBMLNamespaces sbmlns(3,1,"comp",1);

  // create the document
  SBMLDocument *document = new SBMLDocument(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document->getPlugin("comp"));
  compdoc->setRequired(true);

  // create the Model
  Model* model=document->createModel();
  CompModelPlugin* mplugin = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  
  // create a parameter to be a conversion factor
  Parameter* param3 = model->createParameter();
  param3->setId("lcf");
  param3->setConstant(true);

  // create a replacement parameter
  Parameter* parameter = model->createParameter();
  parameter->setId("x");
  parameter->setConstant(true);

  // Convert parameter to the plugin version so we can add the new attributes and replacements to it.
  CompSBasePlugin* splugin = static_cast<CompSBasePlugin*>(parameter->getPlugin("comp"));

  // Add a replaced element.
  ReplacedElement* rep1 = splugin->createReplacedElement();
  rv = rep1->setSubmodelRef("submod1");
  rv = rep1->setConversionFactor("x_conv");
  rv = rep1->setIdRef("param1");

  // Add a second replaced element in a different way.
  ReplacedElement rep2;
  rv = rep2.setSubmodelRef("submod2");
  rv = rep2.setDeletion("del1");
  rv = splugin->addReplacedElement(&rep2);

  //Now create a replaced element that points into a submodel.
  rep2.unsetDeletion();
  rep2.setIdRef("submod2");
  SBaseRef* sbr5 = rep2.createSBaseRef();
  sbr5->setIdRef("submodelG");
  SBaseRef* sbr6 = sbr5->createSBaseRef();
  sbr6->setIdRef("buriedElement");
  splugin->addReplacedElement(&rep2);
      
  // create a parameter to be a conversion factor
  Parameter* param2 = model->createParameter();
  param2->setId("x_conv");
  param2->setMetaId("_110013");
  param2->setConstant(true);

  //Testing getSBMLDocument and getParentSBMLObject.
  SBMLDocument* doctest = splugin->getSBMLDocument();
  SBase* parenttest = splugin->getParentSBMLObject();
  if (doctest == NULL) {
    cout << "SBML document not set, for unknown reason.";
  }
  if (parenttest == NULL) {
    cout << "Parent of 'splugin' not found, for unknown reason.";
  }

  // Create model definitions and external model definitions.
  ModelDefinition* moddef1 = compdoc->createModelDefinition();
  moddef1->setId("Mod1");
  Parameter* m1param1 = moddef1->createParameter();
  m1param1->setId("param1");
  m1param1->setConstant(true);
  Parameter* m1param2 = moddef1->createParameter();
  m1param2->setId("param2");
  m1param2->setConstant(false);
  m1param2->setValue(3.2);

  ModelDefinition moddef2;
  moddef2.setId("Mod2");
  Parameter* subparam2 = moddef2.createParameter();
  subparam2->setId("subparam2");
  subparam2->setConstant(false);
  compdoc->addModelDefinition(&moddef2);


  ExternalModelDefinition* extmod1 = compdoc->createExternalModelDefinition();
  extmod1->setId("ExtMod1");
  extmod1->setSource("urn:miriam:biomodels.db:BIOMD0000000127");

  ExternalModelDefinition extmod2;
  extmod2.setId("ExtMod2");
  extmod2.setSource("otherfile.xml");
  extmod2.setModelRef("modelnamethere");
  extmod2.setMd5("406022s908ge74sklj");
  compdoc->addExternalModelDefinition(&extmod2);

  // Create submodels
  Submodel* submod1 = mplugin->createSubmodel();
  submod1->setId("submod1");
  submod1->setModelRef("Mod1");
  Deletion* del1 = submod1->createDeletion();
  del1->setId("deletionA");
  del1->setIdRef("param2");

  Submodel submod2;
  submod2.setId("submod2");
  submod2.setModelRef("ExtMod1");
  submod2.setSubstanceConversionFactor("subcf");
  submod2.setTimeConversionFactor("tcf");
  submod2.setExtentConversionFactor("xtf");
  Deletion del2;
  del2.setId("deletionB");
  del2.setMetaIdRef("_0010110");
  rv = submod2.addDeletion(&del2);
  del2.setId("deletionC");
  del2.unsetMetaIdRef();
  del2.setPortRef("port2");
  rv = submod2.addDeletion(&del2);
  del2.unsetId();
  del2.unsetPortRef();
  del2.setUnitRef("mph");
  rv = submod2.addDeletion(&del2);
  del2.unsetUnitRef();
  del2.setIdRef("submodG");
  SBaseRef* sbr = del2.createSBaseRef();
  sbr->setIdRef("element5");
  rv = submod2.addDeletion(&del2);
  Deletion del3;
  del3.setIdRef("submodG");
  SBaseRef sbr2;
  sbr2.setIdRef("subsubmodQ");
  SBaseRef* subsbr = sbr2.createSBaseRef();
  subsbr->setPortRef("toBdel");
  del3.setSBaseRef(&sbr2);
  submod2.addDeletion(&del3);
  mplugin->addSubmodel(&submod2);

  Port* port1 = mplugin->createPort();
  port1->setId("port1");
  port1->setMetaIdRef("_110013");
  Port port2;
  port2.setId("port2");
  port2.setIdRef("x");
  mplugin->addPort(&port2);
  port2.setId("port3");
  port2.setIdRef("submod2");
  port2.setSBaseRef(&sbr2);
  mplugin->addPort(&port2);

  Port* testport = mplugin->getPort("port1");
  assert(testport != NULL);

  writeSBMLToFile(document,"comp_example1.xml");
  delete document;
  document = readSBMLFromFile("comp_example1.xml");
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
    writeSBMLToFile(document, "comp_example1_rt.xml");
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
