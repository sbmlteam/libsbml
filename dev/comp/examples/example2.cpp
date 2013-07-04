/**
 * @file    example2.cpp
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

  ExternalModelDefinition* mod1 = compdoc->createExternalModelDefinition();
  mod1->setId("teusink_glycolysis");
  mod1->setName("Teusink2000_Glycolysis");
  mod1->setSource("urn:miriam:biomodels.db:BIOMD0000000064");

  // create the Model
  Model* model=document->createModel();
  model->setId("glycolysis_with_ports");
  
  // Setup the submodel
  CompModelPlugin* mplugin = static_cast<CompModelPlugin*>(model->getPlugin("comp"));
  Submodel* submod1 = mplugin->createSubmodel();
  submod1->setId("glycolysis");
  submod1->setModelRef("teusink_glycolysis");

  //Now create ports
  Port port;
  port.setId("cytosol_port");
  port.setIdRef("glycolysis");
  SBaseRef* sbr = port.createSBaseRef();
  sbr->setIdRef("cytosol");
  mplugin->addPort(&port);

  port.setId("extracellular_port");
  sbr->setIdRef("extracellular");
  mplugin->addPort(&port);


  /*$Glyc in cytosol, $Trh in cytosol, $CO2 in cytosol;
  species $SUCC in cytosol, $GLCo in extracellular, $ETOH in cytosol, $GLY in cytosol;
  species $ATP in cytosol, $ADP in cytosol, $AMP in cytosol, $SUM_P in cytosol;
  species $F26BP in cytosol;
  */

  port.setId("Glyc_port");
  sbr->setIdRef("Glyc");
  mplugin->addPort(&port);

  port.setId("Trh_port");
  sbr->setIdRef("Trh");
  mplugin->addPort(&port);

  port.setId("CO2_port");
  sbr->setIdRef("CO2");
  mplugin->addPort(&port);

  port.setId("SUCC_port");
  sbr->setIdRef("SUCC");
  mplugin->addPort(&port);

  port.setId("GLCo_port");
  sbr->setIdRef("GLCo");
  mplugin->addPort(&port);

  port.setId("ETOH_port");
  sbr->setIdRef("ETOH");
  mplugin->addPort(&port);

  port.setId("GLY_port");
  sbr->setIdRef("GLY");
  mplugin->addPort(&port);

  port.setId("ATP_port");
  sbr->setIdRef("ATP");
  mplugin->addPort(&port);

  port.setId("ADP_port");
  sbr->setIdRef("ADP");
  mplugin->addPort(&port);

  port.setId("AMP_port");
  sbr->setIdRef("AMP");
  mplugin->addPort(&port);

  port.setId("SUM_P_port");
  sbr->setIdRef("SUM_P");
  mplugin->addPort(&port);

  port.setId("F26BP_port");
  sbr->setIdRef("F26BP");
  mplugin->addPort(&port);


  writeSBMLToFile(document,"comp_example2.xml");
  delete document;
  document = readSBMLFromFile("comp_example2.xml");
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
    writeSBMLToFile(document, "comp_example2_rt.xml");
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
