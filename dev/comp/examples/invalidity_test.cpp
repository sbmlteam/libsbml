/**
 * @file    invalidity_test.cpp
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

void createTest1();
void createTest2();
void createTest3();
void createTest4();

int main(int argc,char** argv){

  createTest4();
  createTest3();
  createTest2();
  createTest1();

  return 0;
}

//First, do everything correctly:
void createTest1() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(false);

  Model model(&sbmlns);
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  p.setValue(8);
  model.addParameter(&p);
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model.getPlugin("comp"));
  Port port(3,1,1);
  port.setId("port1");
  port.setIdRef("p8");
  compmod->addPort(&port);

  document.setModel(&model);
  writeSBMLToFile(&document, "invalidity_test1.xml");
}

//Screw things up by creating the document unaware of comp.  This creates '<listOfPorts>' instead of '<comp:listOfPorts>'
void createTest2() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  SBMLDocument document(3,1);

  Model model(&sbmlns);
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  p.setValue(8);
  model.addParameter(&p);
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model.getPlugin("comp"));
  Port port(3,1,1);
  port.setId("port1");
  port.setIdRef("p8");
  compmod->addPort(&port);

  document.setModel(&model);
  writeSBMLToFile(&document, "invalidity_test2.xml");
}

//Screw things up by never setting 'required'
void createTest3() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));

  Model model(&sbmlns);
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  p.setValue(8);
  model.addParameter(&p);
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model.getPlugin("comp"));
  Port port(3,1,1);
  port.setId("port1");
  port.setIdRef("p8");
  compmod->addPort(&port);

  document.setModel(&model);
  writeSBMLToFile(&document, "invalidity_test3.xml");
}

//Screw things up by setting 'required' to 'true'
void createTest4() 
{
  SBMLNamespaces sbmlns(3,1,"comp",1);
  CompPkgNamespaces csbmlns(3,1,1,"comp");

  SBMLDocument document(&sbmlns);
  CompSBMLDocumentPlugin* compdoc = static_cast<CompSBMLDocumentPlugin*>(document.getPlugin("comp"));
  compdoc->setRequired(true);

  Model model(&sbmlns);
  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p8");
  p.setValue(8);
  model.addParameter(&p);
  CompModelPlugin* compmod = static_cast<CompModelPlugin*>(model.getPlugin("comp"));
  Port port(3,1,1);
  port.setId("port1");
  port.setIdRef("p8");
  compmod->addPort(&port);

  document.setModel(&model);
  writeSBMLToFile(&document, "invalidity_test4.xml");
}

