/**
 * @file    example1.cpp
 * @brief   SBML required elements example
 * @author  Frank Bergmann
 *
 * $Id$
 * $HeadURL$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <sbml/SBMLTypes.h>
#include <sbml/packages/req/common/RequiredElementsExtensionTypes.h>

#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>

LIBSBML_CPP_NAMESPACE_USE

static SBMLExtensionRegister<RequiredElementsExtension> requiredElementsExtensionRegistry;

int main(int argc,char** argv){


SBMLNamespaces sbmlns(3,1,"req",1);

// create the document

SBMLDocument *document = new SBMLDocument(&sbmlns);

// create the Model

Model* model=document->createModel();

// create a parameter

Parameter* parameter = model->createParameter();
parameter->setId("x");
parameter->setConstant(true);

// get the required plugin
RequiredElementsSBasePlugin* splugin = static_cast<RequiredElementsSBasePlugin*>(parameter->getPlugin("req"));

// tell the plugin that the math is overridden
splugin->setMathOverridden("spatial");
splugin->setCoreHasAlternateMath(true);

writeSBML(document,"req_example1.xml");
delete document;

}

