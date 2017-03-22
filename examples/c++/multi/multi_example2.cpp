/**
 * @file    example1.cpp
 * @brief   multi create example
 * @author  Sarah Keating
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
#include <sbml/packages/multi/common/MultiExtensionTypes.h>


using namespace std;
LIBSBML_CPP_NAMESPACE_USE

int
main (int argc, char* argv[])
{
  SBMLNamespaces sbmlns(3,1,"multi",1);

  // create the document

  SBMLDocument *document = new SBMLDocument(&sbmlns);

  // set the required attribute to true
  MultiSBMLDocumentPlugin * docPlug = 
    static_cast<MultiSBMLDocumentPlugin*>(document->getPlugin("multi"));
  docPlug->setRequired(true);


  // create the Model

  Model* model=document->createModel();

  // create a  compartment

  Compartment * c = model->createCompartment();
  c->setId("membrane");
  c->setConstant(true);

  // set the multi attribute isType via the compartmentPlugin
  MultiCompartmentPlugin * compPlug = 
    static_cast<MultiCompartmentPlugin*>(c->getPlugin("multi"));
  compPlug->setIsType(true);

  // create species
  Species *s = model->createSpecies();
  s->setId("s1");
  s->setCompartment("membrane");
  s->setBoundaryCondition(false);
  s->setHasOnlySubstanceUnits(false);
  s->setConstant(false);

  // create reaction
  Reaction *r = model->createReaction();
  r->setId("r1");
  r->setFast(false);
  r->setReversible(false);

  // createReactant
  SpeciesReference *sr = r->createReactant();
  sr->setId("sr1");
  sr->setSpecies("s1");
  sr->setConstant(false);

  KineticLaw *kl = r->createKineticLaw();

  ASTNode * ci = new ASTNode(AST_NAME);
  ci->setName("s1");
  MultiASTPlugin * astPlugin = static_cast<MultiASTPlugin*>(ci->getPlugin("multi"));
  astPlugin->setSpeciesReference("r1");

  ASTNode * ci1 = new ASTNode(AST_NAME);
  MultiASTPlugin * astPlugin1 = static_cast<MultiASTPlugin*>(ci1->getPlugin("multi"));
  astPlugin1->setRepresentationType("sum");
  ci1->setName("s1");

  ASTNode *math = new ASTNode(AST_TIMES);
  math->addChild(ci);
  math->addChild(ci1);

  kl->setMath(math);

  writeSBML(document,"multi_example2.xml");
 
  delete document;

  return 0;
}
