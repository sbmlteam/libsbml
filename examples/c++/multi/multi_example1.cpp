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

  // create the compartments

  Compartment * c = model->createCompartment();
  c->setId("membrane");
  c->setConstant(true);

  // set the multi attribute isType via the compartmentPlugin
  MultiCompartmentPlugin * compPlug = 
    static_cast<MultiCompartmentPlugin*>(c->getPlugin("multi"));
  compPlug->setIsType(true);

  c = model->createCompartment();
  c->setId("inter_membrane");
  c->setConstant(true);

  // set the multi attribute isType via the compartmentPlugin
  compPlug = 
    static_cast<MultiCompartmentPlugin*>(c->getPlugin("multi"));
  compPlug->setIsType(true);

  // create the compartmentReferences

  CompartmentReference * compRef = compPlug->createCompartmentReference();
  compRef->setId("m1");
  compRef->setCompartment("membrane");

  compRef = compPlug->createCompartmentReference();
  compRef->setId("m2");
  compRef->setCompartment("membrane");

  // create the speciesTypes

  MultiModelPlugin * modelPlug = 
    static_cast<MultiModelPlugin*>(model->getPlugin("multi"));

  MultiSpeciesType * st = modelPlug->createMultiSpeciesType();
  st->setId("stX");
  st->setCompartment("ct");

  st = modelPlug->createMultiSpeciesType();
  st->setId("stXXcis");
  st->setCompartment("ct");

  // create speciesTypeInstances

  SpeciesTypeInstance * sti = st->createSpeciesTypeInstance();
  sti->setId("stiX1");
  sti->setSpeciesType("stX");

  sti = st->createSpeciesTypeInstance();
  sti->setId("stiX2");
  sti->setSpeciesType("stX");

  // create speciesTypeBond

  InSpeciesTypeBond * istb = st->createInSpeciesTypeBond();
  istb->setBindingSite1("stiX1");
  istb->setBindingSite2("stiX2");

  // another speciesType

  st = modelPlug->createMultiSpeciesType();
  st->setId("stXXtrans");
  st->setCompartment("cct");

  // create speciesTypeInstances

  sti = st->createSpeciesTypeInstance();
  sti->setId("stiX1");
  sti->setSpeciesType("stX");
  sti->setCompartmentReference("cr1");

  sti = st->createSpeciesTypeInstance();
  sti->setId("stiX2");
  sti->setSpeciesType("stX");
  sti->setCompartmentReference("cr2");

  // create speciesTypeBond

  istb = st->createInSpeciesTypeBond();
  istb->setBindingSite1("stiX1");
  istb->setBindingSite2("stiX2");




  writeSBML(document,"multi_example1.xml");
 
  delete document;

  return 0;
}
