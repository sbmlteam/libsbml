/**
 * @file    dyn_example1.cpp
 * @brief   SBML DYN example
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This sample program is distributed under a different license than the rest
 * of libSBML.  This program uses the open-source MIT license, as follows:
 *
 * Copyright (c) 2013-2014 by the California Institute of Technology
 * (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
 * and the University of Heidelberg (Germany), with support from the National
 * Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Neither the name of the California Institute of Technology (Caltech), nor
 * of the European Bioinformatics Institute (EMBL-EBI), nor of the University
 * of Heidelberg, nor the names of any contributors, may be used to endorse
 * or promote products derived from this software without specific prior
 * written permission.
 * ------------------------------------------------------------------------ -->
 */

#include <sbml/SBMLTypes.h>
#include <sbml/packages/dyn/common/DynExtensionTypes.h>

LIBSBML_CPP_NAMESPACE_USE

int main(int argc,char** argv)
{
  
  DynPkgNamespaces sbmlns;

  // create the document

  SBMLDocument *document = new SBMLDocument(&sbmlns);
  document->setPackageRequired("dyn", true);

  // create the Model

  Model* model=document->createModel();
  model->setId("singleCell");

  // create the Compartment

  Compartment* compartment = model->createCompartment();
  compartment->setId("Extracellular");
  compartment->setConstant(true);
  compartment->setSize(8000000);
  compartment->setSpatialDimensions(3.0);

  compartment = model->createCompartment();
  compartment->setId("PlasmaMembrane");
  compartment->setConstant(true);
  compartment->setSize(314);
  compartment->setSpatialDimensions(2.0);

  compartment = model->createCompartment();
  compartment->setId("Cytosol");
  compartment->setConstant(true);
  compartment->setSize(523);
  compartment->setSpatialDimensions(3.0);

  // create the Species

  Species* species = model->createSpecies();
  species->setId("C_EC");
  species->setCompartment("Extracellular");
  species->setBoundaryCondition(false);
  species->setConstant(false);
  species->setHasOnlySubstanceUnits(false);

  species = model->createSpecies();
  species->setId("RTR_M");
  species->setCompartment("PlasmaMembrane");
  species->setBoundaryCondition(false);
  species->setConstant(false);
  species->setHasOnlySubstanceUnits(false);

  species = model->createSpecies();
  species->setId("RCC_M");
  species->setCompartment("PlasmaMembrane");
  species->setBoundaryCondition(false);
  species->setConstant(false);
  species->setHasOnlySubstanceUnits(false);

  species = model->createSpecies();
  species->setId("A_C");
  species->setCompartment("Cytosol");
  species->setBoundaryCondition(false);
  species->setConstant(false);
  species->setHasOnlySubstanceUnits(false);

  species = model->createSpecies();
  species->setId("AA_C");
  species->setCompartment("Cytosol");
  species->setBoundaryCondition(false);
  species->setConstant(false);
  species->setHasOnlySubstanceUnits(false);

  species = model->createSpecies();
  species->setId("T");
  species->setCompartment("Cytosol");
  species->setBoundaryCondition(false);
  species->setConstant(false);
  species->setInitialConcentration(10);
  species->setHasOnlySubstanceUnits(false);

  species = model->createSpecies();
  species->setId("S");
  species->setCompartment("Cytosol");
  species->setBoundaryCondition(false);
  species->setConstant(false);
  species->setInitialConcentration(5);
  species->setHasOnlySubstanceUnits(false);

  // create the Reactions

  Reaction* reaction = model->createReaction();
  reaction->setId("r1");
  reaction->setReversible(true);
  reaction->setFast(false);
  reaction->setCompartment("Extracellular");
  
  SpeciesReference* reactant = reaction->createReactant();
  reactant->setSpecies("RTR_M");
  reactant->setStoichiometry(1);
  reactant->setConstant(true);
  
  reactant = reaction->createReactant();
  reactant->setSpecies("C_EC");
  reactant->setStoichiometry(1);
  reactant->setConstant(true);
  
  SpeciesReference* product = reaction->createProduct();
  product->setSpecies("RCC_M");
  product->setStoichiometry(1);
  product->setConstant(true);

  reaction = model->createReaction();
  reaction->setId("r2");
  reaction->setReversible(true);
  reaction->setFast(false);
  reaction->setCompartment("Cytosol");
  
  reactant = reaction->createReactant();
  reactant->setSpecies("A_C");
  reactant->setStoichiometry(1);
  reactant->setConstant(true);
  
  product = reaction->createProduct();
  product->setSpecies("AA_C");
  product->setStoichiometry(1);
  product->setConstant(true);

  SimpleSpeciesReference* modifier = reaction->createModifier();
  modifier->setSpecies("RCC_M");

  // Create Event

  Event* event = model->createEvent();
  event->setUseValuesFromTriggerTime(true);

  Trigger* trigger = event->createTrigger();
  trigger->setInitialValue(false);
  trigger->setPersistent(true);
  trigger->setMath(SBML_parseFormula("lt(AA_C, T)"));

  //
  // Get a DynEventPlugin object plugged in the event object.
  //
  // The type of the returned value of SBase::getPlugin() function is
  // SBasePlugin*, and thus the value needs to be casted for the
  // corresponding derived class.
  //
  DynEventPlugin* eplugin
    = static_cast<DynEventPlugin*>(event->getPlugin("dyn"));

  eplugin->setApplyToAll(true);
  eplugin->setCboTerm("http://cbo.biocomplexity.indiana.edu/svn/cbo/trunk/CBO_1_0.owl#CellDeath");
 
  event = model->createEvent();
  event->setUseValuesFromTriggerTime(true);

  trigger = event->createTrigger();
  trigger->setInitialValue(false);
  trigger->setPersistent(true);
  trigger->setMath(SBML_parseFormula("lt(AA_C, S)"));

  eplugin = static_cast<DynEventPlugin*>(event->getPlugin("dyn"));

  eplugin->setApplyToAll(true);
  eplugin->setCboTerm("http://cbo.biocomplexity.indiana.edu/svn/cbo/trunk/CBO_1_0.owl#CellDevision");

  document->checkConsistency();

  if (document->getNumErrors(LIBSBML_SEV_ERROR) > 0)
    document->printErrors();

  writeSBML(document,"dyn_example1.xml");
  delete document;
}
