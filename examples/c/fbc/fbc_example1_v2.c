/**
 * @file    fbc_example1_v2.c
 * @brief   FBC example
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This sample program is distributed under a different license than the rest
 * of libSBML.  This program uses the open-source MIT license, as follows:
 *
 * Copyright (c) 2013-2018 by the California Institute of Technology
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

/*#include <iostream>
#include <sstream>
*/
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLDocumentPlugin.h>


#include <sbml/extension/SBMLExtensionRegister.h>

#include <sbml/packages/fbc/common/fbcfwd.h>
#include <sbml/packages/fbc/extension/FbcSBMLDocumentPlugin.h>
#include <sbml/packages/fbc/extension/FbcModelPlugin.h>
#include <sbml/packages/fbc/extension/FbcReactionPlugin.h>
#include <sbml/packages/fbc/sbml/Objective.h>
#include <sbml/packages/fbc/sbml/FluxObjective.h>

LIBSBML_CPP_NAMESPACE_USE


int main(int argc,char** argv)
{
  int retval = 0;
  XMLNamespaces_t * fbc;
  SBMLNamespaces_t * sbmlns;
  SBMLDocument_t * doc;
  Compartment_t * compartment;
  Species_t * species;
  Reaction_t * reaction;
  SpeciesReference_t *reactant;
  SpeciesReference_t *product;
  Model_t * model;
  Parameter_t * min_param;
  Parameter_t * max_param;
  SBMLDocumentPlugin_t * docPlug;
  SBasePlugin_t * modelPlug;
  SBasePlugin_t *reactionPlug;
  Objective_t * objective;
  FluxObjective_t * fluxObjective;

  /* get fbc registry entry */
  SBMLExtension_t *sbmlext = SBMLExtensionRegistry_getExtension("fbc");

  /* create the sbml namespaces object with fbc */
  fbc = XMLNamespaces_create();
  XMLNamespaces_add(fbc, SBMLExtension_getURI(sbmlext, 3, 1, 2), "fbc");
  
  sbmlns = SBMLNamespaces_create(3, 1);
  SBMLNamespaces_addNamespaces(sbmlns, fbc);

  /* create the document */
  doc = SBMLDocument_createWithSBMLNamespaces(sbmlns);

  /* set the fbc reqd attribute to false */
  docPlug = (SBMLDocumentPlugin_t*)(SBase_getPlugin((SBase_t*)(doc), "fbc"));
  SBMLDocumentPlugin_setRequired(docPlug, 0);

  // create the Model

  model = SBMLDocument_createModel(doc);
  Model_setId(model, "model1");

  // Get a SBasePlugin_t object plugged in the model object.

  modelPlug = SBase_getPlugin((SBase_t *)(model), "fbc");

  // set the fbc strict attribute
  FbcModelPlugin_setStrict(modelPlug, 0);

  // create the Compartment

  compartment = Model_createCompartment(model);
  Compartment_setId(compartment, "compartment");
  Compartment_setConstant(compartment, 1);
  Compartment_setSize(compartment, 1);

  // create the Parameters

  min_param = Model_createParameter(model);
  Parameter_setId(min_param, "min_param");
  Parameter_setConstant(min_param, 1);
  Parameter_setValue(min_param, 0);

  max_param = Model_createParameter(model);
  Parameter_setId(max_param, "max_param");
  Parameter_setConstant(max_param, 1);
  Parameter_setValue(max_param, 0);

  // create the Species

  species = Model_createSpecies(model);
  Species_setId(species, "Node1");
  Species_setCompartment(species, "compartment");
  Species_setBoundaryCondition(species, 0);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  species = Model_createSpecies(model);
  Species_setId(species, "Node2");
  Species_setCompartment(species, "compartment");
  Species_setBoundaryCondition(species, 0);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  species = Model_createSpecies(model);
  Species_setId(species, "Node3");
  Species_setCompartment(species, "compartment");
  Species_setBoundaryCondition(species, 0);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  species = Model_createSpecies(model);
  Species_setId(species, "Node4");
  Species_setCompartment(species, "compartment");
  Species_setBoundaryCondition(species, 0);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  species = Model_createSpecies(model);
  Species_setId(species, "Node5");
  Species_setCompartment(species, "compartment");
  Species_setBoundaryCondition(species, 0);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  species = Model_createSpecies(model);
  Species_setId(species, "Node6");
  Species_setCompartment(species, "compartment");
  Species_setBoundaryCondition(species, 0);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  species = Model_createSpecies(model);
  Species_setId(species, "Node7");
  Species_setCompartment(species, "compartment");
  Species_setBoundaryCondition(species, 0);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  species = Model_createSpecies(model);
  Species_setId(species, "Node8");
  Species_setCompartment(species, "compartment");
  Species_setBoundaryCondition(species, 0);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  species = Model_createSpecies(model);
  Species_setId(species, "Node0");
  Species_setCompartment(species, "compartment");
  Species_setBoundaryCondition(species, 1);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  species = Model_createSpecies(model);
  Species_setId(species, "Node9");
  Species_setCompartment(species, "compartment");
  Species_setBoundaryCondition(species, 1);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  // create the Reactions

  reaction = Model_createReaction(model);
  Reaction_setId(reaction, "J0");
  Reaction_setReversible(reaction, 0);
  Reaction_setFast(reaction, 0);
  reactant = Reaction_createReactant(reaction);
  SpeciesReference_setSpecies(reactant, "Node0");
  SpeciesReference_setStoichiometry(reactant, 1);
  SpeciesReference_setConstant(reactant, 1);
  product = Reaction_createProduct(reaction);
  SpeciesReference_setSpecies(product, "Node1");
  SpeciesReference_setStoichiometry(product, 1);
  SpeciesReference_setConstant(product, 1);

  // Get a SBasePlugin_t object plugged in the reaction object.

  reactionPlug = SBase_getPlugin((SBase_t *)(reaction), "fbc");

  // set the flux bounds for this reaction

  FbcReactionPlugin_setLowerFluxBound(reactionPlug, "min_param");
  FbcReactionPlugin_setUpperFluxBound(reactionPlug, "max_param");

  reaction = Model_createReaction(model);
  Reaction_setId(reaction, "J1");
  Reaction_setReversible(reaction, 0);
  Reaction_setFast(reaction, 0);
  reactant = Reaction_createReactant(reaction);
  SpeciesReference_setSpecies(reactant, "Node1");
  SpeciesReference_setStoichiometry(reactant, 1);
  SpeciesReference_setConstant(reactant, 1);
  product = Reaction_createProduct(reaction);
  SpeciesReference_setSpecies(product, "Node2");
  SpeciesReference_setStoichiometry(product, 1);
  SpeciesReference_setConstant(product, 1);

  reaction = Model_createReaction(model);
  Reaction_setId(reaction, "J2");
  Reaction_setReversible(reaction, 0);
  Reaction_setFast(reaction, 0);
  reactant = Reaction_createReactant(reaction);
  SpeciesReference_setSpecies(reactant, "Node2");
  SpeciesReference_setStoichiometry(reactant, 1);
  SpeciesReference_setConstant(reactant, 1);
  product = Reaction_createProduct(reaction);
  SpeciesReference_setSpecies(product, "Node3");
  SpeciesReference_setStoichiometry(product, 1);
  SpeciesReference_setConstant(product, 1);

  reaction = Model_createReaction(model);
  Reaction_setId(reaction, "J3");
  Reaction_setReversible(reaction, 0);
  Reaction_setFast(reaction, 0);
  reactant = Reaction_createReactant(reaction);
  SpeciesReference_setSpecies(reactant, "Node1");
  SpeciesReference_setStoichiometry(reactant, 1);
  SpeciesReference_setConstant(reactant, 1);
  product = Reaction_createProduct(reaction);
  SpeciesReference_setSpecies(product, "Node4");
  SpeciesReference_setStoichiometry(product, 1);
  SpeciesReference_setConstant(product, 1);

  reaction = Model_createReaction(model);
  Reaction_setId(reaction, "J4");
  Reaction_setReversible(reaction, 0);
  Reaction_setFast(reaction, 0);
  reactant = Reaction_createReactant(reaction);
  SpeciesReference_setSpecies(reactant, "Node4");
  SpeciesReference_setStoichiometry(reactant, 1);
  SpeciesReference_setConstant(reactant, 1);
  product = Reaction_createProduct(reaction);
  SpeciesReference_setSpecies(product, "Node3");
  SpeciesReference_setStoichiometry(product, 1);
  SpeciesReference_setConstant(product, 1);

  reaction = Model_createReaction(model);
  Reaction_setId(reaction, "J5");
  Reaction_setReversible(reaction, 0);
  Reaction_setFast(reaction, 0);
  reactant = Reaction_createReactant(reaction);
  SpeciesReference_setSpecies(reactant, "Node3");
  SpeciesReference_setStoichiometry(reactant, 1);
  SpeciesReference_setConstant(reactant, 1);
  product = Reaction_createProduct(reaction);
  SpeciesReference_setSpecies(product, "Node5");
  SpeciesReference_setStoichiometry(product, 1);
  SpeciesReference_setConstant(product, 1);

  reaction = Model_createReaction(model);
  Reaction_setId(reaction, "J6");
  Reaction_setReversible(reaction, 0);
  Reaction_setFast(reaction, 0);
  reactant = Reaction_createReactant(reaction);
  SpeciesReference_setSpecies(reactant, "Node5");
  SpeciesReference_setStoichiometry(reactant, 1);
  SpeciesReference_setConstant(reactant, 1);
  product = Reaction_createProduct(reaction);
  SpeciesReference_setSpecies(product, "Node6");
  SpeciesReference_setStoichiometry(product, 1);
  SpeciesReference_setConstant(product, 1);

  reaction = Model_createReaction(model);
  Reaction_setId(reaction, "J7");
  Reaction_setReversible(reaction, 0);
  Reaction_setFast(reaction, 0);
  reactant = Reaction_createReactant(reaction);
  SpeciesReference_setSpecies(reactant, "Node6");
  SpeciesReference_setStoichiometry(reactant, 1);
  SpeciesReference_setConstant(reactant, 1);
  product = Reaction_createProduct(reaction);
  SpeciesReference_setSpecies(product, "Node7");
  SpeciesReference_setStoichiometry(product, 1);
  SpeciesReference_setConstant(product, 1);

  reaction = Model_createReaction(model);
  Reaction_setId(reaction, "J8");
  Reaction_setReversible(reaction, 0);
  Reaction_setFast(reaction, 0);
  reactant = Reaction_createReactant(reaction);
  SpeciesReference_setSpecies(reactant, "Node5");
  SpeciesReference_setStoichiometry(reactant, 1);
  SpeciesReference_setConstant(reactant, 1);
  product = Reaction_createProduct(reaction);
  SpeciesReference_setSpecies(product, "Node8");
  SpeciesReference_setStoichiometry(product, 1);
  SpeciesReference_setConstant(product, 1);

  reaction = Model_createReaction(model);
  Reaction_setId(reaction, "J9");
  Reaction_setReversible(reaction, 0);
  Reaction_setFast(reaction, 0);
  reactant = Reaction_createReactant(reaction);
  SpeciesReference_setSpecies(reactant, "Node8");
  SpeciesReference_setStoichiometry(reactant, 1);
  SpeciesReference_setConstant(reactant, 1);
  product = Reaction_createProduct(reaction);
  SpeciesReference_setSpecies(product, "Node7");
  SpeciesReference_setStoichiometry(product, 1);
  SpeciesReference_setConstant(product, 1);

  reaction = Model_createReaction(model);
  Reaction_setId(reaction, "J10");
  Reaction_setReversible(reaction, 0);
  Reaction_setFast(reaction, 0);
  reactant = Reaction_createReactant(reaction);
  SpeciesReference_setSpecies(reactant, "Node7");
  SpeciesReference_setStoichiometry(reactant, 1);
  SpeciesReference_setConstant(reactant, 1);
  product = Reaction_createProduct(reaction);
  SpeciesReference_setSpecies(product, "Node9");
  SpeciesReference_setStoichiometry(product, 1);
  SpeciesReference_setConstant(product, 1);

  // create the objective object and add it to model plugin

  objective = Objective_create(3, 1, 2);
  Objective_setId(objective, "obj1");
  Objective_setType(objective, "maximize");

  fluxObjective = Objective_createFluxObjective(objective);
  FluxObjective_setReaction(fluxObjective, "J8");
  FluxObjective_setCoefficient(fluxObjective, 1);

  FbcModelPlugin_addObjective(modelPlug, objective);

  // mark obj1 as active objective
  FbcModelPlugin_setActiveObjectiveId(modelPlug, "obj1");

  /* write the file */
  writeSBMLToFile(doc, "fbc_example1_v2.xml");

  return retval;
}
