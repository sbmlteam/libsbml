# 
# @file    dyn_example1.py
# @brief   SBML DYN example
# @author  Frank T. Bergmann
# 
# <!--------------------------------------------------------------------------
# This sample program is distributed under a different license than the rest
# of libSBML.  This program uses the open-source MIT license, as follows:
# 
# Copyright (c) 2013-2014 by the California Institute of Technology
# (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
# and the University of Heidelberg (Germany), with support from the National
# Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
# 
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
# 
# Neither the name of the California Institute of Technology (Caltech), nor
# of the European Bioinformatics Institute (EMBL-EBI), nor of the University
# of Heidelberg, nor the names of any contributors, may be used to endorse
# or promote products derived from this software without specific prior
# written permission.
# ------------------------------------------------------------------------ -.
# 

from libsbml import *

sbmlns = DynPkgNamespaces();

# create the document

document = SBMLDocument(sbmlns);
document.setPackageRequired("dyn", True);

# create the Model

model=document.createModel();
model.setId("singleCell");

# create the Compartment

compartment = model.createCompartment();
compartment.setId("Extracellular");
compartment.setConstant(True);
compartment.setSize(8000000);
compartment.setSpatialDimensions(3.0);

compartment = model.createCompartment();
compartment.setId("PlasmaMembrane");
compartment.setConstant(True);
compartment.setSize(314);
compartment.setSpatialDimensions(2.0);

compartment = model.createCompartment();
compartment.setId("Cytosol");
compartment.setConstant(True);
compartment.setSize(523);
compartment.setSpatialDimensions(3.0);

# create the Species

species = model.createSpecies();
species.setId("C_EC");
species.setCompartment("Extracellular");
species.setBoundaryCondition(False);
species.setConstant(False);
species.setHasOnlySubstanceUnits(False);

species = model.createSpecies();
species.setId("RTR_M");
species.setCompartment("PlasmaMembrane");
species.setBoundaryCondition(False);
species.setConstant(False);
species.setHasOnlySubstanceUnits(False);

species = model.createSpecies();
species.setId("RCC_M");
species.setCompartment("PlasmaMembrane");
species.setBoundaryCondition(False);
species.setConstant(False);
species.setHasOnlySubstanceUnits(False);

species = model.createSpecies();
species.setId("A_C");
species.setCompartment("Cytosol");
species.setBoundaryCondition(False);
species.setConstant(False);
species.setHasOnlySubstanceUnits(False);

species = model.createSpecies();
species.setId("AA_C");
species.setCompartment("Cytosol");
species.setBoundaryCondition(False);
species.setConstant(False);
species.setHasOnlySubstanceUnits(False);

# create Parameters

param = model.createParameter();
param.initDefaults();
param.setId("S");

param = model.createParameter();
param.initDefaults();
param.setId("T");

# create the Reactions

reaction = model.createReaction();
reaction.setId("r1");
reaction.setReversible(True);
reaction.setFast(False);
reaction.setCompartment("Extracellular");

reactant = reaction.createReactant();
reactant.setSpecies("RTR_M");
reactant.setStoichiometry(1);
reactant.setConstant(True);

reactant = reaction.createReactant();
reactant.setSpecies("C_EC");
reactant.setStoichiometry(1);
reactant.setConstant(True);

product = reaction.createProduct();
product.setSpecies("RCC_M");
product.setStoichiometry(1);
product.setConstant(True);

reaction = model.createReaction();
reaction.setId("r2");
reaction.setReversible(True);
reaction.setFast(False);
reaction.setCompartment("Cytosol");

reactant = reaction.createReactant();
reactant.setSpecies("A_C");
reactant.setStoichiometry(1);
reactant.setConstant(True);

product = reaction.createProduct();
product.setSpecies("AA_C");
product.setStoichiometry(1);
product.setConstant(True);

modifier = reaction.createModifier();
modifier.setSpecies("RCC_M");

# Create Event

event = model.createEvent();
event.setUseValuesFromTriggerTime(True);

trigger = event.createTrigger();
trigger.setInitialValue(False);
trigger.setPersistent(True);
trigger.setMath(parseFormula("lt(AA_C, T)"));

#
# Get a DynEventPlugin object plugged in the event object.
#
eplugin = event.getPlugin("dyn");

eplugin.setApplyToAll(True);
eplugin.setCboTerm("http://cbo.biocomplexity.indiana.edu/svn/cbo/trunk/CBO_1_0.owl#CellDeath");

event = model.createEvent();
event.setUseValuesFromTriggerTime(True);

trigger = event.createTrigger();
trigger.setInitialValue(False);
trigger.setPersistent(True);
trigger.setMath(parseFormula("lt(AA_C, S)"));

eplugin = event.getPlugin("dyn");

eplugin.setApplyToAll(True);
eplugin.setCboTerm("http://cbo.biocomplexity.indiana.edu/svn/cbo/trunk/CBO_1_0.owl#CellDevision");

document.checkConsistency();

if (document.getNumErrors(LIBSBML_SEV_ERROR) > 0):
  document.printErrors();

writeSBML(document,"dyn_example1.xml");

