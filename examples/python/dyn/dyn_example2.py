# 
# @file    dyn_example2.py
# @brief   SBML DYN example
# @author  Frank T. Bergmann
# 
# <!--------------------------------------------------------------------------
# This sample program is distributed under a different license than the rest
# of libSBML.  This program uses the open-source MIT license, as follows:
# 
# Copyright (c) 2013-2018 by the California Institute of Technology
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

sbmlns = DynPkgNamespaces()
sbmlns.addPackageNamespace("comp", 1, "comp");

# create the document

document = SBMLDocument(sbmlns);
document.setPackageRequired("dyn", True);
document.setPackageRequired("comp", True);

# create the Model

model=document.createModel();
model.setId("grid2x2");

# create the Compartment

compartment = model.createCompartment();
compartment.setId("Loc1");
compartment.setConstant(False);
compartment.setSize(1);
compartment.setSpatialDimensions(2.0);

cplugin = compartment.getPlugin("dyn")

component = cplugin.createSpatialComponent();
component.setSpatialIndex(DYN_SPATIALKIND_CARTESIANX);
component.setVariable("q1_X");

component = cplugin.createSpatialComponent();
component.setSpatialIndex(DYN_SPATIALKIND_CARTESIANY);
component.setVariable("q1_Y");

compPlugin = compartment.getPlugin("comp")

relement = compPlugin.createReplacedElement();
relement.setIdRef("C");
relement.setSubmodelRef("GRID_1_1_cell");

compartment = model.createCompartment();
compartment.setId("Loc2");
compartment.setConstant(False);
compartment.setSize(1);
compartment.setSpatialDimensions(2.0);

cplugin = compartment.getPlugin("dyn");

component = cplugin.createSpatialComponent();
component.setSpatialIndex(DYN_SPATIALKIND_CARTESIANX);
component.setVariable("q2_X");

component = cplugin.createSpatialComponent();
component.setSpatialIndex(DYN_SPATIALKIND_CARTESIANY);
component.setVariable("q2_Y");

compPlugin = compartment.getPlugin("comp");

relement = compPlugin.createReplacedElement();
relement.setIdRef("C");
relement.setSubmodelRef("GRID_1_2_cell");

compartment = model.createCompartment();
compartment.setId("Loc3");
compartment.setConstant(False);
compartment.setSize(1);
compartment.setSpatialDimensions(2.0);

cplugin = compartment.getPlugin("dyn");

component = cplugin.createSpatialComponent();
component.setSpatialIndex(DYN_SPATIALKIND_CARTESIANX);
component.setVariable("q3_X");

component = cplugin.createSpatialComponent();
component.setSpatialIndex(DYN_SPATIALKIND_CARTESIANY);
component.setVariable("q3_Y");

compPlugin = compartment.getPlugin("comp");

relement = compPlugin.createReplacedElement();
relement.setIdRef("C");
relement.setSubmodelRef("GRID_2_1_cell");

compartment = model.createCompartment();
compartment.setId("Loc4");
compartment.setConstant(False);
compartment.setSize(1);
compartment.setSpatialDimensions(2.0);

cplugin = compartment.getPlugin("dyn");

component = cplugin.createSpatialComponent();
component.setSpatialIndex(DYN_SPATIALKIND_CARTESIANX);
component.setVariable("q4_X");

component = cplugin.createSpatialComponent();
component.setSpatialIndex(DYN_SPATIALKIND_CARTESIANY);
component.setVariable("q4_Y");

compPlugin = compartment.getPlugin("comp");

relement = compPlugin.createReplacedElement();
relement.setIdRef("C");
relement.setSubmodelRef("GRID_2_2_cell");

# create Parameters

param = model.createParameter();
param.initDefaults();
param.setId("q1_X");
param.setValue(1);

param = model.createParameter();
param.initDefaults();
param.setId("q1_Y");
param.setValue(1);

param = model.createParameter();
param.initDefaults();
param.setId("q2_X");
param.setValue(2);

param = model.createParameter();
param.initDefaults();
param.setId("q2_Y");
param.setValue(1);

param = model.createParameter();
param.initDefaults();
param.setId("q3_X");
param.setValue(1);

param = model.createParameter();
param.initDefaults();
param.setId("q3_Y");
param.setValue(2);

param = model.createParameter();
param.initDefaults();
param.setId("q4_X");
param.setValue(2);

param = model.createParameter();
param.initDefaults();
param.setId("q4_Y");
param.setValue(2);

# create SubModels

mplugin = model.getPlugin("comp");

submodel = mplugin.createSubmodel();
submodel.setId("GRID_1_1_cell");
submodel.setModelRef("Cell");

submodel = mplugin.createSubmodel();
submodel.setId("GRID_1_2_cell");
submodel.setModelRef("Cell");

submodel = mplugin.createSubmodel();
submodel.setId("GRID_2_1_cell");
submodel.setModelRef("Cell");

submodel = mplugin.createSubmodel();
submodel.setId("GRID_2_2_cell");
submodel.setModelRef("Cell");

# create the ModelDefinition

dplugin = document.getPlugin("comp");

mdef = dplugin.createModelDefinition();
mdef.setId("Cell");

compartment = mdef.createCompartment();
compartment.initDefaults();
compartment.setId("C");
compartment.setSpatialDimensions(2.0);
compartment.setSize(1.0);

species = mdef.createSpecies();
species.setId("R");
species.setCompartment("C");
species.setHasOnlySubstanceUnits(False);
species.setBoundaryCondition(False);
species.setConstant(False);

species = mdef.createSpecies();
species.setId("S");
species.setCompartment("C");
species.setHasOnlySubstanceUnits(False);
species.setBoundaryCondition(False);
species.setConstant(False);

reaction = mdef.createReaction();
reaction.setId("Degradation_R");
reaction.setReversible(False);
reaction.setFast(False);
reaction.setCompartment("C");

reactant = reaction.createReactant();  
reactant.setSpecies("R");
reactant.setStoichiometry(1);
reactant.setConstant(True);

reaction = mdef.createReaction();
reaction.setId("Degradation_S");
reaction.setReversible(False);
reaction.setFast(False);
reaction.setCompartment("C");

reactant = reaction.createReactant();
reactant.setSpecies("S");
reactant.setStoichiometry(1);
reactant.setConstant(True);
document.checkConsistency();

event = mdef.createEvent();
event.setId("event0");
event.setUseValuesFromTriggerTime(False);

eplugin = event.getPlugin("dyn");
eplugin.setApplyToAll(True);
eplugin.setCboTerm("http://cbo.biocomplexity.indiana.edu/svn/cbo/trunk/CBO_1_0.owl#CellDivision");

trigger = event.createTrigger();
trigger.setInitialValue(False);
trigger.setPersistent(False);
trigger.setMath(parseFormula("True"));

if (document.getNumErrors(LIBSBML_SEV_ERROR) > 0):
  document.printErrors();

writeSBML(document,"dyn_example2.xml");
