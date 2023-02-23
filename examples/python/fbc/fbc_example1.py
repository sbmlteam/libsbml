#!/usr/bin/env python3
#
#  @file    fbc_example1.py
#  @brief   SBML FBC example
#  @author  Frank Bergmann
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
# ------------------------------------------------------------------------ -->

from libsbml import *

sbmlns = SBMLNamespaces(3,1,"fbc",1)

#  create the document

document = SBMLDocument(sbmlns)

# mark the fbc package as not required

document.setPackageRequired("fbc", False)

#  create the Model

model= document.createModel()

#  create the Compartment

compartment = model.createCompartment()
compartment.setId("compartment")
compartment.setConstant(True)
compartment.setSize(1)

# create the Species

species = model.createSpecies()
species.setId("Node1")
species.setCompartment("compartment")
species.setBoundaryCondition(False)
species.setConstant(False)
species.setHasOnlySubstanceUnits(False)

# just as an example for setting charge and chemicalFormula

splugin = species.getPlugin("fbc")
if splugin is not None:
  splugin.setCharge(-1)
  # chemical formula should follow the hill system
  splugin.setChemicalFormula("C2H5Br")


species = model.createSpecies()
species.setId("Node2")
species.setCompartment("compartment")
species.setBoundaryCondition(False)
species.setConstant(False)
species.setHasOnlySubstanceUnits(False)

species = model.createSpecies()
species.setId("Node3")
species.setCompartment("compartment")
species.setBoundaryCondition(False)
species.setConstant(False)
species.setHasOnlySubstanceUnits(False)

species = model.createSpecies()
species.setId("Node4")
species.setCompartment("compartment")
species.setBoundaryCondition(False)
species.setConstant(False)
species.setHasOnlySubstanceUnits(False)

species = model.createSpecies()
species.setId("Node5")
species.setCompartment("compartment")
species.setBoundaryCondition(False)
species.setConstant(False)
species.setHasOnlySubstanceUnits(False)

species = model.createSpecies()
species.setId("Node6")
species.setCompartment("compartment")
species.setBoundaryCondition(False)
species.setConstant(False)
species.setHasOnlySubstanceUnits(False)

species = model.createSpecies()
species.setId("Node7")
species.setCompartment("compartment")
species.setBoundaryCondition(False)
species.setConstant(False)
species.setHasOnlySubstanceUnits(False)

species = model.createSpecies()
species.setId("Node8")
species.setCompartment("compartment")
species.setBoundaryCondition(False)
species.setConstant(False)
species.setHasOnlySubstanceUnits(False)

species = model.createSpecies()
species.setId("Node0")
species.setCompartment("compartment")
species.setBoundaryCondition(True)
species.setConstant(False)
species.setHasOnlySubstanceUnits(False)

species = model.createSpecies()
species.setId("Node9")
species.setCompartment("compartment")
species.setBoundaryCondition(True)
species.setConstant(False)
species.setHasOnlySubstanceUnits(False)

# create reactions

reaction = model.createReaction()
reaction.setId("J0")
reaction.setReversible(False)
reaction.setFast(False)
reactant = reaction.createReactant()
reactant.setSpecies("Node0")
reactant.setStoichiometry(1)
reactant.setConstant(True)
product = reaction.createProduct()
product.setSpecies("Node1")
product.setStoichiometry(1)
product.setConstant(True)

reaction = model.createReaction()
reaction.setId("J1")
reaction.setReversible(False)
reaction.setFast(False)
reactant = reaction.createReactant()
reactant.setSpecies("Node1")
reactant.setStoichiometry(1)
reactant.setConstant(True)
product = reaction.createProduct()
product.setSpecies("Node2")
product.setStoichiometry(1)
product.setConstant(True)

reaction = model.createReaction()
reaction.setId("J2")
reaction.setReversible(False)
reaction.setFast(False)
reactant = reaction.createReactant()
reactant.setSpecies("Node2")
reactant.setStoichiometry(1)
reactant.setConstant(True)
product = reaction.createProduct()
product.setSpecies("Node3")
product.setStoichiometry(1)
product.setConstant(True)

reaction = model.createReaction()
reaction.setId("J3")
reaction.setReversible(False)
reaction.setFast(False)
reactant = reaction.createReactant()
reactant.setSpecies("Node1")
reactant.setStoichiometry(1)
reactant.setConstant(True)
product = reaction.createProduct()
product.setSpecies("Node4")
product.setStoichiometry(1)
product.setConstant(True)

reaction = model.createReaction()
reaction.setId("J4")
reaction.setReversible(False)
reaction.setFast(False)
reactant = reaction.createReactant()
reactant.setSpecies("Node4")
reactant.setStoichiometry(1)
reactant.setConstant(True)
product = reaction.createProduct()
product.setSpecies("Node3")
product.setStoichiometry(1)
product.setConstant(True)

reaction = model.createReaction()
reaction.setId("J5")
reaction.setReversible(False)
reaction.setFast(False)
reactant = reaction.createReactant()
reactant.setSpecies("Node3")
reactant.setStoichiometry(1)
reactant.setConstant(True)
product = reaction.createProduct()
product.setSpecies("Node5")
product.setStoichiometry(1)
product.setConstant(True)

reaction = model.createReaction()
reaction.setId("J6")
reaction.setReversible(False)
reaction.setFast(False)
reactant = reaction.createReactant()
reactant.setSpecies("Node5")
reactant.setStoichiometry(1)
reactant.setConstant(True)
product = reaction.createProduct()
product.setSpecies("Node6")
product.setStoichiometry(1)
product.setConstant(True)

reaction = model.createReaction()
reaction.setId("J7")
reaction.setReversible(False)
reaction.setFast(False)
reactant = reaction.createReactant()
reactant.setSpecies("Node6")
reactant.setStoichiometry(1)
reactant.setConstant(True)
product = reaction.createProduct()
product.setSpecies("Node7")
product.setStoichiometry(1)
product.setConstant(True)

reaction = model.createReaction()
reaction.setId("J8")
reaction.setReversible(False)
reaction.setFast(False)
reactant = reaction.createReactant()
reactant.setSpecies("Node5")
reactant.setStoichiometry(1)
reactant.setConstant(True)
product = reaction.createProduct()
product.setSpecies("Node8")
product.setStoichiometry(1)
product.setConstant(True)

reaction = model.createReaction()
reaction.setId("J9")
reaction.setReversible(False)
reaction.setFast(False)
reactant = reaction.createReactant()
reactant.setSpecies("Node8")
reactant.setStoichiometry(1)
reactant.setConstant(True)
product = reaction.createProduct()
product.setSpecies("Node7")
product.setStoichiometry(1)
product.setConstant(True)

reaction = model.createReaction()
reaction.setId("J10")
reaction.setReversible(False)
reaction.setFast(False)
reactant = reaction.createReactant()
reactant.setSpecies("Node7")
reactant.setStoichiometry(1)
reactant.setConstant(True)
product = reaction.createProduct()
product.setSpecies("Node9")
product.setStoichiometry(1)
product.setConstant(True)

# add FBC constraints

mplugin = model.getPlugin("fbc")

bound= mplugin.createFluxBound()
bound.setId("bound1")
bound.setReaction("J0")
bound.setOperation("equal")
bound.setValue(10)

objective = mplugin.createObjective()
objective.setId("obj1")
objective.setType("maximize")

mplugin.setActiveObjectiveId("obj1")

fluxObjective = objective.createFluxObjective()
fluxObjective.setReaction("J8")
fluxObjective.setCoefficient(1)

writeSBMLToFile(document,"fbc_example1.xml")
