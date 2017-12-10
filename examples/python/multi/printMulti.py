# 
# @file    printMulti.cs
# @brief   multi print example
# @author  Frank Bergmann
# 
# <!--------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
# Copyright (C) 2009-2013 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
# 
# Copyright (C) 2006-2008 by the California Institute of Technology,
#     Pasadena, CA, USA 
# 
# Copyright (C) 2002-2005 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. Japan Science and Technology Agency, Japan
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
# ------------------------------------------------------------------------ -->
# 

import sys
from libsbml import * 

def printCompartmentInfo(compartment):
  plugin = compartment.getPlugin("multi")
  name = compartment.getName() if compartment.isSetName()else compartment.getId()

  print("Compartment {0}: isType = {1}".format(name, plugin.getIsType()))
  for i in range(plugin.getNumCompartmentReferences()):
    current = plugin.getCompartmentReference(i)
    print("  compartmentReference: {0}".format(current.getCompartment()))

  print('')


def printSpeciesInfo(species):
  plugin = species.getPlugin("multi")
  name = species.getName() if species.isSetName() else species.getId()

  print("Species {0}: speciesType = {1}".format(name, plugin.getSpeciesType()))
  for i in range(plugin.getNumOutwardBindingSites()):
    current = plugin.getOutwardBindingSite(i)
    print("  outwardBindingSite bindingStatus={0} component={1}".format(
      BindingStatus_toString(current.getBindingStatus()), current.getComponent()))

  for i in range(plugin.getNumSpeciesFeatures()):
    current = plugin.getSpeciesFeature(i)
    print("  speciesFeature speciesFeatureType={0} occur={1}".format(current.getSpeciesFeatureType(), current.getOccur()))
    for j in range(current.getNumSpeciesFeatureValues()):
      element = current.getSpeciesFeatureValue(j)
      print("    speciesFeatureValue value={0}".format(element.getValue()))

  print('')


def printMultiMathReferences(node):
  if node is None:
    return

  for i in range(node.getNumChildren()):
    current = node.getChild(i)
    printMultiMathReferences(current)

  plugin = node.getPlugin("multi")
  if plugin is None:
    return

  if plugin.isSetRepresentationType():
    print("   math representationType={0}".format(plugin.getRepresentationType()))

  if plugin.isSetSpeciesReference():
    print("   math speciesReference={0}".format(plugin.getSpeciesReference()))


def printReactionInfo(reaction):
  name = reaction.getName() if reaction.isSetName() else reaction.getId()
  isIntraSpeciesReaction = isinstance(reaction, IntraSpeciesReaction)
  print("Reaction {0}: isIntraSpeciesReaction={1}".format(name, isIntraSpeciesReaction))

  for i in range(reaction.getNumReactants()):
    current = reaction.getReactant(i)
    plugin = current.getPlugin("multi")
    if plugin is None:
      continue

    print("  reactant {0}: compartmentReference={1}".format(current.getSpecies(), plugin.getCompartmentReference()))
    for j in range(plugin.getNumSpeciesTypeComponentMapInProducts()):
      element = plugin.getSpeciesTypeComponentMapInProduct(j)
      print("    speciesTypeComponentMapInProduct: reactant={0} reactantComponent={1} productComponent={2}".format(
        element.getReactant(), element.getReactantComponent(), element.getProductComponent()))

    print('')

  for i in range(reaction.getNumProducts()):
    current = reaction.getProduct(i)
    plugin = current.getPlugin("multi")
    if plugin is None:
      continue

    print("  product {0}: compartmentReference={1}".format(current.getSpecies(), plugin.getCompartmentReference()))
    for j in range(plugin.getNumSpeciesTypeComponentMapInProducts()):
      element = plugin.getSpeciesTypeComponentMapInProduct(j)
      print("    speciesTypeComponentMapInProduct: reactant={0} reactantComponent={1} productComponent={2}".format(
        element.getReactant(), element.getReactantComponent(), element.getProductComponent()))

    print('')

  if not reaction.isSetKineticLaw() or not reaction.getKineticLaw().isSetMath():
    print('')
    return

  printMultiMathReferences(reaction.getKineticLaw().getMath())

  print('')


def printModelInfo(model):
  plugin = model.getPlugin("multi")

  for i in range (plugin.getNumMultiSpeciesTypes()):
    current = plugin.getMultiSpeciesType(i)
    isBindingSiteSpeciesType = isinstance(current, BindingSiteSpeciesType)
    print("speciesType id={0} name={1} compartment={2} isBindingSiteSpeciesType={3}".format(current.getId(), current.getName(), current.getCompartment(), isBindingSiteSpeciesType))

    for j in range(current.getNumSpeciesTypeInstances()):
      element = current.getSpeciesTypeInstance(j)
      print("  speciesTypeInstance id={0} name={1} speciesType={2}".format(element.getId(), element.getName(), element.getSpeciesType()))

    for j in range(current.getNumSpeciesTypeComponentIndexes()):
      element = current.getSpeciesTypeComponentIndex(j)
      print("  speciesTypeComponentIndex id={0} component={1}".format(element.getId(), element.getComponent()))

    for j in range(current.getNumInSpeciesTypeBonds()):
      element = current.getInSpeciesTypeBond(j)
      print("  inSpeciesTypeBond bindingSite1={0} bindingSite2={1}".format(element.getBindingSite1(), element.getBindingSite2()))

    print('')

  print('')


def printMulti(fileName):
  document = readSBMLFromFile(fileName)

  if document.getNumErrors(LIBSBML_SEV_ERROR) > 0:
    document.printErrors()
    return

  model = document.getModel()

  # print multi model information
  printModelInfo(model)

  # print multi compartment information
  for i in range(model.getNumCompartments()):
    printCompartmentInfo(model.getCompartment(i))

  # print multi species information
  for i in range(model.getNumSpecies()):
    printSpeciesInfo(model.getSpecies(i))

  # print multi reaction information
  for i in range(model.getNumReactions()):
    printReactionInfo(model.getReaction(i))


if __name__ == "__main__":
  if len(sys.argv) < 2:
    print("usage: printMulti sbml-file")
  else:
    printMulti(sys.argv[1])

