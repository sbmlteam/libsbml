/**
 * @file    printMulti.cs
 * @brief   multi print example
 * @author  Sarah Keating
 * @author  Frank Bergmann
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

using System;
using libsbmlcs;

public class printMulti
{

  private static void printCompartmentInfo(Compartment compartment)
  {
    var plugin = (MultiCompartmentPlugin)compartment.getPlugin("multi");
    var name = compartment.isSetName() ? compartment.getName() : compartment.getId();

    Console.WriteLine("Compartment {0}: isType = {1}", name, plugin.getIsType());
    for (int i = 0; i < plugin.getNumCompartmentReferences(); ++i)
    {
      var current = plugin.getCompartmentReference(i);
      Console.WriteLine("  compartmentReference: {0}", current.getCompartment());
    }

    Console.WriteLine();
  }


  private static void printSpeciesInfo(Species species)
  {
    var plugin = (MultiSpeciesPlugin)species.getPlugin("multi");
    var name = species.isSetName() ? species.getName() : species.getId();

    Console.WriteLine("Species {0}: speciesType = {1}", name, plugin.getSpeciesType());
    for (int i = 0; i < plugin.getNumOutwardBindingSites(); ++i)
    {
      var current = plugin.getOutwardBindingSite(i);
      Console.WriteLine("  outwardBindingSite bindingStatus={0} component={1}",
        libsbml.BindingStatus_toString(current.getBindingStatus()), current.getComponent());
    }

    for (int i = 0; i < plugin.getNumSpeciesFeatures(); ++i)
    {
      var current = plugin.getSpeciesFeature(i);
      Console.WriteLine("  speciesFeature speciesFeatureType={0} occur={1}", current.getSpeciesFeatureType(), current.getOccur());
      for (int j = 0; j < current.getNumSpeciesFeatureValues(); ++j)
      {
        var element = current.getSpeciesFeatureValue(j);
        Console.WriteLine("    speciesFeatureValue value={0}", element.getValue());
      }
    }


    Console.WriteLine();
  }

  private static void printMultiMathReferences(ASTNode node)
  {
    if (node == null) return;

    for (int i = 0; i < node.getNumChildren(); ++i)
    {
      var current = node.getChild(i);
      printMultiMathReferences(current);
    }

    var plugin = node.getPlugin("multi") as MultiASTPlugin;
    if (plugin == null) return;

    if (plugin.isSetRepresentationType())
      Console.WriteLine("   math representationType={0}", plugin.getRepresentationType());

    if (plugin.isSetSpeciesReference())
      Console.WriteLine("   math speciesReference={0}", plugin.getSpeciesReference());

  }

  private static void printReactionInfo(Reaction reaction)
  {
    var name = reaction.isSetName() ? reaction.getName() : reaction.getId();
    bool isIntraSpeciesReaction = reaction is IntraSpeciesReaction;
    Console.WriteLine("Reaction {0}: isIntraSpeciesReaction={1}", name, isIntraSpeciesReaction);

    for (int i = 0; i < reaction.getNumReactants(); ++i)
    {
      var current = reaction.getReactant(i);
      var plugin = (MultiSpeciesReferencePlugin)current.getPlugin("multi");
      if (plugin == null) continue;

      Console.WriteLine("  reactant {0}: compartmentReference={1}", current.getSpecies(), plugin.getCompartmentReference());
      for (int j = 0; j < plugin.getNumSpeciesTypeComponentMapInProducts(); ++j)
      {
        var element = plugin.getSpeciesTypeComponentMapInProduct(j);
        Console.WriteLine("    speciesTypeComponentMapInProduct: reactant={0} reactantComponent={1} productComponent={2}",
          element.getReactant(), element.getReactantComponent(), element.getProductComponent());
      }
      Console.WriteLine();
    }

    for (int i = 0; i < reaction.getNumProducts(); ++i)
    {
      var current = reaction.getProduct(i);
      var plugin = (MultiSpeciesReferencePlugin)current.getPlugin("multi");
      if (plugin == null) continue;

      Console.WriteLine("  product {0}: compartmentReference={1}", current.getSpecies(), plugin.getCompartmentReference());
      for (int j = 0; j < plugin.getNumSpeciesTypeComponentMapInProducts(); ++j)
      {
        var element = plugin.getSpeciesTypeComponentMapInProduct(j);
        Console.WriteLine("    speciesTypeComponentMapInProduct: reactant={0} reactantComponent={1} productComponent={2}",
          element.getReactant(), element.getReactantComponent(), element.getProductComponent());
      }
      Console.WriteLine();
    }


    if (!reaction.isSetKineticLaw() || !reaction.getKineticLaw().isSetMath())
    {
      Console.WriteLine();
      return;
    }

    printMultiMathReferences(reaction.getKineticLaw().getMath());

    Console.WriteLine();

  }

  private static void printModelInfo(Model model)
  {
    var plugin = (MultiModelPlugin)model.getPlugin("multi");

    for (int i = 0; i < plugin.getNumMultiSpeciesTypes(); ++i)
    {
      var current = plugin.getMultiSpeciesType(i);
      bool isBindingSiteSpeciesType = current is BindingSiteSpeciesType;
      Console.WriteLine("speciesType id={0} name={1} compartment={2} isBindingSiteSpeciesType={3}", current.getId(), current.getName(), current.getCompartment(), isBindingSiteSpeciesType);

      for (int j = 0; j < current.getNumSpeciesTypeInstances(); ++j)
      {
        var element = current.getSpeciesTypeInstance(j);
        Console.WriteLine("  speciesTypeInstance id={0} name={1} speciesType={2}", element.getId(), element.getName(), element.getSpeciesType());
      }

      for (int j = 0; j < current.getNumSpeciesTypeComponentIndexes(); ++j)
      {
        var element = current.getSpeciesTypeComponentIndex(j);
        Console.WriteLine("  speciesTypeComponentIndex id={0} component={1}", element.getId(), element.getComponent());
      }

      for (int j = 0; j < current.getNumInSpeciesTypeBonds(); ++j)
      {
        var element = current.getInSpeciesTypeBond(j);
        Console.WriteLine("  inSpeciesTypeBond bindingSite1={0} bindingSite2={1}", element.getBindingSite1(), element.getBindingSite2());
      }

      Console.WriteLine();

    }

    Console.WriteLine();

  }

  static void Main(string[] args)
  {

    if (args.Length < 1)
    {
      Console.WriteLine("usage: printMulti sbml-file");
      return;
    }

    SBMLDocument document = libsbml.readSBMLFromFile(args[0]);

    if (document.getNumErrors(libsbml.LIBSBML_SEV_ERROR) > 0)
    {
      document.printErrors();
      return;
    }

    Model model = document.getModel();

    // print multi model information
    printModelInfo(model);

    // print multi compartment information
    for (int i = 0; i < model.getNumCompartments(); ++i)
    {
      printCompartmentInfo(model.getCompartment(i));
    }

    // print multi species information
    for (int i = 0; i < model.getNumSpecies(); ++i)
    {
      printSpeciesInfo(model.getSpecies(i));
    }


    // print multi reaction information
    for (int i = 0; i < model.getNumReactions(); ++i)
    {
      printReactionInfo(model.getReaction(i));
    }

  }
}
