/**
 * @file    printModel.cpp
 * @brief   Prints some information about the top-level model
 * @author  Sarah Keating
 * @author  Ben Bornstein
 * @author  Michael Hucka
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using libsbmlcs;

public class PrintSBML
{
    public static int Main(string[] args)
    {

        if (args.Length != 1)
        {
            Console.Write(Environment.NewLine + "Usage: printSBML filename" + Environment.NewLine + Environment.NewLine);
            return 1;
        }

        string filename = args[0];
        SBMLDocument document = libsbml.readSBML(filename);

        if (document.getNumErrors() > 0)
        {
            Console.WriteLine("Encountered the following SBML errors:" + Environment.NewLine);
            document.printErrors();
            return 1;
        }

        int level = (int)document.getLevel();
        int version = (int)document.getVersion();

        Console.Write(Environment.NewLine
                              + "File: " + filename
                              + " (Level " + level + ", version " + version + ")" + Environment.NewLine);

        Model model = document.getModel();

        if (model == null)
        {
            Console.Write("No model present." + Environment.NewLine);
            return 1;
        }

        Console.Write("               "
                              + (level == 1 ? "name: " : "  id: ")
                              + (model.isSetId() ? model.getId() : "(empty)") + Environment.NewLine);

        if (model.isSetSBOTerm())
            Console.Write("      model sboTerm: " + model.getSBOTerm() + Environment.NewLine);

        Console.Write("functionDefinitions: " + model.getNumFunctionDefinitions() + Environment.NewLine);
        Console.Write("    unitDefinitions: " + model.getNumUnitDefinitions() + Environment.NewLine);
        Console.Write("   compartmentTypes: " + model.getNumCompartmentTypes() + Environment.NewLine);
        Console.Write("        specieTypes: " + model.getNumSpeciesTypes() + Environment.NewLine);
        Console.Write("       compartments: " + model.getNumCompartments() + Environment.NewLine);
        Console.Write("            species: " + model.getNumSpecies() + Environment.NewLine);
        Console.Write("         parameters: " + model.getNumParameters() + Environment.NewLine);
        Console.Write(" initialAssignments: " + model.getNumInitialAssignments() + Environment.NewLine);
        Console.Write("              rules: " + model.getNumRules() + Environment.NewLine);
        Console.Write("        constraints: " + model.getNumConstraints() + Environment.NewLine);
        Console.Write("          reactions: " + model.getNumReactions() + Environment.NewLine);
        Console.Write("             events: " + model.getNumEvents() + Environment.NewLine);
        Console.Write(Environment.NewLine);

        return 0;
    }
}
