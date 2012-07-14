/**
 * @file    printNotes.cpp
 * @brief   Prints notes strings for each element
 * @author  Akiya Jouraku
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */



using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using libsbmlcs;

public class PrintNotes
{
	private static void printNotes(SBase sb)
    {
        string id = "";

        if (sb.isSetId())
        {
            id = sb.getId();
        }
		printNotes(sb, id);
	}

    private static void printNotes(SBase sb, string id)
    {
        if (!sb.isSetNotes()) return;


        Console.WriteLine("----- " + sb.getElementName() + " (" + id
        + ") notes -----");
        Console.WriteLine(sb.getNotesString());
        Console.WriteLine();
    }


    public static int Main(string[] args)
    {
        if (args.Length != 1)
        {
            Console.WriteLine("Usage: printNotes filename");
            return 1;
        }

        int i, j;
        string filename = args[0];
        SBMLDocument document;


        document = libsbml.readSBML(filename);

        int errors = (int)document.getNumErrors();

        Console.WriteLine();
        Console.WriteLine("filename: " + filename);
        Console.WriteLine();

        if (errors > 0)
        {
            document.printErrors();

            return errors;
        }

        /* Model */

        Model m = document.getModel();
        printNotes(m);

        for (i = 0; i < m.getNumReactions(); i++)
        {
            Reaction re = m.getReaction(i);
            printNotes(re);

            /* SpeciesReference (Reacatant) */

            for (j = 0; j < re.getNumReactants(); j++)
            {
                SpeciesReference rt = re.getReactant(j);
                if (rt.isSetNotes()) Console.WriteLine("   ");
                printNotes(rt, (rt.isSetSpecies() ? rt.getSpecies() : ""));
            }

            /* SpeciesReference (Product) */

            for (j = 0; j < re.getNumProducts(); j++)
            {
                SpeciesReference rt = re.getProduct(j);
                if (rt.isSetNotes()) Console.WriteLine("   ");
                printNotes(rt, (rt.isSetSpecies() ? rt.getSpecies() : ""));
            }

            /* ModifierSpeciesReference (Modifier) */

            for (j = 0; j < re.getNumModifiers(); j++)
            {
                ModifierSpeciesReference md = re.getModifier(j);
                if (md.isSetNotes()) Console.WriteLine("   ");
                printNotes(md, (md.isSetSpecies() ? md.getSpecies() : ""));
            }

            /* Kineticlaw */

            if (re.isSetKineticLaw())
            {
                KineticLaw kl = re.getKineticLaw();
                if (kl.isSetNotes()) Console.WriteLine("   ");
                printNotes(kl);

                /* Parameter */

                for (j = 0; j < kl.getNumParameters(); j++)
                {
                    Parameter pa = kl.getParameter(j);
                    if (pa.isSetNotes()) Console.WriteLine("   ");
                    printNotes(pa);
                }
            }

        }

        /* Species */

        for (i = 0; i < m.getNumSpecies(); i++)
        {
            Species sp = m.getSpecies(i);
            printNotes(sp);
        }

        /* Compartment */

        for (i = 0; i < m.getNumCompartments(); i++)
        {
            Compartment sp = m.getCompartment(i);
            printNotes(sp);
        }

        /* FunctionDefinition */

        for (i = 0; i < m.getNumFunctionDefinitions(); i++)
        {
            FunctionDefinition sp = m.getFunctionDefinition(i);
            printNotes(sp);
        }

        /* UnitDefinition */

        for (i = 0; i < m.getNumUnitDefinitions(); i++)
        {
            UnitDefinition sp = m.getUnitDefinition(i);
            printNotes(sp);
        }

        /* Parameter */

        for (i = 0; i < m.getNumParameters(); i++)
        {
            Parameter sp = m.getParameter(i);
            printNotes(sp);
        }

        /* Rule */

        for (i = 0; i < m.getNumRules(); i++)
        {
            Rule sp = m.getRule(i);
            printNotes(sp);
        }

        /* InitialAssignment */

        for (i = 0; i < m.getNumInitialAssignments(); i++)
        {
            InitialAssignment sp = m.getInitialAssignment(i);
            printNotes(sp);
        }

        /* Event */

        for (i = 0; i < m.getNumEvents(); i++)
        {
            Event sp = m.getEvent(i);
            printNotes(sp);

            /* Trigger */

            if (sp.isSetTrigger())
            {
                Trigger tg = sp.getTrigger();
                if (tg.isSetNotes()) Console.WriteLine("   ");
                printNotes(tg);
            }

            /* Delay */

            if (sp.isSetDelay())
            {
                Delay dl = sp.getDelay();
                if (dl.isSetNotes()) Console.WriteLine("   ");
                printNotes(dl);
            }

            /* EventAssignment */

            for (j = 0; j < sp.getNumEventAssignments(); j++)
            {
                EventAssignment ea = sp.getEventAssignment(j);
                if (ea.isSetNotes()) Console.WriteLine("   ");
                printNotes(ea);
            }
        }

        /* SpeciesType */

        for (i = 0; i < m.getNumSpeciesTypes(); i++)
        {
            SpeciesType sp = m.getSpeciesType(i);
            printNotes(sp);
        }

        /* Constraint */

        for (i = 0; i < m.getNumConstraints(); i++)
        {
            Constraint sp = m.getConstraint(i);
            printNotes(sp);
        }

        return errors;
    }

}
