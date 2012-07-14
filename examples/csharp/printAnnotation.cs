/**
 * @file    printAnnotation.cpp
 * @brief   Prints annotation strings for each element
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

public class PrintAnnotation
{
	private static void printAnnotation(SBase sb)
    {
		string id = "";

        if (sb.isSetId())
        {
            id = sb.getId();
        }
		printAnnotation(sb, id);
	}
	
    private static void printAnnotation(SBase sb, string id)
    {
        if (!sb.isSetAnnotation()) return;        

        Console.Write("----- " + sb.getElementName() + " (" + id
                          + ") annotation -----" + Environment.NewLine);
        Console.Write(sb.getAnnotationString() + Environment.NewLine);
        Console.Write(Environment.NewLine);
    }


    public static int Main(string[] args)
    {
        if (args.Length != 1)
        {
            Console.Write(Environment.NewLine + "Usage: printAnnotation filename" + Environment.NewLine + Environment.NewLine);
            return 1;
        }

        int i, j;
        string filename = args[0];
        SBMLDocument document;


        document = libsbml.readSBML(filename);

        int errors = (int)document.getNumErrors();

        Console.Write(Environment.NewLine);
        Console.Write("filename: " + filename + Environment.NewLine);
        Console.Write(Environment.NewLine);

        if (errors > 0)
        {
            document.printErrors();

            return errors;
        }


        /* Model */

        Model m = document.getModel();
        printAnnotation(m);

        for (i = 0; i < m.getNumReactions(); i++)
        {
            Reaction re = m.getReaction(i);
            printAnnotation(re);

            /* SpeciesReference (Reacatant) */

            for (j = 0; j < re.getNumReactants(); j++)
            {
                SpeciesReference rt = re.getReactant(j);
                if (rt.isSetAnnotation()) Console.Write("   ");
                printAnnotation(rt, (rt.isSetSpecies() ? rt.getSpecies() : ""));
            }

            /* SpeciesReference (Product) */

            for (j = 0; j < re.getNumProducts(); j++)
            {
                SpeciesReference rt = re.getProduct(j);
                if (rt.isSetAnnotation()) Console.Write("   ");
                printAnnotation(rt, (rt.isSetSpecies() ? rt.getSpecies() : ""));
            }

            /* ModifierSpeciesReference (Modifiers) */

            for (j = 0; j < re.getNumModifiers(); j++)
            {
                ModifierSpeciesReference md = re.getModifier(j);
                if (md.isSetAnnotation()) Console.Write("   ");
                printAnnotation(md, (md.isSetSpecies() ? md.getSpecies() : ""));
            }

            /* KineticLaw */

            if (re.isSetKineticLaw())
            {
                KineticLaw kl = re.getKineticLaw();
                if (kl.isSetAnnotation()) Console.Write("   ");
                printAnnotation(kl);

                /* Parameter */

                for (j = 0; j < kl.getNumParameters(); j++)
                {
                    Parameter pa = kl.getParameter(j);
                    if (pa.isSetAnnotation()) Console.Write("      ");
                    printAnnotation(pa);
                }
            }

        }

        /* Species */

        for (i = 0; i < m.getNumSpecies(); i++)
        {
            Species sp = m.getSpecies(i);
            printAnnotation(sp);
        }

        /* Compartments */

        for (i = 0; i < m.getNumCompartments(); i++)
        {
            Compartment sp = m.getCompartment(i);
            printAnnotation(sp);
        }

        /* FunctionDefinition */

        for (i = 0; i < m.getNumFunctionDefinitions(); i++)
        {
            FunctionDefinition sp = m.getFunctionDefinition(i);
            printAnnotation(sp);
        }

        /* UnitDefinition */

        for (i = 0; i < m.getNumUnitDefinitions(); i++)
        {
            UnitDefinition sp = m.getUnitDefinition(i);
            printAnnotation(sp);
        }

        /* Parameter */

        for (i = 0; i < m.getNumParameters(); i++)
        {
            Parameter sp = m.getParameter(i);
            printAnnotation(sp);
        }

        /* Rule */

        for (i = 0; i < m.getNumRules(); i++)
        {
            Rule sp = m.getRule(i);
            printAnnotation(sp);
        }

        /* InitialAssignment */

        for (i = 0; i < m.getNumInitialAssignments(); i++)
        {
            InitialAssignment sp = m.getInitialAssignment(i);
            printAnnotation(sp);
        }

        /* Event */

        for (i = 0; i < m.getNumEvents(); i++)
        {
            Event sp = m.getEvent(i);
            printAnnotation(sp);

            /* Trigger */

            if (sp.isSetTrigger())
            {
                Trigger tg = sp.getTrigger();
                if (tg.isSetAnnotation()) Console.Write("   ");
                printAnnotation(tg);
            }

            /* Delay */

            if (sp.isSetDelay())
            {
                Delay dl = sp.getDelay();
                if (dl.isSetAnnotation()) Console.Write("   ");
                printAnnotation(dl);
            }

            /* EventAssignment */

            for (j = 0; j < sp.getNumEventAssignments(); j++)
            {
                EventAssignment ea = sp.getEventAssignment(j);
                if (ea.isSetAnnotation()) Console.Write("   ");
                printAnnotation(ea);
            }
        }

        /* SpeciesType */

        for (i = 0; i < m.getNumSpeciesTypes(); i++)
        {
            SpeciesType sp = m.getSpeciesType(i);
            printAnnotation(sp);
        }

        /* Constraints */

        for (i = 0; i < m.getNumConstraints(); i++)
        {
            Constraint sp = m.getConstraint(i);
            printAnnotation(sp);
        }

        return errors;
    }

}
