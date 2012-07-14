/**
 * @file    printMath.cpp
 * @brief   Prints Rule, Reaction, and Event formulas in a given SBML Document
 * @author  Ben Bornstein
 * @author  Sarah Keating
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using libsbmlcs;


public class PrintMath
{

    private static void
    printFunctionDefinition(int n, FunctionDefinition fd)
    {
        ASTNode math;
        string formula;


        if (fd.isSetMath())
        {
            Console.Write("FunctionDefinition " + n + ", " + fd.getId());

            math = fd.getMath();

            /* Print function arguments. */
            if (math.getNumChildren() > 1)
            {
                Console.Write("(" + (math.getLeftChild()).getName());

                for (n = 1; n < math.getNumChildren() - 1; ++n)
                {
                    Console.Write(", " + (math.getChild(n)).getName());
                }
            }

            Console.Write(") := ");

            /* Print function body. */
            if (math.getNumChildren() == 0)
            {
                Console.Write("(no body defined)");
            }
            else
            {
                math = math.getChild(math.getNumChildren() - 1);
                formula = libsbml.formulaToString(math);
                Console.Write(formula + Environment.NewLine);

            }
        }
    }


    private static void
    printRuleMath(int n, Rule r)
    {
        string formula;


        if (r.isSetMath())
        {
            formula = libsbml.formulaToString(r.getMath());

            if (r.getVariable().Length > 0)
            {
                Console.Write("Rule " + n + ", formula: "
                                 + r.getVariable() + " = " + formula + Environment.NewLine);
            }
            else
            {
                Console.Write("Rule " + n + ", formula: "
                                 + formula + " = 0" + Environment.NewLine);
            }

        }
    }


    private static void
    printReactionMath(int n, Reaction r)
    {
        string formula;
        KineticLaw kl;


        if (r.isSetKineticLaw())
        {
            kl = r.getKineticLaw();

            if (kl.isSetMath())
            {
                formula = libsbml.formulaToString(kl.getMath());
                Console.Write("Reaction " + n + ", formula: " + formula + Environment.NewLine);
            }
        }
    }


    private static void
    printEventAssignmentMath(int n, EventAssignment ea)
    {
        string variable;
        string formula;


        if (ea.isSetMath())
        {
            variable = ea.getVariable();
            formula = libsbml.formulaToString(ea.getMath());

            Console.Write("  EventAssignment " + n
                                  + ", trigger: " + variable + " = " + formula + Environment.NewLine);

        }
    }


    private static void
    printEventMath(int n, Event e)
    {
        string formula;
        int i;


        if (e.isSetDelay())
        {
            formula = libsbml.formulaToString(e.getDelay().getMath());
            Console.Write("Event " + n + " delay: " + formula + Environment.NewLine);
        }

        if (e.isSetTrigger())
        {
            formula = libsbml.formulaToString(e.getTrigger().getMath());
            Console.Write("Event " + n + " trigger: " + formula + Environment.NewLine);
        }

        for (i = 0; i < e.getNumEventAssignments(); ++i)
        {
            printEventAssignmentMath(i + 1, e.getEventAssignment(i));
        }

        Console.WriteLine();
    }


    private static void
    printMath(Model m)
    {
        int n;


        for (n = 0; n < m.getNumFunctionDefinitions(); ++n)
        {
            printFunctionDefinition(n + 1, m.getFunctionDefinition(n));
        }

        for (n = 0; n < m.getNumRules(); ++n)
        {
            printRuleMath(n + 1, m.getRule(n));
        }

        Console.WriteLine();

        for (n = 0; n < m.getNumReactions(); ++n)
        {
            printReactionMath(n + 1, m.getReaction(n));
        }

        Console.WriteLine();

        for (n = 0; n < m.getNumEvents(); ++n)
        {
            printEventMath(n + 1, m.getEvent(n));
        }
    }


    public static int Main(string[] args)
    {

        if (args.Length != 1)
        {
            Console.Write(Environment.NewLine + "Usage: printMath filename" + Environment.NewLine + Environment.NewLine);
            return 1;
        }

        string filename = args[0];
        SBMLDocument document = libsbml.readSBML(filename);

        if (document.getNumErrors() > 0)
        {
            Console.Error.Write("Encountered the following SBML errors:" + Environment.NewLine);
            document.printErrors(new OStream(OStream.CERR));
            return 1;
        }

        Model model = document.getModel();

        if (model == null)
        {
            Console.Write("No model present." + Environment.NewLine);
            return 1;
        }

        printMath(model);
        Console.WriteLine();
        return 0;
    }

}
