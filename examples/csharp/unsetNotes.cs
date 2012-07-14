/**
 * @file    unsetNotes.cpp
 * @brief   unset notes for each element
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

public class UnsetNotes
{
    public static int Main(string[] args)
    {
        if (args.Length != 2)
        {
            Console.Write(Environment.NewLine + "Usage: unsetNotes <input-filename> <output-filename>" + Environment.NewLine + Environment.NewLine);
            return 1;
        }


        int i, j;
        string filename = args[0];
        SBMLDocument document;


        document = libsbml.readSBML(filename);


        int errors = (int)document.getNumErrors();

        if (errors > 0)
        {
            document.printErrors();

            return errors;
        }

        Model m = document.getModel();
        m.unsetNotes();

        for (i = 0; i < m.getNumReactions(); i++)
        {
            Reaction re = m.getReaction(i);
            re.unsetNotes();

            for (j = 0; j < re.getNumReactants(); j++)
            {
                SpeciesReference rt = re.getReactant(j);
                rt.unsetNotes();
            }

            for (j = 0; j < re.getNumProducts(); j++)
            {
                SpeciesReference rt = re.getProduct(j);
                rt.unsetNotes();
            }

            for (j = 0; j < re.getNumModifiers(); j++)
            {
                ModifierSpeciesReference md = re.getModifier(j);
                md.unsetNotes();
            }

            if (re.isSetKineticLaw())
            {
                KineticLaw kl = re.getKineticLaw();
                kl.unsetNotes();

                for (j = 0; j < kl.getNumParameters(); j++)
                {
                    Parameter pa = kl.getParameter(j);
                    pa.unsetNotes();
                }
            }

        }

        for (i = 0; i < m.getNumSpecies(); i++)
        {
            Species sp = m.getSpecies(i);
            sp.unsetNotes();
        }

        for (i = 0; i < m.getNumCompartments(); i++)
        {
            Compartment sp = m.getCompartment(i);
            sp.unsetNotes();
        }

        for (i = 0; i < m.getNumFunctionDefinitions(); i++)
        {
            FunctionDefinition sp = m.getFunctionDefinition(i);
            sp.unsetNotes();
        }

        for (i = 0; i < m.getNumUnitDefinitions(); i++)
        {
            UnitDefinition sp = m.getUnitDefinition(i);
            sp.unsetNotes();
        }

        for (i = 0; i < m.getNumParameters(); i++)
        {
            Parameter sp = m.getParameter(i);
            sp.unsetNotes();
        }

        for (i = 0; i < m.getNumRules(); i++)
        {
            Rule sp = m.getRule(i);
            sp.unsetNotes();
        }

        for (i = 0; i < m.getNumInitialAssignments(); i++)
        {
            InitialAssignment sp = m.getInitialAssignment(i);
            sp.unsetNotes();
        }

        for (i = 0; i < m.getNumEvents(); i++)
        {
            Event sp = m.getEvent(i);
            sp.unsetNotes();

            for (j = 0; j < sp.getNumEventAssignments(); j++)
            {
                EventAssignment ea = sp.getEventAssignment(j);
                ea.unsetNotes();
            }
        }

        for (i = 0; i < m.getNumSpeciesTypes(); i++)
        {
            SpeciesType sp = m.getSpeciesType(i);
            sp.unsetNotes();
        }

        for (i = 0; i < m.getNumConstraints(); i++)
        {
            Constraint sp = m.getConstraint(i);
            sp.unsetNotes();
        }

        libsbml.writeSBML(document, args[1]);

        return errors;
    }


}
