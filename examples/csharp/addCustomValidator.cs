/**
 * @file    addCustomValidator.cpp
 * @brief   Example creating a custom validator to be called during validation
 * @author  Frank T. Bergmann
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

using System;
using libsbmlcs;

/** 
 * Declares a custom validator to be called. This allows you to validate 
 * any aspect of an SBML Model that you want to be notified about. You could 
 * use this to notify your application that a model contains an unsupported 
 * feature of SBML (either as warning). 
 * 
 * In this example the validator will go through the model and test for the 
 * presence of 'fast' reactions and algebraic rules. If either is used a 
 * warning will be added to the error log. 
 */
public class MyCustomValidator : SBMLValidator
{
    public MyCustomValidator()
        : base()
    {
    }
    public MyCustomValidator(MyCustomValidator orig)
        : base(orig)
    {

    }

    public new SBMLValidator clone() { return new MyCustomValidator(this); }

    public new virtual long validate()
    {
        // if we don't have a model we don't apply this validator.
        if (getDocument() == null || getModel() == null)
            return 0;

        // if we have no rules and reactions we don't apply this validator either
        if (getModel().getNumReactions() == 0 && getModel().getNumRules() == 0)
            return 0;

        int numErrors = 0;
        // test for algebraic rules
        for (int i = 0; i < getModel().getNumRules(); i++)
        {
            if (getModel().getRule(i).getTypeCode() == libsbml.SBML_ALGEBRAIC_RULE)
            {

                getErrorLog().add(new SBMLError(99999, 3, 1,
                  "This model uses algebraic rules, however this application does not support them.",
                  0, 0,
                  libsbml.LIBSBML_SEV_WARNING, // or LIBSBML_SEV_ERROR if you want to stop
                  libsbml.LIBSBML_CAT_SBML // or whatever category you prefer
                  ));

                numErrors++;
            }
        }

        // test for fast reactions
        for (int i = 0; i < getModel().getNumReactions(); i++)
        {
            // test whether value is set, and true
            if (getModel().getReaction(i).isSetFast() &&
                getModel().getReaction(i).getFast())
            {

                getErrorLog().add(new SBMLError(99999, 3, 1,
                  "This model uses fast reactions, however this application does not support them.",
                  0, 0,
                  libsbml.LIBSBML_SEV_WARNING, // or LIBSBML_SEV_ERROR if you want to stop
                  libsbml.LIBSBML_CAT_SBML // or whatever category you prefer
                  ));

                numErrors++;

            }
        }

        return numErrors;
    }

    public static int Main(string[] args)
    {
        if (args.Length != 1)
        {
            Console.WriteLine("Usage: addCustomValidator filename"); ;
            return 1;
        }


        // read the file name
        SBMLDocument document = libsbml.readSBML(args[0]);

        // add a custom validator
        document.addValidator(new MyCustomValidator());

        // check consistency like before
        int numErrors = (int)document.checkConsistency();

        // print errors and warnings
        document.printErrors();

        // return number of errors
        return numErrors;

    }

};

