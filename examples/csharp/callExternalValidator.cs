/**
 * @file    callExternalValidator.cpp
 * @brief   Example that shows how to call an external program for validation
 * @author  Frank T. Bergmann
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


using System;
using System.Collections.Generic;
using libsbmlcs;

internal class CallExternalValidator
{
    public static int Main(string[] args)
    {
        if (args.Length < 3)
        {
            Console.WriteLine("Usage: callExternalValidator filename externalValidator [ tempSBMLFile outputFile [ ADDITIONAL-ARGS] ]");
            return 1;
        }

        string filename = args[0];

        // read additional args
        string externalValidator = args[1];

        string tempSBMLFileName = filename + "_temp.xml";
        if (args.Length > 2)
            tempSBMLFileName = args[2];

        string outputFile = filename + "_out.xml";
        if (args.Length > 3)
            outputFile = args[3];

        List<string> additionalArgs = new List<string>();
        for (int i = 4; i < args.Length; i++)
            additionalArgs.Add(args[i]);

        // add the output file as additional arg
        additionalArgs.Add(outputFile);

        // read the file name
        SBMLDocument document = libsbml.readSBML(filename);

        // create a new external validator that will write the model to 
        // tempFile, then call teh externalValidator with the given number of arguments
        // to produce the output file. This output file will then be parsed and its
        // errors will be added to the error log.
        SBMLExternalValidator validator = new SBMLExternalValidator();

        validator.setProgram(externalValidator);
        validator.setSBMLFileName(tempSBMLFileName);
        validator.setOutputFileName(outputFile);
        foreach (string item in additionalArgs)
        {
            validator.addArgument(item);
        }

        // this means that the external program will be called with the following arguments
        // 
        //    externalValidator tempSBMLFileName additionalArgs
        //
        // (where additionalargs contains the output file as last argument)
        //
        // The output file that is generated should be an XML document following the 
        // Validator XML format as described here: http://sbml.org/validator/api/#xml
        //

        // disable all regular checks
        document.setApplicableValidators(0);

        // add a custom validator
        document.addValidator(validator);

        // check consistency like before
        int numErrors = (int)document.checkConsistency();

        // print errors and warnings
        document.printErrors();

        // return number of errors
        return numErrors;


    }
}
