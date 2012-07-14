/**
 * @file    callExternalValidator.cpp
 * @brief   Example that shows how to call an external program for validation
 * @author  Frank T. Bergmann
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

import java.util.List;
import java.util.Vector;

import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLExternalValidator;
import org.sbml.libsbml.libsbml;

public class callExternalValidator {
	public static void main(String[] args) {
		System.loadLibrary("sbmlj");

		if (args.length < 3) {
			System.out
					.println("Usage: callExternalValidator filename externalValidator [ tempSBMLFile outputFile [ ADDITIONAL-ARGS] ]");
			System.exit(1);
		}

		String filename = args[0];

		// read additional args
		String externalValidator = args[1];

		String tempSBMLFileName = filename + "_temp.xml";
		if (args.length > 2)
			tempSBMLFileName = args[2];

		String outputFile = filename + "_out.xml";
		if (args.length > 3)
			outputFile = args[3];

		List<String> additionalArgs = new Vector<String>();
		for (int i = 4; i < args.length; i++)
			additionalArgs.add(args[i]);

		// add the output file as additional arg
		additionalArgs.add(outputFile);

		// read the file name
		SBMLDocument document = libsbml.readSBML(filename);

		// create a new external validator that will write the model to
		// tempFile, then call teh externalValidator with the given number of
		// arguments
		// to produce the output file. This output file will then be parsed and
		// its
		// errors will be added to the error log.
		SBMLExternalValidator validator = new SBMLExternalValidator();

		validator.setProgram(externalValidator);
		validator.setSBMLFileName(tempSBMLFileName);
		validator.setOutputFileName(outputFile);
		for (String item : additionalArgs) {
			validator.addArgument(item);
		}

		// this means that the external program will be called with the
		// following arguments
		//
		// externalValidator tempSBMLFileName additionalArgs
		//
		// (where additionalargs contains the output file as last argument)
		//
		// The output file that is generated should be an XML document following
		// the
		// Validator XML format as described here:
		// http://sbml.org/validator/api/#xml
		//

		// disable all regular checks
		document.setApplicableValidators((short) 0);

		// add a custom validator
		document.addValidator(validator);

		// check consistency like before
		int numErrors = (int) document.checkConsistency();

		// print errors and warnings
		document.printErrors();

		// return number of errors
		System.exit(numErrors);

	}
}
