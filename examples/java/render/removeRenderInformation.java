// 
// @file    removeRenderInformation.java
// @brief   removes render information from the given SBML file
// @author  Frank Bergmann
// 
// This file is part of libSBML.  Please visit http://sbml.org for more
// information about SBML, and the latest version of libSBML.
// 

import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBasePlugin;
import org.sbml.libsbml.libsbml;

public class removeRenderInformation {
	public static void main(String[] args) {

		if (args.length != 2) {
			System.err
					.println("usage: removeRenderInformation <input file> <output file>");
			System.err
					.println("       removes the render information object from the input file.");
			System.exit(1);
		}

		String inputFile = args[0];
		String outputFile = args[1];

		SBMLDocument doc = libsbml.readSBMLFromFile(inputFile);
		long numErrors = doc.getNumErrors();

		if (numErrors > 0) {
			System.err.println("Encountered errors while reading the file. ");
			System.err
					.println("Please correct the following errors and try again.");
			doc.printErrors();
			System.exit(2);
		}

		SBasePlugin plugin = doc.getPlugin("render");
		if (plugin == null) {
			System.out
					.println("Warning: the document did not use the render information in the first place. ");
		} else {
			// simply disable the package, this will cause it to no longer being
			// written out
			doc.disablePackage(plugin.getURI(), plugin.getPrefix());
		}

		libsbml.writeSBMLToFile(doc, outputFile);

		System.exit(0);
	}

}
