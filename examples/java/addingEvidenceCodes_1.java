/**
 * \file    addingEvidenceCodes_1.cpp
 * \brief   adds controlled vocabulary terms to a reaction in a model
 * \author  Sarah Keating
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

import org.sbml.libsbml.CVTerm;
import org.sbml.libsbml.Reaction;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.libsbml;

public class addingEvidenceCodes_1 {
	public static void main(String[] args) {

		System.loadLibrary("sbmlj");

		SBMLDocument d;

		long errors, n;
		Reaction r;

		if (args.length != 2) {
			System.out
					.print("\n"
							+ "  usage: addingEvidenceCodes_1 <input-filename> <output-filename>"
							+ "\n"
							+ "  Adds controlled vocabulary term to a reaction"
							+ "\n" + "\n");
			System.exit(2);
		}

		d = libsbml.readSBML(args[1]);
		errors = d.getNumErrors();

		if (errors > 0) {
			System.out.print("Read Error(s):" + "\n");
			d.printErrors();

			System.out.print("Correct the above and re-run." + "\n");
		} else {

			n = d.getModel().getNumReactions();

			if (n <= 0) {
				System.out
						.print("Model has no reactions.\n Cannot add CV terms\n");
			} else {
				r = d.getModel().getReaction(0);

				/*
				 * check that the reaction has a metaid no CVTerms will be added
				 * if there is no metaid to reference
				 */
				if (!r.isSetMetaId())
					r.setMetaId("metaid_0000052");

				CVTerm cv1 = new CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
				cv1.setBiologicalQualifierType(libsbml.BQB_IS_DESCRIBED_BY);
				cv1.addResource("urn:miriam:obo.eco:ECO%3A0000183");

				r.addCVTerm(cv1);

				CVTerm cv2 = new CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
				cv2.setBiologicalQualifierType(libsbml.BQB_IS);
				cv2.addResource("urn:miriam:kegg.reaction:R00756");
				cv2.addResource("urn:miriam:reactome:REACT_736");

				r.addCVTerm(cv2);

				libsbml.writeSBML(d, args[1]);
			}
		}

		System.exit( (int) errors);
	}
}
