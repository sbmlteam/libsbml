/**
 * \file    addingEvidenceCodes_1.cpp
 * \brief   adds controlled vocabulary terms to a reaction in a model
 * \author  Sarah Keating
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

using System;
using libsbmlcs;

public class AddingEvidenceCodes1
{
    public static int Main(string[] args)
    {

        SBMLDocument d;

        long errors, n;
        Reaction r;

        if (args.Length != 2)
        {
            Console.Write(Environment.NewLine
                                      + "  usage: addingEvidenceCodes_1 <input-filename> <output-filename>" + Environment.NewLine
                                      + "  Adds controlled vocabulary term to a reaction" + Environment.NewLine
                                      + Environment.NewLine);
            return 2;
        }


        d = libsbml.readSBML(args[1]);
        errors = d.getNumErrors();

        if (errors > 0)
        {
            Console.Write("Read Error(s):" + Environment.NewLine);
            d.printErrors();

            Console.Write("Correct the above and re-run." + Environment.NewLine);
        }
        else
        {

            n = d.getModel().getNumReactions();

            if (n <= 0)
            {
                Console.Write("Model has no reactions.\n Cannot add CV terms\n");
            }
            else
            {
                r = d.getModel().getReaction(0);

                /* check that the reaction has a metaid
       * no CVTerms will be added if there is no metaid to reference
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

        return (int)errors;
    }
}
