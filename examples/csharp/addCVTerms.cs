/**
 * \file    addCVTerms.cpp
 * \brief   adds controlled vocabulary terms to a species in a model
 * \author  Sarah Keating
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

using System;
using libsbmlcs;

public class AddCVTerms
{
    public static int Main(string[] args)
    {

        SBMLDocument d;
        long errors, n;
        Species s;

        if (args.Length != 2)
        {
            Console.WriteLine("  usage: addCVTerms <input-filename> <output-filename>");
            Console.WriteLine("  Adds controlled vocabulary term to a species");
            return 2;
        }


        d = libsbml.readSBML(args[0]);
        errors = d.getNumErrors();

        if (errors > 0)
        {
            Console.WriteLine("Read Error(s):");
            d.printErrors();

            Console.WriteLine("Correct the above and re-run.");
        }
        else
        {

            n = d.getModel().getNumSpecies();

            if (n <= 0)
            {
                Console.WriteLine("Model has no species.\n Cannot add CV terms\n");
            }
            else
            {
                s = d.getModel().getSpecies(0);

                CVTerm cv = new CVTerm();
                cv.setQualifierType(libsbml.BIOLOGICAL_QUALIFIER);
                cv.setBiologicalQualifierType(libsbml.BQB_IS_VERSION_OF);
                cv.addResource("http://www.geneontology.org/#GO:0005892");

                CVTerm cv2 = new CVTerm();
                cv2.setQualifierType(libsbml.BIOLOGICAL_QUALIFIER);
                cv2.setBiologicalQualifierType(libsbml.BQB_IS);
                cv2.addResource("http://www.geneontology.org/#GO:0005895");

                CVTerm cv1 = new CVTerm();
                cv1.setQualifierType(libsbml.BIOLOGICAL_QUALIFIER);
                cv1.setBiologicalQualifierType(libsbml.BQB_IS_VERSION_OF);
                cv1.addResource("http://www.ebi.ac.uk/interpro/#IPR002394");

                s.addCVTerm(cv);
                s.addCVTerm(cv2);
                s.addCVTerm(cv1);

                libsbml.writeSBML(d, args[1]);
            }
        }

        return (int)errors;
    }

}
