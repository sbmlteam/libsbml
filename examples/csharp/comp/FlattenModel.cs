/**
 * @file    FlattenModel.cs
 * @brief   Flattens the comp code from the given SBML file.
 * @author  Sarah Keating
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */
using System;
using libsbmlcs;

namespace CompExamples
{

    public class FlattenModel
    {
        public static void Main(String[] args)
        {
            if (args.Length < 2 || args.Length > 3)
            {
                Console.WriteLine("Usage: FlattenModel [-p] input-filename output-filename");
                Console.WriteLine(" -p : list unused ports");
                Environment.Exit(2);
            }

            SBMLReader reader = new SBMLReader();
            SBMLWriter writer = new SBMLWriter();
            bool leavePorts = false;

            SBMLDocument doc;
            if (args.Length == 2)
            {
                doc = reader.readSBML(args[0]);
            }
            else
            {
                doc = reader.readSBML(args[1]);
                leavePorts = true;
            }


            if (doc.getErrorLog().getNumFailsWithSeverity(libsbml.LIBSBML_SEV_ERROR) > 0)
            {
                doc.printErrors();
            }
            else
            {
                /* create a new conversion properties structure */
                ConversionProperties props = new ConversionProperties();

                /* add an option that we want to flatten */
                props.addOption("flatten comp", true, "flatten comp");

                /* add an option to leave ports if the user has requested this */
                props.addOption("leavePorts", leavePorts, "unused ports should be listed in the flattened model");

                /* perform the conversion */
                int result = doc.convert(props);
                if (result != libsbml.LIBSBML_OPERATION_SUCCESS)
                {
                    Console.WriteLine("conversion failed ... ({0})", result);
                    doc.printErrors();
                    Environment.Exit(3);
                }

                if (args.Length == 2)
                {
                    writer.writeSBML(doc, args[1]);
                }
                else
                {
                    writer.writeSBML(doc, args[2]);
                }
            }

        }
    }
}