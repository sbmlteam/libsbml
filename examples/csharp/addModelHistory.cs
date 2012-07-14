/**
 * \file    addModelHistory.cpp
 * \brief   adds Model History to a model
 * \author  Sarah Keating
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using libsbmlcs;

public class AddModelHistory
{
    private static void printStatus(string message, long status)
    {
        string statusString;
        switch (status)
        {
            case libsbml.LIBSBML_OPERATION_SUCCESS:
                statusString = "succeeded";
                break;
            case libsbml.LIBSBML_INVALID_OBJECT:
                statusString = "invalid object";
                break;
            case libsbml.LIBSBML_OPERATION_FAILED:
                statusString = "operation failed";
                break;
            default:
                statusString = "unknown";
                break;
        }

        Console.Write(message + statusString + Environment.NewLine);

    }

    public static int Main(string[] args)
    {


        SBMLDocument d;
        int errors;

        if (args.Length != 2)
        {
            Console.Write(Environment.NewLine
                                      + "  usage: addModelHistory <input-filename> <output-filename>" + Environment.NewLine
                                      + Environment.NewLine);
            return 2;
        }


        d = libsbml.readSBML(args[0]);
        errors = (int)d.getNumErrors();

        if (errors > 0)
        {
            Console.Write("Read Error(s):" + Environment.NewLine);
            d.printErrors();

            Console.Write("Correct the above and re-run." + Environment.NewLine);
        }
        else
        {
            ModelHistory h = new ModelHistory();

            ModelCreator c = new ModelCreator();
            c.setFamilyName("Keating");
            c.setGivenName("Sarah");
            c.setEmail("sbml-team@caltech.edu");
            c.setOrganization("University of Hertfordshire");

            long status = h.addCreator(c);
            printStatus("Status for addCreator: ", status);


            Date date = new Date("1999-11-13T06:54:32");
            Date date2 = new Date("2007-11-30T06:54:00-02:00");

            status = h.setCreatedDate(date);
            printStatus("Set created date:      ", status);

            status = h.setModifiedDate(date2);
            printStatus("Set modified date:     ", status);

            status = d.getModel().setModelHistory(h);
            printStatus("Set model history:     ", status);


            libsbml.writeSBML(d, args[1]);
        }

        return errors;
    }

}
