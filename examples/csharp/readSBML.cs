/**
 * @file    readSBML.cpp
 * @brief   Similar to validateSBML, but without the validation
 * @author  Sarah Keating
 * @author  Ben Bornstein
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using libsbmlcs;

internal class ReadSBML
{
    public static int Main(string[] args)
    {
        if (args.Length != 1)
        {
            Console.WriteLine("Usage: readSBML filename");
            return 1;
        }

        string filename = args[0];
        long current = DateTime.Now.Ticks;
        SBMLDocument document = libsbml.readSBML(filename);

        int errors = (int)document.getNumErrors();

        Console.WriteLine();
        Console.WriteLine("            filename: " + filename);
        Console.WriteLine("           file size: " + new FileInfo(filename).Length);
        Console.WriteLine("      read time (ms): " + (DateTime.Now.Ticks - current));
        Console.WriteLine(" validation error(s): " + errors);
        Console.WriteLine();

        document.printErrors();

        return errors;
    }

}
