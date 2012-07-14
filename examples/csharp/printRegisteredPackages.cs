/**
 * @file    printRegisteredPackages.cpp
 * @brief   Prints the registerd packages for this libSBML
 * @author  Frank Bergmann
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using libsbmlcs;

public class PrintRegisteredPackages
{
    public static int Main(string[] args)
    {
        Console.Write("This version of LibSBML: " + libsbml.getLibSBMLDottedVersion() + " includes: " + Environment.NewLine);

        for (int i = 0;
        i < SBMLExtensionRegistry.getNumRegisteredPackages();
        i++)
        {
            Console.Write("\t" + SBMLExtensionRegistry.getRegisteredPackageName(i) + Environment.NewLine);
        }

        Console.Write(Environment.NewLine);

        return 0;
    }


}
