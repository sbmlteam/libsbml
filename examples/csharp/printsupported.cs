/**
 * @file    printsupported.cs
 * @brief   Prints all SBML Levels and Versions supported by this version 
 *          of libsbml.
 * @author  Frank Bergmann
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

using System;
using libsbmlcs;

public static class PrintSupported
{
    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    private static void Main(string[] args)
    {	
        Console.WriteLine("Supported by LibSBML " + 
					libsbml.getLibSBMLDottedVersion());
		Console.WriteLine();
		
		SBMLNamespacesList supported = SBMLNamespaces.getSupportedNamespaces();
		for (uint i = 0; i < supported.getSize(); i++)
		{
			SBMLNamespaces current = supported.get(i);
			Console.WriteLine("\tSBML Level: " + current.getLevel() + 
							" Version: " + current.getVersion());				
		}
		
		Console.WriteLine();
    }
}

