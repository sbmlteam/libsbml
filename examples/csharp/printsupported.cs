/**
 * @file    printsupported.cs
 * @brief   Prints all SBML Levels and Versions supported by this version 
 *          of libsbml.
 * @author  Frank Bergmann
 *
 * $Id$
 * $HeadURL$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

using System;
using libsbml;

public static class PrintSupported
{
    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    private static void Main(string[] args)
    {	
        Console.WriteLine("Supported by LibSBML " + 
					libsbml.libsbml.getLibSBMLDottedVersion());
		Console.WriteLine();
		
		var supported = SBMLNamespaces.getSupportedNamespaces();
		for (uint i = 0; i < supported.getSize(); i++)
		{
			var current = supported.get(i);
			Console.WriteLine("\tSBML Level: " + current.getLevel() + 
							" Version: " + current.getVersion());				
		}
		
		Console.WriteLine();
    }
}
