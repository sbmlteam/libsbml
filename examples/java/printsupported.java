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

import org.sbml.libsbml.SBMLNamespaces;
import org.sbml.libsbml.SBMLNamespacesList;
import org.sbml.libsbml.libsbml;

public class printsupported {
	// / <summary>
	// / The main entry point for the application.
	// / </summary>
	public static void main(String[] args) {
		System.loadLibrary("sbmlj");

		System.out.println("Supported by LibSBML "
				+ libsbml.getLibSBMLDottedVersion());
		System.out.println();

		SBMLNamespacesList supported = SBMLNamespaces.getSupportedNamespaces();
		for (int i = 0; i < supported.getSize(); i++) {
			SBMLNamespaces current = supported.get(i);
			System.out.println("\tSBML Level: " + current.getLevel()
					+ " Version: " + current.getVersion());
		}

		System.out.println();
	}
}
