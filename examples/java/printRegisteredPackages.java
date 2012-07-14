/**
 * @file    printRegisteredPackages.cpp
 * @brief   Prints the registerd packages for this libSBML
 * @author  Frank Bergmann
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

import org.sbml.libsbml.SBMLExtensionRegistry;
import org.sbml.libsbml.libsbml;

public class printRegisteredPackages {
	public static void main(String[] args) {
		System.loadLibrary("sbmlj");
		System.out.print("This version of LibSBML: "
				+ libsbml.getLibSBMLDottedVersion() + " includes: " + "\n");

		for (int i = 0; i < SBMLExtensionRegistry.getNumRegisteredPackages(); i++) {
			System.out.print("\t"
					+ SBMLExtensionRegistry.getRegisteredPackageName(i) + "\n");
		}

		System.out.print("\n");

		System.exit(0);
	}

}
