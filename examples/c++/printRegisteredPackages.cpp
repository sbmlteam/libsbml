/**
 * @file    printRegisteredPackages.cpp
 * @brief   Prints the registerd packages for this libSBML
 * @author  Frank Bergmann
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>
#include <vector>
#include <string>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

int
main (int argc, char* argv[])
{
  cout << "This version of LibSBML: " << getLibSBMLDottedVersion() << " includes: " << endl;

  for (unsigned int i = 0; i <  SBMLExtensionRegistry::getNumRegisteredPackages(); i++)
  {
       cout << "\t" << SBMLExtensionRegistry::getRegisteredPackageName(i) << endl;
  }
    
  cout << endl;

  return 0;
}


