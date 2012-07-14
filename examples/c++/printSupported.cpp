/**
 * @file    printSupported.cpp
 * @brief   Prints supported SBML Levels and Versions for the LibSBML library
 * @author  Frank Bergmann
 * $HeadUR$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>
#include <vector>
#include <string>
#include <sbml/SBMLTypes.h>


using namespace std;
LIBSBML_CPP_NAMESPACE_USE

int
main (int argc, char* argv[])
{
  const List* supported = 
    SBMLNamespaces::getSupportedNamespaces();

  cout << "LibSBML: " << getLibSBMLDottedVersion() << " supports: " << endl;

  for (unsigned int i = 0; i < supported->getSize(); i++)
  {
       const SBMLNamespaces *current = (const SBMLNamespaces *)supported->get(i);
       cout << "\tSBML Level " << current->getLevel() << " Version: " << current->getVersion() << endl;
  }
    
  cout << endl;

  return 0;
}


