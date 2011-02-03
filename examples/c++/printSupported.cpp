/**
 * @file    printSupported.cpp
 * @brief   Prints supported SBML Levels and Versions for the LibSBML library
 * @author  Frank Bergmann
 *
 * $Id$
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
  const vector<const SBMLNamespaces*> supported = 
    SBMLNamespaces::getSupportedNamespaces();

  cout << "LibSBML: " << getLibSBMLDottedVersion() << " supports: " << endl;

  for (vector<const SBMLNamespaces*>::const_iterator 
    it = supported.begin(); it != supported.end(); it++)

       cout << "\tSBML Level " << (*it)->getLevel() << " Version: " << (*it)->getVersion() << endl;
    
  cout << endl;

  return 0;
}

