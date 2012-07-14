/**
 * @file    printRegisteredPackages.cpp
 * @brief   Prints the registerd packages for this libSBML
 * @author  Frank Bergmann
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <stdio.h>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>

int
main (int argc, char* argv[])
{
  unsigned int i;
  List_t* list;

  printf ("This version of LibSBML: %s includes: \n", getLibSBMLDottedVersion());

  list = SBMLExtensionRegistry_getRegisteredPackages();
  for (i = 0; i < List_size(list); i++)
  {
      printf("\t%s\n", (char*)List_get(list, i));      
  }
    
  printf("\n");

  return 0;
}


