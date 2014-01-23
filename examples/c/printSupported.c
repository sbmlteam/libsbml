/**
 * @file    printSupported.cpp
 * @brief   Prints supported SBML Levels and Versions for the LibSBML library
 * @author  Frank Bergmann
 * $HeadUR$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <stdio.h>
#include <sbml/SBMLTypes.h>


int
main (int argc, char* argv[])
{
  int i;
  int length;
  SBMLNamespaces_t** supported = SBMLNamespaces_getSupportedNamespaces(&length);

  printf("LibSBML: %s supports: \n", getLibSBMLDottedVersion());

  for (i = 0; i < length; i++)
  {
       SBMLNamespaces_t *current = supported[i];
       printf("\tSBML Level %d Version: %d\n"
         , SBMLNamespaces_getLevel(current)
         , SBMLNamespaces_getVersion(current)
       );
  }


  printf("\n");
  printf("LibSBML is compiled against: \n");
  if (isLibSBMLCompiledWith("expat"))
    printf("\tExpat:       %s\n", getLibSBMLDependencyVersionOf("expat"));
  if (isLibSBMLCompiledWith("libxml"))
    printf("\tLibXML:      %s\n", getLibSBMLDependencyVersionOf("libxml"));
  if (isLibSBMLCompiledWith("xerces-c"))
    printf("\tXerces-C++:  %s\n", getLibSBMLDependencyVersionOf("xerces-c"));
  if (isLibSBMLCompiledWith("zlib"))
    printf("\tZlib:        %s\n", getLibSBMLDependencyVersionOf("zlib"));
  if (isLibSBMLCompiledWith("bzip"))
    printf("\tbzip2:       %s\n", getLibSBMLDependencyVersionOf("bzip"));
  
  printf("\n");

  return 0;
}


