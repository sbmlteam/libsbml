/**
 * @file    printSBML.c
 * @brief   Prints some information about the top-level model
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <stdio.h>
#include <sbml/SBMLTypes.h>


int
main (int argc, char *argv[])
{
  const char *filename;

  SBMLDocument_t *d;
  Model_t        *m;

  unsigned int level, version;


  if (argc != 2)
  {
    printf("Usage: printSBML filename\n");
    return 2;
  }


  filename = argv[1];
  d        = readSBML(filename);

  SBMLDocument_printErrors(d, stdout);

  m = SBMLDocument_getModel(d);

  level   = SBMLDocument_getLevel  (d);
  version = SBMLDocument_getVersion(d);

  printf("\n");
  printf("File: %s (Level %u, version %u)\n", filename, level, version);

  if (m == NULL)
  {
    printf("No model present.");
    return 1;
  }

  printf("         ");
  printf("  model id: %s\n",  Model_isSetId(m) ? Model_getId(m) : "(empty)");

  printf( "functionDefinitions: %d\n",  Model_getNumFunctionDefinitions(m) );
  printf( "    unitDefinitions: %d\n",  Model_getNumUnitDefinitions    (m) );
  printf( "   compartmentTypes: %d\n",  Model_getNumCompartmentTypes   (m) );
  printf( "        specieTypes: %d\n",  Model_getNumSpeciesTypes       (m) );
  printf( "       compartments: %d\n",  Model_getNumCompartments       (m) );
  printf( "            species: %d\n",  Model_getNumSpecies            (m) );
  printf( "         parameters: %d\n",  Model_getNumParameters         (m) );
  printf( " initialAssignments: %d\n",  Model_getNumInitialAssignments (m) );
  printf( "              rules: %d\n",  Model_getNumRules              (m) );
  printf( "        constraints: %d\n",  Model_getNumConstraints        (m) );
  printf( "          reactions: %d\n",  Model_getNumReactions          (m) );
  printf( "             events: %d\n",  Model_getNumEvents             (m) );
  printf( "\n" );

  SBMLDocument_free(d);
  return 0;
}
