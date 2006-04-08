/**
 * \file    printSBML.c
 * \brief   Prints some information about the top-level model
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
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
    printf("\n  usage: printSBML <filename>\n\n");
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
