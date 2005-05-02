/**
 * \file    printModel.c
 * \brief   Prints some information about the top-level model
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002-2004 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *
 *     The SBML Team
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include <stdio.h>

#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"


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
    return 1;
  }


  filename = argv[1];
  d        = readSBML(filename);

  SBMLDocument_printWarnings(d, stdout);
  SBMLDocument_printErrors  (d, stdout);
  SBMLDocument_printFatals  (d, stdout);

  m = SBMLDocument_getModel(d);
  if (m == NULL) return 2;

  level   = SBMLDocument_getLevel  (d);
  version = SBMLDocument_getVersion(d);

  printf("\n");
  printf("File: %s (Level %u, version %u)\n", filename, level, version);

  printf( "         ");

  if (level == 1)
  {
    printf("model name: %s\n",  Model_getName(m));
  }
  else
  {
    printf("  model id: %s\n",  Model_isSetId(m) ? Model_getId(m) : "(empty)");
  }

  printf( "functionDefinitions: %d\n",  Model_getNumFunctionDefinitions(m) );
  printf( "    unitDefinitions: %d\n",  Model_getNumUnitDefinitions    (m) );
  printf( "       compartments: %d\n",  Model_getNumCompartments       (m) );
  printf( "            species: %d\n",  Model_getNumSpecies            (m) );
  printf( "         parameters: %d\n",  Model_getNumParameters         (m) );
  printf( "          reactions: %d\n",  Model_getNumReactions          (m) );
  printf( "              rules: %d\n",  Model_getNumRules              (m) );
  printf( "             events: %d\n",  Model_getNumEvents             (m) );
  printf( "\n" );

  SBMLDocument_free(d);
  return 0;
}
