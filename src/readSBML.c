/**
 * Filename    : readSBML.c
 * Description : A small example program that uses libsbml
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-12-05
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include <stdio.h>
#include <sys/timeb.h>

#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"


/**
 * Function Prototypes
 */
unsigned long getCurrentMillis (void);


int
main (int argc, char *argv[])
{
  SBMLDocument_t *d;
  Model_t *m;

  unsigned long start, stop;


  if (argc != 2)
  {
    printf("usage: readSBML <filename>\n");
    return 1;
  }

#ifdef TRACE_MEMORY
  MemTrace_init();
#endif

  start = getCurrentMillis();
  d     = readSBML(argv[1]);
  stop  = getCurrentMillis();

  m = d->model;

  printf( "File: %s\n", argv[1]);
  printf( "       model name: %s\n",  m->name );
  printf( "  unitDefinitions: %d\n",  Model_getNumUnitDefinitions(m) );
  printf( "     compartments: %d\n",  Model_getNumCompartments(m)    );
  printf( "          species: %d\n",  Model_getNumSpecies(m)         );
  printf( "       parameters: %d\n",  Model_getNumParameters(m)      );
  printf( "        reactions: %d\n",  Model_getNumReactions(m)       );
  printf( "            rules: %d\n",  Model_getNumRules(m)           );
  printf( "\n");

  SBMLDocument_printWarnings(d, stdout);
  SBMLDocument_printErrors  (d, stdout);
  SBMLDocument_printFatals  (d, stdout);

  printf( "Total Read Time (ms): %lu\n", stop - start);

#ifdef TRACE_MEMORY
  printf( "         Total Bytes: %lu\n", MemTrace_getMaxBytes());
#endif

  SBMLDocument_free(d);
  return 0;
}


unsigned long
getCurrentMillis (void)
{
  struct timeb t;


  ftime(&t);
  return (unsigned long) (t.time * 1000 + t.millitm);
}
