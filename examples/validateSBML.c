/**
 * Filename    : validateSBML.c
 * Description : Validates an SBML file against the appropriate schema
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-12-05
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/common.h"
#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"

#include "util.h"


int
main (int argc, char *argv[])
{
  unsigned long start, stop, size;
  unsigned int  errors = 0;

  SBMLDocument_t *d;
  SBMLReader_t   *sr;


  if (argc != 2)
  {
    printf("\n usage: validateSBML <filename>\n\n");
    return 1;
  }

  sr = SBMLReader_create();

  SBMLReader_setSchemaValidationLevel(sr, XML_SCHEMA_VALIDATION_BASIC);

  SBMLReader_setSchemaFilenameL1v1(sr, "sbml-l1v1.xsd");
  SBMLReader_setSchemaFilenameL1v2(sr, "sbml-l1v2.xsd");
  SBMLReader_setSchemaFilenameL2v1(sr, "sbml-l2v1.xsd");

  start = getCurrentMillis();
  d     = SBMLReader_readSBML(sr, argv[1]);
  stop  = getCurrentMillis();

  errors = SBMLDocument_getNumWarnings(d) + SBMLDocument_getNumErrors(d) +
           SBMLDocument_getNumFatals(d);

  size = getFileSize(argv[1]);

  printf( "\n" );
  printf( "        filename: %s\n" , argv[1]      );
  printf( "       file size: %lu\n", size         );
  printf( "  read time (ms): %lu\n", stop - start );
  printf( "        error(s): %u\n" , errors       );

  if (errors > 0)
  {
    SBMLDocument_printWarnings(d, stdout);
    SBMLDocument_printErrors  (d, stdout);
    SBMLDocument_printFatals  (d, stdout);
  }

  printf("\n");

  SBMLDocument_free(d);
  return errors;
}
