/**
 * \file    validateSBML.c
 * \brief   Validates an SBML file against the appropriate schema
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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


#include "common/common.h"
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

  
  errors += SBMLDocument_checkConsistency(d);

  size = getFileSize(filename);

  printf( "\n" );
  printf( "        filename: %s\n" , filename     );
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
