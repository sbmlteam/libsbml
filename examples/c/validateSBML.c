/**
 * \file    validateSBML.c
 * \brief   Validates an SBML file against the appropriate schema
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
#include "util.h"


int
main (int argc, char *argv[])
{
  const char *filename;

  unsigned long start, stop, size;
  unsigned int  errors = 0;

  SBMLDocument_t *d;
  SBMLReader_t   *sr;


  if (argc != 2)
  {
    printf("\n usage: validateSBML <filename>\n\n");
    return 2;
  }


  sr = SBMLReader_create();
  SBMLReader_setSchemaValidation(sr, 1);

  filename = argv[1];

  start = getCurrentMillis();
  d     = SBMLReader_readSBML(sr, filename);
  stop  = getCurrentMillis();

  errors  = SBMLDocument_getNumErrors(d);
  errors += SBMLDocument_checkConsistency(d);

  size = getFileSize(filename);

  printf( "\n" );
  printf( "        filename: %s\n" , filename     );
  printf( "       file size: %lu\n", size         );
  printf( "  read time (ms): %lu\n", stop - start );
  printf( "        error(s): %u\n" , errors       );

  if (errors > 0) SBMLDocument_printErrors(d, stdout);
  printf("\n");

  SBMLDocument_free(d);
  return errors;
}
