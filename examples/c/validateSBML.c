/**
 * @file    validateSBML.c
 * @brief   Validates an SBML file against the appropriate schema
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
#include "util.h"


int
main (int argc, char *argv[])
{
  const char *filename;

  unsigned long start, stop, size;
  unsigned int  errors = 0;

  SBMLDocument_t *d;


  if (argc != 2)
  {
    printf("Usage: validateSBML filename\n");
    return 2;
  }

  filename = argv[1];

  start = getCurrentMillis();
  d     = readSBML(filename);
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
