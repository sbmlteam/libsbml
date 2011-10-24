/**
 * @file    echoSBML.c
 * @brief   Echos (and pretty prints) an SBML model.
 * @author  Ben Bornstein
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <stdio.h>
#include <sbml/SBMLTypes.h>


int
main (int argc, char *argv[])
{
  SBMLDocument_t *doc;

  if (argc != 3)
  {
    printf("Usage: echoSBML input-filename output-filename\n");
    return 2;
  }

  doc = readSBML(argv[1]);

  if (SBMLDocument_getNumErrors(doc) > 0)
  {
    SBMLDocument_printErrors(doc, stderr);
  }
  else
  {
    writeSBML(doc, argv[2]);
  }

  return 0;
}

