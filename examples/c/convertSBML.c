/**
 * @file    convertSBML.c
 * @brief   Converts SBML documents between levels
 * @author  Ben Bornstein
 * @author  Michael Hucka
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
  unsigned int    latestLevel   = SBMLDocument_getDefaultLevel();
  unsigned int    latestVersion = SBMLDocument_getDefaultVersion();
  unsigned int    errors;
  SBMLDocument_t *d;


  if (argc != 3)
  {
    printf("Usage: convertSBML input-filename output-filename\n");
    printf("This program will attempt to convert a model either to\n");
    printf("SBML Level %d Version %d (if the model is not already) or, if",
	   latestLevel, latestVersion);
    printf("the model is already expressed in Level %d Version %d, this\n",
	   latestLevel, latestVersion);
    printf("program will attempt to convert the model to Level 1 Version 2.\n");
    return 1;
  }

  d      = readSBML(argv[1]);
  errors = SBMLDocument_getNumErrors(d);

  if (errors > 0)
  {
    printf("Encountered the following SBML error(s):\n");
    SBMLDocument_printErrors(d, stdout);
    printf("Conversion skipped.  Please correct the problems above first.\n");
    return errors;
  }
  else
  {
    unsigned int olevel   = SBMLDocument_getLevel(d);
    unsigned int oversion = SBMLDocument_getVersion(d);
    int success;

    if (olevel < latestLevel || oversion < latestVersion)
    {
      printf("Attempting to convert model to SBML Level %d Version %d.\n",
             latestLevel, latestVersion);
      success = SBMLDocument_setLevelAndVersion(d, latestLevel, latestVersion);
    }
    else
    {
      printf("Attempting to convert model to SBML Level 1 Version 2.\n");
      success = SBMLDocument_setLevelAndVersion(d, 1, 2);
    }

    errors = SBMLDocument_getNumErrors(d);

    if (!success)
    {
      printf("Unable to perform conversion due to the following:\n");
      SBMLDocument_printErrors(d, stdout);

      printf("Conversion skipped.  Either libSBML does not (yet) have\n");
      printf("ability to convert this model, or (automatic) conversion\n");
      printf("is not possible in this case.\n");
    }
    else if (errors > 0)
    {
      printf("Information may have been lost in conversion; but a valid model ");
      printf("was produced by the conversion.\nThe following information ");
      printf("was provided:\n");
      SBMLDocument_printErrors(d, stdout);
      writeSBML(d, argv[2]);
    else
    { 	    
      printf("Conversion completed.\n");
      writeSBML(d, argv[2]);
    }
  }

  SBMLDocument_free(d);
  return errors;
}
