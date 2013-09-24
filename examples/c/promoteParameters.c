/**
* @file    promoteParameters.c
* @brief   promotes all local to global paramters
* @author  Frank T. Bergmann
* 
* This file is part of libSBML.  Please visit http://sbml.org for more
* information about SBML, and the latest version of libSBML.
*/


#include <stdio.h>
#include <sbml/SBMLTypes.h>
#include <sbml/conversion/ConversionProperties.h>


int
  main (int argc, char *argv[])
{
  SBMLDocument_t *doc;

  if (argc != 3)
  {
    printf("Usage: promoteParameters input-filename output-filename\n");
    return 2;
  }

  doc = readSBML(argv[1]);

  if (SBMLDocument_getNumErrorsWithSeverity(doc, LIBSBML_SEV_ERROR) > 0)
  {
    SBMLDocument_printErrors(doc, stderr);
  }
  else
  {
    /* need new variables ... */
    ConversionProperties_t* props;
    ConversionOption_t* option1;

    /* create a new conversion properties structure */
    props = ConversionProperties_create();

    /* add an option that we want to convert parameters */
    option1 = ConversionOption_create("promoteLocalParameters");
    ConversionOption_setType(option1, CNV_TYPE_BOOL);
    ConversionOption_setValue(option1, "true");
    ConversionOption_setDescription(option1, "Promotes all Local Parameters to Global ones");
    ConversionProperties_addOption(props, option1);
   
    /* perform the conversion */
    if (SBMLDocument_convert(doc, props) != LIBSBML_OPERATION_SUCCESS)
    {
      printf ("conversion failed ... ");
      return 3;
    }

    /* successfully completed, write the resulting file */
    writeSBML(doc, argv[2]);
  }

  return 0;
}

