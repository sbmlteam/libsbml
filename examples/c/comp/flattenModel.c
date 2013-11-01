/**
* @file    flattenModel.c
* @brief   Flattens the comp code from the given SBML file.
* @author  Sarah Keating
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
  int leavePorts = 0;

  if (argc < 3 || argc > 4)
  {
    printf("Usage: flattenModel [-p] input-filename output-filename\n-p:list unused ports\n");
    return 2;
  }

  if (argc == 3)
  {
    doc = readSBML(argv[1]);
  }
  else
  {
    if (strcmp(argv[1], "-p") != 0)
    {
      printf("Usage: flattenModel [-p] input-filename output-filename\n-p:list unused ports\n");
      return 2;
    }
    doc = readSBML(argv[2]);
    leavePorts = 1;
  }


  /* if there are errors do not try to flatten */
  if (SBMLDocument_getNumErrorsWithSeverity(doc, LIBSBML_SEV_ERROR) > 0)
  {
    SBMLDocument_printErrors(doc, stderr);
  }
  else
  {
    /* need new variables ... */
    ConversionProperties_t* props;
    ConversionOption_t* option1;
    ConversionOption_t* option2;

    /* create a new conversion properties structure */
    props = ConversionProperties_create();

    /* add an option that we want to flatten comp */
    option1 = ConversionOption_create("flatten comp");
    ConversionOption_setType(option1, CNV_TYPE_BOOL);
    ConversionOption_setValue(option1, "true");
    ConversionOption_setDescription(option1, "flatten comp");
    ConversionProperties_addOption(props, option1);

    /* add an option to leave ports if the user has requested this */
    if (leavePorts == 1)
    {
      option2 = ConversionOption_create("leavePorts");
      ConversionOption_setType(option2, CNV_TYPE_BOOL);
      ConversionOption_setValue(option2, "true");
      ConversionOption_setDescription(option2, "unused ports should be listed in the flattened model");
      ConversionProperties_addOption(props, option2);
    }

    /* perform the conversion */
    if (SBMLDocument_convert(doc, props) != LIBSBML_OPERATION_SUCCESS)
    {
      printf ("conversion failed ... ");
	  SBMLDocument_printErrors(doc, stderr);
      return 3;
    }

    /* successfully completed, write the resulting file */
    if (argc == 4)
    {
      writeSBML(doc, argv[3]);
    }
    else
    {
      writeSBML(doc, argv[2]);
    }
  }

  return 0;
}

