/**
* @file    stripPackage.c
* @brief   Strips the given package from the given SBML file.
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

  if (argc != 4)
  {
    printf("Usage: stripPackage input-filename package-to-strip output-filename\n");
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
    ConversionOption_t* option2;

    /* create a new conversion properties structure */
    props = ConversionProperties_create();

    /* add an option that we want to strip a given package */
    option1 = ConversionOption_create("stripPackage");
    ConversionOption_setType(option1, CNV_TYPE_BOOL);
    ConversionOption_setValue(option1, "true");
    ConversionOption_setDescription(option1, "Strip SBML Level 3 package constructs from the model");
    ConversionProperties_addOption(props, option1);

    /* add an option with the package we want to remove */
    option2 = ConversionOption_create("package");
    ConversionOption_setType(option2, CNV_TYPE_STRING);
    ConversionOption_setValue(option2, argv[2]);
    ConversionOption_setDescription(option2, "Name of the SBML Level 3 package to be stripped");
    ConversionProperties_addOption(props, option2);

    /* perform the conversion */
    if (SBMLDocument_convert(doc, props) != LIBSBML_OPERATION_SUCCESS)
    {
      printf ("conversion failed ... ");
      return 3;
    }

    /* successfully completed, write the resulting file */
    writeSBML(doc, argv[3]);
  }

  return 0;
}

