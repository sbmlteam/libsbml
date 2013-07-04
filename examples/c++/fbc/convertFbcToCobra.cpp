/**
* @file    convertFbcToCobra.cpp
* @brief   Convert COBRA L2 to L3 with FBC
* @author  Frank T. Bergmann
* 
* This file is part of libSBML.  Please visit http://sbml.org for more
* information about SBML, and the latest version of libSBML.
*/


#include <iostream>
#include <sbml/SBMLTypes.h>
#include <sbml/conversion/ConversionProperties.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

  int
  main (int argc, char* argv[])
{
  if (argc != 3)
  {
    cout << endl << "Usage: convertFbcToCobra input-filename output-filename"
      << endl << endl;
    return 2;
  }

  SBMLDocument *d = readSBML(argv[1]);
  if ( d->getNumErrors(LIBSBML_SEV_ERROR) > 0)
  {
    d->printErrors();
  }
  else
  {
    /* create a new conversion properties structure */
    ConversionProperties props;

    /* add an option that we want to strip a given package */
    props.addOption("convert fbc to cobra", true, "Convert FBC model to Cobra model");


    /* perform the conversion */
	int result = d->convert(props);
    if (result != LIBSBML_OPERATION_SUCCESS)
    {
      cout<< "conversion failed ... " << endl;
      return 3;
    }

    writeSBML(d, argv[2]);
  }

  return 0;
}
