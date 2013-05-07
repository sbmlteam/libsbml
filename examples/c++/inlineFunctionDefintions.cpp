/**
 * @file    inlineFunctionDefintions.cpp
 * @brief   Loads an SBML File and inlines all function definitions
 * @author  Michael Hucka
 * @author  Sarah Keating
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
main (int argc, char *argv[])
{
 
  if (argc != 3)
  {
    cout 
	     << endl
		 << "Usage: inlineFunctionDefintions input-filename output-filename" << endl
	     << endl
	     << "This program will attempt to inline all function definitions" << endl
	     << "contained in the source model, and write a new SBML file."
	     << endl
	     << endl;
    return 1;
  }

  const char* inputFile   = argv[1];
  const char* outputFile  = argv[2];

  // read document 
  SBMLDocument* document  = readSBML(inputFile);
  unsigned int  errors    = document->getNumErrors(LIBSBML_SEV_ERROR);

  // stop in case of errors
  if (errors > 0)
  {
    cerr << "Encountered the following SBML errors:" << endl;
    document->printErrors(cerr);
    cerr << "Conversion skipped.  Please correct the problems above first."
	 << endl;
    return errors;
  }

  // create conversion object that identifies the function definition converter
  ConversionProperties props;
  props.addOption("expandFunctionDefinitions", true,
                 "Expand all function definitions in the model");
  
  // convert
  int success = document->convert(props);

  if (success != LIBSBML_OPERATION_SUCCESS)
  {
    cerr << "Unable to perform conversion due to the following:" << endl;
    document->printErrors(cerr);
    return errors;
  }   
  else
  {
    cout << "Conversion completed." << endl;
    writeSBML(document, outputFile);
  }

  return 0;
}


