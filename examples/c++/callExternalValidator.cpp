/**
 * @file    callExternalValidator.cpp
 * @brief   Example that shows how to call an external program for validation
 * @author  Frank T. Bergmann
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */
 
#include <iostream>
#include <string>
#include <vector>
#include <sbml/SBMLTypes.h>

#include <sbml/validator/SBMLExternalValidator.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

int
main (int argc, char *argv[])
{
  if (argc < 3)
  {
    cout << endl << "Usage: callExternalValidator filename externalValidator [ tempSBMLFile outputFile [ ADDITIONAL-ARGS] ]" << endl << endl;
    return 1;
  }

  string filename   = argv[1];  

  // read additional args
  string externalValidator = argv[2];

  string tempSBMLFileName = filename + "_temp.xml";
  if (argc > 3)
    tempSBMLFileName = argv[3];

  string outputFile = filename + "_out.xml";
  if (argc > 4)
    outputFile = argv[4];

  vector<string> additionalArgs;
  for (int i = 5; i < argc; i++)
    additionalArgs.push_back(argv[i]);

  // add the output file as additional arg
  additionalArgs.push_back(outputFile);

   // read the file name
  SBMLDocument* document = readSBML(filename.c_str());

  // create a new external validator that will write the model to 
  // tempFile, then call teh externalValidator with the given number of arguments
  // to produce the output file. This output file will then be parsed and its
  // errors will be added to the error log.
  SBMLExternalValidator validator;

  validator.setProgram(externalValidator);
  validator.setSBMLFileName(tempSBMLFileName);
  validator.setOutputFileName(outputFile);
  validator.setArguments(additionalArgs);

  // this means that the external program will be called with the following arguments
  // 
  //    externalValidator tempSBMLFileName additionalArgs
  //
  // (where additionalargs contains the output file as last argument)
  //
  // The output file that is generated should be an XML document following the 
  // Validator XML format as described here: http://sbml.org/validator/api/#xml
  //

  // disable all regular checks
  document->setApplicableValidators(0);

  // add a custom validator
  document->addValidator(&validator);

  // check consistency like before
  int numErrors = document->checkConsistency();

  // print errors and warnings
  document->printErrors();

  // return number of errors
  return  numErrors;


}
