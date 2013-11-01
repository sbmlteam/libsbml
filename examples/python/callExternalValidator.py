#!/usr/bin/env python
## 
## @file    callExternalValidator.py
## @brief   Example that shows how to call an external program for validation
## @author  Frank T. Bergmann
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


import sys
import os.path
from libsbml import *

def main (args):
  """Usage: callExternalValidator filename externalValidator [ tempSBMLFile outputFile [ ADDITIONAL-ARGS] ]
     calls an external validator
  """
  if len(args) < 3:
    print(main.__doc__)
    sys.exit(1)
  
  filename = args[1];
  
  # read additional args
  externalValidator = args[2];
  
  tempSBMLFileName = filename + "_temp.xml";
  if (len(args) > 3):
      tempSBMLFileName = args[3];
  
  outputFile = filename + "_out.xml";
  if (len(args) > 4):
      outputFile = args[4];
  
  additionalArgs = []
  for i in range (5, len(args)):
      additionalArgs = additionalArgs + [ args[i] ] ;
  
  # add the output file as additional arg
  additionalArgs = additionalArgs + [outputFile];
  
  # read the file name
  document = readSBML(filename);
  
  # create a external validator that will write the model to 
  # tempFile, then call teh externalValidator with the given number of arguments
  # to produce the output file. This output file will then be parsed and its
  # errors will be added to the error log.
  validator = SBMLExternalValidator();
  
  validator.setProgram(externalValidator);
  validator.setSBMLFileName(tempSBMLFileName);
  validator.setOutputFileName(outputFile);
  for item in additionalArgs:
      validator.addArgument(item);
  
  # this means that the external program will be called with the following arguments
  # 
  #    externalValidator tempSBMLFileName additionalArgs
  #
  # (where additionalargs contains the output file as last argument)
  #
  # The output file that is generated should be an XML document following the 
  # Validator XML format as described here: http://sbml.org/validator/api/#xml
  #
  
  # disable all regular checks
  document.setApplicableValidators(0);
  
  # add a custom validator
  document.addValidator(validator);
  
  # check consistency like before
  numErrors = document.checkConsistency();
  
  # print errors and warnings
  document.printErrors();
  
  # return number of errors
  return numErrors;
  
if __name__ == '__main__':
  main(sys.argv)  
