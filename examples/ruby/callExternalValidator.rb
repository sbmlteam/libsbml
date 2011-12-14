#!/usr/bin/env ruby
#
## 
## @file    callExternalValidator.py
## @brief   Example that shows how to call an external program for validation
## @author  Frank T. Bergmann
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 



require 'libSBML'

if ARGV.size <= 2
  puts "Usage: callExternalValidator filename externalValidator [ tempSBMLFile outputFile [ ADDITIONAL-ARGS] ]"
  puts "calls an external validator"  
  exit(1)
end
  
filename = ARGV[0];

# read additional args
externalValidator = ARGV[1];

tempSBMLFileName = filename + "_temp.xml";
if ARGV.size  > 2
    tempSBMLFileName = ARGV[2];
end
outputFile = filename + "_out.xml";
if ARGV.size >3
    outputFile = ARGV[3];
end
additionalArgs = []
for i in 5..ARGV.size
    additionalArgs << ARGV[i] ;
end
# add the output file as additional arg
additionalArgs << outputFile

# read the file name
document = LibSBML::readSBML(filename)

# create a external validator that will write the model to 
# tempFile, then call teh externalValidator with the given number of arguments
# to produce the output file. This output file will then be parsed and its
# errors will be added to the error log.
validator = LibSBML::SBMLExternalValidator.new

validator.setProgram(externalValidator)
validator.setSBMLFileName(tempSBMLFileName)
validator.setOutputFileName(outputFile)
additionalArgs.each { |item| validator.addArgument(item) }


# this means that the external program will be called with the following arguments
# 
#    externalValidator tempSBMLFileName additionalArgs
#
# (where additionalargs contains the output file as last argument)
#
# The output file that is generated should be an XML document following the 
# Validator XML format as described here: http:#sbml.org/validator/api/#xml
#

# disable all regular checks
document.setApplicableValidators(0)

# add a custom validator
document.addValidator(validator)

# check consistency like before
numErrors = document.checkConsistency

# print errors and warnings
document.printErrors

# return number of errors
exit(numErrors)
    
