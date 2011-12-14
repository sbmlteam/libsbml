#!/usr/bin/env perl
# -*-Perl-*-
## 
## @file    callExternalValidator.pl
## @brief   Example that shows how to call an external program for validation
## @author  Frank T. Bergmann
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 

use File::Basename;
use LibSBML;
no strict;

if ($#ARGV < 1){
  print "Usage: callExternalValidator filename externalValidator [ tempSBMLFile outputFile [ ADDITIONAL-ARGS] ]\n";
  print "calls an external validator\n";
  exit 1;
}
  
$filename = $ARGV[0];

# read additional args
$externalValidator = $ARGV[1];

$tempSBMLFileName = $filename . "_temp.xml";
if ($#ARGV > 1){
    $tempSBMLFileName = $ARGV[2];
}
$outputFile = $filename. "_out.xml";
if ($#ARGV > 2) {
    $outputFile = $ARGV[3];
}
@additionalArgs = ();
for ($i=4; $i < $#additionalArgs; $i++) {
    push(@additionalArgs, $ARGV[i] );
}
# add the output file as additional arg
push(@additionalArgs, $outputFile );

print "Number of arguments: ", $#ARGV, "\n";
print "Name of external validator: ", $externalValidator, "\n";
print "          file to validate: ", $filename, "\n";
print "                temp file : ", $tempSBMLFileName, "\n";
print "                 out file : ", $outputFile, "\n";

# read the file name
$document = LibSBML::readSBML($filename);

# create a external validator that will write the model to 
# tempFile, then call teh externalValidator with the given number of arguments
# to produce the output file. This output file will then be parsed and its
# errors will be added to the error log.
$validator = new LibSBML::SBMLExternalValidator();
  
$validator->setProgram($externalValidator);
$validator->setSBMLFileName($tempSBMLFileName);
$validator->setOutputFileName($outputFile);
for ($i = 0; $i < $#additionalArgs; $i++) {
    $validator->addArgument($additionalArgs[$i]);
}
# this means that the external program will be called with the following arguments
# 
#    externalValidator tempSBMLFileName additionalArgs
#
# (where additionalArgs contains the output file as last argument)
#
# The output file that is generated should be an XML document following the 
# Validator XML format as described here: http://sbml.org/validator/api/#xml
#

# disable all regular checks
$document->setApplicableValidators(0);

# add a custom validator
$document->addValidator($validator);

# check consistency like before
$numErrors = $document->checkConsistency();

# print errors and warnings
$document->printErrors();

# return number of errors
exit $numErrors;
  
