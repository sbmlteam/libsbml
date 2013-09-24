# 
# @file    promoteParameters.R
# @brief   promotes all local to global paramters
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
#
# Usage: R --slave -f promoteParameters.R --args <full path to input file> <full path to output file> 
#
#

library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2)
{
  stop("Usage: promoteParameters input-filename output-filename\n");
}

doc = readSBML(args[1]);

if (SBMLDocument_getNumErrors(doc) > 0) {
  SBMLDocument_printErrors(doc);
} else {
  props = ConversionProperties();
  ConversionProperties_addOption(props, "promoteLocalParameters", TRUE, "Promotes all Local Parameters to Global ones");
  if (SBMLDocument_convert(doc, props) != 0) {
	stop("Error: conversion failed...\n");
  }
  writeSBML(doc, args[2]);
}

