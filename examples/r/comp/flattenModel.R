#
# @file    flattenModel.R
# @brief   Flattens the comp code from the given SBML file.
# @author  Frank Bergmann
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
#
# Usage: R --slave -f flattenModel.R --args <full path to input file> <full path to output file>
#
#
library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2)
{
  stop("Usage: flattenModel input-filename output-filename\n");
}

doc = readSBML(args[1]);

if (SBMLDocument_getNumErrors(doc) > 0) {
  SBMLDocument_printErrors(doc);
} else {
  props = ConversionProperties();
  ConversionProperties_addOption(props, "flatten comp", TRUE, "flatten comp");
  ConversionProperties_addOption(props, "leavePorts", FALSE, "Leave unused ports");
  if (SBMLDocument_convert(doc, props) != 0) {
	  SBMLDocument_printErrors(doc);
  	stop("Error: conversion failed...\n");
  }
  writeSBML(doc, args[2]);
}

