# 
# @file    convertCobraToFbc.R
# @brief   Convert COBRA L2 to L3 with FBC
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
#
# Usage: R --slave -f convertCobraToFbc.R --args <full path to input file> <full path to output file> 
#
#

library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2)
{
  stop("Usage: convertCobraToFbc input-filename output-filename\n");
}

doc = readSBML(args[1]);

if (SBMLDocument_getNumErrors(doc) > 0) {
  SBMLDocument_printErrors(doc);
} else {
  props = ConversionProperties();
  ConversionProperties_addOption(props, "convert cobra", TRUE, "Convert Cobra model");
  if (SBMLDocument_convert(doc, props) != 0) {
	stop("Error: conversion failed...\n");
  }
  writeSBML(doc, args[2]);
}

