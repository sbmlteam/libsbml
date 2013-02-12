# 
# @file    stripPackage.R
# @brief   Strips the given package from the given SBML file.
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
#
# Usage: R --slave -f stripPackage.R --args <full path to input file> <package-to-strip> <full path to output file> 
#
#

library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 3)
{
  stop("Usage: stripPackage input-filename package-to-strip output-filename\n");
}

doc = readSBML(args[1]);

if (SBMLDocument_getNumErrors(doc) > 0) {
  SBMLDocument_printErrors(doc);
} else {
  props = ConversionProperties();
  ConversionProperties_addOption(props, "stripPackage", TRUE, "Strip SBML Level 3 package constructs from the model");
  ConversionProperties_addOption(props, "package", args[2], "Name of the SBML Level 3 package to be stripped");
  if (SBMLDocument_convert(doc, props) != 0) {
	stop("Error: conversion failed...\n");
  }
  writeSBML(doc, args[3]);
}

