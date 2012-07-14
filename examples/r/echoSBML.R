# 
# @file    echoSBML.R
# @brief   Echos (and pretty prints) an SBML model.
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
#
# Usage: R --slave -f echoSBML.R --args <full path to input file> <full path to output file> 
#
#

library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2)
{
  stop("Usage: echoSBML input-filename output-filename\n");
}

doc = readSBML(args[1]);

if (SBMLDocument_getNumErrors(doc) > 0) {
  SBMLDocument_printErrors(doc);
} else {
  writeSBML(doc, args[2]);
}

