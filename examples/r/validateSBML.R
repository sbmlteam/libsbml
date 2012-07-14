# 
# @file    validateSBML.R
# @brief   Validates an SBML file against the appropriate schema
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
#
# Usage: R --slave -f validateSBML.R --args <full path to sbml file> 
#
#

library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1)
{
  stop("Usage: validateSBML filename\n")
}
filename <- args[1]
d <- readSBML(filename)

errors  <- SBMLDocument_getNumErrors(d) + SBMLDocument_checkConsistency(d)

size <- file.info(filename)[["size"]]

cat( "        filename: ", filename, "\n" )
cat( "       file size: ", size    , "\n" )
cat( "        error(s): ", errors  , "\n" )

if (errors > 0) SBMLDocument_printErrors(d)
