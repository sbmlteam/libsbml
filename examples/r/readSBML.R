# 
# @file    readSBML.R
# @brief   Similar to validateSBML, but without the validation
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
#
# Usage: R --slave -f readSBML.R --args <full path to sbml file> 
#
#

library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1)
{
  stop("Usage: readSBML filename\n")
}
filename <- args[1]
d <- readSBML(filename)

errors  <- SBMLDocument_getNumErrors(d)

size <- file.info(filename)[["size"]]

cat( "        filename: ", filename, "\n" )
cat( "       file size: ", size    , "\n" )
cat( "        error(s): ", errors  , "\n" )

if (errors > 0) SBMLDocument_printErrors(d)
