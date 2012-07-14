# 
# @file    printSBML.R
# @brief   Prints some information about the top-level model
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
#
# Usage: R --slave -f printSBML.R --args <full path to input file>
#
library(libSBML)


args <- commandArgs(trailingOnly = TRUE)


if (length(args) != 1)
{
  stop("Usage: printSBML input-filename\n");
}


filename = args[1];
d        = readSBML(filename);
errors   = SBMLDocument_getNumErrors(d);
SBMLDocument_printErrors(d);

m = SBMLDocument_getModel(d);

level   = SBase_getLevel  (d);
version = SBase_getVersion(d);

cat("\n");
cat("File: ",filename," (Level ",level,", version ",version,")\n");

if (errors > 0) {
  stop("No model present.");  
}

cat("         ");
cat("  model id: ", ifelse(Model_isSetId(m), Model_getId(m) ,"(empty)"),"\n");

cat( "functionDefinitions: ", Model_getNumFunctionDefinitions(m) ,"\n" );
cat( "    unitDefinitions: ", Model_getNumUnitDefinitions    (m) ,"\n" );
cat( "   compartmentTypes: ", Model_getNumCompartmentTypes   (m) ,"\n" );
cat( "        specieTypes: ", Model_getNumSpeciesTypes       (m) ,"\n" );
cat( "       compartments: ", Model_getNumCompartments       (m) ,"\n" );
cat( "            species: ", Model_getNumSpecies            (m) ,"\n" );
cat( "         parameters: ", Model_getNumParameters         (m) ,"\n" );
cat( " initialAssignments: ", Model_getNumInitialAssignments (m) ,"\n" );
cat( "              rules: ", Model_getNumRules              (m) ,"\n" );
cat( "        constraints: ", Model_getNumConstraints        (m) ,"\n" );
cat( "          reactions: ", Model_getNumReactions          (m) ,"\n" );
cat( "             events: ", Model_getNumEvents             (m) ,"\n" );
cat( "\n" );

q(status=0);

