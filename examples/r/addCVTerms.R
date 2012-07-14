# 
# \file    addCVTerms.R
# \brief   adds controlled vocabulary terms to a species in a model
# \author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
#
# Usage: R --slave -f addCVTerms.R --args <input-filename> <output-filename>
#
#

library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2)
{
  stop("Usage: addCVTerms <input-filename> <output-filename>\nAdds controlled vocabulary term to a species\n");
}


d      <- readSBML(args[1])
errors <- SBMLDocument_getNumErrors(d)

if (errors > 0) {
  cat("Read Error(s):\n")
  SBMLDocument_printErrors(d)
  cat("Correct the above and re-run.\n")
} else {
  m <- SBMLDocument_getModel(d)
  n <- Model_getNumSpecies(m)
 
  if (n <= 0) {
    cat( "Model has no species.\n Cannot add CV terms\n")
  } else {
    s <- Model_getSpecies(m ,0)
	if (SBase_isSetMetaId(s) == FALSE)
		SBase_setMetaId(s, "__meta2501")
    cv <- CVTerm("BIOLOGICAL_QUALIFIER")
    CVTerm_setBiologicalQualifierType(cv, "BQB_IS_VERSION_OF")
    CVTerm_addResource(cv, "http://www.geneontology.org/#GO:0005892")

    cv2 <- CVTerm("BIOLOGICAL_QUALIFIER")
    CVTerm_setBiologicalQualifierType(cv2, "BQB_IS")
    CVTerm_addResource(cv2, "http://www.geneontology.org/#GO:0005895")
	
    cv1 <- CVTerm("BIOLOGICAL_QUALIFIER")
    CVTerm_setBiologicalQualifierType(cv1, "BQB_IS_VERSION_OF")
    CVTerm_addResource(cv1, "http://www.ebi.ac.uk/interpro/#IPR002394")

    SBase_addCVTerm(s, cv)
    SBase_addCVTerm(s, cv2)
    SBase_addCVTerm(s, cv1)

	
    writeSBML(d, args[2])
  }
}


