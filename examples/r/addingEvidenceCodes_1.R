# 
# \file    addingEvidenceCodes_1.R
# \brief   adds controlled vocabulary terms to a reaction in a model
# \author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
# 
#
# Usage: R --slave -f addingEvidenceCodes_1.R --args <input-filename> <output-filename>
#
#

library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2) {
  stop(
         "  usage: addingEvidenceCodes_1 <input-filename> <output-filename>\n  Adds controlled vocabulary term to a reaction\n"        
      );
}

d      = readSBML(args[1]);
errors = SBMLDocument_getNumErrors(d);

if (errors > 0) {
  cat("Read Error(s):\n");
  SBMLDocument_printErrors(d);	 
  cat("Correct the above and re-run.\n");
} else {

  m = SBMLDocument_getModel(d);
  n =  Model_getNumReactions(m);
  
  if (n <= 0) {
    cat( "Model has no reactions.\n Cannot add CV terms\n");
  } else {      
    r = Model_getReaction(m, 0);

    # check that the reaction has a metaid
    # no CVTerms will be added if there is no metaid to reference
    # 
    if (SBase_isSetMetaId(r) == FALSE)
      SBase_setMetaId(r, "metaid_0000052");

    cv1 = CVTerm("BIOLOGICAL_QUALIFIER");
    CVTerm_setBiologicalQualifierType(cv1, "BQB_IS_DESCRIBED_BY");
    CVTerm_addResource(cv1, "urn:miriam:obo.eco:ECO%3A0000183");

    SBase_addCVTerm(r, cv1);

    cv2 = CVTerm("BIOLOGICAL_QUALIFIER");
    CVTerm_setBiologicalQualifierType(cv2, "BQB_IS");
    CVTerm_addResource(cv2, "urn:miriam:kegg.reaction:R00756");
    CVTerm_addResource(cv2, "urn:miriam:reactome:REACT_736");
    
    SBase_addCVTerm(r, cv2);

    writeSBML(d, args[2]);
  }
}

q(status=errors);
