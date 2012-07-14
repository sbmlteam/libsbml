## 
## \file    addingEvidenceCodes_1.py
## \brief   adds controlled vocabulary terms to a reaction in a model
## \author  Sarah Keating
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


import sys
import os.path
from libsbml import *

def main (args):
  """usage: addingEvidenceCodes_1 <input-filename> <output-filename>
     Adds controlled vocabulary term to a reaction
  """
  if len(args) != 3:
    print(main.__doc__)
    sys.exit(2)

  d = readSBML(args[1]);
  errors = d.getNumErrors();
  
  if (errors > 0):
    print("Read Error(s):\n");
    d.printErrors();
    
    print("Correct the above and re-run.\n");
  else:
    n = d.getModel().getNumReactions();
    
    if (n <= 0):
        print("Model has no reactions.\n Cannot add CV terms\n");
    else:
        r = d.getModel().getReaction(0);
    
        # check that the reaction has a metaid
        # no CVTerms will be added if there is no metaid to reference
        # 
        if ( not r.isSetMetaId()):
            r.setMetaId("metaid_0000052");
    
        cv1 = CVTerm(BIOLOGICAL_QUALIFIER);
        cv1.setBiologicalQualifierType(BQB_IS_DESCRIBED_BY);
        cv1.addResource("urn:miriam:obo.eco:ECO%3A0000183");
    
        r.addCVTerm(cv1);
    
        cv2 = CVTerm(BIOLOGICAL_QUALIFIER);
        cv2.setBiologicalQualifierType(BQB_IS);
        cv2.addResource("urn:miriam:kegg.reaction:R00756");
        cv2.addResource("urn:miriam:reactome:REACT_736");
    
        r.addCVTerm(cv2);
    
        writeSBML(d, args[2]);
  return errors;


if __name__ == '__main__':
  main(sys.argv)  
