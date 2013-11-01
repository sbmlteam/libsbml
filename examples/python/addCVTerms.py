#!/usr/bin/env python
## 
## \file    addCVTerms.py
## \brief   adds controlled vocabulary terms to a species in a model
## \author  Sarah Keating
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 

import sys
import os.path
from libsbml import *

def main (args):
  """usage: addCVTerms <input-filename> <output-filename>
     Adds controlled vocabulary term to a species
  """
  if len(args) != 3:
    print(main.__doc__)
    sys.exit(2)

  d = readSBML(args[1]);
  errors = d.getNumErrors();
  
  if (errors > 0):
      print("Read Error(s):");
      d.printErrors();  
      print("Correct the above and re-run.");
  else:
      n = d.getModel().getNumSpecies();
 
      if (n <= 0):
          print("Model has no species.\n Cannot add CV terms\n");
      else:
          s = d.getModel().getSpecies(0);
  
          cv = CVTerm();
          cv.setQualifierType(BIOLOGICAL_QUALIFIER);
          cv.setBiologicalQualifierType(BQB_IS_VERSION_OF);
          cv.addResource("http://www.geneontology.org/#GO:0005892");
  
          cv2 = CVTerm();
          cv2.setQualifierType(BIOLOGICAL_QUALIFIER);
          cv2.setBiologicalQualifierType(BQB_IS);
          cv2.addResource("http://www.geneontology.org/#GO:0005895");
  
          cv1 =  CVTerm();
          cv1.setQualifierType(BIOLOGICAL_QUALIFIER);
          cv1.setBiologicalQualifierType(BQB_IS_VERSION_OF);
          cv1.addResource("http://www.ebi.ac.uk/interpro/#IPR002394");
  
          s.addCVTerm(cv);
          s.addCVTerm(cv2);
          s.addCVTerm(cv1);
  
          writeSBML(d, args[2]);
  
  return errors;


if __name__ == '__main__':
  main(sys.argv)  
