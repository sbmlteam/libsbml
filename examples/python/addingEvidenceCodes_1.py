#!/usr/bin/env python3
##
## \file    addingEvidenceCodes_1.py
## \brief   adds controlled vocabulary terms to a reaction in a model
## \author  Sarah Keating
##
## <!--------------------------------------------------------------------------
## This sample program is distributed under a different license than the rest
## of libSBML.  This program uses the open-source MIT license, as follows:
##
## Copyright (c) 2013-2018 by the California Institute of Technology
## (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
## and the University of Heidelberg (Germany), with support from the National
## Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
##
## Permission is hereby granted, free of charge, to any person obtaining a
## copy of this software and associated documentation files (the "Software"),
## to deal in the Software without restriction, including without limitation
## the rights to use, copy, modify, merge, publish, distribute, sublicense,
## and/or sell copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
## THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
## DEALINGS IN THE SOFTWARE.
##
## Neither the name of the California Institute of Technology (Caltech), nor
## of the European Bioinformatics Institute (EMBL-EBI), nor of the University
## of Heidelberg, nor the names of any contributors, may be used to endorse
## or promote products derived from this software without specific prior
## written permission.
## ------------------------------------------------------------------------ -->

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

  d = readSBML(args[1])
  errors = d.getNumErrors()

  if errors > 0:
    print("Read Error(s):\n")
    d.printErrors()

    print("Correct the above and re-run.\n")
  else:
    n = d.getModel().getNumReactions()

    if n <= 0:
        print("Model has no reactions.\n Cannot add CV terms\n")
    else:
        r = d.getModel().getReaction(0)

        # check that the reaction has a metaid
        # no CVTerms will be added if there is no metaid to reference
        # 
        if not r.isSetMetaId():
            r.setMetaId("metaid_0000052")

        cv1 = CVTerm(BIOLOGICAL_QUALIFIER)
        cv1.setBiologicalQualifierType(BQB_IS_DESCRIBED_BY)
        cv1.addResource("urn:miriam:obo.eco:ECO%3A0000183")

        r.addCVTerm(cv1)

        cv2 = CVTerm(BIOLOGICAL_QUALIFIER)
        cv2.setBiologicalQualifierType(BQB_IS)
        cv2.addResource("urn:miriam:kegg.reaction:R00756")
        cv2.addResource("urn:miriam:reactome:REACT_736")

        r.addCVTerm(cv2)

        writeSBML(d, args[2])
  return errors


if __name__ == '__main__':
  main(sys.argv)  
