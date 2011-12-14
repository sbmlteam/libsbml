#!/usr/bin/env ruby
#
## 
## \file    addCVTerms.py
## \brief   adds controlled vocabulary terms to a species in a model
## \author  Sarah Keating
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


require 'libSBML'


if ARGV.size != 2
  puts "usage: addCVTerms <input-filename> <output-filename>"
  puts "       Adds controlled vocabulary term to a species"
  exit(2)
end

d = LibSBML::readSBML(ARGV[0])
errors = d.getNumErrors

if (errors > 0)
  print("Read Error(s):")
  d.printErrors  
  print("Correct the above and re-run.")
  exit(errors);
end

n = d.getModel.getNumSpecies

if (n <= 0)
    print("Model has no species.\n Cannot add CV terms\n")
	exit 0;
end

s = d.getModel.getSpecies(0)

cv = LibSBML::CVTerm.new
cv.setQualifierType(LibSBML::BIOLOGICAL_QUALIFIER)
cv.setBiologicalQualifierType(LibSBML::BQB_IS_VERSION_OF)
cv.addResource("http://www.geneontology.org/#GO:0005892")

cv2 = LibSBML::CVTerm.new
cv2.setQualifierType(LibSBML::BIOLOGICAL_QUALIFIER)
cv2.setBiologicalQualifierType(LibSBML::BQB_IS)
cv2.addResource("http://www.geneontology.org/#GO:0005895")

cv1 = LibSBML::CVTerm.new
cv1.setQualifierType(LibSBML::BIOLOGICAL_QUALIFIER)
cv1.setBiologicalQualifierType(LibSBML::BQB_IS_VERSION_OF)
cv1.addResource("http://www.ebi.ac.uk/interpro/#IPR002394")

s.addCVTerm(cv)
s.addCVTerm(cv2)
s.addCVTerm(cv1)

LibSBML::writeSBML(d, ARGV[1])

exit(errors);

