#!/usr/bin/env ruby
#
## 
## \file    appendAnnotation.py
## \brief   adds annotation strings to a model and a species
## \author  Akiya Jouraku
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 



require 'libSBML'

if ARGV.size < 2
  puts "Usage:appendAnnotation <input-filename> <output-filename>"
  puts "Adds annotatons"  
  exit(1)
end

d = LibSBML::readSBML(ARGV[0]);
errors = d.getNumErrors();

if errors > 0
    puts "Read Error(s):\n"
    d.printErrors

    puts "Correct the above and re-run.\n"
	exit(errors);
end

model_history_annotation = <<-eos
  <annotation>
  <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:vCard="http://www.w3.org/2001/vcard-rdf/3.0#" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/" xmlns:bqmodel="http://biomodels.net/model-qualifiers/">
    <rdf:Description rdf:about="#">
      <dc:creator rdf:parseType="Resource">
        <rdf:Bag>
          <rdf:li rdf:parseType="Resource">
            <vCard:N rdf:parseType="Resource">
              <vCard:Family>Keating</vCard:Family>
              <vCard:Given>Sarah</vCard:Given>
            </vCard:N>
            <vCard:EMAIL>sbml-team@caltech.edu</vCard:EMAIL>
            <vCard:ORG>
              <vCard:Orgname>University of Hertfordshire</vCard:Orgname>
            </vCard:ORG>
          </rdf:li>
        </rdf:Bag>
      </dc:creator>
      <dcterms:created rdf:parseType="Resource">
        <dcterms:W3CDTF>1999-11-13T06:54:32Z</dcterms:W3CDTF>
      </dcterms:created>
      <dcterms:modified rdf:parseType="Resource">
        <dcterms:W3CDTF>2007-11-31T06:54:00-02:00</dcterms:W3CDTF>
      </dcterms:modified>
    </rdf:Description>
  </rdf:RDF>
</annotation>
eos

d.getModel().appendAnnotation(model_history_annotation);

# 
# The above code can be replaced by the following code.
# 
# 
# ModelHistory * h = ModelHistory();
# 
# ModelCreator *c = ModelCreator();
# c.setFamilyName("Keating");
# c.setGivenName("Sarah");
# c.setEmail("sbml-team@caltech.edu");
# c.setOrganisation("University of Hertfordshire");
# 
# h.addCreator(c);
# 
# Date * date = Date("1999-11-13T06:54:32");
# Date * date2 = Date("2007-11-31T06:54:00-02:00");
# 
# h.setCreatedDate(date);
# h.setModifiedDate(date2);
# 
# d.getModel().setModelHistory(h);
# 
# 
# 


n = d.getModel().getNumSpecies

if n <= 0
  print("Model has no species.\n Cannot add CV terms\n")
  exit(0);
end

s = d.getModel().getSpecies(0);

cvterms_annotation = <<-eos
 <annotation>		  
   <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:vCard="http://www.w3.org/2001/vcard-rdf/3.0#" xmlns:bqbiol="http://biomodels.net/biology-qualifiers/" xmlns:bqmodel="http://biomodels.net/model-qualifiers/">
     <rdf:Description rdf:about="#">
       <bqbiol:isVersionOf>
         <rdf:Bag>
           <rdf:li rdf:resource="http://www.geneontology.org/#GO:0005892"/>
           <rdf:li rdf:resource="http://www.ebi.ac.uk/interpro/#IPR002394"/>
         </rdf:Bag>
       </bqbiol:isVersionOf>
       <bqbiol:is>
         <rdf:Bag>
           <rdf:li rdf:resource="http://www.geneontology.org/#GO:0005895"/>
         </rdf:Bag>
       </bqbiol:is>
     </rdf:Description>
   </rdf:RDF>
 </annotation>
  eos
 
s.appendAnnotation(cvterms_annotation);

# 
# The above code can be replaced by the following code.
# 
# 
# CVTerm *cv = CVTerm();
# cv.setQualifierType(BIOLOGICAL_QUALIFIER);
# cv.setBiologicalQualifierType(BQB_IS_VERSION_OF);
# cv.addResource("http://www.geneontology.org/#GO:0005892");
# 
# CVTerm *cv2 = CVTerm();
# cv2.setQualifierType(BIOLOGICAL_QUALIFIER);
# cv2.setBiologicalQualifierType(BQB_IS);
# cv2.addResource("http://www.geneontology.org/#GO:0005895");
# 
# CVTerm *cv1 = CVTerm();
# cv1.setQualifierType(BIOLOGICAL_QUALIFIER);
# cv1.setBiologicalQualifierType(BQB_IS_VERSION_OF);
# cv1.addResource("http://www.ebi.ac.uk/interpro/#IPR002394");
# 
# s.addCVTerm(cv);
# s.addCVTerm(cv2);
# s.addCVTerm(cv1);
# 
# 
# 

LibSBML::writeSBML(d, ARGV[1]);
exit(errors)
 

