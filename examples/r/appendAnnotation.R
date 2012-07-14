# 
# \file    appendAnnotation.R
# \brief   adds annotation strings to a model and a species
# \author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
#
# Usage: R --slave -f appendAnnotation.R --args <input-filename> <output-filename>
#
#

library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2) {
  stop(
         "  usage: appendAnnotation <input-filename> <output-filename>\n"
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
  if (!SBase_isSetMetaId(m))
		SBase_setMetaId(m, "___model2501");


  model_history_annotation = paste(
     "<annotation>\n",
     "  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n",
     "    <rdf:Description rdf:about=\"#", SBase_getMetaId(m), "\">\n",
     "      <dc:creator rdf:parseType=\"Resource\">\n",
     "        <rdf:Bag>\n",
     "          <rdf:li rdf:parseType=\"Resource\">\n",
     "            <vCard:N rdf:parseType=\"Resource\">\n",
     "              <vCard:Family>Keating</vCard:Family>\n",
     "              <vCard:Given>Sarah</vCard:Given>\n",
     "            </vCard:N>\n",
     "            <vCard:EMAIL>sbml-team@caltech.edu</vCard:EMAIL>\n",
     "            <vCard:ORG>\n",
     "              <vCard:Orgname>University of Hertfordshire</vCard:Orgname>\n",
     "            </vCard:ORG>\n",
     "          </rdf:li>\n",
     "        </rdf:Bag>\n",
     "      </dc:creator>\n",
     "      <dcterms:created rdf:parseType=\"Resource\">\n",
     "        <dcterms:W3CDTF>1999-11-13T06:54:32Z</dcterms:W3CDTF>\n",
     "      </dcterms:created>\n",
     "      <dcterms:modified rdf:parseType=\"Resource\">\n",
     "        <dcterms:W3CDTF>2007-11-31T06:54:00-02:00</dcterms:W3CDTF>\n",
     "      </dcterms:modified>\n",
     "    </rdf:Description>\n",
     "  </rdf:RDF>\n",
     "</annotation>\n",
	 sep = "");

  SBase_appendAnnotation(m, model_history_annotation);

  #
  # The above code can be replaced by the code in addModelHistory.R
  #

  n = Model_getNumSpecies(m);

  if (n > 0) {
    s = Model_getSpecies(m, 0);

	if (!SBase_isSetMetaId(s))
		SBase_setMetaId(s, "___species2501");
	
    cvterms_annotation = paste("<annotation>\n",
      "  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n",
      "    <rdf:Description rdf:about=\"#", SBase_getMetaId(s) , "\">\n",
      "      <bqbiol:isVersionOf>\n",
      "        <rdf:Bag>\n",
      "          <rdf:li rdf:resource=\"http://www.geneontology.org/#GO:0005892\"/>\n",
      "          <rdf:li rdf:resource=\"http://www.ebi.ac.uk/interpro/#IPR002394\"/>\n",
      "        </rdf:Bag>\n",
      "      </bqbiol:isVersionOf>\n",
      "      <bqbiol:is>\n",
      "        <rdf:Bag>\n",
      "          <rdf:li rdf:resource=\"http://www.geneontology.org/#GO:0005895\"/>\n",
      "        </rdf:Bag>\n",
      "      </bqbiol:is>\n",
      "    </rdf:Description>\n",
      "  </rdf:RDF>\n",
      "</annotation>\n",
	  sep = "");


    SBase_appendAnnotation(s, cvterms_annotation);

    #
    # The above code can be replaced by the code in addCVTerms.R.
    #
  }

  writeSBML(d, args[2]);
}

q(status=errors)
