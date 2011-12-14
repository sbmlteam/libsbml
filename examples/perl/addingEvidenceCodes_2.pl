#!/usr/bin/env perl
# -*-Perl-*-
## 
## \file    addingEvidenceCodes_2.pl
## \brief   adds evidence codes to a species in a model
## \author  Sarah Keating
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


use LibSBML;
no strict;

if ($#ARGV != 1) {
 print "usage: addingEvidenceCodes_2 <input-filename> <output-filename>\n";
 print "       Adds controlled vocabulary term to a species\n";
 exit 2;
}

$d = LibSBML::readSBML($ARGV[0]);
$errors = $d->getNumErrors();

if ($errors > 0) {
  print("Read Error(s):\n");
  $d->printErrors();  
  print("Correct the above and re-run.\n");
  exit $errors;
}

$n = $d->getModel()->getNumSpecies();

if ($n <= 0) {
    print("Model has no species.\n Cannot add CV terms\n");
	exit 0;
}

$s = $d->getModel()->getSpecies(0);

# check that the species has a metaid
# no CVTerms will be added if there is no metaid to reference
# 
if (not $s->isSetMetaId()) {
    $s->setMetaId("metaid_0000052");
}

$cv1 = new LibSBML::CVTerm($LibSBML::BIOLOGICAL_QUALIFIER);
$cv1->setBiologicalQualifierType($LibSBML::BQB_OCCURS_IN);
$cv1->addResource("urn:miriam:obo.go:GO%3A0005764");

$s->addCVTerm($cv1);

# now create the additional annotation

# <rdf:Statement> 
#   <rdf:subject rdf:resource="#metaid_0000052"/> 
#   <rdf:predicate rdf:resource="http://biomodels.net/biology-qualifiers/occursIn"/> 
#   <rdf:object rdf:resource="urn:miriam:obo.go:GO%3A0005764"/> 
#   <bqbiol:isDescribedBy> 
#     <rdf:Bag> 
#       <rdf:li rdf:resource="urn:miriam:obo.eco:ECO%3A0000004"/> 
#       <rdf:li rdf:resource="urn:miriam:pubmed:7017716"/> 
#     </rdf:Bag> 
#   </bqbiol:isDescribedBy> 
# </rdf:Statement> 

# attributes
$blank_att = new LibSBML::XMLAttributes();

$resource_att = new LibSBML::XMLAttributes();

#  create the outer statement node 
$statement_triple = new LibSBML::XMLTriple("Statement",
                                       "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                                       "rdf");

$statement_token = new LibSBML::XMLToken($statement_triple, $blank_att);

$statement = new LibSBML::XMLNode($statement_token);

# create the subject node
$subject_triple = new LibSBML::XMLTriple("subject",
                                     "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                                     "rdf");

$resource_att->clear();
$resource_att->add("rdf:resource", "#" . $s->getMetaId());

$subject_token = new LibSBML::XMLToken($subject_triple, $resource_att);

$subject = new LibSBML::XMLNode($subject_token);


#create the predicate node 
$predicate_triple = new LibSBML::XMLTriple("predicate",
                                       "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                                       "rdf");

$resource_att->clear();
$resource_att->add("rdf:resource",
                 "http://biomodels.net/biology-qualifiers/occursIn");

$predicate_token = new LibSBML::XMLToken($predicate_triple, $resource_att);

$predicate = new LibSBML::XMLNode($predicate_token);

#create the object node 
$object_triple = new LibSBML::XMLTriple("object",
                                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                                    "rdf");

$resource_att->clear();
$resource_att->add("rdf:resource", "urn:miriam:obo.go:GO%3A0005764");

$object_token = new LibSBML::XMLToken($object_triple, $resource_att);

$object_ = new LibSBML::XMLNode($object_token);

# create the bqbiol node 
$bqbiol_triple = new LibSBML::XMLTriple("isDescribedBy",
                                    "http://biomodels.net/biology-qualifiers/",
                                    "bqbiol");

$bqbiol_token = new LibSBML::XMLToken($bqbiol_triple, $blank_att);

$bqbiol = new LibSBML::XMLNode($bqbiol_token);

# create the bag node 
$bag_triple = new LibSBML::XMLTriple("Bag",
                                 "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                                 "rdf");

$bag_token = new LibSBML::XMLToken($bag_triple, $blank_att);

$bag = new LibSBML::XMLNode($bag_token);

# create each li node and add to the bag 
$li_triple = new LibSBML::XMLTriple("li",
                                "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                                "rdf");

$resource_att->clear();
$resource_att->add("rdf:resource", "urn:miriam:obo.eco:ECO%3A0000004");

$li_token = new LibSBML::XMLToken($li_triple, $resource_att);
$li_token->setEnd();

$li = new LibSBML::XMLNode($li_token);

$bag->addChild($li);

$resource_att->clear();
$resource_att->add("rdf:resource", "urn:miriam:pubmed:7017716");
$li_token = new LibSBML::XMLToken($li_triple, $resource_att);
$li_token->setEnd();
$li = new LibSBML::XMLNode($li_token);

$bag->addChild($li);

# add the bag to bqbiol 
$bqbiol->addChild($bag);

# add subject, predicate, object and bqbiol to statement 
$statement->addChild($subject);
$statement->addChild($predicate);
$statement->addChild($object_);
$statement->addChild($bqbiol);


# create a top-level RDF element 
# this will ensure correct merging
# 

$xmlns = new LibSBML::XMLNamespaces();
$xmlns->add("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf");
$xmlns->add("http://purl.org/dc/elements/1.1/", "dc");
$xmlns->add("http://purl.org/dc/terms/", "dcterms");
$xmlns->add("http://www.w3.org/2001/vcard-rdf/3.0#", "vCard");
$xmlns->add("http://biomodels.net/biology-qualifiers/", "bqbiol");
$xmlns->add("http://biomodels.net/model-qualifiers/", "bqmodel");

$RDF_triple = new LibSBML::XMLTriple("RDF",
                                 "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                                 "rdf");

$RDF_token = new LibSBML::XMLToken($RDF_triple, $blank_att, $xmlns);

$annotation = new LibSBML::XMLNode($RDF_token);

# add the staement node to the RDF node 
$annotation->addChild($statement);

$s->appendAnnotation($annotation);

LibSBML::writeSBML($d, $ARGV[1]);
exit $errors;

