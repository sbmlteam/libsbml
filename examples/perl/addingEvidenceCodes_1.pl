#!/usr/bin/env perl
# -*-Perl-*-
## 
## \file    addingEvidenceCodes_1.pl
## \brief   adds controlled vocabulary terms to a reaction in a model
## \author  Sarah Keating
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


use LibSBML;
no strict;

if ($#ARGV != 1) {
  print "usage: addingEvidenceCodes_1 <input-filename> <output-filename>\n";
  print "       Adds controlled vocabulary term to a reaction\n";
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

$n = $d->getModel()->getNumReactions();

if ($n <= 0) {
    print("Model has no reactions.\n Cannot add CV terms\n");
    exit 0;
}

$r = $d->getModel()->getReaction(0);

# check that the reaction has a metaid
# no CVTerms will be added if there is no metaid to reference
# 
if ( not $r->isSetMetaId()) {
    $r->setMetaId("metaid_0000052");
}

$cv1 = new LibSBML::CVTerm($LibSBML::BIOLOGICAL_QUALIFIER);
$cv1->setBiologicalQualifierType($LibSBML::BQB_IS_DESCRIBED_BY);
$cv1->addResource("urn:miriam:obo.eco:ECO%3A0000183");

$r->addCVTerm($cv1);

$cv2 = new LibSBML::CVTerm($LibSBML::BIOLOGICAL_QUALIFIER);
$cv2->setBiologicalQualifierType($LibSBML::BQB_IS);
$cv2->addResource("urn:miriam:kegg.reaction:R00756");
$cv2->addResource("urn:miriam:reactome:REACT_736");

$r->addCVTerm($cv2);

LibSBML::writeSBML($d, $ARGV[1]);
exit $errors;

