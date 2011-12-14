#!/usr/bin/env perl
# -*-Perl-*-
## 
## \file    addCVTerms.pl
## \brief   adds controlled vocabulary terms to a species in a model
## \author  Sarah Keating
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 

use LibSBML;
no strict;

if ($#ARGV != 1) {
  print "usage: addCVTerms <input-filename> <output-filename>\n";
  print "       Adds controlled vocabulary term to a species\n";
  exit 2;
}

$d = LibSBML::readSBML($ARGV[0]);
$errors = $d->getNumErrors();

if ($errors > 0) {
    print("Read Error(s):");
    $d->printErrors();  
    print("Correct the above and re-run.");
    exit $errors;
}

$n = $d->getModel()->getNumSpecies();

if ($n <= 0) {
    print("Model has no species.\n Cannot add CV terms\n");
    exit 0;
}

$s = $d->getModel()->getSpecies(0);
if ( not $s->isSetMetaId()) {
  $s->setMetaId("metaid_s0000052");
}

$cv = new LibSBML::CVTerm();
$cv->setQualifierType($LibSBML::BIOLOGICAL_QUALIFIER);
$cv->setBiologicalQualifierType($LibSBML::BQB_IS_VERSION_OF);
$cv->addResource("http://www.geneontology.org/#GO:0005892");

$cv2 = new LibSBML::CVTerm();
$cv2->setQualifierType($LibSBML::BIOLOGICAL_QUALIFIER);
$cv2->setBiologicalQualifierType($LibSBML::BQB_IS);
$cv2->addResource("http://www.geneontology.org/#GO:0005895");

$cv1 = new LibSBML::CVTerm();
$cv1->setQualifierType($LibSBML::BIOLOGICAL_QUALIFIER);
$cv1->setBiologicalQualifierType($LibSBML::BQB_IS_VERSION_OF);
$cv1->addResource("http://www.ebi.ac.uk/interpro/#IPR002394");

$s->addCVTerm($cv);
$s->addCVTerm($cv2);
$s->addCVTerm($cv1);

LibSBML::writeSBML($d, $ARGV[1]);

exit $errors;

