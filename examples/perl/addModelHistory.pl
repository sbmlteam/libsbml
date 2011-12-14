#!/usr/bin/env perl
# -*-Perl-*-
## 
## \file    addModelHistory.pl
## \brief   adds Model History to a model
## \author  Sarah Keating
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


use LibSBML;
no strict;

sub printStatus {
  $message = $_[0];
  $status = $_[1];
  $statusString = "";
  if (status == $LibSBML::LIBSBML_OPERATION_SUCCESS) {
    $statusString = "succeeded";
  }
  elsif (status == $LibSBML::LIBSBML_INVALID_OBJECT) {
    $statusString = "invalid object";
  }
  elsif (status == $LibSBML::LIBSBML_OPERATION_FAILED) {
    $statusString = "operation failed";
  }
  else {
    $statusString = "unknown";          
  }  
  print ($message, $statusString, "\n");
}


if ($#ARGV  != 1) {
  print "usage: addModelHistory <input-filename> <output-filename>\n";
  print "       Adds a model history to the model\n";
  exit 2;
}

$d = LibSBML::readSBML($ARGV[0]);
$errors = $d->getNumErrors();

if (errors > 0) {
    print("Read Error(s):", "\n");
    $d->printErrors();  
    print("Correct the above and re-run.", "\n");
	exit $errors;
}

$h = new LibSBML::ModelHistory();

$c = new LibSBML::ModelCreator();
$c->setFamilyName("Keating");
$c->setGivenName("Sarah");
$c->setEmail("sbml-team@caltech.edu");
$c->setOrganization("University of Hertfordshire");

$status = $h->addCreator($c);
printStatus("Status for addCreator: ", $status);


$date = new LibSBML::Date("1999-11-13T06:54:32");
$date2 = new LibSBML::Date("2007-11-30T06:54:00-02:00");

$status = $h->setCreatedDate($date);
printStatus("Set created date:      ", $status);

$status = $h->setModifiedDate($date2);
printStatus("Set modified date:     ", $status);

$status = $d->getModel()->setModelHistory($h);
printStatus("Set model history:     ", $status);


LibSBML::writeSBML($d, $ARGV[1]);

exit $errors;

