#!/usr/bin/env perl
# -*-Perl-*-
##
## @file    echoSBML.pl
## @brief   Echos an SBML model.
## @author  Akiya Jouraku (translated from libSBML C++ examples)
## @author  Ben Bornstein
## @author  Michael Hucka
## 
##
## This file is part of libSBML->  Please visit http://sbml->org for more
## information about SBML, and the latest version of libSBML->

use File::Basename;
use LibSBML;
no strict;

if ($#ARGV != 1) {
   print  "usage: echoSBML.pl input-filename output-filename\n";
   exit 1;
}

$infile  = $ARGV[0];
$outfile = $ARGV[1];

unless (-e $infile) {
  print("[Error] ", $infile, ": No such file.", "\n");
  exit 1;
}
$reader  = new LibSBML::SBMLReader();
$writer  = new LibSBML::SBMLWriter();
$sbmldoc = $reader->readSBML($infile);

if ($sbmldoc->getNumErrors() > 0) {
  if ($sbmldoc->getError(0)->getErrorId() == $LibSBML::XMLFileUnreadable) {
    # Handle case of unreadable file here.
    $sbmldoc->printErrors();
  }
  elsif ($sbmldoc->getError(0)->getErrorId() == $LibSBML::XMLFileOperationError) {
    # Handle case of other file error here.
    $sbmldoc->printErrors();
  }
  else {
    # Handle other error cases here.
    $sbmldoc->printErrors();
  }
  exit 1;
}
$writer->writeSBML($sbmldoc, $outfile);
print("[OK] Echoed ", $infile, " to ", $outfile, "\n");

