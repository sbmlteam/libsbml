#!/usr/bin/env perl
# -*-Perl-*-
##
## @file    flattenModel.pl
## @brief   Flattens the comp code from the given SBML file.
## @author  Frank T. Bergmann
##
##
## This file is part of libSBML->  Please visit http://sbml->org for more
## information about SBML, and the latest version of libSBML->

use File::Basename;
use LibSBML;
no strict;

if ($#ARGV != 1) {
   print  "usage: flattenModel.pl input-filename output-filename\n";
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
$props = new LibSBML::ConversionProperties();
$props->addOption("flatten comp", true, "flatten comp");
$props->addOption("leavePorts", false,"unused ports should be listed in the flattened model");
if ($sbmldoc->convert($props) != $LibSBML::LIBSBML_OPERATION_SUCCESS)
{
  print("[Error] Conversion failed.", "\n");
  $sbmldoc->printErrors();
  exit 3;
}
$writer->writeSBML($sbmldoc, $outfile);
print("[OK] flattend model to ", $outfile, "\n");

