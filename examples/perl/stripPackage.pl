#!/usr/bin/env perl
# -*-Perl-*-
##
## @file    stripPackage.pl
## @brief   Strips the given package from the given SBML file.
## @author  Frank T. Bergmann
## 
##
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.

use File::Basename;
use LibSBML;
no strict;

if ($#ARGV != 2) {
   print  "usage: stripPackage.pl input-filename package-to-strip output-filename\n";
   exit 1;
}

$infile  = $ARGV[0];
$package = $ARGV[1];
$outfile = $ARGV[2];

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
$props->addOption("stripPackage", true, "Strip SBML Level 3 package constructs from the model");
$props->addOption("package", $package,"Name of the SBML Level 3 package to be stripped");
if ($sbmldoc->convert($props) != $LibSBML::LIBSBML_OPERATION_SUCCESS)
{
  print("[Error] Conversion failed.", "\n");
  exit 3;
}
$writer->writeSBML($sbmldoc, $outfile);
print("[OK] striped package '", $package,"' from ", $infile, " to ", $outfile, "\n");

