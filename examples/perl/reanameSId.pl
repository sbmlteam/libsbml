#!/usr/bin/env perl
# -*-Perl-*-
## 
## \file    reanameSId.pl
## \brief   renames a specified SId to the new identifier
## \author  Frank T. Bergmann
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 

use LibSBML;
no strict;

if ($#ARGV != 3) {
  print "usage: reanameSId <input-filename> <output-filename> <oldId> <newId>\n";
  print "       renames an id in the model\n";
  exit 1;
}

print("Using LibSBML : ");
print(LibSBML::getLibSBMLDottedVersion());
print("\n");

if (!LibSBML::SyntaxChecker::isValidInternalSId($ARGV[3])) { 
   print("The newId is not a valid SId\n");
   exit 3;
} 

$d = LibSBML::readSBML($ARGV[0]);
$errors = $d->getNumErrors($LibSBML::LIBSBML_SEV_ERROR);

if ($errors > 0) {
    print("Read Error(s):\n");
    $d->printErrors();  
    print("Correct the above and re-run.\n");
    exit $errors;
}

# find elements for old id
$element = $d->getElementBySId($ARGV[2]);
if (!defined($element))
{
  print("Found no element with the old SId ");
  exit 2;
}
 
# found element --> renaming
$element->setId($ARGV[3]);

# update all references to this element
$allElements = $d->getListOfAllElements();
for ($i = 0; $i < $allElements->getSize(); $i++) {
  $current = $allElements->get($i);
  bless $current, 'LibSBML::SBase';
  $current->renameSIdRefs($ARGV[2], $ARGV[3]);
}

LibSBML::writeSBML($d, $ARGV[1]);

exit $errors;

