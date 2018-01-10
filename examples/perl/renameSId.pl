#!/usr/bin/env perl
# -*-Perl-*-
## 
## \file    reanameSId.pl
## \brief   renames a specified SId to the new identifier
## \author  Frank T. Bergmann
## 
## <!--------------------------------------------------------------------------
## This sample program is distributed under a different license than the rest
## of libSBML.  This program uses the open-source MIT license, as follows:
##
## Copyright (c) 2013-2018 by the California Institute of Technology
## (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
## and the University of Heidelberg (Germany), with support from the National
## Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
##
## Permission is hereby granted, free of charge, to any person obtaining a
## copy of this software and associated documentation files (the "Software"),
## to deal in the Software without restriction, including without limitation
## the rights to use, copy, modify, merge, publish, distribute, sublicense,
## and/or sell copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
## THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
## DEALINGS IN THE SOFTWARE.
##
## Neither the name of the California Institute of Technology (Caltech), nor
## of the European Bioinformatics Institute (EMBL-EBI), nor of the University
## of Heidelberg, nor the names of any contributors, may be used to endorse
## or promote products derived from this software without specific prior
## written permission.
## ------------------------------------------------------------------------ -->
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

