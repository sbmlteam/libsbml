#!/usr/bin/env perl
# -*-Perl-*-
## 
## @file    printRegisteredPackages.pl
## @brief   Prints the registerd packages for this libSBML
## @author  Frank Bergmann
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 

use LibSBML;
no strict;
print("This version of LibSBML: ", LibSBML::getLibSBMLDottedVersion(), " includes: \n");

for ($i = 0; $i < LibSBML::SBMLExtensionRegistry::getNumRegisteredPackages(); $i++){
    print("\t", LibSBML::SBMLExtensionRegistry::getRegisteredPackageName($i),"\n");
}
print("\n");

