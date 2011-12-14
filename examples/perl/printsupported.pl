#!/usr/bin/env perl
# -*-Perl-*-
## 
## @file    printsupported.pl
## @brief   Prints all SBML Levels and Versions supported by this version 
##          of libsbml.
## @author  Frank Bergmann
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 

use LibSBML;
no strict;
print("Supported by LibSBML " , LibSBML::getLibSBMLDottedVersion(), "\n");

$supported = LibSBML::SBMLNamespaces::getSupportedNamespaces();
for ($i = 0; $i < $supported->getSize(); $i ++) {
	$current = $supported->get($i);
	print("\tSBML Level: ", $current->getLevel(), " Version: ", $current->getVersion(), "\n");
}
print("\n");



