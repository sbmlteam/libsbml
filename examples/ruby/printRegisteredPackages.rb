#!/usr/bin/env ruby
#
## 
## @file    printRegisteredPackages.py
## @brief   Prints the registerd packages for this libSBML
## @author  Frank Bergmann
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


require 'libSBML'

puts "This version of LibSBML: #{LibSBML::getLibSBMLDottedVersion()} includes: \n"

LibSBML::SBMLExtensionRegistry.getNumRegisteredPackages.times do |i|
    puts "\t #{LibSBML::SBMLExtensionRegistry.getRegisteredPackageName(i)} \n"
end
puts "\n"


