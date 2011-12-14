#!/usr/bin/env ruby
#
## 
## @file    printsupported.rb
## @brief   Prints all SBML Levels and Versions supported by this version 
##          of libsbml.
## @author  Frank Bergmann
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


require 'libSBML'


puts "Supported by LibSBML #{LibSBML::getLibSBMLDottedVersion()}"
puts 

supported = LibSBML::SBMLNamespaces.getSupportedNamespaces
supported.getSize.times do |i|
	current = supported.get(i)
	puts "\tSBML Level: #{current.getLevel} Version: #{current.getVersion}"
end
puts



