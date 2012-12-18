#!/usr/bin/env ruby
#
# @file    stripPackage.rb
# @brief   Strips the given package from the given SBML file.
# @author  Frank T. Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.

require 'libSBML'

if ARGV.size != 3
  puts "Usage: stripPackage input-filename package-to-strip output-filename"
  exit(2)
end

d = LibSBML::readSBML(ARGV[0])
if (d.getNumErrors() > 0)
	d.printErrors
else
	props = LibSBML::ConversionProperties.new
	props.addOption("stripPackage", true, "Strip SBML Level 3 package constructs from the model")
	props.addOption("package", ARGV[1], "Name of the SBML Level 3 package to be stripped")
	if d.convert(props) != 0
		puts "[Error] conversion failed..."
		exit(3)
	end
	LibSBML::writeSBML(d,ARGV[2])
end

