#!/usr/bin/env ruby
#
# @file    convertFbcToCobra.rb
# @brief   Convert COBRA L2 to L3 with FBC
# @author  Frank T. Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.

require 'libSBML'

if ARGV.size != 2
  puts "Usage: convertFbcToCobra input-filename output-filename"
  exit(2)
end

d = LibSBML::readSBML(ARGV[0])
if (d.getNumErrors() > 0)
	d.printErrors
else
	props = LibSBML::ConversionProperties.new
	props.addOption("convert fbc to cobra", true, "Convert FBC model to Cobra model")
	if d.convert(props) != 0
		puts "[Error] conversion failed..."
		exit(3)
	end
	LibSBML::writeSBML(d,ARGV[1])
end

