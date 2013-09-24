#!/usr/bin/env ruby
#
# @file    promoteParameters.rb
# @brief   promotes all local to global paramters
# @author  Frank T. Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.

require 'libSBML'

if ARGV.size != 2
  puts "Usage: promoteParameters input-filename output-filename"
  exit(2)
end

d = LibSBML::readSBML(ARGV[0])
if (d.getNumErrors() > 0)
	d.printErrors
else
	props = LibSBML::ConversionProperties.new
	props.addOption("promoteLocalParameters", true, "Promotes all Local Parameters to Global ones")
	if d.convert(props) != 0
		puts "[Error] conversion failed..."
		exit(3)
	end
	LibSBML::writeSBML(d,ARGV[1])
end

