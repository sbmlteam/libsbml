#!/usr/bin/env ruby
#
# @file    flattenModel.rb
# @brief   Flattens the comp code from the given SBML file.
# @author  Frank T. Bergmann
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.

require 'libSBML'

if ARGV.size < 2 || ARGV.size > 3
  puts "Usage: flattenModel [-p] input-filename  output-filename"
  puts " -p : list unused ports"
  exit(2)
end

inFile=""
outFile=""
listPorts = ARGV.size == 3
if listPorts then
	inFile=ARGV[1]
	outFile=ARGV[2]
else
	inFile=ARGV[0]
	outFile=ARGV[1]
end

d = LibSBML::readSBML(inFile)
if (d.getNumErrors() > 0)
	d.printErrors
else
	props = LibSBML::ConversionProperties.new
	props.addOption("flatten comp", true, "Flatten Hierarchical model")
	props.addOption("leavePorts", listPorts, "unused ports should be listed in the flattened model")
	if d.convert(props) != 0
		puts "[Error] conversion failed..."
		d.printErrors
		exit(3)
	end
	LibSBML::writeSBML(d,outFile)
end

