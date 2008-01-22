#!/usr/bin/env ruby
#
# @file    validateSBML.rb
# @brief   Validates an SBML file against the appropriate schema
# @author  Alex Gutteridge  (Ruby conversion of examples/c/validateSBML.c)
# @author  Ben Bornstein
#
# $Id$
# $Source$
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#

require 'libSBML'

if ARGV.size != 1
  puts "Usage: ruby validateSBML.rb filename"
  exit(2)
end

filename = ARGV[0]

start = Time.now
d = LibSBML::readSBML(filename)
stop = Time.now

errors  = d.getNumErrors
errors += d.checkConsistency

puts "      filename: #{filename}"
puts "     file size: #{File.size(filename)}"
puts "read time (ms): #{((stop-start)*1000).to_i}"
puts "      error(s): #{errors}"

if errors > 0
  d.printErrors 
end

exit(errors)
