#!/usr/bin/env ruby
#
## 
## @file    readSBML.py
## @brief   Similar to validateSBML, but without the validation
## @author  Sarah Keating
## @author  Ben Bornstein
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


require 'libSBML'



if ARGV.size != 1
  puts "Usage: readSBML filename"
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
puts ""

if errors > 0
  d.printErrors 
end

exit(errors)

