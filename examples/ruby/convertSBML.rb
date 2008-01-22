#!/usr/bin/env ruby
#
# @file    convertSBML.rb
# @brief   Converts SBML documents between levels 
# @author  Alex Gutteridge (Ruby conversion of examples/c/convertSBML.c)
# @author  Ben Bornstein
# @author  Michael Hucka
#
# $Id$
# $Source$
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#

require 'libSBML'
     
latest_level   = LibSBML::SBMLDocument::getDefaultLevel
latest_version = LibSBML::SBMLDocument::getDefaultVersion               

if ARGV.size != 2
  puts "Usage: ruby convertSBML.rb input-filename output-filename"
  puts "This program will attempt to convert a model either to"
  puts "SBML Level #{latest_level} Version #{latest_version} (if the model is not already) or, if"
  puts "the model is already expressed in Level #{latest_level} Version #{latest_version}, this"
  puts "program will attempt to convert the model to Level 1 Version 2."
  exit(1)
end

d = LibSBML::readSBML(ARGV[0])

if d.getNumErrors > 0
  puts "Encountered the following SBML error(s)"
  d.printErrors
  puts "Conversion skipped. Please correct the problems above first"
  exit d.getNumErrors
end
                                    
success = false

if d.getLevel < latest_level || d.getVersion < latest_version
  puts "Attempting to convert model to SBML Level #{latest_level} Version #{latest_version}"
  success = d.setLevelAndVersion(latest_level,latest_version)    
else
  puts "Attempting to convert model to SBML Level 1 Version 2"
  success = d.setLevelAndVersion(1,2)
end

if not success
  puts "Unable to perform conversion due to the following:"  
  d.printErrors
  puts "Conversion skipped.  Either libSBML does not (yet) have"
  puts "ability to convert this model, or (automatic) conversion"
  puts "is not possible in this case."
elsif d.getNumErrors > 0
  puts "Information may have been lost in conversion; but a valid model"
  puts "was produced by the conversion.\nThe following information "
  puts "was provided:"
  d.printErrors
  LibSBML::writeSBML(d, ARGV[1]);
else
  puts "Conversion completed."
  LibSBML::writeSBML(d, ARGV[1])
end
