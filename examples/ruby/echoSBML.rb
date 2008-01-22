#!/usr/bin/env ruby
#
# @file    echoSBML.rb
# @brief   Echos (and pretty prints) an SBML model.
# @author  Alex Gutteridge (Ruby conversion of examples/c/echoSBML.c)
# @author  Ben Bornstein
#
# $Id$
# $Source$
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#

require 'libSBML'

if ARGV.size != 2
  puts "Usage: echoSBML input-filename output-filename"
  exit(2)
end

LibSBML::writeSBML(LibSBML::readSBML(ARGV[0]),ARGV[1])
