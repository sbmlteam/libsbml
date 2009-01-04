#
# @file    TestXMLErrorLog.rb
# @brief   XMLErrorLog unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id:$
# $HeadURL:$
#
# This test file was converted from src/sbml/test/TestXMLErrorLog.c
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2009 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#--------------------------------------------------------------------------->*/
require 'test/unit'
require 'libSBML'

class TestXMLErrorLog < Test::Unit::TestCase

  def test_XMLErrorLog_add
    log = LibSBML::XMLErrorLog.new()
    error = LibSBML::XMLError.new()
    log.add(error)
    assert( log != nil )
    assert( log.getNumErrors() == 1 )
    assert( log.getError(0) != nil )
    assert( log.getError(2) == nil )
    log = nil
  end

  def test_XMLErrorLog_clear
    log = LibSBML::XMLErrorLog.new()
    error = LibSBML::XMLError.new()
    log.add(error)
    log.clearLog()
    assert( log != nil )
    assert( log.getNumErrors() == 0 )
    log = nil
  end

  def test_XMLErrorLog_create
    log = LibSBML::XMLErrorLog.new()
    assert( log != nil )
    assert( log.getNumErrors() == 0 )
    log = nil
  end

end
