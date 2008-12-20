#
# @file    TestXMLErrorC.rb
# @brief   XMLError unit tests, C version
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id:$
# $HeadURL:$
#
# This test file was converted from src/sbml/test/TestXMLErrorC.c
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2008 California Institute of Technology.
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

class TestXMLErrorC < Test::Unit::TestCase

  def test_XMLError_create_C
    error = LibSBML::XMLError.new()
    assert( error != nil )
    assert( error.isInfo() == false )
    assert( error.isWarning() == false )
    assert( error.isError() == false )
    assert( error.isFatal() == true )
    error = nil
    error = LibSBML::XMLError.new(12345, "My message")
    assert( (  "My message" != error.getMessage() ) == false )
    assert( error.getErrorId() == 12345 )
    error = nil
  end

end
