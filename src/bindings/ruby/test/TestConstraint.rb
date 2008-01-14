#
# @file    TestConstraint.rb
# @brief   SBML Constraint unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestConstraint.c
# wiht the help of conversion sciprt (ctest_converter.pl).
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
#
require 'test/unit'
require 'libSBML'

class TestConstraint < Test::Unit::TestCase

  def setup
    @@c = LibSBML::Constraint.new
    if (@@c == nil)
    end
  end

  def test_Constraint_create
    assert( @@c.getTypeCode == LibSBML::SBML_CONSTRAINT )
    assert( @@c.getMetaId == "" )
    assert( @@c.getNotes == nil )
    assert( @@c.getAnnotation == nil )
    assert_equal false, @@c.isSetMessage
    assert_equal false, @@c.isSetMath
  end

  def test_Constraint_createWithMath
    math = LibSBML::parseFormula("1 + 1")
    c = LibSBML::Constraint.new(math)
    assert( c.getTypeCode == LibSBML::SBML_CONSTRAINT )
    assert( c.getMetaId == "" )
    assert( c.getMath != math )
    assert_equal false, c.isSetMessage
    assert_equal true, c.isSetMath
  end

  def test_Constraint_free_NULL
  end

  def test_Constraint_setMath
    math = LibSBML::parseFormula("2 * k")
    @@c.setMath(math)
    assert( @@c.getMath != math )
    assert_equal true, @@c.isSetMath
    @@c.setMath(@@c.getMath)
    assert( @@c.getMath != math )
    @@c.setMath(nil)
    assert_equal false, @@c.isSetMath
    if (@@c.getMath != nil)
    end
  end

  def test_Constraint_setMessage
    node = LibSBML::XMLNode.new
    @@c.setMessage(node)
    assert( @@c.getMessage != node )
    assert( @@c.isSetMessage == true )
    @@c.setMessage(@@c.getMessage)
    assert( @@c.getMessage != node )
    @@c.unsetMessage
    assert_equal false, @@c.isSetMessage
    if (@@c.getMessage != nil)
    end
  end

end
