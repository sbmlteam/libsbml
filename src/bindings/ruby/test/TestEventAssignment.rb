#
# @file    TestEventAssignment.rb
# @brief   SBML EventAssignment unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestEventAssignment.c
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

class TestEventAssignment < Test::Unit::TestCase

  def setup
    @@ea = LibSBML::EventAssignment.new
    if (@@ea == nil)
    end
  end

  def test_EventAssignment_create
    assert( @@ea.getTypeCode == LibSBML::SBML_EVENT_ASSIGNMENT )
    assert( @@ea.getMetaId == "" )
    assert( @@ea.getNotes == nil )
    assert( @@ea.getAnnotation == nil )
    assert( @@ea.getVariable == "" )
    assert( @@ea.getMath == nil )
  end

  def test_EventAssignment_createWith
    math = LibSBML::parseFormula("0")
    ea = LibSBML::EventAssignment.new("k",math)
    assert( ea.getTypeCode == LibSBML::SBML_EVENT_ASSIGNMENT )
    assert( ea.getMetaId == "" )
    assert( ea.getNotes == nil )
    assert( ea.getAnnotation == nil )
    assert( ea.getMath != math )
    assert_equal true, ea.isSetMath
    assert ((  "k" == ea.getVariable ))
    assert_equal true, ea.isSetVariable
  end

  def test_EventAssignment_free_NULL
  end

  def test_EventAssignment_setMath
    math = LibSBML::parseFormula("2 * k")
    @@ea.setMath(math)
    math1 = @@ea.getMath
    assert( math1 != nil )
    formula = LibSBML::formulaToString(math1)
    assert( formula != nil )
    assert ((  "2 * k" == formula ))
    assert( @@ea.getMath != math )
    assert_equal true, @@ea.isSetMath
    @@ea.setMath(@@ea.getMath)
    math1 = @@ea.getMath
    assert( math1 != nil )
    formula = LibSBML::formulaToString(math1)
    assert( formula != nil )
    assert ((  "2 * k" == formula ))
    assert( @@ea.getMath != math )
    @@ea.setMath(nil)
    assert_equal false, @@ea.isSetMath
    if (@@ea.getMath != nil)
    end
  end

  def test_EventAssignment_setVariable
    variable = "k2"
    @@ea.setVariable(variable)
    assert (( variable == @@ea.getVariable ))
    assert_equal true, @@ea.isSetVariable
    if (@@ea.getVariable == variable)
    end
    @@ea.setVariable(@@ea.getVariable)
    assert (( variable == @@ea.getVariable ))
    @@ea.setVariable("")
    assert_equal false, @@ea.isSetVariable
    if (@@ea.getVariable != nil)
    end
  end

end
