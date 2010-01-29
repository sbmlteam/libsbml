#
# @file    TestRule_newSetters.rb
# @brief   Rule unit tests for new set function API
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestRule_newSetters.c
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2010 California Institute of Technology.
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

class TestRule_newSetters < Test::Unit::TestCase

  def setup
    @@r = LibSBML::AssignmentRule.new(2,4)
    if (@@r == nil)
    end
  end

  def teardown
    @@r = nil
  end

  def test_Rule_setFormula1
    formula =  "k1*X0";
    i = @@r.setFormula(formula)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert (( formula == @@r.getFormula() ))
    assert_equal true, @@r.isSetFormula()
  end

  def test_Rule_setFormula2
    i = @@r.setFormula("")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, @@r.isSetFormula()
  end

  def test_Rule_setFormula3
    formula =  "k1 X0";
    i = @@r.setFormula(formula)
    assert( i == LibSBML::LIBSBML_INVALID_OBJECT )
    assert_equal false, @@r.isSetFormula()
  end

  def test_Rule_setMath1
    math = LibSBML::ASTNode.new(LibSBML::AST_TIMES)
    a = LibSBML::ASTNode.new()
    b = LibSBML::ASTNode.new()
    a.setName( "a")
    b.setName( "b")
    math.addChild(a)
    math.addChild(b)
    i = @@r.setMath(math)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal true, @@r.isSetMath()
    math1 = @@r.getMath()
    assert( math1 != nil )
    formula = LibSBML::formulaToString(math1)
    assert( formula != nil )
    assert ((  "a * b" == formula ))
    math = nil
  end

  def test_Rule_setMath2
    math = LibSBML::ASTNode.new(LibSBML::AST_TIMES)
    a = LibSBML::ASTNode.new()
    a.setName( "a")
    math.addChild(a)
    i = @@r.setMath(math)
    assert( i == LibSBML::LIBSBML_INVALID_OBJECT )
    assert_equal false, @@r.isSetMath()
    math = nil
  end

  def test_Rule_setMath3
    i = @@r.setMath(nil)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, @@r.isSetMath()
  end

  def test_Rule_setUnits1
    i = @@r.setUnits( "second")
    assert( i == LibSBML::LIBSBML_UNEXPECTED_ATTRIBUTE )
    assert_equal false, @@r.isSetUnits()
  end

  def test_Rule_setUnits2
    r1 = LibSBML::AssignmentRule.new(1,2)
    r1.setL1TypeCode(LibSBML::SBML_PARAMETER_RULE)
    i = r1.setUnits( "second")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal true, r1.isSetUnits()
    i = r1.unsetUnits()
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, r1.isSetUnits()
    r1 = nil
  end

  def test_Rule_setUnits3
    r1 = LibSBML::AssignmentRule.new(1,2)
    r1.setL1TypeCode(LibSBML::SBML_PARAMETER_RULE)
    i = r1.setUnits( "1second")
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert_equal false, r1.isSetUnits()
    i = r1.unsetUnits()
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, r1.isSetUnits()
    r1 = nil
  end

  def test_Rule_setUnits4
    r1 = LibSBML::AssignmentRule.new(1,2)
    r1.setL1TypeCode(LibSBML::SBML_PARAMETER_RULE)
    i = r1.setUnits( "second")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal true, r1.isSetUnits()
    i = r1.setUnits("")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, r1.isSetUnits()
    r1 = nil
  end

  def test_Rule_setVariable1
    i = @@r.setVariable( "1mole")
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert_equal false, @@r.isSetVariable()
  end

  def test_Rule_setVariable2
    i = @@r.setVariable( "mole")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal true, @@r.isSetVariable()
    i = @@r.setVariable( "")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, @@r.isSetVariable()
  end

  def test_Rule_setVariable3
    r1 = LibSBML::AlgebraicRule.new(1,2)
    i = r1.setVariable( "r")
    assert( i == LibSBML::LIBSBML_UNEXPECTED_ATTRIBUTE )
    assert_equal false, r1.isSetVariable()
    r1 = nil
  end

end
