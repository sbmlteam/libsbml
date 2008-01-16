#
# @file    TestRateRule.rb
# @brief   RateRule unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestRateRule.c
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
#
require 'test/unit'
require 'libSBML'

class TestRateRule < Test::Unit::TestCase

  def setup
    @@rr = LibSBML::RateRule.new()
    if (@@rr == nil)
    end
  end

  def teardown
    @@rr = nil
  end

  def test_RateRule_create
    assert( @@rr.getTypeCode == LibSBML::SBML_RATE_RULE )
    assert( @@rr.getMetaId == "" )
    assert( @@rr.getNotes == nil )
    assert( @@rr.getAnnotation == nil )
    assert( @@rr.getFormula == "" )
    assert( @@rr.getMath == nil )
    assert( @@rr.getVariable == "" )
    assert( @@rr.getType == LibSBML::RULE_TYPE_RATE )
  end

  def test_RateRule_createWithFormula
    ar = LibSBML::RateRule.new("s", "1 + 1")
    assert( ar.getTypeCode == LibSBML::SBML_RATE_RULE )
    assert( ar.getMetaId == "" )
    assert ((  "s" == ar.getVariable ))
    math = ar.getMath
    assert( math != nil )
    formula = LibSBML::formulaToString(math)
    assert( formula != nil )
    assert ((  "1 + 1" == formula ))
    assert (( formula == ar.getFormula ))
    ar = nil
  end

  def test_RateRule_createWithMath
    math = LibSBML::parseFormula("1 + 1")
    ar = LibSBML::RateRule.new("s",math)
    assert( ar.getTypeCode == LibSBML::SBML_RATE_RULE )
    assert( ar.getMetaId == "" )
    assert ((  "s" == ar.getVariable ))
    assert ((  "1 + 1" == ar.getFormula ))
    assert( ar.getMath != math )
    ar = nil
  end

  def test_RateRule_free_NULL
    
  end

  def test_RateRule_setVariable
    variable = "x"
    @@rr.setVariable(variable)
    assert (( variable == @@rr.getVariable ))
    assert_equal true, @@rr.isSetVariable
    if (@@rr.getVariable == variable)
    end
    @@rr.setVariable(@@rr.getVariable)
    assert (( variable == @@rr.getVariable ))
    @@rr.setVariable("")
    assert_equal false, @@rr.isSetVariable
    if (@@rr.getVariable != nil)
    end
  end

end
