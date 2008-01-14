#
# @file    TestAlgebraicRule.rb
# @brief   AlgebraicRule unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestAlgebraicRule.c
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

class TestAlgebraicRule < Test::Unit::TestCase

  def setup
    @@ar = LibSBML::AlgebraicRule.new()
    if (@@ar == nil)
    end
  end

  def test_AlgebraicRule_create
    assert( @@ar.getTypeCode == LibSBML::SBML_ALGEBRAIC_RULE )
    assert( @@ar.getMetaId == "" )
    assert( @@ar.getNotes == nil )
    assert( @@ar.getAnnotation == nil )
    assert( @@ar.getFormula == "" )
    assert( @@ar.getMath == nil )
  end

  def test_AlgebraicRule_createWithFormula
    ar = LibSBML::AlgebraicRule.new("1 + 1")
    assert( ar.getTypeCode == LibSBML::SBML_ALGEBRAIC_RULE )
    assert( ar.getMetaId == "" )
    math = ar.getMath
    assert( math != nil )
    formula = LibSBML::formulaToString(math)
    assert( formula != nil )
    assert ((  "1 + 1" == formula ))
    assert (( formula == ar.getFormula ))
  end

  def test_AlgebraicRule_createWithMath
    math = LibSBML::parseFormula("1 + 1")
    ar = LibSBML::AlgebraicRule.new(math)
    assert( ar.getTypeCode == LibSBML::SBML_ALGEBRAIC_RULE )
    assert( ar.getMetaId == "" )
    assert ((  "1 + 1" == ar.getFormula ))
    assert( ar.getMath != math )
  end

  def test_AlgebraicRule_free_NULL
  end

end
