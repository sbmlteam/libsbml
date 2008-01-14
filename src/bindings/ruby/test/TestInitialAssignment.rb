#
# @file    TestInitialAssignment.rb
# @brief   SBML InitialAssignment unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestInitialAssignment.c
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

class TestInitialAssignment < Test::Unit::TestCase

  def setup
    @@ia = LibSBML::InitialAssignment.new
    if (@@ia == nil)
    end
  end

  def test_InitialAssignment_create
    assert( @@ia.getTypeCode == LibSBML::SBML_INITIAL_ASSIGNMENT )
    assert( @@ia.getMetaId == "" )
    assert( @@ia.getNotes == nil )
    assert( @@ia.getAnnotation == nil )
    assert( @@ia.getSymbol == "" )
    assert( @@ia.getMath == nil )
  end

  def test_InitialAssignment_createWith
    ia = LibSBML::InitialAssignment.new("k")
    assert( ia.getTypeCode == LibSBML::SBML_INITIAL_ASSIGNMENT )
    assert( ia.getMetaId == "" )
    assert( ia.getNotes == nil )
    assert( ia.getAnnotation == nil )
    assert_equal false, ia.isSetMath
    assert ((  "k" == ia.getSymbol ))
    assert_equal true, ia.isSetSymbol
  end

  def test_InitialAssignment_free_NULL
  end

  def test_InitialAssignment_setMath
    math = LibSBML::parseFormula("2 * k")
    @@ia.setMath(math)
    math1 = @@ia.getMath
    assert( math1 != nil )
    formula = LibSBML::formulaToString(math1)
    assert( formula != nil )
    assert ((  "2 * k" == formula ))
    assert( @@ia.getMath != math )
    assert_equal true, @@ia.isSetMath
    @@ia.setMath(@@ia.getMath)
    math1 = @@ia.getMath
    assert( math1 != nil )
    formula = LibSBML::formulaToString(math1)
    assert( formula != nil )
    assert ((  "2 * k" == formula ))
    assert( @@ia.getMath != math )
    @@ia.setMath(nil)
    assert_equal false, @@ia.isSetMath
    if (@@ia.getMath != nil)
    end
  end

  def test_InitialAssignment_setSymbol
    symbol = "k2"
    @@ia.setSymbol(symbol)
    assert (( symbol == @@ia.getSymbol ))
    assert_equal true, @@ia.isSetSymbol
    if (@@ia.getSymbol == symbol)
    end
    @@ia.setSymbol(@@ia.getSymbol)
    assert (( symbol == @@ia.getSymbol ))
    @@ia.setSymbol("")
    assert_equal false, @@ia.isSetSymbol
    if (@@ia.getSymbol != nil)
    end
  end

end
