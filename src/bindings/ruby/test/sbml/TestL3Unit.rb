#
# @file    TestL3Unit.rb
# @brief   L3 Unit unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestL3Unit.c
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

class TestL3Unit < Test::Unit::TestCase

  def isnan(x)
    return (x != x)
  end
  @@SBML_INT_MAX = 2147483647
  def setup
    @@u = LibSBML::Unit.new(3,1)
    if (@@u == nil)
    end
  end

  def teardown
    @@u = nil
  end

  def test_L3_Unit_create
    assert( @@u.getTypeCode() == LibSBML::SBML_UNIT )
    assert( @@u.getMetaId() == "" )
    assert( @@u.getNotes() == nil )
    assert( @@u.getAnnotation() == nil )
    assert( @@u.getKind() == LibSBML::UNIT_KIND_INVALID )
    assert_equal true, isnan(@@u.getExponentAsDouble())
    assert_equal true, isnan(@@u.getMultiplier())
    assert( @@u.getScale() == @@SBML_INT_MAX )
    assert_equal false, @@u.isSetKind()
    assert_equal false, @@u.isSetExponent()
    assert_equal false, @@u.isSetMultiplier()
    assert_equal false, @@u.isSetScale()
  end

  def test_L3_Unit_createWithNS
    xmlns = LibSBML::XMLNamespaces.new()
    xmlns.add( "http://www.sbml.org", "testsbml")
    sbmlns = LibSBML::SBMLNamespaces.new(3,1)
    sbmlns.addNamespaces(xmlns)
    u = LibSBML::Unit.new(sbmlns)
    assert( u.getTypeCode() == LibSBML::SBML_UNIT )
    assert( u.getMetaId() == "" )
    assert( u.getNotes() == nil )
    assert( u.getAnnotation() == nil )
    assert( u.getLevel() == 3 )
    assert( u.getVersion() == 1 )
    assert( u.getNamespaces() != nil )
    assert( u.getNamespaces().getLength() == 2 )
    assert( u.getKind() == LibSBML::UNIT_KIND_INVALID )
    assert_equal true, isnan(u.getExponentAsDouble())
    assert_equal true, isnan(u.getMultiplier())
    assert_equal false, u.isSetKind()
    assert_equal false, u.isSetExponent()
    assert_equal false, u.isSetMultiplier()
    assert_equal false, u.isSetScale()
    u = nil
  end

  def test_L3_Unit_exponent
    exponent = 0.2
    assert_equal false, @@u.isSetExponent()
    assert_equal true, isnan(@@u.getExponentAsDouble())
    @@u.setExponent(exponent)
    assert( @@u.getExponentAsDouble() == exponent )
    assert_equal true, @@u.isSetExponent()
  end

  def test_L3_Unit_free_NULL
  end

  def test_L3_Unit_hasRequiredAttributes
    u = LibSBML::Unit.new(3,1)
    assert_equal false, u.hasRequiredAttributes()
    u.setKind(LibSBML::UNIT_KIND_MOLE)
    assert_equal false, u.hasRequiredAttributes()
    u.setExponent(0)
    assert_equal false, u.hasRequiredAttributes()
    u.setMultiplier(0.45)
    assert_equal false, u.hasRequiredAttributes()
    u.setScale(2)
    assert_equal true, u.hasRequiredAttributes()
    u = nil
  end

  def test_L3_Unit_kind
    kind =  "mole";
    assert_equal false, @@u.isSetKind()
    @@u.setKind(LibSBML::UnitKind_forName(kind))
    assert( @@u.getKind() == LibSBML::UNIT_KIND_MOLE )
    assert_equal true, @@u.isSetKind()
  end

  def test_L3_Unit_multiplier
    multiplier = 0.2
    assert_equal false, @@u.isSetMultiplier()
    assert_equal true, isnan(@@u.getMultiplier())
    @@u.setMultiplier(multiplier)
    assert( @@u.getMultiplier() == multiplier )
    assert_equal true, @@u.isSetMultiplier()
  end

  def test_L3_Unit_scale
    scale = 2
    assert_equal false, @@u.isSetScale()
    assert( @@u.getScale() == @@SBML_INT_MAX )
    @@u.setScale(scale)
    assert( @@u.getScale() == scale )
    assert_equal true, @@u.isSetScale()
  end

end
