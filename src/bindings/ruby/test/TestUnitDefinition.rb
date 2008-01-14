#
# @file    TestUnitDefinition.rb
# @brief   SBML UnitDefinition unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestUnitDefinition.c
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

class TestUnitDefinition < Test::Unit::TestCase

  def setup
    @@ud = LibSBML::UnitDefinition.new
    if (@@ud == nil)
    end
  end

  def test_UnitDefinition_addUnit
    u = LibSBML::Unit.new
    @@ud.addUnit(u)
    assert( @@ud.getNumUnits == 1 )
  end

  def test_UnitDefinition_create
    assert( @@ud.getTypeCode == LibSBML::SBML_UNIT_DEFINITION )
    assert( @@ud.getMetaId == "" )
    assert( @@ud.getNotes == nil )
    assert( @@ud.getAnnotation == nil )
    assert( @@ud.getId == "" )
    assert( @@ud.getName == "" )
    assert_equal false, @@ud.isSetId
    assert_equal false, @@ud.isSetName
    assert( @@ud.getNumUnits == 0 )
  end

  def test_UnitDefinition_createWith
    ud = LibSBML::UnitDefinition.new("mmls", "")
    assert( ud.getTypeCode == LibSBML::SBML_UNIT_DEFINITION )
    assert( ud.getMetaId == "" )
    assert( ud.getNotes == nil )
    assert( ud.getAnnotation == nil )
    assert( ud.getName == "" )
    assert ((  "mmls" == ud.getId ))
    assert_equal true, ud.isSetId
    assert( ud.getNumUnits == 0 )
  end

  def test_UnitDefinition_createWithName
    ud = LibSBML::UnitDefinition.new("", "mmol liter^-1 sec^-1")
    assert( ud.getTypeCode == LibSBML::SBML_UNIT_DEFINITION )
    assert( ud.getMetaId == "" )
    assert( ud.getNotes == nil )
    assert( ud.getAnnotation == nil )
    assert( ud.getId == "" )
    assert ((  "mmol liter^-1 sec^-1" == ud.getName ))
    assert_equal true, ud.isSetName
    assert( ud.getNumUnits == 0 )
  end

  def test_UnitDefinition_free_NULL
  end

  def test_UnitDefinition_getUnit
    mole = LibSBML::Unit.new
    litre = LibSBML::Unit.new
    second = LibSBML::Unit.new
    mole.setKind(LibSBML::UnitKind_forName("mole"))
    litre.setKind(LibSBML::UnitKind_forName("litre"))
    second.setKind(LibSBML::UnitKind_forName("second"))
    mole.setScale(-3)
    litre.setExponent(-1)
    second.setExponent(-1)
    @@ud.addUnit(mole)
    @@ud.addUnit(litre)
    @@ud.addUnit(second)
    assert( @@ud.getNumUnits == 3 )
    mole = @@ud.getUnit(0)
    litre = @@ud.getUnit(1)
    second = @@ud.getUnit(2)
    assert( mole.getKind == LibSBML::UNIT_KIND_MOLE )
    assert( litre.getKind == LibSBML::UNIT_KIND_LITRE )
    assert( second.getKind == LibSBML::UNIT_KIND_SECOND )
    assert( mole.getScale == -3 )
    assert( litre.getExponent == -1 )
    assert( second.getExponent == -1 )
  end

  def test_UnitDefinition_isVariantOfArea
    u = @@ud.createUnit
    assert_equal false, @@ud.isVariantOfArea
    u.setKind(LibSBML::UNIT_KIND_METRE)
    u.setExponent(2)
    assert_equal true, @@ud.isVariantOfArea
    u.setScale(-1)
    assert_equal true, @@ud.isVariantOfArea
    u.setMultiplier(2)
    assert_equal true, @@ud.isVariantOfArea
    u.setOffset(3)
    assert_equal true, @@ud.isVariantOfArea
    u.setExponent(3)
    assert_equal false, @@ud.isVariantOfArea
  end

  def test_UnitDefinition_isVariantOfLength
    u = @@ud.createUnit
    assert_equal false, @@ud.isVariantOfLength
    u.setKind(LibSBML::UNIT_KIND_METRE)
    u.setExponent(1)
    assert_equal true, @@ud.isVariantOfLength
    u.setScale(-1)
    assert_equal true, @@ud.isVariantOfLength
    u.setMultiplier(2)
    assert_equal true, @@ud.isVariantOfLength
    u.setOffset(3)
    assert_equal true, @@ud.isVariantOfLength
    u.setExponent(2)
    assert_equal false, @@ud.isVariantOfLength
  end

  def test_UnitDefinition_isVariantOfSubstance_1
    u = @@ud.createUnit
    assert_equal false, @@ud.isVariantOfSubstance
    u.setKind(LibSBML::UNIT_KIND_MOLE)
    u.setExponent(1)
    assert_equal true, @@ud.isVariantOfSubstance
    u.setScale(-1)
    assert_equal true, @@ud.isVariantOfSubstance
    u.setMultiplier(2)
    assert_equal true, @@ud.isVariantOfSubstance
    u.setOffset(3)
    assert_equal true, @@ud.isVariantOfSubstance
    u.setExponent(-3)
    assert_equal false, @@ud.isVariantOfSubstance
  end

  def test_UnitDefinition_isVariantOfSubstance_2
    u = @@ud.createUnit
    assert_equal false, @@ud.isVariantOfSubstance
    u.setKind(LibSBML::UNIT_KIND_ITEM)
    u.setExponent(1)
    assert_equal true, @@ud.isVariantOfSubstance
    u.setScale(-1)
    assert_equal true, @@ud.isVariantOfSubstance
    u.setMultiplier(2)
    assert_equal true, @@ud.isVariantOfSubstance
    u.setOffset(3)
    assert_equal true, @@ud.isVariantOfSubstance
    u.setExponent(-2)
    assert_equal false, @@ud.isVariantOfSubstance
  end

  def test_UnitDefinition_isVariantOfTime
    u = @@ud.createUnit
    assert_equal false, @@ud.isVariantOfTime
    u.setKind(LibSBML::UNIT_KIND_SECOND)
    u.setExponent(1)
    assert_equal true, @@ud.isVariantOfTime
    u.setScale(-10)
    assert_equal true, @@ud.isVariantOfTime
    u.setMultiplier(10)
    assert_equal true, @@ud.isVariantOfTime
    u.setOffset(30)
    assert_equal true, @@ud.isVariantOfTime
    u.setExponent(2)
    assert_equal false, @@ud.isVariantOfTime
  end

  def test_UnitDefinition_isVariantOfVolume_1
    u = @@ud.createUnit
    assert_equal false, @@ud.isVariantOfVolume
    u.setKind(LibSBML::UNIT_KIND_LITRE)
    u.setExponent(1)
    assert_equal true, @@ud.isVariantOfVolume
    u.setScale(100)
    assert_equal true, @@ud.isVariantOfVolume
    u.setMultiplier(5)
    assert_equal true, @@ud.isVariantOfVolume
    u.setOffset(-5)
    assert_equal true, @@ud.isVariantOfVolume
    u.setExponent(-1)
    assert_equal false, @@ud.isVariantOfVolume
  end

  def test_UnitDefinition_isVariantOfVolume_2
    u = @@ud.createUnit
    assert_equal false, @@ud.isVariantOfVolume
    u.setKind(LibSBML::UNIT_KIND_METRE)
    u.setExponent(3)
    assert_equal true, @@ud.isVariantOfVolume
    u.setScale(100)
    assert_equal true, @@ud.isVariantOfVolume
    u.setMultiplier(5)
    assert_equal true, @@ud.isVariantOfVolume
    u.setOffset(-5)
    assert_equal true, @@ud.isVariantOfVolume
    u.setExponent(2)
    assert_equal false, @@ud.isVariantOfVolume
  end

  def test_UnitDefinition_setId
    id = "mmls"
    @@ud.setId(id)
    assert (( id == @@ud.getId ))
    assert_equal true, @@ud.isSetId
    if (@@ud.getId == id)
    end
    @@ud.setId(@@ud.getId)
    assert (( id == @@ud.getId ))
    @@ud.setId("")
    assert_equal false, @@ud.isSetId
    if (@@ud.getId != nil)
    end
  end

  def test_UnitDefinition_setName
    name = "mmol liter^-1 sec^-1"
    @@ud.setName(name)
    assert (( name == @@ud.getName ))
    assert_equal true, @@ud.isSetName
    if (@@ud.getName == name)
    end
    @@ud.setName(@@ud.getName)
    assert (( name == @@ud.getName ))
    @@ud.setName("")
    assert_equal false, @@ud.isSetName
    if (@@ud.getName != nil)
    end
  end

end
