#
# @file    TestUnit.rb
# @brief   Unit unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestUnit.c
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

class TestUnit < Test::Unit::TestCase

  def setup
    @@u = LibSBML::Unit.new
    if (@@u == nil)
    end
  end

  def test_Unit_create
    assert( @@u.getTypeCode == LibSBML::SBML_UNIT )
    assert( @@u.getMetaId == "" )
    assert( @@u.getNotes == nil )
    assert( @@u.getAnnotation == nil )
    assert( @@u.getKind == LibSBML::UNIT_KIND_INVALID )
    assert( @@u.getExponent == 1 )
    assert( @@u.getScale == 0 )
    assert( @@u.getMultiplier == 1.0 )
    assert_equal false, @@u.isSetKind
  end

  def test_Unit_createWith
    u = LibSBML::Unit.new(LibSBML::UNIT_KIND_SECOND,-2,1)
    assert( u.getTypeCode == LibSBML::SBML_UNIT )
    assert( u.getMetaId == "" )
    assert( u.getNotes == nil )
    assert( u.getAnnotation == nil )
    assert( u.getKind == LibSBML::UNIT_KIND_SECOND )
    assert( u.getExponent == -2 )
    assert( u.getScale == 1 )
    assert( u.getMultiplier == 1.0 )
    assert( u.getOffset == 0.0 )
    assert_equal true, u.isSetKind
  end

  def test_Unit_free_NULL
  end

  def test_Unit_isBuiltIn
    assert_equal true, LibSBML::Unit.isBuiltIn( "substance",1)
    assert_equal true, LibSBML::Unit.isBuiltIn( "volume"   ,1)
    assert_equal false, LibSBML::Unit.isBuiltIn( "area"     ,1)
    assert_equal false, LibSBML::Unit.isBuiltIn( "length"   ,1)
    assert_equal true, LibSBML::Unit.isBuiltIn( "time"     ,1)
    assert_equal true, LibSBML::Unit.isBuiltIn( "substance",2)
    assert_equal true, LibSBML::Unit.isBuiltIn( "volume"   ,2)
    assert_equal true, LibSBML::Unit.isBuiltIn( "area"     ,2)
    assert_equal true, LibSBML::Unit.isBuiltIn( "length"   ,2)
    assert_equal true, LibSBML::Unit.isBuiltIn( "time"     ,2)
    assert_equal false, LibSBML::Unit.isBuiltIn("",1)
    assert_equal false, LibSBML::Unit.isBuiltIn( ""       ,1)
    assert_equal false, LibSBML::Unit.isBuiltIn( "volt"   ,1)
    assert_equal false, LibSBML::Unit.isBuiltIn( "foobar" ,1)
    assert_equal false, LibSBML::Unit.isBuiltIn("",2)
    assert_equal false, LibSBML::Unit.isBuiltIn( ""       ,2)
    assert_equal false, LibSBML::Unit.isBuiltIn( "volt"   ,2)
    assert_equal false, LibSBML::Unit.isBuiltIn( "foobar" ,2)
  end

  def test_Unit_isXXX
    assert_equal false, @@u.isSetKind
    @@u.setKind(LibSBML::UNIT_KIND_AMPERE)
    assert_equal true, @@u.isAmpere
    @@u.setKind(LibSBML::UNIT_KIND_BECQUEREL)
    assert_equal true, @@u.isBecquerel
    @@u.setKind(LibSBML::UNIT_KIND_CANDELA)
    assert_equal true, @@u.isCandela
    @@u.setKind(LibSBML::UNIT_KIND_CELSIUS)
    assert_equal true, @@u.isCelsius
    @@u.setKind(LibSBML::UNIT_KIND_COULOMB)
    assert_equal true, @@u.isCoulomb
    @@u.setKind(LibSBML::UNIT_KIND_DIMENSIONLESS)
    assert_equal true, @@u.isDimensionless
    @@u.setKind(LibSBML::UNIT_KIND_FARAD)
    assert_equal true, @@u.isFarad
    @@u.setKind(LibSBML::UNIT_KIND_GRAM)
    assert_equal true, @@u.isGram
    @@u.setKind(LibSBML::UNIT_KIND_GRAY)
    assert_equal true, @@u.isGray
    @@u.setKind(LibSBML::UNIT_KIND_HENRY)
    assert_equal true, @@u.isHenry
    @@u.setKind(LibSBML::UNIT_KIND_HERTZ)
    assert_equal true, @@u.isHertz
    @@u.setKind(LibSBML::UNIT_KIND_ITEM)
    assert_equal true, @@u.isItem
    @@u.setKind(LibSBML::UNIT_KIND_JOULE)
    assert_equal true, @@u.isJoule
    @@u.setKind(LibSBML::UNIT_KIND_KATAL)
    assert_equal true, @@u.isKatal
    @@u.setKind(LibSBML::UNIT_KIND_KELVIN)
    assert_equal true, @@u.isKelvin
    @@u.setKind(LibSBML::UNIT_KIND_KILOGRAM)
    assert_equal true, @@u.isKilogram
    @@u.setKind(LibSBML::UNIT_KIND_LITRE)
    assert_equal true, @@u.isLitre
    @@u.setKind(LibSBML::UNIT_KIND_LUMEN)
    assert_equal true, @@u.isLumen
    @@u.setKind(LibSBML::UNIT_KIND_LUX)
    assert_equal true, @@u.isLux
    @@u.setKind(LibSBML::UNIT_KIND_METRE)
    assert_equal true, @@u.isMetre
    @@u.setKind(LibSBML::UNIT_KIND_MOLE)
    assert_equal true, @@u.isMole
    @@u.setKind(LibSBML::UNIT_KIND_NEWTON)
    assert_equal true, @@u.isNewton
    @@u.setKind(LibSBML::UNIT_KIND_OHM)
    assert_equal true, @@u.isOhm
    @@u.setKind(LibSBML::UNIT_KIND_PASCAL)
    assert_equal true, @@u.isPascal
    @@u.setKind(LibSBML::UNIT_KIND_RADIAN)
    assert_equal true, @@u.isRadian
    @@u.setKind(LibSBML::UNIT_KIND_SECOND)
    assert_equal true, @@u.isSecond
    @@u.setKind(LibSBML::UNIT_KIND_SIEMENS)
    assert_equal true, @@u.isSiemens
    @@u.setKind(LibSBML::UNIT_KIND_SIEVERT)
    assert_equal true, @@u.isSievert
    @@u.setKind(LibSBML::UNIT_KIND_STERADIAN)
    assert_equal true, @@u.isSteradian
    @@u.setKind(LibSBML::UNIT_KIND_TESLA)
    assert_equal true, @@u.isTesla
    @@u.setKind(LibSBML::UNIT_KIND_VOLT)
    assert_equal true, @@u.isVolt
    @@u.setKind(LibSBML::UNIT_KIND_WATT)
    assert_equal true, @@u.isWatt
    @@u.setKind(LibSBML::UNIT_KIND_WEBER)
    assert_equal true, @@u.isWeber
  end

  def test_Unit_set_get
    u = LibSBML::Unit.new
    assert( u.getKind == LibSBML::UNIT_KIND_INVALID )
    assert( u.getExponent == 1 )
    assert( u.getScale == 0 )
    assert( u.getMultiplier == 1.0 )
    assert_equal false, u.isSetKind
    u.setKind(LibSBML::UNIT_KIND_WATT)
    assert( u.getKind == LibSBML::UNIT_KIND_WATT )
    u.setExponent(3)
    assert( u.getExponent == 3 )
    u.setScale(4)
    assert( u.getScale == 4 )
    u.setMultiplier(3.2)
    assert( u.getMultiplier == 3.2 )
  end

end
