#
# This file was converted from libsbml/src/sbml/test/TestUnit.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestUnit < Test::Unit::TestCase

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

  def test_Unit_free_NULL
  end

  def test_Unit_create
    assert_equal LibSBML::SBML_UNIT, @@u.getTypeCode
    assert_equal "", @@u.getMetaId
    assert_equal nil, @@u.getNotes
    assert_equal nil, @@u.getAnnotation
    assert_equal LibSBML::UNIT_KIND_INVALID, @@u.getKind
    assert_equal 1, @@u.getExponent
    assert_equal 0, @@u.getScale
    assert_equal 1.0, @@u.getMultiplier
    assert_equal false, @@u.isSetKind
  end

  def test_Unit_set_get
    u = LibSBML::Unit.new
    assert_equal LibSBML::UNIT_KIND_INVALID, u.getKind
    assert_equal 1, u.getExponent
    assert_equal 0, u.getScale
    assert_equal 1.0, u.getMultiplier
    assert_equal false, u.isSetKind
    u.setKind(LibSBML::UNIT_KIND_WATT)
    assert_equal LibSBML::UNIT_KIND_WATT, u.getKind
    u.setExponent(3)
    assert_equal 3, u.getExponent
    u.setScale(4)
    assert_equal 4, u.getScale
    u.setMultiplier(3.2)
    assert_equal 3.2, u.getMultiplier
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

  def test_Unit_createWith
    u = LibSBML::Unit.new(LibSBML::UNIT_KIND_SECOND,-2,1)
    assert_equal LibSBML::SBML_UNIT, u.getTypeCode
    assert_equal "", u.getMetaId
    assert_equal nil, u.getNotes
    assert_equal nil, u.getAnnotation
    assert_equal LibSBML::UNIT_KIND_SECOND, u.getKind
    assert_equal -2, u.getExponent
    assert_equal 1, u.getScale
    assert_equal 1.0, u.getMultiplier
    assert_equal 0.0, u.getOffset
    assert_equal true, u.isSetKind
  end

  def setup
    @@u = LibSBML::Unit.new
      
  end

end
