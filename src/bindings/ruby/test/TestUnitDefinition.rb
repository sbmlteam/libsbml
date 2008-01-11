#
# This file was converted from libsbml/src/sbml/test/TestUnitDefinition.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestUnitDefinition < Test::Unit::TestCase

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

  def test_UnitDefinition_free_NULL
  end

  def test_UnitDefinition_addUnit
    u = LibSBML::Unit.new
    @@ud.addUnit(u)
    assert_equal 1, @@ud.getNumUnits
  end

  def test_UnitDefinition_createWithName
    ud = LibSBML::UnitDefinition.new("", "mmol liter^-1 sec^-1")
    assert_equal LibSBML::SBML_UNIT_DEFINITION, ud.getTypeCode
    assert_equal "", ud.getMetaId
    assert_equal nil, ud.getNotes
    assert_equal nil, ud.getAnnotation
    assert_equal "", ud.getId
    assert_equal  "mmol liter^-1 sec^-1",ud.getName
    assert_equal true, ud.isSetName
    assert_equal 0, ud.getNumUnits
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

  def test_UnitDefinition_createWith
    ud = LibSBML::UnitDefinition.new("mmls", "")
    assert_equal LibSBML::SBML_UNIT_DEFINITION, ud.getTypeCode
    assert_equal "", ud.getMetaId
    assert_equal nil, ud.getNotes
    assert_equal nil, ud.getAnnotation
    assert_equal "", ud.getName
    assert_equal  "mmls",ud.getId
    assert_equal true, ud.isSetId
    assert_equal 0, ud.getNumUnits
  end

  def test_UnitDefinition_setId
    id = "mmls"
    @@ud.setId(id)
    assert_equal id,@@ud.getId
    assert_equal true, @@ud.isSetId
      @@ud.setId(@@ud.getId)
      assert_equal id,@@ud.getId
      @@ud.setId("")
      assert_equal false, @@ud.isSetId
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

  def test_UnitDefinition_setName
    name = "mmol liter^-1 sec^-1"
    @@ud.setName(name)
    assert_equal name,@@ud.getName
    assert_equal true, @@ud.isSetName
      @@ud.setName(@@ud.getName)
      assert_equal name,@@ud.getName
      @@ud.setName("")
      assert_equal false, @@ud.isSetName
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

  def test_UnitDefinition_create
    assert_equal LibSBML::SBML_UNIT_DEFINITION, @@ud.getTypeCode
    assert_equal "", @@ud.getMetaId
    assert_equal nil, @@ud.getNotes
    assert_equal nil, @@ud.getAnnotation
    assert_equal "", @@ud.getId
    assert_equal "", @@ud.getName
    assert_equal false, @@ud.isSetId
    assert_equal false, @@ud.isSetName
    assert_equal 0, @@ud.getNumUnits
  end

  def setup
    @@ud = LibSBML::UnitDefinition.new
      
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
    assert_equal 3, @@ud.getNumUnits
    mole = @@ud.getUnit(0)
    litre = @@ud.getUnit(1)
    second = @@ud.getUnit(2)
    assert_equal LibSBML::UNIT_KIND_MOLE, mole.getKind
    assert_equal LibSBML::UNIT_KIND_LITRE, litre.getKind
    assert_equal LibSBML::UNIT_KIND_SECOND, second.getKind
    assert_equal -3, mole.getScale
    assert_equal -1, litre.getExponent
    assert_equal -1, second.getExponent
  end

end
