#
# This file was converted from libsbml/src/sbml/test/TestCompartment.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestCompartment < Test::Unit::TestCase

  def test_Compartment_setId
    id = "mitochondria"
    @@c.setId(id)
    assert_equal id,@@c.getId
    assert_equal true, @@c.isSetId
      @@c.setId(@@c.getId)
      assert_equal id,@@c.getId
      @@c.setId("")
      assert_equal false, @@c.isSetId
  end

  def test_Compartment_setUnits
    units = "volume"
    @@c.setUnits(units)
    assert_equal units,@@c.getUnits
    assert_equal true, @@c.isSetUnits
      @@c.setUnits(@@c.getUnits)
      assert_equal units,@@c.getUnits
      @@c.setUnits("")
      assert_equal false, @@c.isSetUnits
  end

  def test_Compartment_getsetType
    @@c.setCompartmentType( "cell")
    assert_equal  "cell" ,@@c.getCompartmentType
    assert_equal true, @@c.isSetCompartmentType
    @@c.unsetCompartmentType
    assert_equal false, @@c.isSetCompartmentType
  end

  def test_Compartment_unsetVolume
    @@c.setVolume(1.0)
    assert_equal 1.0, @@c.getVolume
    assert_equal true, @@c.isSetVolume
    @@c.unsetVolume
    assert_equal false, @@c.isSetVolume
  end

  def test_Compartment_getsetConstant
    @@c.setConstant(1)
    assert_equal true, @@c.getConstant
  end

  def test_Compartment_free_NULL
  end

  def test_Compartment_setOutside
    outside = "cell"
    @@c.setOutside(outside)
    assert_equal outside,@@c.getOutside
    assert_equal true, @@c.isSetOutside
      @@c.setOutside(@@c.getOutside)
      assert_equal outside,@@c.getOutside
      @@c.setOutside("")
      assert_equal false, @@c.isSetOutside
  end

  def test_Compartment_createWith
    c = LibSBML::Compartment.new("A", "")
    assert_equal LibSBML::SBML_COMPARTMENT, c.getTypeCode
    assert_equal "", c.getMetaId
    assert_equal nil, c.getNotes
    assert_equal nil, c.getAnnotation
    assert_equal "", c.getName
    assert_equal 3, c.getSpatialDimensions
    assert_equal  "A"     ,c.getId
    assert_equal true, c.getConstant
    assert_equal true, c.isSetId
    assert_equal false, c.isSetName
  end

  def setup
    @@c = LibSBML::Compartment.new
      
  end

  def test_Compartment_getSpatialDimensions
    @@c.setSpatialDimensions(1)
    assert_equal 1, @@c.getSpatialDimensions
  end

  def test_Compartment_setName
    name = "My Favorite Factory"
    @@c.setName(name)
    assert_equal name,@@c.getName
    assert_equal true, @@c.isSetName
      @@c.setName(@@c.getName)
      assert_equal name,@@c.getName
      @@c.setName("")
      assert_equal false, @@c.isSetName
  end

  def test_Compartment_initDefaults
    c = LibSBML::Compartment.new("A", "")
    c.initDefaults
    assert_equal  "A",c.getId
    assert_equal "", c.getName
    assert_equal "", c.getUnits
    assert_equal "", c.getOutside
    assert_equal 3, c.getSpatialDimensions
    assert_equal 1.0, c.getVolume
    assert_equal true, c.getConstant
    assert_equal true, c.isSetId
    assert_equal false, c.isSetName
    assert_equal false, c.isSetSize
    assert_equal false, c.isSetVolume
    assert_equal false, c.isSetUnits
    assert_equal false, c.isSetOutside
  end

  def test_Compartment_unsetSize
    @@c.setSize(0.2)
    assert_equal 0.2, @@c.getSize
    assert_equal true, @@c.isSetSize
    @@c.unsetSize
    assert_equal false, @@c.isSetSize
  end

  def test_Compartment_create
    assert_equal LibSBML::SBML_COMPARTMENT, @@c.getTypeCode
    assert_equal "", @@c.getMetaId
    assert_equal nil, @@c.getNotes
    assert_equal nil, @@c.getAnnotation
    assert_equal "", @@c.getId
    assert_equal "", @@c.getName
    assert_equal "", @@c.getUnits
    assert_equal "", @@c.getOutside
    assert_equal 3, @@c.getSpatialDimensions
    assert_equal 1.0, @@c.getVolume
    assert_equal true, @@c.getConstant
    assert_equal false, @@c.isSetId
    assert_equal false, @@c.isSetName
    assert_equal false, @@c.isSetSize
    assert_equal false, @@c.isSetVolume
    assert_equal false, @@c.isSetUnits
    assert_equal false, @@c.isSetOutside
  end

end
