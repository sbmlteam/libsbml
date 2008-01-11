#
# This file was converted from libsbml/src/sbml/test/TestSpecies.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestSpecies < Test::Unit::TestCase

  def test_Species_setInitialAmount
    assert_equal false, @@s.isSetInitialAmount
    assert_equal false, @@s.isSetInitialConcentration
    @@s.setInitialAmount(1.2)
    assert_equal true, @@s.isSetInitialAmount
    assert_equal false, @@s.isSetInitialConcentration
    assert_equal 1.2, @@s.getInitialAmount
  end

  def test_Species_setSubstanceUnits
    units = "item"
    @@s.setSubstanceUnits(units)
    assert_equal units,@@s.getSubstanceUnits
    assert_equal true, @@s.isSetSubstanceUnits
      @@s.setSubstanceUnits(@@s.getSubstanceUnits)
      assert_equal units,@@s.getSubstanceUnits
      @@s.setSubstanceUnits("")
      assert_equal false, @@s.isSetSubstanceUnits
  end

  def test_Species_setCompartment
    compartment = "cell"
    @@s.setCompartment(compartment)
    assert_equal compartment,@@s.getCompartment
    assert_equal true, @@s.isSetCompartment
      @@s.setCompartment(@@s.getCompartment)
      assert_equal compartment,@@s.getCompartment
      @@s.setCompartment("")
      assert_equal false, @@s.isSetCompartment
  end

  def test_Species_setId
    id = "Glucose"
    @@s.setId(id)
    assert_equal id,@@s.getId
    assert_equal true, @@s.isSetId
      @@s.setId(@@s.getId)
      assert_equal id,@@s.getId
      @@s.setId("")
      assert_equal false, @@s.isSetId
  end

  def test_Species_setSpatialSizeUnits
    units = "volume"
    @@s.setSpatialSizeUnits(units)
    assert_equal units,@@s.getSpatialSizeUnits
    assert_equal true, @@s.isSetSpatialSizeUnits
      @@s.setSpatialSizeUnits(@@s.getSpatialSizeUnits)
      assert_equal units,@@s.getSpatialSizeUnits
      @@s.setSpatialSizeUnits("")
      assert_equal false, @@s.isSetSpatialSizeUnits
  end

  def test_Species_create
    assert_equal LibSBML::SBML_SPECIES, @@s.getTypeCode
    assert_equal "", @@s.getMetaId
    assert_equal nil, @@s.getNotes
    assert_equal nil, @@s.getAnnotation
    assert_equal "", @@s.getId
    assert_equal "", @@s.getName
    assert_equal "", @@s.getCompartment
    assert_equal 0.0, @@s.getInitialAmount
    assert_equal 0.0, @@s.getInitialConcentration
    assert_equal "", @@s.getSubstanceUnits
    assert_equal "", @@s.getSpatialSizeUnits
    assert_equal false, @@s.getHasOnlySubstanceUnits
    assert_equal false, @@s.getBoundaryCondition
    assert_equal 0, @@s.getCharge
    assert_equal false, @@s.getConstant
    assert_equal false, @@s.isSetId
    assert_equal false, @@s.isSetName
    assert_equal false, @@s.isSetCompartment
    assert_equal false, @@s.isSetInitialAmount
    assert_equal false, @@s.isSetInitialConcentration
    assert_equal false, @@s.isSetSubstanceUnits
    assert_equal false, @@s.isSetSpatialSizeUnits
    assert_equal false, @@s.isSetUnits
    assert_equal false, @@s.isSetCharge
  end

  def setup
    @@s = LibSBML::Species.new
      
  end

  def test_Species_free_NULL
  end

  def test_Species_setName
    name = "So Sweet"
    @@s.setName(name)
    assert_equal name,@@s.getName
    assert_equal true, @@s.isSetName
      @@s.setName(@@s.getName)
      assert_equal name,@@s.getName
      @@s.setName("")
      assert_equal false, @@s.isSetName
  end

  def test_Species_setInitialConcentration
    assert_equal false, @@s.isSetInitialAmount
    assert_equal false, @@s.isSetInitialConcentration
    @@s.setInitialConcentration(3.4)
    assert_equal false, @@s.isSetInitialAmount
    assert_equal true, @@s.isSetInitialConcentration
    assert_equal 3.4, @@s.getInitialConcentration
  end

  def test_Species_createWith
    s = LibSBML::Species.new("Ca", "Calcium")
    assert_equal LibSBML::SBML_SPECIES, s.getTypeCode
    assert_equal "", s.getMetaId
    assert_equal nil, s.getNotes
    assert_equal nil, s.getAnnotation
    assert_equal  "Calcium"  ,s.getName
    assert_equal "", s.getSpatialSizeUnits
    assert_equal false, s.getHasOnlySubstanceUnits
    assert_equal false, s.getConstant
    assert_equal  "Ca"  ,s.getId
    assert_equal true, s.isSetId
    assert_equal true, s.isSetName
    assert_equal false, s.isSetCompartment
    assert_equal false, s.isSetSubstanceUnits
    assert_equal false, s.isSetSpatialSizeUnits
    assert_equal false, s.isSetUnits
    assert_equal false, s.isSetInitialAmount
    assert_equal false, s.isSetInitialConcentration
    assert_equal false, s.isSetCharge
  end

  def test_Species_setUnits
    units = "mole"
    @@s.setUnits(units)
    assert_equal units,@@s.getUnits
    assert_equal true, @@s.isSetUnits
      @@s.setUnits(@@s.getSubstanceUnits)
      assert_equal units,@@s.getUnits
      @@s.setUnits("")
      assert_equal false, @@s.isSetUnits
  end

end
