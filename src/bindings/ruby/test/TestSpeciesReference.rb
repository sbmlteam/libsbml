#
# This file was converted from libsbml/src/sbml/test/TestSpeciesReference.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestSpeciesReference < Test::Unit::TestCase

  def test_SpeciesReference_setId
    species = "X0"
    @@sr.setId(species)
    assert_equal species,@@sr.getId
    assert_equal true, @@sr.isSetId
      @@sr.setId(@@sr.getId)
      assert_equal species,@@sr.getId
      @@sr.setId("")
      assert_equal false, @@sr.isSetId
  end

  def test_SpeciesReference_setSpecies
    species = "X0"
    @@sr.setSpecies(species)
    assert_equal species,@@sr.getSpecies
    assert_equal true, @@sr.isSetSpecies
      @@sr.setSpecies(@@sr.getSpecies)
      assert_equal species,@@sr.getSpecies
      @@sr.setSpecies("")
      assert_equal false, @@sr.isSetSpecies
  end

  def test_SpeciesReference_free_NULL
  end

  def test_SpeciesReference_create
    assert_equal LibSBML::SBML_SPECIES_REFERENCE, @@sr.getTypeCode
    assert_equal "", @@sr.getMetaId
    assert_equal nil, @@sr.getNotes
    assert_equal nil, @@sr.getAnnotation
    assert_equal "", @@sr.getSpecies
    assert_equal 1, @@sr.getStoichiometry
    assert_equal nil, @@sr.getStoichiometryMath
    assert_equal 1, @@sr.getDenominator
    assert_equal false, @@sr.isSetSpecies
    assert_equal false, @@sr.isSetStoichiometryMath
  end

  def test_SpeciesReference_createModifier
    sr = LibSBML::ModifierSpeciesReference.new()
    assert_equal LibSBML::SBML_MODIFIER_SPECIES_REFERENCE, sr.getTypeCode
    assert_equal "", sr.getMetaId
    assert_equal nil, sr.getNotes
    assert_equal nil, sr.getAnnotation
    assert_equal true, sr.isModifier
  end

  def test_SpeciesReference_setStoichiometryMath
    math = LibSBML::parseFormula("k3 / k2")
    stoich = LibSBML::StoichiometryMath.new(math)
    @@sr.setStoichiometryMath(stoich)
    math1 = @@sr.getStoichiometryMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1.getMath)
    assert_not_equal nil, formula
    assert_equal  "k3 / k2",formula
    assert_equal true, @@sr.isSetStoichiometryMath
  end

  def setup
    @@sr = LibSBML::SpeciesReference.new
      
  end

  def test_SpeciesReference_createWith
    sr = LibSBML::SpeciesReference.new("s3",4,2)
    assert_equal LibSBML::SBML_SPECIES_REFERENCE, sr.getTypeCode
    assert_equal "", sr.getMetaId
    assert_equal nil, sr.getNotes
    assert_equal nil, sr.getAnnotation
    assert_equal  "s3",sr.getSpecies
    assert_equal 4, sr.getStoichiometry
    assert_equal 2, sr.getDenominator
    assert_equal true, sr.isSetSpecies
  end

end
