#
# This file was converted from libsbml/src/sbml/test/TestModifierSpeciesReference.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestModifierSpeciesReference < Test::Unit::TestCase

  def setup
    @@msr = LibSBML::ModifierSpeciesReference.new()
      
  end

  def test_ModifierSpeciesReference_free_NULL
  end

  def test_ModifierSpeciesReference_create
    assert_equal LibSBML::SBML_MODIFIER_SPECIES_REFERENCE, @@msr.getTypeCode
    assert_equal "", @@msr.getMetaId
    assert_equal nil, @@msr.getNotes
    assert_equal nil, @@msr.getAnnotation
    assert_equal "", @@msr.getSpecies
    assert_equal false, @@msr.isSetSpecies
    assert_equal true, @@msr.isModifier
  end

  def test_ModifierSpeciesReference_setSpecies
    species = "s1"
    @@msr.setSpecies(species)
    s = @@msr.getSpecies
    assert_equal species,s
    assert_equal true, @@msr.isSetSpecies
    s = @@msr.getSpecies
    @@msr.setSpecies(s)
    s = @@msr.getSpecies
    assert_equal species,s
    @@msr.setSpecies("")
    assert_equal false, @@msr.isSetSpecies
  end

end
