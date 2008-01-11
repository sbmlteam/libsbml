#
# This file was converted from libsbml/src/sbml/test/TestSpeciesConcentrationRule.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestSpeciesConcentrationRule < Test::Unit::TestCase

  def test_SpeciesConcentrationRule_setSpecies
    species = "s2"
    @@scr.setVariable(species)
    assert_equal species,@@scr.getVariable
    assert_equal true, @@scr.isSetVariable
    s = @@scr.getVariable
    @@scr.setVariable(s)
    assert_equal species,@@scr.getVariable
    @@scr.setVariable("")
    assert_equal false, @@scr.isSetVariable
  end

  def test_SpeciesConcentrationRule_createWith
    scr = LibSBML::RateRule.new("c", "v + 1")
    scr.setL1TypeCode(LibSBML::SBML_SPECIES_CONCENTRATION_RULE)
    assert_equal LibSBML::SBML_RATE_RULE, scr.getTypeCode
    assert_equal LibSBML::SBML_SPECIES_CONCENTRATION_RULE, scr.getL1TypeCode
    assert_equal nil, scr.getNotes
    assert_equal nil, scr.getAnnotation
    assert_equal  "v + 1",scr.getFormula
    assert_equal  "c",scr.getVariable
    assert_equal LibSBML::RULE_TYPE_RATE, scr.getType
    assert_equal true, scr.isSetVariable
  end

  def test_SpeciesConcentrationRule_free_NULL
  end

  def setup
    @@scr = LibSBML::AssignmentRule.new()
    @@scr.setL1TypeCode(LibSBML::SBML_SPECIES_CONCENTRATION_RULE)
      
  end

  def test_SpeciesConcentrationRule_create
    assert_equal LibSBML::SBML_ASSIGNMENT_RULE, @@scr.getTypeCode
    assert_equal LibSBML::SBML_SPECIES_CONCENTRATION_RULE, @@scr.getL1TypeCode
    assert_equal nil, @@scr.getNotes
    assert_equal nil, @@scr.getAnnotation
    assert_equal "", @@scr.getFormula
    assert_equal LibSBML::RULE_TYPE_SCALAR, @@scr.getType
    assert_equal "", @@scr.getVariable
    assert_equal false, @@scr.isSetVariable
  end

end
