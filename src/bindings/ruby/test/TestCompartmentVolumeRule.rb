#
# This file was converted from libsbml/src/sbml/test/TestCompartmentVolumeRule.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestCompartmentVolumeRule < Test::Unit::TestCase

  def test_CompartmentVolumeRule_createWith
    cvr = LibSBML::RateRule.new("c", "v + 1")
    cvr.setL1TypeCode(LibSBML::SBML_COMPARTMENT_VOLUME_RULE)
    assert_equal LibSBML::SBML_RATE_RULE, cvr.getTypeCode
    assert_equal LibSBML::SBML_COMPARTMENT_VOLUME_RULE, cvr.getL1TypeCode
    assert_equal nil, cvr.getNotes
    assert_equal nil, cvr.getAnnotation
    assert_equal  "v + 1",cvr.getFormula
    assert_equal  "c",cvr.getVariable
    assert_equal LibSBML::RULE_TYPE_RATE, cvr.getType
    assert_equal true, cvr.isSetVariable
  end

  def test_CompartmentVolumeRule_create
    assert_equal LibSBML::SBML_ASSIGNMENT_RULE, @@cvr.getTypeCode
    assert_equal LibSBML::SBML_COMPARTMENT_VOLUME_RULE, @@cvr.getL1TypeCode
    assert_equal nil, @@cvr.getNotes
    assert_equal nil, @@cvr.getAnnotation
    assert_equal "", @@cvr.getFormula
    assert_equal LibSBML::RULE_TYPE_SCALAR, @@cvr.getType
    assert_equal "", @@cvr.getVariable
    assert_equal false, @@cvr.isSetVariable
  end

  def test_CompartmentVolumeRule_setCompartment
    compartment = "cell"
    @@cvr.setVariable(compartment)
    assert_equal compartment,@@cvr.getVariable
    assert_equal true, @@cvr.isSetVariable
    c = @@cvr.getVariable
    @@cvr.setVariable(c)
    assert_equal compartment,@@cvr.getVariable
    @@cvr.setVariable("")
    assert_equal false, @@cvr.isSetVariable
  end

  def test_CompartmentVolumeRule_free_NULL
  end

  def setup
    @@cvr = LibSBML::AssignmentRule.new()
    @@cvr.setL1TypeCode(LibSBML::SBML_COMPARTMENT_VOLUME_RULE)
      
  end

end
