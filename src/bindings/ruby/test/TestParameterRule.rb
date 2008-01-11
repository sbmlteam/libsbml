#
# This file was converted from libsbml/src/sbml/test/TestParameterRule.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestParameterRule < Test::Unit::TestCase

  def test_ParameterRule_free_NULL
  end

  def setup
    @@pr = LibSBML::AssignmentRule.new()
    @@pr.setL1TypeCode(LibSBML::SBML_PARAMETER_RULE)
      
  end

  def test_ParameterRule_setName
    name = "cell"
    @@pr.setVariable(name)
    assert_equal name,@@pr.getVariable
    assert_equal true, @@pr.isSetVariable
    c = @@pr.getVariable
    @@pr.setVariable(c)
    assert_equal name,@@pr.getVariable
    @@pr.setVariable("")
    assert_equal false, @@pr.isSetVariable
  end

  def test_ParameterRule_createWith
    pr = LibSBML::RateRule.new("c", "v + 1")
    pr.setL1TypeCode(LibSBML::SBML_PARAMETER_RULE)
    assert_equal LibSBML::SBML_RATE_RULE, pr.getTypeCode
    assert_equal LibSBML::SBML_PARAMETER_RULE, pr.getL1TypeCode
    assert_equal nil, pr.getNotes
    assert_equal nil, pr.getAnnotation
    assert_equal "", pr.getUnits
    assert_equal  "v + 1",pr.getFormula
    assert_equal  "c",pr.getVariable
    assert_equal LibSBML::RULE_TYPE_RATE, pr.getType
    assert_equal true, pr.isSetVariable
    assert_equal false, pr.isSetUnits
  end

  def test_ParameterRule_create
    assert_equal LibSBML::SBML_ASSIGNMENT_RULE, @@pr.getTypeCode
    assert_equal LibSBML::SBML_PARAMETER_RULE, @@pr.getL1TypeCode
    assert_equal nil, @@pr.getNotes
    assert_equal nil, @@pr.getAnnotation
    assert_equal "", @@pr.getFormula
    assert_equal "", @@pr.getUnits
    assert_equal "", @@pr.getVariable
    assert_equal LibSBML::RULE_TYPE_SCALAR, @@pr.getType
    assert_equal false, @@pr.isSetVariable
    assert_equal false, @@pr.isSetUnits
  end

  def test_ParameterRule_setUnits
    units = "cell"
    @@pr.setUnits(units)
    assert_equal units,@@pr.getUnits
    assert_equal true, @@pr.isSetUnits
      @@pr.setUnits(@@pr.getUnits)
      assert_equal units,@@pr.getUnits
      @@pr.setUnits("")
      assert_equal false, @@pr.isSetUnits
  end

end
