#
# This file was converted from libsbml/src/sbml/test/TestRateRule.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestRateRule < Test::Unit::TestCase

  def test_RateRule_createWithMath
    math = LibSBML::parseFormula("1 + 1")
    ar = LibSBML::RateRule.new("s",math)
    assert_equal LibSBML::SBML_RATE_RULE, ar.getTypeCode
    assert_equal "", ar.getMetaId
    assert_equal  "s",ar.getVariable
    assert_equal  "1 + 1",ar.getFormula
    assert_not_equal math, ar.getMath
  end

  def test_RateRule_setVariable
    variable = "x"
    @@rr.setVariable(variable)
    assert_equal variable,@@rr.getVariable
    assert_equal true, @@rr.isSetVariable
      @@rr.setVariable(@@rr.getVariable)
      assert_equal variable,@@rr.getVariable
      @@rr.setVariable("")
      assert_equal false, @@rr.isSetVariable
  end

  def test_RateRule_free_NULL
  end

  def test_RateRule_create
    assert_equal LibSBML::SBML_RATE_RULE, @@rr.getTypeCode
    assert_equal "", @@rr.getMetaId
    assert_equal nil, @@rr.getNotes
    assert_equal nil, @@rr.getAnnotation
    assert_equal "", @@rr.getFormula
    assert_equal nil, @@rr.getMath
    assert_equal "", @@rr.getVariable
    assert_equal LibSBML::RULE_TYPE_RATE, @@rr.getType
  end

  def test_RateRule_createWithFormula
    ar = LibSBML::RateRule.new("s", "1 + 1")
    assert_equal LibSBML::SBML_RATE_RULE, ar.getTypeCode
    assert_equal "", ar.getMetaId
    assert_equal  "s",ar.getVariable
    math = ar.getMath
    assert_not_equal nil, math
    formula = LibSBML::formulaToString(math)
    assert_not_equal nil, formula
    assert_equal  "1 + 1",formula
    assert_equal formula,ar.getFormula
  end

  def setup
    @@rr = LibSBML::RateRule.new()
      
  end

end
