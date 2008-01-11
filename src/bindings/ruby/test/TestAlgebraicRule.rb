#
# This file was converted from libsbml/src/sbml/test/TestAlgebraicRule.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestAlgebraicRule < Test::Unit::TestCase

  def setup
    @@ar = LibSBML::AlgebraicRule.new()
      
  end

  def test_AlgebraicRule_createWithFormula
    ar = LibSBML::AlgebraicRule.new("1 + 1")
    assert_equal LibSBML::SBML_ALGEBRAIC_RULE, ar.getTypeCode
    assert_equal "", ar.getMetaId
    math = ar.getMath
    assert_not_equal nil, math
    formula = LibSBML::formulaToString(math)
    assert_not_equal nil, formula
    assert_equal  "1 + 1",formula
    assert_equal formula,ar.getFormula
  end

  def test_AlgebraicRule_createWithMath
    math = LibSBML::parseFormula("1 + 1")
    ar = LibSBML::AlgebraicRule.new(math)
    assert_equal LibSBML::SBML_ALGEBRAIC_RULE, ar.getTypeCode
    assert_equal "", ar.getMetaId
    assert_equal  "1 + 1",ar.getFormula
    assert_not_equal math, ar.getMath
  end

  def test_AlgebraicRule_free_NULL
  end

  def test_AlgebraicRule_create
    assert_equal LibSBML::SBML_ALGEBRAIC_RULE, @@ar.getTypeCode
    assert_equal "", @@ar.getMetaId
    assert_equal nil, @@ar.getNotes
    assert_equal nil, @@ar.getAnnotation
    assert_equal "", @@ar.getFormula
    assert_equal nil, @@ar.getMath
  end

end
