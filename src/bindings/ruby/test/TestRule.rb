#
# This file was converted from libsbml/src/sbml/test/TestRule.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestRule < Test::Unit::TestCase

  def test_Rule_setMath
    math = LibSBML::parseFormula("1 + 1")
    @@r.setMath(math)
    assert_not_equal math, @@r.getMath
    assert_equal true, @@r.isSetMath
    @@r.setMath(@@r.getMath)
    assert_not_equal math, @@r.getMath
    @@r.setMath(nil)
    assert_equal false, @@r.isSetMath
  end

  def setup
    @@r = LibSBML::AlgebraicRule.new()
      
  end

  def test_Rule_init
    assert_equal LibSBML::SBML_ALGEBRAIC_RULE, @@r.getTypeCode
    assert_equal "", @@r.getMetaId
    assert_equal nil, @@r.getNotes
    assert_equal nil, @@r.getAnnotation
    assert_equal "", @@r.getFormula
    assert_equal nil, @@r.getMath
  end

  def test_Rule_setFormula
    formula = "k1*X0"
    @@r.setFormula(formula)
    assert_equal formula,@@r.getFormula
    assert_equal true, @@r.isSetFormula
      @@r.setFormula(@@r.getFormula)
      assert_equal formula,@@r.getFormula
      @@r.setFormula( "")
      assert_equal false, @@r.isSetFormula
  end

end
