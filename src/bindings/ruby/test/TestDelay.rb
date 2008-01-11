#
# This file was converted from libsbml/src/sbml/test/TestDelay.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestDelay < Test::Unit::TestCase

  def test_Delay_create
    assert_equal LibSBML::SBML_DELAY, @@d.getTypeCode
    assert_equal "", @@d.getMetaId
    assert_equal nil, @@d.getNotes
    assert_equal nil, @@d.getAnnotation
    assert_equal nil, @@d.getMath
  end

  def test_Delay_setMath
    math = LibSBML::parseFormula("lambda(x, x^3)")
    @@d.setMath(math)
    math1 = @@d.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "lambda(x, x^3)",formula
    assert_not_equal math, @@d.getMath
    assert_equal true, @@d.isSetMath
    @@d.setMath(@@d.getMath)
    math1 = @@d.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "lambda(x, x^3)",formula
    @@d.setMath(nil)
    assert_equal false, @@d.isSetMath
  end

  def test_Delay_free_NULL
  end

  def setup
    @@d = LibSBML::Delay.new
      
  end

  def test_Delay_createWithMath
    math = LibSBML::parseFormula("x^3")
    fd = LibSBML::Delay.new(math)
    assert_equal LibSBML::SBML_DELAY, fd.getTypeCode
    assert_equal "", fd.getMetaId
    assert_equal nil, fd.getNotes
    assert_equal nil, fd.getAnnotation
    math1 = fd.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "x^3",formula
    assert_not_equal math, fd.getMath
    assert_equal true, fd.isSetMath
  end

end
