#
# This file was converted from libsbml/src/sbml/test/TestInitialAssignment.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestInitialAssignment < Test::Unit::TestCase

  def test_InitialAssignment_setMath
    math = LibSBML::parseFormula("2 * k")
    @@ia.setMath(math)
    math1 = @@ia.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "2 * k",formula
    assert_not_equal math, @@ia.getMath
    assert_equal true, @@ia.isSetMath
    @@ia.setMath(@@ia.getMath)
    math1 = @@ia.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "2 * k",formula
    assert_not_equal math, @@ia.getMath
    @@ia.setMath(nil)
    assert_equal false, @@ia.isSetMath
  end

  def setup
    @@ia = LibSBML::InitialAssignment.new
      
  end

  def test_InitialAssignment_create
    assert_equal LibSBML::SBML_INITIAL_ASSIGNMENT, @@ia.getTypeCode
    assert_equal "", @@ia.getMetaId
    assert_equal nil, @@ia.getNotes
    assert_equal nil, @@ia.getAnnotation
    assert_equal "", @@ia.getSymbol
    assert_equal nil, @@ia.getMath
  end

  def test_InitialAssignment_setSymbol
    symbol = "k2"
    @@ia.setSymbol(symbol)
    assert_equal symbol,@@ia.getSymbol
    assert_equal true, @@ia.isSetSymbol
      @@ia.setSymbol(@@ia.getSymbol)
      assert_equal symbol,@@ia.getSymbol
      @@ia.setSymbol("")
      assert_equal false, @@ia.isSetSymbol
  end

  def test_InitialAssignment_createWith
    ia = LibSBML::InitialAssignment.new("k")
    assert_equal LibSBML::SBML_INITIAL_ASSIGNMENT, ia.getTypeCode
    assert_equal "", ia.getMetaId
    assert_equal nil, ia.getNotes
    assert_equal nil, ia.getAnnotation
    assert_equal false, ia.isSetMath
    assert_equal  "k",ia.getSymbol
    assert_equal true, ia.isSetSymbol
  end

  def test_InitialAssignment_free_NULL
  end

end
