#
# This file was converted from libsbml/src/sbml/test/TestEventAssignment.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestEventAssignment < Test::Unit::TestCase

  def test_EventAssignment_createWith
    math = LibSBML::parseFormula("0")
    ea = LibSBML::EventAssignment.new("k",math)
    assert_equal LibSBML::SBML_EVENT_ASSIGNMENT, ea.getTypeCode
    assert_equal "", ea.getMetaId
    assert_equal nil, ea.getNotes
    assert_equal nil, ea.getAnnotation
    assert_not_equal math, ea.getMath
    assert_equal true, ea.isSetMath
    assert_equal  "k",ea.getVariable
    assert_equal true, ea.isSetVariable
  end

  def test_EventAssignment_free_NULL
  end

  def test_EventAssignment_create
    assert_equal LibSBML::SBML_EVENT_ASSIGNMENT, @@ea.getTypeCode
    assert_equal "", @@ea.getMetaId
    assert_equal nil, @@ea.getNotes
    assert_equal nil, @@ea.getAnnotation
    assert_equal "", @@ea.getVariable
    assert_equal nil, @@ea.getMath
  end

  def setup
    @@ea = LibSBML::EventAssignment.new
      
  end

  def test_EventAssignment_setMath
    math = LibSBML::parseFormula("2 * k")
    @@ea.setMath(math)
    math1 = @@ea.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "2 * k",formula
    assert_not_equal math, @@ea.getMath
    assert_equal true, @@ea.isSetMath
    @@ea.setMath(@@ea.getMath)
    math1 = @@ea.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "2 * k",formula
    assert_not_equal math, @@ea.getMath
    @@ea.setMath(nil)
    assert_equal false, @@ea.isSetMath
  end

  def test_EventAssignment_setVariable
    variable = "k2"
    @@ea.setVariable(variable)
    assert_equal variable,@@ea.getVariable
    assert_equal true, @@ea.isSetVariable
      @@ea.setVariable(@@ea.getVariable)
      assert_equal variable,@@ea.getVariable
      @@ea.setVariable("")
      assert_equal false, @@ea.isSetVariable
  end

end
