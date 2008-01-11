#
# This file was converted from libsbml/src/sbml/test/TestAssignmentRule.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestAssignmentRule < Test::Unit::TestCase

  def test_AssignmentRule_L2_create
    assert_equal LibSBML::SBML_ASSIGNMENT_RULE, @@ar.getTypeCode
    assert_equal "", @@ar.getMetaId
    assert_equal nil, @@ar.getNotes
    assert_equal nil, @@ar.getAnnotation
    assert_equal "", @@ar.getFormula
    assert_equal nil, @@ar.getMath
    assert_equal "", @@ar.getVariable
    assert_equal LibSBML::RULE_TYPE_SCALAR, @@ar.getType
  end

  def test_AssignmentRule_createWithMath
    math = LibSBML::parseFormula("1 + 1")
    ar = LibSBML::AssignmentRule.new("s",math)
    assert_equal LibSBML::SBML_ASSIGNMENT_RULE, ar.getTypeCode
    assert_equal "", ar.getMetaId
    assert_equal  "s",ar.getVariable
    assert_equal  "1 + 1",ar.getFormula
    assert_not_equal math, ar.getMath
  end

  def test_AssignmentRule_createWithFormula
    ar = LibSBML::AssignmentRule.new("s", "1 + 1")
    assert_equal LibSBML::SBML_ASSIGNMENT_RULE, ar.getTypeCode
    assert_equal "", ar.getMetaId
    assert_equal  "s",ar.getVariable
    math = ar.getMath
    assert_not_equal nil, math
    formula = LibSBML::formulaToString(math)
    assert_not_equal nil, formula
    assert_equal  "1 + 1",formula
    assert_equal formula,ar.getFormula
  end

  def test_AssignmentRule_setVariable
    variable = "x"
    @@ar.setVariable(variable)
    assert_equal variable,@@ar.getVariable
    assert_equal true, @@ar.isSetVariable
      @@ar.setVariable(@@ar.getVariable)
      assert_equal variable,@@ar.getVariable
      @@ar.setVariable("")
      assert_equal false, @@ar.isSetVariable
  end

  def setup
    @@ar = LibSBML::AssignmentRule.new()
      
  end

  def test_AssignmentRule_free_NULL
  end

end
