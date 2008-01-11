#
# This file was converted from libsbml/src/sbml/test/TestConstraint.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestConstraint < Test::Unit::TestCase

  def test_Constraint_free_NULL
  end

  def test_Constraint_create
    assert_equal LibSBML::SBML_CONSTRAINT, @@c.getTypeCode
    assert_equal "", @@c.getMetaId
    assert_equal nil, @@c.getNotes
    assert_equal nil, @@c.getAnnotation
    assert_equal false, @@c.isSetMessage
    assert_equal false, @@c.isSetMath
  end

  def test_Constraint_setMath
    math = LibSBML::parseFormula("2 * k")
    @@c.setMath(math)
    assert_not_equal math, @@c.getMath
    assert_equal true, @@c.isSetMath
    @@c.setMath(@@c.getMath)
    assert_not_equal math, @@c.getMath
    @@c.setMath(nil)
    assert_equal false, @@c.isSetMath
  end

  def setup
    @@c = LibSBML::Constraint.new
      
  end

  def test_Constraint_setMessage
    node = LibSBML::XMLNode.new
    @@c.setMessage(node)
    assert_not_equal node, @@c.getMessage
    assert_equal true, @@c.isSetMessage
    @@c.setMessage(@@c.getMessage)
    assert_not_equal node, @@c.getMessage
    @@c.unsetMessage
    assert_equal false, @@c.isSetMessage
  end

  def test_Constraint_createWithMath
    math = LibSBML::parseFormula("1 + 1")
    c = LibSBML::Constraint.new(math)
    assert_equal LibSBML::SBML_CONSTRAINT, c.getTypeCode
    assert_equal "", c.getMetaId
    assert_not_equal math, c.getMath
    assert_equal false, c.isSetMessage
    assert_equal true, c.isSetMath
  end

end
