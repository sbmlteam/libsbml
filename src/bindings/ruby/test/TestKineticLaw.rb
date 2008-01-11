#
# This file was converted from libsbml/src/sbml/test/TestKineticLaw.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestKineticLaw < Test::Unit::TestCase

  def test_KineticLaw_setFormulaFromMath
    math = LibSBML::parseFormula("k1 * X0")
    assert_equal false, @@kl.isSetMath
    assert_equal false, @@kl.isSetFormula
    @@kl.setMath(math)
    assert_equal true, @@kl.isSetMath
    assert_equal true, @@kl.isSetFormula
    assert_equal  "k1 * X0",@@kl.getFormula
  end

  def test_KineticLaw_addParameter
    p = LibSBML::Parameter.new
    @@kl.addParameter(p)
    assert_equal 1, @@kl.getNumParameters
  end

  def test_KineticLaw_createWith
    kl = LibSBML::KineticLaw.new("k1 * X0")
    assert_equal LibSBML::SBML_KINETIC_LAW, kl.getTypeCode
    assert_equal "", kl.getMetaId
    assert_equal nil, kl.getNotes
    assert_equal nil, kl.getAnnotation
    math = kl.getMath
    assert_not_equal nil, math
    formula = LibSBML::formulaToString(math)
    assert_not_equal nil, formula
    assert_equal  "k1 * X0",formula
    assert_equal formula,kl.getFormula
    assert_equal true, kl.isSetMath
    assert_equal true, kl.isSetFormula
    assert_equal 0, kl.getNumParameters
  end

  def test_KineticLaw_getParameter
    k1 = LibSBML::Parameter.new
    k2 = LibSBML::Parameter.new
    k1.setName( "k1")
    k2.setName( "k2")
    k1.setValue(3.14)
    k2.setValue(2.72)
    @@kl.addParameter(k1)
    @@kl.addParameter(k2)
    assert_equal 2, @@kl.getNumParameters
    k1 = @@kl.getParameter(0)
    k2 = @@kl.getParameter(1)
    assert_equal  "k1",k1.getName
    assert_equal  "k2",k2.getName
    assert_equal 3.14, k1.getValue
    assert_equal 2.72, k2.getValue
  end

  def test_KineticLaw_createWithMath
    math1 = LibSBML::parseFormula("k3 / k2")
    kl = LibSBML::KineticLaw.new(math1)
    assert_equal LibSBML::SBML_KINETIC_LAW, kl.getTypeCode
    assert_equal "", kl.getMetaId
    assert_equal nil, kl.getNotes
    assert_equal nil, kl.getAnnotation
    math = kl.getMath
    assert_not_equal nil, math
    formula = LibSBML::formulaToString(math)
    assert_not_equal nil, formula
    assert_equal  "k3 / k2",formula
    assert_equal formula,kl.getFormula
    assert_equal true, kl.isSetMath
    assert_equal true, kl.isSetFormula
    assert_equal false, kl.isSetTimeUnits
    assert_equal false, kl.isSetSubstanceUnits
    assert_equal 0, kl.getNumParameters
  end

  def test_KineticLaw_setMath
    math = LibSBML::parseFormula("k3 / k2")
    @@kl.setMath(math)
    math1 = @@kl.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "k3 / k2",formula
    assert_not_equal math, @@kl.getMath
    assert_equal true, @@kl.isSetMath
    @@kl.setMath(@@kl.getMath)
    math1 = @@kl.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "k3 / k2",formula
    assert_not_equal math, @@kl.getMath
    @@kl.setMath(nil)
    assert_equal false, @@kl.isSetMath
  end

  def setup
    @@kl = LibSBML::KineticLaw.new
      
  end

  def test_KineticLaw_create
    assert_equal LibSBML::SBML_KINETIC_LAW, @@kl.getTypeCode
    assert_equal "", @@kl.getMetaId
    assert_equal nil, @@kl.getNotes
    assert_equal nil, @@kl.getAnnotation
    assert_equal "", @@kl.getFormula
    assert_equal nil, @@kl.getMath
    assert_equal "", @@kl.getTimeUnits
    assert_equal "", @@kl.getSubstanceUnits
    assert_equal false, @@kl.isSetFormula
    assert_equal false, @@kl.isSetMath
    assert_equal false, @@kl.isSetTimeUnits
    assert_equal false, @@kl.isSetSubstanceUnits
    assert_equal 0, @@kl.getNumParameters
  end

  def test_KineticLaw_setFormula
    formula = "k1*X0"
    @@kl.setFormula(formula)
    assert_equal formula,@@kl.getFormula
    assert_equal true, @@kl.isSetFormula
      @@kl.setFormula(@@kl.getFormula)
      assert_equal formula,@@kl.getFormula
      @@kl.setFormula("")
      assert_equal false, @@kl.isSetFormula
  end

  def test_KineticLaw_setMathFromFormula
    formula = "k3 / k2"
    assert_equal false, @@kl.isSetMath
    assert_equal false, @@kl.isSetFormula
    @@kl.setFormula(formula)
    assert_equal true, @@kl.isSetMath
    assert_equal true, @@kl.isSetFormula
    formula = LibSBML::formulaToString(@@kl.getMath)
    assert_equal  "k3 / k2",formula
  end

  def test_KineticLaw_getParameterById
    k1 = LibSBML::Parameter.new
    k2 = LibSBML::Parameter.new
    k1.setId( "k1")
    k2.setId( "k2")
    k1.setValue(3.14)
    k2.setValue(2.72)
    @@kl.addParameter(k1)
    @@kl.addParameter(k2)
    assert_equal 2, @@kl.getNumParameters
    k1 = @@kl.getParameter( "k1")
    k2 = @@kl.getParameter( "k2")
    assert_equal  "k1",k1.getId
    assert_equal  "k2",k2.getId
    assert_equal 3.14, k1.getValue
    assert_equal 2.72, k2.getValue
  end

  def test_KineticLaw_free_NULL
  end

end
