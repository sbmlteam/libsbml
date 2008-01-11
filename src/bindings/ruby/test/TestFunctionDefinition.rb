#
# This file was converted from libsbml/src/sbml/test/TestFunctionDefinition.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestFunctionDefinition < Test::Unit::TestCase

  def test_FunctionDefinition_getArguments
    @@fd.setMath(LibSBML::parseFormula("lambda(x, y, x^y)"))
    assert_equal 2, @@fd.getNumArguments
    math = @@fd.getArgument(0)
    assert_not_equal nil, math
    assert_equal true, math.isName
    assert_equal  "x",math.getName
    assert_equal 0, math.getNumChildren
    math = @@fd.getArgument(1)
    assert_not_equal nil, math
    assert_equal true, math.isName
    assert_equal  "y",math.getName
    assert_equal 0, math.getNumChildren
    assert_equal @@fd.getArgument( "x"), @@fd.getArgument(0)
    assert_equal @@fd.getArgument( "y"), @@fd.getArgument(1)
  end

  def test_FunctionDefinition_setName
    name = "Cube Me"
    @@fd.setName(name)
    assert_equal name,@@fd.getName
    assert_equal true, @@fd.isSetName
      @@fd.setName(@@fd.getName)
      assert_equal name,@@fd.getName
      @@fd.setName("")
      assert_equal false, @@fd.isSetName
  end

  def test_FunctionDefinition_getBody
    math1 = LibSBML::parseFormula("lambda(x, x)")
    @@fd.setMath(math1)
    math = @@fd.getBody
    assert_not_equal nil, math
    assert_equal true, math.isName
    assert_equal  "x",math.getName
    assert_equal 0, math.getNumChildren
  end

  def test_FunctionDefinition_create
    assert_equal LibSBML::SBML_FUNCTION_DEFINITION, @@fd.getTypeCode
    assert_equal "", @@fd.getMetaId
    assert_equal nil, @@fd.getNotes
    assert_equal nil, @@fd.getAnnotation
    assert_equal "", @@fd.getId
    assert_equal "", @@fd.getName
    assert_equal nil, @@fd.getMath
  end

  def setup
    @@fd = LibSBML::FunctionDefinition.new
      
  end

  def test_FunctionDefinition_setId
    id = "pow3"
    @@fd.setId(id)
    assert_equal id,@@fd.getId
    assert_equal true, @@fd.isSetId
      @@fd.setId(@@fd.getId)
      assert_equal id,@@fd.getId
      @@fd.setId("")
      assert_equal false, @@fd.isSetId
  end

  def test_FunctionDefinition_free_NULL
  end

  def test_FunctionDefinition_createWith
    math = LibSBML::parseFormula("lambda(x, x^3)")
    fd = LibSBML::FunctionDefinition.new("pow3",math)
    assert_equal LibSBML::SBML_FUNCTION_DEFINITION, fd.getTypeCode
    assert_equal "", fd.getMetaId
    assert_equal nil, fd.getNotes
    assert_equal nil, fd.getAnnotation
    assert_equal "", fd.getName
    math1 = fd.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "lambda(x, x^3)",formula
    assert_not_equal math, fd.getMath
    assert_equal true, fd.isSetMath
    assert_equal  "pow3",fd.getId
    assert_equal true, fd.isSetId
  end

  def test_FunctionDefinition_setMath
    math = LibSBML::parseFormula("lambda(x, x^3)")
    @@fd.setMath(math)
    math1 = @@fd.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "lambda(x, x^3)",formula
    assert_not_equal math, @@fd.getMath
    assert_equal true, @@fd.isSetMath
    @@fd.setMath(@@fd.getMath)
    math1 = @@fd.getMath
    assert_not_equal nil, math1
    formula = LibSBML::formulaToString(math1)
    assert_not_equal nil, formula
    assert_equal  "lambda(x, x^3)",formula
    assert_not_equal math, @@fd.getMath
    @@fd.setMath(nil)
    assert_equal false, @@fd.isSetMath
  end

end
