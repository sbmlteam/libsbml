#
# @file    TestSBMLTransforms.rb
# @brief   SBMLTransforms unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestSBMLTransforms.cpp
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2010 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#--------------------------------------------------------------------------->*/
require 'test/unit'
require 'libSBML'
include Math

class TestSBMLTransforms < Test::Unit::TestCase

  @@DBL_EPSILON =  2.2204460492503131e-16

  def equalDouble (a, b)
    return ((a-b).abs < sqrt(@@DBL_EPSILON))
  end

  def isnan(x)
    return (x != x)
  end

  def pow(x, y)
    return x ** y
  end

  def test_SBMLTransforms_evaluateAST
    node = LibSBML::ASTNode.new()
    node.setValue((2))
    assert( LibSBML::SBMLTransforms.evaluateASTNode(node) == 2 )
    node.setValue((3.2))
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),3.2)
    node.setValue((1),(4))
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.25)
    node.setValue((4.234),(2))
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),423.4)
    node.setType(LibSBML::AST_NAME_AVOGADRO)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),6.02214179e23)
    node.setType(LibSBML::AST_NAME_TIME)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.0)
    node.setType(LibSBML::AST_NAME)
    assert_equal true, isnan(LibSBML::SBMLTransforms.evaluateASTNode(node))
    node.setType(LibSBML::AST_CONSTANT_E)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),exp(1.0))
    node.setType(LibSBML::AST_CONSTANT_FALSE)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.0)
    node.setType(LibSBML::AST_CONSTANT_PI)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node), 4.0 * atan(1.0))
    node.setType(LibSBML::AST_CONSTANT_TRUE)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),1.0)
    node = LibSBML::parseFormula("2.5 + 6.1")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),8.6)
    node = LibSBML::parseFormula("-4.3")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node), - 4.3)
    node = LibSBML::parseFormula("9.2-4.3")
    temp = 9.2 - 4.3
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("2*3")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),6)
    node = LibSBML::parseFormula("1/5")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.2)
    node = LibSBML::parseFormula("pow(2, 3)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),8)
    node = LibSBML::parseFormula("3^3")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),27)
    node = LibSBML::parseFormula("abs(-9.456)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),9.456)
    node = LibSBML::parseFormula("ceil(9.456)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),10)
    node = LibSBML::parseFormula("exp(2.0)")
    temp = exp(2.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("floor(2.04567)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),2)
    node = LibSBML::parseFormula("ln(2.0)")
    temp = log(2.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("log10(100.0)")
    temp = log10(100.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("sin(2.0)")
    temp = sin(2.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("cos(4.1)")
    temp = cos(4.1)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("tan(0.345)")
    temp = tan(0.345)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arcsin(0.456)")
    temp = asin(0.456)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arccos(0.41)")
    temp = acos(0.41)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arctan(0.345)")
    temp = atan(0.345)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("sinh(2.0)")
    temp = sinh(2.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("cosh(4.1)")
    temp = cosh(4.1)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("tanh(0.345)")
    temp = tanh(0.345)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("and(1, 0)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.0)
    node = LibSBML::parseFormula("or(1, 0)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),1.0)
    node = LibSBML::parseFormula("not(1)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.0)
    node = LibSBML::parseFormula("xor(1, 0)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),1.0)
    node = LibSBML::parseFormula("xor(1, 1)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.0)
    node = LibSBML::parseFormula("eq(1, 2)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.0)
    node = LibSBML::parseFormula("eq(1, 1)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),1.0)
    node = LibSBML::parseFormula("geq(2,1)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),1.0)
    node = LibSBML::parseFormula("geq(2,4)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.0)
    node = LibSBML::parseFormula("geq(2,2)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),1.0)
    node = LibSBML::parseFormula("gt(2,1)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),1.0)
    node = LibSBML::parseFormula("gt(2,4)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.0)
    node = LibSBML::parseFormula("leq(2,1)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.0)
    node = LibSBML::parseFormula("leq(2,4)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),1.0)
    node = LibSBML::parseFormula("leq(2,2)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),1.0)
    node = LibSBML::parseFormula("lt(2,1)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.0)
    node = LibSBML::parseFormula("lt(2,4)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),1.0)
    node = LibSBML::parseFormula("neq(2,2)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),0.0)
    node = LibSBML::parseFormula("neq(3,2)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),1.0)
    node = LibSBML::parseFormula("cot(2.0)")
    temp = 1.0 / tan(2.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("csc(4.1)")
    temp = 1.0 / sin(4.1)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("sec(0.345)")
    temp = 1.0 / cos(0.345)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("coth(2.0)")
    temp = cosh(2.0) / sinh(2.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("sech(2.0)")
    temp = 1.0 / cosh(2.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("csch(2.0)")
    temp = 1.0 / sinh(2.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arccot(2.0)")
    temp = atan(1 / 2.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arccsc(2.0)")
    temp = asin(1 / 2.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arcsec(2.0)")
    temp = acos(1 / 2.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arccosh(2.0)")
    temp = log(2.0 + pow(3.0,0.5))
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arccoth(2.0)")
    temp = 0.5 * log(3.0)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arcsech(0.2)")
    temp = log(2 * pow(6,0.5) + 5)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arccsch(0.2)")
    temp = 2.312438341272753
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arcsinh(3.0)")
    temp = log(3.0 + pow(10.0,0.5))
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node = LibSBML::parseFormula("arctanh(0.2)")
    temp = 0.5 * log(1.5)
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),temp)
    node.setType(LibSBML::AST_FUNCTION_DELAY)
    assert_equal true, isnan(LibSBML::SBMLTransforms.evaluateASTNode(node))
    node = LibSBML::parseFormula("factorial(3)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),6)
    node.setType(LibSBML::AST_FUNCTION_PIECEWISE)
    assert_equal true, isnan(LibSBML::SBMLTransforms.evaluateASTNode(node))
    node = LibSBML::parseFormula("root(2, 4)")
    assert_equal true, equalDouble(LibSBML::SBMLTransforms.evaluateASTNode(node),2)
  end

  def test_SBMLTransforms_replaceFD
    reader = LibSBML::SBMLReader.new()
    filename = "../../sbml/test/test-data/"
    filename += "multiple-functions.xml"
    d = reader.readSBML(filename)
    if (d == nil)
    end
    m = d.getModel()
    assert( m.getNumFunctionDefinitions() == 2 )
    ast = m.getReaction(2).getKineticLaw().getMath()
    assert ((  "f(S1, p) * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    fd = m.getFunctionDefinition(0)
    LibSBML::SBMLTransforms.replaceFD(ast,fd)
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    ast = m.getReaction(1).getKineticLaw().getMath()
    assert ((  "f(f(S1, p), compartmentOne) / t" == LibSBML::formulaToString(ast) ))
    LibSBML::SBMLTransforms.replaceFD(ast,fd)
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    ast = m.getReaction(0).getKineticLaw().getMath()
    assert ((  "g(f(S1, p), compartmentOne) / t" == LibSBML::formulaToString(ast) ))
    LibSBML::SBMLTransforms.replaceFD(ast,fd)
    assert ((  "g(S1 * p, compartmentOne) / t" == LibSBML::formulaToString(ast) ))
    fd = m.getFunctionDefinition(1)
    LibSBML::SBMLTransforms.replaceFD(ast,fd)
    assert ((  "f(S1 * p, compartmentOne) / t" == LibSBML::formulaToString(ast) ))
    ast = m.getReaction(0).getKineticLaw().getMath()
    lofd = m.getListOfFunctionDefinitions()
    LibSBML::SBMLTransforms.replaceFD(ast,lofd)
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    d.expandFunctionDefinitions()
    assert( d.getModel().getNumFunctionDefinitions() == 0 )
    ast = d.getModel().getReaction(0).getKineticLaw().getMath()
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    ast = d.getModel().getReaction(1).getKineticLaw().getMath()
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    ast = d.getModel().getReaction(2).getKineticLaw().getMath()
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
  end

  def test_SBMLTransforms_replaceIA
    reader = LibSBML::SBMLReader.new()
    filename = "../../sbml/test/test-data/"
    filename += "initialAssignments.xml"
    d = reader.readSBML(filename)
    if (d == nil)
    end
    m = d.getModel()
    assert( m.getNumInitialAssignments() == 2 )
    assert_equal false, (m.getCompartment(0).isSetSize())
    assert( m.getParameter(1).getValue() == 2 )
    d.expandInitialAssignments()
    assert( d.getModel().getNumInitialAssignments() == 0 )
    assert_equal true, d.getModel().getCompartment(0).isSetSize()
    assert( d.getModel().getCompartment(0).getSize() == 25.0 )
    assert( m.getParameter(1).getValue() == 50 )
  end

  def test_SBMLTransforms_replaceIA_species
    reader = LibSBML::SBMLReader.new()
    filename = "../../sbml/test/test-data/"
    filename += "initialAssignments_species.xml"
    d = reader.readSBML(filename)
    if (d == nil)
    end
    m = d.getModel()
    assert( m.getNumInitialAssignments() == 3 )
    assert( m.getParameter(1).getValue() == 0.75 )
    assert_equal false, (m.getParameter(2).isSetValue())
    assert_equal true, m.getSpecies(2).isSetInitialAmount()
    assert( m.getSpecies(2).getInitialAmount() == 2 )
    d.expandInitialAssignments()
    assert( d.getModel().getNumInitialAssignments() == 0 )
    assert( m.getParameter(1).getValue() == 3 )
    assert_equal true, m.getParameter(2).isSetValue()
    assert( m.getParameter(2).getValue() == 0.75 )
    assert_equal false, (m.getSpecies(2).isSetInitialAmount())
    assert( m.getSpecies(2).getInitialConcentration() == 2 )
  end

end
