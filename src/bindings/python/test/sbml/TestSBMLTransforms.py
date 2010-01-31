#
# @file    TestSBMLTransforms.py
# @brief   SBMLTransforms unit tests
#
# @author  Akiya Jouraku (Python conversion)
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
import sys
import unittest
import libsbml
import math

DBL_EPSILON =  2.2204460492503131e-16

def equalDouble (a, b):
  return (abs(a-b) < math.sqrt(DBL_EPSILON))

def isnan(x):
  return (x != x)
  pass

class TestSBMLTransforms(unittest.TestCase):

  def test_SBMLTransforms_evaluateAST(self):
    node = libsbml.ASTNode()
    node.setValue((2))
    self.assert_( libsbml.SBMLTransforms.evaluateASTNode(node) == 2 )
    node.setValue((3.2))
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),3.2) )
    node.setValue((1),(4))
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.25) )
    node.setValue((4.234),(2))
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),423.4) )
    node.setType(libsbml.AST_NAME_AVOGADRO)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),6.02214179e23) )
    node.setType(libsbml.AST_NAME_TIME)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.0) )
    node.setType(libsbml.AST_NAME)
    self.assertEqual( True, isnan(libsbml.SBMLTransforms.evaluateASTNode(node)) )
    node.setType(libsbml.AST_CONSTANT_E)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),math.exp(1.0)) )
    node.setType(libsbml.AST_CONSTANT_FALSE)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.0) )
    node.setType(libsbml.AST_CONSTANT_PI)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node), 4.0 * math.atan(1.0)) )
    node.setType(libsbml.AST_CONSTANT_TRUE)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),1.0) )
    node = libsbml.parseFormula("2.5 + 6.1")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),8.6) )
    node = libsbml.parseFormula("-4.3")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node), - 4.3) )
    node = libsbml.parseFormula("9.2-4.3")
    temp = 9.2 - 4.3
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("2*3")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),6) )
    node = libsbml.parseFormula("1/5")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.2) )
    node = libsbml.parseFormula("pow(2, 3)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),8) )
    node = libsbml.parseFormula("3^3")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),27) )
    node = libsbml.parseFormula("abs(-9.456)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),9.456) )
    node = libsbml.parseFormula("ceil(9.456)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),10) )
    node = libsbml.parseFormula("exp(2.0)")
    temp = math.exp(2.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("floor(2.04567)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),2) )
    node = libsbml.parseFormula("ln(2.0)")
    temp = math.log(2.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("log10(100.0)")
    temp = math.log10(100.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("sin(2.0)")
    temp = math.sin(2.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("cos(4.1)")
    temp = math.cos(4.1)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("tan(0.345)")
    temp = math.tan(0.345)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arcsin(0.456)")
    temp = math.asin(0.456)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arccos(0.41)")
    temp = math.acos(0.41)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arctan(0.345)")
    temp = math.atan(0.345)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("sinh(2.0)")
    temp = math.sinh(2.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("cosh(4.1)")
    temp = math.cosh(4.1)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("tanh(0.345)")
    temp = math.tanh(0.345)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("and(1, 0)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.0) )
    node = libsbml.parseFormula("or(1, 0)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),1.0) )
    node = libsbml.parseFormula("not(1)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.0) )
    node = libsbml.parseFormula("xor(1, 0)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),1.0) )
    node = libsbml.parseFormula("xor(1, 1)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.0) )
    node = libsbml.parseFormula("eq(1, 2)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.0) )
    node = libsbml.parseFormula("eq(1, 1)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),1.0) )
    node = libsbml.parseFormula("geq(2,1)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),1.0) )
    node = libsbml.parseFormula("geq(2,4)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.0) )
    node = libsbml.parseFormula("geq(2,2)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),1.0) )
    node = libsbml.parseFormula("gt(2,1)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),1.0) )
    node = libsbml.parseFormula("gt(2,4)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.0) )
    node = libsbml.parseFormula("leq(2,1)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.0) )
    node = libsbml.parseFormula("leq(2,4)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),1.0) )
    node = libsbml.parseFormula("leq(2,2)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),1.0) )
    node = libsbml.parseFormula("lt(2,1)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.0) )
    node = libsbml.parseFormula("lt(2,4)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),1.0) )
    node = libsbml.parseFormula("neq(2,2)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),0.0) )
    node = libsbml.parseFormula("neq(3,2)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),1.0) )
    node = libsbml.parseFormula("cot(2.0)")
    temp = 1.0 / math.tan(2.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("csc(4.1)")
    temp = 1.0 / math.sin(4.1)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("sec(0.345)")
    temp = 1.0 / math.cos(0.345)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("coth(2.0)")
    temp = math.cosh(2.0) / math.sinh(2.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("sech(2.0)")
    temp = 1.0 / math.cosh(2.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("csch(2.0)")
    temp = 1.0 / math.sinh(2.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arccot(2.0)")
    temp = math.atan(1 / 2.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arccsc(2.0)")
    temp = math.asin(1 / 2.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arcsec(2.0)")
    temp = math.acos(1 / 2.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arccosh(2.0)")
    temp = math.log(2.0 + pow(3.0,0.5))
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arccoth(2.0)")
    temp = 0.5 * math.log(3.0)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arcsech(0.2)")
    temp = math.log(2 * pow(6,0.5) + 5)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arccsch(0.2)")
    temp = 2.312438341272753
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arcsinh(3.0)")
    temp = math.log(3.0 + pow(10.0,0.5))
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node = libsbml.parseFormula("arctanh(0.2)")
    temp = 0.5 * math.log(1.5)
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),temp) )
    node.setType(libsbml.AST_FUNCTION_DELAY)
    self.assertEqual( True, isnan(libsbml.SBMLTransforms.evaluateASTNode(node)) )
    node = libsbml.parseFormula("factorial(3)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),6) )
    node.setType(libsbml.AST_FUNCTION_PIECEWISE)
    self.assertEqual( True, isnan(libsbml.SBMLTransforms.evaluateASTNode(node)) )
    node = libsbml.parseFormula("root(2, 4)")
    self.assertEqual( True, equalDouble(libsbml.SBMLTransforms.evaluateASTNode(node),2) )
    pass  

  def test_SBMLTransforms_replaceFD(self):
    reader = libsbml.SBMLReader()
    filename = "../../sbml/test/test-data/"
    filename += "multiple-functions.xml"
    d = reader.readSBML(filename)
    if (d == None):
      pass    
    m = d.getModel()
    self.assert_( m.getNumFunctionDefinitions() == 2 )
    ast = m.getReaction(2).getKineticLaw().getMath()
    self.assert_((  "f(S1, p) * compartmentOne / t" == libsbml.formulaToString(ast) ))
    fd = m.getFunctionDefinition(0)
    libsbml.SBMLTransforms.replaceFD(ast,fd)
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    ast = m.getReaction(1).getKineticLaw().getMath()
    self.assert_((  "f(f(S1, p), compartmentOne) / t" == libsbml.formulaToString(ast) ))
    libsbml.SBMLTransforms.replaceFD(ast,fd)
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    ast = m.getReaction(0).getKineticLaw().getMath()
    self.assert_((  "g(f(S1, p), compartmentOne) / t" == libsbml.formulaToString(ast) ))
    libsbml.SBMLTransforms.replaceFD(ast,fd)
    self.assert_((  "g(S1 * p, compartmentOne) / t" == libsbml.formulaToString(ast) ))
    fd = m.getFunctionDefinition(1)
    libsbml.SBMLTransforms.replaceFD(ast,fd)
    self.assert_((  "f(S1 * p, compartmentOne) / t" == libsbml.formulaToString(ast) ))
    ast = m.getReaction(0).getKineticLaw().getMath()
    lofd = m.getListOfFunctionDefinitions()
    libsbml.SBMLTransforms.replaceFD(ast,lofd)
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    d.expandFunctionDefinitions()
    self.assert_( d.getModel().getNumFunctionDefinitions() == 0 )
    ast = d.getModel().getReaction(0).getKineticLaw().getMath()
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    ast = d.getModel().getReaction(1).getKineticLaw().getMath()
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    ast = d.getModel().getReaction(2).getKineticLaw().getMath()
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    pass  

  def test_SBMLTransforms_replaceIA(self):
    reader = libsbml.SBMLReader()
    filename = "../../sbml/test/test-data/"
    filename += "initialAssignments.xml"
    d = reader.readSBML(filename)
    if (d == None):
      pass    
    m = d.getModel()
    self.assert_( m.getNumInitialAssignments() == 2 )
    self.assertEqual( False, (m.getCompartment(0).isSetSize()) )
    self.assert_( m.getParameter(1).getValue() == 2 )
    d.expandInitialAssignments()
    self.assert_( d.getModel().getNumInitialAssignments() == 0 )
    self.assertEqual( True, d.getModel().getCompartment(0).isSetSize() )
    self.assert_( d.getModel().getCompartment(0).getSize() == 25.0 )
    self.assert_( m.getParameter(1).getValue() == 50 )
    pass  

  def test_SBMLTransforms_replaceIA_species(self):
    reader = libsbml.SBMLReader()
    filename = "../../sbml/test/test-data/"
    filename += "initialAssignments_species.xml"
    d = reader.readSBML(filename)
    if (d == None):
      pass    
    m = d.getModel()
    self.assert_( m.getNumInitialAssignments() == 3 )
    self.assert_( m.getParameter(1).getValue() == 0.75 )
    self.assertEqual( False, (m.getParameter(2).isSetValue()) )
    self.assertEqual( True, m.getSpecies(2).isSetInitialAmount() )
    self.assert_( m.getSpecies(2).getInitialAmount() == 2 )
    d.expandInitialAssignments()
    self.assert_( d.getModel().getNumInitialAssignments() == 0 )
    self.assert_( m.getParameter(1).getValue() == 3 )
    self.assertEqual( True, m.getParameter(2).isSetValue() )
    self.assert_( m.getParameter(2).getValue() == 0.75 )
    self.assertEqual( False, (m.getSpecies(2).isSetInitialAmount()) )
    self.assert_( m.getSpecies(2).getInitialConcentration() == 2 )
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestSBMLTransforms))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
