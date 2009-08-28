#
# @file    TestValidASTNode.py
# @brief   Test the isWellFormedASTNode function
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestValidASTNode.cpp
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2009 California Institute of Technology.
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

class TestValidASTNode(unittest.TestCase):


  def test_ValidASTNode_Name(self):
    N = libsbml.parseFormula("c")
    self.assertEqual( True, N.isWellFormedASTNode() )
    d = libsbml.parseFormula("d")
    N.addChild(d)
    self.assertEqual( False, (N.isWellFormedASTNode()) )
    N = None
    pass  

  def test_ValidASTNode_Number(self):
    N = libsbml.parseFormula("1.2")
    self.assertEqual( True, N.isWellFormedASTNode() )
    d = libsbml.parseFormula("d")
    N.addChild(d)
    self.assertEqual( False, (N.isWellFormedASTNode()) )
    N = None
    pass  

  def test_ValidASTNode_binary(self):
    N = libsbml.ASTNode(libsbml.AST_DIVIDE)
    self.assertEqual( False, (N.isWellFormedASTNode()) )
    c = libsbml.parseFormula("c")
    N.addChild(c)
    self.assertEqual( False, (N.isWellFormedASTNode()) )
    d = libsbml.parseFormula("d")
    N.addChild(d)
    self.assertEqual( True, N.isWellFormedASTNode() )
    N = None
    pass  

  def test_ValidASTNode_lambda(self):
    N = libsbml.ASTNode(libsbml.AST_LAMBDA)
    self.assertEqual( False, (N.isWellFormedASTNode()) )
    c = libsbml.parseFormula("c")
    N.addChild(c)
    self.assertEqual( True, N.isWellFormedASTNode() )
    d = libsbml.parseFormula("d")
    N.addChild(d)
    self.assertEqual( True, N.isWellFormedASTNode() )
    e = libsbml.parseFormula("e")
    N.addChild(e)
    self.assertEqual( True, N.isWellFormedASTNode() )
    N = None
    pass  

  def test_ValidASTNode_nary(self):
    N = libsbml.ASTNode(libsbml.AST_TIMES)
    self.assertEqual( False, (N.isWellFormedASTNode()) )
    c = libsbml.parseFormula("c")
    N.addChild(c)
    self.assertEqual( False, (N.isWellFormedASTNode()) )
    d = libsbml.parseFormula("d")
    N.addChild(d)
    self.assertEqual( True, N.isWellFormedASTNode() )
    e = libsbml.parseFormula("e")
    N.addChild(e)
    self.assertEqual( True, N.isWellFormedASTNode() )
    N = None
    pass  

  def test_ValidASTNode_root(self):
    N = libsbml.ASTNode(libsbml.AST_FUNCTION_ROOT)
    self.assertEqual( False, (N.isWellFormedASTNode()) )
    c = libsbml.parseFormula("c")
    N.addChild(c)
    self.assertEqual( True, N.isWellFormedASTNode() )
    d = libsbml.parseFormula("3")
    N.addChild(d)
    self.assertEqual( True, N.isWellFormedASTNode() )
    e = libsbml.parseFormula("3")
    N.addChild(e)
    self.assertEqual( False, (N.isWellFormedASTNode()) )
    N = None
    pass  

  def test_ValidASTNode_setType(self):
    N = libsbml.ASTNode()
    i = N.setType(libsbml.AST_REAL)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( N.getType() == libsbml.AST_REAL )
    i = N.setType(libsbml.AST_PLUS)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( N.getType() == libsbml.AST_PLUS )
    c = N.getCharacter()
    self.assert_(("+"  == c ))
    i = N.setType(libsbml.AST_FUNCTION_ARCCOSH)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( N.getType() == libsbml.AST_FUNCTION_ARCCOSH )
    i = N.setType(libsbml.AST_UNKNOWN)
    self.assert_( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE )
    self.assert_( N.getType() == libsbml.AST_UNKNOWN )
    N = None
    pass  

  def test_ValidASTNode_unary(self):
    N = libsbml.ASTNode(libsbml.AST_FUNCTION_ABS)
    self.assertEqual( False, (N.isWellFormedASTNode()) )
    c = libsbml.parseFormula("c")
    N.addChild(c)
    self.assertEqual( True, N.isWellFormedASTNode() )
    d = libsbml.parseFormula("d")
    N.addChild(d)
    self.assertEqual( False, (N.isWellFormedASTNode()) )
    N = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestValidASTNode))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
