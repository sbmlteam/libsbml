#
# @file    TestASTNode.rb
# @brief   ASTNode unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestASTNode.c
# wiht the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2008 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#--------------------------------------------------------------------------->*/
#
require 'test/unit'
require 'libSBML'

class TestASTNode < Test::Unit::TestCase

  @@DBL_EPSILON =  2.2204460492503131e-16

  def test_ASTNode_canonicalizeConstants
    n = LibSBML::ASTNode.new
    n.setName( "ExponentialE")
    assert_equal true, n.isName
    n.canonicalize
    assert( n.getType == LibSBML::AST_CONSTANT_E )
    n.setType(LibSBML::AST_NAME)
    n.setName( "False")
    assert_equal true, n.isName
    n.canonicalize
    assert( n.getType == LibSBML::AST_CONSTANT_FALSE )
    n.setType(LibSBML::AST_NAME)
    n.setName( "Pi")
    assert_equal true, n.isName
    n.canonicalize
    assert( n.getType == LibSBML::AST_CONSTANT_PI )
    n.setType(LibSBML::AST_NAME)
    n.setName( "True")
    assert_equal true, n.isName
    n.canonicalize
    assert( n.getType == LibSBML::AST_CONSTANT_TRUE )
    n.setType(LibSBML::AST_NAME)
    n.setName( "Foo")
    assert_equal true, n.isName
    n.canonicalize
    assert_equal true, n.isName
  end

  def test_ASTNode_canonicalizeFunctions
    n = LibSBML::ASTNode.new(LibSBML::AST_FUNCTION)
    n.setName( "abs")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ABS )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arccos")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCCOS )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arccosh")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCCOSH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arccot")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCCOT )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arccoth")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCCOTH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arccsc")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCCSC )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arccsch")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCCSCH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arcsec")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCSEC )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arcsech")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCSECH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arcsin")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCSIN )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arcsinh")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCSINH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arctan")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCTAN )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "arctanh")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCTANH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "ceiling")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_CEILING )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "cos")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_COS )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "cosh")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_COSH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "cot")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_COT )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "coth")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_COTH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "csc")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_CSC )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "csch")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_CSCH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "exp")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_EXP )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "factorial")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_FACTORIAL )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "floor")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_FLOOR )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "lambda")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_LAMBDA )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "ln")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_LN )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "log")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_LOG )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "piecewise")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_PIECEWISE )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "power")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_POWER )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "root")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ROOT )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "sec")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_SEC )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "sech")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_SECH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "sin")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_SIN )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "sinh")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_SINH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "tan")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_TAN )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "tanh")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_TANH )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "Foo")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION )
  end

  def test_ASTNode_canonicalizeFunctionsL1
    n = LibSBML::ASTNode.new(LibSBML::AST_FUNCTION)
    n.setName( "acos")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCCOS )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "asin")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCSIN )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "atan")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ARCTAN )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "ceil")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_CEILING )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "pow")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_POWER )
    n = LibSBML::ASTNode.new(LibSBML::AST_FUNCTION)
    n.setName( "log")
    c = LibSBML::ASTNode.new
    c.setName( "x")
    n.addChild(c)
    assert( n.getType == LibSBML::AST_FUNCTION )
    assert( n.getNumChildren == 1 )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_LN )
    assert( n.getNumChildren == 1 )
    n.setType(LibSBML::AST_FUNCTION)
    c = LibSBML::ASTNode.new
    c.setName( "y")
    n.addChild(c)
    assert( n.getType == LibSBML::AST_FUNCTION )
    assert( n.getNumChildren == 2 )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_LOG )
    n = LibSBML::ASTNode.new(LibSBML::AST_FUNCTION)
    n.setName( "log10")
    c = LibSBML::ASTNode.new
    c.setName( "x")
    n.addChild(c)
    assert( n.getType == LibSBML::AST_FUNCTION )
    assert( n.getNumChildren == 1 )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_LOG )
    assert( n.getNumChildren == 2 )
    c = n.getLeftChild
    assert( c.getType == LibSBML::AST_INTEGER )
    assert( c.getInteger == 10 )
    c = n.getRightChild
    assert( c.getType == LibSBML::AST_NAME )
    assert ((  "x" == c.getName ))
    n = LibSBML::ASTNode.new(LibSBML::AST_FUNCTION)
    n.setName( "sqr")
    c = LibSBML::ASTNode.new
    c.setName( "x")
    n.addChild(c)
    assert( n.getType == LibSBML::AST_FUNCTION )
    assert( n.getNumChildren == 1 )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_POWER )
    assert( n.getNumChildren == 2 )
    c = n.getLeftChild
    assert( c.getType == LibSBML::AST_NAME )
    assert ((  "x" == c.getName ))
    c = n.getRightChild
    assert( c.getType == LibSBML::AST_INTEGER )
    assert( c.getInteger == 2 )
    n = LibSBML::ASTNode.new(LibSBML::AST_FUNCTION)
    n.setName( "sqrt")
    c = LibSBML::ASTNode.new
    c.setName( "x")
    n.addChild(c)
    assert( n.getType == LibSBML::AST_FUNCTION )
    assert( n.getNumChildren == 1 )
    n.canonicalize
    assert( n.getType == LibSBML::AST_FUNCTION_ROOT )
    assert( n.getNumChildren == 2 )
    c = n.getLeftChild
    assert( c.getType == LibSBML::AST_INTEGER )
    assert( c.getInteger == 2 )
    c = n.getRightChild
    assert( c.getType == LibSBML::AST_NAME )
    assert ((  "x" == c.getName ))
  end

  def test_ASTNode_canonicalizeLogical
    n = LibSBML::ASTNode.new(LibSBML::AST_FUNCTION)
    n.setName( "and")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_LOGICAL_AND )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "not")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_LOGICAL_NOT )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "or")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_LOGICAL_OR )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "xor")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_LOGICAL_XOR )
    n.setType(LibSBML::AST_FUNCTION)
  end

  def test_ASTNode_canonicalizeRelational
    n = LibSBML::ASTNode.new(LibSBML::AST_FUNCTION)
    n.setName( "eq")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_RELATIONAL_EQ )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "geq")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_RELATIONAL_GEQ )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "gt")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_RELATIONAL_GT )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "leq")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_RELATIONAL_LEQ )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "lt")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_RELATIONAL_LT )
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "neq")
    assert( n.getType == LibSBML::AST_FUNCTION )
    n.canonicalize
    assert( n.getType == LibSBML::AST_RELATIONAL_NEQ )
    n.setType(LibSBML::AST_FUNCTION)
  end

  def test_ASTNode_children
    parent = LibSBML::ASTNode.new
    left = LibSBML::ASTNode.new
    right = LibSBML::ASTNode.new
    right2 = LibSBML::ASTNode.new
    parent.setType(LibSBML::AST_PLUS)
    left.setValue(1)
    right.setValue(2)
    right2.setValue(3)
    parent.addChild(left)
    parent.addChild(right)
    assert( parent.getNumChildren == 2 )
    assert( left.getNumChildren == 0 )
    assert( right.getNumChildren == 0 )
    assert( parent.getLeftChild == left )
    assert( parent.getRightChild == right )
    assert( parent.getChild(0) == left )
    assert( parent.getChild(1) == right )
    assert( parent.getChild(2) == nil )
    parent.addChild(right2)
    assert( parent.getNumChildren == 3 )
    assert( left.getNumChildren == 0 )
    assert( right.getNumChildren == 0 )
    assert( right2.getNumChildren == 0 )
    assert( parent.getLeftChild == left )
    assert( parent.getRightChild == right2 )
    assert( parent.getChild(0) == left )
    assert( parent.getChild(1) == right )
    assert( parent.getChild(2) == right2 )
    assert( parent.getChild(3) == nil )
  end

  def test_ASTNode_create
    n = LibSBML::ASTNode.new
    assert( n.getType == LibSBML::AST_UNKNOWN )
    assert( n.getCharacter == "\000" )
    assert( n.getName == nil )
    assert( n.getInteger == 0 )
    assert( n.getExponent == 0 )
    assert( n.getNumChildren == 0 )
  end

  def test_ASTNode_deepCopy_1
    node = LibSBML::ASTNode.new
    node.setCharacter( '+')
    node.addChild(LibSBML::ASTNode.new)
    node.addChild(LibSBML::ASTNode.new)
    node.getLeftChild.setValue(1)
    node.getRightChild.setValue(2)
    assert( node.getType == LibSBML::AST_PLUS )
    assert( node.getCharacter == '+' )
    assert( node.getNumChildren == 2 )
    child = node.getLeftChild
    assert( child.getType == LibSBML::AST_INTEGER )
    assert( child.getInteger == 1 )
    assert( child.getNumChildren == 0 )
    child = node.getRightChild
    assert( child.getType == LibSBML::AST_INTEGER )
    assert( child.getInteger == 2 )
    assert( child.getNumChildren == 0 )
    copy = node.deepCopy
    assert( copy != node )
    assert( copy.getType == LibSBML::AST_PLUS )
    assert( copy.getCharacter == '+' )
    assert( copy.getNumChildren == 2 )
    child = copy.getLeftChild
    assert( child != node.getLeftChild )
    assert( child.getType == LibSBML::AST_INTEGER )
    assert( child.getInteger == 1 )
    assert( child.getNumChildren == 0 )
    child = copy.getRightChild
    assert( child != node.getRightChild )
    assert( child.getType == LibSBML::AST_INTEGER )
    assert( child.getInteger == 2 )
    assert( child.getNumChildren == 0 )
  end

  def test_ASTNode_deepCopy_2
    node = LibSBML::ASTNode.new
    node.setName( "Foo")
    assert( node.getType == LibSBML::AST_NAME )
    assert ((  "Foo" == node.getName ))
    assert( node.getNumChildren == 0 )
    copy = node.deepCopy
    assert( copy != node )
    assert( copy.getType == LibSBML::AST_NAME )
    assert ((  "Foo" == copy.getName ))
    assert( copy.getNumChildren == 0 )
    assert( !copy.getName.equal?(node.getName) )
  end

  def test_ASTNode_deepCopy_3
    node = LibSBML::ASTNode.new(LibSBML::AST_FUNCTION)
    node.setName( "Foo")
    assert( node.getType == LibSBML::AST_FUNCTION )
    assert ((  "Foo" == node.getName ))
    assert( node.getNumChildren == 0 )
    copy = node.deepCopy
    assert( copy != node )
    assert( copy.getType == LibSBML::AST_FUNCTION )
    assert ((  "Foo" == copy.getName ))
    assert( copy.getNumChildren == 0 )
    assert( !copy.getName.equal?(node.getName) )
  end

  def test_ASTNode_deepCopy_4
    node = LibSBML::ASTNode.new(LibSBML::AST_FUNCTION_ABS)
    node.setName( "ABS")
    assert( node.getType == LibSBML::AST_FUNCTION_ABS )
    assert ((  "ABS" == node.getName ))
    assert( node.getNumChildren == 0 )
    copy = node.deepCopy
    assert( copy != node )
    assert( copy.getType == LibSBML::AST_FUNCTION_ABS )
    assert ((  "ABS" == copy.getName ))
    assert( copy.getNumChildren == 0 )
    assert( !copy.getName.equal?(node.getName) )
  end

  def test_ASTNode_free_NULL
  end

  def test_ASTNode_getName
    n = LibSBML::ASTNode.new
    n.setName( "foo")
    assert ((  "foo" == n.getName ))
    n.setType(LibSBML::AST_NAME_TIME)
    assert ((  "foo" == n.getName ))
    n.setName(nil)
    assert( n.getName == nil )
    n.setType(LibSBML::AST_CONSTANT_E)
    assert ((  "exponentiale" == n.getName ))
    n.setType(LibSBML::AST_CONSTANT_FALSE)
    assert ((  "false" == n.getName ))
    n.setType(LibSBML::AST_CONSTANT_PI)
    assert ((  "pi" == n.getName ))
    n.setType(LibSBML::AST_CONSTANT_TRUE)
    assert ((  "true" == n.getName ))
    n.setType(LibSBML::AST_LAMBDA)
    assert ((  "lambda" == n.getName ))
    n.setType(LibSBML::AST_FUNCTION)
    n.setName( "f")
    assert ((  "f" == n.getName ))
    n.setType(LibSBML::AST_FUNCTION_DELAY)
    assert ((  "f" == n.getName ))
    n.setName(nil)
    assert ((  "delay" == n.getName ))
    n.setType(LibSBML::AST_FUNCTION)
    assert( n.getName == nil )
    n.setType(LibSBML::AST_FUNCTION_ABS)
    assert ((  "abs" == n.getName ))
    n.setType(LibSBML::AST_FUNCTION_ARCCOS)
    assert ((  "arccos" == n.getName ))
    n.setType(LibSBML::AST_FUNCTION_TAN)
    assert ((  "tan" == n.getName ))
    n.setType(LibSBML::AST_FUNCTION_TANH)
    assert ((  "tanh" == n.getName ))
    n.setType(LibSBML::AST_LOGICAL_AND)
    assert ((  "and" == n.getName ))
    n.setType(LibSBML::AST_LOGICAL_NOT)
    assert ((  "not" == n.getName ))
    n.setType(LibSBML::AST_LOGICAL_OR)
    assert ((  "or" == n.getName ))
    n.setType(LibSBML::AST_LOGICAL_XOR)
    assert ((  "xor" == n.getName ))
    n.setType(LibSBML::AST_RELATIONAL_EQ)
    assert ((  "eq" == n.getName ))
    n.setType(LibSBML::AST_RELATIONAL_GEQ)
    assert ((  "geq" == n.getName ))
    n.setType(LibSBML::AST_RELATIONAL_LT)
    assert ((  "lt" == n.getName ))
    n.setType(LibSBML::AST_RELATIONAL_NEQ)
    assert ((  "neq" == n.getName ))
  end

  def test_ASTNode_getPrecedence
    n = LibSBML::ASTNode.new
    n.setType(LibSBML::AST_PLUS)
    assert( n.getPrecedence == 2 )
    n.setType(LibSBML::AST_MINUS)
    assert( n.getPrecedence == 2 )
    n.setType(LibSBML::AST_TIMES)
    assert( n.getPrecedence == 3 )
    n.setType(LibSBML::AST_DIVIDE)
    assert( n.getPrecedence == 3 )
    n.setType(LibSBML::AST_POWER)
    assert( n.getPrecedence == 4 )
    n.setType(LibSBML::AST_MINUS)
    n.addChild(LibSBML::ASTNode.new(LibSBML::AST_NAME))
    assert( n.isUMinus == true )
    assert( n.getPrecedence == 5 )
    n.setType(LibSBML::AST_NAME)
    assert( n.getPrecedence == 6 )
    n.setType(LibSBML::AST_FUNCTION)
    assert( n.getPrecedence == 6 )
  end

  def test_ASTNode_getReal
    n = LibSBML::ASTNode.new
    n.setType(LibSBML::AST_REAL)
    n.setValue(1.6)
    assert( n.getReal == 1.6 )
    n.setType(LibSBML::AST_REAL_E)
    n.setValue(12.3,3)
    assert( (n.getReal - 12300.0).abs  < @@DBL_EPSILON )
    n.setType(LibSBML::AST_RATIONAL)
    n.setValue(1,2)
    assert( n.getReal == 0.5 )
  end

  def test_ASTNode_isLog10
    n = LibSBML::ASTNode.new
    n.setType(LibSBML::AST_FUNCTION)
    assert( n.isLog10 == false )
    n.setType(LibSBML::AST_FUNCTION_LOG)
    assert( n.isLog10 == false )
    c = LibSBML::ASTNode.new
    n.addChild(c)
    c.setValue(10)
    assert( n.isLog10 == false )
    n.addChild(LibSBML::ASTNode.new)
    assert( n.isLog10 == true )
    c.setValue(2)
    assert( n.isLog10 == false )
  end

  def test_ASTNode_isSqrt
    n = LibSBML::ASTNode.new
    n.setType(LibSBML::AST_FUNCTION)
    assert( n.isSqrt == false )
    n.setType(LibSBML::AST_FUNCTION_ROOT)
    assert( n.isSqrt == false )
    c = LibSBML::ASTNode.new
    n.addChild(c)
    c.setValue(2)
    assert( n.isSqrt == false )
    n.addChild(LibSBML::ASTNode.new)
    assert( n.isSqrt == true )
    c.setValue(3)
    assert( n.isSqrt == false )
  end

  def test_ASTNode_isUMinus
    n = LibSBML::ASTNode.new
    n.setType(LibSBML::AST_MINUS)
    assert( n.isUMinus == false )
    n.addChild(LibSBML::ASTNode.new(LibSBML::AST_NAME))
    assert( n.isUMinus == true )
  end

  def test_ASTNode_no_children
    node = LibSBML::ASTNode.new
    assert( node.getNumChildren == 0 )
    assert( node.getLeftChild == nil )
    assert( node.getRightChild == nil )
    assert( node.getChild(0) == nil )
  end

  def test_ASTNode_one_child
    node = LibSBML::ASTNode.new
    child = LibSBML::ASTNode.new
    node.addChild(child)
    assert( node.getNumChildren == 1 )
    assert( node.getLeftChild == child )
    assert( node.getRightChild == nil )
    assert( node.getChild(0) == child )
    assert( node.getChild(1) == nil )
  end

  def test_ASTNode_replaceArgument
    node = LibSBML::ASTNode.new
    c1 = LibSBML::ASTNode.new
    c2 = LibSBML::ASTNode.new
    arg = LibSBML::ASTNode.new
    bvar = "foo"
    node.setType(LibSBML::AST_PLUS)
    c1.setName( "foo")
    c2.setName( "foo2")
    node.addChild(c1)
    node.addChild(c2)
    assert ((  "foo" == node.getChild(0).getName ))
    arg.setName( "repl")
    node.ReplaceArgument(bvar,arg)
    assert ((  "repl" == node.getChild(0).getName ))
  end

  def test_ASTNode_setCharacter
    node = LibSBML::ASTNode.new
    node.setName( "foo")
    assert( node.getType == LibSBML::AST_NAME )
    node.setCharacter( '+')
    assert( node.getType == LibSBML::AST_PLUS )
    assert( node.getCharacter == '+' )
    node.setCharacter( '-')
    assert( node.getType == LibSBML::AST_MINUS )
    assert( node.getCharacter == '-' )
    node.setCharacter( '*')
    assert( node.getType == LibSBML::AST_TIMES )
    assert( node.getCharacter == '*' )
    node.setCharacter( '/')
    assert( node.getType == LibSBML::AST_DIVIDE )
    assert( node.getCharacter == '/' )
    node.setCharacter( '^')
    assert( node.getType == LibSBML::AST_POWER )
    assert( node.getCharacter == '^' )
    node.setCharacter( '$')
    assert( node.getType == LibSBML::AST_UNKNOWN )
    assert( node.getCharacter == '$' )
  end

  def test_ASTNode_setInteger
    node = LibSBML::ASTNode.new
    node.setName( "foo")
    assert( node.getType == LibSBML::AST_NAME )
    node.setValue(321)
    assert( node.getType == LibSBML::AST_INTEGER )
    assert( node.getInteger == 321 )
  end

  def test_ASTNode_setName
    name = "foo"
    node = LibSBML::ASTNode.new
    assert( node.getType == LibSBML::AST_UNKNOWN )
    node.setName(name)
    assert( node.getType == LibSBML::AST_NAME )
    assert (( name == node.getName ))
    if (node.getName == name)
    end
    node.setName(nil)
    assert( node.getType == LibSBML::AST_NAME )
    if (node.getName != nil)
    end
  end

  def test_ASTNode_setName_override
    node = LibSBML::ASTNode.new(LibSBML::AST_FUNCTION_SIN)
    assert ((  "sin" == node.getName ))
    assert( node.getType == LibSBML::AST_FUNCTION_SIN )
    node.setName( "MySinFunc")
    assert ((  "MySinFunc" == node.getName ))
    assert( node.getType == LibSBML::AST_FUNCTION_SIN )
    node.setName(nil)
    assert ((  "sin" == node.getName ))
    assert( node.getType == LibSBML::AST_FUNCTION_SIN )
  end

  def test_ASTNode_setReal
    node = LibSBML::ASTNode.new
    node.setName( "foo")
    assert( node.getType == LibSBML::AST_NAME )
    node.setValue(32.1)
    assert( node.getType == LibSBML::AST_REAL )
    assert( node.getReal == 32.1 )
  end

  def test_ASTNode_setType
    node = LibSBML::ASTNode.new
    node.setName( "foo")
    assert( node.getType == LibSBML::AST_NAME )
    node.setType(LibSBML::AST_FUNCTION)
    assert( node.getType == LibSBML::AST_FUNCTION )
    assert ((  "foo" == node.getName ))
    node.setType(LibSBML::AST_NAME)
    assert( node.getType == LibSBML::AST_NAME )
    assert ((  "foo" == node.getName ))
    node.setType(LibSBML::AST_INTEGER)
    assert( node.getType == LibSBML::AST_INTEGER )
    node.setType(LibSBML::AST_REAL)
    assert( node.getType == LibSBML::AST_REAL )
    node.setType(LibSBML::AST_UNKNOWN)
    assert( node.getType == LibSBML::AST_UNKNOWN )
    node.setType(LibSBML::AST_PLUS)
    assert( node.getType == LibSBML::AST_PLUS )
    assert( node.getCharacter == '+' )
    node.setType(LibSBML::AST_MINUS)
    assert( node.getType == LibSBML::AST_MINUS )
    assert( node.getCharacter == '-' )
    node.setType(LibSBML::AST_TIMES)
    assert( node.getType == LibSBML::AST_TIMES )
    assert( node.getCharacter == '*' )
    node.setType(LibSBML::AST_DIVIDE)
    assert( node.getType == LibSBML::AST_DIVIDE )
    assert( node.getCharacter == '/' )
    node.setType(LibSBML::AST_POWER)
    assert( node.getType == LibSBML::AST_POWER )
    assert( node.getCharacter == '^' )
  end

end
