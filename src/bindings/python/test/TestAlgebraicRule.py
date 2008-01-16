#
# @file    TestAlgebraicRule.py
# @brief   AlgebraicRule unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestAlgebraicRule.c
# with the help of conversion sciprt (ctest_converter.pl).
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
import sys
import unittest
import libsbml

class TestAlgebraicRule(unittest.TestCase):

  AR = None

  def setUp(self):
    self.AR = libsbml.AlgebraicRule()
    if (self.AR == None):
      pass    
    pass  

  def tearDown(self):
    self.AR = None
    pass  

  def test_AlgebraicRule_create(self):
    self.assert_( self.AR.getTypeCode() == libsbml.SBML_ALGEBRAIC_RULE )
    self.assert_( self.AR.getMetaId() == "" )
    self.assert_( self.AR.getNotes() == None )
    self.assert_( self.AR.getAnnotation() == None )
    self.assert_( self.AR.getFormula() == "" )
    self.assert_( self.AR.getMath() == None )
    pass  

  def test_AlgebraicRule_createWithFormula(self):
    ar = libsbml.AlgebraicRule("1 + 1")
    self.assert_( ar.getTypeCode() == libsbml.SBML_ALGEBRAIC_RULE )
    self.assert_( ar.getMetaId() == "" )
    math = ar.getMath()
    self.assert_( math != None )
    formula = libsbml.formulaToString(math)
    self.assert_( formula != None )
    self.assert_((  "1 + 1" == formula ))
    self.assert_(( formula == ar.getFormula() ))
    ar = None
    pass  

  def test_AlgebraicRule_createWithMath(self):
    math = libsbml.parseFormula("1 + 1")
    ar = libsbml.AlgebraicRule(math)
    self.assert_( ar.getTypeCode() == libsbml.SBML_ALGEBRAIC_RULE )
    self.assert_( ar.getMetaId() == "" )
    self.assert_((  "1 + 1" == ar.getFormula() ))
    self.assert_( ar.getMath() != math )
    ar = None
    pass  

  def test_AlgebraicRule_free_NULL(self):
    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestAlgebraicRule))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
