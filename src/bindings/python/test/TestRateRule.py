#
# @file    TestRateRule.py
# @brief   RateRule unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestRateRule.c
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

class TestRateRule(unittest.TestCase):

  RR = None

  def setUp(self):
    self.RR = libsbml.RateRule()
    if (self.RR == None):
      pass    
    pass  

  def tearDown(self):
    self.RR = None
    pass  

  def test_RateRule_create(self):
    self.assert_( self.RR.getTypeCode() == libsbml.SBML_RATE_RULE )
    self.assert_( self.RR.getMetaId() == "" )
    self.assert_( self.RR.getNotes() == None )
    self.assert_( self.RR.getAnnotation() == None )
    self.assert_( self.RR.getFormula() == "" )
    self.assert_( self.RR.getMath() == None )
    self.assert_( self.RR.getVariable() == "" )
    self.assert_( self.RR.getType() == libsbml.RULE_TYPE_RATE )
    pass  

  def test_RateRule_createWithFormula(self):
    ar = libsbml.RateRule("s", "1 + 1")
    self.assert_( ar.getTypeCode() == libsbml.SBML_RATE_RULE )
    self.assert_( ar.getMetaId() == "" )
    self.assert_((  "s" == ar.getVariable() ))
    math = ar.getMath()
    self.assert_( math != None )
    formula = libsbml.formulaToString(math)
    self.assert_( formula != None )
    self.assert_((  "1 + 1" == formula ))
    self.assert_(( formula == ar.getFormula() ))
    ar = None
    pass  

  def test_RateRule_createWithMath(self):
    math = libsbml.parseFormula("1 + 1")
    ar = libsbml.RateRule("s",math)
    self.assert_( ar.getTypeCode() == libsbml.SBML_RATE_RULE )
    self.assert_( ar.getMetaId() == "" )
    self.assert_((  "s" == ar.getVariable() ))
    self.assert_((  "1 + 1" == ar.getFormula() ))
    self.assert_( ar.getMath() != math )
    ar = None
    pass  

  def test_RateRule_free_NULL(self):
    
    pass  

  def test_RateRule_setVariable(self):
    variable = "x"
    self.RR.setVariable(variable)
    self.assert_(( variable == self.RR.getVariable() ))
    self.assertEqual( True, self.RR.isSetVariable() )
    if (self.RR.getVariable() == variable):
      pass    
    self.RR.setVariable(self.RR.getVariable())
    self.assert_(( variable == self.RR.getVariable() ))
    self.RR.setVariable("")
    self.assertEqual( False, self.RR.isSetVariable() )
    if (self.RR.getVariable() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestRateRule))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
