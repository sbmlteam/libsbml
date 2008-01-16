#
# @file    TestTrigger.py
# @brief   SBML Trigger unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestTrigger.c
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

class TestTrigger(unittest.TestCase):

  D = None

  def setUp(self):
    self.D = libsbml.Trigger()
    if (self.D == None):
      pass    
    pass  

  def tearDown(self):
    self.D = None
    pass  

  def test_Trigger_create(self):
    self.assert_( self.D.getTypeCode() == libsbml.SBML_TRIGGER )
    self.assert_( self.D.getMetaId() == "" )
    self.assert_( self.D.getNotes() == None )
    self.assert_( self.D.getAnnotation() == None )
    self.assert_( self.D.getMath() == None )
    pass  

  def test_Trigger_createWithMath(self):
    math = libsbml.parseFormula("x^3")
    fd = libsbml.Trigger(math)
    self.assert_( fd.getTypeCode() == libsbml.SBML_TRIGGER )
    self.assert_( fd.getMetaId() == "" )
    self.assert_( fd.getNotes() == None )
    self.assert_( fd.getAnnotation() == None )
    math1 = fd.getMath()
    self.assert_( math1 != None )
    formula = libsbml.formulaToString(math1)
    self.assert_( formula != None )
    self.assert_((  "x^3" == formula ))
    self.assert_( fd.getMath() != math )
    self.assertEqual( True, fd.isSetMath() )
    fd = None
    pass  

  def test_Trigger_free_NULL(self):
    
    pass  

  def test_Trigger_setMath(self):
    math = libsbml.parseFormula("lambda(x, x^3)")
    self.D.setMath(math)
    math1 = self.D.getMath()
    self.assert_( math1 != None )
    formula = libsbml.formulaToString(math1)
    self.assert_( formula != None )
    self.assert_((  "lambda(x, x^3)" == formula ))
    self.assert_( self.D.getMath() != math )
    self.assertEqual( True, self.D.isSetMath() )
    self.D.setMath(self.D.getMath())
    math1 = self.D.getMath()
    self.assert_( math1 != None )
    formula = libsbml.formulaToString(math1)
    self.assert_( formula != None )
    self.assert_((  "lambda(x, x^3)" == formula ))
    self.D.setMath(None)
    self.assertEqual( False, self.D.isSetMath() )
    if (self.D.getMath() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestTrigger))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
