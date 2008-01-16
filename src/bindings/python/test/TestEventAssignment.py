#
# @file    TestEventAssignment.py
# @brief   SBML EventAssignment unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestEventAssignment.c
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

class TestEventAssignment(unittest.TestCase):

  EA = None

  def setUp(self):
    self.EA = libsbml.EventAssignment()
    if (self.EA == None):
      pass    
    pass  

  def tearDown(self):
    self.EA = None
    pass  

  def test_EventAssignment_create(self):
    self.assert_( self.EA.getTypeCode() == libsbml.SBML_EVENT_ASSIGNMENT )
    self.assert_( self.EA.getMetaId() == "" )
    self.assert_( self.EA.getNotes() == None )
    self.assert_( self.EA.getAnnotation() == None )
    self.assert_( self.EA.getVariable() == "" )
    self.assert_( self.EA.getMath() == None )
    pass  

  def test_EventAssignment_createWith(self):
    math = libsbml.parseFormula("0")
    ea = libsbml.EventAssignment("k",math)
    self.assert_( ea.getTypeCode() == libsbml.SBML_EVENT_ASSIGNMENT )
    self.assert_( ea.getMetaId() == "" )
    self.assert_( ea.getNotes() == None )
    self.assert_( ea.getAnnotation() == None )
    self.assert_( ea.getMath() != math )
    self.assertEqual( True, ea.isSetMath() )
    self.assert_((  "k" == ea.getVariable() ))
    self.assertEqual( True, ea.isSetVariable() )
    math = None
    ea = None
    pass  

  def test_EventAssignment_free_NULL(self):
    
    pass  

  def test_EventAssignment_setMath(self):
    math = libsbml.parseFormula("2 * k")
    self.EA.setMath(math)
    math1 = self.EA.getMath()
    self.assert_( math1 != None )
    formula = libsbml.formulaToString(math1)
    self.assert_( formula != None )
    self.assert_((  "2 * k" == formula ))
    self.assert_( self.EA.getMath() != math )
    self.assertEqual( True, self.EA.isSetMath() )
    self.EA.setMath(self.EA.getMath())
    math1 = self.EA.getMath()
    self.assert_( math1 != None )
    formula = libsbml.formulaToString(math1)
    self.assert_( formula != None )
    self.assert_((  "2 * k" == formula ))
    self.assert_( self.EA.getMath() != math )
    self.EA.setMath(None)
    self.assertEqual( False, self.EA.isSetMath() )
    if (self.EA.getMath() != None):
      pass    
    math = None
    pass  

  def test_EventAssignment_setVariable(self):
    variable = "k2"
    self.EA.setVariable(variable)
    self.assert_(( variable == self.EA.getVariable() ))
    self.assertEqual( True, self.EA.isSetVariable() )
    if (self.EA.getVariable() == variable):
      pass    
    self.EA.setVariable(self.EA.getVariable())
    self.assert_(( variable == self.EA.getVariable() ))
    self.EA.setVariable("")
    self.assertEqual( False, self.EA.isSetVariable() )
    if (self.EA.getVariable() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestEventAssignment))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
