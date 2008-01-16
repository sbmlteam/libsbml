#
# @file    TestParameterRule.py
# @brief   ParameterRule unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestParameterRule.c
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

class TestParameterRule(unittest.TestCase):

  PR = None

  def setUp(self):
    self.PR = libsbml.AssignmentRule()
    self.PR.setL1TypeCode(libsbml.SBML_PARAMETER_RULE)
    if (self.PR == None):
      pass    
    pass  

  def tearDown(self):
    self.PR = None
    pass  

  def test_ParameterRule_create(self):
    self.assert_( self.PR.getTypeCode() == libsbml.SBML_ASSIGNMENT_RULE )
    self.assert_( self.PR.getL1TypeCode() == libsbml.SBML_PARAMETER_RULE )
    self.assert_( self.PR.getNotes() == None )
    self.assert_( self.PR.getAnnotation() == None )
    self.assert_( self.PR.getFormula() == "" )
    self.assert_( self.PR.getUnits() == "" )
    self.assert_( self.PR.getVariable() == "" )
    self.assert_( self.PR.getType() == libsbml.RULE_TYPE_SCALAR )
    self.assertEqual( False, self.PR.isSetVariable() )
    self.assertEqual( False, self.PR.isSetUnits() )
    pass  

  def test_ParameterRule_createWith(self):
    pr = libsbml.RateRule("c", "v + 1")
    pr.setL1TypeCode(libsbml.SBML_PARAMETER_RULE)
    self.assert_( pr.getTypeCode() == libsbml.SBML_RATE_RULE )
    self.assert_( pr.getL1TypeCode() == libsbml.SBML_PARAMETER_RULE )
    self.assert_( pr.getNotes() == None )
    self.assert_( pr.getAnnotation() == None )
    self.assert_( pr.getUnits() == "" )
    self.assert_((  "v + 1" == pr.getFormula() ))
    self.assert_((  "c" == pr.getVariable() ))
    self.assert_( pr.getType() == libsbml.RULE_TYPE_RATE )
    self.assertEqual( True, pr.isSetVariable() )
    self.assertEqual( False, pr.isSetUnits() )
    pr = None
    pass  

  def test_ParameterRule_free_NULL(self):
    
    pass  

  def test_ParameterRule_setName(self):
    name = "cell"
    self.PR.setVariable(name)
    self.assert_(( name == self.PR.getVariable() ))
    self.assertEqual( True, self.PR.isSetVariable() )
    if (self.PR.getVariable() == name):
      pass    
    c = self.PR.getVariable()
    self.PR.setVariable(c)
    self.assert_(( name == self.PR.getVariable() ))
    self.PR.setVariable("")
    self.assertEqual( False, self.PR.isSetVariable() )
    if (self.PR.getVariable() != None):
      pass    
    pass  

  def test_ParameterRule_setUnits(self):
    units = "cell"
    self.PR.setUnits(units)
    self.assert_(( units == self.PR.getUnits() ))
    self.assertEqual( True, self.PR.isSetUnits() )
    if (self.PR.getUnits() == units):
      pass    
    self.PR.setUnits(self.PR.getUnits())
    self.assert_(( units == self.PR.getUnits() ))
    self.PR.setUnits("")
    self.assertEqual( False, self.PR.isSetUnits() )
    if (self.PR.getUnits() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestParameterRule))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
