#
# @file    TestEventAssignment.py
# @brief   SBML EventAssignment unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
# 
# ====== WARNING ===== WARNING ===== WARNING ===== WARNING ===== WARNING ======
#
# DO NOT EDIT THIS FILE.
#
# This file was generated automatically by converting the file located at
# src/sbml/test/TestEventAssignment.c
# using the conversion program dev/utilities/translateTests/translateTests.pl.
# Any changes made here will be lost the next time the file is regenerated.
#
# -----------------------------------------------------------------------------
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
# -----------------------------------------------------------------------------

import sys
import unittest
import libsbml


class TestEventAssignment(unittest.TestCase):

  global EA
  EA = None

  def setUp(self):
    self.EA = libsbml.EventAssignment(2,4)
    if (self.EA == None):
      pass    
    pass  

  def tearDown(self):
    _dummyList = [ self.EA ]; _dummyList[:] = []; del _dummyList
    pass  

  def test_EventAssignment_create(self):
    self.assertTrue( self.EA.getTypeCode() == libsbml.SBML_EVENT_ASSIGNMENT )
    self.assertTrue( self.EA.getMetaId() == "" )
    self.assertTrue( self.EA.getNotes() == None )
    self.assertTrue( self.EA.getAnnotation() == None )
    self.assertTrue( self.EA.getVariable() == "" )
    self.assertTrue( self.EA.getMath() == None )
    pass  

  def test_EventAssignment_createWithNS(self):
    xmlns = libsbml.XMLNamespaces()
    xmlns.add( "http://www.sbml.org", "testsbml")
    sbmlns = libsbml.SBMLNamespaces(2,1)
    sbmlns.addNamespaces(xmlns)
    object = libsbml.EventAssignment(sbmlns)
    self.assertTrue( object.getTypeCode() == libsbml.SBML_EVENT_ASSIGNMENT )
    self.assertTrue( object.getMetaId() == "" )
    self.assertTrue( object.getNotes() == None )
    self.assertTrue( object.getAnnotation() == None )
    self.assertTrue( object.getLevel() == 2 )
    self.assertTrue( object.getVersion() == 1 )
    self.assertTrue( object.getNamespaces() != None )
    self.assertTrue( object.getNamespaces().getLength() == 2 )
    _dummyList = [ object ]; _dummyList[:] = []; del _dummyList
    pass  

  def test_EventAssignment_free_NULL(self):
    _dummyList = [ None ]; _dummyList[:] = []; del _dummyList
    pass  

  def test_EventAssignment_setMath(self):
    math = libsbml.parseFormula("2 * k")
    self.EA.setMath(math)
    math1 = self.EA.getMath()
    self.assertTrue( math1 != None )
    formula = libsbml.formulaToString(math1)
    self.assertTrue( formula != None )
    self.assertTrue((  "2 * k" == formula ))
    self.assertTrue( self.EA.getMath() != math )
    self.assertEqual( True, self.EA.isSetMath() )
    self.EA.setMath(self.EA.getMath())
    math1 = self.EA.getMath()
    self.assertTrue( math1 != None )
    formula = libsbml.formulaToString(math1)
    self.assertTrue( formula != None )
    self.assertTrue((  "2 * k" == formula ))
    self.assertTrue( self.EA.getMath() != math )
    self.EA.setMath(None)
    self.assertEqual( False, self.EA.isSetMath() )
    if (self.EA.getMath() != None):
      pass    
    _dummyList = [ math ]; _dummyList[:] = []; del _dummyList
    pass  

  def test_EventAssignment_setVariable(self):
    variable =  "k2";
    self.EA.setVariable(variable)
    self.assertTrue(( variable == self.EA.getVariable() ))
    self.assertEqual( True, self.EA.isSetVariable() )
    if (self.EA.getVariable() == variable):
      pass    
    self.EA.setVariable(self.EA.getVariable())
    self.assertTrue(( variable == self.EA.getVariable() ))
    self.EA.setVariable("")
    self.assertEqual( False, self.EA.isSetVariable() )
    if (self.EA.getVariable() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.TestLoader.loadTestsFromTestCase(TestEventAssignment))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
