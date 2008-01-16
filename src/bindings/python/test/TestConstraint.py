#
# @file    TestConstraint.py
# @brief   SBML Constraint unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestConstraint.c
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

class TestConstraint(unittest.TestCase):

  C = None

  def setUp(self):
    self.C = libsbml.Constraint()
    if (self.C == None):
      pass    
    pass  

  def tearDown(self):
    self.C = None
    pass  

  def test_Constraint_create(self):
    self.assert_( self.C.getTypeCode() == libsbml.SBML_CONSTRAINT )
    self.assert_( self.C.getMetaId() == "" )
    self.assert_( self.C.getNotes() == None )
    self.assert_( self.C.getAnnotation() == None )
    self.assertEqual( False, self.C.isSetMessage() )
    self.assertEqual( False, self.C.isSetMath() )
    pass  

  def test_Constraint_createWithMath(self):
    math = libsbml.parseFormula("1 + 1")
    c = libsbml.Constraint(math)
    self.assert_( c.getTypeCode() == libsbml.SBML_CONSTRAINT )
    self.assert_( c.getMetaId() == "" )
    self.assert_( c.getMath() != math )
    self.assertEqual( False, c.isSetMessage() )
    self.assertEqual( True, c.isSetMath() )
    c = None
    pass  

  def test_Constraint_free_NULL(self):
    
    pass  

  def test_Constraint_setMath(self):
    math = libsbml.parseFormula("2 * k")
    self.C.setMath(math)
    self.assert_( self.C.getMath() != math )
    self.assertEqual( True, self.C.isSetMath() )
    self.C.setMath(self.C.getMath())
    self.assert_( self.C.getMath() != math )
    self.C.setMath(None)
    self.assertEqual( False, self.C.isSetMath() )
    if (self.C.getMath() != None):
      pass    
    math = None
    pass  

  def test_Constraint_setMessage(self):
    node = libsbml.XMLNode()
    self.C.setMessage(node)
    self.assert_( self.C.getMessage() != node )
    self.assert_( self.C.isSetMessage() == True )
    self.C.setMessage(self.C.getMessage())
    self.assert_( self.C.getMessage() != node )
    self.C.unsetMessage()
    self.assertEqual( False, self.C.isSetMessage() )
    if (self.C.getMessage() != None):
      pass    
    node = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestConstraint))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
