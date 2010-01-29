#
# @file    TestUnit_newSetters.py
# @brief   Unit unit tests for new set function API
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestUnit_newSetters.c
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

class TestUnit_newSetters(unittest.TestCase):

  U = None

  def setUp(self):
    self.U = libsbml.Unit(1,2)
    if (self.U == None):
      pass    
    pass  

  def tearDown(self):
    self.U = None
    pass  

  def test_Unit_removeScale(self):
    i = self.U.setScale(2)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( self.U.getScale() == 2 )
    i = libsbml.Unit.removeScale(self.U)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( self.U.getScale() == 0 )
    self.assert_( self.U.getMultiplier() == 100 )
    pass  

  def test_Unit_setExponent1(self):
    i = self.U.setExponent(2)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( self.U.getExponent() == 2 )
    pass  

  def test_Unit_setKind1(self):
    i = self.U.setKind(libsbml.UnitKind_forName("cell"))
    self.assert_( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE )
    self.assertEqual( False, self.U.isSetKind() )
    pass  

  def test_Unit_setKind2(self):
    i = self.U.setKind(libsbml.UnitKind_forName("litre"))
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, self.U.isSetKind() )
    pass  

  def test_Unit_setMultiplier1(self):
    i = self.U.setMultiplier(2)
    self.assert_( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE )
    self.assert_( self.U.getMultiplier() == 2 )
    pass  

  def test_Unit_setMultiplier2(self):
    c = libsbml.Unit(2,2)
    i = c.setMultiplier(4)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( c.getMultiplier() == 4 )
    c = None
    pass  

  def test_Unit_setOffset1(self):
    i = self.U.setOffset(2.0)
    self.assert_( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE )
    self.assert_( self.U.getOffset() == 0 )
    pass  

  def test_Unit_setOffset2(self):
    U1 = libsbml.Unit(2,1)
    i = U1.setOffset(2.0)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( U1.getOffset() == 2 )
    pass  

  def test_Unit_setScale1(self):
    i = self.U.setScale(2)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( self.U.getScale() == 2 )
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestUnit_newSetters))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
