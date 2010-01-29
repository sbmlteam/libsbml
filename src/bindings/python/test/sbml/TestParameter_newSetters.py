#
# @file    TestParameter_newSetters.py
# @brief   Parameter unit tests for new set function API
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestParameter_newSetters.c
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

class TestParameter_newSetters(unittest.TestCase):

  P = None

  def setUp(self):
    self.P = libsbml.Parameter(1,2)
    if (self.P == None):
      pass    
    pass  

  def tearDown(self):
    self.P = None
    pass  

  def test_Parameter_setConstant1(self):
    i = self.P.setConstant(False)
    self.assert_( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE )
    self.assert_( self.P.getConstant() == False )
    pass  

  def test_Parameter_setConstant2(self):
    p = libsbml.Parameter(2,2)
    i = p.setConstant(False)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( p.getConstant() == False )
    p = None
    pass  

  def test_Parameter_setId1(self):
    i = self.P.setId( "1cell")
    self.assert_( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE )
    self.assertEqual( False, self.P.isSetId() )
    pass  

  def test_Parameter_setId2(self):
    i = self.P.setId( "cell")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, self.P.isSetId() )
    self.assert_((  "cell"  == self.P.getId() ))
    i = self.P.setId("")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.P.isSetId() )
    pass  

  def test_Parameter_setName1(self):
    i = self.P.setName( "cell")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, self.P.isSetName() )
    i = self.P.unsetName()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.P.isSetName() )
    pass  

  def test_Parameter_setName2(self):
    p = libsbml.Parameter(2,2)
    i = p.setName( "1cell")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, p.isSetName() )
    i = p.unsetName()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, p.isSetName() )
    p = None
    pass  

  def test_Parameter_setName3(self):
    p = libsbml.Parameter(1,2)
    i = p.setName( "11pp")
    self.assert_( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE )
    self.assertEqual( False, p.isSetName() )
    i = p.setName("")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, p.isSetName() )
    p = None
    pass  

  def test_Parameter_setUnits1(self):
    i = self.P.setUnits( "1cell")
    self.assert_( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE )
    self.assertEqual( False, self.P.isSetUnits() )
    i = self.P.unsetUnits()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.P.isSetUnits() )
    pass  

  def test_Parameter_setUnits2(self):
    i = self.P.setUnits( "litre")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, self.P.isSetUnits() )
    i = self.P.unsetUnits()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.P.isSetUnits() )
    pass  

  def test_Parameter_setUnits3(self):
    i = self.P.setUnits("")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.P.isSetUnits() )
    pass  

  def test_Parameter_setValue1(self):
    i = self.P.setValue(2.0)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( self.P.getValue() == 2.0 )
    self.assertEqual( True, self.P.isSetValue() )
    i = self.P.unsetValue()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.P.isSetValue() )
    pass  

  def test_Parameter_setValue2(self):
    p = libsbml.Parameter(2,2)
    i = p.unsetValue()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, p.isSetValue() )
    p = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestParameter_newSetters))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
