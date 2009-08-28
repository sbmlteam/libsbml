#
# @file    TestSpeciesType_newSetters.py
# @brief   SpeciesType unit tests for new set function API
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestSpeciesType_newSetters.c
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2009 California Institute of Technology.
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

class TestSpeciesType_newSetters(unittest.TestCase):

  ST = None

  def setUp(self):
    self.ST = libsbml.SpeciesType(2,2)
    if (self.ST == None):
      pass    
    pass  

  def tearDown(self):
    self.ST = None
    pass  

  def test_SpeciesType_setId2(self):
    i = self.ST.setId( "1cell")
    self.assert_( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE )
    self.assertEqual( False, self.ST.isSetId() )
    pass  

  def test_SpeciesType_setId3(self):
    i = self.ST.setId( "cell")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, self.ST.isSetId() )
    self.assert_((  "cell"  == self.ST.getId() ))
    pass  

  def test_SpeciesType_setId4(self):
    i = self.ST.setId( "cell")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, self.ST.isSetId() )
    self.assert_((  "cell"  == self.ST.getId() ))
    i = self.ST.setId("")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.ST.isSetId() )
    pass  

  def test_SpeciesType_setName1(self):
    i = self.ST.setName( "cell")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, self.ST.isSetName() )
    i = self.ST.unsetName()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.ST.isSetName() )
    pass  

  def test_SpeciesType_setName2(self):
    i = self.ST.setName( "1cell")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, self.ST.isSetName() )
    i = self.ST.unsetName()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.ST.isSetName() )
    pass  

  def test_SpeciesType_setName3(self):
    i = self.ST.setName( "cell")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, self.ST.isSetName() )
    i = self.ST.setName("")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.ST.isSetName() )
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestSpeciesType_newSetters))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
