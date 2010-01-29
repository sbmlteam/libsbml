#
# @file    TestModelHistory_newSetters.py
# @brief   ModelHistory unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestModelHistory_newSetters.c
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

class TestModelHistory_newSetters(unittest.TestCase):


  def test_ModelHistory_addCreator1(self):
    mh = libsbml.ModelHistory()
    mc = libsbml.ModelCreator()
    mc.setFamilyName( "Keating")
    mc.setGivenName( "Sarah")
    i = mh.addCreator(mc)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mh.getNumCreators() == 1 )
    mc = None
    mh = None
    pass  

  def test_ModelHistory_addCreator2(self):
    mh = libsbml.ModelHistory()
    mc = libsbml.ModelCreator()
    mc.setGivenName( "Sarah")
    i = mh.addCreator(mc)
    self.assert_( i == libsbml.LIBSBML_INVALID_OBJECT )
    self.assert_( mh.getNumCreators() == 0 )
    mc = None
    mh = None
    pass  

  def test_ModelHistory_addCreator3(self):
    mh = libsbml.ModelHistory()
    mc = None
    i = mh.addCreator(mc)
    self.assert_( i == libsbml.LIBSBML_OPERATION_FAILED )
    self.assert_( mh.getNumCreators() == 0 )
    mh = None
    pass  

  def test_ModelHistory_setCreatedDate1(self):
    mh = libsbml.ModelHistory()
    self.assert_( mh != None )
    date = libsbml.Date("2005-12-30T12:15:32+02:00")
    i = mh.setCreatedDate(date)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mh.isSetCreatedDate() == True )
    self.assert_( date != mh.getCreatedDate() )
    dateChar = mh.getCreatedDate().getDateAsString()
    self.assert_((  "2005-12-30T12:15:32+02:00" == dateChar ))
    i = mh.setCreatedDate(None)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mh.isSetCreatedDate() == False )
    date = None
    mh = None
    pass  

  def test_ModelHistory_setCreatedDate2(self):
    mh = libsbml.ModelHistory()
    self.assert_( mh != None )
    date = libsbml.Date("Jan 12")
    i = mh.setCreatedDate(date)
    self.assert_( i == libsbml.LIBSBML_INVALID_OBJECT )
    self.assert_( mh.isSetCreatedDate() == False )
    date = None
    mh = None
    pass  

  def test_ModelHistory_setModifiedDate1(self):
    mh = libsbml.ModelHistory()
    self.assert_( mh != None )
    date = libsbml.Date("2005-12-30T12:15:32+02:00")
    i = mh.setModifiedDate(date)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mh.isSetModifiedDate() == True )
    self.assert_( date != mh.getModifiedDate() )
    dateChar = mh.getModifiedDate().getDateAsString()
    self.assert_((  "2005-12-30T12:15:32+02:00" == dateChar ))
    i = mh.setModifiedDate(None)
    self.assert_( i == libsbml.LIBSBML_OPERATION_FAILED )
    self.assert_( mh.isSetModifiedDate() == True )
    date = None
    mh = None
    pass  

  def test_ModelHistory_setModifiedDate2(self):
    mh = libsbml.ModelHistory()
    self.assert_( mh != None )
    date = libsbml.Date(200,13,76,56,89,90,0,0,0)
    i = mh.setModifiedDate(date)
    self.assert_( i == libsbml.LIBSBML_INVALID_OBJECT )
    self.assert_( mh.isSetModifiedDate() == False )
    date = None
    mh = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestModelHistory_newSetters))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
