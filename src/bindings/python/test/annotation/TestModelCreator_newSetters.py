#
# @file    TestModelCreator_newSetters.py
# @brief   ModelCreator unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestModelCreator_newSetters.c
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

class TestModelCreator_newSetters(unittest.TestCase):


  def test_ModelCreator_setEmail(self):
    mc = libsbml.ModelCreator()
    self.assert_( mc != None )
    i = mc.setEmail( "Keating")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetEmail() == True )
    self.assert_((  "Keating" == mc.getEmail() ))
    i = mc.setEmail( "")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetEmail() == False )
    i = mc.setEmail( "Keating")
    self.assert_( mc.isSetEmail() == True )
    i = mc.unsetEmail()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetEmail() == False )
    mc = None
    pass  

  def test_ModelCreator_setFamilyName(self):
    mc = libsbml.ModelCreator()
    self.assert_( mc != None )
    i = mc.setFamilyName( "Keating")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetFamilyName() == True )
    self.assert_((  "Keating" == mc.getFamilyName() ))
    i = mc.setFamilyName( "")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetFamilyName() == False )
    i = mc.setFamilyName( "Keating")
    self.assert_( mc.isSetFamilyName() == True )
    i = mc.unsetFamilyName()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetFamilyName() == False )
    mc = None
    pass  

  def test_ModelCreator_setGivenName(self):
    mc = libsbml.ModelCreator()
    self.assert_( mc != None )
    i = mc.setGivenName( "Sarah")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetGivenName() == True )
    self.assert_((  "Sarah" == mc.getGivenName() ))
    i = mc.setGivenName( "")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetGivenName() == False )
    i = mc.setGivenName( "Sarah")
    self.assert_( mc.isSetGivenName() == True )
    i = mc.unsetGivenName()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetGivenName() == False )
    mc = None
    pass  

  def test_ModelCreator_setOrganization(self):
    mc = libsbml.ModelCreator()
    self.assert_( mc != None )
    i = mc.setOrganization( "Caltech")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetOrganization() == True )
    self.assert_((  "Caltech" == mc.getOrganization() ))
    i = mc.setOrganization( "")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetOrganization() == False )
    i = mc.setOrganization( "Caltech")
    self.assert_( mc.isSetOrganization() == True )
    i = mc.unsetOrganization()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( mc.isSetOrganization() == False )
    mc = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestModelCreator_newSetters))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
