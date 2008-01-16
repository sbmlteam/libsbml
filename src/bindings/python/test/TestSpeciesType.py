#
# @file    TestSpeciesType.py
# @brief   SpeciesType unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestSpeciesType.c
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

class TestSpeciesType(unittest.TestCase):

  CT = None

  def setUp(self):
    self.CT = libsbml.SpeciesType()
    if (self.CT == None):
      pass    
    pass  

  def tearDown(self):
    self.CT = None
    pass  

  def test_SpeciesType_create(self):
    self.assert_( self.CT.getTypeCode() == libsbml.SBML_SPECIES_TYPE )
    self.assert_( self.CT.getMetaId() == "" )
    self.assert_( self.CT.getNotes() == None )
    self.assert_( self.CT.getAnnotation() == None )
    self.assert_( self.CT.getId() == "" )
    self.assert_( self.CT.getName() == "" )
    self.assertEqual( False, self.CT.isSetId() )
    self.assertEqual( False, self.CT.isSetName() )
    pass  

  def test_SpeciesType_createWith(self):
    c = libsbml.SpeciesType("A", "")
    self.assert_( c.getTypeCode() == libsbml.SBML_SPECIES_TYPE )
    self.assert_( c.getMetaId() == "" )
    self.assert_( c.getNotes() == None )
    self.assert_( c.getAnnotation() == None )
    self.assert_( c.getName() == "" )
    self.assert_((  "A"      == c.getId() ))
    self.assertEqual( True, c.isSetId() )
    self.assertEqual( False, c.isSetName() )
    c = None
    pass  

  def test_SpeciesType_free_NULL(self):
    
    pass  

  def test_SpeciesType_setId(self):
    id = "mitochondria"
    self.CT.setId(id)
    self.assert_(( id == self.CT.getId() ))
    self.assertEqual( True, self.CT.isSetId() )
    if (self.CT.getId() == id):
      pass    
    self.CT.setId(self.CT.getId())
    self.assert_(( id == self.CT.getId() ))
    self.CT.setId("")
    self.assertEqual( False, self.CT.isSetId() )
    if (self.CT.getId() != None):
      pass    
    pass  

  def test_SpeciesType_setName(self):
    name = "My Favorite Factory"
    self.CT.setName(name)
    self.assert_(( name == self.CT.getName() ))
    self.assertEqual( True, self.CT.isSetName() )
    if (self.CT.getName() == name):
      pass    
    self.CT.setName(self.CT.getName())
    self.assert_(( name == self.CT.getName() ))
    self.CT.setName("")
    self.assertEqual( False, self.CT.isSetName() )
    if (self.CT.getName() != None):
      pass    
    pass  

  def test_SpeciesType_unsetName(self):
    self.CT.setName( "name")
    self.assert_((  "name"      == self.CT.getName() ))
    self.assertEqual( True, self.CT.isSetName() )
    self.CT.unsetName()
    self.assertEqual( False, self.CT.isSetName() )
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestSpeciesType))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
