#
# @file    TestModifierSpeciesReference.py
# @brief   ModifierSpeciesReference unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestModifierSpeciesReference.c
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

class TestModifierSpeciesReference(unittest.TestCase):

  MSR = None

  def setUp(self):
    self.MSR = libsbml.ModifierSpeciesReference()
    if (self.MSR == None):
      pass    
    pass  

  def tearDown(self):
    self.MSR = None
    pass  

  def test_ModifierSpeciesReference_create(self):
    self.assert_( self.MSR.getTypeCode() == libsbml.SBML_MODIFIER_SPECIES_REFERENCE )
    self.assert_( self.MSR.getMetaId() == "" )
    self.assert_( self.MSR.getNotes() == None )
    self.assert_( self.MSR.getAnnotation() == None )
    self.assert_( self.MSR.getSpecies() == "" )
    self.assertEqual( False, self.MSR.isSetSpecies() )
    self.assertEqual( True, self.MSR.isModifier() )
    pass  

  def test_ModifierSpeciesReference_free_NULL(self):
    
    pass  

  def test_ModifierSpeciesReference_setSpecies(self):
    species = "s1"
    self.MSR.setSpecies(species)
    s = self.MSR.getSpecies()
    self.assert_(( species == s ))
    self.assertEqual( True, self.MSR.isSetSpecies() )
    if (self.MSR.getSpecies() == species):
      pass    
    s = self.MSR.getSpecies()
    self.MSR.setSpecies(s)
    s = self.MSR.getSpecies()
    self.assert_(( species == s ))
    self.MSR.setSpecies("")
    self.assertEqual( False, self.MSR.isSetSpecies() )
    if (self.MSR.getSpecies() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestModifierSpeciesReference))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
