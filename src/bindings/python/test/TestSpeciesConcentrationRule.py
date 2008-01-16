#
# @file    TestSpeciesConcentrationRule.py
# @brief   SpeciesConcentrationRule unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestSpeciesConcentrationRule.c
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

class TestSpeciesConcentrationRule(unittest.TestCase):

  SCR = None

  def setUp(self):
    self.SCR = libsbml.AssignmentRule()
    self.SCR.setL1TypeCode(libsbml.SBML_SPECIES_CONCENTRATION_RULE)
    if (self.SCR == None):
      pass    
    pass  

  def tearDown(self):
    self.SCR = None
    pass  

  def test_SpeciesConcentrationRule_create(self):
    self.assert_( self.SCR.getTypeCode() == libsbml.SBML_ASSIGNMENT_RULE )
    self.assert_( self.SCR.getL1TypeCode() == libsbml.SBML_SPECIES_CONCENTRATION_RULE )
    self.assert_( self.SCR.getNotes() == None )
    self.assert_( self.SCR.getAnnotation() == None )
    self.assert_( self.SCR.getFormula() == "" )
    self.assert_( self.SCR.getType() == libsbml.RULE_TYPE_SCALAR )
    self.assert_( self.SCR.getVariable() == "" )
    self.assertEqual( False, self.SCR.isSetVariable() )
    pass  

  def test_SpeciesConcentrationRule_createWith(self):
    scr = libsbml.RateRule("c", "v + 1")
    scr.setL1TypeCode(libsbml.SBML_SPECIES_CONCENTRATION_RULE)
    self.assert_( scr.getTypeCode() == libsbml.SBML_RATE_RULE )
    self.assert_( scr.getL1TypeCode() == libsbml.SBML_SPECIES_CONCENTRATION_RULE )
    self.assert_( scr.getNotes() == None )
    self.assert_( scr.getAnnotation() == None )
    self.assert_((  "v + 1" == scr.getFormula() ))
    self.assert_((  "c" == scr.getVariable() ))
    self.assert_( scr.getType() == libsbml.RULE_TYPE_RATE )
    self.assertEqual( True, scr.isSetVariable() )
    scr = None
    pass  

  def test_SpeciesConcentrationRule_free_NULL(self):
    
    pass  

  def test_SpeciesConcentrationRule_setSpecies(self):
    species = "s2"
    self.SCR.setVariable(species)
    self.assert_(( species == self.SCR.getVariable() ))
    self.assertEqual( True, self.SCR.isSetVariable() )
    if (self.SCR.getVariable() == species):
      pass    
    s = self.SCR.getVariable()
    self.SCR.setVariable(s)
    self.assert_(( species == self.SCR.getVariable() ))
    self.SCR.setVariable("")
    self.assertEqual( False, self.SCR.isSetVariable() )
    if (self.SCR.getVariable() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestSpeciesConcentrationRule))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
