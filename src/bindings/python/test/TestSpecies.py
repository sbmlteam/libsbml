#
# @file    TestSpecies.py
# @brief   Species unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestSpecies.c
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

class TestSpecies(unittest.TestCase):

  S = None

  def setUp(self):
    self.S = libsbml.Species()
    if (self.S == None):
      pass    
    pass  

  def tearDown(self):
    self.S = None
    pass  

  def test_Species_create(self):
    self.assert_( self.S.getTypeCode() == libsbml.SBML_SPECIES )
    self.assert_( self.S.getMetaId() == "" )
    self.assert_( self.S.getNotes() == None )
    self.assert_( self.S.getAnnotation() == None )
    self.assert_( self.S.getId() == "" )
    self.assert_( self.S.getName() == "" )
    self.assert_( self.S.getCompartment() == "" )
    self.assert_( self.S.getInitialAmount() == 0.0 )
    self.assert_( self.S.getInitialConcentration() == 0.0 )
    self.assert_( self.S.getSubstanceUnits() == "" )
    self.assert_( self.S.getSpatialSizeUnits() == "" )
    self.assert_( self.S.getHasOnlySubstanceUnits() == False )
    self.assert_( self.S.getBoundaryCondition() == False )
    self.assert_( self.S.getCharge() == 0 )
    self.assert_( self.S.getConstant() == False )
    self.assertEqual( False, self.S.isSetId() )
    self.assertEqual( False, self.S.isSetName() )
    self.assertEqual( False, self.S.isSetCompartment() )
    self.assertEqual( False, self.S.isSetInitialAmount() )
    self.assertEqual( False, self.S.isSetInitialConcentration() )
    self.assertEqual( False, self.S.isSetSubstanceUnits() )
    self.assertEqual( False, self.S.isSetSpatialSizeUnits() )
    self.assertEqual( False, self.S.isSetUnits() )
    self.assertEqual( False, self.S.isSetCharge() )
    pass  

  def test_Species_createWith(self):
    s = libsbml.Species("Ca", "Calcium")
    self.assert_( s.getTypeCode() == libsbml.SBML_SPECIES )
    self.assert_( s.getMetaId() == "" )
    self.assert_( s.getNotes() == None )
    self.assert_( s.getAnnotation() == None )
    self.assert_((  "Calcium"   == s.getName() ))
    self.assert_( s.getSpatialSizeUnits() == "" )
    self.assert_( s.getHasOnlySubstanceUnits() == False )
    self.assert_( s.getConstant() == False )
    self.assert_((  "Ca"   == s.getId() ))
    self.assertEqual( True, s.isSetId() )
    self.assertEqual( True, s.isSetName() )
    self.assertEqual( False, s.isSetCompartment() )
    self.assertEqual( False, s.isSetSubstanceUnits() )
    self.assertEqual( False, s.isSetSpatialSizeUnits() )
    self.assertEqual( False, s.isSetUnits() )
    self.assertEqual( False, s.isSetInitialAmount() )
    self.assertEqual( False, s.isSetInitialConcentration() )
    self.assertEqual( False, s.isSetCharge() )
    s = None
    pass  

  def test_Species_free_NULL(self):
    
    pass  

  def test_Species_setCompartment(self):
    compartment = "cell"
    self.S.setCompartment(compartment)
    self.assert_(( compartment == self.S.getCompartment() ))
    self.assertEqual( True, self.S.isSetCompartment() )
    if (self.S.getCompartment() == compartment):
      pass    
    self.S.setCompartment(self.S.getCompartment())
    self.assert_(( compartment == self.S.getCompartment() ))
    self.S.setCompartment("")
    self.assertEqual( False, self.S.isSetCompartment() )
    if (self.S.getCompartment() != None):
      pass    
    pass  

  def test_Species_setId(self):
    id = "Glucose"
    self.S.setId(id)
    self.assert_(( id == self.S.getId() ))
    self.assertEqual( True, self.S.isSetId() )
    if (self.S.getId() == id):
      pass    
    self.S.setId(self.S.getId())
    self.assert_(( id == self.S.getId() ))
    self.S.setId("")
    self.assertEqual( False, self.S.isSetId() )
    if (self.S.getId() != None):
      pass    
    pass  

  def test_Species_setInitialAmount(self):
    self.assertEqual( False, self.S.isSetInitialAmount() )
    self.assertEqual( False, self.S.isSetInitialConcentration() )
    self.S.setInitialAmount(1.2)
    self.assertEqual( True, self.S.isSetInitialAmount() )
    self.assertEqual( False, self.S.isSetInitialConcentration() )
    self.assert_( self.S.getInitialAmount() == 1.2 )
    pass  

  def test_Species_setInitialConcentration(self):
    self.assertEqual( False, self.S.isSetInitialAmount() )
    self.assertEqual( False, self.S.isSetInitialConcentration() )
    self.S.setInitialConcentration(3.4)
    self.assertEqual( False, self.S.isSetInitialAmount() )
    self.assertEqual( True, self.S.isSetInitialConcentration() )
    self.assert_( self.S.getInitialConcentration() == 3.4 )
    pass  

  def test_Species_setName(self):
    name = "So Sweet"
    self.S.setName(name)
    self.assert_(( name == self.S.getName() ))
    self.assertEqual( True, self.S.isSetName() )
    if (self.S.getName() == name):
      pass    
    self.S.setName(self.S.getName())
    self.assert_(( name == self.S.getName() ))
    self.S.setName("")
    self.assertEqual( False, self.S.isSetName() )
    if (self.S.getName() != None):
      pass    
    pass  

  def test_Species_setSpatialSizeUnits(self):
    units = "volume"
    self.S.setSpatialSizeUnits(units)
    self.assert_(( units == self.S.getSpatialSizeUnits() ))
    self.assertEqual( True, self.S.isSetSpatialSizeUnits() )
    if (self.S.getSpatialSizeUnits() == units):
      pass    
    self.S.setSpatialSizeUnits(self.S.getSpatialSizeUnits())
    self.assert_(( units == self.S.getSpatialSizeUnits() ))
    self.S.setSpatialSizeUnits("")
    self.assertEqual( False, self.S.isSetSpatialSizeUnits() )
    if (self.S.getSpatialSizeUnits() != None):
      pass    
    pass  

  def test_Species_setSubstanceUnits(self):
    units = "item"
    self.S.setSubstanceUnits(units)
    self.assert_(( units == self.S.getSubstanceUnits() ))
    self.assertEqual( True, self.S.isSetSubstanceUnits() )
    if (self.S.getSubstanceUnits() == units):
      pass    
    self.S.setSubstanceUnits(self.S.getSubstanceUnits())
    self.assert_(( units == self.S.getSubstanceUnits() ))
    self.S.setSubstanceUnits("")
    self.assertEqual( False, self.S.isSetSubstanceUnits() )
    if (self.S.getSubstanceUnits() != None):
      pass    
    pass  

  def test_Species_setUnits(self):
    units = "mole"
    self.S.setUnits(units)
    self.assert_(( units == self.S.getUnits() ))
    self.assertEqual( True, self.S.isSetUnits() )
    if (self.S.getSubstanceUnits() == units):
      pass    
    self.S.setUnits(self.S.getSubstanceUnits())
    self.assert_(( units == self.S.getUnits() ))
    self.S.setUnits("")
    self.assertEqual( False, self.S.isSetUnits() )
    if (self.S.getSubstanceUnits() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestSpecies))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
