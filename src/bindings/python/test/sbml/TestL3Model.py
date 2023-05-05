#
# @file    TestL3Model.py
# @brief   L3 Model unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
# 
# ====== WARNING ===== WARNING ===== WARNING ===== WARNING ===== WARNING ======
#
# DO NOT EDIT THIS FILE.
#
# This file was generated automatically by converting the file located at
# src/sbml/test/TestL3Model.c
# using the conversion program dev/utilities/translateTests/translateTests.pl.
# Any changes made here will be lost the next time the file is regenerated.
#
# -----------------------------------------------------------------------------
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
# -----------------------------------------------------------------------------

import sys
import unittest
import libsbml


class TestL3Model(unittest.TestCase):

  global M
  M = None

  def setUp(self):
    self.M = libsbml.Model(3,1)
    if (self.M == None):
      pass    
    pass  

  def tearDown(self):
    _dummyList = [ self.M ]; _dummyList[:] = []; del _dummyList
    pass  

  def test_L3_Model_NS(self):
    self.assertTrue( self.M.getNamespaces() != None )
    self.assertTrue( self.M.getNamespaces().getLength() == 1 )
    self.assertTrue((     "http://www.sbml.org/sbml/level3/version1/core" == self.M.getNamespaces().getURI(0) ))
    pass  

  def test_L3_Model_areaUnits(self):
    units =  "mole";
    self.assertEqual( False, self.M.isSetAreaUnits() )
    self.M.setAreaUnits(units)
    self.assertTrue(( units == self.M.getAreaUnits() ))
    self.assertEqual( True, self.M.isSetAreaUnits() )
    if (self.M.getAreaUnits() == units):
      pass    
    self.M.unsetAreaUnits()
    self.assertEqual( False, self.M.isSetAreaUnits() )
    if (self.M.getAreaUnits() != None):
      pass    
    pass  

  def test_L3_Model_conversionFactor(self):
    units =  "mole";
    self.assertEqual( False, self.M.isSetConversionFactor() )
    self.M.setConversionFactor(units)
    self.assertTrue(( units == self.M.getConversionFactor() ))
    self.assertEqual( True, self.M.isSetConversionFactor() )
    if (self.M.getConversionFactor() == units):
      pass    
    self.M.unsetConversionFactor()
    self.assertEqual( False, self.M.isSetConversionFactor() )
    if (self.M.getConversionFactor() != None):
      pass    
    pass  

  def test_L3_Model_create(self):
    self.assertTrue( self.M.getTypeCode() == libsbml.SBML_MODEL )
    self.assertTrue( self.M.getMetaId() == "" )
    self.assertTrue( self.M.getNotes() == None )
    self.assertTrue( self.M.getAnnotation() == None )
    self.assertTrue( self.M.getId() == "" )
    self.assertTrue( self.M.getName() == "" )
    self.assertTrue( self.M.getSubstanceUnits() == "" )
    self.assertTrue( self.M.getTimeUnits() == "" )
    self.assertTrue( self.M.getVolumeUnits() == "" )
    self.assertTrue( self.M.getAreaUnits() == "" )
    self.assertTrue( self.M.getLengthUnits() == "" )
    self.assertTrue( self.M.getConversionFactor() == "" )
    self.assertEqual( False, self.M.isSetId() )
    self.assertEqual( False, self.M.isSetName() )
    self.assertEqual( False, self.M.isSetSubstanceUnits() )
    self.assertEqual( False, self.M.isSetTimeUnits() )
    self.assertEqual( False, self.M.isSetVolumeUnits() )
    self.assertEqual( False, self.M.isSetAreaUnits() )
    self.assertEqual( False, self.M.isSetLengthUnits() )
    self.assertEqual( False, self.M.isSetConversionFactor() )
    pass  

  def test_L3_Model_createWithNS(self):
    xmlns = libsbml.XMLNamespaces()
    xmlns.add( "http://www.sbml.org", "testsbml")
    sbmlns = libsbml.SBMLNamespaces(3,1)
    sbmlns.addNamespaces(xmlns)
    m = libsbml.Model(sbmlns)
    self.assertTrue( m.getTypeCode() == libsbml.SBML_MODEL )
    self.assertTrue( m.getMetaId() == "" )
    self.assertTrue( m.getNotes() == None )
    self.assertTrue( m.getAnnotation() == None )
    self.assertTrue( m.getLevel() == 3 )
    self.assertTrue( m.getVersion() == 1 )
    self.assertTrue( m.getNamespaces() != None )
    self.assertTrue( m.getNamespaces().getLength() == 2 )
    self.assertTrue( m.getId() == "" )
    self.assertTrue( m.getName() == "" )
    self.assertTrue( m.getSubstanceUnits() == "" )
    self.assertTrue( m.getTimeUnits() == "" )
    self.assertTrue( m.getVolumeUnits() == "" )
    self.assertTrue( m.getAreaUnits() == "" )
    self.assertTrue( m.getLengthUnits() == "" )
    self.assertTrue( m.getConversionFactor() == "" )
    self.assertEqual( False, m.isSetId() )
    self.assertEqual( False, m.isSetName() )
    self.assertEqual( False, m.isSetSubstanceUnits() )
    self.assertEqual( False, m.isSetTimeUnits() )
    self.assertEqual( False, m.isSetVolumeUnits() )
    self.assertEqual( False, m.isSetAreaUnits() )
    self.assertEqual( False, m.isSetLengthUnits() )
    self.assertEqual( False, m.isSetConversionFactor() )
    _dummyList = [ m ]; _dummyList[:] = []; del _dummyList
    pass  

  def test_L3_Model_extentUnits(self):
    units =  "mole";
    self.assertEqual( False, self.M.isSetExtentUnits() )
    self.M.setExtentUnits(units)
    self.assertTrue(( units == self.M.getExtentUnits() ))
    self.assertEqual( True, self.M.isSetExtentUnits() )
    if (self.M.getExtentUnits() == units):
      pass    
    self.M.unsetExtentUnits()
    self.assertEqual( False, self.M.isSetExtentUnits() )
    if (self.M.getExtentUnits() != None):
      pass    
    pass  

  def test_L3_Model_free_NULL(self):
    _dummyList = [ None ]; _dummyList[:] = []; del _dummyList
    pass  

  def test_L3_Model_id(self):
    id =  "mitochondria";
    self.assertEqual( False, self.M.isSetId() )
    self.M.setId(id)
    self.assertTrue(( id == self.M.getId() ))
    self.assertEqual( True, self.M.isSetId() )
    if (self.M.getId() == id):
      pass    
    self.M.unsetId()
    self.assertEqual( False, self.M.isSetId() )
    if (self.M.getId() != None):
      pass    
    pass  

  def test_L3_Model_lengthUnits(self):
    units =  "mole";
    self.assertEqual( False, self.M.isSetLengthUnits() )
    self.M.setLengthUnits(units)
    self.assertTrue(( units == self.M.getLengthUnits() ))
    self.assertEqual( True, self.M.isSetLengthUnits() )
    if (self.M.getLengthUnits() == units):
      pass    
    self.M.unsetLengthUnits()
    self.assertEqual( False, self.M.isSetLengthUnits() )
    if (self.M.getLengthUnits() != None):
      pass    
    pass  

  def test_L3_Model_name(self):
    name =  "My_Favorite_Factory";
    self.assertEqual( False, self.M.isSetName() )
    self.M.setName(name)
    self.assertTrue(( name == self.M.getName() ))
    self.assertEqual( True, self.M.isSetName() )
    if (self.M.getName() == name):
      pass    
    self.M.unsetName()
    self.assertEqual( False, self.M.isSetName() )
    if (self.M.getName() != None):
      pass    
    pass  

  def test_L3_Model_substanceUnits(self):
    units =  "mole";
    self.assertEqual( False, self.M.isSetSubstanceUnits() )
    self.M.setSubstanceUnits(units)
    self.assertTrue(( units == self.M.getSubstanceUnits() ))
    self.assertEqual( True, self.M.isSetSubstanceUnits() )
    if (self.M.getSubstanceUnits() == units):
      pass    
    self.M.unsetSubstanceUnits()
    self.assertEqual( False, self.M.isSetSubstanceUnits() )
    if (self.M.getSubstanceUnits() != None):
      pass    
    pass  

  def test_L3_Model_timeUnits(self):
    units =  "mole";
    self.assertEqual( False, self.M.isSetTimeUnits() )
    self.M.setTimeUnits(units)
    self.assertTrue(( units == self.M.getTimeUnits() ))
    self.assertEqual( True, self.M.isSetTimeUnits() )
    if (self.M.getTimeUnits() == units):
      pass    
    self.M.unsetTimeUnits()
    self.assertEqual( False, self.M.isSetTimeUnits() )
    if (self.M.getTimeUnits() != None):
      pass    
    pass  

  def test_L3_Model_volumeUnits(self):
    units =  "mole";
    self.assertEqual( False, self.M.isSetVolumeUnits() )
    self.M.setVolumeUnits(units)
    self.assertTrue(( units == self.M.getVolumeUnits() ))
    self.assertEqual( True, self.M.isSetVolumeUnits() )
    if (self.M.getVolumeUnits() == units):
      pass    
    self.M.unsetVolumeUnits()
    self.assertEqual( False, self.M.isSetVolumeUnits() )
    if (self.M.getVolumeUnits() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestL3Model))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)

