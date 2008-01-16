#
# @file    TestUnitDefinition.py
# @brief   SBML UnitDefinition unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestUnitDefinition.c
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

class TestUnitDefinition(unittest.TestCase):

  UD = None

  def setUp(self):
    self.UD = libsbml.UnitDefinition()
    if (self.UD == None):
      pass    
    pass  

  def tearDown(self):
    self.UD = None
    pass  

  def test_UnitDefinition_addUnit(self):
    u = libsbml.Unit()
    self.UD.addUnit(u)
    self.assert_( self.UD.getNumUnits() == 1 )
    u = None
    pass  

  def test_UnitDefinition_create(self):
    self.assert_( self.UD.getTypeCode() == libsbml.SBML_UNIT_DEFINITION )
    self.assert_( self.UD.getMetaId() == "" )
    self.assert_( self.UD.getNotes() == None )
    self.assert_( self.UD.getAnnotation() == None )
    self.assert_( self.UD.getId() == "" )
    self.assert_( self.UD.getName() == "" )
    self.assertEqual( False, self.UD.isSetId() )
    self.assertEqual( False, self.UD.isSetName() )
    self.assert_( self.UD.getNumUnits() == 0 )
    pass  

  def test_UnitDefinition_createWith(self):
    ud = libsbml.UnitDefinition("mmls", "")
    self.assert_( ud.getTypeCode() == libsbml.SBML_UNIT_DEFINITION )
    self.assert_( ud.getMetaId() == "" )
    self.assert_( ud.getNotes() == None )
    self.assert_( ud.getAnnotation() == None )
    self.assert_( ud.getName() == "" )
    self.assert_((  "mmls" == ud.getId() ))
    self.assertEqual( True, ud.isSetId() )
    self.assert_( ud.getNumUnits() == 0 )
    ud = None
    pass  

  def test_UnitDefinition_createWithName(self):
    ud = libsbml.UnitDefinition("", "mmol liter^-1 sec^-1")
    self.assert_( ud.getTypeCode() == libsbml.SBML_UNIT_DEFINITION )
    self.assert_( ud.getMetaId() == "" )
    self.assert_( ud.getNotes() == None )
    self.assert_( ud.getAnnotation() == None )
    self.assert_( ud.getId() == "" )
    self.assert_((  "mmol liter^-1 sec^-1" == ud.getName() ))
    self.assertEqual( True, ud.isSetName() )
    self.assert_( ud.getNumUnits() == 0 )
    ud = None
    pass  

  def test_UnitDefinition_free_NULL(self):
    
    pass  

  def test_UnitDefinition_getUnit(self):
    mole = libsbml.Unit()
    litre = libsbml.Unit()
    second = libsbml.Unit()
    mole.setKind(libsbml.UnitKind_forName("mole"))
    litre.setKind(libsbml.UnitKind_forName("litre"))
    second.setKind(libsbml.UnitKind_forName("second"))
    mole.setScale(-3)
    litre.setExponent(-1)
    second.setExponent(-1)
    self.UD.addUnit(mole)
    self.UD.addUnit(litre)
    self.UD.addUnit(second)
    mole = None
    litre = None
    second = None
    self.assert_( self.UD.getNumUnits() == 3 )
    mole = self.UD.getUnit(0)
    litre = self.UD.getUnit(1)
    second = self.UD.getUnit(2)
    self.assert_( mole.getKind() == libsbml.UNIT_KIND_MOLE )
    self.assert_( litre.getKind() == libsbml.UNIT_KIND_LITRE )
    self.assert_( second.getKind() == libsbml.UNIT_KIND_SECOND )
    self.assert_( mole.getScale() == -3 )
    self.assert_( litre.getExponent() == -1 )
    self.assert_( second.getExponent() == -1 )
    pass  

  def test_UnitDefinition_isVariantOfArea(self):
    dim = libsbml.Unit()
    dim.setKind(libsbml.UnitKind_forName("dimensionless"))
    u = self.UD.createUnit()
    self.assertEqual( False, self.UD.isVariantOfArea() )
    u.setKind(libsbml.UNIT_KIND_METRE)
    u.setExponent(2)
    self.assertEqual( True, self.UD.isVariantOfArea() )
    u.setScale(-1)
    self.assertEqual( True, self.UD.isVariantOfArea() )
    u.setMultiplier(2)
    self.assertEqual( True, self.UD.isVariantOfArea() )
    u.setOffset(3)
    self.assertEqual( True, self.UD.isVariantOfArea() )
    u.setExponent(3)
    self.assertEqual( False, self.UD.isVariantOfArea() )
    u.setExponent(2)
    self.UD.addUnit(dim)
    self.assertEqual( True, self.UD.isVariantOfArea() )
    pass  

  def test_UnitDefinition_isVariantOfLength(self):
    dim = libsbml.Unit()
    dim.setKind(libsbml.UnitKind_forName("dimensionless"))
    u = self.UD.createUnit()
    self.assertEqual( False, self.UD.isVariantOfLength() )
    u.setKind(libsbml.UNIT_KIND_METRE)
    u.setExponent(1)
    self.assertEqual( True, self.UD.isVariantOfLength() )
    u.setScale(-1)
    self.assertEqual( True, self.UD.isVariantOfLength() )
    u.setMultiplier(2)
    self.assertEqual( True, self.UD.isVariantOfLength() )
    u.setOffset(3)
    self.assertEqual( True, self.UD.isVariantOfLength() )
    u.setExponent(2)
    self.assertEqual( False, self.UD.isVariantOfLength() )
    u.setExponent(1)
    self.UD.addUnit(dim)
    self.assertEqual( True, self.UD.isVariantOfLength() )
    pass  

  def test_UnitDefinition_isVariantOfSubstance_1(self):
    dim = libsbml.Unit()
    dim.setKind(libsbml.UnitKind_forName("dimensionless"))
    u = self.UD.createUnit()
    self.assertEqual( False, self.UD.isVariantOfSubstance() )
    u.setKind(libsbml.UNIT_KIND_MOLE)
    u.setExponent(1)
    self.assertEqual( True, self.UD.isVariantOfSubstance() )
    u.setScale(-1)
    self.assertEqual( True, self.UD.isVariantOfSubstance() )
    u.setMultiplier(2)
    self.assertEqual( True, self.UD.isVariantOfSubstance() )
    u.setOffset(3)
    self.assertEqual( True, self.UD.isVariantOfSubstance() )
    u.setExponent(-3)
    self.assertEqual( False, self.UD.isVariantOfSubstance() )
    u.setExponent(1)
    self.UD.addUnit(dim)
    self.assertEqual( True, self.UD.isVariantOfSubstance() )
    pass  

  def test_UnitDefinition_isVariantOfSubstance_2(self):
    dim = libsbml.Unit()
    dim.setKind(libsbml.UnitKind_forName("dimensionless"))
    u = self.UD.createUnit()
    self.assertEqual( False, self.UD.isVariantOfSubstance() )
    u.setKind(libsbml.UNIT_KIND_ITEM)
    u.setExponent(1)
    self.assertEqual( True, self.UD.isVariantOfSubstance() )
    u.setScale(-1)
    self.assertEqual( True, self.UD.isVariantOfSubstance() )
    u.setMultiplier(2)
    self.assertEqual( True, self.UD.isVariantOfSubstance() )
    u.setOffset(3)
    self.assertEqual( True, self.UD.isVariantOfSubstance() )
    u.setExponent(-2)
    self.assertEqual( False, self.UD.isVariantOfSubstance() )
    u.setExponent(1)
    self.UD.addUnit(dim)
    self.assertEqual( True, self.UD.isVariantOfSubstance() )
    pass  

  def test_UnitDefinition_isVariantOfTime(self):
    dim = libsbml.Unit()
    dim.setKind(libsbml.UnitKind_forName("dimensionless"))
    u = self.UD.createUnit()
    self.assertEqual( False, self.UD.isVariantOfTime() )
    u.setKind(libsbml.UNIT_KIND_SECOND)
    u.setExponent(1)
    self.assertEqual( True, self.UD.isVariantOfTime() )
    u.setScale(-10)
    self.assertEqual( True, self.UD.isVariantOfTime() )
    u.setMultiplier(10)
    self.assertEqual( True, self.UD.isVariantOfTime() )
    u.setOffset(30)
    self.assertEqual( True, self.UD.isVariantOfTime() )
    u.setExponent(2)
    self.assertEqual( False, self.UD.isVariantOfTime() )
    u.setExponent(1)
    self.UD.addUnit(dim)
    self.assertEqual( True, self.UD.isVariantOfTime() )
    pass  

  def test_UnitDefinition_isVariantOfVolume_1(self):
    dim = libsbml.Unit()
    dim.setKind(libsbml.UnitKind_forName("dimensionless"))
    u = self.UD.createUnit()
    self.assertEqual( False, self.UD.isVariantOfVolume() )
    u.setKind(libsbml.UNIT_KIND_LITRE)
    u.setExponent(1)
    self.assertEqual( True, self.UD.isVariantOfVolume() )
    u.setScale(100)
    self.assertEqual( True, self.UD.isVariantOfVolume() )
    u.setMultiplier(5)
    self.assertEqual( True, self.UD.isVariantOfVolume() )
    u.setOffset(-5)
    self.assertEqual( True, self.UD.isVariantOfVolume() )
    u.setExponent(-1)
    self.assertEqual( False, self.UD.isVariantOfVolume() )
    u.setExponent(1)
    self.UD.addUnit(dim)
    self.assertEqual( True, self.UD.isVariantOfVolume() )
    pass  

  def test_UnitDefinition_isVariantOfVolume_2(self):
    dim = libsbml.Unit()
    dim.setKind(libsbml.UnitKind_forName("dimensionless"))
    u = self.UD.createUnit()
    self.assertEqual( False, self.UD.isVariantOfVolume() )
    u.setKind(libsbml.UNIT_KIND_METRE)
    u.setExponent(3)
    self.assertEqual( True, self.UD.isVariantOfVolume() )
    u.setScale(100)
    self.assertEqual( True, self.UD.isVariantOfVolume() )
    u.setMultiplier(5)
    self.assertEqual( True, self.UD.isVariantOfVolume() )
    u.setOffset(-5)
    self.assertEqual( True, self.UD.isVariantOfVolume() )
    u.setExponent(2)
    self.assertEqual( False, self.UD.isVariantOfVolume() )
    u.setExponent(3)
    self.UD.addUnit(dim)
    self.assertEqual( True, self.UD.isVariantOfVolume() )
    pass  

  def test_UnitDefinition_setId(self):
    id = "mmls"
    self.UD.setId(id)
    self.assert_(( id == self.UD.getId() ))
    self.assertEqual( True, self.UD.isSetId() )
    if (self.UD.getId() == id):
      pass    
    self.UD.setId(self.UD.getId())
    self.assert_(( id == self.UD.getId() ))
    self.UD.setId("")
    self.assertEqual( False, self.UD.isSetId() )
    if (self.UD.getId() != None):
      pass    
    pass  

  def test_UnitDefinition_setName(self):
    name = "mmol liter^-1 sec^-1"
    self.UD.setName(name)
    self.assert_(( name == self.UD.getName() ))
    self.assertEqual( True, self.UD.isSetName() )
    if (self.UD.getName() == name):
      pass    
    self.UD.setName(self.UD.getName())
    self.assert_(( name == self.UD.getName() ))
    self.UD.setName("")
    self.assertEqual( False, self.UD.isSetName() )
    if (self.UD.getName() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestUnitDefinition))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
