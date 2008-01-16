#
# @file    TestUnit.py
# @brief   Unit unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestUnit.c
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

class TestUnit(unittest.TestCase):

  U = None

  def setUp(self):
    self.U = libsbml.Unit()
    if (self.U == None):
      pass    
    pass  

  def tearDown(self):
    self.U = None
    pass  

  def test_Unit_create(self):
    self.assert_( self.U.getTypeCode() == libsbml.SBML_UNIT )
    self.assert_( self.U.getMetaId() == "" )
    self.assert_( self.U.getNotes() == None )
    self.assert_( self.U.getAnnotation() == None )
    self.assert_( self.U.getKind() == libsbml.UNIT_KIND_INVALID )
    self.assert_( self.U.getExponent() == 1 )
    self.assert_( self.U.getScale() == 0 )
    self.assert_( self.U.getMultiplier() == 1.0 )
    self.assertEqual( False, self.U.isSetKind() )
    pass  

  def test_Unit_createWith(self):
    u = libsbml.Unit(libsbml.UNIT_KIND_SECOND,-2,1)
    self.assert_( u.getTypeCode() == libsbml.SBML_UNIT )
    self.assert_( u.getMetaId() == "" )
    self.assert_( u.getNotes() == None )
    self.assert_( u.getAnnotation() == None )
    self.assert_( u.getKind() == libsbml.UNIT_KIND_SECOND )
    self.assert_( u.getExponent() == -2 )
    self.assert_( u.getScale() == 1 )
    self.assert_( u.getMultiplier() == 1.0 )
    self.assert_( u.getOffset() == 0.0 )
    self.assertEqual( True, u.isSetKind() )
    u = None
    pass  

  def test_Unit_free_NULL(self):
    
    pass  

  def test_Unit_isBuiltIn(self):
    self.assertEqual( True, libsbml.Unit.isBuiltIn( "substance",1) )
    self.assertEqual( True, libsbml.Unit.isBuiltIn( "volume"   ,1) )
    self.assertEqual( False, libsbml.Unit.isBuiltIn( "area"     ,1) )
    self.assertEqual( False, libsbml.Unit.isBuiltIn( "length"   ,1) )
    self.assertEqual( True, libsbml.Unit.isBuiltIn( "time"     ,1) )
    self.assertEqual( True, libsbml.Unit.isBuiltIn( "substance",2) )
    self.assertEqual( True, libsbml.Unit.isBuiltIn( "volume"   ,2) )
    self.assertEqual( True, libsbml.Unit.isBuiltIn( "area"     ,2) )
    self.assertEqual( True, libsbml.Unit.isBuiltIn( "length"   ,2) )
    self.assertEqual( True, libsbml.Unit.isBuiltIn( "time"     ,2) )
    self.assertEqual( False, libsbml.Unit.isBuiltIn("",1) )
    self.assertEqual( False, libsbml.Unit.isBuiltIn( ""       ,1) )
    self.assertEqual( False, libsbml.Unit.isBuiltIn( "volt"   ,1) )
    self.assertEqual( False, libsbml.Unit.isBuiltIn( "foobar" ,1) )
    self.assertEqual( False, libsbml.Unit.isBuiltIn("",2) )
    self.assertEqual( False, libsbml.Unit.isBuiltIn( ""       ,2) )
    self.assertEqual( False, libsbml.Unit.isBuiltIn( "volt"   ,2) )
    self.assertEqual( False, libsbml.Unit.isBuiltIn( "foobar" ,2) )
    pass  

  def test_Unit_isXXX(self):
    self.assertEqual( False, self.U.isSetKind() )
    self.U.setKind(libsbml.UNIT_KIND_AMPERE)
    self.assertEqual( True, self.U.isAmpere() )
    self.U.setKind(libsbml.UNIT_KIND_BECQUEREL)
    self.assertEqual( True, self.U.isBecquerel() )
    self.U.setKind(libsbml.UNIT_KIND_CANDELA)
    self.assertEqual( True, self.U.isCandela() )
    self.U.setKind(libsbml.UNIT_KIND_CELSIUS)
    self.assertEqual( True, self.U.isCelsius() )
    self.U.setKind(libsbml.UNIT_KIND_COULOMB)
    self.assertEqual( True, self.U.isCoulomb() )
    self.U.setKind(libsbml.UNIT_KIND_DIMENSIONLESS)
    self.assertEqual( True, self.U.isDimensionless() )
    self.U.setKind(libsbml.UNIT_KIND_FARAD)
    self.assertEqual( True, self.U.isFarad() )
    self.U.setKind(libsbml.UNIT_KIND_GRAM)
    self.assertEqual( True, self.U.isGram() )
    self.U.setKind(libsbml.UNIT_KIND_GRAY)
    self.assertEqual( True, self.U.isGray() )
    self.U.setKind(libsbml.UNIT_KIND_HENRY)
    self.assertEqual( True, self.U.isHenry() )
    self.U.setKind(libsbml.UNIT_KIND_HERTZ)
    self.assertEqual( True, self.U.isHertz() )
    self.U.setKind(libsbml.UNIT_KIND_ITEM)
    self.assertEqual( True, self.U.isItem() )
    self.U.setKind(libsbml.UNIT_KIND_JOULE)
    self.assertEqual( True, self.U.isJoule() )
    self.U.setKind(libsbml.UNIT_KIND_KATAL)
    self.assertEqual( True, self.U.isKatal() )
    self.U.setKind(libsbml.UNIT_KIND_KELVIN)
    self.assertEqual( True, self.U.isKelvin() )
    self.U.setKind(libsbml.UNIT_KIND_KILOGRAM)
    self.assertEqual( True, self.U.isKilogram() )
    self.U.setKind(libsbml.UNIT_KIND_LITRE)
    self.assertEqual( True, self.U.isLitre() )
    self.U.setKind(libsbml.UNIT_KIND_LUMEN)
    self.assertEqual( True, self.U.isLumen() )
    self.U.setKind(libsbml.UNIT_KIND_LUX)
    self.assertEqual( True, self.U.isLux() )
    self.U.setKind(libsbml.UNIT_KIND_METRE)
    self.assertEqual( True, self.U.isMetre() )
    self.U.setKind(libsbml.UNIT_KIND_MOLE)
    self.assertEqual( True, self.U.isMole() )
    self.U.setKind(libsbml.UNIT_KIND_NEWTON)
    self.assertEqual( True, self.U.isNewton() )
    self.U.setKind(libsbml.UNIT_KIND_OHM)
    self.assertEqual( True, self.U.isOhm() )
    self.U.setKind(libsbml.UNIT_KIND_PASCAL)
    self.assertEqual( True, self.U.isPascal() )
    self.U.setKind(libsbml.UNIT_KIND_RADIAN)
    self.assertEqual( True, self.U.isRadian() )
    self.U.setKind(libsbml.UNIT_KIND_SECOND)
    self.assertEqual( True, self.U.isSecond() )
    self.U.setKind(libsbml.UNIT_KIND_SIEMENS)
    self.assertEqual( True, self.U.isSiemens() )
    self.U.setKind(libsbml.UNIT_KIND_SIEVERT)
    self.assertEqual( True, self.U.isSievert() )
    self.U.setKind(libsbml.UNIT_KIND_STERADIAN)
    self.assertEqual( True, self.U.isSteradian() )
    self.U.setKind(libsbml.UNIT_KIND_TESLA)
    self.assertEqual( True, self.U.isTesla() )
    self.U.setKind(libsbml.UNIT_KIND_VOLT)
    self.assertEqual( True, self.U.isVolt() )
    self.U.setKind(libsbml.UNIT_KIND_WATT)
    self.assertEqual( True, self.U.isWatt() )
    self.U.setKind(libsbml.UNIT_KIND_WEBER)
    self.assertEqual( True, self.U.isWeber() )
    pass  

  def test_Unit_set_get(self):
    u = libsbml.Unit()
    self.assert_( u.getKind() == libsbml.UNIT_KIND_INVALID )
    self.assert_( u.getExponent() == 1 )
    self.assert_( u.getScale() == 0 )
    self.assert_( u.getMultiplier() == 1.0 )
    self.assertEqual( False, u.isSetKind() )
    u.setKind(libsbml.UNIT_KIND_WATT)
    self.assert_( u.getKind() == libsbml.UNIT_KIND_WATT )
    u.setExponent(3)
    self.assert_( u.getExponent() == 3 )
    u.setScale(4)
    self.assert_( u.getScale() == 4 )
    u.setMultiplier(3.2)
    self.assert_( u.getMultiplier() == 3.2 )
    u = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestUnit))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
