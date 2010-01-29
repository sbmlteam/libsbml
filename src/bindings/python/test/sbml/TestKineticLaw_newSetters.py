#
# @file    TestKineticLaw_newSetters.py
# @brief   KineticLaw unit tests for new set function API
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestKineticLaw_newSetters.c
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

class TestKineticLaw_newSetters(unittest.TestCase):

  kl = None

  def setUp(self):
    self.kl = libsbml.KineticLaw(2,4)
    if (self.kl == None):
      pass    
    pass  

  def tearDown(self):
    self.kl = None
    pass  

  def test_KineticLaw_addParameter1(self):
    self.kl = libsbml.KineticLaw(2,2)
    p = libsbml.Parameter(2,2)
    i = self.kl.addParameter(p)
    self.assert_( i == libsbml.LIBSBML_INVALID_OBJECT )
    p.setId( "p")
    i = self.kl.addParameter(p)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( self.kl.getNumParameters() == 1 )
    p = None
    self.kl = None
    pass  

  def test_KineticLaw_addParameter2(self):
    self.kl = libsbml.KineticLaw(2,2)
    p = libsbml.Parameter(2,1)
    p.setId( "p")
    i = self.kl.addParameter(p)
    self.assert_( i == libsbml.LIBSBML_VERSION_MISMATCH )
    self.assert_( self.kl.getNumParameters() == 0 )
    p = None
    self.kl = None
    pass  

  def test_KineticLaw_addParameter3(self):
    self.kl = libsbml.KineticLaw(2,2)
    p = libsbml.Parameter(1,2)
    p.setId( "p")
    i = self.kl.addParameter(p)
    self.assert_( i == libsbml.LIBSBML_LEVEL_MISMATCH )
    self.assert_( self.kl.getNumParameters() == 0 )
    p = None
    self.kl = None
    pass  

  def test_KineticLaw_addParameter4(self):
    self.kl = libsbml.KineticLaw(2,2)
    p = None
    i = self.kl.addParameter(p)
    self.assert_( i == libsbml.LIBSBML_OPERATION_FAILED )
    self.assert_( self.kl.getNumParameters() == 0 )
    self.kl = None
    pass  

  def test_KineticLaw_createParameter(self):
    self.kl = libsbml.KineticLaw(2,2)
    p = self.kl.createParameter()
    self.assert_( self.kl.getNumParameters() == 1 )
    self.assert_( (p).getLevel() == 2 )
    self.assert_( (p).getVersion() == 2 )
    self.kl = None
    pass  

  def test_KineticLaw_setFormula1(self):
    formula =  "k1*X0";
    i = self.kl.setFormula(formula)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_(( formula == self.kl.getFormula() ))
    self.assertEqual( True, self.kl.isSetFormula() )
    pass  

  def test_KineticLaw_setFormula2(self):
    i = self.kl.setFormula("")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.kl.isSetFormula() )
    pass  

  def test_KineticLaw_setFormula3(self):
    formula =  "k1 X0";
    i = self.kl.setFormula(formula)
    self.assert_( i == libsbml.LIBSBML_INVALID_OBJECT )
    self.assertEqual( False, self.kl.isSetFormula() )
    pass  

  def test_KineticLaw_setMath1(self):
    math = libsbml.ASTNode(libsbml.AST_TIMES)
    a = libsbml.ASTNode()
    b = libsbml.ASTNode()
    a.setName( "a")
    b.setName( "b")
    math.addChild(a)
    math.addChild(b)
    i = self.kl.setMath(math)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, self.kl.isSetMath() )
    math1 = self.kl.getMath()
    self.assert_( math1 != None )
    formula = libsbml.formulaToString(math1)
    self.assert_( formula != None )
    self.assert_((  "a * b" == formula ))
    math = None
    pass  

  def test_KineticLaw_setMath2(self):
    math = libsbml.ASTNode(libsbml.AST_TIMES)
    a = libsbml.ASTNode()
    a.setName( "a")
    math.addChild(a)
    i = self.kl.setMath(math)
    self.assert_( i == libsbml.LIBSBML_INVALID_OBJECT )
    self.assertEqual( False, self.kl.isSetMath() )
    math = None
    pass  

  def test_KineticLaw_setMath3(self):
    i = self.kl.setMath(None)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, self.kl.isSetMath() )
    pass  

  def test_KineticLaw_setSubstanceUnits1(self):
    i = self.kl.setSubstanceUnits( "mole")
    self.assert_( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE )
    self.assertEqual( False, self.kl.isSetSubstanceUnits() )
    i = self.kl.unsetSubstanceUnits()
    self.assert_( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE )
    self.assertEqual( False, self.kl.isSetSubstanceUnits() )
    pass  

  def test_KineticLaw_setSubstanceUnits2(self):
    kl1 = libsbml.KineticLaw(1,2)
    i = kl1.setSubstanceUnits( "mole")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, kl1.isSetSubstanceUnits() )
    i = kl1.unsetSubstanceUnits()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, kl1.isSetSubstanceUnits() )
    kl1 = None
    pass  

  def test_KineticLaw_setSubstanceUnits3(self):
    kl1 = libsbml.KineticLaw(1,2)
    i = kl1.setSubstanceUnits( "1second")
    self.assert_( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE )
    self.assertEqual( False, kl1.isSetSubstanceUnits() )
    i = kl1.unsetSubstanceUnits()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, kl1.isSetSubstanceUnits() )
    kl1 = None
    pass  

  def test_KineticLaw_setSubstanceUnits4(self):
    kl1 = libsbml.KineticLaw(1,2)
    i = kl1.setSubstanceUnits("")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, kl1.isSetSubstanceUnits() )
    kl1 = None
    pass  

  def test_KineticLaw_setTimeUnits1(self):
    i = self.kl.setTimeUnits( "second")
    self.assert_( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE )
    self.assertEqual( False, self.kl.isSetTimeUnits() )
    i = self.kl.unsetTimeUnits()
    self.assert_( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE )
    self.assertEqual( False, self.kl.isSetTimeUnits() )
    pass  

  def test_KineticLaw_setTimeUnits2(self):
    kl1 = libsbml.KineticLaw(1,2)
    i = kl1.setTimeUnits( "second")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( True, kl1.isSetTimeUnits() )
    i = kl1.unsetTimeUnits()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, kl1.isSetTimeUnits() )
    kl1 = None
    pass  

  def test_KineticLaw_setTimeUnits3(self):
    kl1 = libsbml.KineticLaw(1,2)
    i = kl1.setTimeUnits( "1second")
    self.assert_( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE )
    self.assertEqual( False, kl1.isSetTimeUnits() )
    i = kl1.unsetTimeUnits()
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, kl1.isSetTimeUnits() )
    kl1 = None
    pass  

  def test_KineticLaw_setTimeUnits4(self):
    kl1 = libsbml.KineticLaw(1,2)
    i = kl1.setTimeUnits("")
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assertEqual( False, kl1.isSetTimeUnits() )
    kl1 = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestKineticLaw_newSetters))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
