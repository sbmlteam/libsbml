#
# @file    TestKineticLaw.py
# @brief   SBML KineticLaw unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestKineticLaw.c
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

class TestKineticLaw(unittest.TestCase):

  KL = None

  def setUp(self):
    self.KL = libsbml.KineticLaw()
    if (self.KL == None):
      pass    
    pass  

  def tearDown(self):
    self.KL = None
    pass  

  def test_KineticLaw_addParameter(self):
    p = libsbml.Parameter()
    self.KL.addParameter(p)
    self.assert_( self.KL.getNumParameters() == 1 )
    p = None
    pass  

  def test_KineticLaw_create(self):
    self.assert_( self.KL.getTypeCode() == libsbml.SBML_KINETIC_LAW )
    self.assert_( self.KL.getMetaId() == "" )
    self.assert_( self.KL.getNotes() == None )
    self.assert_( self.KL.getAnnotation() == None )
    self.assert_( self.KL.getFormula() == "" )
    self.assert_( self.KL.getMath() == None )
    self.assert_( self.KL.getTimeUnits() == "" )
    self.assert_( self.KL.getSubstanceUnits() == "" )
    self.assertEqual( False, self.KL.isSetFormula() )
    self.assertEqual( False, self.KL.isSetMath() )
    self.assertEqual( False, self.KL.isSetTimeUnits() )
    self.assertEqual( False, self.KL.isSetSubstanceUnits() )
    self.assert_( self.KL.getNumParameters() == 0 )
    pass  

  def test_KineticLaw_createWith(self):
    kl = libsbml.KineticLaw("k1 * X0")
    self.assert_( kl.getTypeCode() == libsbml.SBML_KINETIC_LAW )
    self.assert_( kl.getMetaId() == "" )
    self.assert_( kl.getNotes() == None )
    self.assert_( kl.getAnnotation() == None )
    math = kl.getMath()
    self.assert_( math != None )
    formula = libsbml.formulaToString(math)
    self.assert_( formula != None )
    self.assert_((  "k1 * X0" == formula ))
    self.assert_(( formula == kl.getFormula() ))
    self.assertEqual( True, kl.isSetMath() )
    self.assertEqual( True, kl.isSetFormula() )
    self.assert_( kl.getNumParameters() == 0 )
    kl = None
    pass  

  def test_KineticLaw_createWithMath(self):
    math1 = libsbml.parseFormula("k3 / k2")
    kl = libsbml.KineticLaw(math1)
    self.assert_( kl.getTypeCode() == libsbml.SBML_KINETIC_LAW )
    self.assert_( kl.getMetaId() == "" )
    self.assert_( kl.getNotes() == None )
    self.assert_( kl.getAnnotation() == None )
    math = kl.getMath()
    self.assert_( math != None )
    formula = libsbml.formulaToString(math)
    self.assert_( formula != None )
    self.assert_((  "k3 / k2" == formula ))
    self.assert_(( formula == kl.getFormula() ))
    self.assertEqual( True, kl.isSetMath() )
    self.assertEqual( True, kl.isSetFormula() )
    self.assertEqual( False, kl.isSetTimeUnits() )
    self.assertEqual( False, kl.isSetSubstanceUnits() )
    self.assert_( kl.getNumParameters() == 0 )
    kl = None
    pass  

  def test_KineticLaw_free_NULL(self):
    
    pass  

  def test_KineticLaw_getParameter(self):
    k1 = libsbml.Parameter()
    k2 = libsbml.Parameter()
    k1.setName( "k1")
    k2.setName( "k2")
    k1.setValue(3.14)
    k2.setValue(2.72)
    self.KL.addParameter(k1)
    self.KL.addParameter(k2)
    k1 = None
    k2 = None
    self.assert_( self.KL.getNumParameters() == 2 )
    k1 = self.KL.getParameter(0)
    k2 = self.KL.getParameter(1)
    self.assert_((  "k1" == k1.getName() ))
    self.assert_((  "k2" == k2.getName() ))
    self.assert_( k1.getValue() == 3.14 )
    self.assert_( k2.getValue() == 2.72 )
    pass  

  def test_KineticLaw_getParameterById(self):
    k1 = libsbml.Parameter()
    k2 = libsbml.Parameter()
    k1.setId( "k1")
    k2.setId( "k2")
    k1.setValue(3.14)
    k2.setValue(2.72)
    self.KL.addParameter(k1)
    self.KL.addParameter(k2)
    k1 = None
    k2 = None
    self.assert_( self.KL.getNumParameters() == 2 )
    k1 = self.KL.getParameter( "k1")
    k2 = self.KL.getParameter( "k2")
    self.assert_((  "k1" == k1.getId() ))
    self.assert_((  "k2" == k2.getId() ))
    self.assert_( k1.getValue() == 3.14 )
    self.assert_( k2.getValue() == 2.72 )
    pass  

  def test_KineticLaw_setFormula(self):
    formula = "k1*X0"
    self.KL.setFormula(formula)
    self.assert_(( formula == self.KL.getFormula() ))
    self.assertEqual( True, self.KL.isSetFormula() )
    if (self.KL.getFormula() == formula):
      pass    
    self.KL.setFormula(self.KL.getFormula())
    self.assert_(( formula == self.KL.getFormula() ))
    self.KL.setFormula("")
    self.assertEqual( False, self.KL.isSetFormula() )
    if (self.KL.getFormula() != None):
      pass    
    pass  

  def test_KineticLaw_setFormulaFromMath(self):
    math = libsbml.parseFormula("k1 * X0")
    self.assertEqual( False, self.KL.isSetMath() )
    self.assertEqual( False, self.KL.isSetFormula() )
    self.KL.setMath(math)
    self.assertEqual( True, self.KL.isSetMath() )
    self.assertEqual( True, self.KL.isSetFormula() )
    self.assert_((  "k1 * X0" == self.KL.getFormula() ))
    math = None
    pass  

  def test_KineticLaw_setMath(self):
    math = libsbml.parseFormula("k3 / k2")
    self.KL.setMath(math)
    math1 = self.KL.getMath()
    self.assert_( math1 != None )
    formula = libsbml.formulaToString(math1)
    self.assert_( formula != None )
    self.assert_((  "k3 / k2" == formula ))
    self.assert_( self.KL.getMath() != math )
    self.assertEqual( True, self.KL.isSetMath() )
    self.KL.setMath(self.KL.getMath())
    math1 = self.KL.getMath()
    self.assert_( math1 != None )
    formula = libsbml.formulaToString(math1)
    self.assert_( formula != None )
    self.assert_((  "k3 / k2" == formula ))
    self.assert_( self.KL.getMath() != math )
    self.KL.setMath(None)
    self.assertEqual( False, self.KL.isSetMath() )
    if (self.KL.getMath() != None):
      pass    
    math = None
    pass  

  def test_KineticLaw_setMathFromFormula(self):
    formula = "k3 / k2"
    self.assertEqual( False, self.KL.isSetMath() )
    self.assertEqual( False, self.KL.isSetFormula() )
    self.KL.setFormula(formula)
    self.assertEqual( True, self.KL.isSetMath() )
    self.assertEqual( True, self.KL.isSetFormula() )
    formula = libsbml.formulaToString(self.KL.getMath())
    self.assert_((  "k3 / k2" == formula ))
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestKineticLaw))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
