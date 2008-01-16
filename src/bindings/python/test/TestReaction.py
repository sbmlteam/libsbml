#
# @file    TestReaction.py
# @brief   SBML Reaction unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestReaction.c
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

class TestReaction(unittest.TestCase):

  R = None

  def setUp(self):
    self.R = libsbml.Reaction()
    if (self.R == None):
      pass    
    pass  

  def tearDown(self):
    self.R = None
    pass  

  def test_Reaction_addModifier(self):
    self.R.addModifier(libsbml.ModifierSpeciesReference())
    self.assert_( self.R.getNumReactants() == 0 )
    self.assert_( self.R.getNumProducts() == 0 )
    self.assert_( self.R.getNumModifiers() == 1 )
    pass  

  def test_Reaction_addProduct(self):
    sr = libsbml.SpeciesReference()
    self.R.addProduct(sr)
    self.assert_( self.R.getNumReactants() == 0 )
    self.assert_( self.R.getNumProducts() == 1 )
    self.assert_( self.R.getNumModifiers() == 0 )
    sr = None
    pass  

  def test_Reaction_addReactant(self):
    sr = libsbml.SpeciesReference()
    self.R.addReactant(sr)
    self.assert_( self.R.getNumReactants() == 1 )
    self.assert_( self.R.getNumProducts() == 0 )
    self.assert_( self.R.getNumModifiers() == 0 )
    sr = None
    pass  

  def test_Reaction_create(self):
    self.assert_( self.R.getTypeCode() == libsbml.SBML_REACTION )
    self.assert_( self.R.getMetaId() == "" )
    self.assert_( self.R.getNotes() == None )
    self.assert_( self.R.getAnnotation() == None )
    self.assert_( self.R.getId() == "" )
    self.assert_( self.R.getName() == "" )
    self.assert_( self.R.getKineticLaw() == None )
    self.assert_( self.R.getReversible() != False )
    self.assert_( self.R.getFast() == False )
    self.assertEqual( False, self.R.isSetId() )
    self.assertEqual( False, self.R.isSetName() )
    self.assertEqual( False, self.R.isSetKineticLaw() )
    self.assert_( self.R.getNumReactants() == 0 )
    self.assert_( self.R.getNumProducts() == 0 )
    self.assert_( self.R.getNumModifiers() == 0 )
    pass  

  def test_Reaction_createWith(self):
    kl = libsbml.KineticLaw()
    r = libsbml.Reaction("r1", "",kl,0)
    r.setFast(1)
    self.assert_( r.getTypeCode() == libsbml.SBML_REACTION )
    self.assert_( r.getMetaId() == "" )
    self.assert_( r.getNotes() == None )
    self.assert_( r.getAnnotation() == None )
    self.assert_( r.getName() == "" )
    self.assert_((  "r1" == r.getId() ))
    self.assert_( r.getReversible() == False )
    self.assert_( r.getFast() == True )
    self.assertEqual( True, r.isSetId() )
    self.assertEqual( False, r.isSetName() )
    self.assertEqual( True, r.isSetKineticLaw() )
    self.assert_( r.getNumReactants() == 0 )
    self.assert_( r.getNumProducts() == 0 )
    self.assert_( r.getNumModifiers() == 0 )
    kl = None
    r = None
    pass  

  def test_Reaction_free_NULL(self):
    
    pass  

  def test_Reaction_getModifier(self):
    msr1 = libsbml.ModifierSpeciesReference()
    msr2 = libsbml.ModifierSpeciesReference()
    msr1.setSpecies( "M1")
    msr2.setSpecies( "M2")
    self.R.addModifier(msr1)
    self.R.addModifier(msr2)
    msr1 = None
    msr2 = None
    self.assert_( self.R.getNumReactants() == 0 )
    self.assert_( self.R.getNumProducts() == 0 )
    self.assert_( self.R.getNumModifiers() == 2 )
    msr1 = self.R.getModifier(0)
    msr2 = self.R.getModifier(1)
    self.assert_((  "M1" == msr1.getSpecies() ))
    self.assert_((  "M2" == msr2.getSpecies() ))
    pass  

  def test_Reaction_getModifierById(self):
    msr1 = libsbml.ModifierSpeciesReference()
    msr2 = libsbml.ModifierSpeciesReference()
    msr1.setSpecies( "M1")
    msr2.setSpecies( "M2")
    self.R.addModifier(msr1)
    self.R.addModifier(msr2)
    self.assert_( self.R.getNumReactants() == 0 )
    self.assert_( self.R.getNumProducts() == 0 )
    self.assert_( self.R.getNumModifiers() == 2 )
    self.assert_( self.R.getModifier( "M1") != msr1 )
    self.assert_( self.R.getModifier( "M2") != msr2 )
    self.assert_( self.R.getModifier( "M3") == None )
    msr1 = None
    msr2 = None
    pass  

  def test_Reaction_getProduct(self):
    sr1 = libsbml.SpeciesReference()
    sr2 = libsbml.SpeciesReference()
    sr1.setSpecies( "P1")
    sr2.setSpecies( "P2")
    self.R.addProduct(sr1)
    self.R.addProduct(sr2)
    sr1 = None
    sr2 = None
    self.assert_( self.R.getNumReactants() == 0 )
    self.assert_( self.R.getNumProducts() == 2 )
    self.assert_( self.R.getNumModifiers() == 0 )
    sr1 = self.R.getProduct(0)
    sr2 = self.R.getProduct(1)
    self.assert_((  "P1" == sr1.getSpecies() ))
    self.assert_((  "P2" == sr2.getSpecies() ))
    pass  

  def test_Reaction_getProductById(self):
    sr1 = libsbml.SpeciesReference("P1",1,1)
    sr2 = libsbml.SpeciesReference("P2",1,1)
    self.R.addProduct(sr1)
    self.R.addProduct(sr2)
    self.assert_( self.R.getNumReactants() == 0 )
    self.assert_( self.R.getNumProducts() == 2 )
    self.assert_( self.R.getNumModifiers() == 0 )
    self.assert_( self.R.getProduct( "P1") != sr1 )
    self.assert_( self.R.getProduct( "P2") != sr2 )
    self.assert_( self.R.getProduct( "P3") == None )
    sr1 = None
    sr2 = None
    pass  

  def test_Reaction_getReactant(self):
    sr1 = libsbml.SpeciesReference()
    sr2 = libsbml.SpeciesReference()
    sr1.setSpecies( "R1")
    sr2.setSpecies( "R2")
    self.R.addReactant(sr1)
    self.R.addReactant(sr2)
    sr1 = None
    sr2 = None
    self.assert_( self.R.getNumReactants() == 2 )
    self.assert_( self.R.getNumProducts() == 0 )
    self.assert_( self.R.getNumModifiers() == 0 )
    sr1 = self.R.getReactant(0)
    sr2 = self.R.getReactant(1)
    self.assert_((  "R1" == sr1.getSpecies() ))
    self.assert_((  "R2" == sr2.getSpecies() ))
    pass  

  def test_Reaction_getReactantById(self):
    sr1 = libsbml.SpeciesReference("R1",1,1)
    sr2 = libsbml.SpeciesReference("R2",1,1)
    self.R.addReactant(sr1)
    self.R.addReactant(sr2)
    self.assert_( self.R.getNumReactants() == 2 )
    self.assert_( self.R.getNumProducts() == 0 )
    self.assert_( self.R.getNumModifiers() == 0 )
    self.assert_( self.R.getReactant( "R1") != sr1 )
    self.assert_( self.R.getReactant( "R2") != sr2 )
    self.assert_( self.R.getReactant( "R3") == None )
    sr1 = None
    sr2 = None
    pass  

  def test_Reaction_setId(self):
    id = "J1"
    self.R.setId(id)
    self.assert_(( id == self.R.getId() ))
    self.assertEqual( True, self.R.isSetId() )
    if (self.R.getId() == id):
      pass    
    self.R.setId(self.R.getId())
    self.assert_(( id == self.R.getId() ))
    self.R.setId("")
    self.assertEqual( False, self.R.isSetId() )
    if (self.R.getId() != None):
      pass    
    pass  

  def test_Reaction_setName(self):
    name = "MapK Cascade"
    self.R.setName(name)
    self.assert_(( name == self.R.getName() ))
    self.assertEqual( True, self.R.isSetName() )
    if (self.R.getName() == name):
      pass    
    self.R.setName(self.R.getName())
    self.assert_(( name == self.R.getName() ))
    self.R.setName("")
    self.assertEqual( False, self.R.isSetName() )
    if (self.R.getName() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestReaction))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
