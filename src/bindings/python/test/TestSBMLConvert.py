#
# @file    TestSBMLConvert.py
# @brief   SBMLConvert unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestSBMLConvert.c
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

class TestSBMLConvert(unittest.TestCase):


  def test_SBMLConvert_addModifiersToReaction(self):
    d = libsbml.SBMLDocument(1,2)
    m = d.createModel()
    kl = libsbml.KineticLaw("k1*S1*S2*S3*S4*S5")
    r = libsbml.Reaction("R", "",kl,1)
    m.addSpecies(libsbml.Species("S1", ""))
    m.addSpecies(libsbml.Species("S2", ""))
    m.addSpecies(libsbml.Species("S3", ""))
    m.addSpecies(libsbml.Species("S4", ""))
    m.addSpecies(libsbml.Species("S5", ""))
    r.addReactant(libsbml.SpeciesReference("S1",1,1))
    r.addReactant(libsbml.SpeciesReference("S2",1,1))
    r.addProduct(libsbml.SpeciesReference("S5",1,1))
    m.addReaction(r)
    self.assert_( r.getNumModifiers() == 0 )
    d.setLevelAndVersion(2,1)
    self.assert_( d.getLevel() == 2 )
    self.assert_( d.getVersion() == 1 )
    self.assert_( m.getReaction(0).getNumModifiers() == 2 )
    ssr1 = m.getReaction(0).getModifier(0)
    ssr2 = m.getReaction(0).getModifier(1)
    self.assert_((  "S3" == ssr1.getSpecies() ))
    self.assert_((  "S4" == ssr2.getSpecies() ))
    d = None
    pass  

  def test_SBMLConvert_convertToL1_SBMLDocument(self):
    d = libsbml.SBMLDocument(2,1)
    d.setLevelAndVersion(1,2)
    self.assert_( d.getLevel() == 1 )
    self.assert_( d.getVersion() == 2 )
    d = None
    pass  

  def test_SBMLConvert_convertToL1_Species_Amount(self):
    d = libsbml.SBMLDocument(2,1)
    m = d.createModel()
    sid = "C"
    c = libsbml.Compartment()
    s = libsbml.Species()
    c.setId(sid)
    m.addCompartment(c)
    s.setCompartment(sid)
    s.setInitialAmount(2.34)
    m.addSpecies(s)
    d.setLevelAndVersion(1,2)
    self.assert_( s.getInitialAmount() == 2.34 )
    d = None
    pass  

  def test_SBMLConvert_convertToL1_Species_Concentration(self):
    d = libsbml.SBMLDocument(2,1)
    m = d.createModel()
    sid = "C"
    c = libsbml.Compartment()
    s = libsbml.Species()
    c.setId(sid)
    c.setSize(1.2)
    m.addCompartment(c)
    s.setCompartment(sid)
    s.setInitialConcentration(2.34)
    m.addSpecies(s)
    d.setLevelAndVersion(1,2)
    #ifndef CYGWIN
    self.assert_( m.getSpecies(0).getInitialAmount() == 2.808 )
    #endif
    s1 = m.getSpecies(0)
    self.assert_( s1 != None )
    self.assert_((  "C" == s1.getCompartment() ))
    self.assert_( m.getCompartment( "C").getSize() == 1.2 )
    self.assert_( s1.getInitialConcentration() == 2.34 )
    self.assert_( s1.isSetInitialConcentration() == True )
    d = None
    pass  

  def test_SBMLConvert_convertToL2_SBMLDocument(self):
    d = libsbml.SBMLDocument(1,2)
    d.setLevelAndVersion(2,1)
    self.assert_( d.getLevel() == 2 )
    self.assert_( d.getVersion() == 1 )
    d.setLevelAndVersion(2,2)
    self.assert_( d.getLevel() == 2 )
    self.assert_( d.getVersion() == 2 )
    d.setLevelAndVersion(2,3)
    self.assert_( d.getLevel() == 2 )
    self.assert_( d.getVersion() == 3 )
    d = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestSBMLConvert))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
