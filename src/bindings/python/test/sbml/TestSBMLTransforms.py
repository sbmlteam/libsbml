#
# @file    TestSBMLTransforms.py
# @brief   SBMLTransforms unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestSBMLTransforms.cpp
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
import math

DBL_EPSILON =  2.2204460492503131e-16

def equalDouble (a, b):
  return (abs(a-b) < math.sqrt(DBL_EPSILON))

def isnan(x):
  return (x != x)
  pass

class TestSBMLTransforms(unittest.TestCase):


  def test_SBMLTransforms_replaceFD(self):
    reader = libsbml.SBMLReader()
    filename = "../../sbml/test/test-data/"
    filename += "multiple-functions.xml"
    d = reader.readSBML(filename)
    if (d == None):
      pass    
    m = d.getModel()
    self.assert_( m.getNumFunctionDefinitions() == 2 )
    d.expandFunctionDefinitions()
    self.assert_( d.getModel().getNumFunctionDefinitions() == 0 )
    ast = d.getModel().getReaction(0).getKineticLaw().getMath()
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    ast = d.getModel().getReaction(1).getKineticLaw().getMath()
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    ast = d.getModel().getReaction(2).getKineticLaw().getMath()
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    pass  

  def test_SBMLTransforms_replaceIA(self):
    reader = libsbml.SBMLReader()
    filename = "../../sbml/test/test-data/"
    filename += "initialAssignments.xml"
    d = reader.readSBML(filename)
    if (d == None):
      pass    
    m = d.getModel()
    self.assert_( m.getNumInitialAssignments() == 2 )
    self.assertEqual( False, (m.getCompartment(0).isSetSize()) )
    self.assert_( m.getParameter(1).getValue() == 2 )
    d.expandInitialAssignments()
    self.assert_( d.getModel().getNumInitialAssignments() == 0 )
    self.assertEqual( True, d.getModel().getCompartment(0).isSetSize() )
    self.assert_( d.getModel().getCompartment(0).getSize() == 25.0 )
    self.assert_( m.getParameter(1).getValue() == 50 )
    pass  

  def test_SBMLTransforms_replaceIA_species(self):
    reader = libsbml.SBMLReader()
    filename = "../../sbml/test/test-data/"
    filename += "initialAssignments_species.xml"
    d = reader.readSBML(filename)
    if (d == None):
      pass    
    m = d.getModel()
    self.assert_( m.getNumInitialAssignments() == 3 )
    self.assert_( m.getParameter(1).getValue() == 0.75 )
    self.assertEqual( False, (m.getParameter(2).isSetValue()) )
    self.assertEqual( True, m.getSpecies(2).isSetInitialAmount() )
    self.assert_( m.getSpecies(2).getInitialAmount() == 2 )
    d.expandInitialAssignments()
    self.assert_( d.getModel().getNumInitialAssignments() == 0 )
    self.assert_( m.getParameter(1).getValue() == 3 )
    self.assertEqual( True, m.getParameter(2).isSetValue() )
    self.assert_( m.getParameter(2).getValue() == 0.75 )
    self.assertEqual( False, (m.getSpecies(2).isSetInitialAmount()) )
    self.assert_( m.getSpecies(2).getInitialConcentration() == 2 )
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestSBMLTransforms))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
