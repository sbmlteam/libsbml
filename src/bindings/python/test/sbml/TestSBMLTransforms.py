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
# Copyright 2005-2009 California Institute of Technology.
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
    ast = m.getReaction(2).getKineticLaw().getMath()
    self.assert_((  "f(S1, p) * compartmentOne / t" == libsbml.formulaToString(ast) ))
    fd = m.getFunctionDefinition(0)
    libsbml.SBMLTransforms.replaceFD(ast,fd)
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    ast = m.getReaction(1).getKineticLaw().getMath()
    self.assert_((  "f(f(S1, p), compartmentOne) / t" == libsbml.formulaToString(ast) ))
    libsbml.SBMLTransforms.replaceFD(ast,fd)
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    ast = m.getReaction(0).getKineticLaw().getMath()
    self.assert_((  "g(f(S1, p), compartmentOne) / t" == libsbml.formulaToString(ast) ))
    libsbml.SBMLTransforms.replaceFD(ast,fd)
    self.assert_((  "g(S1 * p, compartmentOne) / t" == libsbml.formulaToString(ast) ))
    fd = m.getFunctionDefinition(1)
    libsbml.SBMLTransforms.replaceFD(ast,fd)
    self.assert_((  "f(S1 * p, compartmentOne) / t" == libsbml.formulaToString(ast) ))
    ast = m.getReaction(0).getKineticLaw().getMath()
    lofd = m.getListOfFunctionDefinitions()
    libsbml.SBMLTransforms.replaceFD(ast,lofd)
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    d.expandFunctionDefinitions()
    self.assert_( d.getModel().getNumFunctionDefinitions() == 0 )
    ast = d.getModel().getReaction(0).getKineticLaw().getMath()
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    ast = d.getModel().getReaction(1).getKineticLaw().getMath()
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
    ast = d.getModel().getReaction(2).getKineticLaw().getMath()
    self.assert_((  "S1 * p * compartmentOne / t" == libsbml.formulaToString(ast) ))
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
