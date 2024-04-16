#
# @file    TestMathReadFromFile1.py
# @brief   Tests for reading MathML from files into ASTNodes.
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
# 
# ====== WARNING ===== WARNING ===== WARNING ===== WARNING ===== WARNING ======
#
# DO NOT EDIT THIS FILE.
#
# This file was generated automatically by converting the file located at
# src/math/test/TestReadFromFile1.cpp
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


class TestMathReadFromFile1(unittest.TestCase):


  def test_read_MathML_1(self):
    reader = libsbml.SBMLReader()
    filename = "../../sbml/math/test/test-data/"
    filename += "mathML_1-invalid.xml"
    d = reader.readSBML(filename)
    if (d == None):
      pass    
    m = d.getModel()
    self.assertTrue( m != None )
    self.assertTrue( m.getNumFunctionDefinitions() == 2 )
    self.assertTrue( m.getNumInitialAssignments() == 1 )
    self.assertTrue( m.getNumRules() == 2 )
    self.assertTrue( m.getNumReactions() == 1 )
    fd = m.getFunctionDefinition(0)
    fd_math = fd.getMath()
    self.assertTrue( fd_math.getType() == libsbml.AST_LAMBDA )
    self.assertTrue( fd_math.getNumChildren() == 2 )
    self.assertTrue((  "lambda(x, )" == libsbml.formulaToString(fd_math) ))
    self.assertTrue( fd_math.getParentSBMLObject() == fd )
    child = fd_math.getRightChild()
    self.assertTrue( child.getType() == libsbml.AST_UNKNOWN )
    self.assertTrue( child.getNumChildren() == 0 )
    self.assertTrue((  "" == libsbml.formulaToString(child) ))
    fd = m.getFunctionDefinition(1)
    fd1_math = fd.getMath()
    self.assertTrue( fd1_math.getType() == libsbml.AST_LAMBDA )
    self.assertTrue( fd1_math.getNumChildren() == 2 )
    self.assertTrue((  "lambda(x, true)" == libsbml.formulaToString(fd1_math) ))
    self.assertTrue( fd1_math.getParentSBMLObject() == fd )
    child1 = fd1_math.getRightChild()
    self.assertTrue( child1.getType() == libsbml.AST_CONSTANT_TRUE )
    self.assertTrue( child1.getNumChildren() == 0 )
    self.assertTrue((  "true" == libsbml.formulaToString(child1) ))
    ia = m.getInitialAssignment(0)
    ia_math = ia.getMath()
    self.assertTrue( ia_math.getType() == libsbml.AST_UNKNOWN )
    self.assertTrue( ia_math.getNumChildren() == 0 )
    self.assertTrue((  "" == libsbml.formulaToString(ia_math) ))
    self.assertTrue( ia_math.getParentSBMLObject() == ia )
    r = m.getRule(0)
    r_math = r.getMath()
    self.assertTrue( r_math.getType() == libsbml.AST_CONSTANT_TRUE )
    self.assertTrue( r_math.getNumChildren() == 0 )
    self.assertTrue((  "true" == libsbml.formulaToString(r_math) ))
    self.assertTrue( r_math.getParentSBMLObject() == r )
    r = m.getRule(1)
    r1_math = r.getMath()
    self.assertTrue( r1_math.getType() == libsbml.AST_REAL )
    self.assertTrue( r1_math.getNumChildren() == 0 )
    self.assertTrue((  "INF" == libsbml.formulaToString(r1_math) ))
    self.assertTrue( r1_math.getParentSBMLObject() == r )
    # kl = m.getReaction(0).getKineticLaw()
    # kl_math = kl.getMath()
    # self.assertTrue( kl_math == None )
    d = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.TestLoader.loadTestsFromTestCase(TestMathReadFromFile1))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)

