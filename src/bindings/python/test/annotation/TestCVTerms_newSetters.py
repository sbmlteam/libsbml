#
# @file    TestCVTerms_newSetters.py
# @brief   CVTerms unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestCVTerms_newSetters.c
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

class TestCVTerms_newSetters(unittest.TestCase):


  def test_CVTerm_addResource(self):
    term = libsbml.CVTerm(libsbml.MODEL_QUALIFIER)
    resource =  "GO6666";
    self.assert_( term != None )
    self.assert_( term.getQualifierType() == libsbml.MODEL_QUALIFIER )
    i = term.addResource( "")
    self.assert_( i == libsbml.LIBSBML_OPERATION_FAILED )
    xa = term.getResources()
    self.assert_( xa.getLength() == 0 )
    i = term.addResource(resource)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    xa = term.getResources()
    self.assert_( xa.getLength() == 1 )
    self.assert_((  "rdf:resource" == xa.getName(0) ))
    self.assert_((  "GO6666" == xa.getValue(0) ))
    term = None
    pass  

  def test_CVTerm_removeResource(self):
    term = libsbml.CVTerm(libsbml.MODEL_QUALIFIER)
    resource =  "GO6666";
    self.assert_( term != None )
    self.assert_( term.getQualifierType() == libsbml.MODEL_QUALIFIER )
    term.addResource(resource)
    xa = term.getResources()
    self.assert_( xa.getLength() == 1 )
    i = term.removeResource( "CCC")
    self.assert_( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE )
    xa = term.getResources()
    self.assert_( xa.getLength() == 1 )
    i = term.removeResource(resource)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    xa = term.getResources()
    self.assert_( xa.getLength() == 0 )
    term = None
    pass  

  def test_CVTerm_setBiolQualifierType(self):
    term = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    self.assert_( term != None )
    self.assert_( term.getQualifierType() == libsbml.BIOLOGICAL_QUALIFIER )
    self.assert_( term.getModelQualifierType() == libsbml.BQM_UNKNOWN )
    self.assert_( term.getBiologicalQualifierType() == libsbml.BQB_UNKNOWN )
    i = term.setBiologicalQualifierType(libsbml.BQB_IS)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( term.getQualifierType() == libsbml.BIOLOGICAL_QUALIFIER )
    self.assert_( term.getBiologicalQualifierType() == libsbml.BQB_IS )
    self.assert_( term.getModelQualifierType() == libsbml.BQM_UNKNOWN )
    i = term.setQualifierType(libsbml.MODEL_QUALIFIER)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( term.getQualifierType() == libsbml.MODEL_QUALIFIER )
    self.assert_( term.getModelQualifierType() == libsbml.BQM_UNKNOWN )
    self.assert_( term.getBiologicalQualifierType() == libsbml.BQB_UNKNOWN )
    i = term.setBiologicalQualifierType(libsbml.BQB_IS)
    self.assert_( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE )
    self.assert_( term.getQualifierType() == libsbml.MODEL_QUALIFIER )
    self.assert_( term.getModelQualifierType() == libsbml.BQM_UNKNOWN )
    self.assert_( term.getBiologicalQualifierType() == libsbml.BQB_UNKNOWN )
    term = None
    pass  

  def test_CVTerm_setModelQualifierType(self):
    term = libsbml.CVTerm(libsbml.MODEL_QUALIFIER)
    self.assert_( term != None )
    self.assert_( term.getQualifierType() == libsbml.MODEL_QUALIFIER )
    self.assert_( term.getModelQualifierType() == libsbml.BQM_UNKNOWN )
    self.assert_( term.getBiologicalQualifierType() == libsbml.BQB_UNKNOWN )
    i = term.setModelQualifierType(libsbml.BQM_IS)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( term.getQualifierType() == libsbml.MODEL_QUALIFIER )
    self.assert_( term.getModelQualifierType() == libsbml.BQM_IS )
    self.assert_( term.getBiologicalQualifierType() == libsbml.BQB_UNKNOWN )
    i = term.setQualifierType(libsbml.BIOLOGICAL_QUALIFIER)
    self.assert_( i == libsbml.LIBSBML_OPERATION_SUCCESS )
    self.assert_( term.getQualifierType() == libsbml.BIOLOGICAL_QUALIFIER )
    self.assert_( term.getModelQualifierType() == libsbml.BQM_UNKNOWN )
    self.assert_( term.getBiologicalQualifierType() == libsbml.BQB_UNKNOWN )
    i = term.setModelQualifierType(libsbml.BQM_IS)
    self.assert_( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE )
    self.assert_( term.getQualifierType() == libsbml.BIOLOGICAL_QUALIFIER )
    self.assert_( term.getBiologicalQualifierType() == libsbml.BQB_UNKNOWN )
    self.assert_( term.getModelQualifierType() == libsbml.BQM_UNKNOWN )
    term = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestCVTerms_newSetters))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
