#
# @file    TestCVTerms.py
# @brief   CVTerms unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestCVTerms.c
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

class TestCVTerms(unittest.TestCase):


  def test_CVTerm_addResource(self):
    term = libsbml.CVTerm(libsbml.MODEL_QUALIFIER)
    resource =  "GO6666";
    self.assert_( term != None )
    self.assert_( term.getQualifierType() == libsbml.MODEL_QUALIFIER )
    term.addResource(resource)
    xa = term.getResources()
    self.assert_( xa.getLength() == 1 )
    self.assert_((  "rdf:resource" == xa.getName(0) ))
    self.assert_((  "GO6666" == xa.getValue(0) ))
    term = None
    pass  

  def test_CVTerm_create(self):
    term = libsbml.CVTerm(libsbml.MODEL_QUALIFIER)
    self.assert_( term != None )
    self.assert_( term.getQualifierType() == libsbml.MODEL_QUALIFIER )
    term = None
    pass  

  def test_CVTerm_createFromNode(self):
    qual_triple = libsbml.XMLTriple("is", "", "bqbiol")
    bag_triple = libsbml.XMLTriple()
    li_triple = libsbml.XMLTriple()
    att = libsbml.XMLAttributes()
    att.add( "", "This is my resource")
    att1 = libsbml.XMLAttributes()
    li_token = libsbml.XMLToken(li_triple,att)
    bag_token = libsbml.XMLToken(bag_triple,att1)
    qual_token = libsbml.XMLToken(qual_triple,att1)
    li = libsbml.XMLNode(li_token)
    bag = libsbml.XMLNode(bag_token)
    node = libsbml.XMLNode(qual_token)
    bag.addChild(li)
    node.addChild(bag)
    term = libsbml.CVTerm(node)
    self.assert_( term != None )
    self.assert_( term.getQualifierType() == libsbml.BIOLOGICAL_QUALIFIER )
    self.assert_( term.getBiologicalQualifierType() == libsbml.BQB_IS )
    xa = term.getResources()
    self.assert_( xa.getLength() == 1 )
    self.assert_((  "rdf:resource" == xa.getName(0) ))
    self.assert_((  "This is my resource" == xa.getValue(0) ))
    qual_triple = None
    bag_triple = None
    li_triple = None
    li_token = None
    bag_token = None
    qual_token = None
    att = None
    att1 = None
    term = None
    node = None
    bag = None
    li = None
    pass  

  def test_CVTerm_getResources(self):
    term = libsbml.CVTerm(libsbml.MODEL_QUALIFIER)
    resource =  "GO6666";
    resource1 =  "OtherURI";
    term.addResource(resource)
    term.addResource(resource1)
    number = term.getNumResources()
    self.assert_( number == 2 )
    self.assert_((  "GO6666" == term.getResourceURI(0) ))
    self.assert_((  "OtherURI" == term.getResourceURI(1) ))
    term = None
    pass  

  def test_CVTerm_set_get(self):
    term = libsbml.CVTerm(libsbml.MODEL_QUALIFIER)
    self.assert_( term != None )
    self.assert_( term.getQualifierType() == libsbml.MODEL_QUALIFIER )
    term.setModelQualifierType(libsbml.BQM_IS)
    self.assert_( term != None )
    self.assert_( term.getQualifierType() == libsbml.MODEL_QUALIFIER )
    self.assert_( term.getModelQualifierType() == libsbml.BQM_IS )
    term.setQualifierType(libsbml.BIOLOGICAL_QUALIFIER)
    term.setBiologicalQualifierType(libsbml.BQB_IS)
    self.assert_( term.getQualifierType() == libsbml.BIOLOGICAL_QUALIFIER )
    self.assert_( term.getBiologicalQualifierType() == libsbml.BQB_IS )
    term = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestCVTerms))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
