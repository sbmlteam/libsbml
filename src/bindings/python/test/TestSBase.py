#
# @file    TestSBase.py
# @brief   SBase unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestSBase.cpp
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

class TestSBase(unittest.TestCase):

  S = None

  def setUp(self):
    self.S = libsbml.Model()
    if (self.S == None):
      pass    
    pass  

  def tearDown(self):
    self.S = None
    pass  

  def test_SBase_CVTerms(self):
    cv = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    self.assert_( self.S.getNumCVTerms() == 0 )
    self.S.addCVTerm(cv)
    self.assert_( self.S.getNumCVTerms() == 1 )
    self.assert_( self.S.getCVTerm(0) != cv )
    cv = None
    pass  

  def test_SBase_addCVTerms(self):
    cv = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv.setBiologicalQualifierType(libsbml.BQB_ENCODES)
    cv.addResource( "foo")
    self.S.addCVTerm(cv)
    self.assert_( self.S.getNumCVTerms() == 1 )
    res = self.S.getCVTerm(0).getResources()
    self.assert_((  "foo" == res.getValue(0) ))
    cv1 = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv1.setBiologicalQualifierType(libsbml.BQB_IS)
    cv1.addResource( "bar")
    self.S.addCVTerm(cv1)
    self.assert_( self.S.getNumCVTerms() == 2 )
    cv2 = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv2.setBiologicalQualifierType(libsbml.BQB_IS)
    cv2.addResource( "bar1")
    self.S.addCVTerm(cv2)
    self.assert_( self.S.getNumCVTerms() == 2 )
    res = self.S.getCVTerm(1).getResources()
    self.assert_( res.getLength() == 2 )
    self.assert_((  "bar" == res.getValue(0) ))
    self.assert_((  "bar1" == res.getValue(1) ))
    cv4 = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv4.setBiologicalQualifierType(libsbml.BQB_IS)
    cv4.addResource( "bar1")
    self.S.addCVTerm(cv4)
    self.assert_( self.S.getNumCVTerms() == 2 )
    res = self.S.getCVTerm(1).getResources()
    self.assert_( res.getLength() == 2 )
    self.assert_((  "bar" == res.getValue(0) ))
    self.assert_((  "bar1" == res.getValue(1) ))
    cv5 = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv5.setBiologicalQualifierType(libsbml.BQB_HAS_PART)
    cv5.addResource( "bar1")
    self.S.addCVTerm(cv5)
    self.assert_( self.S.getNumCVTerms() == 2 )
    res = self.S.getCVTerm(1).getResources()
    self.assert_( res.getLength() == 2 )
    self.assert_((  "bar" == res.getValue(0) ))
    self.assert_((  "bar1" == res.getValue(1) ))
    cv = None
    cv2 = None
    cv1 = None
    cv4 = None
    pass  

  def test_SBase_getQualifiersFromResources(self):
    cv = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv.setBiologicalQualifierType(libsbml.BQB_ENCODES)
    cv.addResource( "foo")
    self.S.addCVTerm(cv)
    self.assert_( self.S.getResourceBiologicalQualifier( "foo") == libsbml.BQB_ENCODES )
    cv1 = libsbml.CVTerm(libsbml.MODEL_QUALIFIER)
    cv1.setModelQualifierType(libsbml.BQM_IS)
    cv1.addResource( "bar")
    self.S.addCVTerm(cv1)
    self.assert_( self.S.getResourceModelQualifier( "bar") == libsbml.BQM_IS )
    cv = None
    cv1 = None
    pass  

  def test_SBase_setAnnotation(self):
    token = libsbml.XMLToken("This is a test note")
    node = libsbml.XMLNode(token)
    self.S.setAnnotation(node)
    self.assert_( self.S.isSetAnnotation() == True )
    t1 = self.S.getAnnotation()
    self.assert_( t1.getNumChildren() == 1 )
    self.assert_((  "This is a test note" == t1.getChild(0).getCharacters() ))
    if (self.S.getAnnotation() == node):
      pass    
    self.S.setAnnotation(self.S.getAnnotation())
    self.assert_((  "This is a test note" == self.S.getAnnotation().getChild(0).getCharacters() ))
    self.S.setAnnotation(None)
    self.assert_( self.S.isSetAnnotation() == False )
    if (self.S.getAnnotation() != None):
      pass    
    self.S.setAnnotation(node)
    self.assert_( self.S.isSetAnnotation() == True )
    self.S.unsetAnnotation()
    self.assert_( self.S.isSetAnnotation() == False )
    pass  

  def test_SBase_setAnnotationString(self):
    annotation = "This is a test note"
    taggedannotation = "<annotation>This is a test note</annotation>"
    self.S.setAnnotation(annotation)
    self.assert_( self.S.isSetAnnotation() == True )
    if (( taggedannotation != self.S.getAnnotationString() )):
      pass    
    t1 = self.S.getAnnotation()
    self.assert_( t1.getNumChildren() == 1 )
    t2 = t1.getChild(0)
    self.assert_((  "This is a test note" == t2.getChild(0).getCharacters() ))
    self.S.setAnnotation(self.S.getAnnotationString())
    t1 = self.S.getAnnotation()
    self.assert_( t1.getNumChildren() == 1 )
    chars = self.S.getAnnotationString()
    self.assert_(( taggedannotation == chars ))
    self.S.setAnnotation( "")
    self.assert_( self.S.isSetAnnotation() == False )
    if (self.S.getAnnotationString() != None):
      pass    
    self.S.setAnnotation(taggedannotation)
    self.assert_( self.S.isSetAnnotation() == True )
    if (( taggedannotation != self.S.getAnnotationString() )):
      pass    
    t1 = self.S.getAnnotation()
    self.assert_( t1.getNumChildren() == 1 )
    t2 = t1.getChild(0)
    self.assert_((  "This is a test note" == t2.getCharacters() ))
    pass  

  def test_SBase_setMetaId(self):
    metaid = "x12345"
    self.S.setMetaId(metaid)
    self.assert_(( metaid == self.S.getMetaId() ))
    self.assertEqual( True, self.S.isSetMetaId() )
    if (self.S.getMetaId() == metaid):
      pass    
    self.S.setMetaId(self.S.getMetaId())
    self.assert_(( metaid == self.S.getMetaId() ))
    self.S.setMetaId("")
    self.assertEqual( False, self.S.isSetMetaId() )
    if (self.S.getMetaId() != None):
      pass    
    pass  

  def test_SBase_setNotes(self):
    token = libsbml.XMLToken("This is a test note")
    node = libsbml.XMLNode(token)
    self.S.setNotes(node)
    self.assert_( self.S.isSetNotes() == True )
    if (self.S.getNotes() == node):
      pass    
    t1 = self.S.getNotes()
    self.assert_( t1.getNumChildren() == 1 )
    self.assert_((  "This is a test note" == t1.getChild(0).getCharacters() ))
    self.S.setNotes(self.S.getNotes())
    t1 = self.S.getNotes()
    self.assert_( t1.getNumChildren() == 1 )
    chars = t1.getChild(0).getCharacters()
    self.assert_((  "This is a test note" == chars ))
    self.S.setNotes(None)
    self.assert_( self.S.isSetNotes() == False )
    if (self.S.getNotes() != None):
      pass    
    self.S.setNotes(node)
    self.assert_( self.S.isSetNotes() == True )
    node = None
    pass  

  def test_SBase_setNotesString(self):
    notes = "This is a test note"
    taggednotes = "<notes>This is a test note</notes>"
    self.S.setNotes(notes)
    self.assert_( self.S.isSetNotes() == True )
    if (( taggednotes != self.S.getNotesString() )):
      pass    
    t1 = self.S.getNotes()
    self.assert_( t1.getNumChildren() == 1 )
    t2 = t1.getChild(0)
    self.assert_((  "This is a test note" == t2.getChild(0).getCharacters() ))
    self.S.setNotes(self.S.getNotesString())
    t1 = self.S.getNotes()
    self.assert_( t1.getNumChildren() == 1 )
    chars = self.S.getNotesString()
    self.assert_(( taggednotes == chars ))
    self.S.setNotes( "")
    self.assert_( self.S.isSetNotes() == False )
    if (self.S.getNotesString() != None):
      pass    
    self.S.setNotes(taggednotes)
    self.assert_( self.S.isSetNotes() == True )
    if (( taggednotes != self.S.getNotesString() )):
      pass    
    t1 = self.S.getNotes()
    self.assert_( t1.getNumChildren() == 1 )
    t2 = t1.getChild(0)
    self.assert_((  "This is a test note" == t2.getCharacters() ))
    pass  

  def test_SBase_unsetCVTerms(self):
    cv = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv.setBiologicalQualifierType(libsbml.BQB_ENCODES)
    cv.addResource( "foo")
    self.S.addCVTerm(cv)
    cv1 = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv1.setBiologicalQualifierType(libsbml.BQB_IS)
    cv1.addResource( "bar")
    self.S.addCVTerm(cv1)
    cv2 = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv2.setBiologicalQualifierType(libsbml.BQB_IS)
    cv2.addResource( "bar1")
    self.S.addCVTerm(cv2)
    cv4 = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv4.setBiologicalQualifierType(libsbml.BQB_IS)
    cv4.addResource( "bar1")
    self.S.addCVTerm(cv4)
    self.assert_( self.S.getNumCVTerms() == 2 )
    self.S.unsetCVTerms()
    self.assert_( self.S.getNumCVTerms() == 0 )
    cv = None
    cv2 = None
    cv1 = None
    cv4 = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestSBase))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
