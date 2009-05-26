#
# @file    TestSBase.py
# @brief   SBase unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestSBase.cpp
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

def wrapString(s):
  return s
  pass

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
    self.assert_( self.S.getCVTerms() == None )
    self.S.setMetaId( "sbase1")
    self.S.addCVTerm(cv)
    self.assert_( self.S.getNumCVTerms() == 1 )
    self.assert_( self.S.getCVTerms() != None )
    self.assert_( self.S.getCVTerm(0) != cv )
    cv = None
    pass  

  def test_SBase_addCVTerms(self):
    cv = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv.setBiologicalQualifierType(libsbml.BQB_ENCODES)
    cv.addResource( "foo")
    self.S.setMetaId( "sbase1")
    self.S.addCVTerm(cv)
    self.assert_( self.S.getNumCVTerms() == 1 )
    self.assert_( self.S.getCVTerms() != None )
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

  def test_SBase_appendNotes(self):
    triple = libsbml.XMLTriple("p", "", "")
    att = libsbml.XMLAttributes()
    ns = libsbml.XMLNamespaces()
    ns.add( "http://www.w3.org/1999/xhtml", "")
    token4 = libsbml.XMLToken("This is my text")
    node4 = libsbml.XMLNode(token4)
    token5 = libsbml.XMLToken("This is additional text")
    node5 = libsbml.XMLNode(token5)
    token = libsbml.XMLToken(triple,att,ns)
    node = libsbml.XMLNode(token)
    node.addChild(node4)
    self.S.setNotes(node)
    self.assert_( self.S.isSetNotes() == True )
    token1 = libsbml.XMLToken(triple,att,ns)
    node1 = libsbml.XMLNode(token1)
    node1.addChild(node5)
    self.S.appendNotes(node1)
    self.assert_( self.S.isSetNotes() == True )
    node2 = self.S.getNotes()
    self.assert_( node2.getNumChildren() == 2 )
    self.assert_((  "p" == node2.getChild(0).getName() ))
    self.assert_( node2.getChild(0).getNumChildren() == 1 )
    self.assert_((  "p" == node2.getChild(1).getName() ))
    self.assert_( node2.getChild(1).getNumChildren() == 1 )
    chars1 = node2.getChild(0).getChild(0).getCharacters()
    chars2 = node2.getChild(1).getChild(0).getCharacters()
    self.assert_((  "This is my text" == chars1 ))
    self.assert_((  "This is additional text" == chars2 ))
    node = None
    node1 = None
    pass  

  def test_SBase_appendNotes1(self):
    att = libsbml.XMLAttributes()
    ns = libsbml.XMLNamespaces()
    ns.add( "http://www.w3.org/1999/xhtml", "")
    html_triple = libsbml.XMLTriple("html", "", "")
    head_triple = libsbml.XMLTriple("head", "", "")
    body_triple = libsbml.XMLTriple("body", "", "")
    p_triple = libsbml.XMLTriple("p", "", "")
    html_token = libsbml.XMLToken(html_triple,att,ns)
    head_token = libsbml.XMLToken(head_triple,att)
    body_token = libsbml.XMLToken(body_triple,att)
    p_token = libsbml.XMLToken(p_triple,att)
    text_token = libsbml.XMLToken("This is my text")
    html_node = libsbml.XMLNode(html_token)
    head_node = libsbml.XMLNode(head_token)
    body_node = libsbml.XMLNode(body_token)
    p_node = libsbml.XMLNode(p_token)
    text_node = libsbml.XMLNode(text_token)
    text_token1 = libsbml.XMLToken("This is more text")
    html_node1 = libsbml.XMLNode(html_token)
    head_node1 = libsbml.XMLNode(head_token)
    body_node1 = libsbml.XMLNode(body_token)
    p_node1 = libsbml.XMLNode(p_token)
    text_node1 = libsbml.XMLNode(text_token1)
    p_node.addChild(text_node)
    body_node.addChild(p_node)
    html_node.addChild(head_node)
    html_node.addChild(body_node)
    p_node1.addChild(text_node1)
    body_node1.addChild(p_node1)
    html_node1.addChild(head_node1)
    html_node1.addChild(body_node1)
    self.S.setNotes(html_node)
    self.S.appendNotes(html_node1)
    notes = self.S.getNotes()
    self.assert_((  "notes" == notes.getName() ))
    self.assert_( notes.getNumChildren() == 1 )
    child = notes.getChild(0)
    self.assert_((  "html" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child = child.getChild(1)
    self.assert_((  "body" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child1 = child.getChild(0)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is my text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    child1 = child.getChild(1)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is more text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    att = None
    ns = None
    html_triple = None
    head_triple = None
    body_triple = None
    p_triple = None
    html_token = None
    head_token = None
    body_token = None
    p_token = None
    text_token = None
    text_token1 = None
    html_node = None
    head_node = None
    body_node = None
    p_node = None
    text_node = None
    html_node1 = None
    head_node1 = None
    body_node1 = None
    p_node1 = None
    text_node1 = None
    pass  

  def test_SBase_appendNotes2(self):
    att = libsbml.XMLAttributes()
    ns = libsbml.XMLNamespaces()
    ns.add( "http://www.w3.org/1999/xhtml", "")
    html_triple = libsbml.XMLTriple("html", "", "")
    head_triple = libsbml.XMLTriple("head", "", "")
    body_triple = libsbml.XMLTriple("body", "", "")
    p_triple = libsbml.XMLTriple("p", "", "")
    html_token = libsbml.XMLToken(html_triple,att,ns)
    head_token = libsbml.XMLToken(head_triple,att)
    body_token = libsbml.XMLToken(body_triple,att)
    p_token = libsbml.XMLToken(p_triple,att)
    text_token = libsbml.XMLToken("This is my text")
    html_node = libsbml.XMLNode(html_token)
    head_node = libsbml.XMLNode(head_token)
    body_node = libsbml.XMLNode(body_token)
    p_node = libsbml.XMLNode(p_token)
    text_node = libsbml.XMLNode(text_token)
    body_token1 = libsbml.XMLToken(body_triple,att,ns)
    text_token1 = libsbml.XMLToken("This is more text")
    body_node1 = libsbml.XMLNode(body_token1)
    p_node1 = libsbml.XMLNode(p_token)
    text_node1 = libsbml.XMLNode(text_token1)
    p_node.addChild(text_node)
    body_node.addChild(p_node)
    html_node.addChild(head_node)
    html_node.addChild(body_node)
    p_node1.addChild(text_node1)
    body_node1.addChild(p_node1)
    self.S.setNotes(html_node)
    self.S.appendNotes(body_node1)
    notes = self.S.getNotes()
    self.assert_((  "notes" == notes.getName() ))
    self.assert_( notes.getNumChildren() == 1 )
    child = notes.getChild(0)
    self.assert_((  "html" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child = child.getChild(1)
    self.assert_((  "body" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child1 = child.getChild(0)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is my text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    child1 = child.getChild(1)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is more text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    att = None
    ns = None
    html_triple = None
    head_triple = None
    body_triple = None
    p_triple = None
    html_token = None
    head_token = None
    body_token = None
    p_token = None
    text_token = None
    text_token1 = None
    body_token1 = None
    html_node = None
    head_node = None
    body_node = None
    p_node = None
    text_node = None
    body_node1 = None
    p_node1 = None
    text_node1 = None
    pass  

  def test_SBase_appendNotes3(self):
    att = libsbml.XMLAttributes()
    ns = libsbml.XMLNamespaces()
    ns.add( "http://www.w3.org/1999/xhtml", "")
    html_triple = libsbml.XMLTriple("html", "", "")
    head_triple = libsbml.XMLTriple("head", "", "")
    body_triple = libsbml.XMLTriple("body", "", "")
    p_triple = libsbml.XMLTriple("p", "", "")
    html_token = libsbml.XMLToken(html_triple,att,ns)
    head_token = libsbml.XMLToken(head_triple,att)
    body_token = libsbml.XMLToken(body_triple,att)
    p_token = libsbml.XMLToken(p_triple,att)
    text_token = libsbml.XMLToken("This is my text")
    html_node = libsbml.XMLNode(html_token)
    head_node = libsbml.XMLNode(head_token)
    body_node = libsbml.XMLNode(body_token)
    p_node = libsbml.XMLNode(p_token)
    text_node = libsbml.XMLNode(text_token)
    p_token1 = libsbml.XMLToken(p_triple,att,ns)
    text_token1 = libsbml.XMLToken("This is more text")
    p_node1 = libsbml.XMLNode(p_token1)
    text_node1 = libsbml.XMLNode(text_token1)
    p_node.addChild(text_node)
    body_node.addChild(p_node)
    html_node.addChild(head_node)
    html_node.addChild(body_node)
    p_node1.addChild(text_node1)
    self.S.setNotes(html_node)
    self.S.appendNotes(p_node1)
    notes = self.S.getNotes()
    self.assert_((  "notes" == notes.getName() ))
    self.assert_( notes.getNumChildren() == 1 )
    child = notes.getChild(0)
    self.assert_((  "html" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child = child.getChild(1)
    self.assert_((  "body" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child1 = child.getChild(0)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is my text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    child1 = child.getChild(1)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is more text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    att = None
    ns = None
    html_triple = None
    head_triple = None
    body_triple = None
    p_triple = None
    html_token = None
    head_token = None
    body_token = None
    p_token = None
    text_token = None
    text_token1 = None
    p_token1 = None
    html_node = None
    head_node = None
    body_node = None
    p_node = None
    text_node = None
    p_node1 = None
    text_node1 = None
    pass  

  def test_SBase_appendNotes4(self):
    att = libsbml.XMLAttributes()
    ns = libsbml.XMLNamespaces()
    ns.add( "http://www.w3.org/1999/xhtml", "")
    html_triple = libsbml.XMLTriple("html", "", "")
    head_triple = libsbml.XMLTriple("head", "", "")
    body_triple = libsbml.XMLTriple("body", "", "")
    p_triple = libsbml.XMLTriple("p", "", "")
    html_token = libsbml.XMLToken(html_triple,att,ns)
    head_token = libsbml.XMLToken(head_triple,att)
    body_token = libsbml.XMLToken(body_triple,att)
    p_token = libsbml.XMLToken(p_triple,att)
    body_token1 = libsbml.XMLToken(body_triple,att,ns)
    text_token = libsbml.XMLToken("This is my text")
    body_node = libsbml.XMLNode(body_token1)
    p_node = libsbml.XMLNode(p_token)
    text_node = libsbml.XMLNode(text_token)
    text_token1 = libsbml.XMLToken("This is more text")
    html_node1 = libsbml.XMLNode(html_token)
    head_node1 = libsbml.XMLNode(head_token)
    body_node1 = libsbml.XMLNode(body_token)
    p_node1 = libsbml.XMLNode(p_token)
    text_node1 = libsbml.XMLNode(text_token1)
    p_node.addChild(text_node)
    body_node.addChild(p_node)
    p_node1.addChild(text_node1)
    body_node1.addChild(p_node1)
    html_node1.addChild(head_node1)
    html_node1.addChild(body_node1)
    self.S.setNotes(body_node)
    self.S.appendNotes(html_node1)
    notes = self.S.getNotes()
    self.assert_((  "notes" == notes.getName() ))
    self.assert_( notes.getNumChildren() == 1 )
    child = notes.getChild(0)
    self.assert_((  "html" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child = child.getChild(1)
    self.assert_((  "body" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child1 = child.getChild(0)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is my text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    child1 = child.getChild(1)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is more text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    att = None
    ns = None
    html_triple = None
    head_triple = None
    body_triple = None
    p_triple = None
    body_token = None
    p_token = None
    text_token = None
    text_token1 = None
    body_token1 = None
    body_node = None
    p_node = None
    text_node = None
    html_node1 = None
    head_node1 = None
    body_node1 = None
    p_node1 = None
    text_node1 = None
    pass  

  def test_SBase_appendNotes5(self):
    att = libsbml.XMLAttributes()
    ns = libsbml.XMLNamespaces()
    ns.add( "http://www.w3.org/1999/xhtml", "")
    html_triple = libsbml.XMLTriple("html", "", "")
    head_triple = libsbml.XMLTriple("head", "", "")
    body_triple = libsbml.XMLTriple("body", "", "")
    p_triple = libsbml.XMLTriple("p", "", "")
    html_token = libsbml.XMLToken(html_triple,att,ns)
    head_token = libsbml.XMLToken(head_triple,att)
    body_token = libsbml.XMLToken(body_triple,att)
    p_token = libsbml.XMLToken(p_triple,att)
    p_token1 = libsbml.XMLToken(p_triple,att,ns)
    text_token = libsbml.XMLToken("This is my text")
    p_node = libsbml.XMLNode(p_token1)
    text_node = libsbml.XMLNode(text_token)
    text_token1 = libsbml.XMLToken("This is more text")
    html_node1 = libsbml.XMLNode(html_token)
    head_node1 = libsbml.XMLNode(head_token)
    body_node1 = libsbml.XMLNode(body_token)
    p_node1 = libsbml.XMLNode(p_token)
    text_node1 = libsbml.XMLNode(text_token1)
    p_node.addChild(text_node)
    p_node1.addChild(text_node1)
    body_node1.addChild(p_node1)
    html_node1.addChild(head_node1)
    html_node1.addChild(body_node1)
    self.S.setNotes(p_node)
    self.S.appendNotes(html_node1)
    notes = self.S.getNotes()
    self.assert_((  "notes" == notes.getName() ))
    self.assert_( notes.getNumChildren() == 1 )
    child = notes.getChild(0)
    self.assert_((  "html" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child = child.getChild(1)
    self.assert_((  "body" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child1 = child.getChild(0)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is my text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    child1 = child.getChild(1)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is more text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    att = None
    ns = None
    html_triple = None
    head_triple = None
    body_triple = None
    p_triple = None
    body_token = None
    p_token = None
    p_token1 = None
    text_token = None
    text_token1 = None
    p_node = None
    text_node = None
    html_node1 = None
    head_node1 = None
    body_node1 = None
    p_node1 = None
    text_node1 = None
    pass  

  def test_SBase_appendNotes6(self):
    att = libsbml.XMLAttributes()
    ns = libsbml.XMLNamespaces()
    ns.add( "http://www.w3.org/1999/xhtml", "")
    body_triple = libsbml.XMLTriple("body", "", "")
    p_triple = libsbml.XMLTriple("p", "", "")
    body_token = libsbml.XMLToken(body_triple,att,ns)
    p_token = libsbml.XMLToken(p_triple,att)
    text_token = libsbml.XMLToken("This is my text")
    body_node = libsbml.XMLNode(body_token)
    p_node = libsbml.XMLNode(p_token)
    text_node = libsbml.XMLNode(text_token)
    text_token1 = libsbml.XMLToken("This is more text")
    body_node1 = libsbml.XMLNode(body_token)
    p_node1 = libsbml.XMLNode(p_token)
    text_node1 = libsbml.XMLNode(text_token1)
    p_node.addChild(text_node)
    body_node.addChild(p_node)
    p_node1.addChild(text_node1)
    body_node1.addChild(p_node1)
    self.S.setNotes(body_node)
    self.S.appendNotes(body_node1)
    notes = self.S.getNotes()
    self.assert_((  "notes" == notes.getName() ))
    self.assert_( notes.getNumChildren() == 1 )
    child = notes.getChild(0)
    self.assert_((  "body" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child1 = child.getChild(0)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is my text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    child1 = child.getChild(1)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is more text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    att = None
    ns = None
    body_triple = None
    p_triple = None
    body_token = None
    p_token = None
    text_token = None
    text_token1 = None
    body_node = None
    p_node = None
    text_node = None
    body_node1 = None
    p_node1 = None
    text_node1 = None
    pass  

  def test_SBase_appendNotes7(self):
    att = libsbml.XMLAttributes()
    ns = libsbml.XMLNamespaces()
    ns.add( "http://www.w3.org/1999/xhtml", "")
    body_triple = libsbml.XMLTriple("body", "", "")
    p_triple = libsbml.XMLTriple("p", "", "")
    body_token = libsbml.XMLToken(body_triple,att,ns)
    p_token1 = libsbml.XMLToken(p_triple,att,ns)
    text_token = libsbml.XMLToken("This is my text")
    p_token = libsbml.XMLToken(p_triple,att)
    p_node = libsbml.XMLNode(p_token1)
    text_node = libsbml.XMLNode(text_token)
    text_token1 = libsbml.XMLToken("This is more text")
    body_node1 = libsbml.XMLNode(body_token)
    p_node1 = libsbml.XMLNode(p_token)
    text_node1 = libsbml.XMLNode(text_token1)
    p_node.addChild(text_node)
    p_node1.addChild(text_node1)
    body_node1.addChild(p_node1)
    self.S.setNotes(p_node)
    self.S.appendNotes(body_node1)
    notes = self.S.getNotes()
    self.assert_((  "notes" == notes.getName() ))
    self.assert_( notes.getNumChildren() == 1 )
    child = notes.getChild(0)
    self.assert_((  "body" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child1 = child.getChild(0)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is my text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    child1 = child.getChild(1)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is more text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    att = None
    ns = None
    body_triple = None
    p_triple = None
    body_token = None
    p_token = None
    p_token1 = None
    text_token = None
    text_token1 = None
    p_node = None
    text_node = None
    body_node1 = None
    p_node1 = None
    text_node1 = None
    pass  

  def test_SBase_appendNotes8(self):
    att = libsbml.XMLAttributes()
    ns = libsbml.XMLNamespaces()
    ns.add( "http://www.w3.org/1999/xhtml", "")
    body_triple = libsbml.XMLTriple("body", "", "")
    p_triple = libsbml.XMLTriple("p", "", "")
    body_token = libsbml.XMLToken(body_triple,att,ns)
    p_token = libsbml.XMLToken(p_triple,att)
    text_token = libsbml.XMLToken("This is my text")
    body_node = libsbml.XMLNode(body_token)
    p_node = libsbml.XMLNode(p_token)
    text_node = libsbml.XMLNode(text_token)
    p_token1 = libsbml.XMLToken(p_triple,att,ns)
    text_token1 = libsbml.XMLToken("This is more text")
    p_node1 = libsbml.XMLNode(p_token1)
    text_node1 = libsbml.XMLNode(text_token1)
    p_node.addChild(text_node)
    body_node.addChild(p_node)
    p_node1.addChild(text_node1)
    self.S.setNotes(body_node)
    self.S.appendNotes(p_node1)
    notes = self.S.getNotes()
    self.assert_((  "notes" == notes.getName() ))
    self.assert_( notes.getNumChildren() == 1 )
    child = notes.getChild(0)
    self.assert_((  "body" == child.getName() ))
    self.assert_( child.getNumChildren() == 2 )
    child1 = child.getChild(0)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is my text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    child1 = child.getChild(1)
    self.assert_((  "p" == child1.getName() ))
    self.assert_( child1.getNumChildren() == 1 )
    child1 = child1.getChild(0)
    self.assert_((  "This is more text" == child1.getCharacters() ))
    self.assert_( child1.getNumChildren() == 0 )
    att = None
    ns = None
    body_triple = None
    p_triple = None
    body_token = None
    p_token = None
    text_token = None
    text_token1 = None
    p_token1 = None
    body_node = None
    p_node = None
    text_node = None
    p_node1 = None
    text_node1 = None
    pass  

  def test_SBase_appendNotesString(self):
    notes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>";
    taggednewnotes = wrapString("<notes>\n" + "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + 
    "</notes>")
    taggednewnotes2 = wrapString("<notes>\n" + "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "</notes>")
    newnotes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>";
    newnotes2 = "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>""<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>"
    newnotes3 = wrapString("<notes>\n" + "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + "</notes>")
    newnotes4 = wrapString("<notes>\n" "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "</notes>")
    self.S.setNotes(notes)
    self.assert_( self.S.isSetNotes() == True )
    self.S.appendNotes(newnotes)
    notes1 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes1 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(newnotes2)
    notes2 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes2 == taggednewnotes2 ))
    self.S.setNotes(notes)
    self.S.appendNotes(newnotes3)
    notes3 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes3 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(newnotes4)
    notes4 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes4 == taggednewnotes2 ))
    pass  

  def test_SBase_appendNotesString1(self):
    notes = wrapString("<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is a test note </p>\n" + 
    "  </body>\n" + 
    "</html>")
    taggednewnotes = wrapString("<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is a test note </p>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>")
    addnotes = wrapString("<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</html>")
    addnotes2 = wrapString("<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>")
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes)
    notes1 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes1 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes2)
    notes2 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes2 == taggednewnotes ))
    pass  

  def test_SBase_appendNotesString2(self):
    notes = wrapString("<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is a test note </p>\n" + 
    "  </body>\n" + 
    "</html>")
    taggednewnotes = wrapString("<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is a test note </p>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>")
    addnotes = wrapString("<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is more test notes </p>\n" + "</body>\n")
    addnotes2 = wrapString("<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>")
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes)
    notes1 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes1 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes2)
    notes2 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes2 == taggednewnotes ))
    pass  

  def test_SBase_appendNotesString3(self):
    notes = wrapString("<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is a test note </p>\n" + 
    "  </body>\n" + 
    "</html>")
    taggednewnotes = wrapString("<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is a test note </p>\n" + 
    "      <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>")
    taggednewnotes2 = wrapString("<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is a test note </p>\n" + 
    "      <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "      <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>")
    addnotes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n";
    addnotes2 = "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n""<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>"
    addnotes3 = wrapString("<notes>\n" "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + 
    "</notes>")
    addnotes4 = wrapString("<notes>\n" + "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "</notes>")
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes)
    notes1 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes1 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes2)
    notes2 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes2 == taggednewnotes2 ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes3)
    notes3 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes3 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes4)
    notes4 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes4 == taggednewnotes2 ))
    pass  

  def test_SBase_appendNotesString4(self):
    notes = wrapString("<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is a test note </p>\n" + "</body>")
    taggednewnotes = wrapString("<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is a test note </p>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>")
    addnotes = wrapString("<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</html>")
    addnotes2 = wrapString("<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>")
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes)
    notes1 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes1 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes2)
    notes2 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes2 == taggednewnotes ))
    pass  

  def test_SBase_appendNotesString5(self):
    notes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>";
    taggednewnotes = wrapString("<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>")
    addnotes = wrapString("<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</html>")
    addnotes2 = wrapString("<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>")
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes)
    notes1 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes1 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes2)
    notes2 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes2 == taggednewnotes ))
    pass  

  def test_SBase_appendNotesString6(self):
    notes = wrapString("<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is a test note </p>\n" + "</body>")
    taggednewnotes = wrapString("<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is a test note </p>\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>")
    addnotes = wrapString("<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is more test notes </p>\n" + "</body>")
    addnotes2 = wrapString("<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>")
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes)
    notes1 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes1 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes2)
    notes2 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes2 == taggednewnotes ))
    pass  

  def test_SBase_appendNotesString7(self):
    notes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>";
    taggednewnotes = wrapString("<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>")
    addnotes = wrapString("<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is more test notes </p>\n" + "</body>")
    addnotes2 = wrapString("<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>")
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes)
    notes1 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes1 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes2)
    notes2 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes2 == taggednewnotes ))
    pass  

  def test_SBase_appendNotesString8(self):
    notes = wrapString("<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is a test note </p>\n" + "</body>")
    taggednewnotes = wrapString("<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is a test note </p>\n" + 
    "    <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>")
    taggednewnotes2 = wrapString("<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is a test note </p>\n" + 
    "    <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "    <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "  </body>\n" + 
    "</notes>")
    addnotes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>";
    addnotes2 = "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n""<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>"
    addnotes3 = wrapString("<notes>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + 
    "</notes>")
    addnotes4 = wrapString("<notes>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "</notes>")
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes)
    notes1 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes1 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes2)
    notes2 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes2 == taggednewnotes2 ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes3)
    notes3 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes3 == taggednewnotes ))
    self.S.setNotes(notes)
    self.S.appendNotes(addnotes4)
    notes4 = self.S.getNotesString()
    self.assert_( self.S.isSetNotes() == True )
    self.assert_(( notes4 == taggednewnotes2 ))
    pass  

  def test_SBase_getQualifiersFromResources(self):
    cv = libsbml.CVTerm(libsbml.BIOLOGICAL_QUALIFIER)
    cv.setBiologicalQualifierType(libsbml.BQB_ENCODES)
    cv.addResource( "foo")
    self.S.setMetaId( "sbase1")
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
    annotation =  "This is a test note";
    taggedannotation =  "<annotation>This is a test note</annotation>";
    self.S.setAnnotation(annotation)
    self.assert_( self.S.isSetAnnotation() == True )
    if (( taggedannotation != self.S.getAnnotationString() )):
      pass    
    t1 = self.S.getAnnotation()
    self.assert_( t1.getNumChildren() == 1 )
    self.assert_((  "This is a test note" == t1.getChild(0).getCharacters() ))
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
    metaid =  "x12345";
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
    notes =  "This is a test note";
    taggednotes =  "<notes>This is a test note</notes>";
    self.S.setNotes(notes)
    self.assert_( self.S.isSetNotes() == True )
    if (( taggednotes != self.S.getNotesString() )):
      pass    
    t1 = self.S.getNotes()
    self.assert_( t1.getNumChildren() == 1 )
    self.assert_((  "This is a test note" == t1.getChild(0).getCharacters() ))
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
    self.S.setMetaId( "sbase1")
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
    self.assert_( self.S.getCVTerms() == None )
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
