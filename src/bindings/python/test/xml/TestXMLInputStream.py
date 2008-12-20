#
# @file    TestXMLInputStream.py
# @brief   XMLInputStream unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id:$
# $HeadURL:$
#
# This test file was converted from src/sbml/test/TestXMLInputStream.c
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
import sys
import unittest
import libsbml

def wrapString(s):
  return s
  pass

def LV_L1v1():
  return "level=\"1\" version=\"1\">\n"
  pass

def LV_L1v2():
  return "level=\"1\" version=\"2\">\n"
  pass

def LV_L2v1():
  return "level=\"2\" version=\"1\">\n"
  pass

def LV_L2v2():
  return "level=\"2\" version=\"2\">\n"
  pass

def NS_L1():
  return "xmlns=\"http://www.sbml.org/sbml/level1\" "
  pass

def NS_L2v1():
  return "xmlns=\"http://www.sbml.org/sbml/level2\" "
  pass

def NS_L2v2():
  return "xmlns=\"http://www.sbml.org/sbml/level2/version2\" "
  pass

def SBML_END():
  return "</sbml>\n"
  pass

def SBML_START():
  return "<sbml "
  pass

def XML_START():
  return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  pass

def wrapSBML_L1v1(s):
  r = XML_START()
  r += SBML_START()
  r += NS_L1()
  r += LV_L1v1()
  r += s
  r += SBML_END()
  return r
  pass

def wrapSBML_L1v2(s):
  r = XML_START()
  r += SBML_START()
  r += NS_L1()
  r += LV_L1v2()
  r += s
  r += SBML_END()
  return r
  pass

def wrapSBML_L2v1(s):
  r = XML_START()
  r += SBML_START()
  r += NS_L2v1()
  r += LV_L2v1()
  r += s
  r += SBML_END()
  return r
  pass

def wrapSBML_L2v2(s):
  r = XML_START()
  r += SBML_START()
  r += NS_L2v2()
  r += LV_L2v2()
  r += s
  r += SBML_END()
  return r
  pass

def wrapXML(s):
  r = XML_START()
  r += s
  return r
  pass

class TestXMLInputStream(unittest.TestCase):


  def test_XMLInputStream_create(self):
    text = wrapSBML_L2v1("  <model id=\"Branch\"/>\n")
    stream = libsbml.XMLInputStream(text,0, "")
    self.assert_( stream != None )
    self.assert_( stream.isEOF() == False )
    self.assert_( stream.isGood() == True )
    self.assert_( stream.isError() == False )
    stream.next()
    self.assert_( (  "UTF-8" != stream.getEncoding() ) == False )
    stream = None
    pass  

  def test_XMLInputStream_next_peek(self):
    text = wrapString("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
    "<sbml " + 
    "xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\">\n" + 
    "  <model id=\"Branch\"/>\n" + 
    "</sbml>")
    stream = libsbml.XMLInputStream(text,0, "")
    next0 = stream.peek()
    self.assert_( stream != None )
    self.assert_( (  "sbml" != next0.getName() ) == False )
    next1 = stream.next()
    self.assert_( (  "sbml" != next1.getName() ) == False )
    stream = None
    pass  

  def test_XMLInputStream_setErrorLog(self):
    text = wrapString("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
    "<sbml " + 
    "xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\">\n" + 
    "<listOfFunctionDefinitions>\n" + 
    "<notes>My Functions</notes>\n" + 
    "<functionDefinition/>\n" + 
    "</listOfFunctionDefinitions>\n" + 
    "<listOfUnitDefinitions>\n" + 
    "<notes>My Units</notes>\n" + 
    "<unitDefinition/>\n" + 
    "</listOfUnitDefinitions>\n" + 
    "</sbml>")
    stream = libsbml.XMLInputStream(text,0, "")
    self.assert_( stream != None )
    log = libsbml.XMLErrorLog()
    stream.setErrorLog(log)
    self.assert_( stream.getErrorLog() == log )
    pass  

  def test_XMLInputStream_skip(self):
    text = wrapString("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
    "<sbml " + 
    "xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\">\n" + 
    "<listOfFunctionDefinitions>\n" + 
    "<notes>My Functions</notes>\n" + 
    "<functionDefinition/>\n" + 
    "</listOfFunctionDefinitions>\n" + 
    "<listOfUnitDefinitions>\n" + 
    "<notes>My Units</notes>\n" + 
    "<unitDefinition/>\n" + 
    "</listOfUnitDefinitions>\n" + 
    "</sbml>")
    stream = libsbml.XMLInputStream(text,0, "")
    self.assert_( stream != None )
    next0 = stream.next()
    stream.skipText()
    stream.skipPastEnd(stream.next())
    stream.skipText()
    next0 = stream.next()
    self.assert_( (  "listOfUnitDefinitions" != next0.getName() ) == False )
    stream = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestXMLInputStream))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
