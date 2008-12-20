#
# @file    TestXMLInputStream.rb
# @brief   XMLInputStream unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
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
require 'test/unit'
require 'libSBML'

class TestXMLInputStream < Test::Unit::TestCase

  def LV_L1v1
    return "level=\"1\" version=\"1\">\n"
  end

  def LV_L1v2
    return "level=\"1\" version=\"2\">\n"
  end

  def LV_L2v1
    return "level=\"2\" version=\"1\">\n"
  end

  def LV_L2v2
    return "level=\"2\" version=\"2\">\n"
  end

  def NS_L1
    return "xmlns=\"http://www.sbml.org/sbml/level1\" "
  end

  def NS_L2v1
    return "xmlns=\"http://www.sbml.org/sbml/level2\" "
  end

  def NS_L2v2
    return "xmlns=\"http://www.sbml.org/sbml/level2/version2\" "
  end

  def SBML_END
    return "</sbml>\n"
  end

  def SBML_START
    return "<sbml "
  end

  def XML_START
    return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  end

  def wrapSBML_L1v1(s)
    r = XML_START()
    r += SBML_START()
    r += NS_L1()
    r += LV_L1v1()
    r += s
    r += SBML_END()
    return r
  end

  def wrapSBML_L1v2(s)
    r = XML_START()
    r += SBML_START()
    r += NS_L1()
    r += LV_L1v2()
    r += s
    r += SBML_END()
    return r
  end

  def wrapSBML_L2v1(s)
    r = XML_START()
    r += SBML_START()
    r += NS_L2v1()
    r += LV_L2v1()
    r += s
    r += SBML_END()
    return r
  end

  def wrapSBML_L2v2(s)
    r = XML_START()
    r += SBML_START()
    r += NS_L2v2()
    r += LV_L2v2()
    r += s
    r += SBML_END()
    return r
  end

  def wrapXML(s)
    r = XML_START()
    r += s
    return r
  end

  def test_XMLInputStream_create
    text = wrapSBML_L2v1("  <model id=\"Branch\"/>\n")
    stream = LibSBML::XMLInputStream.new(text,0, "")
    assert( stream != nil )
    assert( stream.isEOF() == false )
    assert( stream.isGood() == true )
    assert( stream.isError() == false )
    stream.next()
    assert( (  "UTF-8" != stream.getEncoding() ) == false )
    stream = nil
  end

  def test_XMLInputStream_next_peek
    text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
    "<sbml " + 
    "xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\">\n" + 
    "  <model id=\"Branch\"/>\n" + 
    "</sbml>"
    stream = LibSBML::XMLInputStream.new(text,0, "")
    next0 = stream.peek()
    assert( stream != nil )
    assert( (  "sbml" != next0.getName() ) == false )
    next1 = stream.next()
    assert( (  "sbml" != next1.getName() ) == false )
    stream = nil
  end

  def test_XMLInputStream_setErrorLog
    text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
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
    "</sbml>"
    stream = LibSBML::XMLInputStream.new(text,0, "")
    assert( stream != nil )
    log = LibSBML::XMLErrorLog.new()
    stream.setErrorLog(log)
    assert( stream.getErrorLog() == log )
  end

  def test_XMLInputStream_skip
    text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
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
    "</sbml>"
    stream = LibSBML::XMLInputStream.new(text,0, "")
    assert( stream != nil )
    next0 = stream.next()
    stream.skipText()
    stream.skipPastEnd(stream.next())
    stream.skipText()
    next0 = stream.next()
    assert( (  "listOfUnitDefinitions" != next0.getName() ) == false )
    stream = nil
  end

end
