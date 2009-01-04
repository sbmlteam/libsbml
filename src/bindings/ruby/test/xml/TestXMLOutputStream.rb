#
# @file    TestXMLOutputStream.rb
# @brief   XMLOutputStream unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id:$
# $HeadURL:$
#
# This test file was converted from src/sbml/test/TestXMLOutputStream.c
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
require 'test/unit'
require 'libSBML'

class TestXMLOutputStream < Test::Unit::TestCase

  def test_XMLOutputStream_Elements
    d = 2.4
    l = 123456789
    ui = 5
    i = -3
    oss = LibSBML::Ostringstream.new
    stream = LibSBML::XMLOutputStream.new(oss,"",false)
    stream.startElement( "fred")
    stream.writeAttribute( "chars", "two")
    stream.writeAttributeBool( "bool",true)
    stream.writeAttribute( "double",d)

    stream.writeAttribute( "long",l)
    stream.writeAttribute( "uint",ui)
    stream.writeAttribute( "int",i)
    stream.endElement( "fred")
    expected =  "<fred chars=\"two\" bool=\"true\" double=\"2.4\" long=\"123456789\" uint=\"5\" int=\"-3\"/>";
    s = oss.str()
    assert (( expected == s ))
    stream = nil
  end

  def test_XMLOutputStream_createStdout
    stream = LibSBML::XMLOutputStream.new(LibSBML::cout,"UTF-8",false)
    assert( stream != nil )
    stream = nil
  end

  def test_XMLOutputStream_createStdoutWithProgramInfo
    stream = LibSBML::XMLOutputStream.new(LibSBML::cout,"UTF-8",false, "foo", "bar")
    assert( stream != nil )
    stream = nil
  end

  def test_XMLOutputStream_createString
    expected =  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    oss = LibSBML::Ostringstream.new
    stream = LibSBML::XMLOutputStream.new(oss,"UTF-8",true)
    assert( stream != nil )
    string = oss.str()
    assert (( expected == string ))
    stream = nil
  end

  def test_XMLOutputStream_createStringWithProgramInfo
    expected =  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    oss = LibSBML::Ostringstream.new
    stream = LibSBML::XMLOutputStream.new(oss,"UTF-8",true, "", "")
    assert( stream != nil )
    string = oss.str()
    assert (( expected == string ))
    stream = nil
  end

  def test_XMLOutputStream_startEnd
    oss = LibSBML::Ostringstream.new
    stream = LibSBML::XMLOutputStream.new(oss,"",false)
    assert( stream != nil )
    stream.startEndElement( "id")
    string = oss.str()
    assert ((  "<id/>" == string ))
    stream = nil
  end

end
