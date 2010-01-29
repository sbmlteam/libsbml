#
# @file    TestL3Event.rb
# @brief   L3 Event unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestL3Event.c
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
require 'test/unit'
require 'libSBML'

class TestL3Event < Test::Unit::TestCase

  def setup
    @@e = LibSBML::Event.new(3,1)
    if (@@e == nil)
    end
  end

  def teardown
    @@e = nil
  end

  def test_L3_Event_NS
    assert( @@e.getNamespaces() != nil )
    assert( @@e.getNamespaces().getLength() == 1 )
    assert ((     "http://www.sbml.org/sbml/level3/version1/core" == @@e.getNamespaces().getURI(0) ))
  end

  def test_L3_Event_create
    assert( @@e.getTypeCode() == LibSBML::SBML_EVENT )
    assert( @@e.getMetaId() == "" )
    assert( @@e.getNotes() == nil )
    assert( @@e.getAnnotation() == nil )
    assert( @@e.getId() == "" )
    assert( @@e.getName() == "" )
    assert( @@e.getUseValuesFromTriggerTime() == true )
    assert_equal false, @@e.isSetId()
    assert_equal false, @@e.isSetName()
    assert_equal false, @@e.isSetUseValuesFromTriggerTime()
  end

  def test_L3_Event_createWithNS
    xmlns = LibSBML::XMLNamespaces.new()
    xmlns.add( "http://www.sbml.org", "testsbml")
    sbmlns = LibSBML::SBMLNamespaces.new(3,1)
    sbmlns.addNamespaces(xmlns)
    e = LibSBML::Event.new(sbmlns)
    assert( e.getTypeCode() == LibSBML::SBML_EVENT )
    assert( e.getMetaId() == "" )
    assert( e.getNotes() == nil )
    assert( e.getAnnotation() == nil )
    assert( e.getLevel() == 3 )
    assert( e.getVersion() == 1 )
    assert( e.getNamespaces() != nil )
    assert( e.getNamespaces().getLength() == 2 )
    assert( e.getId() == "" )
    assert( e.getName() == "" )
    assert( e.getUseValuesFromTriggerTime() == true )
    assert_equal false, e.isSetId()
    assert_equal false, e.isSetName()
    assert_equal false, e.isSetUseValuesFromTriggerTime()
    e = nil
  end

  def test_L3_Event_free_NULL
  end

  def test_L3_Event_hasRequiredAttributes
    e = LibSBML::Event.new(3,1)
    assert_equal false, e.hasRequiredAttributes()
    e.setUseValuesFromTriggerTime(true)
    assert_equal true, e.hasRequiredAttributes()
    e = nil
  end

  def test_L3_Event_hasRequiredElements
    e = LibSBML::Event.new(3,1)
    assert_equal false, e.hasRequiredElements()
    t = LibSBML::Trigger.new(3,1)
    e.setTrigger(t)
    assert_equal true, e.hasRequiredElements()
    e = nil
  end

  def test_L3_Event_id
    id =  "mitochondria";
    assert_equal false, @@e.isSetId()
    @@e.setId(id)
    assert (( id == @@e.getId() ))
    assert_equal true, @@e.isSetId()
    if (@@e.getId() == id)
    end
    @@e.unsetId()
    assert_equal false, @@e.isSetId()
    if (@@e.getId() != nil)
    end
  end

  def test_L3_Event_name
    name =  "My_Favorite_Factory";
    assert_equal false, @@e.isSetName()
    @@e.setName(name)
    assert (( name == @@e.getName() ))
    assert_equal true, @@e.isSetName()
    if (@@e.getName() == name)
    end
    @@e.unsetName()
    assert_equal false, @@e.isSetName()
    if (@@e.getName() != nil)
    end
  end

  def test_L3_Event_useValuesFromTriggerTime
    assert( @@e.isSetUseValuesFromTriggerTime() == false )
    @@e.setUseValuesFromTriggerTime(true)
    assert( @@e.getUseValuesFromTriggerTime() == true )
    assert( @@e.isSetUseValuesFromTriggerTime() == true )
    @@e.setUseValuesFromTriggerTime(false)
    assert( @@e.getUseValuesFromTriggerTime() == false )
    assert( @@e.isSetUseValuesFromTriggerTime() == true )
  end

end
