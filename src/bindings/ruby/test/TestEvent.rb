#
# @file    TestEvent.rb
# @brief   SBML Event unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestEvent.c
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
require 'test/unit'
require 'libSBML'

class TestEvent < Test::Unit::TestCase

  def setup
    @@e = LibSBML::Event.new
    if (@@e == nil)
    end
  end

  def teardown
    @@e = nil
  end

  def test_Event_create
    assert( @@e.getTypeCode == LibSBML::SBML_EVENT )
    assert( @@e.getMetaId == "" )
    assert( @@e.getNotes == nil )
    assert( @@e.getAnnotation == nil )
    assert( @@e.getId == "" )
    assert( @@e.getName == "" )
    assert( @@e.getTrigger == nil )
    assert( @@e.getDelay == nil )
    assert( @@e.getTimeUnits == "" )
    assert( @@e.getNumEventAssignments == 0 )
  end

  def test_Event_createWith
    e = LibSBML::Event.new("e1", "")
    assert( e.getTypeCode == LibSBML::SBML_EVENT )
    assert( e.getMetaId == "" )
    assert( e.getNotes == nil )
    assert( e.getAnnotation == nil )
    assert( e.getName == "" )
    assert( e.getDelay == nil )
    assert( e.getTimeUnits == "" )
    assert( e.getNumEventAssignments == 0 )
    assert_equal false, e.isSetTrigger
    assert ((  "e1" == e.getId ))
    assert_equal true, e.isSetId
    e = nil
  end

  def test_Event_free_NULL
    
  end

  def test_Event_full
    math1 = LibSBML::parseFormula("0")
    trigger = LibSBML::Trigger.new(math1)
    math = LibSBML::parseFormula("0")
    e = LibSBML::Event.new("e1", "")
    ea = LibSBML::EventAssignment.new("k",math)
    e.setTrigger(trigger)
    e.setName( "Set k2 to zero when P1 <= t")
    e.addEventAssignment(ea)
    assert( e.getNumEventAssignments == 1 )
    assert( e.getEventAssignment(0) != ea )
    math = nil
    e = nil
  end

  def test_Event_setDelay
    math1 = LibSBML::parseFormula("0")
    delay = LibSBML::Delay.new(math1)
    @@e.setDelay(delay)
    assert( @@e.getDelay != nil )
    assert_equal true, @@e.isSetDelay
    if (@@e.getDelay == delay)
    end
    @@e.setDelay(@@e.getDelay)
    assert( @@e.getDelay != delay )
    @@e.setDelay(nil)
    assert_equal false, @@e.isSetDelay
    if (@@e.getDelay != nil)
    end
  end

  def test_Event_setId
    id = "e1"
    @@e.setId(id)
    assert (( id == @@e.getId ))
    assert_equal true, @@e.isSetId
    if (@@e.getId == id)
    end
    @@e.setId(@@e.getId)
    assert (( id == @@e.getId ))
    @@e.setId("")
    assert_equal false, @@e.isSetId
    if (@@e.getId != nil)
    end
  end

  def test_Event_setName
    name = "Set k2 to zero when P1 <= t"
    @@e.setName(name)
    assert (( name == @@e.getName ))
    assert_equal true, @@e.isSetName
    if (@@e.getName == name)
    end
    @@e.setName(@@e.getName)
    assert (( name == @@e.getName ))
    @@e.setName("")
    assert_equal false, @@e.isSetName
    if (@@e.getName != nil)
    end
  end

  def test_Event_setTimeUnits
    units = "second"
    @@e.setTimeUnits(units)
    assert (( units == @@e.getTimeUnits ))
    assert_equal true, @@e.isSetTimeUnits
    if (@@e.getTimeUnits == units)
    end
    @@e.setTimeUnits(@@e.getTimeUnits)
    assert (( units == @@e.getTimeUnits ))
    @@e.setTimeUnits("")
    assert_equal false, @@e.isSetTimeUnits
    if (@@e.getTimeUnits != nil)
    end
  end

  def test_Event_setTrigger
    math1 = LibSBML::parseFormula("0")
    trigger = LibSBML::Trigger.new(math1)
    @@e.setTrigger(trigger)
    assert( @@e.getTrigger != nil )
    assert_equal true, @@e.isSetTrigger
    if (@@e.getTrigger == trigger)
    end
    @@e.setTrigger(@@e.getTrigger)
    assert( @@e.getTrigger != trigger )
    @@e.setTrigger(nil)
    assert_equal false, @@e.isSetTrigger
    if (@@e.getTrigger != nil)
    end
  end

end
