#
# This file was converted from libsbml/src/sbml/test/TestEvent.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestEvent < Test::Unit::TestCase

  def test_Event_free_NULL
  end

  def test_Event_setTimeUnits
    units = "second"
    @@e.setTimeUnits(units)
    assert_equal units,@@e.getTimeUnits
    assert_equal true, @@e.isSetTimeUnits
      @@e.setTimeUnits(@@e.getTimeUnits)
      assert_equal units,@@e.getTimeUnits
      @@e.setTimeUnits("")
      assert_equal false, @@e.isSetTimeUnits
  end

  def test_Event_setName
    name = "Set k2 to zero when P1 <= t"
    @@e.setName(name)
    assert_equal name,@@e.getName
    assert_equal true, @@e.isSetName
      @@e.setName(@@e.getName)
      assert_equal name,@@e.getName
      @@e.setName("")
      assert_equal false, @@e.isSetName
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
    assert_equal 1, e.getNumEventAssignments
    assert_not_equal ea, e.getEventAssignment(0)
  end

  def test_Event_setTrigger
    math1 = LibSBML::parseFormula("0")
    trigger = LibSBML::Trigger.new(math1)
    @@e.setTrigger(trigger)
    assert_not_equal nil, @@e.getTrigger
    assert_equal true, @@e.isSetTrigger
    @@e.setTrigger(@@e.getTrigger)
    assert_not_equal trigger, @@e.getTrigger
    @@e.setTrigger(nil)
    assert_equal false, @@e.isSetTrigger
  end

  def setup
    @@e = LibSBML::Event.new
      
  end

  def test_Event_create
    assert_equal LibSBML::SBML_EVENT, @@e.getTypeCode
    assert_equal "", @@e.getMetaId
    assert_equal nil, @@e.getNotes
    assert_equal nil, @@e.getAnnotation
    assert_equal "", @@e.getId
    assert_equal "", @@e.getName
    assert_equal nil, @@e.getTrigger
    assert_equal nil, @@e.getDelay
    assert_equal "", @@e.getTimeUnits
    assert_equal 0, @@e.getNumEventAssignments
  end

  def test_Event_setDelay
    math1 = LibSBML::parseFormula("0")
    delay = LibSBML::Delay.new(math1)
    @@e.setDelay(delay)
    assert_not_equal nil, @@e.getDelay
    assert_equal true, @@e.isSetDelay
    @@e.setDelay(@@e.getDelay)
    assert_not_equal delay, @@e.getDelay
    @@e.setDelay(nil)
    assert_equal false, @@e.isSetDelay
  end

  def test_Event_setId
    id = "e1"
    @@e.setId(id)
    assert_equal id,@@e.getId
    assert_equal true, @@e.isSetId
    @@e.setId(@@e.getId)
    assert_equal id,@@e.getId
    @@e.setId("")
    assert_equal false, @@e.isSetId
  end

  def test_Event_createWith
    e = LibSBML::Event.new("e1", "")
    assert_equal LibSBML::SBML_EVENT, e.getTypeCode
    assert_equal "", e.getMetaId
    assert_equal nil, e.getNotes
    assert_equal nil, e.getAnnotation
    assert_equal "", e.getName
    assert_equal nil, e.getDelay
    assert_equal "", e.getTimeUnits
    assert_equal 0, e.getNumEventAssignments
    assert_equal false, e.isSetTrigger
    assert_equal  "e1",e.getId
    assert_equal true, e.isSetId
  end

end
