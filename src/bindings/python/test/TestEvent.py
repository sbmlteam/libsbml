#
# @file    TestEvent.py
# @brief   SBML Event unit tests
# @author  Akiya Jouraku (Python conversion)
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
import sys
import unittest
import libsbml

class TestEvent(unittest.TestCase):

  E = None

  def setUp(self):
    self.E = libsbml.Event()
    if (self.E == None):
      pass    
    pass  

  def tearDown(self):
    self.E = None
    pass  

  def test_Event_create(self):
    self.assert_( self.E.getTypeCode() == libsbml.SBML_EVENT )
    self.assert_( self.E.getMetaId() == "" )
    self.assert_( self.E.getNotes() == None )
    self.assert_( self.E.getAnnotation() == None )
    self.assert_( self.E.getId() == "" )
    self.assert_( self.E.getName() == "" )
    self.assert_( self.E.getTrigger() == None )
    self.assert_( self.E.getDelay() == None )
    self.assert_( self.E.getTimeUnits() == "" )
    self.assert_( self.E.getNumEventAssignments() == 0 )
    pass  

  def test_Event_createWith(self):
    e = libsbml.Event("e1", "")
    self.assert_( e.getTypeCode() == libsbml.SBML_EVENT )
    self.assert_( e.getMetaId() == "" )
    self.assert_( e.getNotes() == None )
    self.assert_( e.getAnnotation() == None )
    self.assert_( e.getName() == "" )
    self.assert_( e.getDelay() == None )
    self.assert_( e.getTimeUnits() == "" )
    self.assert_( e.getNumEventAssignments() == 0 )
    self.assertEqual( False, e.isSetTrigger() )
    self.assert_((  "e1" == e.getId() ))
    self.assertEqual( True, e.isSetId() )
    e = None
    pass  

  def test_Event_free_NULL(self):
    
    pass  

  def test_Event_full(self):
    math1 = libsbml.parseFormula("0")
    trigger = libsbml.Trigger(math1)
    math = libsbml.parseFormula("0")
    e = libsbml.Event("e1", "")
    ea = libsbml.EventAssignment("k",math)
    e.setTrigger(trigger)
    e.setName( "Set k2 to zero when P1 <= t")
    e.addEventAssignment(ea)
    self.assert_( e.getNumEventAssignments() == 1 )
    self.assert_( e.getEventAssignment(0) != ea )
    math = None
    e = None
    pass  

  def test_Event_setDelay(self):
    math1 = libsbml.parseFormula("0")
    Delay = libsbml.Delay(math1)
    self.E.setDelay(Delay)
    self.assert_( self.E.getDelay() != None )
    self.assertEqual( True, self.E.isSetDelay() )
    if (self.E.getDelay() == Delay):
      pass    
    self.E.setDelay(self.E.getDelay())
    self.assert_( self.E.getDelay() != Delay )
    self.E.setDelay(None)
    self.assertEqual( False, self.E.isSetDelay() )
    if (self.E.getDelay() != None):
      pass    
    pass  

  def test_Event_setId(self):
    id = "e1"
    self.E.setId(id)
    self.assert_(( id == self.E.getId() ))
    self.assertEqual( True, self.E.isSetId() )
    if (self.E.getId() == id):
      pass    
    self.E.setId(self.E.getId())
    self.assert_(( id == self.E.getId() ))
    self.E.setId("")
    self.assertEqual( False, self.E.isSetId() )
    if (self.E.getId() != None):
      pass    
    pass  

  def test_Event_setName(self):
    name = "Set k2 to zero when P1 <= t"
    self.E.setName(name)
    self.assert_(( name == self.E.getName() ))
    self.assertEqual( True, self.E.isSetName() )
    if (self.E.getName() == name):
      pass    
    self.E.setName(self.E.getName())
    self.assert_(( name == self.E.getName() ))
    self.E.setName("")
    self.assertEqual( False, self.E.isSetName() )
    if (self.E.getName() != None):
      pass    
    pass  

  def test_Event_setTimeUnits(self):
    units = "second"
    self.E.setTimeUnits(units)
    self.assert_(( units == self.E.getTimeUnits() ))
    self.assertEqual( True, self.E.isSetTimeUnits() )
    if (self.E.getTimeUnits() == units):
      pass    
    self.E.setTimeUnits(self.E.getTimeUnits())
    self.assert_(( units == self.E.getTimeUnits() ))
    self.E.setTimeUnits("")
    self.assertEqual( False, self.E.isSetTimeUnits() )
    if (self.E.getTimeUnits() != None):
      pass    
    pass  

  def test_Event_setTrigger(self):
    math1 = libsbml.parseFormula("0")
    trigger = libsbml.Trigger(math1)
    self.E.setTrigger(trigger)
    self.assert_( self.E.getTrigger() != None )
    self.assertEqual( True, self.E.isSetTrigger() )
    if (self.E.getTrigger() == trigger):
      pass    
    self.E.setTrigger(self.E.getTrigger())
    self.assert_( self.E.getTrigger() != trigger )
    self.E.setTrigger(None)
    self.assertEqual( False, self.E.isSetTrigger() )
    if (self.E.getTrigger() != None):
      pass    
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestEvent))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
