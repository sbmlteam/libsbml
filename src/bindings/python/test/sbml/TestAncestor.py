#
# @file    TestAncestor.py
# @brief   SBML ancestor objects unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id:$
# $HeadURL:$
#
# This test file was converted from src/sbml/test/TestAncestor.cpp
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

class TestAncestor(unittest.TestCase):


  def test_AlgebraicRule_ancestor_create(self):
    m = libsbml.Model()
    r = m.createAlgebraicRule()
    lo = m.getListOfRules()
    self.assert_( r.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( r.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( r.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( r.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getRule(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_AssignmentRule_ancestor_create(self):
    m = libsbml.Model()
    r = m.createAssignmentRule()
    lo = m.getListOfRules()
    self.assert_( r.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( r.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( r.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( r.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getRule(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_CompartmentType_ancestor_add(self):
    ct = libsbml.CompartmentType()
    m = libsbml.Model()
    m.addCompartmentType(ct)
    ct = None
    lo = m.getListOfCompartmentTypes()
    obj = m.getCompartmentType(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_CompartmentType_ancestor_create(self):
    m = libsbml.Model()
    ct = m.createCompartmentType()
    lo = m.getListOfCompartmentTypes()
    self.assert_( ct.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( ct.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( ct.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( ct.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getCompartmentType(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Compartment_ancestor_add(self):
    c = libsbml.Compartment()
    m = libsbml.Model()
    m.addCompartment(c)
    c = None
    lo = m.getListOfCompartments()
    obj = m.getCompartment(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Compartment_ancestor_create(self):
    m = libsbml.Model()
    c = m.createCompartment()
    lo = m.getListOfCompartments()
    self.assert_( c.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( c.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( c.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( c.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getCompartment(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Constraint_ancestor_add(self):
    ct = libsbml.Constraint()
    m = libsbml.Model()
    m.addConstraint(ct)
    ct = None
    lo = m.getListOfConstraints()
    obj = m.getConstraint(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Constraint_ancestor_create(self):
    m = libsbml.Model()
    ct = m.createConstraint()
    lo = m.getListOfConstraints()
    self.assert_( ct.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( ct.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( ct.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( ct.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getConstraint(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Delay_ancestor_add(self):
    d = libsbml.Delay()
    e = libsbml.Event()
    e.setDelay(d)
    d = None
    obj = e.getDelay()
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == e )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    e = None
    pass  

  def test_EventAssignment_ancestor_add(self):
    e = libsbml.Event()
    ea = libsbml.EventAssignment()
    e.addEventAssignment(ea)
    ea = None
    lo = e.getListOfEventAssignments()
    obj = e.getEventAssignment(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == e )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_EventAssignment_ancestor_create(self):
    e = libsbml.Event()
    ea = e.createEventAssignment()
    lo = e.getListOfEventAssignments()
    self.assert_( ea.getAncestorOfType(libsbml.SBML_EVENT) == e )
    self.assert_( ea.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( ea.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( ea.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = e.getEventAssignment(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == e )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_EventAssignment_ancestor_create_model(self):
    m = libsbml.Model()
    e = m.createEvent()
    ea = m.createEventAssignment()
    lo = e.getListOfEventAssignments()
    self.assert_( ea.getAncestorOfType(libsbml.SBML_EVENT) == e )
    self.assert_( ea.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( ea.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( ea.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( ea.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = e.getEventAssignment(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == e )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_Event_ancestor_add(self):
    e = libsbml.Event()
    m = libsbml.Model()
    m.addEvent(e)
    e = None
    lo = m.getListOfEvents()
    obj = m.getEvent(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_Event_ancestor_create(self):
    m = libsbml.Model()
    e = m.createEvent()
    lo = m.getListOfEvents()
    self.assert_( e.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( e.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( e.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( e.getAncestorOfType(libsbml.SBML_PARAMETER) == None )
    obj = m.getEvent(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_FunctionDefinition_ancestor_add(self):
    fd = libsbml.FunctionDefinition()
    m = libsbml.Model()
    m.addFunctionDefinition(fd)
    fd = None
    lo = m.getListOfFunctionDefinitions()
    obj = m.getFunctionDefinition(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_FunctionDefinition_ancestor_create(self):
    m = libsbml.Model()
    fd = m.createFunctionDefinition()
    lo = m.getListOfFunctionDefinitions()
    self.assert_( fd.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( fd.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( fd.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( fd.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getFunctionDefinition(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_InitialAssignment_ancestor_add(self):
    ia = libsbml.InitialAssignment()
    m = libsbml.Model()
    m.addInitialAssignment(ia)
    ia = None
    lo = m.getListOfInitialAssignments()
    obj = m.getInitialAssignment(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_InitialAssignment_ancestor_create(self):
    m = libsbml.Model()
    ia = m.createInitialAssignment()
    lo = m.getListOfInitialAssignments()
    self.assert_( ia.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( ia.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( ia.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( ia.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getInitialAssignment(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_KineticLaw_Parameter_ancestor_add(self):
    kl = libsbml.KineticLaw()
    p = libsbml.Parameter("jake")
    kl.addParameter(p)
    p = None
    lop = kl.getListOfParameters()
    obj = kl.getParameter(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_KINETIC_LAW) == kl )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lop )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    kl = None
    pass  

  def test_KineticLaw_Parameter_ancestor_create(self):
    kl = libsbml.KineticLaw()
    p = kl.createParameter()
    self.assert_( kl.getNumParameters() == 1 )
    lop = kl.getListOfParameters()
    self.assert_( p.getAncestorOfType(libsbml.SBML_KINETIC_LAW) == kl )
    self.assert_( p.getAncestorOfType(libsbml.SBML_LIST_OF) == lop )
    self.assert_( p.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( p.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = kl.getParameter(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_KINETIC_LAW) == kl )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lop )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    kl = None
    pass  

  def test_KineticLaw_Parameter_ancestor_create_model(self):
    m = libsbml.Model()
    r = m.createReaction()
    kl = m.createKineticLaw()
    p = m.createKineticLawParameter()
    self.assert_( kl.getNumParameters() == 1 )
    lop = kl.getListOfParameters()
    self.assert_( p.getAncestorOfType(libsbml.SBML_KINETIC_LAW) == kl )
    self.assert_( p.getAncestorOfType(libsbml.SBML_LIST_OF) == lop )
    self.assert_( p.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( p.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( p.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( p.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = kl.getParameter(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_KINETIC_LAW) == kl )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lop )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    kl = None
    pass  

  def test_KineticLaw_ancestor_add(self):
    kl = libsbml.KineticLaw()
    r = libsbml.Reaction()
    r.setKineticLaw(kl)
    obj = r.getKineticLaw()
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    r = None
    pass  

  def test_KineticLaw_ancestor_create(self):
    r = libsbml.Reaction()
    kl = r.createKineticLaw()
    self.assert_( kl.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( kl.getAncestorOfType(libsbml.SBML_DELAY) == None )
    self.assert_( kl.getAncestorOfType(libsbml.SBML_MODEL) == None )
    self.assert_( kl.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    obj = r.getKineticLaw()
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DELAY) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    r = None
    pass  

  def test_KineticLaw_ancestor_create_model(self):
    m = libsbml.Model()
    r = m.createReaction()
    kl = r.createKineticLaw()
    self.assert_( kl.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( kl.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( kl.getAncestorOfType(libsbml.SBML_DELAY) == None )
    self.assert_( kl.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    obj = r.getKineticLaw()
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DELAY) == None )
    r = None
    pass  

  def test_Model_ancestor_add(self):
    d = libsbml.SBMLDocument()
    m = libsbml.Model()
    d.setModel(m)
    self.assert_( d == d.getModel().getAncestorOfType(libsbml.SBML_DOCUMENT) )
    d = None
    pass  

  def test_Model_ancestor_create(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    self.assert_( m.getAncestorOfType(libsbml.SBML_DOCUMENT) == d )
    obj = d.getModel()
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == d )
    d = None
    pass  

  def test_Parameter_ancestor_add(self):
    ia = libsbml.Parameter()
    m = libsbml.Model()
    m.addParameter(ia)
    ia = None
    lo = m.getListOfParameters()
    obj = m.getParameter(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Parameter_ancestor_create(self):
    m = libsbml.Model()
    p = m.createParameter()
    lo = m.getListOfParameters()
    self.assert_( p.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( p.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( p.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( p.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getParameter(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_RateRule_ancestor_create(self):
    m = libsbml.Model()
    r = m.createRateRule()
    lo = m.getListOfRules()
    self.assert_( r.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( r.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( r.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( r.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getRule(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Reaction_ancestor_add(self):
    ia = libsbml.Reaction()
    m = libsbml.Model()
    m.addReaction(ia)
    ia = None
    lo = m.getListOfReactions()
    obj = m.getReaction(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Reaction_ancestor_create(self):
    m = libsbml.Model()
    r = m.createReaction()
    lo = m.getListOfReactions()
    self.assert_( r.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( r.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( r.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( r.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getReaction(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Rule_ancestor_add(self):
    ia = libsbml.RateRule("a")
    m = libsbml.Model()
    m.addRule(ia)
    ia = None
    lo = m.getListOfRules()
    obj = m.getRule(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_SpeciesReference_Modifier_ancestor_add(self):
    sr = libsbml.ModifierSpeciesReference()
    r = libsbml.Reaction()
    r.addModifier(sr)
    sr = None
    lo = r.getListOfModifiers()
    obj = r.getModifier(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_SpeciesReference_Modifier_ancestor_create(self):
    r = libsbml.Reaction()
    sr = r.createModifier()
    lo = r.getListOfModifiers()
    self.assert_( sr.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = r.getModifier(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_SpeciesReference_Modifier_ancestor_create_model(self):
    m = libsbml.Model()
    r = m.createReaction()
    sr = m.createModifier()
    lo = r.getListOfModifiers()
    self.assert_( sr.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = r.getModifier(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_SpeciesReference_Product_ancestor_add(self):
    sr = libsbml.SpeciesReference()
    r = libsbml.Reaction()
    r.addProduct(sr)
    sr = None
    lo = r.getListOfProducts()
    obj = r.getProduct(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_SpeciesReference_Product_ancestor_create(self):
    r = libsbml.Reaction()
    sr = r.createProduct()
    lo = r.getListOfProducts()
    self.assert_( sr.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = r.getProduct(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_SpeciesReference_Product_ancestor_create_model(self):
    m = libsbml.Model()
    r = m.createReaction()
    sr = m.createProduct()
    lo = r.getListOfProducts()
    self.assert_( sr.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = r.getProduct(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_SpeciesReference_Reactant_ancestor_add(self):
    sr = libsbml.SpeciesReference()
    r = libsbml.Reaction()
    r.addReactant(sr)
    sr = None
    lo = r.getListOfReactants()
    obj = r.getReactant(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_SpeciesReference_Reactant_ancestor_create(self):
    r = libsbml.Reaction()
    sr = r.createReactant()
    lo = r.getListOfReactants()
    self.assert_( sr.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = r.getReactant(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_SpeciesReference_Reactant_ancestor_create_model(self):
    m = libsbml.Model()
    r = m.createReaction()
    sr = m.createReactant()
    lo = r.getListOfReactants()
    self.assert_( sr.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = r.getReactant(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_REACTION) == r )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    pass  

  def test_SpeciesType_ancestor_add(self):
    ia = libsbml.SpeciesType()
    m = libsbml.Model()
    m.addSpeciesType(ia)
    ia = None
    lo = m.getListOfSpeciesTypes()
    obj = m.getSpeciesType(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_SpeciesType_ancestor_create(self):
    m = libsbml.Model()
    st = m.createSpeciesType()
    lo = m.getListOfSpeciesTypes()
    self.assert_( st.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( st.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( st.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( st.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getSpeciesType(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Species_ancestor_add(self):
    ia = libsbml.Species()
    m = libsbml.Model()
    m.addSpecies(ia)
    ia = None
    lo = m.getListOfSpecies()
    obj = m.getSpecies(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Species_ancestor_create(self):
    m = libsbml.Model()
    s = m.createSpecies()
    lo = m.getListOfSpecies()
    self.assert_( s.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( s.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( s.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( s.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getSpecies(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_StoichiometryMath_ancestor_add(self):
    m = libsbml.StoichiometryMath()
    sr = libsbml.SpeciesReference()
    sr.setStoichiometryMath(m)
    m = None
    obj = sr.getStoichiometryMath()
    self.assert_( obj.getAncestorOfType(libsbml.SBML_SPECIES_REFERENCE) == sr )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    sr = None
    pass  

  def test_Trigger_ancestor_add(self):
    d = libsbml.Trigger()
    e = libsbml.Event()
    e.setTrigger(d)
    d = None
    obj = e.getTrigger()
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == e )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    e = None
    pass  

  def test_UnitDefinition_ancestor_add(self):
    ia = libsbml.UnitDefinition()
    m = libsbml.Model()
    m.addUnitDefinition(ia)
    ia = None
    lo = m.getListOfUnitDefinitions()
    obj = m.getUnitDefinition(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_UnitDefinition_ancestor_create(self):
    m = libsbml.Model()
    ud = m.createUnitDefinition()
    lo = m.getListOfUnitDefinitions()
    self.assert_( ud.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( ud.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( ud.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( ud.getAncestorOfType(libsbml.SBML_EVENT) == None )
    obj = m.getUnitDefinition(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_EVENT) == None )
    pass  

  def test_Unit_ancestor_add(self):
    ud = libsbml.UnitDefinition()
    u = libsbml.Unit()
    ud.addUnit(u)
    u = None
    self.assert_( ud.getNumUnits() == 1 )
    lo = ud.getListOfUnits()
    obj = ud.getUnit(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_UNIT_DEFINITION) == ud )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    ud = None
    pass  

  def test_Unit_ancestor_create(self):
    ud = libsbml.UnitDefinition()
    u = ud.createUnit()
    self.assert_( ud.getNumUnits() == 1 )
    lo = ud.getListOfUnits()
    self.assert_( u.getAncestorOfType(libsbml.SBML_UNIT_DEFINITION) == ud )
    self.assert_( u.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( u.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( u.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = ud.getUnit(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_UNIT_DEFINITION) == ud )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    ud = None
    pass  

  def test_Unit_ancestor_create_model(self):
    m = libsbml.Model()
    ud = m.createUnitDefinition()
    u = m.createUnit()
    self.assert_( ud.getNumUnits() == 1 )
    lo = ud.getListOfUnits()
    self.assert_( u.getAncestorOfType(libsbml.SBML_UNIT_DEFINITION) == ud )
    self.assert_( u.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( u.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( u.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( u.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    obj = ud.getUnit(0)
    self.assert_( obj.getAncestorOfType(libsbml.SBML_UNIT_DEFINITION) == ud )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == None )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_MODEL) == m )
    self.assert_( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == None )
    ud = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestAncestor))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
