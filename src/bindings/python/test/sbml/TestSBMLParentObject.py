#
# @file    TestSBMLParentObject.py
# @brief   SBML parent object unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id:$
# $HeadURL:$
#
# This test file was converted from src/sbml/test/TestSBMLParentObject.cpp
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

class TestSBMLParentObject(unittest.TestCase):


  def test_AlgebraicRule_parent_create(self):
    m = libsbml.Model()
    r = m.createAlgebraicRule()
    lo = m.getListOfRules()
    self.assert_( lo == m.getRule(0).getParentSBMLObject() )
    self.assert_( lo == r.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_AssignmentRule_parent_create(self):
    m = libsbml.Model()
    r = m.createAssignmentRule()
    lo = m.getListOfRules()
    self.assert_( lo == m.getRule(0).getParentSBMLObject() )
    self.assert_( lo == r.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_CompartmentType_parent_add(self):
    ct = libsbml.CompartmentType()
    m = libsbml.Model()
    m.addCompartmentType(ct)
    ct = None
    lo = m.getListOfCompartmentTypes()
    self.assert_( lo == m.getCompartmentType(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_CompartmentType_parent_create(self):
    m = libsbml.Model()
    ct = m.createCompartmentType()
    lo = m.getListOfCompartmentTypes()
    self.assert_( lo == m.getCompartmentType(0).getParentSBMLObject() )
    self.assert_( lo == ct.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Compartment_parent_add(self):
    c = libsbml.Compartment()
    m = libsbml.Model()
    m.addCompartment(c)
    c = None
    lo = m.getListOfCompartments()
    self.assert_( lo == m.getCompartment(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Compartment_parent_create(self):
    m = libsbml.Model()
    c = m.createCompartment()
    lo = m.getListOfCompartments()
    self.assert_( lo == m.getCompartment(0).getParentSBMLObject() )
    self.assert_( lo == c.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Constraint_parent_add(self):
    ct = libsbml.Constraint()
    m = libsbml.Model()
    m.addConstraint(ct)
    ct = None
    lo = m.getListOfConstraints()
    self.assert_( lo == m.getConstraint(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Constraint_parent_create(self):
    m = libsbml.Model()
    ct = m.createConstraint()
    lo = m.getListOfConstraints()
    self.assert_( lo == m.getConstraint(0).getParentSBMLObject() )
    self.assert_( lo == ct.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Delay_parent_add(self):
    d = libsbml.Delay()
    e = libsbml.Event()
    e.setDelay(d)
    d = None
    self.assert_( e == e.getDelay().getParentSBMLObject() )
    e = None
    pass  

  def test_EventAssignment_parent_add(self):
    e = libsbml.Event()
    ea = libsbml.EventAssignment()
    e.addEventAssignment(ea)
    ea = None
    lo = e.getListOfEventAssignments()
    self.assert_( lo == e.getEventAssignment(0).getParentSBMLObject() )
    self.assert_( e == lo.getParentSBMLObject() )
    pass  

  def test_EventAssignment_parent_create(self):
    e = libsbml.Event()
    ea = e.createEventAssignment()
    lo = e.getListOfEventAssignments()
    self.assert_( lo == e.getEventAssignment(0).getParentSBMLObject() )
    self.assert_( lo == ea.getParentSBMLObject() )
    self.assert_( e == lo.getParentSBMLObject() )
    pass  

  def test_EventAssignment_parent_create_model(self):
    m = libsbml.Model()
    e = m.createEvent()
    ea = m.createEventAssignment()
    lo = e.getListOfEventAssignments()
    self.assert_( lo == e.getEventAssignment(0).getParentSBMLObject() )
    self.assert_( lo == ea.getParentSBMLObject() )
    self.assert_( e == lo.getParentSBMLObject() )
    pass  

  def test_Event_parent_add(self):
    e = libsbml.Event()
    m = libsbml.Model()
    m.addEvent(e)
    e = None
    lo = m.getListOfEvents()
    self.assert_( lo == m.getEvent(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Event_parent_create(self):
    m = libsbml.Model()
    e = m.createEvent()
    lo = m.getListOfEvents()
    self.assert_( lo == m.getEvent(0).getParentSBMLObject() )
    self.assert_( lo == e.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_FunctionDefinition_parent_add(self):
    fd = libsbml.FunctionDefinition()
    m = libsbml.Model()
    m.addFunctionDefinition(fd)
    fd = None
    lo = m.getListOfFunctionDefinitions()
    self.assert_( lo == m.getFunctionDefinition(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_FunctionDefinition_parent_create(self):
    m = libsbml.Model()
    fd = m.createFunctionDefinition()
    lo = m.getListOfFunctionDefinitions()
    self.assert_( lo == m.getFunctionDefinition(0).getParentSBMLObject() )
    self.assert_( lo == fd.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_InitialAssignment_parent_add(self):
    ia = libsbml.InitialAssignment()
    m = libsbml.Model()
    m.addInitialAssignment(ia)
    ia = None
    lo = m.getListOfInitialAssignments()
    self.assert_( lo == m.getInitialAssignment(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_InitialAssignment_parent_create(self):
    m = libsbml.Model()
    ia = m.createInitialAssignment()
    lo = m.getListOfInitialAssignments()
    self.assert_( lo == m.getInitialAssignment(0).getParentSBMLObject() )
    self.assert_( lo == ia.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_KineticLaw_Parameter_parent_add(self):
    kl = libsbml.KineticLaw()
    p = libsbml.Parameter("jake")
    kl.addParameter(p)
    p = None
    self.assert_( kl.getNumParameters() == 1 )
    self.assert_( kl.getParameter(0).getId() ==  "jake" )
    lop = kl.getListOfParameters()
    self.assert_( kl == lop.getParentSBMLObject() )
    self.assert_( lop == kl.getParameter(0).getParentSBMLObject() )
    kl = None
    pass  

  def test_KineticLaw_Parameter_parent_create(self):
    kl = libsbml.KineticLaw()
    p = kl.createParameter()
    self.assert_( kl.getNumParameters() == 1 )
    lop = kl.getListOfParameters()
    self.assert_( kl == lop.getParentSBMLObject() )
    self.assert_( lop == p.getParentSBMLObject() )
    self.assert_( lop == kl.getParameter(0).getParentSBMLObject() )
    kl = None
    pass  

  def test_KineticLaw_Parameter_parent_create_model(self):
    m = libsbml.Model()
    r = m.createReaction()
    kl = m.createKineticLaw()
    p = m.createKineticLawParameter()
    self.assert_( kl.getNumParameters() == 1 )
    lop = kl.getListOfParameters()
    self.assert_( r == kl.getParentSBMLObject() )
    self.assert_( kl == lop.getParentSBMLObject() )
    self.assert_( lop == p.getParentSBMLObject() )
    self.assert_( lop == kl.getParameter(0).getParentSBMLObject() )
    kl = None
    pass  

  def test_KineticLaw_parent_add(self):
    kl = libsbml.KineticLaw()
    r = libsbml.Reaction()
    r.setKineticLaw(kl)
    self.assert_( r == r.getKineticLaw().getParentSBMLObject() )
    r = None
    pass  

  def test_KineticLaw_parent_create(self):
    r = libsbml.Reaction()
    kl = r.createKineticLaw()
    self.assert_( r == kl.getParentSBMLObject() )
    r = None
    pass  

  def test_KineticLaw_parent_create_model(self):
    m = libsbml.Model()
    r = m.createReaction()
    kl = r.createKineticLaw()
    self.assert_( r == kl.getParentSBMLObject() )
    self.assert_( r == r.getKineticLaw().getParentSBMLObject() )
    r = None
    pass  

  def test_Model_parent_add(self):
    d = libsbml.SBMLDocument()
    m = libsbml.Model()
    d.setModel(m)
    self.assert_( d == d.getModel().getParentSBMLObject() )
    d = None
    pass  

  def test_Model_parent_create(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    self.assert_( d == m.getParentSBMLObject() )
    d = None
    pass  

  def test_Parameter_parent_add(self):
    ia = libsbml.Parameter()
    m = libsbml.Model()
    m.addParameter(ia)
    ia = None
    lo = m.getListOfParameters()
    self.assert_( lo == m.getParameter(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Parameter_parent_create(self):
    m = libsbml.Model()
    p = m.createParameter()
    lo = m.getListOfParameters()
    self.assert_( lo == m.getParameter(0).getParentSBMLObject() )
    self.assert_( lo == p.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_RateRule_parent_create(self):
    m = libsbml.Model()
    r = m.createRateRule()
    lo = m.getListOfRules()
    self.assert_( lo == m.getRule(0).getParentSBMLObject() )
    self.assert_( lo == r.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Reaction_parent_add(self):
    ia = libsbml.Reaction()
    m = libsbml.Model()
    m.addReaction(ia)
    ia = None
    lo = m.getListOfReactions()
    self.assert_( lo == m.getReaction(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Reaction_parent_create(self):
    m = libsbml.Model()
    r = m.createReaction()
    lo = m.getListOfReactions()
    self.assert_( lo == m.getReaction(0).getParentSBMLObject() )
    self.assert_( lo == r.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Rule_parent_add(self):
    ia = libsbml.RateRule("a")
    m = libsbml.Model()
    m.addRule(ia)
    ia = None
    lo = m.getListOfRules()
    self.assert_( lo == m.getRule(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_SpeciesReference_Modifier_parent_add(self):
    sr = libsbml.ModifierSpeciesReference()
    r = libsbml.Reaction()
    r.addModifier(sr)
    sr = None
    lo = r.getListOfModifiers()
    self.assert_( lo == r.getModifier(0).getParentSBMLObject() )
    self.assert_( r == lo.getParentSBMLObject() )
    pass  

  def test_SpeciesReference_Modifier_parent_create(self):
    r = libsbml.Reaction()
    sr = r.createModifier()
    lo = r.getListOfModifiers()
    self.assert_( lo == sr.getParentSBMLObject() )
    self.assert_( lo == r.getModifier(0).getParentSBMLObject() )
    self.assert_( r == lo.getParentSBMLObject() )
    pass  

  def test_SpeciesReference_Modifier_parent_create_model(self):
    m = libsbml.Model()
    r = m.createReaction()
    sr = m.createModifier()
    lo = r.getListOfModifiers()
    self.assert_( lo == sr.getParentSBMLObject() )
    self.assert_( lo == r.getModifier(0).getParentSBMLObject() )
    self.assert_( r == lo.getParentSBMLObject() )
    pass  

  def test_SpeciesReference_Product_parent_add(self):
    sr = libsbml.SpeciesReference()
    r = libsbml.Reaction()
    r.addProduct(sr)
    sr = None
    lo = r.getListOfProducts()
    self.assert_( lo == r.getProduct(0).getParentSBMLObject() )
    self.assert_( r == lo.getParentSBMLObject() )
    pass  

  def test_SpeciesReference_Product_parent_create(self):
    r = libsbml.Reaction()
    sr = r.createProduct()
    lo = r.getListOfProducts()
    self.assert_( lo == r.getProduct(0).getParentSBMLObject() )
    self.assert_( lo == sr.getParentSBMLObject() )
    self.assert_( r == lo.getParentSBMLObject() )
    pass  

  def test_SpeciesReference_Product_parent_create_model(self):
    m = libsbml.Model()
    r = m.createReaction()
    sr = m.createProduct()
    lo = r.getListOfProducts()
    self.assert_( lo == r.getProduct(0).getParentSBMLObject() )
    self.assert_( lo == sr.getParentSBMLObject() )
    self.assert_( r == lo.getParentSBMLObject() )
    pass  

  def test_SpeciesReference_Reactant_parent_add(self):
    sr = libsbml.SpeciesReference()
    r = libsbml.Reaction()
    r.addReactant(sr)
    sr = None
    lo = r.getListOfReactants()
    self.assert_( lo == r.getReactant(0).getParentSBMLObject() )
    self.assert_( r == lo.getParentSBMLObject() )
    pass  

  def test_SpeciesReference_Reactant_parent_create(self):
    r = libsbml.Reaction()
    sr = r.createReactant()
    lo = r.getListOfReactants()
    self.assert_( lo == r.getReactant(0).getParentSBMLObject() )
    self.assert_( lo == sr.getParentSBMLObject() )
    self.assert_( r == lo.getParentSBMLObject() )
    pass  

  def test_SpeciesReference_Reactant_parent_create_model(self):
    m = libsbml.Model()
    r = m.createReaction()
    sr = m.createReactant()
    lo = r.getListOfReactants()
    self.assert_( lo == r.getReactant(0).getParentSBMLObject() )
    self.assert_( lo == sr.getParentSBMLObject() )
    self.assert_( r == lo.getParentSBMLObject() )
    pass  

  def test_SpeciesType_parent_add(self):
    ia = libsbml.SpeciesType()
    m = libsbml.Model()
    m.addSpeciesType(ia)
    ia = None
    lo = m.getListOfSpeciesTypes()
    self.assert_( lo == m.getSpeciesType(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_SpeciesType_parent_create(self):
    m = libsbml.Model()
    st = m.createSpeciesType()
    lo = m.getListOfSpeciesTypes()
    self.assert_( lo == m.getSpeciesType(0).getParentSBMLObject() )
    self.assert_( lo == st.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Species_parent_add(self):
    ia = libsbml.Species()
    m = libsbml.Model()
    m.addSpecies(ia)
    ia = None
    lo = m.getListOfSpecies()
    self.assert_( lo == m.getSpecies(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Species_parent_create(self):
    m = libsbml.Model()
    s = m.createSpecies()
    lo = m.getListOfSpecies()
    self.assert_( lo == s.getParentSBMLObject() )
    self.assert_( lo == m.getSpecies(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_StoichiometryMath_parent_add(self):
    m = libsbml.StoichiometryMath()
    sr = libsbml.SpeciesReference()
    sr.setStoichiometryMath(m)
    m = None
    self.assert_( sr == sr.getStoichiometryMath().getParentSBMLObject() )
    sr = None
    pass  

  def test_Trigger_parent_add(self):
    d = libsbml.Trigger()
    e = libsbml.Event()
    e.setTrigger(d)
    d = None
    self.assert_( e == e.getTrigger().getParentSBMLObject() )
    e = None
    pass  

  def test_UnitDefinition_parent_add(self):
    ia = libsbml.UnitDefinition()
    m = libsbml.Model()
    m.addUnitDefinition(ia)
    ia = None
    lo = m.getListOfUnitDefinitions()
    self.assert_( lo == m.getUnitDefinition(0).getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_UnitDefinition_parent_create(self):
    m = libsbml.Model()
    ud = m.createUnitDefinition()
    lo = m.getListOfUnitDefinitions()
    self.assert_( lo == m.getUnitDefinition(0).getParentSBMLObject() )
    self.assert_( lo == ud.getParentSBMLObject() )
    self.assert_( m == lo.getParentSBMLObject() )
    pass  

  def test_Unit_parent_add(self):
    ud = libsbml.UnitDefinition()
    u = libsbml.Unit()
    ud.addUnit(u)
    u = None
    self.assert_( ud.getNumUnits() == 1 )
    lo = ud.getListOfUnits()
    self.assert_( lo == ud.getUnit(0).getParentSBMLObject() )
    self.assert_( ud == lo.getParentSBMLObject() )
    ud = None
    pass  

  def test_Unit_parent_create(self):
    ud = libsbml.UnitDefinition()
    u = ud.createUnit()
    self.assert_( ud.getNumUnits() == 1 )
    lo = ud.getListOfUnits()
    self.assert_( lo == ud.getUnit(0).getParentSBMLObject() )
    self.assert_( lo == u.getParentSBMLObject() )
    self.assert_( ud == lo.getParentSBMLObject() )
    ud = None
    pass  

  def test_Unit_parent_create_model(self):
    m = libsbml.Model()
    ud = m.createUnitDefinition()
    u = m.createUnit()
    self.assert_( ud.getNumUnits() == 1 )
    lo = ud.getListOfUnits()
    self.assert_( lo == ud.getUnit(0).getParentSBMLObject() )
    self.assert_( lo == u.getParentSBMLObject() )
    self.assert_( ud == lo.getParentSBMLObject() )
    ud = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestSBMLParentObject))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
