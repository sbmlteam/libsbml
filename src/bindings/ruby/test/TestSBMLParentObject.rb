#
# @file    TestSBMLParentObject.rb
# @brief   SBML parent object unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
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
require 'test/unit'
require 'libSBML'

class TestSBMLParentObject < Test::Unit::TestCase

  def test_AlgebraicRule_parent_create
    m = LibSBML::Model.new()
    r = m.createAlgebraicRule()
    lo = m.getListOfRules()
    assert( lo == m.getRule(0).getParentSBMLObject() )
    assert( lo == r.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_AssignmentRule_parent_create
    m = LibSBML::Model.new()
    r = m.createAssignmentRule()
    lo = m.getListOfRules()
    assert( lo == m.getRule(0).getParentSBMLObject() )
    assert( lo == r.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_CompartmentType_parent_add
    ct = LibSBML::CompartmentType.new()
    m = LibSBML::Model.new()
    m.addCompartmentType(ct)
    ct = nil
    lo = m.getListOfCompartmentTypes()
    assert( lo == m.getCompartmentType(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_CompartmentType_parent_create
    m = LibSBML::Model.new()
    ct = m.createCompartmentType()
    lo = m.getListOfCompartmentTypes()
    assert( lo == m.getCompartmentType(0).getParentSBMLObject() )
    assert( lo == ct.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Compartment_parent_add
    c = LibSBML::Compartment.new()
    m = LibSBML::Model.new()
    m.addCompartment(c)
    c = nil
    lo = m.getListOfCompartments()
    assert( lo == m.getCompartment(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Compartment_parent_create
    m = LibSBML::Model.new()
    c = m.createCompartment()
    lo = m.getListOfCompartments()
    assert( lo == m.getCompartment(0).getParentSBMLObject() )
    assert( lo == c.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Constraint_parent_add
    ct = LibSBML::Constraint.new()
    m = LibSBML::Model.new()
    m.addConstraint(ct)
    ct = nil
    lo = m.getListOfConstraints()
    assert( lo == m.getConstraint(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Constraint_parent_create
    m = LibSBML::Model.new()
    ct = m.createConstraint()
    lo = m.getListOfConstraints()
    assert( lo == m.getConstraint(0).getParentSBMLObject() )
    assert( lo == ct.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Delay_parent_add
    d = LibSBML::Delay.new()
    e = LibSBML::Event.new()
    e.setDelay(d)
    d = nil
    assert( e == e.getDelay().getParentSBMLObject() )
    e = nil
  end

  def test_EventAssignment_parent_add
    e = LibSBML::Event.new()
    ea = LibSBML::EventAssignment.new()
    e.addEventAssignment(ea)
    ea = nil
    lo = e.getListOfEventAssignments()
    assert( lo == e.getEventAssignment(0).getParentSBMLObject() )
    assert( e == lo.getParentSBMLObject() )
  end

  def test_EventAssignment_parent_create
    e = LibSBML::Event.new()
    ea = e.createEventAssignment()
    lo = e.getListOfEventAssignments()
    assert( lo == e.getEventAssignment(0).getParentSBMLObject() )
    assert( lo == ea.getParentSBMLObject() )
    assert( e == lo.getParentSBMLObject() )
  end

  def test_EventAssignment_parent_create_model
    m = LibSBML::Model.new()
    e = m.createEvent()
    ea = m.createEventAssignment()
    lo = e.getListOfEventAssignments()
    assert( lo == e.getEventAssignment(0).getParentSBMLObject() )
    assert( lo == ea.getParentSBMLObject() )
    assert( e == lo.getParentSBMLObject() )
  end

  def test_Event_parent_add
    e = LibSBML::Event.new()
    m = LibSBML::Model.new()
    m.addEvent(e)
    e = nil
    lo = m.getListOfEvents()
    assert( lo == m.getEvent(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Event_parent_create
    m = LibSBML::Model.new()
    e = m.createEvent()
    lo = m.getListOfEvents()
    assert( lo == m.getEvent(0).getParentSBMLObject() )
    assert( lo == e.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_FunctionDefinition_parent_add
    fd = LibSBML::FunctionDefinition.new()
    m = LibSBML::Model.new()
    m.addFunctionDefinition(fd)
    fd = nil
    lo = m.getListOfFunctionDefinitions()
    assert( lo == m.getFunctionDefinition(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_FunctionDefinition_parent_create
    m = LibSBML::Model.new()
    fd = m.createFunctionDefinition()
    lo = m.getListOfFunctionDefinitions()
    assert( lo == m.getFunctionDefinition(0).getParentSBMLObject() )
    assert( lo == fd.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_InitialAssignment_parent_add
    ia = LibSBML::InitialAssignment.new()
    m = LibSBML::Model.new()
    m.addInitialAssignment(ia)
    ia = nil
    lo = m.getListOfInitialAssignments()
    assert( lo == m.getInitialAssignment(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_InitialAssignment_parent_create
    m = LibSBML::Model.new()
    ia = m.createInitialAssignment()
    lo = m.getListOfInitialAssignments()
    assert( lo == m.getInitialAssignment(0).getParentSBMLObject() )
    assert( lo == ia.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_KineticLaw_Parameter_parent_add
    kl = LibSBML::KineticLaw.new()
    p = LibSBML::Parameter.new("jake")
    kl.addParameter(p)
    p = nil
    assert( kl.getNumParameters() == 1 )
    assert( kl.getParameter(0).getId() == "jake" )
    lop = kl.getListOfParameters()
    assert( kl == lop.getParentSBMLObject() )
    assert( lop == kl.getParameter(0).getParentSBMLObject() )
    kl = nil
  end

  def test_KineticLaw_Parameter_parent_create
    kl = LibSBML::KineticLaw.new()
    p = kl.createParameter()
    assert( kl.getNumParameters() == 1 )
    lop = kl.getListOfParameters()
    assert( kl == lop.getParentSBMLObject() )
    assert( lop == p.getParentSBMLObject() )
    assert( lop == kl.getParameter(0).getParentSBMLObject() )
    kl = nil
  end

  def test_KineticLaw_Parameter_parent_create_model
    m = LibSBML::Model.new()
    r = m.createReaction()
    kl = m.createKineticLaw()
    p = m.createKineticLawParameter()
    assert( kl.getNumParameters() == 1 )
    lop = kl.getListOfParameters()
    assert( r == kl.getParentSBMLObject() )
    assert( kl == lop.getParentSBMLObject() )
    assert( lop == p.getParentSBMLObject() )
    assert( lop == kl.getParameter(0).getParentSBMLObject() )
    kl = nil
  end

  def test_KineticLaw_parent_add
    kl = LibSBML::KineticLaw.new()
    r = LibSBML::Reaction.new()
    r.setKineticLaw(kl)
    assert( r == r.getKineticLaw().getParentSBMLObject() )
    r = nil
  end

  def test_KineticLaw_parent_create
    r = LibSBML::Reaction.new()
    kl = r.createKineticLaw()
    assert( r == kl.getParentSBMLObject() )
    r = nil
  end

  def test_KineticLaw_parent_create_model
    m = LibSBML::Model.new()
    r = m.createReaction()
    kl = r.createKineticLaw()
    assert( r == kl.getParentSBMLObject() )
    assert( r == r.getKineticLaw().getParentSBMLObject() )
    r = nil
  end

  def test_Model_parent_add
    d = LibSBML::SBMLDocument.new()
    m = LibSBML::Model.new()
    d.setModel(m)
    assert( d == d.getModel().getParentSBMLObject() )
    d = nil
  end

  def test_Model_parent_create
    d = LibSBML::SBMLDocument.new()
    m = d.createModel()
    assert( d == m.getParentSBMLObject() )
    d = nil
  end

  def test_Parameter_parent_add
    ia = LibSBML::Parameter.new()
    m = LibSBML::Model.new()
    m.addParameter(ia)
    ia = nil
    lo = m.getListOfParameters()
    assert( lo == m.getParameter(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Parameter_parent_create
    m = LibSBML::Model.new()
    p = m.createParameter()
    lo = m.getListOfParameters()
    assert( lo == m.getParameter(0).getParentSBMLObject() )
    assert( lo == p.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_RateRule_parent_create
    m = LibSBML::Model.new()
    r = m.createRateRule()
    lo = m.getListOfRules()
    assert( lo == m.getRule(0).getParentSBMLObject() )
    assert( lo == r.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Reaction_parent_add
    ia = LibSBML::Reaction.new()
    m = LibSBML::Model.new()
    m.addReaction(ia)
    ia = nil
    lo = m.getListOfReactions()
    assert( lo == m.getReaction(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Reaction_parent_create
    m = LibSBML::Model.new()
    r = m.createReaction()
    lo = m.getListOfReactions()
    assert( lo == m.getReaction(0).getParentSBMLObject() )
    assert( lo == r.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Rule_parent_add
    ia = LibSBML::RateRule.new("a")
    m = LibSBML::Model.new()
    m.addRule(ia)
    ia = nil
    lo = m.getListOfRules()
    assert( lo == m.getRule(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_SpeciesReference_Modifier_parent_add
    sr = LibSBML::ModifierSpeciesReference.new()
    r = LibSBML::Reaction.new()
    r.addModifier(sr)
    sr = nil
    lo = r.getListOfModifiers()
    assert( lo == r.getModifier(0).getParentSBMLObject() )
    assert( r == lo.getParentSBMLObject() )
  end

  def test_SpeciesReference_Modifier_parent_create
    r = LibSBML::Reaction.new()
    sr = r.createModifier()
    lo = r.getListOfModifiers()
    assert( lo == sr.getParentSBMLObject() )
    assert( lo == r.getModifier(0).getParentSBMLObject() )
    assert( r == lo.getParentSBMLObject() )
  end

  def test_SpeciesReference_Modifier_parent_create_model
    m = LibSBML::Model.new()
    r = m.createReaction()
    sr = m.createModifier()
    lo = r.getListOfModifiers()
    assert( lo == sr.getParentSBMLObject() )
    assert( lo == r.getModifier(0).getParentSBMLObject() )
    assert( r == lo.getParentSBMLObject() )
  end

  def test_SpeciesReference_Product_parent_add
    sr = LibSBML::SpeciesReference.new()
    r = LibSBML::Reaction.new()
    r.addProduct(sr)
    sr = nil
    lo = r.getListOfProducts()
    assert( lo == r.getProduct(0).getParentSBMLObject() )
    assert( r == lo.getParentSBMLObject() )
  end

  def test_SpeciesReference_Product_parent_create
    r = LibSBML::Reaction.new()
    sr = r.createProduct()
    lo = r.getListOfProducts()
    assert( lo == r.getProduct(0).getParentSBMLObject() )
    assert( lo == sr.getParentSBMLObject() )
    assert( r == lo.getParentSBMLObject() )
  end

  def test_SpeciesReference_Product_parent_create_model
    m = LibSBML::Model.new()
    r = m.createReaction()
    sr = m.createProduct()
    lo = r.getListOfProducts()
    assert( lo == r.getProduct(0).getParentSBMLObject() )
    assert( lo == sr.getParentSBMLObject() )
    assert( r == lo.getParentSBMLObject() )
  end

  def test_SpeciesReference_Reactant_parent_add
    sr = LibSBML::SpeciesReference.new()
    r = LibSBML::Reaction.new()
    r.addReactant(sr)
    sr = nil
    lo = r.getListOfReactants()
    assert( lo == r.getReactant(0).getParentSBMLObject() )
    assert( r == lo.getParentSBMLObject() )
  end

  def test_SpeciesReference_Reactant_parent_create
    r = LibSBML::Reaction.new()
    sr = r.createReactant()
    lo = r.getListOfReactants()
    assert( lo == r.getReactant(0).getParentSBMLObject() )
    assert( lo == sr.getParentSBMLObject() )
    assert( r == lo.getParentSBMLObject() )
  end

  def test_SpeciesReference_Reactant_parent_create_model
    m = LibSBML::Model.new()
    r = m.createReaction()
    sr = m.createReactant()
    lo = r.getListOfReactants()
    assert( lo == r.getReactant(0).getParentSBMLObject() )
    assert( lo == sr.getParentSBMLObject() )
    assert( r == lo.getParentSBMLObject() )
  end

  def test_SpeciesType_parent_add
    ia = LibSBML::SpeciesType.new()
    m = LibSBML::Model.new()
    m.addSpeciesType(ia)
    ia = nil
    lo = m.getListOfSpeciesTypes()
    assert( lo == m.getSpeciesType(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_SpeciesType_parent_create
    m = LibSBML::Model.new()
    st = m.createSpeciesType()
    lo = m.getListOfSpeciesTypes()
    assert( lo == m.getSpeciesType(0).getParentSBMLObject() )
    assert( lo == st.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Species_parent_add
    ia = LibSBML::Species.new()
    m = LibSBML::Model.new()
    m.addSpecies(ia)
    ia = nil
    lo = m.getListOfSpecies()
    assert( lo == m.getSpecies(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Species_parent_create
    m = LibSBML::Model.new()
    s = m.createSpecies()
    lo = m.getListOfSpecies()
    assert( lo == s.getParentSBMLObject() )
    assert( lo == m.getSpecies(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_StoichiometryMath_parent_add
    m = LibSBML::StoichiometryMath.new()
    sr = LibSBML::SpeciesReference.new()
    sr.setStoichiometryMath(m)
    m = nil
    assert( sr == sr.getStoichiometryMath().getParentSBMLObject() )
    sr = nil
  end

  def test_Trigger_parent_add
    d = LibSBML::Trigger.new()
    e = LibSBML::Event.new()
    e.setTrigger(d)
    d = nil
    assert( e == e.getTrigger().getParentSBMLObject() )
    e = nil
  end

  def test_UnitDefinition_parent_add
    ia = LibSBML::UnitDefinition.new()
    m = LibSBML::Model.new()
    m.addUnitDefinition(ia)
    ia = nil
    lo = m.getListOfUnitDefinitions()
    assert( lo == m.getUnitDefinition(0).getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_UnitDefinition_parent_create
    m = LibSBML::Model.new()
    ud = m.createUnitDefinition()
    lo = m.getListOfUnitDefinitions()
    assert( lo == m.getUnitDefinition(0).getParentSBMLObject() )
    assert( lo == ud.getParentSBMLObject() )
    assert( m == lo.getParentSBMLObject() )
  end

  def test_Unit_parent_add
    ud = LibSBML::UnitDefinition.new()
    u = LibSBML::Unit.new()
    ud.addUnit(u)
    u = nil
    assert( ud.getNumUnits() == 1 )
    lo = ud.getListOfUnits()
    assert( lo == ud.getUnit(0).getParentSBMLObject() )
    assert( ud == lo.getParentSBMLObject() )
    ud = nil
  end

  def test_Unit_parent_create
    ud = LibSBML::UnitDefinition.new()
    u = ud.createUnit()
    assert( ud.getNumUnits() == 1 )
    lo = ud.getListOfUnits()
    assert( lo == ud.getUnit(0).getParentSBMLObject() )
    assert( lo == u.getParentSBMLObject() )
    assert( ud == lo.getParentSBMLObject() )
    ud = nil
  end

  def test_Unit_parent_create_model
    m = LibSBML::Model.new()
    ud = m.createUnitDefinition()
    u = m.createUnit()
    assert( ud.getNumUnits() == 1 )
    lo = ud.getListOfUnits()
    assert( lo == ud.getUnit(0).getParentSBMLObject() )
    assert( lo == u.getParentSBMLObject() )
    assert( ud == lo.getParentSBMLObject() )
    ud = nil
  end

end
