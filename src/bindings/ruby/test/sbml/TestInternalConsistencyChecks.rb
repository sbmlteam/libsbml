#
# @file    TestInternalConsistencyChecks.rb
# @brief   Tests the internal consistency validation.
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestInternalConsistencyChecks.cpp
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

class TestInternalConsistencyChecks < Test::Unit::TestCase

  def test_internal_consistency_check_99901
    d = LibSBML::SBMLDocument.new()
    c = LibSBML::Compartment.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setSpatialDimensions(2)
    c.setId("c")
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    assert( errors == 1 )
    assert( d.getError(0).getErrorId() == 10103 )
    d = nil
  end

  def test_internal_consistency_check_99902
    d = LibSBML::SBMLDocument.new()
    c = LibSBML::Compartment.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setCompartmentType("hh")
    c.setId("c")
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    assert( errors == 1 )
    assert( d.getError(0).getErrorId() == 10103 )
    d = nil
  end

  def test_internal_consistency_check_99903
    d = LibSBML::SBMLDocument.new()
    c = LibSBML::Compartment.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setConstant(true)
    c.setId("c")
    m.addCompartment(c)
    r = m.createAssignmentRule()
    r.setVariable("c")
    r.setFormula("2*3")
    errors = d.checkInternalConsistency()
    assert( errors == 3 )
    d = nil
  end

  def test_internal_consistency_check_99903_localparam
    d = LibSBML::SBMLDocument.new()
    p = LibSBML::Parameter.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    r = m.createReaction()
    r.setId("r")
    kl = r.createKineticLaw()
    kl.setFormula("2")
    p.setId("p")
    p.setConstant(false)
    kl.addParameter(p)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99903_param
    d = LibSBML::SBMLDocument.new()
    p = LibSBML::Parameter.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    p.setConstant(true)
    p.setId("c")
    m.addParameter(p)
    r = m.createAssignmentRule()
    r.setVariable("c")
    r.setFormula("2*3")
    errors = d.checkInternalConsistency()
    assert( errors == 2 )
    d = nil
  end

  def test_internal_consistency_check_99904
    d = LibSBML::SBMLDocument.new()
    c = LibSBML::Compartment.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setId("c")
    c.setMetaId("mmm")
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    assert( errors == 1 )
    assert( d.getError(0).getErrorId() == 10103 )
    d = nil
  end

  def test_internal_consistency_check_99904_kl
    d = LibSBML::SBMLDocument.new()
    kl = LibSBML::KineticLaw.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(1,2)
    c = m.createCompartment()
    c.setId("cc")
    r = m.createReaction()
    r.setId("r")
    kl.setFormula("2")
    kl.setMetaId("mmm")
    r.setKineticLaw(kl)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99904_model
    d = LibSBML::SBMLDocument.new()
    d.setLevelAndVersion(1,2)
    m = LibSBML::Model.new(2,4)
    c = m.createCompartment()
    c.setId("cc")
    m.setMetaId("mmm")
    d.setModel(m)
    errors = d.checkInternalConsistency()
    assert( errors == 1 )
    assert( d.getError(0).getErrorId() == 20201 )
    d = nil
  end

  def test_internal_consistency_check_99904_param
    d = LibSBML::SBMLDocument.new()
    p = LibSBML::Parameter.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    p.setId("p")
    p.setMetaId("mmm")
    m.addParameter(p)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99904_react
    d = LibSBML::SBMLDocument.new()
    r = LibSBML::Reaction.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    r.setId("r")
    r.setMetaId("mmm")
    m.addReaction(r)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99904_rule_alg
    d = LibSBML::SBMLDocument.new()
    r = LibSBML::AlgebraicRule.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    r.setMetaId("mmm")
    r.setFormula("2")
    m.addRule(r)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99904_rule_assign
    d = LibSBML::SBMLDocument.new()
    r = LibSBML::AssignmentRule.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    c.setConstant(false)
    r.setVariable("cc")
    r.setFormula("2")
    r.setMetaId("mmm")
    m.addRule(r)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99904_rule_rate
    d = LibSBML::SBMLDocument.new()
    r = LibSBML::RateRule.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    c.setConstant(false)
    r.setVariable("cc")
    r.setFormula("2")
    r.setMetaId("mmm")
    m.addRule(r)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99904_species
    d = LibSBML::SBMLDocument.new()
    s = LibSBML::Species.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setCompartment("c")
    s.setId("s")
    s.setMetaId("mmm")
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99904_speciesRef
    d = LibSBML::SBMLDocument.new()
    sr = LibSBML::SpeciesReference.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s = m.createSpecies()
    s.setId("s")
    r = m.createReaction()
    r.setId("r")
    s.setCompartment("c")
    sr.setSpecies("s")
    sr.setMetaId("mmm")
    r.addProduct(sr)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99904_unit
    d = LibSBML::SBMLDocument.new()
    u = LibSBML::Unit.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    ud = m.createUnitDefinition()
    ud.setId("ud")
    u.setMetaId("mmm")
    u.setKind(LibSBML::UNIT_KIND_MOLE)
    ud.addUnit(u)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99904_unitdef
    d = LibSBML::SBMLDocument.new()
    u = LibSBML::UnitDefinition.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    u.setId("ud")
    u.setMetaId("mmm")
    u.createUnit()
    m.addUnitDefinition(u)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99905
    d = LibSBML::SBMLDocument.new()
    c = LibSBML::Compartment.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setId("c")
    c.setSBOTerm(2)
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    assert( errors == 1 )
    assert( d.getError(0).getErrorId() == 10103 )
    d = nil
  end

  def test_internal_consistency_check_99905_ct
    d = LibSBML::SBMLDocument.new()
    ct = LibSBML::CompartmentType.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(2,2)
    ct.setId("ct")
    ct.setSBOTerm(5)
    m.addCompartmentType(ct)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99905_delay
    d = LibSBML::SBMLDocument.new()
    delay = LibSBML::Delay.new(2,4)
    e = LibSBML::Event.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(2,2)
    delay.setSBOTerm(5)
    e.setDelay(delay)
    m.addEvent(e)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99905_species
    d = LibSBML::SBMLDocument.new()
    s = LibSBML::Species.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setId("s")
    s.setCompartment("c")
    s.setSBOTerm(2)
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99905_st
    d = LibSBML::SBMLDocument.new()
    ct = LibSBML::SpeciesType.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(2,2)
    ct.setId("st")
    ct.setSBOTerm(5)
    m.addSpeciesType(ct)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99905_stoichmath
    d = LibSBML::SBMLDocument.new()
    sm = LibSBML::StoichiometryMath.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(2,2)
    s = m.createSpecies()
    s.setId("s")
    c = m.createCompartment()
    c.setId("c")
    s.setCompartment("c")
    r = m.createReaction()
    r.setId("r")
    sr = r.createProduct()
    sr.setSpecies("s")
    sm.setSBOTerm(5)
    sr.setStoichiometryMath(sm)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99905_trigger
    d = LibSBML::SBMLDocument.new()
    trigger = LibSBML::Trigger.new(2,4)
    e = LibSBML::Event.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(2,2)
    trigger.setSBOTerm(5)
    e.setTrigger(trigger)
    m.addEvent(e)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99905_unit
    d = LibSBML::SBMLDocument.new()
    u = LibSBML::Unit.new(2,4)
    d.setLevelAndVersion(2,2)
    m = d.createModel()
    ud = m.createUnitDefinition()
    ud.setId("ud")
    u.setKind(LibSBML::UNIT_KIND_MOLE)
    u.setSBOTerm(9)
    ud.addUnit(u)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99905_unitdef
    d = LibSBML::SBMLDocument.new()
    u = LibSBML::UnitDefinition.new(2,4)
    d.setLevelAndVersion(2,2)
    m = d.createModel()
    u.setId("ud")
    u.setSBOTerm(9)
    u.createUnit()
    m.addUnitDefinition(u)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99906
    d = LibSBML::SBMLDocument.new()
    c = LibSBML::Compartment.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setId("c")
    c.setUnits("mole")
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    assert( errors == 1 )
    assert( d.getError(0).getErrorId() == 10103 )
    d = nil
  end

  def test_internal_consistency_check_99907
    d = LibSBML::SBMLDocument.new()
    c = LibSBML::Compartment.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setId("c")
    c.unsetVolume()
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    assert( errors == 1 )
    assert( d.getError(0).getErrorId() == 10103 )
    d = nil
  end

  def test_internal_consistency_check_99908
    d = LibSBML::SBMLDocument.new()
    ct = LibSBML::CompartmentType.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(2,1)
    ct.setId("ct")
    m.addCompartmentType(ct)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99909
    d = LibSBML::SBMLDocument.new()
    ct = LibSBML::Constraint.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(2,1)
    m.addConstraint(ct)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99910
    d = LibSBML::SBMLDocument.new()
    e = LibSBML::Event.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(1,2)
    c = m.createCompartment()
    c.setId("cc")
    c.setConstant(false)
    m.addEvent(e)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99911_ea
    d = LibSBML::SBMLDocument.new()
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    c.setConstant(false)
    e = m.createEvent()
    ea = LibSBML::EventAssignment.new(2,4)
    d.setLevelAndVersion(2,1)
    ea.setVariable("c")
    ea.setSBOTerm(2)
    e.addEventAssignment(ea)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99911_event
    d = LibSBML::SBMLDocument.new()
    e = LibSBML::Event.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(2,1)
    e.setSBOTerm(2)
    m.addEvent(e)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99911_fd
    d = LibSBML::SBMLDocument.new()
    m = d.createModel()
    fd = LibSBML::FunctionDefinition.new(2,4)
    d.setLevelAndVersion(2,1)
    fd.setId("fd")
    fd.setSBOTerm(2)
    m.addFunctionDefinition(fd)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99911_kl
    d = LibSBML::SBMLDocument.new()
    m = d.createModel()
    r = m.createReaction()
    r.setId("r")
    kl = LibSBML::KineticLaw.new(2,4)
    d.setLevelAndVersion(2,1)
    kl.setSBOTerm(2)
    p = kl.createParameter()
    p.setId("p")
    r.setKineticLaw(kl)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99911_model
    d = LibSBML::SBMLDocument.new()
    d.setLevelAndVersion(2,1)
    m = LibSBML::Model.new(2,4)
    m.setSBOTerm(2)
    d.setModel(m)
    errors = d.checkInternalConsistency()
    assert( errors == 1 )
    assert( d.getError(0).getErrorId() == 20201 )
    d = nil
  end

  def test_internal_consistency_check_99911_param
    d = LibSBML::SBMLDocument.new()
    m = d.createModel()
    p = LibSBML::Parameter.new(2,4)
    d.setLevelAndVersion(2,1)
    p.setId("p")
    p.setSBOTerm(2)
    m.addParameter(p)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99911_react
    d = LibSBML::SBMLDocument.new()
    m = d.createModel()
    r = LibSBML::Reaction.new(2,4)
    d.setLevelAndVersion(2,1)
    r.setId("r")
    r.setSBOTerm(2)
    m.addReaction(r)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99911_rule_alg
    d = LibSBML::SBMLDocument.new()
    m = d.createModel()
    r = LibSBML::AlgebraicRule.new(2,4)
    d.setLevelAndVersion(2,1)
    r.setSBOTerm(2)
    m.addRule(r)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99911_rule_assign
    d = LibSBML::SBMLDocument.new()
    m = d.createModel()
    p = m.createParameter()
    p.setId("p")
    p.setConstant(false)
    r = LibSBML::AssignmentRule.new(2,4)
    d.setLevelAndVersion(2,1)
    r.setVariable("p")
    r.setSBOTerm(2)
    m.addRule(r)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99911_rule_rate
    d = LibSBML::SBMLDocument.new()
    m = d.createModel()
    p = m.createParameter()
    p.setId("p")
    p.setConstant(false)
    r = LibSBML::RateRule.new(2,4)
    d.setLevelAndVersion(2,1)
    r.setVariable("p")
    r.setSBOTerm(2)
    m.addRule(r)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99911_speciesRef
    d = LibSBML::SBMLDocument.new()
    sr = LibSBML::SpeciesReference.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s = m.createSpecies()
    s.setId("s")
    r = m.createReaction()
    r.setId("r")
    s.setCompartment("c")
    sr.setSpecies("s")
    sr.setSBOTerm(4)
    r.addReactant(sr)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99912
    d = LibSBML::SBMLDocument.new()
    fd = LibSBML::FunctionDefinition.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(1,2)
    c = m.createCompartment()
    c.setId("cc")
    c.setConstant(false)
    m.addFunctionDefinition(fd)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99913
    d = LibSBML::SBMLDocument.new()
    ia = LibSBML::InitialAssignment.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(1,2)
    c = m.createCompartment()
    c.setId("cc")
    c.setConstant(false)
    m.addInitialAssignment(ia)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99914
    d = LibSBML::SBMLDocument.new()
    m = d.createModel()
    r = LibSBML::AlgebraicRule.new(2,4)
    d.setLevelAndVersion(2,1)
    r.setVariable("kk")
    m.addRule(r)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99915_alg
    d = LibSBML::SBMLDocument.new()
    m = d.createModel()
    r = LibSBML::AlgebraicRule.new(2,4)
    d.setLevelAndVersion(2,1)
    r.setUnits("kk")
    m.addRule(r)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99915_assign
    d = LibSBML::SBMLDocument.new()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    c.setConstant(false)
    r = m.createAssignmentRule()
    r.setL1TypeCode(LibSBML::SBML_SPECIES_CONCENTRATION_RULE)
    r.setVariable("c")
    r.setFormula("2")
    r.setUnits("mmm")
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99915_rate
    d = LibSBML::SBMLDocument.new()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    c.setConstant(false)
    r = m.createRateRule()
    r.setL1TypeCode(LibSBML::SBML_SPECIES_CONCENTRATION_RULE)
    r.setFormula("2")
    r.setVariable("c")
    r.setUnits("mmm")
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99916_reaction
    d = LibSBML::SBMLDocument.new()
    s = LibSBML::Species.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    r = m.createReaction()
    r.setId("r")
    sr = r.createReactant()
    s.setId("s")
    s.setCompartment("c")
    s.setConstant(true)
    sr.setSpecies("s")
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99916_rule
    d = LibSBML::SBMLDocument.new()
    s = LibSBML::Species.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setId("s")
    s.setCompartment("c")
    s.setConstant(true)
    m.addSpecies(s)
    r = m.createAssignmentRule()
    r.setVariable("s")
    r.setFormula("2")
    errors = d.checkInternalConsistency()
    assert( errors == 2 )
    d = nil
  end

  def test_internal_consistency_check_99917
    d = LibSBML::SBMLDocument.new()
    s = LibSBML::Species.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setId("s")
    s.setCompartment("c")
    s.setSpatialSizeUnits("kkk")
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99918
    d = LibSBML::SBMLDocument.new()
    s = LibSBML::Species.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setId("s")
    s.setCompartment("c")
    s.setSpeciesType("kkk")
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99919
    d = LibSBML::SBMLDocument.new()
    s = LibSBML::Species.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setId("s")
    s.setCompartment("c")
    s.setHasOnlySubstanceUnits(true)
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99920
    d = LibSBML::SBMLDocument.new()
    sr = LibSBML::SpeciesReference.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s = m.createSpecies()
    s.setId("s")
    r = m.createReaction()
    r.setId("r")
    s.setCompartment("c")
    sr.setSpecies("s")
    sr.setId("mmm")
    r.addProduct(sr)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99921
    d = LibSBML::SBMLDocument.new()
    sr = LibSBML::SpeciesReference.new(2,4)
    d.setLevelAndVersion(2,1)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s = m.createSpecies()
    s.setId("s")
    r = m.createReaction()
    r.setId("r")
    s.setCompartment("c")
    sr.setSpecies("s")
    sr.setName("mmm")
    r.addReactant(sr)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99922
    d = LibSBML::SBMLDocument.new()
    ct = LibSBML::SpeciesType.new(2,4)
    m = d.createModel()
    ct.setId("st")
    d.setLevelAndVersion(2,1)
    m.addSpeciesType(ct)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99923
    d = LibSBML::SBMLDocument.new()
    sm = LibSBML::StoichiometryMath.new(2,4)
    m = d.createModel()
    d.setLevelAndVersion(1,2)
    s = m.createSpecies()
    s.setId("s")
    c = m.createCompartment()
    c.setId("c")
    s.setCompartment("c")
    r = m.createReaction()
    r.setId("r")
    sr = r.createProduct()
    sr.setSpecies("s")
    sr.setStoichiometryMath(sm)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99924
    d = LibSBML::SBMLDocument.new()
    u = LibSBML::Unit.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    ud = m.createUnitDefinition()
    ud.setId("ud")
    u.setKind(LibSBML::UNIT_KIND_MOLE)
    u.setMultiplier(9)
    ud.addUnit(u)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

  def test_internal_consistency_check_99925
    d = LibSBML::SBMLDocument.new()
    u = LibSBML::Unit.new(2,4)
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    ud = m.createUnitDefinition()
    ud.setId("ud")
    u.setKind(LibSBML::UNIT_KIND_MOLE)
    u.setOffset(9)
    ud.addUnit(u)
    errors = d.checkInternalConsistency()
    assert( errors == 0 )
    d = nil
  end

end
