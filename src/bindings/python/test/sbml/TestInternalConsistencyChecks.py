#
# @file    TestInternalConsistencyChecks.py
# @brief   Tests the internal consistency validation.
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
#
# $Id:$
# $HeadURL:$
#
# This test file was converted from src/sbml/test/TestInternalConsistencyChecks.cpp
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

class TestInternalConsistencyChecks(unittest.TestCase):


  def test_internal_consistency_check_99901(self):
    d = libsbml.SBMLDocument()
    c = libsbml.Compartment()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setSpatialDimensions(2)
    c.setId("c")
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99901 )
    d = None
    pass  

  def test_internal_consistency_check_99902(self):
    d = libsbml.SBMLDocument()
    c = libsbml.Compartment()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setCompartmentType("hh")
    c.setId("c")
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99902 )
    d = None
    pass  

  def test_internal_consistency_check_99903(self):
    d = libsbml.SBMLDocument()
    c = libsbml.Compartment()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setConstant(True)
    c.setId("c")
    m.addCompartment(c)
    r = m.createAssignmentRule()
    r.setVariable("c")
    r.setFormula("2*3")
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99903 )
    d = None
    pass  

  def test_internal_consistency_check_99903_localparam(self):
    d = libsbml.SBMLDocument()
    p = libsbml.Parameter()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    r = m.createReaction()
    r.setId("r")
    kl = r.createKineticLaw()
    kl.setFormula("2")
    p.setId("p")
    p.setConstant(False)
    kl.addParameter(p)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99903 )
    d = None
    pass  

  def test_internal_consistency_check_99903_param(self):
    d = libsbml.SBMLDocument()
    p = libsbml.Parameter()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    p.setConstant(True)
    p.setId("c")
    m.addParameter(p)
    r = m.createAssignmentRule()
    r.setVariable("c")
    r.setFormula("2*3")
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99903 )
    d = None
    pass  

  def test_internal_consistency_check_99904(self):
    d = libsbml.SBMLDocument()
    c = libsbml.Compartment()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setId("c")
    c.setMetaId("mmm")
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99904_kl(self):
    d = libsbml.SBMLDocument()
    kl = libsbml.KineticLaw()
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
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99904_model(self):
    d = libsbml.SBMLDocument()
    d.setLevelAndVersion(1,2)
    m = libsbml.Model()
    c = m.createCompartment()
    c.setId("cc")
    m.setMetaId("mmm")
    d.setModel(m)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99904_param(self):
    d = libsbml.SBMLDocument()
    p = libsbml.Parameter()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    p.setId("p")
    p.setMetaId("mmm")
    m.addParameter(p)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99904_react(self):
    d = libsbml.SBMLDocument()
    r = libsbml.Reaction()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    r.setId("r")
    r.setMetaId("mmm")
    m.addReaction(r)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99904_rule_alg(self):
    d = libsbml.SBMLDocument()
    r = libsbml.AlgebraicRule()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    r.setMetaId("mmm")
    r.setFormula("2")
    m.addRule(r)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99904_rule_assign(self):
    d = libsbml.SBMLDocument()
    r = libsbml.AssignmentRule()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    c.setConstant(False)
    r.setVariable("cc")
    r.setFormula("2")
    r.setMetaId("mmm")
    m.addRule(r)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99904_rule_rate(self):
    d = libsbml.SBMLDocument()
    r = libsbml.RateRule()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    c.setConstant(False)
    r.setVariable("cc")
    r.setFormula("2")
    r.setMetaId("mmm")
    m.addRule(r)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99904_species(self):
    d = libsbml.SBMLDocument()
    s = libsbml.Species()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setCompartment("c")
    s.setId("s")
    s.setMetaId("mmm")
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99904_speciesRef(self):
    d = libsbml.SBMLDocument()
    sr = libsbml.SpeciesReference()
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
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99904_unit(self):
    d = libsbml.SBMLDocument()
    u = libsbml.Unit()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    ud = m.createUnitDefinition()
    ud.setId("ud")
    u.setMetaId("mmm")
    u.setKind(libsbml.UNIT_KIND_MOLE)
    ud.addUnit(u)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99904_unitdef(self):
    d = libsbml.SBMLDocument()
    u = libsbml.UnitDefinition()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    u.setId("ud")
    u.setMetaId("mmm")
    m.addUnitDefinition(u)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99904 )
    d = None
    pass  

  def test_internal_consistency_check_99905(self):
    d = libsbml.SBMLDocument()
    c = libsbml.Compartment()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setId("c")
    c.setSBOTerm(2)
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99905 )
    d = None
    pass  

  def test_internal_consistency_check_99905_ct(self):
    d = libsbml.SBMLDocument()
    ct = libsbml.CompartmentType()
    m = d.createModel()
    d.setLevelAndVersion(2,2)
    ct.setId("ct")
    ct.setSBOTerm(5)
    m.addCompartmentType(ct)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99905 )
    d = None
    pass  

  def test_internal_consistency_check_99905_delay(self):
    d = libsbml.SBMLDocument()
    delay = libsbml.Delay()
    e = libsbml.Event()
    m = d.createModel()
    d.setLevelAndVersion(2,2)
    delay.setSBOTerm(5)
    e.setDelay(delay)
    m.addEvent(e)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99905 )
    d = None
    pass  

  def test_internal_consistency_check_99905_species(self):
    d = libsbml.SBMLDocument()
    s = libsbml.Species()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setId("s")
    s.setCompartment("c")
    s.setSBOTerm(2)
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99905 )
    d = None
    pass  

  def test_internal_consistency_check_99905_st(self):
    d = libsbml.SBMLDocument()
    ct = libsbml.SpeciesType()
    m = d.createModel()
    d.setLevelAndVersion(2,2)
    ct.setId("st")
    ct.setSBOTerm(5)
    m.addSpeciesType(ct)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99905 )
    d = None
    pass  

  def test_internal_consistency_check_99905_stoichmath(self):
    d = libsbml.SBMLDocument()
    sm = libsbml.StoichiometryMath()
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
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99905 )
    d = None
    pass  

  def test_internal_consistency_check_99905_trigger(self):
    d = libsbml.SBMLDocument()
    trigger = libsbml.Trigger()
    e = libsbml.Event()
    m = d.createModel()
    d.setLevelAndVersion(2,2)
    trigger.setSBOTerm(5)
    e.setTrigger(trigger)
    m.addEvent(e)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99905 )
    d = None
    pass  

  def test_internal_consistency_check_99905_unit(self):
    d = libsbml.SBMLDocument()
    u = libsbml.Unit()
    d.setLevelAndVersion(2,2)
    m = d.createModel()
    ud = m.createUnitDefinition()
    ud.setId("ud")
    u.setKind(libsbml.UNIT_KIND_MOLE)
    u.setSBOTerm(9)
    ud.addUnit(u)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99905 )
    d = None
    pass  

  def test_internal_consistency_check_99905_unitdef(self):
    d = libsbml.SBMLDocument()
    u = libsbml.UnitDefinition()
    d.setLevelAndVersion(2,2)
    m = d.createModel()
    u.setId("ud")
    u.setSBOTerm(9)
    m.addUnitDefinition(u)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99905 )
    d = None
    pass  

  def test_internal_consistency_check_99906(self):
    d = libsbml.SBMLDocument()
    c = libsbml.Compartment()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setId("c")
    c.setUnits("mole")
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99906 )
    d = None
    pass  

  def test_internal_consistency_check_99907(self):
    d = libsbml.SBMLDocument()
    c = libsbml.Compartment()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c.setId("c")
    c.unsetVolume()
    m.addCompartment(c)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 0 )
    d = None
    pass  

  def test_internal_consistency_check_99908(self):
    d = libsbml.SBMLDocument()
    ct = libsbml.CompartmentType()
    m = d.createModel()
    d.setLevelAndVersion(2,1)
    ct.setId("ct")
    m.addCompartmentType(ct)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99908 )
    d = None
    pass  

  def test_internal_consistency_check_99909(self):
    d = libsbml.SBMLDocument()
    ct = libsbml.Constraint()
    m = d.createModel()
    d.setLevelAndVersion(2,1)
    m.addConstraint(ct)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99909 )
    d = None
    pass  

  def test_internal_consistency_check_99910(self):
    d = libsbml.SBMLDocument()
    e = libsbml.Event()
    m = d.createModel()
    d.setLevelAndVersion(1,2)
    c = m.createCompartment()
    c.setId("cc")
    c.setConstant(False)
    m.addEvent(e)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99910 )
    d = None
    pass  

  def test_internal_consistency_check_99911_ea(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    c.setConstant(False)
    e = m.createEvent()
    ea = libsbml.EventAssignment()
    d.setLevelAndVersion(2,1)
    ea.setVariable("c")
    ea.setSBOTerm(2)
    e.addEventAssignment(ea)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99911 )
    d = None
    pass  

  def test_internal_consistency_check_99911_event(self):
    d = libsbml.SBMLDocument()
    e = libsbml.Event()
    m = d.createModel()
    d.setLevelAndVersion(2,1)
    e.setSBOTerm(2)
    m.addEvent(e)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99911 )
    d = None
    pass  

  def test_internal_consistency_check_99911_fd(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    fd = libsbml.FunctionDefinition()
    d.setLevelAndVersion(2,1)
    fd.setId("fd")
    fd.setSBOTerm(2)
    m.addFunctionDefinition(fd)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99911 )
    d = None
    pass  

  def test_internal_consistency_check_99911_kl(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    r = m.createReaction()
    r.setId("r")
    kl = libsbml.KineticLaw()
    d.setLevelAndVersion(2,1)
    kl.setSBOTerm(2)
    p = kl.createParameter()
    p.setId("p")
    r.setKineticLaw(kl)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99911 )
    d = None
    pass  

  def test_internal_consistency_check_99911_model(self):
    d = libsbml.SBMLDocument()
    d.setLevelAndVersion(2,1)
    m = libsbml.Model()
    m.setSBOTerm(2)
    d.setModel(m)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99911 )
    d = None
    pass  

  def test_internal_consistency_check_99911_param(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    p = libsbml.Parameter()
    d.setLevelAndVersion(2,1)
    p.setId("p")
    p.setSBOTerm(2)
    m.addParameter(p)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99911 )
    d = None
    pass  

  def test_internal_consistency_check_99911_react(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    r = libsbml.Reaction()
    d.setLevelAndVersion(2,1)
    r.setId("r")
    r.setSBOTerm(2)
    m.addReaction(r)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99911 )
    d = None
    pass  

  def test_internal_consistency_check_99911_rule_alg(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    r = libsbml.AlgebraicRule()
    d.setLevelAndVersion(2,1)
    r.setSBOTerm(2)
    m.addRule(r)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99911 )
    d = None
    pass  

  def test_internal_consistency_check_99911_rule_assign(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    p = m.createParameter()
    p.setId("p")
    p.setConstant(False)
    r = libsbml.AssignmentRule()
    d.setLevelAndVersion(2,1)
    r.setVariable("p")
    r.setSBOTerm(2)
    m.addRule(r)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99911 )
    d = None
    pass  

  def test_internal_consistency_check_99911_rule_rate(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    p = m.createParameter()
    p.setId("p")
    p.setConstant(False)
    r = libsbml.RateRule()
    d.setLevelAndVersion(2,1)
    r.setVariable("p")
    r.setSBOTerm(2)
    m.addRule(r)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99911 )
    d = None
    pass  

  def test_internal_consistency_check_99911_speciesRef(self):
    d = libsbml.SBMLDocument()
    sr = libsbml.SpeciesReference()
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
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99911 )
    d = None
    pass  

  def test_internal_consistency_check_99912(self):
    d = libsbml.SBMLDocument()
    fd = libsbml.FunctionDefinition()
    m = d.createModel()
    d.setLevelAndVersion(1,2)
    c = m.createCompartment()
    c.setId("cc")
    c.setConstant(False)
    m.addFunctionDefinition(fd)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99912 )
    d = None
    pass  

  def test_internal_consistency_check_99913(self):
    d = libsbml.SBMLDocument()
    ia = libsbml.InitialAssignment()
    m = d.createModel()
    d.setLevelAndVersion(1,2)
    c = m.createCompartment()
    c.setId("cc")
    c.setConstant(False)
    m.addInitialAssignment(ia)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99913 )
    d = None
    pass  

  def test_internal_consistency_check_99914(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    r = libsbml.AlgebraicRule()
    d.setLevelAndVersion(2,1)
    r.setVariable("kk")
    m.addRule(r)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99914 )
    d = None
    pass  

  def test_internal_consistency_check_99915_alg(self):
    d = libsbml.SBMLDocument()
    m = d.createModel()
    r = libsbml.AlgebraicRule()
    d.setLevelAndVersion(2,1)
    r.setUnits("kk")
    m.addRule(r)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99915 )
    d = None
    pass  

  def test_internal_consistency_check_99915_assign(self):
    d = libsbml.SBMLDocument()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    c.setConstant(False)
    r = m.createAssignmentRule()
    r.setL1TypeCode(libsbml.SBML_SPECIES_CONCENTRATION_RULE)
    r.setVariable("c")
    r.setFormula("2")
    r.setUnits("mmm")
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99915 )
    d = None
    pass  

  def test_internal_consistency_check_99915_rate(self):
    d = libsbml.SBMLDocument()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    c.setConstant(False)
    r = m.createRateRule()
    r.setL1TypeCode(libsbml.SBML_SPECIES_CONCENTRATION_RULE)
    r.setFormula("2")
    r.setVariable("c")
    r.setUnits("mmm")
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99915 )
    d = None
    pass  

  def test_internal_consistency_check_99916_reaction(self):
    d = libsbml.SBMLDocument()
    s = libsbml.Species()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    r = m.createReaction()
    r.setId("r")
    sr = r.createReactant()
    s.setId("s")
    s.setCompartment("c")
    s.setConstant(True)
    sr.setSpecies("s")
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99916 )
    d = None
    pass  

  def test_internal_consistency_check_99916_rule(self):
    d = libsbml.SBMLDocument()
    s = libsbml.Species()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setId("s")
    s.setCompartment("c")
    s.setConstant(True)
    m.addSpecies(s)
    r = m.createAssignmentRule()
    r.setVariable("s")
    r.setFormula("2")
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99916 )
    d = None
    pass  

  def test_internal_consistency_check_99917(self):
    d = libsbml.SBMLDocument()
    s = libsbml.Species()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setId("s")
    s.setCompartment("c")
    s.setSpatialSizeUnits("kkk")
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99917 )
    d = None
    pass  

  def test_internal_consistency_check_99918(self):
    d = libsbml.SBMLDocument()
    s = libsbml.Species()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setId("s")
    s.setCompartment("c")
    s.setSpeciesType("kkk")
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99918 )
    d = None
    pass  

  def test_internal_consistency_check_99919(self):
    d = libsbml.SBMLDocument()
    s = libsbml.Species()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("c")
    s.setId("s")
    s.setCompartment("c")
    s.setHasOnlySubstanceUnits(True)
    m.addSpecies(s)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99919 )
    d = None
    pass  

  def test_internal_consistency_check_99920(self):
    d = libsbml.SBMLDocument()
    sr = libsbml.SpeciesReference()
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
    self.assert_( errors == 2 )
    self.assert_( d.getError(0).getErrorId() == 99920 )
    self.assert_( d.getError(1).getErrorId() == 99921 )
    d = None
    pass  

  def test_internal_consistency_check_99921(self):
    d = libsbml.SBMLDocument()
    sr = libsbml.SpeciesReference()
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
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99921 )
    d = None
    pass  

  def test_internal_consistency_check_99922(self):
    d = libsbml.SBMLDocument()
    ct = libsbml.SpeciesType()
    m = d.createModel()
    ct.setId("st")
    d.setLevelAndVersion(2,1)
    m.addSpeciesType(ct)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99922 )
    d = None
    pass  

  def test_internal_consistency_check_99923(self):
    d = libsbml.SBMLDocument()
    sm = libsbml.StoichiometryMath()
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
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99923 )
    d = None
    pass  

  def test_internal_consistency_check_99924(self):
    d = libsbml.SBMLDocument()
    u = libsbml.Unit()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    ud = m.createUnitDefinition()
    ud.setId("ud")
    u.setKind(libsbml.UNIT_KIND_MOLE)
    u.setMultiplier(9)
    ud.addUnit(u)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99924 )
    d = None
    pass  

  def test_internal_consistency_check_99925(self):
    d = libsbml.SBMLDocument()
    u = libsbml.Unit()
    d.setLevelAndVersion(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId("cc")
    ud = m.createUnitDefinition()
    ud.setId("ud")
    u.setKind(libsbml.UNIT_KIND_MOLE)
    u.setOffset(9)
    ud.addUnit(u)
    errors = d.checkInternalConsistency()
    self.assert_( errors == 1 )
    self.assert_( d.getError(0).getErrorId() == 99925 )
    d = None
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestInternalConsistencyChecks))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
