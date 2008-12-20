/*
 *
 * @file    TestInternalConsistencyChecks.java
 * @brief   Tests the internal consistency validation.
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Sarah Keating 
 *
 * $Id:$
 * $HeadURL:$
 *
 * This test file was converted from src/sbml/test/TestInternalConsistencyChecks.cpp
 * with the help of conversion sciprt (ctest_converter.pl).
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2008 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *--------------------------------------------------------------------------->*/


package org.sbml.libsbml.test.sbml;

import org.sbml.libsbml.*;

import java.io.File;
import java.lang.AssertionError;

public class TestInternalConsistencyChecks {

  static void assertTrue(boolean condition) throws AssertionError
  {
    if (condition == true)
    {
      return;
    }
    throw new AssertionError();
  }

  static void assertEquals(Object a, Object b) throws AssertionError
  {
    if ( (a == null) && (b == null) )
    {
      return;
    }
    else if (a.equals(b))
    {
      return;
    }

    throw new AssertionError();
  }

  static void assertNotEquals(Object a, Object b) throws AssertionError
  {
    if ( (a == null) && (b == null) )
    {
      throw new AssertionError();
    }
    else if (a.equals(b))
    {
      throw new AssertionError();
    }
  }

  static void assertEquals(boolean a, boolean b) throws AssertionError
  {
    if ( a == b )
    {
      return;
    }
    throw new AssertionError();
  }

  static void assertNotEquals(boolean a, boolean b) throws AssertionError
  {
    if ( a != b )
    {
      return;
    }
    throw new AssertionError();
  }

  static void assertEquals(int a, int b) throws AssertionError
  {
    if ( a == b )
    {
      return;
    }
    throw new AssertionError();
  }

  static void assertNotEquals(int a, int b) throws AssertionError
  {
    if ( a != b )
    {
      return;
    }
    throw new AssertionError();
  }

  public void test_internal_consistency_check_99901()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Compartment c = new Compartment();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    c.setSpatialDimensions(2);
    c.setId("c");
    m.addCompartment(c);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99901 );
    d = null;
  }

  public void test_internal_consistency_check_99902()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Compartment c = new Compartment();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    c.setCompartmentType("hh");
    c.setId("c");
    m.addCompartment(c);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99902 );
    d = null;
  }

  public void test_internal_consistency_check_99903()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Compartment c = new Compartment();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    c.setConstant(true);
    c.setId("c");
    m.addCompartment(c);
    Rule r = m.createAssignmentRule();
    r.setVariable("c");
    r.setFormula("2*3");
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99903 );
    d = null;
  }

  public void test_internal_consistency_check_99903_localparam()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Parameter p = new Parameter();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("cc");
    Reaction r = m.createReaction();
    r.setId("r");
    KineticLaw kl = r.createKineticLaw();
    kl.setFormula("2");
    p.setId("p");
    p.setConstant(false);
    kl.addParameter(p);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99903 );
    d = null;
  }

  public void test_internal_consistency_check_99903_param()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Parameter p = new Parameter();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("cc");
    p.setConstant(true);
    p.setId("c");
    m.addParameter(p);
    Rule r = m.createAssignmentRule();
    r.setVariable("c");
    r.setFormula("2*3");
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99903 );
    d = null;
  }

  public void test_internal_consistency_check_99904()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Compartment c = new Compartment();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    c.setId("c");
    c.setMetaId("mmm");
    m.addCompartment(c);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99904_kl()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    KineticLaw kl = new KineticLaw();
    Model m = d.createModel();
    d.setLevelAndVersion(1,2);
    Compartment c = m.createCompartment();
    c.setId("cc");
    Reaction r = m.createReaction();
    r.setId("r");
    kl.setFormula("2");
    kl.setMetaId("mmm");
    r.setKineticLaw(kl);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99904_model()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    d.setLevelAndVersion(1,2);
    Model m = new Model();
    Compartment c = m.createCompartment();
    c.setId("cc");
    m.setMetaId("mmm");
    d.setModel(m);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99904_param()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Parameter p = new Parameter();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("cc");
    p.setId("p");
    p.setMetaId("mmm");
    m.addParameter(p);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99904_react()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Reaction r = new Reaction();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("cc");
    r.setId("r");
    r.setMetaId("mmm");
    m.addReaction(r);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99904_rule_alg()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Rule r = new AlgebraicRule();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("cc");
    r.setMetaId("mmm");
    r.setFormula("2");
    m.addRule(r);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99904_rule_assign()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Rule r = new AssignmentRule();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("cc");
    c.setConstant(false);
    r.setVariable("cc");
    r.setFormula("2");
    r.setMetaId("mmm");
    m.addRule(r);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99904_rule_rate()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Rule r = new RateRule();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("cc");
    c.setConstant(false);
    r.setVariable("cc");
    r.setFormula("2");
    r.setMetaId("mmm");
    m.addRule(r);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99904_species()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Species s = new Species();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    s.setCompartment("c");
    s.setId("s");
    s.setMetaId("mmm");
    m.addSpecies(s);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99904_speciesRef()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    SpeciesReference sr = new SpeciesReference();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    Species s = m.createSpecies();
    s.setId("s");
    Reaction r = m.createReaction();
    r.setId("r");
    s.setCompartment("c");
    sr.setSpecies("s");
    sr.setMetaId("mmm");
    r.addProduct(sr);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99904_unit()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Unit u = new Unit();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("cc");
    UnitDefinition ud = m.createUnitDefinition();
    ud.setId("ud");
    u.setMetaId("mmm");
    u.setKind(libsbml.UNIT_KIND_MOLE);
    ud.addUnit(u);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99904_unitdef()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    UnitDefinition u = new UnitDefinition();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("cc");
    u.setId("ud");
    u.setMetaId("mmm");
    m.addUnitDefinition(u);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99904 );
    d = null;
  }

  public void test_internal_consistency_check_99905()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Compartment c = new Compartment();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    c.setId("c");
    c.setSBOTerm(2);
    m.addCompartment(c);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99905 );
    d = null;
  }

  public void test_internal_consistency_check_99905_ct()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    CompartmentType ct = new CompartmentType();
    Model m = d.createModel();
    d.setLevelAndVersion(2,2);
    ct.setId("ct");
    ct.setSBOTerm(5);
    m.addCompartmentType(ct);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99905 );
    d = null;
  }

  public void test_internal_consistency_check_99905_delay()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Delay delay = new Delay();
    Event e = new Event();
    Model m = d.createModel();
    d.setLevelAndVersion(2,2);
    delay.setSBOTerm(5);
    e.setDelay(delay);
    m.addEvent(e);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99905 );
    d = null;
  }

  public void test_internal_consistency_check_99905_species()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Species s = new Species();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    s.setId("s");
    s.setCompartment("c");
    s.setSBOTerm(2);
    m.addSpecies(s);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99905 );
    d = null;
  }

  public void test_internal_consistency_check_99905_st()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    SpeciesType ct = new SpeciesType();
    Model m = d.createModel();
    d.setLevelAndVersion(2,2);
    ct.setId("st");
    ct.setSBOTerm(5);
    m.addSpeciesType(ct);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99905 );
    d = null;
  }

  public void test_internal_consistency_check_99905_stoichmath()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    StoichiometryMath sm = new StoichiometryMath();
    Model m = d.createModel();
    d.setLevelAndVersion(2,2);
    Species s = m.createSpecies();
    s.setId("s");
    Compartment c = m.createCompartment();
    c.setId("c");
    s.setCompartment("c");
    Reaction r = m.createReaction();
    r.setId("r");
    SpeciesReference sr = r.createProduct();
    sr.setSpecies("s");
    sm.setSBOTerm(5);
    sr.setStoichiometryMath(sm);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99905 );
    d = null;
  }

  public void test_internal_consistency_check_99905_trigger()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Trigger trigger = new Trigger();
    Event e = new Event();
    Model m = d.createModel();
    d.setLevelAndVersion(2,2);
    trigger.setSBOTerm(5);
    e.setTrigger(trigger);
    m.addEvent(e);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99905 );
    d = null;
  }

  public void test_internal_consistency_check_99905_unit()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Unit u = new Unit();
    d.setLevelAndVersion(2,2);
    Model m = d.createModel();
    UnitDefinition ud = m.createUnitDefinition();
    ud.setId("ud");
    u.setKind(libsbml.UNIT_KIND_MOLE);
    u.setSBOTerm(9);
    ud.addUnit(u);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99905 );
    d = null;
  }

  public void test_internal_consistency_check_99905_unitdef()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    UnitDefinition u = new UnitDefinition();
    d.setLevelAndVersion(2,2);
    Model m = d.createModel();
    u.setId("ud");
    u.setSBOTerm(9);
    m.addUnitDefinition(u);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99905 );
    d = null;
  }

  public void test_internal_consistency_check_99906()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Compartment c = new Compartment();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    c.setId("c");
    c.setUnits("mole");
    m.addCompartment(c);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99906 );
    d = null;
  }

  public void test_internal_consistency_check_99907()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Compartment c = new Compartment();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    c.setId("c");
    c.unsetVolume();
    m.addCompartment(c);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 0 );
    d = null;
  }

  public void test_internal_consistency_check_99908()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    CompartmentType ct = new CompartmentType();
    Model m = d.createModel();
    d.setLevelAndVersion(2,1);
    ct.setId("ct");
    m.addCompartmentType(ct);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99908 );
    d = null;
  }

  public void test_internal_consistency_check_99909()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Constraint ct = new Constraint();
    Model m = d.createModel();
    d.setLevelAndVersion(2,1);
    m.addConstraint(ct);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99909 );
    d = null;
  }

  public void test_internal_consistency_check_99910()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Event e = new Event();
    Model m = d.createModel();
    d.setLevelAndVersion(1,2);
    Compartment c = m.createCompartment();
    c.setId("cc");
    c.setConstant(false);
    m.addEvent(e);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99910 );
    d = null;
  }

  public void test_internal_consistency_check_99911_ea()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    c.setConstant(false);
    Event e = m.createEvent();
    EventAssignment ea = new EventAssignment();
    d.setLevelAndVersion(2,1);
    ea.setVariable("c");
    ea.setSBOTerm(2);
    e.addEventAssignment(ea);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99911 );
    d = null;
  }

  public void test_internal_consistency_check_99911_event()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Event e = new Event();
    Model m = d.createModel();
    d.setLevelAndVersion(2,1);
    e.setSBOTerm(2);
    m.addEvent(e);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99911 );
    d = null;
  }

  public void test_internal_consistency_check_99911_fd()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Model m = d.createModel();
    FunctionDefinition fd = new FunctionDefinition();
    d.setLevelAndVersion(2,1);
    fd.setId("fd");
    fd.setSBOTerm(2);
    m.addFunctionDefinition(fd);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99911 );
    d = null;
  }

  public void test_internal_consistency_check_99911_kl()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Model m = d.createModel();
    Reaction r = m.createReaction();
    r.setId("r");
    KineticLaw kl = new KineticLaw();
    d.setLevelAndVersion(2,1);
    kl.setSBOTerm(2);
    Parameter p = kl.createParameter();
    p.setId("p");
    r.setKineticLaw(kl);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99911 );
    d = null;
  }

  public void test_internal_consistency_check_99911_model()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    d.setLevelAndVersion(2,1);
    Model m = new Model();
    m.setSBOTerm(2);
    d.setModel(m);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99911 );
    d = null;
  }

  public void test_internal_consistency_check_99911_param()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Model m = d.createModel();
    Parameter p = new Parameter();
    d.setLevelAndVersion(2,1);
    p.setId("p");
    p.setSBOTerm(2);
    m.addParameter(p);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99911 );
    d = null;
  }

  public void test_internal_consistency_check_99911_react()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Model m = d.createModel();
    Reaction r = new Reaction();
    d.setLevelAndVersion(2,1);
    r.setId("r");
    r.setSBOTerm(2);
    m.addReaction(r);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99911 );
    d = null;
  }

  public void test_internal_consistency_check_99911_rule_alg()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Model m = d.createModel();
    Rule r = new AlgebraicRule();
    d.setLevelAndVersion(2,1);
    r.setSBOTerm(2);
    m.addRule(r);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99911 );
    d = null;
  }

  public void test_internal_consistency_check_99911_rule_assign()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Model m = d.createModel();
    Parameter p = m.createParameter();
    p.setId("p");
    p.setConstant(false);
    Rule r = new AssignmentRule();
    d.setLevelAndVersion(2,1);
    r.setVariable("p");
    r.setSBOTerm(2);
    m.addRule(r);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99911 );
    d = null;
  }

  public void test_internal_consistency_check_99911_rule_rate()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Model m = d.createModel();
    Parameter p = m.createParameter();
    p.setId("p");
    p.setConstant(false);
    Rule r = new RateRule();
    d.setLevelAndVersion(2,1);
    r.setVariable("p");
    r.setSBOTerm(2);
    m.addRule(r);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99911 );
    d = null;
  }

  public void test_internal_consistency_check_99911_speciesRef()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    SpeciesReference sr = new SpeciesReference();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    Species s = m.createSpecies();
    s.setId("s");
    Reaction r = m.createReaction();
    r.setId("r");
    s.setCompartment("c");
    sr.setSpecies("s");
    sr.setSBOTerm(4);
    r.addReactant(sr);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99911 );
    d = null;
  }

  public void test_internal_consistency_check_99912()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    FunctionDefinition fd = new FunctionDefinition();
    Model m = d.createModel();
    d.setLevelAndVersion(1,2);
    Compartment c = m.createCompartment();
    c.setId("cc");
    c.setConstant(false);
    m.addFunctionDefinition(fd);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99912 );
    d = null;
  }

  public void test_internal_consistency_check_99913()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    InitialAssignment ia = new InitialAssignment();
    Model m = d.createModel();
    d.setLevelAndVersion(1,2);
    Compartment c = m.createCompartment();
    c.setId("cc");
    c.setConstant(false);
    m.addInitialAssignment(ia);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99913 );
    d = null;
  }

  public void test_internal_consistency_check_99914()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Model m = d.createModel();
    Rule r = new AlgebraicRule();
    d.setLevelAndVersion(2,1);
    r.setVariable("kk");
    m.addRule(r);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99914 );
    d = null;
  }

  public void test_internal_consistency_check_99915_alg()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Model m = d.createModel();
    Rule r = new AlgebraicRule();
    d.setLevelAndVersion(2,1);
    r.setUnits("kk");
    m.addRule(r);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99915 );
    d = null;
  }

  public void test_internal_consistency_check_99915_assign()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    c.setConstant(false);
    AssignmentRule r = m.createAssignmentRule();
    r.setL1TypeCode(libsbml.SBML_SPECIES_CONCENTRATION_RULE);
    r.setVariable("c");
    r.setFormula("2");
    r.setUnits("mmm");
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99915 );
    d = null;
  }

  public void test_internal_consistency_check_99915_rate()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    c.setConstant(false);
    RateRule r = m.createRateRule();
    r.setL1TypeCode(libsbml.SBML_SPECIES_CONCENTRATION_RULE);
    r.setFormula("2");
    r.setVariable("c");
    r.setUnits("mmm");
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99915 );
    d = null;
  }

  public void test_internal_consistency_check_99916_reaction()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Species s = new Species();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    Reaction r = m.createReaction();
    r.setId("r");
    SpeciesReference sr = r.createReactant();
    s.setId("s");
    s.setCompartment("c");
    s.setConstant(true);
    sr.setSpecies("s");
    m.addSpecies(s);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99916 );
    d = null;
  }

  public void test_internal_consistency_check_99916_rule()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Species s = new Species();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    s.setId("s");
    s.setCompartment("c");
    s.setConstant(true);
    m.addSpecies(s);
    Rule r = m.createAssignmentRule();
    r.setVariable("s");
    r.setFormula("2");
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99916 );
    d = null;
  }

  public void test_internal_consistency_check_99917()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Species s = new Species();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    s.setId("s");
    s.setCompartment("c");
    s.setSpatialSizeUnits("kkk");
    m.addSpecies(s);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99917 );
    d = null;
  }

  public void test_internal_consistency_check_99918()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Species s = new Species();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    s.setId("s");
    s.setCompartment("c");
    s.setSpeciesType("kkk");
    m.addSpecies(s);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99918 );
    d = null;
  }

  public void test_internal_consistency_check_99919()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Species s = new Species();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    s.setId("s");
    s.setCompartment("c");
    s.setHasOnlySubstanceUnits(true);
    m.addSpecies(s);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99919 );
    d = null;
  }

  public void test_internal_consistency_check_99920()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    SpeciesReference sr = new SpeciesReference();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    Species s = m.createSpecies();
    s.setId("s");
    Reaction r = m.createReaction();
    r.setId("r");
    s.setCompartment("c");
    sr.setSpecies("s");
    sr.setId("mmm");
    r.addProduct(sr);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 2 );
    assertTrue( d.getError(0).getErrorId() == 99920 );
    assertTrue( d.getError(1).getErrorId() == 99921 );
    d = null;
  }

  public void test_internal_consistency_check_99921()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    SpeciesReference sr = new SpeciesReference();
    d.setLevelAndVersion(2,1);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("c");
    Species s = m.createSpecies();
    s.setId("s");
    Reaction r = m.createReaction();
    r.setId("r");
    s.setCompartment("c");
    sr.setSpecies("s");
    sr.setName("mmm");
    r.addReactant(sr);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99921 );
    d = null;
  }

  public void test_internal_consistency_check_99922()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    SpeciesType ct = new SpeciesType();
    Model m = d.createModel();
    ct.setId("st");
    d.setLevelAndVersion(2,1);
    m.addSpeciesType(ct);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99922 );
    d = null;
  }

  public void test_internal_consistency_check_99923()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    StoichiometryMath sm = new StoichiometryMath();
    Model m = d.createModel();
    d.setLevelAndVersion(1,2);
    Species s = m.createSpecies();
    s.setId("s");
    Compartment c = m.createCompartment();
    c.setId("c");
    s.setCompartment("c");
    Reaction r = m.createReaction();
    r.setId("r");
    SpeciesReference sr = r.createProduct();
    sr.setSpecies("s");
    sr.setStoichiometryMath(sm);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99923 );
    d = null;
  }

  public void test_internal_consistency_check_99924()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Unit u = new Unit();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("cc");
    UnitDefinition ud = m.createUnitDefinition();
    ud.setId("ud");
    u.setKind(libsbml.UNIT_KIND_MOLE);
    u.setMultiplier(9);
    ud.addUnit(u);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99924 );
    d = null;
  }

  public void test_internal_consistency_check_99925()
  {
    SBMLDocument d = new SBMLDocument();
    long errors;
    Unit u = new Unit();
    d.setLevelAndVersion(1,2);
    Model m = d.createModel();
    Compartment c = m.createCompartment();
    c.setId("cc");
    UnitDefinition ud = m.createUnitDefinition();
    ud.setId("ud");
    u.setKind(libsbml.UNIT_KIND_MOLE);
    u.setOffset(9);
    ud.addUnit(u);
    errors = d.checkInternalConsistency();
    assertTrue( errors == 1 );
    assertTrue( d.getError(0).getErrorId() == 99925 );
    d = null;
  }

  /**
   * Loads the SWIG-generated libSBML Java module when this class is
   * loaded, or reports a sensible diagnostic message about why it failed.
   */
  static
  {
    String varname;
    String shlibname;

    if (System.getProperty("mrj.version") != null)
    {
      varname = "DYLD_LIBRARY_PATH";    // We're on a Mac.
      shlibname = "libsbmlj.jnilib and/or libsbml.dylib";
    }
    else
    {
      varname = "LD_LIBRARY_PATH";      // We're not on a Mac.
      shlibname = "libsbmlj.so and/or libsbml.so";
    }

    try
    {
      System.loadLibrary("sbmlj");
      // For extra safety, check that the jar file is in the classpath.
      Class.forName("org.sbml.libsbml.libsbml");
    }
    catch (SecurityException e)
    {
      e.printStackTrace();
      System.err.println("Could not load the libSBML library files due to a"+
                         " security exception.\n");
      System.exit(1);
    }
    catch (UnsatisfiedLinkError e)
    {
      e.printStackTrace();
      System.err.println("Error: could not link with the libSBML library files."+
                         " It is likely\nyour " + varname +
                         " environment variable does not include the directories\n"+
                         "containing the " + shlibname + " library files.\n");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      e.printStackTrace();
      System.err.println("Error: unable to load the file libsbmlj.jar."+
                         " It is likely\nyour -classpath option and CLASSPATH" +
                         " environment variable\n"+
                         "do not include the path to libsbmlj.jar.\n");
      System.exit(1);
    }
  }
}
