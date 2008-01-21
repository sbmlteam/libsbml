/*
 *
 * @file    TestModel.java
 * @brief   SBML Model unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestModel.c
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


package org.sbml.libsbml.test;

import org.sbml.libsbml.*;

import java.io.File;
import java.lang.AssertionError;

public class TestModel {

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

  private Model M;

  protected void setUp() throws Exception
  {
    M = new  Model();
    if (M == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    M = null;
  }

  public void test_KineticLaw_getParameterById()
  {
    Parameter k1 = new  Parameter();
    Parameter k2 = new  Parameter();
    k1.setId( "k1");
    k2.setId( "k2");
    k1.setValue(3.14);
    k2.setValue(2.72);
    M.addParameter(k1);
    M.addParameter(k2);
    Reaction r1 = new  Reaction();
    r1.setId( "reaction_1" );
    KineticLaw kl = new  KineticLaw("k1 * X0");
    Parameter k3 = new  Parameter();
    Parameter k4 = new  Parameter();
    k3.setId( "k1");
    k4.setId( "k2");
    k3.setValue(2.72);
    k4.setValue(3.14);
    kl.addParameter(k3);
    kl.addParameter(k4);
    r1.setKineticLaw(kl);
    M.addReaction(r1);
    KineticLaw kl1 = M.getReaction(0).getKineticLaw();
    assertNotEquals(kl1.getParameter( "k1" ),k3);
    assertNotEquals(kl1.getParameter( "k1" ),k1);
    assertNotEquals(kl1.getParameter( "k2" ),k4);
    assertEquals(kl1.getParameter( "k3" ),null);
  }

  public void test_Model_addCompartment()
  {
    M.addCompartment(new  Compartment());
    assertTrue( M.getNumCompartments() == 1 );
  }

  public void test_Model_addParameter()
  {
    M.addParameter(new  Parameter());
    assertTrue( M.getNumParameters() == 1 );
  }

  public void test_Model_addReaction()
  {
    M.addReaction(new  Reaction());
    assertTrue( M.getNumReactions() == 1 );
  }

  public void test_Model_addRules()
  {
    M.addRule(new AlgebraicRule());
    M.addRule(new AssignmentRule());
    M.addRule(new RateRule());
    assertTrue( M.getNumRules() == 3 );
  }

  public void test_Model_addSpecies()
  {
    M.addSpecies(new  Species());
    assertTrue( M.getNumSpecies() == 1 );
  }

  public void test_Model_add_get_Event()
  {
    Event e1 = new  Event();
    Event e2 = new  Event();
    M.addEvent(e1);
    M.addEvent(e2);
    assertTrue( M.getNumEvents() == 2 );
    assertNotEquals(M.getEvent(0),e1);
    assertNotEquals(M.getEvent(1),e2);
    assertEquals(M.getEvent(2),null);
    assertEquals(M.getEvent(-2),null);
  }

  public void test_Model_add_get_FunctionDefinitions()
  {
    FunctionDefinition fd1 = new  FunctionDefinition();
    FunctionDefinition fd2 = new  FunctionDefinition();
    M.addFunctionDefinition(fd1);
    M.addFunctionDefinition(fd2);
    assertTrue( M.getNumFunctionDefinitions() == 2 );
    assertNotEquals(M.getFunctionDefinition(0),fd1);
    assertNotEquals(M.getFunctionDefinition(1),fd2);
    assertEquals(M.getFunctionDefinition(2),null);
    assertEquals(M.getFunctionDefinition(-2),null);
  }

  public void test_Model_add_get_UnitDefinitions()
  {
    UnitDefinition ud1 = new  UnitDefinition();
    UnitDefinition ud2 = new  UnitDefinition();
    M.addUnitDefinition(ud1);
    M.addUnitDefinition(ud2);
    assertTrue( M.getNumUnitDefinitions() == 2 );
    assertNotEquals(M.getUnitDefinition(0),ud1);
    assertNotEquals(M.getUnitDefinition(1),ud2);
    assertEquals(M.getUnitDefinition(2),null);
    assertEquals(M.getUnitDefinition(-2),null);
  }

  public void test_Model_create()
  {
    assertTrue( M.getTypeCode() == libsbml.SBML_MODEL );
    assertTrue( M.getMetaId().equals("") == true );
    assertTrue( M.getNotes() == null );
    assertTrue( M.getAnnotation() == null );
    assertTrue( M.getId().equals("") == true );
    assertTrue( M.getName().equals("") == true );
    assertEquals( false, M.isSetId() );
    assertEquals( false, M.isSetName() );
    assertTrue( M.getNumUnitDefinitions() == 0 );
    assertTrue( M.getNumCompartments() == 0 );
    assertTrue( M.getNumSpecies() == 0 );
    assertTrue( M.getNumParameters() == 0 );
    assertTrue( M.getNumReactions() == 0 );
  }

  public void test_Model_createAlgebraicRule()
  {
    Rule ar = M.createAlgebraicRule();
    assertTrue( ar != null );
    assertTrue( M.getNumRules() == 1 );
    assertEquals(M.getRule(0),ar);
  }

  public void test_Model_createAssignmentRule()
  {
    Rule ar = M.createAssignmentRule();
    assertTrue( ar != null );
    assertTrue( M.getNumRules() == 1 );
    assertEquals(M.getRule(0),ar);
  }

  public void test_Model_createCompartment()
  {
    Compartment c = M.createCompartment();
    assertTrue( c != null );
    assertTrue( M.getNumCompartments() == 1 );
    assertEquals(M.getCompartment(0),c);
  }

  public void test_Model_createCompartmentType()
  {
    CompartmentType c = M.createCompartmentType();
    assertTrue( c != null );
    assertTrue( M.getNumCompartmentTypes() == 1 );
    assertEquals(M.getCompartmentType(0),c);
  }

  public void test_Model_createConstraint()
  {
    Constraint c = M.createConstraint();
    assertTrue( c != null );
    assertTrue( M.getNumConstraints() == 1 );
    assertEquals(M.getConstraint(0),c);
  }

  public void test_Model_createEvent()
  {
    Event e = M.createEvent();
    assertTrue( e != null );
    assertTrue( M.getNumEvents() == 1 );
    assertEquals(M.getEvent(0),e);
  }

  public void test_Model_createEventAssignment()
  {
    Event e;
    EventAssignment ea;
    M.createEvent();
    M.createEvent();
    ea = M.createEventAssignment();
    assertTrue( ea != null );
    assertTrue( M.getNumEvents() == 2 );
    e = M.getEvent(1);
    assertTrue( e.getNumEventAssignments() == 1 );
    assertEquals(e.getEventAssignment(0),ea);
  }

  public void test_Model_createEventAssignment_noEvent()
  {
    assertTrue( M.getNumEvents() == 0 );
    assertTrue( M.createEventAssignment() == null );
  }

  public void test_Model_createFunctionDefinition()
  {
    FunctionDefinition fd = M.createFunctionDefinition();
    assertTrue( fd != null );
    assertTrue( M.getNumFunctionDefinitions() == 1 );
    assertEquals(M.getFunctionDefinition(0),fd);
  }

  public void test_Model_createInitialAssignment()
  {
    InitialAssignment c = M.createInitialAssignment();
    assertTrue( c != null );
    assertTrue( M.getNumInitialAssignments() == 1 );
    assertEquals(M.getInitialAssignment(0),c);
  }

  public void test_Model_createKineticLaw()
  {
    Reaction r;
    KineticLaw kl;
    M.createReaction();
    M.createReaction();
    kl = M.createKineticLaw();
    assertTrue( kl != null );
    assertTrue( M.getNumReactions() == 2 );
    r = M.getReaction(0);
    assertEquals(r.getKineticLaw(),null);
    r = M.getReaction(1);
    assertEquals(r.getKineticLaw(),kl);
  }

  public void test_Model_createKineticLawParameter()
  {
    Reaction r;
    KineticLaw kl;
    Parameter p;
    M.createReaction();
    M.createReaction();
    M.createKineticLaw();
    p = M.createKineticLawParameter();
    assertTrue( M.getNumReactions() == 2 );
    r = M.getReaction(0);
    assertEquals(r.getKineticLaw(),null);
    r = M.getReaction(1);
    assertNotEquals(r.getKineticLaw(),null);
    kl = r.getKineticLaw();
    assertTrue( kl.getNumParameters() == 1 );
    assertEquals(kl.getParameter(0),p);
  }

  public void test_Model_createKineticLawParameter_noKineticLaw()
  {
    Reaction r;
    r = M.createReaction();
    assertEquals(r.getKineticLaw(),null);
    assertTrue( M.createKineticLawParameter() == null );
  }

  public void test_Model_createKineticLawParameter_noReaction()
  {
    assertTrue( M.getNumReactions() == 0 );
    assertTrue( M.createKineticLawParameter() == null );
  }

  public void test_Model_createKineticLaw_alreadyExists()
  {
    Reaction r;
    KineticLaw kl;
    r = M.createReaction();
    kl = M.createKineticLaw();
    assertEquals(r.getKineticLaw(),kl);
  }

  public void test_Model_createKineticLaw_noReaction()
  {
    assertTrue( M.getNumReactions() == 0 );
    assertTrue( M.createKineticLaw() == null );
  }

  public void test_Model_createModifier()
  {
    Reaction r;
    ModifierSpeciesReference msr;
    M.createReaction();
    M.createReaction();
    msr = M.createModifier();
    assertTrue( msr != null );
    assertTrue( M.getNumReactions() == 2 );
    r = M.getReaction(1);
    assertTrue( r.getNumModifiers() == 1 );
    assertEquals(r.getModifier(0),msr);
  }

  public void test_Model_createModifier_noReaction()
  {
    assertTrue( M.getNumReactions() == 0 );
    assertTrue( M.createModifier() == null );
  }

  public void test_Model_createParameter()
  {
    Parameter p = M.createParameter();
    assertTrue( p != null );
    assertTrue( M.getNumParameters() == 1 );
    assertEquals(M.getParameter(0),p);
  }

  public void test_Model_createProduct()
  {
    Reaction r;
    SpeciesReference sr;
    M.createReaction();
    M.createReaction();
    sr = M.createProduct();
    assertTrue( sr != null );
    assertTrue( M.getNumReactions() == 2 );
    r = M.getReaction(1);
    assertTrue( r.getNumProducts() == 1 );
    assertEquals(r.getProduct(0),sr);
  }

  public void test_Model_createProduct_noReaction()
  {
    assertTrue( M.getNumReactions() == 0 );
    assertTrue( M.createProduct() == null );
  }

  public void test_Model_createRateRule()
  {
    Rule rr = M.createRateRule();
    assertTrue( rr != null );
    assertTrue( M.getNumRules() == 1 );
    assertEquals(M.getRule(0),rr);
  }

  public void test_Model_createReactant()
  {
    Reaction r;
    SpeciesReference sr;
    M.createReaction();
    M.createReaction();
    sr = M.createReactant();
    assertTrue( sr != null );
    assertTrue( M.getNumReactions() == 2 );
    r = M.getReaction(1);
    assertTrue( r.getNumReactants() == 1 );
    assertEquals(r.getReactant(0),sr);
  }

  public void test_Model_createReactant_noReaction()
  {
    assertTrue( M.getNumReactions() == 0 );
    assertTrue( M.createReactant() == null );
  }

  public void test_Model_createReaction()
  {
    Reaction r = M.createReaction();
    assertTrue( r != null );
    assertTrue( M.getNumReactions() == 1 );
    assertEquals(M.getReaction(0),r);
  }

  public void test_Model_createSpecies()
  {
    Species s = M.createSpecies();
    assertTrue( s != null );
    assertTrue( M.getNumSpecies() == 1 );
    assertEquals(M.getSpecies(0),s);
  }

  public void test_Model_createSpeciesType()
  {
    SpeciesType c = M.createSpeciesType();
    assertTrue( c != null );
    assertTrue( M.getNumSpeciesTypes() == 1 );
    assertEquals(M.getSpeciesType(0),c);
  }

  public void test_Model_createUnit()
  {
    UnitDefinition ud;
    Unit u;
    M.createUnitDefinition();
    M.createUnitDefinition();
    u = M.createUnit();
    assertTrue( u != null );
    assertTrue( M.getNumUnitDefinitions() == 2 );
    ud = M.getUnitDefinition(1);
    assertTrue( ud.getNumUnits() == 1 );
    assertEquals(ud.getUnit(0),u);
  }

  public void test_Model_createUnitDefinition()
  {
    UnitDefinition ud = M.createUnitDefinition();
    assertTrue( ud != null );
    assertTrue( M.getNumUnitDefinitions() == 1 );
    assertEquals(M.getUnitDefinition(0),ud);
  }

  public void test_Model_createUnit_noUnitDefinition()
  {
    assertTrue( M.getNumUnitDefinitions() == 0 );
    assertTrue( M.createUnit() == null );
  }

  public void test_Model_createWith()
  {
    Model m = new  Model("repressilator", "");
    assertTrue( m.getTypeCode() == libsbml.SBML_MODEL );
    assertTrue( m.getMetaId().equals("") == true );
    assertTrue( m.getNotes() == null );
    assertTrue( m.getAnnotation() == null );
    assertTrue( m.getName().equals("") == true );
    assertTrue(m.getId().equals( "repressilator"));
    assertEquals( true, m.isSetId() );
    assertTrue( m.getNumUnitDefinitions() == 0 );
    assertTrue( m.getNumFunctionDefinitions() == 0 );
    assertTrue( m.getNumCompartments() == 0 );
    assertTrue( m.getNumSpecies() == 0 );
    assertTrue( m.getNumParameters() == 0 );
    assertTrue( m.getNumReactions() == 0 );
    assertTrue( m.getNumRules() == 0 );
    assertTrue( m.getNumConstraints() == 0 );
    assertTrue( m.getNumEvents() == 0 );
    assertTrue( m.getNumCompartmentTypes() == 0 );
    assertTrue( m.getNumSpeciesTypes() == 0 );
    assertTrue( m.getNumInitialAssignments() == 0 );
    m = null;
  }

  public void test_Model_free_NULL()
  {
  }

  public void test_Model_getCompartment()
  {
    Compartment c1 = new  Compartment();
    Compartment c2 = new  Compartment();
    c1.setName( "A");
    c2.setName( "B");
    M.addCompartment(c1);
    M.addCompartment(c2);
    assertTrue( M.getNumCompartments() == 2 );
    c1 = M.getCompartment(0);
    c2 = M.getCompartment(1);
    assertTrue(c1.getName().equals( "A"));
    assertTrue(c2.getName().equals( "B"));
  }

  public void test_Model_getCompartmentById()
  {
    Compartment c1 = new  Compartment();
    Compartment c2 = new  Compartment();
    c1.setId( "A" );
    c2.setId( "B" );
    M.addCompartment(c1);
    M.addCompartment(c2);
    assertTrue( M.getNumCompartments() == 2 );
    assertNotEquals(M.getCompartment( "A" ),c1);
    assertNotEquals(M.getCompartment( "B" ),c2);
    assertEquals(M.getCompartment( "C" ),null);
  }

  public void test_Model_getEventById()
  {
    Event e1 = new  Event();
    Event e2 = new  Event();
    e1.setId( "e1" );
    e2.setId( "e2" );
    M.addEvent(e1);
    M.addEvent(e2);
    assertTrue( M.getNumEvents() == 2 );
    assertNotEquals(M.getEvent( "e1" ),e1);
    assertNotEquals(M.getEvent( "e2" ),e2);
    assertEquals(M.getEvent( "e3" ),null);
  }

  public void test_Model_getFunctionDefinitionById()
  {
    FunctionDefinition fd1 = new  FunctionDefinition();
    FunctionDefinition fd2 = new  FunctionDefinition();
    fd1.setId( "sin" );
    fd2.setId( "cos" );
    M.addFunctionDefinition(fd1);
    M.addFunctionDefinition(fd2);
    assertTrue( M.getNumFunctionDefinitions() == 2 );
    assertNotEquals(M.getFunctionDefinition( "sin" ),fd1);
    assertNotEquals(M.getFunctionDefinition( "cos" ),fd2);
    assertEquals(M.getFunctionDefinition( "tan" ),null);
  }

  public void test_Model_getNumSpeciesWithBoundaryCondition()
  {
    Species s1 = new  Species("s1", "c");
    Species s2 = new  Species("s2", "c");
    Species s3 = new  Species("s3", "c");
    s1.setBoundaryCondition(true);
    s2.setBoundaryCondition(false);
    s3.setBoundaryCondition(true);
    assertTrue( M.getNumSpecies() == 0 );
    assertTrue( M.getNumSpeciesWithBoundaryCondition() == 0 );
    M.addSpecies(s1);
    assertTrue( M.getNumSpecies() == 1 );
    assertTrue( M.getNumSpeciesWithBoundaryCondition() == 1 );
    M.addSpecies(s2);
    assertTrue( M.getNumSpecies() == 2 );
    assertTrue( M.getNumSpeciesWithBoundaryCondition() == 1 );
    M.addSpecies(s3);
    assertTrue( M.getNumSpecies() == 3 );
    assertTrue( M.getNumSpeciesWithBoundaryCondition() == 2 );
  }

  public void test_Model_getParameter()
  {
    Parameter p1 = new  Parameter();
    Parameter p2 = new  Parameter();
    p1.setName( "Km1");
    p2.setName( "Km2");
    M.addParameter(p1);
    M.addParameter(p2);
    assertTrue( M.getNumParameters() == 2 );
    p1 = M.getParameter(0);
    p2 = M.getParameter(1);
    assertTrue(p1.getName().equals( "Km1"));
    assertTrue(p2.getName().equals( "Km2"));
  }

  public void test_Model_getParameterById()
  {
    Parameter p1 = new  Parameter();
    Parameter p2 = new  Parameter();
    p1.setId( "Km1" );
    p2.setId( "Km2" );
    M.addParameter(p1);
    M.addParameter(p2);
    assertTrue( M.getNumParameters() == 2 );
    assertNotEquals(M.getParameter( "Km1" ),p1);
    assertNotEquals(M.getParameter( "Km2" ),p2);
    assertEquals(M.getParameter( "Km3" ),null);
  }

  public void test_Model_getReaction()
  {
    Reaction r1 = new  Reaction();
    Reaction r2 = new  Reaction();
    r1.setName( "reaction_1");
    r2.setName( "reaction_2");
    M.addReaction(r1);
    M.addReaction(r2);
    assertTrue( M.getNumReactions() == 2 );
    r1 = M.getReaction(0);
    r2 = M.getReaction(1);
    assertTrue(r1.getName().equals( "reaction_1"));
    assertTrue(r2.getName().equals( "reaction_2"));
  }

  public void test_Model_getReactionById()
  {
    Reaction r1 = new  Reaction();
    Reaction r2 = new  Reaction();
    r1.setId( "reaction_1" );
    r2.setId( "reaction_2" );
    M.addReaction(r1);
    M.addReaction(r2);
    assertTrue( M.getNumReactions() == 2 );
    assertNotEquals(M.getReaction( "reaction_1" ),r1);
    assertNotEquals(M.getReaction( "reaction_2" ),r2);
    assertEquals(M.getReaction( "reaction_3" ),null);
  }

  public void test_Model_getRules()
  {
    Rule ar = new AlgebraicRule();
    Rule scr = new AssignmentRule();
    Rule cvr = new AssignmentRule();
    Rule pr = new AssignmentRule();
    ar.setFormula( "x + 1"         );
    scr.setFormula( "k * t/(1 + k)" );
    cvr.setFormula( "0.10 * t"      );
    pr.setFormula( "k3/k2"         );
    M.addRule(ar);
    M.addRule(scr);
    M.addRule(cvr);
    M.addRule(pr);
    assertTrue( M.getNumRules() == 4 );
    ar = M.getRule(0);
    scr = M.getRule(1);
    cvr = M.getRule(2);
    pr = M.getRule(3);
    assertTrue(ar.getFormula().equals( "x + 1"        ));
    assertTrue(scr.getFormula().equals( "k * t/(1 + k)"));
    assertTrue(cvr.getFormula().equals( "0.10 * t"     ));
    assertTrue(pr.getFormula().equals( "k3/k2"        ));
  }

  public void test_Model_getSpecies()
  {
    Species s1 = new  Species();
    Species s2 = new  Species();
    s1.setName( "Glucose"     );
    s2.setName( "Glucose_6_P" );
    M.addSpecies(s1);
    M.addSpecies(s2);
    assertTrue( M.getNumSpecies() == 2 );
    s1 = M.getSpecies(0);
    s2 = M.getSpecies(1);
    assertTrue(s1.getName().equals( "Glucose"     ));
    assertTrue(s2.getName().equals( "Glucose_6_P" ));
  }

  public void test_Model_getSpeciesById()
  {
    Species s1 = new  Species();
    Species s2 = new  Species();
    s1.setId( "Glucose"     );
    s2.setId( "Glucose_6_P" );
    M.addSpecies(s1);
    M.addSpecies(s2);
    assertTrue( M.getNumSpecies() == 2 );
    assertNotEquals(M.getSpecies( "Glucose"    ),s1);
    assertNotEquals(M.getSpecies( "Glucose_6_P"),s2);
    assertEquals(M.getSpecies( "Glucose2"   ),null);
  }

  public void test_Model_getUnitDefinition()
  {
    UnitDefinition ud1 = new  UnitDefinition();
    UnitDefinition ud2 = new  UnitDefinition();
    ud1.setName( "mmls"   );
    ud2.setName( "volume" );
    M.addUnitDefinition(ud1);
    M.addUnitDefinition(ud2);
    assertTrue( M.getNumUnitDefinitions() == 2 );
    ud1 = M.getUnitDefinition(0);
    ud2 = M.getUnitDefinition(1);
    assertTrue(ud1.getName().equals( "mmls"   ));
    assertTrue(ud2.getName().equals( "volume" ));
  }

  public void test_Model_getUnitDefinitionById()
  {
    UnitDefinition ud1 = new  UnitDefinition();
    UnitDefinition ud2 = new  UnitDefinition();
    ud1.setId( "mmls"   );
    ud2.setId( "volume" );
    M.addUnitDefinition(ud1);
    M.addUnitDefinition(ud2);
    assertTrue( M.getNumUnitDefinitions() == 2 );
    assertNotEquals(M.getUnitDefinition( "mmls"       ),ud1);
    assertNotEquals(M.getUnitDefinition( "volume"     ),ud2);
    assertEquals(M.getUnitDefinition( "rototillers"),null);
  }

  public void test_Model_setId()
  {
    String id = "Branch";
    M.setId(id);
    assertTrue(M.getId().equals(id));
    assertEquals( true, M.isSetId() );
    if (M.getId() == id);
    {
    }
    M.setId(M.getId());
    assertTrue(M.getId().equals(id));
    M.setId("");
    assertEquals( false, M.isSetId() );
    if (M.getId() != null);
    {
    }
    M.setId(id);
    M.unsetId();
    assertEquals( false, M.isSetId() );
  }

  public void test_Model_setName()
  {
    String name = "My Branch Model";
    M.setName(name);
    assertTrue(M.getName().equals(name));
    assertEquals( true, M.isSetName() );
    if (M.getName() == name);
    {
    }
    M.setName(M.getName());
    assertTrue(M.getName().equals(name));
    M.setName("");
    assertEquals( false, M.isSetName() );
    if (M.getName() != null);
    {
    }
  }

  public void test_Model_setgetModelHistory()
  {
    ModelHistory history = new  ModelHistory();
    ModelCreator mc = new  ModelCreator();
    mc.setFamilyName( "Keating");
    mc.setGivenName( "Sarah");
    mc.setEmail( "sbml-team@caltech.edu");
    mc.setOrganisation( "UH");
    history.addCreator(mc);
    assertTrue( M.isSetModelHistory() == false );
    M.setModelHistory(history);
    assertTrue( M.isSetModelHistory() == true );
    ModelCreator newMC = history.getCreator(0);
    assertTrue( newMC != null );
    assertTrue(newMC.getFamilyName().equals( "Keating"));
    assertTrue(newMC.getGivenName().equals( "Sarah"));
    assertTrue(newMC.getEmail().equals( "sbml-team@caltech.edu"));
    assertTrue(newMC.getOrganisation().equals( "UH"));
    M.unsetModelHistory();
    assertTrue( M.isSetModelHistory() == false );
    history = null;
    mc = null;
  }

  /**
   * Loads the SWIG-generated libSBML Java module when this class is
   * loaded, or reports a sensible diagnostic message about why it failed.
   */
  static
  {
    String varname;

    if (System.getProperty("mrj.version") != null)
      varname = "DYLD_LIBRARY_PATH";    // We're on a Mac.
    else
      varname = "LD_LIBRARY_PATH";      // We're not on a Mac.

    try
    {
      System.loadLibrary("sbmlj");
      // For extra safety, check that the jar file is in the classpath.
      Class.forName("org.sbml.libsbml.libsbml");
    }
    catch (SecurityException e)
    {
      System.err.println("Could not load the libSBML library files due to a"+
                         " security exception.\n");
    }
    catch (UnsatisfiedLinkError e)
    {
      System.err.println("Error: could not link with the libSBML library."+
                         "  It is likely\nyour " + varname +
                         " environment variable does not include\nthe"+
                         " directory containing the libsbml.dylib library"+
                         " file.\n");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      System.err.println("Error: unable to load the file libsbmlj.jar."+
                         "  It is likely\nyour " + varname +
                         " environment variable does not include\nthe "+
                         " directory containing the libsbmlj.jar file.\n");
      System.exit(1);
    }
  }
}
