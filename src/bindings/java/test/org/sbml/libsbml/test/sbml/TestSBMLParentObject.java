/*
 *
 * @file    TestSBMLParentObject.java
 * @brief   SBML parent object unit tests
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Sarah Keating 
 *
 * $Id:$
 * $HeadURL:$
 *
 * This test file was converted from src/sbml/test/TestSBMLParentObject.cpp
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

public class TestSBMLParentObject {

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

  public void test_AlgebraicRule_parent_create()
  {
    Model m = new Model();
    AlgebraicRule r = m.createAlgebraicRule();
    ListOf lo = m.getListOfRules();
    assertTrue( lo.equals(m.getRule(0).getParentSBMLObject()) );
    assertTrue( lo.equals(r.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_AssignmentRule_parent_create()
  {
    Model m = new Model();
    AssignmentRule r = m.createAssignmentRule();
    ListOf lo = m.getListOfRules();
    assertTrue( lo.equals(m.getRule(0).getParentSBMLObject()) );
    assertTrue( lo.equals(r.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_CompartmentType_parent_add()
  {
    CompartmentType ct = new CompartmentType();
    Model m = new Model();
    m.addCompartmentType(ct);
    ct = null;
    ListOf lo = m.getListOfCompartmentTypes();
    assertTrue( lo.equals(m.getCompartmentType(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_CompartmentType_parent_create()
  {
    Model m = new Model();
    CompartmentType ct = m.createCompartmentType();
    ListOf lo = m.getListOfCompartmentTypes();
    assertTrue( lo.equals(m.getCompartmentType(0).getParentSBMLObject()) );
    assertTrue( lo.equals(ct.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Compartment_parent_add()
  {
    Compartment c = new Compartment();
    Model m = new Model();
    m.addCompartment(c);
    c = null;
    ListOf lo = m.getListOfCompartments();
    assertTrue( lo.equals(m.getCompartment(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Compartment_parent_create()
  {
    Model m = new Model();
    Compartment c = m.createCompartment();
    ListOf lo = m.getListOfCompartments();
    assertTrue( lo.equals(m.getCompartment(0).getParentSBMLObject()) );
    assertTrue( lo.equals(c.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Constraint_parent_add()
  {
    Constraint ct = new Constraint();
    Model m = new Model();
    m.addConstraint(ct);
    ct = null;
    ListOf lo = m.getListOfConstraints();
    assertTrue( lo.equals(m.getConstraint(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Constraint_parent_create()
  {
    Model m = new Model();
    Constraint ct = m.createConstraint();
    ListOf lo = m.getListOfConstraints();
    assertTrue( lo.equals(m.getConstraint(0).getParentSBMLObject()) );
    assertTrue( lo.equals(ct.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Delay_parent_add()
  {
    Delay d = new Delay();
    Event e = new Event();
    e.setDelay(d);
    d = null;
    assertTrue( e.equals(e.getDelay().getParentSBMLObject()) );
    e = null;
  }

  public void test_EventAssignment_parent_add()
  {
    Event e = new Event();
    EventAssignment ea = new EventAssignment();
    e.addEventAssignment(ea);
    ea = null;
    ListOf lo = e.getListOfEventAssignments();
    assertTrue( lo.equals(e.getEventAssignment(0).getParentSBMLObject()) );
    assertTrue( e.equals(lo.getParentSBMLObject()) );
  }

  public void test_EventAssignment_parent_create()
  {
    Event e = new Event();
    EventAssignment ea = e.createEventAssignment();
    ListOf lo = e.getListOfEventAssignments();
    assertTrue( lo.equals(e.getEventAssignment(0).getParentSBMLObject()) );
    assertTrue( lo.equals(ea.getParentSBMLObject()) );
    assertTrue( e.equals(lo.getParentSBMLObject()) );
  }

  public void test_EventAssignment_parent_create_model()
  {
    Model m = new Model();
    Event e = m.createEvent();
    EventAssignment ea = m.createEventAssignment();
    ListOf lo = e.getListOfEventAssignments();
    assertTrue( lo.equals(e.getEventAssignment(0).getParentSBMLObject()) );
    assertTrue( lo.equals(ea.getParentSBMLObject()) );
    assertTrue( e.equals(lo.getParentSBMLObject()) );
  }

  public void test_Event_parent_add()
  {
    Event e = new Event();
    Model m = new Model();
    m.addEvent(e);
    e = null;
    ListOf lo = m.getListOfEvents();
    assertTrue( lo.equals(m.getEvent(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Event_parent_create()
  {
    Model m = new Model();
    Event e = m.createEvent();
    ListOf lo = m.getListOfEvents();
    assertTrue( lo.equals(m.getEvent(0).getParentSBMLObject()) );
    assertTrue( lo.equals(e.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_FunctionDefinition_parent_add()
  {
    FunctionDefinition fd = new FunctionDefinition();
    Model m = new Model();
    m.addFunctionDefinition(fd);
    fd = null;
    ListOf lo = m.getListOfFunctionDefinitions();
    assertTrue( lo.equals(m.getFunctionDefinition(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_FunctionDefinition_parent_create()
  {
    Model m = new Model();
    FunctionDefinition fd = m.createFunctionDefinition();
    ListOf lo = m.getListOfFunctionDefinitions();
    assertTrue( lo.equals(m.getFunctionDefinition(0).getParentSBMLObject()) );
    assertTrue( lo.equals(fd.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_InitialAssignment_parent_add()
  {
    InitialAssignment ia = new InitialAssignment();
    Model m = new Model();
    m.addInitialAssignment(ia);
    ia = null;
    ListOf lo = m.getListOfInitialAssignments();
    assertTrue( lo.equals(m.getInitialAssignment(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_InitialAssignment_parent_create()
  {
    Model m = new Model();
    InitialAssignment ia = m.createInitialAssignment();
    ListOf lo = m.getListOfInitialAssignments();
    assertTrue( lo.equals(m.getInitialAssignment(0).getParentSBMLObject()) );
    assertTrue( lo.equals(ia.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_KineticLaw_Parameter_parent_add()
  {
    KineticLaw kl = new KineticLaw();
    Parameter p = new Parameter("jake");
    kl.addParameter(p);
    p = null;
    assertTrue( kl.getNumParameters() == 1 );
    assertTrue( kl.getParameter(0).getId().equals( "jake") == true );
    ListOfParameters lop = kl.getListOfParameters();
    assertTrue( kl.equals(lop.getParentSBMLObject()) );
    assertTrue( lop.equals(kl.getParameter(0).getParentSBMLObject()) );
    kl = null;
  }

  public void test_KineticLaw_Parameter_parent_create()
  {
    KineticLaw kl = new KineticLaw();
    Parameter p = kl.createParameter();
    assertTrue( kl.getNumParameters() == 1 );
    ListOfParameters lop = kl.getListOfParameters();
    assertTrue( kl.equals(lop.getParentSBMLObject()) );
    assertTrue( lop.equals(p.getParentSBMLObject()) );
    assertTrue( lop.equals(kl.getParameter(0).getParentSBMLObject()) );
    kl = null;
  }

  public void test_KineticLaw_Parameter_parent_create_model()
  {
    Model m = new Model();
    Reaction r = m.createReaction();
    KineticLaw kl = m.createKineticLaw();
    Parameter p = m.createKineticLawParameter();
    assertTrue( kl.getNumParameters() == 1 );
    ListOfParameters lop = kl.getListOfParameters();
    assertTrue( r.equals(kl.getParentSBMLObject()) );
    assertTrue( kl.equals(lop.getParentSBMLObject()) );
    assertTrue( lop.equals(p.getParentSBMLObject()) );
    assertTrue( lop.equals(kl.getParameter(0).getParentSBMLObject()) );
    kl = null;
  }

  public void test_KineticLaw_parent_add()
  {
    KineticLaw kl = new KineticLaw();
    Reaction r = new Reaction();
    r.setKineticLaw(kl);
    assertTrue( r.equals(r.getKineticLaw().getParentSBMLObject()) );
    r = null;
  }

  public void test_KineticLaw_parent_create()
  {
    Reaction r = new Reaction();
    KineticLaw kl = r.createKineticLaw();
    assertTrue( r.equals(kl.getParentSBMLObject()) );
    r = null;
  }

  public void test_KineticLaw_parent_create_model()
  {
    Model m = new Model();
    Reaction r = m.createReaction();
    KineticLaw kl = r.createKineticLaw();
    assertTrue( r.equals(kl.getParentSBMLObject()) );
    assertTrue( r.equals(r.getKineticLaw().getParentSBMLObject()) );
    r = null;
  }

  public void test_Model_parent_add()
  {
    SBMLDocument d = new SBMLDocument();
    Model m = new Model();
    d.setModel(m);
    assertTrue( d.equals(d.getModel().getParentSBMLObject()) );
    d = null;
  }

  public void test_Model_parent_create()
  {
    SBMLDocument d = new SBMLDocument();
    Model m = d.createModel();
    assertTrue( d.equals(m.getParentSBMLObject()) );
    d = null;
  }

  public void test_Parameter_parent_add()
  {
    Parameter ia = new Parameter();
    Model m = new Model();
    m.addParameter(ia);
    ia = null;
    ListOf lo = m.getListOfParameters();
    assertTrue( lo.equals(m.getParameter(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Parameter_parent_create()
  {
    Model m = new Model();
    Parameter p = m.createParameter();
    ListOf lo = m.getListOfParameters();
    assertTrue( lo.equals(m.getParameter(0).getParentSBMLObject()) );
    assertTrue( lo.equals(p.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_RateRule_parent_create()
  {
    Model m = new Model();
    RateRule r = m.createRateRule();
    ListOf lo = m.getListOfRules();
    assertTrue( lo.equals(m.getRule(0).getParentSBMLObject()) );
    assertTrue( lo.equals(r.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Reaction_parent_add()
  {
    Reaction ia = new Reaction();
    Model m = new Model();
    m.addReaction(ia);
    ia = null;
    ListOf lo = m.getListOfReactions();
    assertTrue( lo.equals(m.getReaction(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Reaction_parent_create()
  {
    Model m = new Model();
    Reaction r = m.createReaction();
    ListOf lo = m.getListOfReactions();
    assertTrue( lo.equals(m.getReaction(0).getParentSBMLObject()) );
    assertTrue( lo.equals(r.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Rule_parent_add()
  {
    Rule ia = new RateRule("a");
    Model m = new Model();
    m.addRule(ia);
    ia = null;
    ListOf lo = m.getListOfRules();
    assertTrue( lo.equals(m.getRule(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_SpeciesReference_Modifier_parent_add()
  {
    ModifierSpeciesReference sr = new ModifierSpeciesReference();
    Reaction r = new Reaction();
    r.addModifier(sr);
    sr = null;
    ListOf lo = r.getListOfModifiers();
    assertTrue( lo.equals(r.getModifier(0).getParentSBMLObject()) );
    assertTrue( r.equals(lo.getParentSBMLObject()) );
  }

  public void test_SpeciesReference_Modifier_parent_create()
  {
    Reaction r = new Reaction();
    ModifierSpeciesReference sr = r.createModifier();
    ListOf lo = r.getListOfModifiers();
    assertTrue( lo.equals(sr.getParentSBMLObject()) );
    assertTrue( lo.equals(r.getModifier(0).getParentSBMLObject()) );
    assertTrue( r.equals(lo.getParentSBMLObject()) );
  }

  public void test_SpeciesReference_Modifier_parent_create_model()
  {
    Model m = new Model();
    Reaction r = m.createReaction();
    ModifierSpeciesReference sr = m.createModifier();
    ListOf lo = r.getListOfModifiers();
    assertTrue( lo.equals(sr.getParentSBMLObject()) );
    assertTrue( lo.equals(r.getModifier(0).getParentSBMLObject()) );
    assertTrue( r.equals(lo.getParentSBMLObject()) );
  }

  public void test_SpeciesReference_Product_parent_add()
  {
    SpeciesReference sr = new SpeciesReference();
    Reaction r = new Reaction();
    r.addProduct(sr);
    sr = null;
    ListOf lo = r.getListOfProducts();
    assertTrue( lo.equals(r.getProduct(0).getParentSBMLObject()) );
    assertTrue( r.equals(lo.getParentSBMLObject()) );
  }

  public void test_SpeciesReference_Product_parent_create()
  {
    Reaction r = new Reaction();
    SpeciesReference sr = r.createProduct();
    ListOf lo = r.getListOfProducts();
    assertTrue( lo.equals(r.getProduct(0).getParentSBMLObject()) );
    assertTrue( lo.equals(sr.getParentSBMLObject()) );
    assertTrue( r.equals(lo.getParentSBMLObject()) );
  }

  public void test_SpeciesReference_Product_parent_create_model()
  {
    Model m = new Model();
    Reaction r = m.createReaction();
    SpeciesReference sr = m.createProduct();
    ListOf lo = r.getListOfProducts();
    assertTrue( lo.equals(r.getProduct(0).getParentSBMLObject()) );
    assertTrue( lo.equals(sr.getParentSBMLObject()) );
    assertTrue( r.equals(lo.getParentSBMLObject()) );
  }

  public void test_SpeciesReference_Reactant_parent_add()
  {
    SpeciesReference sr = new SpeciesReference();
    Reaction r = new Reaction();
    r.addReactant(sr);
    sr = null;
    ListOf lo = r.getListOfReactants();
    assertTrue( lo.equals(r.getReactant(0).getParentSBMLObject()) );
    assertTrue( r.equals(lo.getParentSBMLObject()) );
  }

  public void test_SpeciesReference_Reactant_parent_create()
  {
    Reaction r = new Reaction();
    SpeciesReference sr = r.createReactant();
    ListOf lo = r.getListOfReactants();
    assertTrue( lo.equals(r.getReactant(0).getParentSBMLObject()) );
    assertTrue( lo.equals(sr.getParentSBMLObject()) );
    assertTrue( r.equals(lo.getParentSBMLObject()) );
  }

  public void test_SpeciesReference_Reactant_parent_create_model()
  {
    Model m = new Model();
    Reaction r = m.createReaction();
    SpeciesReference sr = m.createReactant();
    ListOf lo = r.getListOfReactants();
    assertTrue( lo.equals(r.getReactant(0).getParentSBMLObject()) );
    assertTrue( lo.equals(sr.getParentSBMLObject()) );
    assertTrue( r.equals(lo.getParentSBMLObject()) );
  }

  public void test_SpeciesType_parent_add()
  {
    SpeciesType ia = new SpeciesType();
    Model m = new Model();
    m.addSpeciesType(ia);
    ia = null;
    ListOf lo = m.getListOfSpeciesTypes();
    assertTrue( lo.equals(m.getSpeciesType(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_SpeciesType_parent_create()
  {
    Model m = new Model();
    SpeciesType st = m.createSpeciesType();
    ListOf lo = m.getListOfSpeciesTypes();
    assertTrue( lo.equals(m.getSpeciesType(0).getParentSBMLObject()) );
    assertTrue( lo.equals(st.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Species_parent_add()
  {
    Species ia = new Species();
    Model m = new Model();
    m.addSpecies(ia);
    ia = null;
    ListOf lo = m.getListOfSpecies();
    assertTrue( lo.equals(m.getSpecies(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Species_parent_create()
  {
    Model m = new Model();
    Species s = m.createSpecies();
    ListOf lo = m.getListOfSpecies();
    assertTrue( lo.equals(s.getParentSBMLObject()) );
    assertTrue( lo.equals(m.getSpecies(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_StoichiometryMath_parent_add()
  {
    StoichiometryMath m = new StoichiometryMath();
    SpeciesReference sr = new SpeciesReference();
    sr.setStoichiometryMath(m);
    m = null;
    assertTrue( sr.equals(sr.getStoichiometryMath().getParentSBMLObject()) );
    sr = null;
  }

  public void test_Trigger_parent_add()
  {
    Trigger d = new Trigger();
    Event e = new Event();
    e.setTrigger(d);
    d = null;
    assertTrue( e.equals(e.getTrigger().getParentSBMLObject()) );
    e = null;
  }

  public void test_UnitDefinition_parent_add()
  {
    UnitDefinition ia = new UnitDefinition();
    Model m = new Model();
    m.addUnitDefinition(ia);
    ia = null;
    ListOf lo = m.getListOfUnitDefinitions();
    assertTrue( lo.equals(m.getUnitDefinition(0).getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_UnitDefinition_parent_create()
  {
    Model m = new Model();
    UnitDefinition ud = m.createUnitDefinition();
    ListOf lo = m.getListOfUnitDefinitions();
    assertTrue( lo.equals(m.getUnitDefinition(0).getParentSBMLObject()) );
    assertTrue( lo.equals(ud.getParentSBMLObject()) );
    assertTrue( m.equals(lo.getParentSBMLObject()) );
  }

  public void test_Unit_parent_add()
  {
    UnitDefinition ud = new UnitDefinition();
    Unit u = new Unit();
    ud.addUnit(u);
    u = null;
    assertTrue( ud.getNumUnits() == 1 );
    ListOf lo = ud.getListOfUnits();
    assertTrue( lo.equals(ud.getUnit(0).getParentSBMLObject()) );
    assertTrue( ud.equals(lo.getParentSBMLObject()) );
    ud = null;
  }

  public void test_Unit_parent_create()
  {
    UnitDefinition ud = new UnitDefinition();
    Unit u = ud.createUnit();
    assertTrue( ud.getNumUnits() == 1 );
    ListOf lo = ud.getListOfUnits();
    assertTrue( lo.equals(ud.getUnit(0).getParentSBMLObject()) );
    assertTrue( lo.equals(u.getParentSBMLObject()) );
    assertTrue( ud.equals(lo.getParentSBMLObject()) );
    ud = null;
  }

  public void test_Unit_parent_create_model()
  {
    Model m = new Model();
    UnitDefinition ud = m.createUnitDefinition();
    Unit u = m.createUnit();
    assertTrue( ud.getNumUnits() == 1 );
    ListOf lo = ud.getListOfUnits();
    assertTrue( lo.equals(ud.getUnit(0).getParentSBMLObject()) );
    assertTrue( lo.equals(u.getParentSBMLObject()) );
    assertTrue( ud.equals(lo.getParentSBMLObject()) );
    ud = null;
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
