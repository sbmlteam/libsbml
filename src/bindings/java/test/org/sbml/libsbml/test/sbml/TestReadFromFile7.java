/*
 *
 * @file    TestReadFromFile7.java
 * @brief   Reads test-data/l2v3-all.xml into memory and tests it.
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Sarah Keating 
 *
 * $Id$
 * $HeadURL$
 *
 * This test file was converted from src/sbml/test/TestReadFromFile7.cpp
 * with the help of conversion sciprt (ctest_converter.pl).
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
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

public class TestReadFromFile7 {

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

  public void test_read_l2v3_all()
  {
    SBMLReader reader = new SBMLReader();
    SBMLDocument d;
    Model m;
    Compartment c;
    CompartmentType ct;
    Species s;
    Parameter p;
    AssignmentRule ar;
    Reaction r;
    SpeciesReference sr;
    KineticLaw kl;
    UnitDefinition ud;
    Constraint con;
    Event e;
    Delay delay;
    Trigger trigger;
    EventAssignment ea;
    FunctionDefinition fd;
    InitialAssignment ia;
    AlgebraicRule alg;
    RateRule rr;
    SpeciesType st;
    StoichiometryMath stoich;
    Unit u;
    ListOfEvents loe;
    Event e1;
    ListOfEventAssignments loea;
    EventAssignment ea1;
    ListOfFunctionDefinitions lofd;
    FunctionDefinition fd1;
    ListOfParameters lop;
    Parameter p1;
    ListOfSpeciesTypes lost;
    SpeciesType st1;
    ListOfUnitDefinitions loud;
    UnitDefinition ud1;
    ListOfUnits lou;
    Unit u1;
    ASTNode ast;
    String filename = new String( "../../sbml/test/test-data/" );
    filename += "l2v3-all.xml";
    d = reader.readSBML(filename);
    if (d == null);
    {
    }
    assertTrue( d.getLevel() == 2 );
    assertTrue( d.getVersion() == 3 );
    m = d.getModel();
    assertTrue( m != null );
    assertTrue( m.getId().equals( "l2v3_all") );
    assertTrue( m.getNumCompartments() == 1 );
    c = m.getCompartment(0);
    assertTrue( c != null );
    assertTrue( c.getId().equals( "a") );
    assertEquals(c.getCompartmentType(), "hh");
    assertTrue( c.getSBOTerm() == 236 );
    assertTrue( c.getSBOTermID().equals( "SBO:0000236") );
    assertTrue( c.getSize() == 2.3 );
    assertTrue( m.getNumCompartmentTypes() == 1 );
    ct = m.getCompartmentType(0);
    assertTrue( ct != null );
    assertTrue( ct.getId().equals( "hh") );
    assertTrue( ct.getSBOTerm() == 236 );
    assertTrue( ct.getSBOTermID().equals( "SBO:0000236") );
    assertTrue( m.getNumSpeciesTypes() == 1 );
    st = m.getSpeciesType(0);
    assertTrue( st != null );
    assertTrue( st.getId().equals( "gg") );
    assertTrue( st.getName().equals( "dd") );
    assertTrue( st.getSBOTerm() == 236 );
    assertTrue( st.getSBOTermID().equals( "SBO:0000236") );
    lost = m.getListOfSpeciesTypes();
    st1 = lost.get(0);
    assertTrue( st1.equals(st) );
    st1 = lost.get("gg");
    assertTrue( st1.equals(st) );
    assertTrue( m.getNumConstraints() == 1 );
    con = m.getConstraint(0);
    assertTrue( con != null );
    ast = con.getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "lt(x, 3)"));
    assertTrue( m.getNumEvents() == 1 );
    e = m.getEvent(0);
    assertTrue( e != null );
    assertTrue( e.getId().equals( "e1") );
    assertTrue( e.getSBOTerm() == 231 );
    assertTrue( e.getSBOTermID().equals( "SBO:0000231") );
    assertEquals( true, e.isSetDelay() );
    delay = e.getDelay();
    assertTrue( delay != null );
    assertTrue( delay.getSBOTerm() == 64 );
    assertTrue( delay.getSBOTermID().equals( "SBO:0000064") );
    ast = delay.getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "p + 3"));
    assertEquals( true, e.isSetTrigger() );
    trigger = e.getTrigger();
    assertTrue( trigger != null );
    assertTrue( trigger.getSBOTerm() == 64 );
    assertTrue( trigger.getSBOTermID().equals( "SBO:0000064") );
    ast = trigger.getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "lt(x, 3)"));
    loe = m.getListOfEvents();
    e1 = loe.get(0);
    assertTrue( e1.equals(e) );
    e1 = loe.get("e1");
    assertTrue( e1.equals(e) );
    assertTrue( e.getNumEventAssignments() == 1 );
    ea = e.getEventAssignment(0);
    assertTrue( ea != null );
    assertTrue( ea.getVariable().equals( "a") );
    assertTrue( ea.getSBOTerm() == 64 );
    assertTrue( ea.getSBOTermID().equals( "SBO:0000064") );
    ast = ea.getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "x * p3"));
    loea = e.getListOfEventAssignments();
    ea1 = loea.get(0);
    assertTrue( ea1.equals(ea) );
    ea1 = loea.get("a");
    assertTrue( ea1.equals(ea) );
    assertTrue( m.getNumFunctionDefinitions() == 1 );
    fd = m.getFunctionDefinition(0);
    assertTrue( fd != null );
    assertTrue( fd.getId().equals( "fd") );
    assertTrue( fd.getSBOTerm() == 64 );
    assertTrue( fd.getSBOTermID().equals( "SBO:0000064") );
    ast = fd.getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "lambda(x, pow(x, 3))"));
    lofd = m.getListOfFunctionDefinitions();
    fd1 = lofd.get(0);
    assertTrue( fd1.equals(fd) );
    fd1 = lofd.get("fd");
    assertTrue( fd1.equals(fd) );
    assertTrue( m.getNumInitialAssignments() == 1 );
    ia = m.getInitialAssignment(0);
    assertTrue( ia != null );
    assertTrue( ia.getSymbol().equals( "p1") );
    ast = ia.getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "x * p3"));
    assertTrue( m.getNumRules() == 3 );
    alg = (AlgebraicRule)  m.getRule(0);
    assertTrue( alg != null );
    assertTrue( alg.getSBOTerm() == 64 );
    assertTrue( alg.getSBOTermID().equals( "SBO:0000064") );
    ast = alg.getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "pow(x, 3)"));
    ar = (AssignmentRule) m.getRule(1);
    assertTrue( ar != null );
    assertTrue( ar.getVariable().equals( "p2") );
    assertTrue( ar.getSBOTerm() == 64 );
    assertTrue( ar.getSBOTermID().equals( "SBO:0000064") );
    ast = ar.getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "x * p3"));
    rr = (RateRule) m.getRule(2);
    assertTrue( rr != null );
    assertTrue( rr.getVariable().equals( "p3") );
    assertTrue( rr.getSBOTerm() == 64 );
    assertTrue( rr.getSBOTermID().equals( "SBO:0000064") );
    ast = rr.getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "p1 / p"));
    assertTrue( m.getNumSpecies() == 1 );
    s = m.getSpecies(0);
    assertTrue( s != null );
    assertTrue( s.getId().equals( "s") );
    assertEquals(s.getSpeciesType(), "gg");
    assertTrue( s.getCompartment().equals( "a") );
    assertTrue( s.getSBOTerm() == 236 );
    assertTrue( s.getSBOTermID().equals( "SBO:0000236") );
    assertEquals( true, s.isSetInitialAmount() );
    assertEquals( false, s.isSetInitialConcentration() );
    assertTrue( s.getInitialAmount() == 0 );
    assertTrue( m.getNumReactions() == 1 );
    r = m.getReaction(0);
    assertTrue( r != null );
    assertTrue( r.getId().equals( "r") );
    assertEquals( false, r.getReversible() );
    assertEquals( true, r.getFast() );
    assertEquals( true, r.isSetKineticLaw() );
    kl = r.getKineticLaw();
    assertTrue( kl != null );
    assertEquals( true, kl.isSetMath() );
    ast = kl.getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "s * k / p"));
    assertTrue( kl.getNumParameters() == 2 );
    p = kl.getParameter(0);
    assertTrue( p != null );
    assertTrue( p.getId().equals( "k") );
    assertTrue( p.getUnits().equals( "litre") );
    assertTrue( p.getValue() == 9 );
    ud = p.getDerivedUnitDefinition();
    assertTrue( ud.getNumUnits() == 1 );
    assertTrue( ud.getUnit(0).getKind() == libsbml.UNIT_KIND_LITRE );
    assertTrue( ud.getUnit(0).getExponent() == 1 );
    lop = kl.getListOfParameters();
    p1 = lop.get(0);
    assertTrue( p1.equals(p) );
    p1 = lop.get("k");
    assertTrue( p1.equals(p) );
    p = kl.getParameter(1);
    assertTrue( p != null );
    assertTrue( p.getId().equals( "k1") );
    assertTrue( p.getUnits().equals( "ud1") );
    assertTrue( p.getValue() == 9 );
    ud = p.getDerivedUnitDefinition();
    assertTrue( ud.getNumUnits() == 1 );
    assertTrue( ud.getUnit(0).getKind() == libsbml.UNIT_KIND_MOLE );
    assertTrue( ud.getUnit(0).getExponent() == 1 );
    assertTrue( r.getNumReactants() == 1 );
    assertTrue( r.getNumProducts() == 0 );
    assertTrue( r.getNumModifiers() == 0 );
    sr = r.getReactant(0);
    assertTrue( sr != null );
    assertTrue( sr.getSpecies().equals( "s") );
    assertTrue( sr.getSBOTerm() == 11 );
    assertTrue( sr.getSBOTermID().equals( "SBO:0000011") );
    stoich = sr.getStoichiometryMath();
    assertTrue( stoich != null );
    assertTrue( stoich.getSBOTerm() == 64 );
    assertTrue( stoich.getSBOTermID().equals( "SBO:0000064") );
    ast = stoich.getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "s * p"));
    assertTrue( m.getNumUnitDefinitions() == 1 );
    ud = m.getUnitDefinition(0);
    assertTrue( ud != null );
    assertTrue( ud.getId().equals( "ud1") );
    loud = m.getListOfUnitDefinitions();
    ud1 = loud.get(0);
    assertTrue( ud1.equals(ud) );
    ud1 = loud.get("ud1");
    assertTrue( ud1.equals(ud) );
    assertTrue( ud.getNumUnits() == 1 );
    u = ud.getUnit(0);
    assertTrue( u != null );
    assertTrue( u.getKind() == libsbml.UNIT_KIND_MOLE );
    lou = ud.getListOfUnits();
    u1 = lou.get(0);
    assertTrue( u1.equals(u) );
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
