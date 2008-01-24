/*
 *
 * @file    TestReadFromFile5.java
 * @brief   Reads test-data/l2v1-assignment.xml into memory and tests it.
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestReadFromFile5.cpp
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

public class TestReadFromFile5 {

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


  public void test_read_l2v1_assignment()
  {
    SBMLReader reader = new SBMLReader();
    SBMLDocument d;
    Model m;
    Compartment c;
    Species s;
    Parameter p;
    AssignmentRule ar;
    Reaction r;
    SpeciesReference sr;
    KineticLaw kl;
    UnitDefinition ud;
    String filename = new String( "../../sbml/test/test-data/" );
    filename += "l2v1-assignment.xml";
    d = reader.readSBML(filename);
    if (d == null);
    {
    }
    assertTrue( d.getLevel() == 2 );
    assertTrue( d.getVersion() == 1 );
    m = d.getModel();
    assertTrue( m != null );
    assertTrue( m.getNumCompartments() == 1 );
    c = m.getCompartment(0);
    assertTrue( c != null );
    assertTrue( c.getId().equals("cell") == true );
    ud = c.getDerivedUnitDefinition();
    assertTrue( ud.getNumUnits() == 1 );
    assertTrue( ud.getUnit(0).getKind() == libsbml.UNIT_KIND_LITRE );
    assertTrue( m.getNumSpecies() == 5 );
    s = m.getSpecies(0);
    assertTrue( s != null );
    assertTrue( s.getId().equals("X0") == true );
    assertEquals(s.getCompartment(),"cell");
    assertTrue( s.getInitialConcentration() == 1.0 );
    s = m.getSpecies(1);
    assertTrue( s != null );
    assertTrue( s.getId().equals("X1") == true );
    assertEquals(s.getCompartment(),"cell");
    assertTrue( s.getInitialConcentration() == 0.0 );
    s = m.getSpecies(2);
    assertTrue( s != null );
    assertTrue( s.getId().equals("T") == true );
    assertEquals(s.getCompartment(),"cell");
    assertTrue( s.getInitialConcentration() == 0.0 );
    s = m.getSpecies(3);
    assertTrue( s != null );
    assertTrue( s.getId().equals("S1") == true );
    assertEquals(s.getCompartment(),"cell");
    assertTrue( s.getInitialConcentration() == 0.0 );
    s = m.getSpecies(4);
    assertTrue( s != null );
    assertTrue( s.getId().equals("S2") == true );
    assertEquals(s.getCompartment(),"cell");
    assertTrue( s.getInitialConcentration() == 0.0 );
    assertTrue( m.getNumParameters() == 1 );
    p = m.getParameter(0);
    assertTrue( p != null );
    assertTrue( p.getId().equals("Keq") == true );
    assertTrue( p.getValue() == 2.5 );
    ud = p.getDerivedUnitDefinition();
    assertTrue( ud.getNumUnits() == 0 );
    assertTrue( m.getNumRules() == 2 );
    ar = ((AssignmentRule)  m.getRule(0) );
    assertTrue( ar != null );
    assertTrue( ar.getVariable().equals("S1") == true );
    assertTrue( ar.getFormula().equals("T / (1 + Keq)") == true );
    ud = ar.getDerivedUnitDefinition();
    assertTrue( ud.getNumUnits() == 2 );
    assertTrue( ud.getUnit(0).getKind() == libsbml.UNIT_KIND_MOLE );
    assertTrue( ud.getUnit(0).getExponent() == 1 );
    assertTrue( ud.getUnit(1).getKind() == libsbml.UNIT_KIND_LITRE );
    assertTrue( ud.getUnit(1).getExponent() == -1 );
    assertTrue( ar.containsUndeclaredUnits() == true );
    ar = ((AssignmentRule)  m.getRule(1) );
    assertTrue( ar != null );
    assertTrue( ar.getVariable().equals("S2") == true );
    assertTrue( ar.getFormula().equals("Keq * S1") == true );
    assertTrue( m.getNumReactions() == 2 );
    r = m.getReaction(0);
    assertTrue( r != null );
    assertTrue( r.getId().equals("in") == true );
    assertTrue( r.getNumReactants() == 1 );
    assertTrue( r.getNumProducts() == 1 );
    sr = r.getReactant(0);
    assertTrue( sr != null );
    assertEquals(sr.getSpecies(),"X0");
    sr = r.getProduct(0);
    assertTrue( sr != null );
    assertEquals(sr.getSpecies(),"T");
    kl = r.getKineticLaw();
    assertTrue( kl != null );
    assertTrue( kl.getFormula().equals("k1 * X0") == true );
    assertTrue( kl.getNumParameters() == 1 );
    p = kl.getParameter(0);
    assertTrue( p != null );
    assertTrue( p.getId().equals("k1") == true );
    assertTrue( p.getValue() == 0.1 );
    r = m.getReaction(1);
    assertTrue( r != null );
    assertTrue( r.getId().equals("out") == true );
    assertTrue( r.getNumReactants() == 1 );
    assertTrue( r.getNumProducts() == 1 );
    sr = r.getReactant(0);
    assertTrue( sr != null );
    assertEquals(sr.getSpecies(),"T");
    sr = r.getProduct(0);
    assertTrue( sr != null );
    assertEquals(sr.getSpecies(),"X1");
    kl = r.getKineticLaw();
    assertTrue( kl != null );
    assertTrue( kl.getFormula().equals("k2 * T") == true );
    assertTrue( kl.getNumParameters() == 1 );
    p = kl.getParameter(0);
    assertTrue( p != null );
    assertTrue( p.getId().equals("k2") == true );
    assertTrue( p.getValue() == 0.15 );
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
