/*
 *
 * @file    TestRequiredAttributes.java
 * @brief   Test hasRequiredAttributes unit tests
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $HeadURL$
 *
 * This test file was converted from src/sbml/test/TestRequiredAttributes.cpp
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

public class TestRequiredAttributes {

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

  public void test_AlgebraicRule()
  {
    AlgebraicRule ar = new AlgebraicRule(2,4);
    assertEquals( true, ar.hasRequiredAttributes() );
    ar = null;
  }

  public void test_AlgebraicRule_L1()
  {
    AlgebraicRule ar = new AlgebraicRule(1,2);
    assertEquals( false, (ar.hasRequiredAttributes()) );
    ar.setFormula("ar");
    assertEquals( true, ar.hasRequiredAttributes() );
    ar = null;
  }

  public void test_AssignmentRule()
  {
    AssignmentRule r = new AssignmentRule(2,4);
    assertEquals( false, (r.hasRequiredAttributes()) );
    r.setVariable("r");
    assertEquals( true, r.hasRequiredAttributes() );
    r = null;
  }

  public void test_AssignmentRule_L1()
  {
    AssignmentRule r = new AssignmentRule(1,2);
    assertEquals( false, (r.hasRequiredAttributes()) );
    r.setVariable("r");
    assertEquals( false, (r.hasRequiredAttributes()) );
    r.setFormula("r");
    assertEquals( true, r.hasRequiredAttributes() );
    r = null;
  }

  public void test_Compartment()
  {
    Compartment c = new Compartment(2,4);
    assertEquals( false, (c.hasRequiredAttributes()) );
    c.setId("c");
    assertEquals( true, c.hasRequiredAttributes() );
    c = null;
  }

  public void test_CompartmentType()
  {
    CompartmentType ct = new CompartmentType(2,4);
    assertEquals( false, (ct.hasRequiredAttributes()) );
    ct.setId("c");
    assertEquals( true, ct.hasRequiredAttributes() );
    ct = null;
  }

  public void test_Constraint()
  {
    Constraint c = new Constraint(2,4);
    assertEquals( true, c.hasRequiredAttributes() );
    c = null;
  }

  public void test_Delay()
  {
    Delay d = new Delay(2,4);
    assertEquals( true, d.hasRequiredAttributes() );
    d = null;
  }

  public void test_Event()
  {
    Event e = new Event(2,4);
    assertEquals( true, e.hasRequiredAttributes() );
    e = null;
  }

  public void test_EventAssignment()
  {
    EventAssignment ea = new EventAssignment(2,4);
    assertEquals( false, (ea.hasRequiredAttributes()) );
    ea.setVariable("ea");
    assertEquals( true, ea.hasRequiredAttributes() );
    ea = null;
  }

  public void test_FunctionDefinition()
  {
    FunctionDefinition fd = new FunctionDefinition(2,4);
    assertEquals( false, (fd.hasRequiredAttributes()) );
    fd.setId("fd");
    assertEquals( true, fd.hasRequiredAttributes() );
    fd = null;
  }

  public void test_InitialAssignment()
  {
    InitialAssignment ia = new InitialAssignment(2,4);
    assertEquals( false, (ia.hasRequiredAttributes()) );
    ia.setSymbol("ia");
    assertEquals( true, ia.hasRequiredAttributes() );
    ia = null;
  }

  public void test_KineticLaw()
  {
    KineticLaw kl = new KineticLaw(2,4);
    assertEquals( true, kl.hasRequiredAttributes() );
    kl = null;
  }

  public void test_KineticLaw_L1()
  {
    KineticLaw kl = new KineticLaw(1,2);
    assertEquals( false, (kl.hasRequiredAttributes()) );
    kl.setFormula("kl");
    assertEquals( true, kl.hasRequiredAttributes() );
    kl = null;
  }

  public void test_Model()
  {
    Model m = new Model(2,4);
    assertEquals( true, m.hasRequiredAttributes() );
    m = null;
  }

  public void test_ModifierSpeciesReference()
  {
    ModifierSpeciesReference msr = new ModifierSpeciesReference(2,4);
    assertEquals( false, (msr.hasRequiredAttributes()) );
    msr.setSpecies("msr");
    assertEquals( true, msr.hasRequiredAttributes() );
    msr = null;
  }

  public void test_Parameter()
  {
    Parameter p = new Parameter(2,4);
    assertEquals( false, (p.hasRequiredAttributes()) );
    p.setId("p");
    assertEquals( true, p.hasRequiredAttributes() );
    p = null;
  }

  public void test_Parameter_L1V1()
  {
    Parameter p = new Parameter(1,1);
    assertEquals( false, (p.hasRequiredAttributes()) );
    p.setId("p");
    assertEquals( false, (p.hasRequiredAttributes()) );
    p.setValue(12);
    assertEquals( true, p.hasRequiredAttributes() );
    p = null;
  }

  public void test_RateRule()
  {
    RateRule r = new RateRule(2,4);
    assertEquals( false, (r.hasRequiredAttributes()) );
    r.setVariable("r");
    assertEquals( true, r.hasRequiredAttributes() );
    r = null;
  }

  public void test_RateRule_L1()
  {
    RateRule r = new RateRule(1,2);
    assertEquals( false, (r.hasRequiredAttributes()) );
    r.setVariable("r");
    assertEquals( false, (r.hasRequiredAttributes()) );
    r.setFormula("r");
    assertEquals( true, r.hasRequiredAttributes() );
    r = null;
  }

  public void test_Reaction()
  {
    Reaction r = new Reaction(2,4);
    assertEquals( false, (r.hasRequiredAttributes()) );
    r.setId("r");
    assertEquals( true, r.hasRequiredAttributes() );
    r = null;
  }

  public void test_Species()
  {
    Species s = new Species(2,4);
    assertEquals( false, (s.hasRequiredAttributes()) );
    s.setId("s");
    assertEquals( false, (s.hasRequiredAttributes()) );
    s.setCompartment("c");
    assertEquals( true, s.hasRequiredAttributes() );
    s = null;
  }

  public void test_SpeciesReference()
  {
    SpeciesReference sr = new SpeciesReference(2,4);
    assertEquals( false, (sr.hasRequiredAttributes()) );
    sr.setSpecies("sr");
    assertEquals( true, sr.hasRequiredAttributes() );
    sr = null;
  }

  public void test_SpeciesType()
  {
    SpeciesType st = new SpeciesType(2,4);
    assertEquals( false, (st.hasRequiredAttributes()) );
    st.setId("st");
    assertEquals( true, st.hasRequiredAttributes() );
    st = null;
  }

  public void test_Species_L1()
  {
    Species s = new Species(1,2);
    assertEquals( false, (s.hasRequiredAttributes()) );
    s.setId("s");
    assertEquals( false, (s.hasRequiredAttributes()) );
    s.setCompartment("c");
    assertEquals( false, (s.hasRequiredAttributes()) );
    s.setInitialAmount(2);
    assertEquals( true, s.hasRequiredAttributes() );
    s = null;
  }

  public void test_StoichiometryMath()
  {
    StoichiometryMath sm = new StoichiometryMath(2,4);
    assertEquals( true, sm.hasRequiredAttributes() );
    sm = null;
  }

  public void test_Trigger()
  {
    Trigger t = new Trigger(2,4);
    assertEquals( true, t.hasRequiredAttributes() );
    t = null;
  }

  public void test_Unit()
  {
    Unit u = new Unit(2,4);
    assertEquals( false, (u.hasRequiredAttributes()) );
    u.setKind(libsbml.UNIT_KIND_MOLE);
    assertEquals( true, u.hasRequiredAttributes() );
    u = null;
  }

  public void test_UnitDefinition()
  {
    UnitDefinition ud = new UnitDefinition(2,4);
    assertEquals( false, (ud.hasRequiredAttributes()) );
    ud.setId("ud");
    assertEquals( true, ud.hasRequiredAttributes() );
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
