/*
 *
 * @file    TestSBMLConvert.java
 * @brief   SBMLConvert unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestSBMLConvert.c
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

public class TestSBMLConvert {

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


  public void test_SBMLConvert_addModifiersToReaction()
  {
    SBMLDocument d = new  SBMLDocument(1,2);
    Model m = d.createModel();
    KineticLaw kl = new  KineticLaw("k1*S1*S2*S3*S4*S5");
    Reaction r = new  Reaction("R", "",kl,false);
    SimpleSpeciesReference ssr1;
    SimpleSpeciesReference ssr2;
    m.addSpecies(new  Species("S1", ""));
    m.addSpecies(new  Species("S2", ""));
    m.addSpecies(new  Species("S3", ""));
    m.addSpecies(new  Species("S4", ""));
    m.addSpecies(new  Species("S5", ""));
    r.addReactant(new  SpeciesReference("S1",1,1));
    r.addReactant(new  SpeciesReference("S2",1,1));
    r.addProduct(new  SpeciesReference("S5",1,1));
    m.addReaction(r);
    assertTrue( r.getNumModifiers() == 0 );
    d.setLevelAndVersion(2,1);
    assertTrue( d.getLevel() == 2 );
    assertTrue( d.getVersion() == 1 );
    assertTrue( m.getReaction(0).getNumModifiers() == 2 );
    ssr1 = m.getReaction(0).getModifier(0);
    ssr2 = m.getReaction(0).getModifier(1);
    assertTrue(ssr1.getSpecies().equals( "S3"));
    assertTrue(ssr2.getSpecies().equals( "S4"));
    d = null;
  }

  public void test_SBMLConvert_convertToL1_SBMLDocument()
  {
    SBMLDocument d = new  SBMLDocument(2,1);
    d.setLevelAndVersion(1,2);
    assertTrue( d.getLevel() == 1 );
    assertTrue( d.getVersion() == 2 );
    d = null;
  }

  public void test_SBMLConvert_convertToL1_Species_Amount()
  {
    SBMLDocument d = new  SBMLDocument(2,1);
    Model m = d.createModel();
    String sid = "C";
    Compartment c = new  Compartment();
    Species s = new  Species();
    c.setId(sid);
    m.addCompartment(c);
    s.setCompartment(sid);
    s.setInitialAmount(2.34);
    m.addSpecies(s);
    d.setLevelAndVersion(1,2);
    assertTrue( s.getInitialAmount() == 2.34 );
    d = null;
  }

  public void test_SBMLConvert_convertToL1_Species_Concentration()
  {
    SBMLDocument d = new  SBMLDocument(2,1);
    Model m = d.createModel();
    String sid = "C";
    Compartment c = new  Compartment();
    Species s = new  Species();
    c.setId(sid);
    c.setSize(1.2);
    m.addCompartment(c);
    s.setCompartment(sid);
    s.setInitialConcentration(2.34);
    m.addSpecies(s);
    d.setLevelAndVersion(1,2);
    assertTrue( m.getSpecies(0).getInitialAmount() == 2.808 );
    Species s1 = m.getSpecies(0);
    assertTrue( s1 != null );
    assertTrue(s1.getCompartment().equals( "C"));
    assertTrue( m.getCompartment( "C").getSize() == 1.2 );
    assertTrue( s1.getInitialConcentration() == 2.34 );
    assertTrue( s1.isSetInitialConcentration() == true );
    d = null;
  }

  public void test_SBMLConvert_convertToL2_SBMLDocument()
  {
    SBMLDocument d = new  SBMLDocument(1,2);
    d.setLevelAndVersion(2,1);
    assertTrue( d.getLevel() == 2 );
    assertTrue( d.getVersion() == 1 );
    d.setLevelAndVersion(2,2);
    assertTrue( d.getLevel() == 2 );
    assertTrue( d.getVersion() == 2 );
    d.setLevelAndVersion(2,3);
    assertTrue( d.getLevel() == 2 );
    assertTrue( d.getVersion() == 3 );
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
