/*
 *
 * @file    TestReaction.java
 * @brief   SBML Reaction unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestReaction.c
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

public class TestReaction {

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

  private Reaction R;

  protected void setUp() throws Exception
  {
    R = new  Reaction();
    if (R == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    R = null;
  }

  public void test_Reaction_addModifier()
  {
    R.addModifier(new ModifierSpeciesReference());
    assertTrue( R.getNumReactants() == 0 );
    assertTrue( R.getNumProducts() == 0 );
    assertTrue( R.getNumModifiers() == 1 );
  }

  public void test_Reaction_addProduct()
  {
    SpeciesReference sr = new  SpeciesReference();
    R.addProduct(sr);
    assertTrue( R.getNumReactants() == 0 );
    assertTrue( R.getNumProducts() == 1 );
    assertTrue( R.getNumModifiers() == 0 );
    sr = null;
  }

  public void test_Reaction_addReactant()
  {
    SpeciesReference sr = new  SpeciesReference();
    R.addReactant(sr);
    assertTrue( R.getNumReactants() == 1 );
    assertTrue( R.getNumProducts() == 0 );
    assertTrue( R.getNumModifiers() == 0 );
    sr = null;
  }

  public void test_Reaction_create()
  {
    assertTrue( R.getTypeCode() == libsbml.SBML_REACTION );
    assertTrue( R.getMetaId().equals("") == true );
    assertTrue( R.getNotes() == null );
    assertTrue( R.getAnnotation() == null );
    assertTrue( R.getId().equals("") == true );
    assertTrue( R.getName().equals("") == true );
    assertEquals(R.getKineticLaw(),null);
    assertTrue( R.getReversible() != false );
    assertTrue( R.getFast() == false );
    assertEquals( false, R.isSetId() );
    assertEquals( false, R.isSetName() );
    assertEquals( false, R.isSetKineticLaw() );
    assertTrue( R.getNumReactants() == 0 );
    assertTrue( R.getNumProducts() == 0 );
    assertTrue( R.getNumModifiers() == 0 );
  }

  public void test_Reaction_createWith()
  {
    KineticLaw kl = new  KineticLaw();
    Reaction r = new  Reaction("r1", "",kl,false);
    r.setFast(true);
    assertTrue( r.getTypeCode() == libsbml.SBML_REACTION );
    assertTrue( r.getMetaId().equals("") == true );
    assertTrue( r.getNotes() == null );
    assertTrue( r.getAnnotation() == null );
    assertTrue( r.getName().equals("") == true );
    assertTrue(r.getId().equals( "r1"));
    assertTrue( r.getReversible() == false );
    assertTrue( r.getFast() == true );
    assertEquals( true, r.isSetId() );
    assertEquals( false, r.isSetName() );
    assertEquals( true, r.isSetKineticLaw() );
    assertTrue( r.getNumReactants() == 0 );
    assertTrue( r.getNumProducts() == 0 );
    assertTrue( r.getNumModifiers() == 0 );
    kl = null;
    r = null;
  }

  public void test_Reaction_free_NULL()
  {
  }

  public void test_Reaction_getModifier()
  {
    ModifierSpeciesReference msr1 = new ModifierSpeciesReference();
    ModifierSpeciesReference msr2 = new ModifierSpeciesReference();
    msr1.setSpecies( "M1");
    msr2.setSpecies( "M2");
    R.addModifier(msr1);
    R.addModifier(msr2);
    msr1 = null;
    msr2 = null;
    assertTrue( R.getNumReactants() == 0 );
    assertTrue( R.getNumProducts() == 0 );
    assertTrue( R.getNumModifiers() == 2 );
    msr1 = R.getModifier(0);
    msr2 = R.getModifier(1);
    assertTrue(msr1.getSpecies().equals( "M1"));
    assertTrue(msr2.getSpecies().equals( "M2"));
  }

  public void test_Reaction_getModifierById()
  {
    ModifierSpeciesReference msr1 = new ModifierSpeciesReference();
    ModifierSpeciesReference msr2 = new ModifierSpeciesReference();
    msr1.setSpecies( "M1");
    msr2.setSpecies( "M2");
    R.addModifier(msr1);
    R.addModifier(msr2);
    assertTrue( R.getNumReactants() == 0 );
    assertTrue( R.getNumProducts() == 0 );
    assertTrue( R.getNumModifiers() == 2 );
    assertNotEquals(R.getModifier( "M1"),msr1);
    assertNotEquals(R.getModifier( "M2"),msr2);
    assertEquals(R.getModifier( "M3"),null);
    msr1 = null;
    msr2 = null;
  }

  public void test_Reaction_getProduct()
  {
    SpeciesReference sr1 = new  SpeciesReference();
    SpeciesReference sr2 = new  SpeciesReference();
    sr1.setSpecies( "P1");
    sr2.setSpecies( "P2");
    R.addProduct(sr1);
    R.addProduct(sr2);
    sr1 = null;
    sr2 = null;
    assertTrue( R.getNumReactants() == 0 );
    assertTrue( R.getNumProducts() == 2 );
    assertTrue( R.getNumModifiers() == 0 );
    sr1 = R.getProduct(0);
    sr2 = R.getProduct(1);
    assertTrue(sr1.getSpecies().equals( "P1"));
    assertTrue(sr2.getSpecies().equals( "P2"));
  }

  public void test_Reaction_getProductById()
  {
    SpeciesReference sr1 = new  SpeciesReference("P1",1,1);
    SpeciesReference sr2 = new  SpeciesReference("P2",1,1);
    R.addProduct(sr1);
    R.addProduct(sr2);
    assertTrue( R.getNumReactants() == 0 );
    assertTrue( R.getNumProducts() == 2 );
    assertTrue( R.getNumModifiers() == 0 );
    assertNotEquals(R.getProduct( "P1"),sr1);
    assertNotEquals(R.getProduct( "P2"),sr2);
    assertEquals(R.getProduct( "P3"),null);
    sr1 = null;
    sr2 = null;
  }

  public void test_Reaction_getReactant()
  {
    SpeciesReference sr1 = new  SpeciesReference();
    SpeciesReference sr2 = new  SpeciesReference();
    sr1.setSpecies( "R1");
    sr2.setSpecies( "R2");
    R.addReactant(sr1);
    R.addReactant(sr2);
    sr1 = null;
    sr2 = null;
    assertTrue( R.getNumReactants() == 2 );
    assertTrue( R.getNumProducts() == 0 );
    assertTrue( R.getNumModifiers() == 0 );
    sr1 = R.getReactant(0);
    sr2 = R.getReactant(1);
    assertTrue(sr1.getSpecies().equals( "R1"));
    assertTrue(sr2.getSpecies().equals( "R2"));
  }

  public void test_Reaction_getReactantById()
  {
    SpeciesReference sr1 = new  SpeciesReference("R1",1,1);
    SpeciesReference sr2 = new  SpeciesReference("R2",1,1);
    R.addReactant(sr1);
    R.addReactant(sr2);
    assertTrue( R.getNumReactants() == 2 );
    assertTrue( R.getNumProducts() == 0 );
    assertTrue( R.getNumModifiers() == 0 );
    assertNotEquals(R.getReactant( "R1"),sr1);
    assertNotEquals(R.getReactant( "R2"),sr2);
    assertEquals(R.getReactant( "R3"),null);
    sr1 = null;
    sr2 = null;
  }

  public void test_Reaction_setId()
  {
    String id = "J1";
    R.setId(id);
    assertTrue(R.getId().equals(id));
    assertEquals( true, R.isSetId() );
    if (R.getId() == id);
    {
    }
    R.setId(R.getId());
    assertTrue(R.getId().equals(id));
    R.setId("");
    assertEquals( false, R.isSetId() );
    if (R.getId() != null);
    {
    }
  }

  public void test_Reaction_setName()
  {
    String name = "MapK Cascade";
    R.setName(name);
    assertTrue(R.getName().equals(name));
    assertEquals( true, R.isSetName() );
    if (R.getName() == name);
    {
    }
    R.setName(R.getName());
    assertTrue(R.getName().equals(name));
    R.setName("");
    assertEquals( false, R.isSetName() );
    if (R.getName() != null);
    {
    }
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
