/*
 *
 * @file    TestSpeciesConcentrationRule.java
 * @brief   SpeciesConcentrationRule unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestSpeciesConcentrationRule.c
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

public class TestSpeciesConcentrationRule {

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

  private Rule SCR;

  protected void setUp() throws Exception
  {
    SCR = new AssignmentRule();
    SCR.setL1TypeCode(libsbml.SBML_SPECIES_CONCENTRATION_RULE);
    if (SCR == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    SCR = null;
  }

  public void test_SpeciesConcentrationRule_create()
  {
    assertTrue( SCR.getTypeCode() == libsbml.SBML_ASSIGNMENT_RULE );
    assertTrue( SCR.getL1TypeCode() == libsbml.SBML_SPECIES_CONCENTRATION_RULE );
    assertTrue( SCR.getNotes() == null );
    assertTrue( SCR.getAnnotation() == null );
    assertTrue( SCR.getFormula().equals("") == true );
    assertTrue( SCR.getType() == libsbml.RULE_TYPE_SCALAR );
    assertTrue( SCR.getVariable().equals("") == true );
    assertEquals( false, SCR.isSetVariable() );
  }

  public void test_SpeciesConcentrationRule_createWith()
  {
    Rule scr;
    scr = new RateRule("c", "v + 1");
    scr.setL1TypeCode(libsbml.SBML_SPECIES_CONCENTRATION_RULE);
    assertTrue( scr.getTypeCode() == libsbml.SBML_RATE_RULE );
    assertTrue( scr.getL1TypeCode() == libsbml.SBML_SPECIES_CONCENTRATION_RULE );
    assertTrue( scr.getNotes() == null );
    assertTrue( scr.getAnnotation() == null );
    assertTrue(scr.getFormula().equals( "v + 1"));
    assertTrue(scr.getVariable().equals( "c"));
    assertTrue( scr.getType() == libsbml.RULE_TYPE_RATE );
    assertEquals( true, scr.isSetVariable() );
    scr = null;
  }

  public void test_SpeciesConcentrationRule_free_NULL()
  {
  }

  public void test_SpeciesConcentrationRule_setSpecies()
  {
    String species = "s2";
    String s;
    SCR.setVariable(species);
    assertTrue(SCR.getVariable().equals(species));
    assertEquals( true, SCR.isSetVariable() );
    if (SCR.getVariable() == species);
    {
    }
    s = SCR.getVariable();
    SCR.setVariable(s);
    assertTrue(SCR.getVariable().equals(species));
    SCR.setVariable("");
    assertEquals( false, SCR.isSetVariable() );
    if (SCR.getVariable() != null);
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
