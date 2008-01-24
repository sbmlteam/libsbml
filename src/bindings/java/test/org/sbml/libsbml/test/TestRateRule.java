/*
 *
 * @file    TestRateRule.java
 * @brief   RateRule unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestRateRule.c
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

public class TestRateRule {

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

  private Rule RR;

  protected void setUp() throws Exception
  {
    RR = new RateRule();
    if (RR == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    RR = null;
  }

  public void test_RateRule_create()
  {
    assertTrue( RR.getTypeCode() == libsbml.SBML_RATE_RULE );
    assertTrue( RR.getMetaId().equals("") == true );
    assertTrue( RR.getNotes() == null );
    assertTrue( RR.getAnnotation() == null );
    assertTrue( RR.getFormula().equals("") == true );
    assertTrue( RR.getMath() == null );
    assertTrue( RR.getVariable().equals("") == true );
    assertTrue( RR.getType() == libsbml.RULE_TYPE_RATE );
  }

  public void test_RateRule_createWithFormula()
  {
    ASTNode math;
    String formula;
    Rule ar = new RateRule("s", "1 + 1");
    assertTrue( ar.getTypeCode() == libsbml.SBML_RATE_RULE );
    assertTrue( ar.getMetaId().equals("") == true );
    assertTrue(ar.getVariable().equals( "s"));
    math = ar.getMath();
    assertTrue( math != null );
    formula = libsbml.formulaToString(math);
    assertTrue( formula != null );
    assertTrue(formula.equals( "1 + 1"));
    assertTrue(ar.getFormula().equals(formula));
    ar = null;
  }

  public void test_RateRule_createWithMath()
  {
    ASTNode math = libsbml.parseFormula("1 + 1");
    Rule ar = new RateRule("s",math);
    assertTrue( ar.getTypeCode() == libsbml.SBML_RATE_RULE );
    assertTrue( ar.getMetaId().equals("") == true );
    assertTrue(ar.getVariable().equals( "s"));
    assertTrue(ar.getFormula().equals( "1 + 1"));
    assertTrue( ar.getMath() != math );
    ar = null;
  }

  public void test_RateRule_free_NULL()
  {
  }

  public void test_RateRule_setVariable()
  {
    String variable = "x";
    RR.setVariable(variable);
    assertTrue(RR.getVariable().equals(variable));
    assertEquals( true, RR.isSetVariable() );
    if (RR.getVariable() == variable);
    {
    }
    RR.setVariable(RR.getVariable());
    assertTrue(RR.getVariable().equals(variable));
    RR.setVariable("");
    assertEquals( false, RR.isSetVariable() );
    if (RR.getVariable() != null);
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
