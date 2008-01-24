/*
 *
 * @file    TestParameterRule.java
 * @brief   ParameterRule unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestParameterRule.c
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

public class TestParameterRule {

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

  private Rule PR;

  protected void setUp() throws Exception
  {
    PR = new AssignmentRule();
    PR.setL1TypeCode(libsbml.SBML_PARAMETER_RULE);
    if (PR == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    PR = null;
  }

  public void test_ParameterRule_create()
  {
    assertTrue( PR.getTypeCode() == libsbml.SBML_ASSIGNMENT_RULE );
    assertTrue( PR.getL1TypeCode() == libsbml.SBML_PARAMETER_RULE );
    assertTrue( PR.getNotes() == null );
    assertTrue( PR.getAnnotation() == null );
    assertTrue( PR.getFormula().equals("") == true );
    assertTrue( PR.getUnits().equals("") == true );
    assertTrue( PR.getVariable().equals("") == true );
    assertTrue( PR.getType() == libsbml.RULE_TYPE_SCALAR );
    assertEquals( false, PR.isSetVariable() );
    assertEquals( false, PR.isSetUnits() );
  }

  public void test_ParameterRule_createWith()
  {
    Rule pr;
    pr = new RateRule("c", "v + 1");
    pr.setL1TypeCode(libsbml.SBML_PARAMETER_RULE);
    assertTrue( pr.getTypeCode() == libsbml.SBML_RATE_RULE );
    assertTrue( pr.getL1TypeCode() == libsbml.SBML_PARAMETER_RULE );
    assertTrue( pr.getNotes() == null );
    assertTrue( pr.getAnnotation() == null );
    assertTrue( pr.getUnits().equals("") == true );
    assertTrue(pr.getFormula().equals( "v + 1"));
    assertTrue(pr.getVariable().equals( "c"));
    assertTrue( pr.getType() == libsbml.RULE_TYPE_RATE );
    assertEquals( true, pr.isSetVariable() );
    assertEquals( false, pr.isSetUnits() );
    pr = null;
  }

  public void test_ParameterRule_free_NULL()
  {
  }

  public void test_ParameterRule_setName()
  {
    String name = "cell";
    String c;
    PR.setVariable(name);
    assertTrue(PR.getVariable().equals(name));
    assertEquals( true, PR.isSetVariable() );
    if (PR.getVariable() == name);
    {
    }
    c = PR.getVariable();
    PR.setVariable(c);
    assertTrue(PR.getVariable().equals(name));
    PR.setVariable("");
    assertEquals( false, PR.isSetVariable() );
    if (PR.getVariable() != null);
    {
    }
  }

  public void test_ParameterRule_setUnits()
  {
    String units = "cell";
    PR.setUnits(units);
    assertTrue(PR.getUnits().equals(units));
    assertEquals( true, PR.isSetUnits() );
    if (PR.getUnits() == units);
    {
    }
    PR.setUnits(PR.getUnits());
    assertTrue(PR.getUnits().equals(units));
    PR.setUnits("");
    assertEquals( false, PR.isSetUnits() );
    if (PR.getUnits() != null);
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
