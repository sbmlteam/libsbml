/*
 *
 * @file    TestRule.java
 * @brief   Rule unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestRule.c
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

public class TestRule {

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

  private Rule R;

  protected void setUp() throws Exception
  {
    R = new AlgebraicRule();
    if (R == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    R = null;
  }

  public void test_Rule_init()
  {
    assertTrue( R.getTypeCode() == libsbml.SBML_ALGEBRAIC_RULE );
    assertTrue( R.getMetaId().equals("") == true );
    assertTrue( R.getNotes() == null );
    assertTrue( R.getAnnotation() == null );
    assertTrue( R.getFormula().equals("") == true );
    assertTrue( R.getMath() == null );
  }

  public void test_Rule_setFormula()
  {
    String formula = "k1*X0";
    R.setFormula(formula);
    assertTrue(R.getFormula().equals(formula));
    assertTrue( R.isSetFormula() == true );
    if (R.getFormula() == formula);
    {
    }
    R.setFormula(R.getFormula());
    assertTrue(R.getFormula().equals(formula));
    R.setFormula( "");
    assertTrue( R.isSetFormula() == false );
    if (R.getFormula() != null);
    {
    }
  }

  public void test_Rule_setMath()
  {
    ASTNode math = libsbml.parseFormula("1 + 1");
    R.setMath(math);
    assertTrue( R.getMath() != math );
    assertEquals( true, R.isSetMath() );
    R.setMath(R.getMath());
    assertTrue( R.getMath() != math );
    R.setMath(null);
    assertEquals( false, R.isSetMath() );
    if (R.getMath() != null);
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
