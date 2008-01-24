/*
 *
 * @file    TestKineticLaw.java
 * @brief   SBML KineticLaw unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestKineticLaw.c
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

public class TestKineticLaw {

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

  private KineticLaw KL;

  protected void setUp() throws Exception
  {
    KL = new  KineticLaw();
    if (KL == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    KL = null;
  }

  public void test_KineticLaw_addParameter()
  {
    Parameter p = new  Parameter();
    KL.addParameter(p);
    assertTrue( KL.getNumParameters() == 1 );
    p = null;
  }

  public void test_KineticLaw_create()
  {
    assertTrue( KL.getTypeCode() == libsbml.SBML_KINETIC_LAW );
    assertTrue( KL.getMetaId().equals("") == true );
    assertTrue( KL.getNotes() == null );
    assertTrue( KL.getAnnotation() == null );
    assertTrue( KL.getFormula().equals("") == true );
    assertTrue( KL.getMath() == null );
    assertTrue( KL.getTimeUnits().equals("") == true );
    assertTrue( KL.getSubstanceUnits().equals("") == true );
    assertEquals( false, KL.isSetFormula() );
    assertEquals( false, KL.isSetMath() );
    assertEquals( false, KL.isSetTimeUnits() );
    assertEquals( false, KL.isSetSubstanceUnits() );
    assertTrue( KL.getNumParameters() == 0 );
  }

  public void test_KineticLaw_createWith()
  {
    ASTNode math;
    String formula;
    KineticLaw kl = new  KineticLaw("k1 * X0");
    assertTrue( kl.getTypeCode() == libsbml.SBML_KINETIC_LAW );
    assertTrue( kl.getMetaId().equals("") == true );
    assertTrue( kl.getNotes() == null );
    assertTrue( kl.getAnnotation() == null );
    math = kl.getMath();
    assertTrue( math != null );
    formula = libsbml.formulaToString(math);
    assertTrue( formula != null );
    assertTrue(formula.equals( "k1 * X0"));
    assertTrue(kl.getFormula().equals(formula));
    assertEquals( true, kl.isSetMath() );
    assertEquals( true, kl.isSetFormula() );
    assertTrue( kl.getNumParameters() == 0 );
    kl = null;
  }

  public void test_KineticLaw_createWithMath()
  {
    ASTNode math1 = libsbml.parseFormula("k3 / k2");
    ASTNode math;
    String formula;
    KineticLaw kl = new  KineticLaw(math1);
    assertTrue( kl.getTypeCode() == libsbml.SBML_KINETIC_LAW );
    assertTrue( kl.getMetaId().equals("") == true );
    assertTrue( kl.getNotes() == null );
    assertTrue( kl.getAnnotation() == null );
    math = kl.getMath();
    assertTrue( math != null );
    formula = libsbml.formulaToString(math);
    assertTrue( formula != null );
    assertTrue(formula.equals( "k3 / k2"));
    assertTrue(kl.getFormula().equals(formula));
    assertEquals( true, kl.isSetMath() );
    assertEquals( true, kl.isSetFormula() );
    assertEquals( false, kl.isSetTimeUnits() );
    assertEquals( false, kl.isSetSubstanceUnits() );
    assertTrue( kl.getNumParameters() == 0 );
    kl = null;
  }

  public void test_KineticLaw_free_NULL()
  {
  }

  public void test_KineticLaw_getParameter()
  {
    Parameter k1 = new  Parameter();
    Parameter k2 = new  Parameter();
    k1.setName( "k1");
    k2.setName( "k2");
    k1.setValue(3.14);
    k2.setValue(2.72);
    KL.addParameter(k1);
    KL.addParameter(k2);
    k1 = null;
    k2 = null;
    assertTrue( KL.getNumParameters() == 2 );
    k1 = KL.getParameter(0);
    k2 = KL.getParameter(1);
    assertTrue(k1.getName().equals( "k1"));
    assertTrue(k2.getName().equals( "k2"));
    assertTrue( k1.getValue() == 3.14 );
    assertTrue( k2.getValue() == 2.72 );
  }

  public void test_KineticLaw_getParameterById()
  {
    Parameter k1 = new  Parameter();
    Parameter k2 = new  Parameter();
    k1.setId( "k1");
    k2.setId( "k2");
    k1.setValue(3.14);
    k2.setValue(2.72);
    KL.addParameter(k1);
    KL.addParameter(k2);
    k1 = null;
    k2 = null;
    assertTrue( KL.getNumParameters() == 2 );
    k1 = KL.getParameter( "k1");
    k2 = KL.getParameter( "k2");
    assertTrue(k1.getId().equals( "k1"));
    assertTrue(k2.getId().equals( "k2"));
    assertTrue( k1.getValue() == 3.14 );
    assertTrue( k2.getValue() == 2.72 );
  }

  public void test_KineticLaw_setFormula()
  {
    String formula = "k1*X0";
    KL.setFormula(formula);
    assertTrue(KL.getFormula().equals(formula));
    assertEquals( true, KL.isSetFormula() );
    if (KL.getFormula() == formula);
    {
    }
    KL.setFormula(KL.getFormula());
    assertTrue(KL.getFormula().equals(formula));
    KL.setFormula("");
    assertEquals( false, KL.isSetFormula() );
    if (KL.getFormula() != null);
    {
    }
  }

  public void test_KineticLaw_setFormulaFromMath()
  {
    ASTNode math = libsbml.parseFormula("k1 * X0");
    assertEquals( false, KL.isSetMath() );
    assertEquals( false, KL.isSetFormula() );
    KL.setMath(math);
    assertEquals( true, KL.isSetMath() );
    assertEquals( true, KL.isSetFormula() );
    assertTrue(KL.getFormula().equals( "k1 * X0"));
    math = null;
  }

  public void test_KineticLaw_setMath()
  {
    ASTNode math = libsbml.parseFormula("k3 / k2");
    String formula;
    ASTNode math1;
    KL.setMath(math);
    math1 = KL.getMath();
    assertTrue( math1 != null );
    formula = libsbml.formulaToString(math1);
    assertTrue( formula != null );
    assertTrue(formula.equals( "k3 / k2"));
    assertTrue( KL.getMath() != math );
    assertEquals( true, KL.isSetMath() );
    KL.setMath(KL.getMath());
    math1 = KL.getMath();
    assertTrue( math1 != null );
    formula = libsbml.formulaToString(math1);
    assertTrue( formula != null );
    assertTrue(formula.equals( "k3 / k2"));
    assertTrue( KL.getMath() != math );
    KL.setMath(null);
    assertEquals( false, KL.isSetMath() );
    if (KL.getMath() != null);
    {
    }
    math = null;
  }

  public void test_KineticLaw_setMathFromFormula()
  {
    String formula = "k3 / k2";
    assertEquals( false, KL.isSetMath() );
    assertEquals( false, KL.isSetFormula() );
    KL.setFormula(formula);
    assertEquals( true, KL.isSetMath() );
    assertEquals( true, KL.isSetFormula() );
    formula = libsbml.formulaToString(KL.getMath());
    assertTrue(formula.equals( "k3 / k2"));
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
