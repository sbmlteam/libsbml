/*
 * @file    TestSpeciesReference_newSetters.java
 * @brief   SpeciesReference unit tests for new set function API
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Sarah Keating 
 * 
 * ====== WARNING ===== WARNING ===== WARNING ===== WARNING ===== WARNING ======
 *
 * DO NOT EDIT THIS FILE.
 *
 * This file was generated automatically by converting the file located at
 * src/sbml/test/TestSpeciesReference_newSetters.c
 * using the conversion program dev/utilities/translateTests/translateTests.pl.
 * Any changes made here will be lost the next time the file is regenerated.
 *
 * -----------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
 * -----------------------------------------------------------------------------
 */

package org.sbml.libsbml.test.sbml;

import org.sbml.libsbml.*;

import java.io.File;
import java.lang.AssertionError;

public class TestSpeciesReference_newSetters {

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
    else if ( (a == null) || (b == null) )
    {
      throw new AssertionError();
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
    else if ( (a == null) || (b == null) )
    {
      return;
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
  private SpeciesReference sr;

  protected void setUp() throws Exception
  {
    sr = new  SpeciesReference(2,4);
    if (sr == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    sr = null;
  }

  public void test_SpeciesReference_setDenominator1()
  {
    int i = sr.setDenominator(2);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( sr.getDenominator() == 2 );
  }

  public void test_SpeciesReference_setDenominator2()
  {
    SpeciesReference c = new  SpeciesReference(2,2);
    int i = c.setDenominator(4);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( c.getDenominator() == 4 );
    c = null;
  }

  public void test_SpeciesReference_setId1()
  {
    int i = sr.setId( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, sr.isSetId() );
    assertTrue(sr.getId().equals( "cell" ));
  }

  public void test_SpeciesReference_setId2()
  {
    int i = sr.setId( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, sr.isSetId() );
  }

  public void test_SpeciesReference_setId3()
  {
    SpeciesReference c = new  SpeciesReference(2,1);
    int i = c.setId( "cell");
    c = null;
  }

  public void test_SpeciesReference_setId4()
  {
    int i = sr.setId( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, sr.isSetId() );
    assertTrue(sr.getId().equals( "cell" ));
    i = sr.setId("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, sr.isSetId() );
  }

  public void test_SpeciesReference_setName1()
  {
    int i = sr.setName( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, sr.isSetName() );
    i = sr.unsetName();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, sr.isSetName() );
  }

  public void test_SpeciesReference_setName2()
  {
    int i = sr.setName( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, sr.isSetName() );
    i = sr.unsetName();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, sr.isSetName() );
  }

  public void test_SpeciesReference_setName3()
  {
    SpeciesReference c = new  SpeciesReference(2,1);
    int i = c.setName( "cell");
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    assertEquals( false, c.isSetName() );
    c = null;
  }

  public void test_SpeciesReference_setName4()
  {
    int i = sr.setName( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, sr.isSetName() );
    i = sr.setName("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, sr.isSetName() );
  }

  public void test_SpeciesReference_setSpecies1()
  {
    int i = sr.setSpecies( "mm");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, sr.isSetSpecies() );
  }

  public void test_SpeciesReference_setSpecies2()
  {
    SpeciesReference c = new  SpeciesReference(2,2);
    int i = c.setSpecies( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, c.isSetSpecies() );
    c = null;
  }

  public void test_SpeciesReference_setSpecies3()
  {
    SpeciesReference c = new  SpeciesReference(2,2);
    int i = c.setSpecies( "mole");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue(c.getSpecies().equals( "mole"));
    assertEquals( true, c.isSetSpecies() );
    c = null;
  }

  public void test_SpeciesReference_setSpecies4()
  {
    int i = sr.setSpecies( "mm");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, sr.isSetSpecies() );
    i = sr.setSpecies("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, sr.isSetSpecies() );
  }

  public void test_SpeciesReference_setStoichiometry1()
  {
    int i = sr.setStoichiometry(2.0);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( sr.getStoichiometry() == 2.0 );
  }

  public void test_SpeciesReference_setStoichiometry2()
  {
    SpeciesReference c = new  SpeciesReference(2,2);
    int i = c.setStoichiometry(4);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( c.getStoichiometry() == 4.0 );
    c = null;
  }

  public void test_SpeciesReference_setStoichiometryMath1()
  {
    StoichiometryMath sm = new  StoichiometryMath(2,4);
    ASTNode math = new  ASTNode(libsbml.AST_TIMES);
    ASTNode a = new  ASTNode();
    ASTNode b = new  ASTNode();
    a.setName( "a");
    b.setName( "b");
    math.addChild(a);
    math.addChild(b);
    sm.setMath(math);
    int i = sr.setStoichiometryMath(sm);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, sr.isSetStoichiometryMath() );
    assertTrue( sr.getStoichiometry() == 1 );
    i = sr.unsetStoichiometryMath();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, sr.isSetStoichiometryMath() );
    sm = null;
  }

  public void test_SpeciesReference_setStoichiometryMath2()
  {
    StoichiometryMath sm = new  StoichiometryMath(2,4);
    ASTNode math = new  ASTNode(libsbml.AST_TIMES);
    ASTNode a = new  ASTNode();
    a.setName( "a");
    math.addChild(a);
    sm.setMath(math);
    int i = sr.setStoichiometryMath(sm);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, sr.isSetStoichiometryMath() );
    sm = null;
  }

  public void test_SpeciesReference_setStoichiometryMath3()
  {
    int i = sr.setStoichiometryMath(null);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, sr.isSetStoichiometryMath() );
    i = sr.unsetStoichiometryMath();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, sr.isSetStoichiometryMath() );
  }

  public void test_SpeciesReference_setStoichiometryMath4()
  {
    StoichiometryMath sm = new  StoichiometryMath(2,4);
    ASTNode math = null;
    sm.setMath(math);
    int i = sr.setStoichiometryMath(sm);
      assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
      assertEquals( false, sr.isSetStoichiometryMath() );
    assertTrue( sr.getStoichiometry() == 1 );
    i = sr.unsetStoichiometryMath();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, sr.isSetStoichiometryMath() );
    sm = null;
  }

  public void test_SpeciesReference_setStoichiometryMath5()
  {
    SpeciesReference sr1 = new  SpeciesReference(1,2);
    StoichiometryMath sm = new  StoichiometryMath(2,4);
    ASTNode math = new  ASTNode(libsbml.AST_TIMES);
    ASTNode a = new  ASTNode();
    ASTNode b = new  ASTNode();
    a.setName( "a");
    b.setName( "b");
    math.addChild(a);
    math.addChild(b);
    sm.setMath(math);
    int i = sr1.setStoichiometryMath(sm);
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    assertEquals( false, sr1.isSetStoichiometryMath() );
    sm = null;
    sr1 = null;
  }

  public void test_SpeciesReference_setStoichiometryMath6()
  {
    StoichiometryMath sm = new  StoichiometryMath(2,1);
      sm.setMath(libsbml.parseFormula("1"));
    int i = sr.setStoichiometryMath(sm);
    assertTrue( i == libsbml.LIBSBML_VERSION_MISMATCH );
    assertEquals( false, sr.isSetStoichiometryMath() );
    sm = null;
  }

  public void test_SpeciesReference_setStoichiometryMath7()
  {
    SpeciesReference sr1 = new  SpeciesReference(1,2);
    int i = sr1.unsetStoichiometryMath();
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    sr1 = null;
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
