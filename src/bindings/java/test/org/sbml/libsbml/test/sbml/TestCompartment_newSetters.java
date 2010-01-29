/*
 *
 * @file    TestCompartment_newSetters.java
 * @brief   Compartment unit tests for new set function API
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Sarah Keating 
 *
 * $Id$
 * $HeadURL$
 *
 * This test file was converted from src/sbml/test/TestCompartment_newSetters.c
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

public class TestCompartment_newSetters {

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
  private Compartment C;

  protected void setUp() throws Exception
  {
    C = new  Compartment(1,2);
    if (C == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    C = null;
  }

  public void test_Compartment_setCompartmentType1()
  {
    long i = C.setCompartmentType( "cell");
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    assertEquals( false, C.isSetCompartmentType() );
    i = C.unsetCompartmentType();
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    assertEquals( false, C.isSetCompartmentType() );
  }

  public void test_Compartment_setCompartmentType2()
  {
    Compartment c = new  Compartment(2,2);
    long i = c.setCompartmentType( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, c.isSetCompartmentType() );
    i = c.unsetCompartmentType();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetCompartmentType() );
    c = null;
  }

  public void test_Compartment_setCompartmentType3()
  {
    Compartment c = new  Compartment(2,2);
    long i = c.setCompartmentType( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, c.isSetCompartmentType() );
    assertTrue(c.getCompartmentType().equals( "cell" ));
    i = c.unsetCompartmentType();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetCompartmentType() );
    c = null;
  }

  public void test_Compartment_setCompartmentType4()
  {
    Compartment c = new  Compartment(2,2);
    long i = c.setCompartmentType("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetCompartmentType() );
    c = null;
  }

  public void test_Compartment_setConstant1()
  {
    long i = C.setConstant(false);
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    assertTrue( C.getConstant() == false );
  }

  public void test_Compartment_setConstant2()
  {
    Compartment c = new  Compartment(2,2);
    long i = c.setConstant(false);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( c.getConstant() == false );
    c = null;
  }

  public void test_Compartment_setId2()
  {
    Compartment c = new  Compartment(2,2);
    long i = c.setId( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, c.isSetId() );
    c = null;
  }

  public void test_Compartment_setId3()
  {
    Compartment c = new  Compartment(2,2);
    long i = c.setId( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, c.isSetId() );
    assertTrue(c.getId().equals( "cell" ));
    c = null;
  }

  public void test_Compartment_setId4()
  {
    Compartment c = new  Compartment(2,2);
    long i = c.setId("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetId() );
    c = null;
  }

  public void test_Compartment_setName1()
  {
    long i = C.setName( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, C.isSetName() );
    i = C.unsetName();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetName() );
  }

  public void test_Compartment_setName2()
  {
    Compartment c = new  Compartment(1,2);
    long i = c.setName( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, c.isSetName() );
    i = c.unsetName();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetName() );
    c = null;
  }

  public void test_Compartment_setName3()
  {
    long i = C.setName("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetName() );
  }

  public void test_Compartment_setOutside1()
  {
    long i = C.setOutside( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, C.isSetOutside() );
    i = C.unsetOutside();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetOutside() );
  }

  public void test_Compartment_setOutside2()
  {
    long i = C.setOutside( "litre");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, C.isSetOutside() );
    i = C.unsetOutside();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetOutside() );
  }

  public void test_Compartment_setOutside3()
  {
    long i = C.setOutside("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetOutside() );
  }

  public void test_Compartment_setSize1()
  {
    long i = C.setSize(2.0);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( C.getSize() == 2.0 );
    i = C.unsetSize();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
  }

  public void test_Compartment_setSize2()
  {
    Compartment c = new  Compartment(2,2);
    long i = c.setSize(4);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( c.getSize() == 4 );
    assertEquals( true, c.isSetSize() );
    i = c.unsetSize();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetSize() );
    c = null;
  }

  public void test_Compartment_setSpatialDimensions1()
  {
    long i = C.setSpatialDimensions(2);
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    assertTrue( C.getSpatialDimensions() == 3 );
  }

  public void test_Compartment_setSpatialDimensions2()
  {
    Compartment c = new  Compartment(2,2);
    long i = c.setSpatialDimensions(4);
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertTrue( c.getSpatialDimensions() == 3 );
    c = null;
  }

  public void test_Compartment_setSpatialDimensions3()
  {
    Compartment c = new  Compartment(2,2);
    long i = c.setSpatialDimensions(2);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( c.getSpatialDimensions() == 2 );
    c = null;
  }

  public void test_Compartment_setUnits1()
  {
    long i = C.setUnits( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, C.isSetUnits() );
    i = C.unsetUnits();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetUnits() );
  }

  public void test_Compartment_setUnits2()
  {
    long i = C.setUnits( "litre");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, C.isSetUnits() );
    i = C.unsetUnits();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetUnits() );
  }

  public void test_Compartment_setUnits3()
  {
    long i = C.setUnits("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetUnits() );
  }

  public void test_Compartment_setVolume1()
  {
    long i = C.setVolume(2.0);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( C.getVolume() == 2.0 );
    assertEquals( true, C.isSetVolume() );
    i = C.unsetVolume();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( C.getVolume() == 1.0 );
    assertEquals( true, C.isSetVolume() );
  }

  public void test_Compartment_setVolume2()
  {
    Compartment c = new  Compartment(2,2);
    long i = c.setVolume(4);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( c.getVolume() == 4.0 );
    assertEquals( true, c.isSetVolume() );
    i = c.unsetVolume();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetVolume() );
    c = null;
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
