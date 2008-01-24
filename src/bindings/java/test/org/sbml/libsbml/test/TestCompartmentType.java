/*
 *
 * @file    TestCompartmentType.java
 * @brief   CompartmentTypeType unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Sarah Keating 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestCompartmentType.c
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

public class TestCompartmentType {

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

  private CompartmentType CT;

  protected void setUp() throws Exception
  {
    CT = new  CompartmentType();
    if (CT == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    CT = null;
  }

  public void test_CompartmentType_create()
  {
    assertTrue( CT.getTypeCode() == libsbml.SBML_COMPARTMENT_TYPE );
    assertTrue( CT.getMetaId().equals("") == true );
    assertTrue( CT.getNotes() == null );
    assertTrue( CT.getAnnotation() == null );
    assertTrue( CT.getId().equals("") == true );
    assertTrue( CT.getName().equals("") == true );
    assertEquals( false, CT.isSetId() );
    assertEquals( false, CT.isSetName() );
  }

  public void test_CompartmentType_createWith()
  {
    CompartmentType c = new  CompartmentType("A", "");
    assertTrue( c.getTypeCode() == libsbml.SBML_COMPARTMENT_TYPE );
    assertTrue( c.getMetaId().equals("") == true );
    assertTrue( c.getNotes() == null );
    assertTrue( c.getAnnotation() == null );
    assertTrue( c.getName().equals("") == true );
    assertTrue(c.getId().equals( "A"     ));
    assertEquals( true, c.isSetId() );
    assertEquals( false, c.isSetName() );
    c = null;
  }

  public void test_CompartmentType_free_NULL()
  {
  }

  public void test_CompartmentType_setId()
  {
    String id = "mitochondria";
    CT.setId(id);
    assertTrue(CT.getId().equals(id));
    assertEquals( true, CT.isSetId() );
    if (CT.getId() == id);
    {
    }
    CT.setId(CT.getId());
    assertTrue(CT.getId().equals(id));
    CT.setId("");
    assertEquals( false, CT.isSetId() );
    if (CT.getId() != null);
    {
    }
  }

  public void test_CompartmentType_setName()
  {
    String name = "My Favorite Factory";
    CT.setName(name);
    assertTrue(CT.getName().equals(name));
    assertEquals( true, CT.isSetName() );
    if (CT.getName() == name);
    {
    }
    CT.setName(CT.getName());
    assertTrue(CT.getName().equals(name));
    CT.setName("");
    assertEquals( false, CT.isSetName() );
    if (CT.getName() != null);
    {
    }
  }

  public void test_CompartmentType_unsetName()
  {
    CT.setName( "name");
    assertTrue(CT.getName().equals( "name"     ));
    assertEquals( true, CT.isSetName() );
    CT.unsetName();
    assertEquals( false, CT.isSetName() );
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
