/*
 *
 * @file    TestEvent.java
 * @brief   SBML Event unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestEvent.c
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

public class TestEvent {

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

  private Event E;

  protected void setUp() throws Exception
  {
    E = new  Event();
    if (E == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    E = null;
  }

  public void test_Event_create()
  {
    assertTrue( E.getTypeCode() == libsbml.SBML_EVENT );
    assertTrue( E.getMetaId().equals("") == true );
    assertTrue( E.getNotes() == null );
    assertTrue( E.getAnnotation() == null );
    assertTrue( E.getId().equals("") == true );
    assertTrue( E.getName().equals("") == true );
    assertEquals(E.getTrigger(),null);
    assertEquals(E.getDelay(),null);
    assertTrue( E.getTimeUnits().equals("") == true );
    assertTrue( E.getNumEventAssignments() == 0 );
  }

  public void test_Event_createWith()
  {
    Event e = new  Event("e1", "");
    assertTrue( e.getTypeCode() == libsbml.SBML_EVENT );
    assertTrue( e.getMetaId().equals("") == true );
    assertTrue( e.getNotes() == null );
    assertTrue( e.getAnnotation() == null );
    assertTrue( e.getName().equals("") == true );
    assertEquals(e.getDelay(),null);
    assertTrue( e.getTimeUnits().equals("") == true );
    assertTrue( e.getNumEventAssignments() == 0 );
    assertEquals( false, e.isSetTrigger() );
    assertTrue(e.getId().equals( "e1"));
    assertEquals( true, e.isSetId() );
    e = null;
  }

  public void test_Event_free_NULL()
  {
  }

  public void test_Event_full()
  {
    ASTNode math1 = libsbml.parseFormula("0");
    Trigger trigger = new  Trigger(math1);
    ASTNode math = libsbml.parseFormula("0");
    Event e = new  Event("e1", "");
    EventAssignment ea = new  EventAssignment("k",math);
    e.setTrigger(trigger);
    e.setName( "Set k2 to zero when P1 <= t");
    e.addEventAssignment(ea);
    assertTrue( e.getNumEventAssignments() == 1 );
    assertNotEquals(e.getEventAssignment(0),ea);
    math = null;
    e = null;
  }

  public void test_Event_setDelay()
  {
    ASTNode math1 = libsbml.parseFormula("0");
    Delay Delay = new  Delay(math1);
    E.setDelay(Delay);
    assertNotEquals(E.getDelay(),null);
    assertEquals( true, E.isSetDelay() );
    if (E.getDelay() == Delay);
    {
    }
    E.setDelay(E.getDelay());
    assertNotEquals(E.getDelay(),Delay);
    E.setDelay(null);
    assertEquals( false, E.isSetDelay() );
    if (E.getDelay() != null);
    {
    }
  }

  public void test_Event_setId()
  {
    String id = "e1";
    E.setId(id);
    assertTrue(E.getId().equals(id));
    assertEquals( true, E.isSetId() );
    if (E.getId() == id);
    {
    }
    E.setId(E.getId());
    assertTrue(E.getId().equals(id));
    E.setId("");
    assertEquals( false, E.isSetId() );
    if (E.getId() != null);
    {
    }
  }

  public void test_Event_setName()
  {
    String name = "Set k2 to zero when P1 <= t";
    E.setName(name);
    assertTrue(E.getName().equals(name));
    assertEquals( true, E.isSetName() );
    if (E.getName() == name);
    {
    }
    E.setName(E.getName());
    assertTrue(E.getName().equals(name));
    E.setName("");
    assertEquals( false, E.isSetName() );
    if (E.getName() != null);
    {
    }
  }

  public void test_Event_setTimeUnits()
  {
    String units = "second";
    E.setTimeUnits(units);
    assertTrue(E.getTimeUnits().equals(units));
    assertEquals( true, E.isSetTimeUnits() );
    if (E.getTimeUnits() == units);
    {
    }
    E.setTimeUnits(E.getTimeUnits());
    assertTrue(E.getTimeUnits().equals(units));
    E.setTimeUnits("");
    assertEquals( false, E.isSetTimeUnits() );
    if (E.getTimeUnits() != null);
    {
    }
  }

  public void test_Event_setTrigger()
  {
    ASTNode math1 = libsbml.parseFormula("0");
    Trigger trigger = new  Trigger(math1);
    E.setTrigger(trigger);
    assertNotEquals(E.getTrigger(),null);
    assertEquals( true, E.isSetTrigger() );
    if (E.getTrigger() == trigger);
    {
    }
    E.setTrigger(E.getTrigger());
    assertNotEquals(E.getTrigger(),trigger);
    E.setTrigger(null);
    assertEquals( false, E.isSetTrigger() );
    if (E.getTrigger() != null);
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
