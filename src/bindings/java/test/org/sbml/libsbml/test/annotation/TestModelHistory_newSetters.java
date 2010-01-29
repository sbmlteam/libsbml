/*
 *
 * @file    TestModelHistory_newSetters.java
 * @brief   ModelHistory unit tests
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Sarah Keating 
 *
 * $Id$
 * $HeadURL$
 *
 * This test file was converted from src/sbml/test/TestModelHistory_newSetters.c
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


package org.sbml.libsbml.test.annotation;

import org.sbml.libsbml.*;

import java.io.File;
import java.lang.AssertionError;

public class TestModelHistory_newSetters {

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

  public void test_ModelHistory_addCreator1()
  {
    ModelHistory mh = new  ModelHistory();
    ModelCreator mc = new  ModelCreator();
    mc.setFamilyName( "Keating");
    mc.setGivenName( "Sarah");
    long i = mh.addCreator(mc);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( mh.getNumCreators() == 1 );
    mc = null;
    mh = null;
  }

  public void test_ModelHistory_addCreator2()
  {
    ModelHistory mh = new  ModelHistory();
    ModelCreator mc = new  ModelCreator();
    mc.setGivenName( "Sarah");
    long i = mh.addCreator(mc);
    assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
    assertTrue( mh.getNumCreators() == 0 );
    mc = null;
    mh = null;
  }

  public void test_ModelHistory_addCreator3()
  {
    ModelHistory mh = new  ModelHistory();
    ModelCreator mc = null;
    long i = mh.addCreator(mc);
    assertTrue( i == libsbml.LIBSBML_OPERATION_FAILED );
    assertTrue( mh.getNumCreators() == 0 );
    mh = null;
  }

  public void test_ModelHistory_setCreatedDate1()
  {
    ModelHistory mh = new  ModelHistory();
    assertTrue( mh != null );
    Date date = new  Date("2005-12-30T12:15:32+02:00");
    long i = mh.setCreatedDate(date);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( mh.isSetCreatedDate() == true );
    assertTrue( !date.equals(mh.getCreatedDate()) );
    String dateChar = mh.getCreatedDate().getDateAsString();
    assertTrue(dateChar.equals( "2005-12-30T12:15:32+02:00"));
    i = mh.setCreatedDate(null);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( mh.isSetCreatedDate() == false );
    date = null;
    mh = null;
  }

  public void test_ModelHistory_setCreatedDate2()
  {
    ModelHistory mh = new  ModelHistory();
    assertTrue( mh != null );
    Date date = new  Date("Jan 12");
    long i = mh.setCreatedDate(date);
    assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
    assertTrue( mh.isSetCreatedDate() == false );
    date = null;
    mh = null;
  }

  public void test_ModelHistory_setModifiedDate1()
  {
    ModelHistory mh = new  ModelHistory();
    assertTrue( mh != null );
    Date date = new  Date("2005-12-30T12:15:32+02:00");
    long i = mh.setModifiedDate(date);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( mh.isSetModifiedDate() == true );
    assertTrue( !date.equals(mh.getModifiedDate()) );
    String dateChar = mh.getModifiedDate().getDateAsString();
    assertTrue(dateChar.equals( "2005-12-30T12:15:32+02:00"));
    i = mh.setModifiedDate(null);
    assertTrue( i == libsbml.LIBSBML_OPERATION_FAILED );
    assertTrue( mh.isSetModifiedDate() == true );
    date = null;
    mh = null;
  }

  public void test_ModelHistory_setModifiedDate2()
  {
    ModelHistory mh = new  ModelHistory();
    assertTrue( mh != null );
    Date date = new  Date(200,13,76,56,89,90,0,0,0);
    long i = mh.setModifiedDate(date);
    assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
    assertTrue( mh.isSetModifiedDate() == false );
    date = null;
    mh = null;
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
