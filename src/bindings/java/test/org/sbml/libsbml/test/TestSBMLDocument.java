/*
 *
 * @file    TestSBMLDocument.java
 * @brief   SBMLDocument unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestSBMLDocument.c
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

public class TestSBMLDocument {

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


  public void test_SBMLDocument_create()
  {
    SBMLDocument d = new  SBMLDocument();
    assertTrue( d.getTypeCode() == libsbml.SBML_DOCUMENT );
    assertTrue( d.getNotes() == null );
    assertTrue( d.getAnnotation() == null );
    assertTrue( d.getLevel() == 2 );
    assertTrue( d.getVersion() == 3 );
    assertTrue( d.getNumErrors() == 0 );
    d = null;
  }

  public void test_SBMLDocument_createWith()
  {
    SBMLDocument d = new  SBMLDocument(1,2);
    assertTrue( d.getTypeCode() == libsbml.SBML_DOCUMENT );
    assertTrue( d.getNotes() == null );
    assertTrue( d.getAnnotation() == null );
    assertTrue( d.getLevel() == 1 );
    assertTrue( d.getVersion() == 2 );
    assertTrue( d.getNumErrors() == 0 );
    d = null;
  }

  public void test_SBMLDocument_free_NULL()
  {
  }

  public void test_SBMLDocument_setLevelAndVersion()
  {
    SBMLDocument d = new  SBMLDocument();
    d.setLevelAndVersion(2,2);
    Model m1 = new  Model();
    d.setModel(m1);
    assertTrue( d.setLevelAndVersion(2,3) == true );
    assertTrue( d.setLevelAndVersion(2,1) == true );
    assertTrue( d.setLevelAndVersion(1,2) == true );
    assertTrue( d.setLevelAndVersion(1,1) == false );
    d = null;
  }

  public void test_SBMLDocument_setLevelAndVersion_Error()
  {
    SBMLDocument d = new  SBMLDocument();
    d.setLevelAndVersion(2,1);
    Model m1 = new  Model();
    Unit u = new  Unit();
    u.setKind(libsbml.UnitKind_forName("mole"));
    u.setOffset(3.2);
    UnitDefinition ud = new  UnitDefinition();
    ud.addUnit(u);
    m1.addUnitDefinition(ud);
    d.setModel(m1);
    assertTrue( d.setLevelAndVersion(2,2) == false );
    assertTrue( d.setLevelAndVersion(2,3) == false );
    assertTrue( d.setLevelAndVersion(1,2) == false );
    assertTrue( d.setLevelAndVersion(1,1) == false );
    d = null;
  }

  public void test_SBMLDocument_setLevelAndVersion_Warning()
  {
    SBMLDocument d = new  SBMLDocument();
    d.setLevelAndVersion(2,2);
    Model m1 = new  Model();
    (m1).setSBOTerm(2);
    d.setModel(m1);
    assertTrue( d.setLevelAndVersion(2,3) == true );
    assertTrue( d.setLevelAndVersion(2,1) == true );
    assertTrue( d.setLevelAndVersion(1,2) == true );
    assertTrue( d.setLevelAndVersion(1,1) == false );
    d = null;
  }

  public void test_SBMLDocument_setModel()
  {
    SBMLDocument d = new  SBMLDocument();
    Model m1 = new  Model();
    Model m2 = new  Model();
    assertEquals(d.getModel(),null);
    d.setModel(m1);
    assertNotEquals(d.getModel(),m1);
    d.setModel(d.getModel());
    assertNotEquals(d.getModel(),m1);
    d.setModel(m2);
    assertNotEquals(d.getModel(),m2);
    d = null;
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
