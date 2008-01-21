/*
 *
 * @file    TestSpecies.java
 * @brief   Species unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestSpecies.c
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

public class TestSpecies {

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

  private Species S;

  protected void setUp() throws Exception
  {
    S = new  Species();
    if (S == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    S = null;
  }

  public void test_Species_create()
  {
    assertTrue( S.getTypeCode() == libsbml.SBML_SPECIES );
    assertTrue( S.getMetaId().equals("") == true );
    assertTrue( S.getNotes() == null );
    assertTrue( S.getAnnotation() == null );
    assertTrue( S.getId().equals("") == true );
    assertTrue( S.getName().equals("") == true );
    assertEquals(S.getCompartment(),"");
    assertTrue( S.getInitialAmount() == 0.0 );
    assertTrue( S.getInitialConcentration() == 0.0 );
    assertTrue( S.getSubstanceUnits().equals("") == true );
    assertTrue( S.getSpatialSizeUnits().equals("") == true );
    assertTrue( S.getHasOnlySubstanceUnits() == false );
    assertTrue( S.getBoundaryCondition() == false );
    assertTrue( S.getCharge() == 0 );
    assertTrue( S.getConstant() == false );
    assertEquals( false, S.isSetId() );
    assertEquals( false, S.isSetName() );
    assertEquals( false, S.isSetCompartment() );
    assertEquals( false, S.isSetInitialAmount() );
    assertEquals( false, S.isSetInitialConcentration() );
    assertEquals( false, S.isSetSubstanceUnits() );
    assertEquals( false, S.isSetSpatialSizeUnits() );
    assertEquals( false, S.isSetUnits() );
    assertEquals( false, S.isSetCharge() );
  }

  public void test_Species_createWith()
  {
    Species s = new  Species("Ca", "Calcium");
    assertTrue( s.getTypeCode() == libsbml.SBML_SPECIES );
    assertTrue( s.getMetaId().equals("") == true );
    assertTrue( s.getNotes() == null );
    assertTrue( s.getAnnotation() == null );
    assertTrue(s.getName().equals( "Calcium"  ));
    assertTrue( s.getSpatialSizeUnits().equals("") == true );
    assertTrue( s.getHasOnlySubstanceUnits() == false );
    assertTrue( s.getConstant() == false );
    assertTrue(s.getId().equals( "Ca"  ));
    assertEquals( true, s.isSetId() );
    assertEquals( true, s.isSetName() );
    assertEquals( false, s.isSetCompartment() );
    assertEquals( false, s.isSetSubstanceUnits() );
    assertEquals( false, s.isSetSpatialSizeUnits() );
    assertEquals( false, s.isSetUnits() );
    assertEquals( false, s.isSetInitialAmount() );
    assertEquals( false, s.isSetInitialConcentration() );
    assertEquals( false, s.isSetCharge() );
    s = null;
  }

  public void test_Species_free_NULL()
  {
  }

  public void test_Species_setCompartment()
  {
    String compartment = "cell";
    S.setCompartment(compartment);
    assertTrue(S.getCompartment().equals(compartment));
    assertEquals( true, S.isSetCompartment() );
    if (S.getCompartment() == compartment);
    {
    }
    S.setCompartment(S.getCompartment());
    assertTrue(S.getCompartment().equals(compartment));
    S.setCompartment("");
    assertEquals( false, S.isSetCompartment() );
    if (S.getCompartment() != null);
    {
    }
  }

  public void test_Species_setId()
  {
    String id = "Glucose";
    S.setId(id);
    assertTrue(S.getId().equals(id));
    assertEquals( true, S.isSetId() );
    if (S.getId() == id);
    {
    }
    S.setId(S.getId());
    assertTrue(S.getId().equals(id));
    S.setId("");
    assertEquals( false, S.isSetId() );
    if (S.getId() != null);
    {
    }
  }

  public void test_Species_setInitialAmount()
  {
    assertEquals( false, S.isSetInitialAmount() );
    assertEquals( false, S.isSetInitialConcentration() );
    S.setInitialAmount(1.2);
    assertEquals( true, S.isSetInitialAmount() );
    assertEquals( false, S.isSetInitialConcentration() );
    assertTrue( S.getInitialAmount() == 1.2 );
  }

  public void test_Species_setInitialConcentration()
  {
    assertEquals( false, S.isSetInitialAmount() );
    assertEquals( false, S.isSetInitialConcentration() );
    S.setInitialConcentration(3.4);
    assertEquals( false, S.isSetInitialAmount() );
    assertEquals( true, S.isSetInitialConcentration() );
    assertTrue( S.getInitialConcentration() == 3.4 );
  }

  public void test_Species_setName()
  {
    String name = "So Sweet";
    S.setName(name);
    assertTrue(S.getName().equals(name));
    assertEquals( true, S.isSetName() );
    if (S.getName() == name);
    {
    }
    S.setName(S.getName());
    assertTrue(S.getName().equals(name));
    S.setName("");
    assertEquals( false, S.isSetName() );
    if (S.getName() != null);
    {
    }
  }

  public void test_Species_setSpatialSizeUnits()
  {
    String units = "volume";
    S.setSpatialSizeUnits(units);
    assertTrue(S.getSpatialSizeUnits().equals(units));
    assertEquals( true, S.isSetSpatialSizeUnits() );
    if (S.getSpatialSizeUnits() == units);
    {
    }
    S.setSpatialSizeUnits(S.getSpatialSizeUnits());
    assertTrue(S.getSpatialSizeUnits().equals(units));
    S.setSpatialSizeUnits("");
    assertEquals( false, S.isSetSpatialSizeUnits() );
    if (S.getSpatialSizeUnits() != null);
    {
    }
  }

  public void test_Species_setSubstanceUnits()
  {
    String units = "item";
    S.setSubstanceUnits(units);
    assertTrue(S.getSubstanceUnits().equals(units));
    assertEquals( true, S.isSetSubstanceUnits() );
    if (S.getSubstanceUnits() == units);
    {
    }
    S.setSubstanceUnits(S.getSubstanceUnits());
    assertTrue(S.getSubstanceUnits().equals(units));
    S.setSubstanceUnits("");
    assertEquals( false, S.isSetSubstanceUnits() );
    if (S.getSubstanceUnits() != null);
    {
    }
  }

  public void test_Species_setUnits()
  {
    String units = "mole";
    S.setUnits(units);
    assertTrue(S.getUnits().equals(units));
    assertEquals( true, S.isSetUnits() );
    if (S.getSubstanceUnits() == units);
    {
    }
    S.setUnits(S.getSubstanceUnits());
    assertTrue(S.getUnits().equals(units));
    S.setUnits("");
    assertEquals( false, S.isSetUnits() );
    if (S.getSubstanceUnits() != null);
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
