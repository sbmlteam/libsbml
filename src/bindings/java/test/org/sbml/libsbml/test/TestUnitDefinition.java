/*
 *
 * @file    TestUnitDefinition.java
 * @brief   SBML UnitDefinition unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestUnitDefinition.c
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

public class TestUnitDefinition {

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

  private UnitDefinition UD;

  protected void setUp() throws Exception
  {
    UD = new  UnitDefinition();
    if (UD == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    UD = null;
  }

  public void test_UnitDefinition_addUnit()
  {
    Unit u = new  Unit();
    UD.addUnit(u);
    assertTrue( UD.getNumUnits() == 1 );
    u = null;
  }

  public void test_UnitDefinition_create()
  {
    assertTrue( UD.getTypeCode() == libsbml.SBML_UNIT_DEFINITION );
    assertTrue( UD.getMetaId().equals("") == true );
    assertTrue( UD.getNotes() == null );
    assertTrue( UD.getAnnotation() == null );
    assertTrue( UD.getId().equals("") == true );
    assertTrue( UD.getName().equals("") == true );
    assertEquals( false, UD.isSetId() );
    assertEquals( false, UD.isSetName() );
    assertTrue( UD.getNumUnits() == 0 );
  }

  public void test_UnitDefinition_createWith()
  {
    UnitDefinition ud = new  UnitDefinition("mmls", "");
    assertTrue( ud.getTypeCode() == libsbml.SBML_UNIT_DEFINITION );
    assertTrue( ud.getMetaId().equals("") == true );
    assertTrue( ud.getNotes() == null );
    assertTrue( ud.getAnnotation() == null );
    assertTrue( ud.getName().equals("") == true );
    assertTrue(ud.getId().equals( "mmls"));
    assertEquals( true, ud.isSetId() );
    assertTrue( ud.getNumUnits() == 0 );
    ud = null;
  }

  public void test_UnitDefinition_createWithName()
  {
    UnitDefinition ud = new  UnitDefinition("", "mmol liter^-1 sec^-1");
    assertTrue( ud.getTypeCode() == libsbml.SBML_UNIT_DEFINITION );
    assertTrue( ud.getMetaId().equals("") == true );
    assertTrue( ud.getNotes() == null );
    assertTrue( ud.getAnnotation() == null );
    assertTrue( ud.getId().equals("") == true );
    assertTrue(ud.getName().equals( "mmol liter^-1 sec^-1"));
    assertEquals( true, ud.isSetName() );
    assertTrue( ud.getNumUnits() == 0 );
    ud = null;
  }

  public void test_UnitDefinition_free_NULL()
  {
  }

  public void test_UnitDefinition_getUnit()
  {
    Unit mole = new  Unit();
    Unit litre = new  Unit();
    Unit second = new  Unit();
    mole.setKind(libsbml.UnitKind_forName("mole"));
    litre.setKind(libsbml.UnitKind_forName("litre"));
    second.setKind(libsbml.UnitKind_forName("second"));
    mole.setScale(-3);
    litre.setExponent(-1);
    second.setExponent(-1);
    UD.addUnit(mole);
    UD.addUnit(litre);
    UD.addUnit(second);
    mole = null;
    litre = null;
    second = null;
    assertTrue( UD.getNumUnits() == 3 );
    mole = UD.getUnit(0);
    litre = UD.getUnit(1);
    second = UD.getUnit(2);
    assertTrue( mole.getKind() == libsbml.UNIT_KIND_MOLE );
    assertTrue( litre.getKind() == libsbml.UNIT_KIND_LITRE );
    assertTrue( second.getKind() == libsbml.UNIT_KIND_SECOND );
    assertTrue( mole.getScale() == -3 );
    assertTrue( litre.getExponent() == -1 );
    assertTrue( second.getExponent() == -1 );
  }

  public void test_UnitDefinition_isVariantOfArea()
  {
    Unit dim = new  Unit();
    dim.setKind(libsbml.UnitKind_forName("dimensionless"));
    Unit u = UD.createUnit();
    assertEquals( false, UD.isVariantOfArea() );
    u.setKind(libsbml.UNIT_KIND_METRE);
    u.setExponent(2);
    assertEquals( true, UD.isVariantOfArea() );
    u.setScale(-1);
    assertEquals( true, UD.isVariantOfArea() );
    u.setMultiplier(2);
    assertEquals( true, UD.isVariantOfArea() );
    u.setOffset(3);
    assertEquals( true, UD.isVariantOfArea() );
    u.setExponent(3);
    assertEquals( false, UD.isVariantOfArea() );
    u.setExponent(2);
    UD.addUnit(dim);
    assertEquals( true, UD.isVariantOfArea() );
  }

  public void test_UnitDefinition_isVariantOfLength()
  {
    Unit dim = new  Unit();
    dim.setKind(libsbml.UnitKind_forName("dimensionless"));
    Unit u = UD.createUnit();
    assertEquals( false, UD.isVariantOfLength() );
    u.setKind(libsbml.UNIT_KIND_METRE);
    u.setExponent(1);
    assertEquals( true, UD.isVariantOfLength() );
    u.setScale(-1);
    assertEquals( true, UD.isVariantOfLength() );
    u.setMultiplier(2);
    assertEquals( true, UD.isVariantOfLength() );
    u.setOffset(3);
    assertEquals( true, UD.isVariantOfLength() );
    u.setExponent(2);
    assertEquals( false, UD.isVariantOfLength() );
    u.setExponent(1);
    UD.addUnit(dim);
    assertEquals( true, UD.isVariantOfLength() );
  }

  public void test_UnitDefinition_isVariantOfSubstance_1()
  {
    Unit dim = new  Unit();
    dim.setKind(libsbml.UnitKind_forName("dimensionless"));
    Unit u = UD.createUnit();
    assertEquals( false, UD.isVariantOfSubstance() );
    u.setKind(libsbml.UNIT_KIND_MOLE);
    u.setExponent(1);
    assertEquals( true, UD.isVariantOfSubstance() );
    u.setScale(-1);
    assertEquals( true, UD.isVariantOfSubstance() );
    u.setMultiplier(2);
    assertEquals( true, UD.isVariantOfSubstance() );
    u.setOffset(3);
    assertEquals( true, UD.isVariantOfSubstance() );
    u.setExponent(-3);
    assertEquals( false, UD.isVariantOfSubstance() );
    u.setExponent(1);
    UD.addUnit(dim);
    assertEquals( true, UD.isVariantOfSubstance() );
  }

  public void test_UnitDefinition_isVariantOfSubstance_2()
  {
    Unit dim = new  Unit();
    dim.setKind(libsbml.UnitKind_forName("dimensionless"));
    Unit u = UD.createUnit();
    assertEquals( false, UD.isVariantOfSubstance() );
    u.setKind(libsbml.UNIT_KIND_ITEM);
    u.setExponent(1);
    assertEquals( true, UD.isVariantOfSubstance() );
    u.setScale(-1);
    assertEquals( true, UD.isVariantOfSubstance() );
    u.setMultiplier(2);
    assertEquals( true, UD.isVariantOfSubstance() );
    u.setOffset(3);
    assertEquals( true, UD.isVariantOfSubstance() );
    u.setExponent(-2);
    assertEquals( false, UD.isVariantOfSubstance() );
    u.setExponent(1);
    UD.addUnit(dim);
    assertEquals( true, UD.isVariantOfSubstance() );
  }

  public void test_UnitDefinition_isVariantOfTime()
  {
    Unit dim = new  Unit();
    dim.setKind(libsbml.UnitKind_forName("dimensionless"));
    Unit u = UD.createUnit();
    assertEquals( false, UD.isVariantOfTime() );
    u.setKind(libsbml.UNIT_KIND_SECOND);
    u.setExponent(1);
    assertEquals( true, UD.isVariantOfTime() );
    u.setScale(-10);
    assertEquals( true, UD.isVariantOfTime() );
    u.setMultiplier(10);
    assertEquals( true, UD.isVariantOfTime() );
    u.setOffset(30);
    assertEquals( true, UD.isVariantOfTime() );
    u.setExponent(2);
    assertEquals( false, UD.isVariantOfTime() );
    u.setExponent(1);
    UD.addUnit(dim);
    assertEquals( true, UD.isVariantOfTime() );
  }

  public void test_UnitDefinition_isVariantOfVolume_1()
  {
    Unit dim = new  Unit();
    dim.setKind(libsbml.UnitKind_forName("dimensionless"));
    Unit u = UD.createUnit();
    assertEquals( false, UD.isVariantOfVolume() );
    u.setKind(libsbml.UNIT_KIND_LITRE);
    u.setExponent(1);
    assertEquals( true, UD.isVariantOfVolume() );
    u.setScale(100);
    assertEquals( true, UD.isVariantOfVolume() );
    u.setMultiplier(5);
    assertEquals( true, UD.isVariantOfVolume() );
    u.setOffset(-5);
    assertEquals( true, UD.isVariantOfVolume() );
    u.setExponent(-1);
    assertEquals( false, UD.isVariantOfVolume() );
    u.setExponent(1);
    UD.addUnit(dim);
    assertEquals( true, UD.isVariantOfVolume() );
  }

  public void test_UnitDefinition_isVariantOfVolume_2()
  {
    Unit dim = new  Unit();
    dim.setKind(libsbml.UnitKind_forName("dimensionless"));
    Unit u = UD.createUnit();
    assertEquals( false, UD.isVariantOfVolume() );
    u.setKind(libsbml.UNIT_KIND_METRE);
    u.setExponent(3);
    assertEquals( true, UD.isVariantOfVolume() );
    u.setScale(100);
    assertEquals( true, UD.isVariantOfVolume() );
    u.setMultiplier(5);
    assertEquals( true, UD.isVariantOfVolume() );
    u.setOffset(-5);
    assertEquals( true, UD.isVariantOfVolume() );
    u.setExponent(2);
    assertEquals( false, UD.isVariantOfVolume() );
    u.setExponent(3);
    UD.addUnit(dim);
    assertEquals( true, UD.isVariantOfVolume() );
  }

  public void test_UnitDefinition_setId()
  {
    String id = "mmls";
    UD.setId(id);
    assertTrue(UD.getId().equals(id));
    assertEquals( true, UD.isSetId() );
    if (UD.getId() == id);
    {
    }
    UD.setId(UD.getId());
    assertTrue(UD.getId().equals(id));
    UD.setId("");
    assertEquals( false, UD.isSetId() );
    if (UD.getId() != null);
    {
    }
  }

  public void test_UnitDefinition_setName()
  {
    String name = "mmol liter^-1 sec^-1";
    UD.setName(name);
    assertTrue(UD.getName().equals(name));
    assertEquals( true, UD.isSetName() );
    if (UD.getName() == name);
    {
    }
    UD.setName(UD.getName());
    assertTrue(UD.getName().equals(name));
    UD.setName("");
    assertEquals( false, UD.isSetName() );
    if (UD.getName() != null);
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
