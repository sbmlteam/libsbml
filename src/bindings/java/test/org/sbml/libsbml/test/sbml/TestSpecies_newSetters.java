/*
 *
 * @file    TestSpecies_newSetters.java
 * @brief   Species unit tests for new set function API
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Sarah Keating 
 *
 * $Id$
 * $HeadURL$
 *
 * This test file was converted from src/sbml/test/TestSpecies_newSetters.c
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

public class TestSpecies_newSetters {

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
  private Species C;

  protected void setUp() throws Exception
  {
    C = new  Species(1,2);
    if (C == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    C = null;
  }

  public void test_Species_setBoundaryCondition1()
  {
    long i = C.setBoundaryCondition(false);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( C.getBoundaryCondition() == false );
    i = C.setBoundaryCondition(true);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( C.getBoundaryCondition() == true );
  }

  public void test_Species_setCharge1()
  {
    long i = C.setCharge(2);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, C.isSetCharge() );
    assertTrue( C.getCharge() == 2 );
    i = C.unsetCharge();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetCharge() );
  }

  public void test_Species_setCharge2()
  {
    Species c = new  Species(2,2);
    long i = c.setCharge(4);
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    assertEquals( false, c.isSetCharge() );
    c = null;
  }

  public void test_Species_setCharge3()
  {
    Species c = new  Species(2,1);
    long i = c.unsetCharge();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetCharge() );
    c = null;
  }

  public void test_Species_setCompartment1()
  {
    long i = C.setCompartment( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, C.isSetCompartment() );
    i = C.setCompartment( "");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetCompartment() );
  }

  public void test_Species_setCompartment2()
  {
    long i = C.setCompartment( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, C.isSetCompartment() );
    i = C.setCompartment( "");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetCompartment() );
  }

  public void test_Species_setConstant1()
  {
    long i = C.setConstant(false);
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    assertTrue( C.getConstant() == false );
  }

  public void test_Species_setConstant2()
  {
    Species c = new  Species(2,2);
    long i = c.setConstant(true);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( c.getConstant() == true );
    c = null;
  }

  public void test_Species_setHasOnlySubstanceUnits1()
  {
    long i = C.setHasOnlySubstanceUnits(false);
    assertTrue( C.getHasOnlySubstanceUnits() == false );
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
  }

  public void test_Species_setHasOnlySubstanceUnits2()
  {
    Species c = new  Species(2,2);
    long i = c.setHasOnlySubstanceUnits(false);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( c.getHasOnlySubstanceUnits() == false );
    i = c.setHasOnlySubstanceUnits(true);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( c.getHasOnlySubstanceUnits() == true );
    c = null;
  }

  public void test_Species_setId2()
  {
    Species c = new  Species(2,2);
    long i = c.setId( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, c.isSetId() );
    c = null;
  }

  public void test_Species_setId3()
  {
    Species c = new  Species(2,2);
    long i = c.setId( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, c.isSetId() );
    assertTrue(c.getId().equals( "cell" ));
    c = null;
  }

  public void test_Species_setId4()
  {
    Species c = new  Species(2,2);
    long i = c.setId( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, c.isSetId() );
    assertTrue(c.getId().equals( "cell" ));
    i = c.setId("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetId() );
    c = null;
  }

  public void test_Species_setInitialAmount1()
  {
    long i = C.setInitialAmount(2.0);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( C.getInitialAmount() == 2.0 );
    assertEquals( true, C.isSetInitialAmount() );
    i = C.unsetInitialAmount();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetInitialAmount() );
  }

  public void test_Species_setInitialAmount2()
  {
    Species c = new  Species(2,2);
    long i = c.setInitialAmount(4);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( c.getInitialAmount() == 4.0 );
    assertEquals( true, c.isSetInitialAmount() );
    i = c.unsetInitialAmount();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetInitialAmount() );
    c = null;
  }

  public void test_Species_setInitialConcentration1()
  {
    long i = C.setInitialConcentration(2.0);
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    assertEquals( false, C.isSetInitialConcentration() );
  }

  public void test_Species_setInitialConcentration2()
  {
    Species c = new  Species(2,2);
    long i = c.setInitialConcentration(4);
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue( c.getInitialConcentration() == 4 );
    assertEquals( true, c.isSetInitialConcentration() );
    i = c.unsetInitialConcentration();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetInitialConcentration() );
    c = null;
  }

  public void test_Species_setName1()
  {
    long i = C.setName( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, C.isSetName() );
    i = C.unsetName();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetName() );
  }

  public void test_Species_setName2()
  {
    Species c = new  Species(2,2);
    long i = c.setName( "1cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, c.isSetName() );
    i = c.unsetName();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetName() );
    c = null;
  }

  public void test_Species_setName3()
  {
    long i = C.setName( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, C.isSetName() );
    i = C.setName("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetName() );
  }

  public void test_Species_setSpatialSizeUnits1()
  {
    long i = C.setSpatialSizeUnits( "mm");
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    assertEquals( false, C.isSetSpatialSizeUnits() );
  }

  public void test_Species_setSpatialSizeUnits2()
  {
    Species c = new  Species(2,2);
    long i = c.setSpatialSizeUnits( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, c.isSetSpatialSizeUnits() );
    c = null;
  }

  public void test_Species_setSpatialSizeUnits3()
  {
    Species c = new  Species(2,2);
    long i = c.setSpatialSizeUnits( "mole");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue(c.getSpatialSizeUnits().equals( "mole"));
    assertEquals( true, c.isSetSpatialSizeUnits() );
    c = null;
  }

  public void test_Species_setSpatialSizeUnits4()
  {
    Species c = new  Species(2,2);
    long i = c.setSpatialSizeUnits("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetSpatialSizeUnits() );
    c = null;
  }

  public void test_Species_setSpeciesType1()
  {
    long i = C.setSpeciesType( "cell");
    assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
    assertEquals( false, C.isSetSpeciesType() );
    i = C.unsetSpeciesType();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetSpeciesType() );
  }

  public void test_Species_setSpeciesType2()
  {
    Species c = new  Species(2,2);
    long i = c.setSpeciesType( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, c.isSetSpeciesType() );
    i = c.unsetSpeciesType();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetSpeciesType() );
    c = null;
  }

  public void test_Species_setSpeciesType3()
  {
    Species c = new  Species(2,2);
    long i = c.setSpeciesType( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, c.isSetSpeciesType() );
    assertTrue(c.getSpeciesType().equals( "cell" ));
    i = c.unsetSpeciesType();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetSpeciesType() );
    c = null;
  }

  public void test_Species_setSpeciesType4()
  {
    Species c = new  Species(2,2);
    long i = c.setSpeciesType( "cell");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, c.isSetSpeciesType() );
    assertTrue(c.getSpeciesType().equals( "cell" ));
    i = c.setSpeciesType("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetSpeciesType() );
    c = null;
  }

  public void test_Species_setSubstanceUnits1()
  {
    long i = C.setSubstanceUnits( "mm");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, C.isSetSubstanceUnits() );
  }

  public void test_Species_setSubstanceUnits2()
  {
    Species c = new  Species(2,2);
    long i = c.setSubstanceUnits( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, c.isSetSubstanceUnits() );
    c = null;
  }

  public void test_Species_setSubstanceUnits3()
  {
    Species c = new  Species(2,2);
    long i = c.setSubstanceUnits( "mole");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue(c.getSubstanceUnits().equals( "mole"));
    assertEquals( true, c.isSetSubstanceUnits() );
    c = null;
  }

  public void test_Species_setSubstanceUnits4()
  {
    Species c = new  Species(2,2);
    long i = c.setSubstanceUnits( "mole");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertTrue(c.getSubstanceUnits().equals( "mole"));
    assertEquals( true, c.isSetSubstanceUnits() );
    i = c.setSubstanceUnits("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, c.isSetSubstanceUnits() );
    c = null;
  }

  public void test_Species_setUnits1()
  {
    long i = C.setUnits( "1cell");
    assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
    assertEquals( false, C.isSetUnits() );
    i = C.unsetUnits();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetUnits() );
  }

  public void test_Species_setUnits2()
  {
    long i = C.setUnits( "litre");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, C.isSetUnits() );
    i = C.unsetUnits();
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetUnits() );
  }

  public void test_Species_setUnits3()
  {
    long i = C.setUnits( "litre");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( true, C.isSetUnits() );
    i = C.setUnits("");
    assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    assertEquals( false, C.isSetUnits() );
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
