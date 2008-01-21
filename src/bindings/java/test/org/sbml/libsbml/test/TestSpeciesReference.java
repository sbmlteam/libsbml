/*
 *
 * @file    TestSpeciesReference.java
 * @brief   SpeciesReference unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestSpeciesReference.c
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

public class TestSpeciesReference {

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

  private SpeciesReference SR;

  protected void setUp() throws Exception
  {
    SR = new  SpeciesReference();
    if (SR == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    SR = null;
  }

  public void test_SpeciesReference_create()
  {
    assertTrue( SR.getTypeCode() == libsbml.SBML_SPECIES_REFERENCE );
    assertTrue( SR.getMetaId().equals("") == true );
    assertTrue( SR.getNotes() == null );
    assertTrue( SR.getAnnotation() == null );
    assertEquals(SR.getSpecies(),"");
    assertTrue( SR.getStoichiometry() == 1 );
    assertEquals(SR.getStoichiometryMath(),null);
    assertTrue( SR.getDenominator() == 1 );
    assertEquals( false, SR.isSetSpecies() );
    assertEquals( false, SR.isSetStoichiometryMath() );
  }

  public void test_SpeciesReference_createModifier()
  {
    ModifierSpeciesReference sr = new ModifierSpeciesReference();
    assertTrue( sr.getTypeCode() == libsbml.SBML_MODIFIER_SPECIES_REFERENCE );
    assertTrue( sr.getMetaId().equals("") == true );
    assertTrue( sr.getNotes() == null );
    assertTrue( sr.getAnnotation() == null );
    assertEquals( true, sr.isModifier() );
    sr = null;
  }

  public void test_SpeciesReference_createWith()
  {
    SpeciesReference sr = new  SpeciesReference("s3",4,2);
    assertTrue( sr.getTypeCode() == libsbml.SBML_SPECIES_REFERENCE );
    assertTrue( sr.getMetaId().equals("") == true );
    assertTrue( sr.getNotes() == null );
    assertTrue( sr.getAnnotation() == null );
    assertTrue(sr.getSpecies().equals( "s3"));
    assertTrue( sr.getStoichiometry() == 4 );
    assertTrue( sr.getDenominator() == 2 );
    assertEquals( true, sr.isSetSpecies() );
    sr = null;
  }

  public void test_SpeciesReference_free_NULL()
  {
  }

  public void test_SpeciesReference_setId()
  {
    String species = "X0";
    SR.setId(species);
    assertTrue(SR.getId().equals(species));
    assertEquals( true, SR.isSetId() );
    if (SR.getId() == species);
    {
    }
    SR.setId(SR.getId());
    assertTrue(SR.getId().equals(species));
    SR.setId("");
    assertEquals( false, SR.isSetId() );
    if (SR.getId() != null);
    {
    }
  }

  public void test_SpeciesReference_setSpecies()
  {
    String species = "X0";
    SR.setSpecies(species);
    assertTrue(SR.getSpecies().equals(species));
    assertEquals( true, SR.isSetSpecies() );
    if (SR.getSpecies() == species);
    {
    }
    SR.setSpecies(SR.getSpecies());
    assertTrue(SR.getSpecies().equals(species));
    SR.setSpecies("");
    assertEquals( false, SR.isSetSpecies() );
    if (SR.getSpecies() != null);
    {
    }
  }

  public void test_SpeciesReference_setStoichiometryMath()
  {
    ASTNode math = libsbml.parseFormula("k3 / k2");
    StoichiometryMath stoich = new  StoichiometryMath(math);
    StoichiometryMath math1;
    String formula;
    SR.setStoichiometryMath(stoich);
    math1 = SR.getStoichiometryMath();
    assertTrue( math1 != null );
    formula = libsbml.formulaToString(math1.getMath());
    assertTrue( formula != null );
    assertTrue(formula.equals( "k3 / k2"));
    assertEquals( true, SR.isSetStoichiometryMath() );
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
