/*
 *
 * @file    TestSBMLTransforms.java
 * @brief   SBMLTransforms unit tests
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Sarah Keating 
 *
 * $Id$
 * $HeadURL$
 *
 * This test file was converted from src/sbml/test/TestSBMLTransforms.cpp
 * with the help of conversion sciprt (ctest_converter.pl).
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
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

public class TestSBMLTransforms {

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

  public void test_SBMLTransforms_replaceFD()
  {
    SBMLReader reader = new SBMLReader();
    SBMLDocument d;
    Model m;
    ASTNode ast;
    FunctionDefinition fd;
    ListOfFunctionDefinitions lofd;
    String filename = new String( "../../sbml/test/test-data/" );
    filename += "multiple-functions.xml";
    d = reader.readSBML(filename);
    if (d == null);
    {
    }
    m = d.getModel();
    assertTrue( m.getNumFunctionDefinitions() == 2 );
    ast = m.getReaction(2).getKineticLaw().getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "f(S1, p) * compartmentOne / t"));
    fd = m.getFunctionDefinition(0);
    SBMLTransforms.replaceFD(((ASTNode) ast),fd);
    assertTrue(libsbml.formulaToString(ast).equals( "S1 * p * compartmentOne / t"));
    ast = m.getReaction(1).getKineticLaw().getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "f(f(S1, p), compartmentOne) / t"));
    SBMLTransforms.replaceFD(((ASTNode) ast),fd);
    assertTrue(libsbml.formulaToString(ast).equals( "S1 * p * compartmentOne / t"));
    ast = m.getReaction(0).getKineticLaw().getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "g(f(S1, p), compartmentOne) / t"));
    SBMLTransforms.replaceFD(((ASTNode) ast),fd);
    assertTrue(libsbml.formulaToString(ast).equals( "g(S1 * p, compartmentOne) / t"));
    fd = m.getFunctionDefinition(1);
    SBMLTransforms.replaceFD(((ASTNode) ast),fd);
    assertTrue(libsbml.formulaToString(ast).equals( "f(S1 * p, compartmentOne) / t"));
    ast = m.getReaction(0).getKineticLaw().getMath();
    lofd = m.getListOfFunctionDefinitions();
    SBMLTransforms.replaceFD(((ASTNode) ast),lofd);
    assertTrue(libsbml.formulaToString(ast).equals( "S1 * p * compartmentOne / t"));
    d.expandFunctionDefinitions();
    assertTrue( d.getModel().getNumFunctionDefinitions() == 0 );
    ast = d.getModel().getReaction(0).getKineticLaw().getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "S1 * p * compartmentOne / t"));
    ast = d.getModel().getReaction(1).getKineticLaw().getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "S1 * p * compartmentOne / t"));
    ast = d.getModel().getReaction(2).getKineticLaw().getMath();
    assertTrue(libsbml.formulaToString(ast).equals( "S1 * p * compartmentOne / t"));
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
