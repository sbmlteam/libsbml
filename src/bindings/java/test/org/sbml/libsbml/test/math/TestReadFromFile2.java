/*
 *
 * @file    TestReadFromFile2.java
 * @brief   Tests for reading MathML from files into ASTNodes.
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Sarah Keating 
 *
 * $Id$
 * $HeadURL$
 *
 * This test file was converted from src/sbml/test/TestReadFromFile2.cpp
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


package org.sbml.libsbml.test.math;

import org.sbml.libsbml.*;

import java.io.File;
import java.lang.AssertionError;

public class TestReadFromFile2 {

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

  public void test_read_MathML_2()
  {
    SBMLReader reader = new SBMLReader();
    SBMLDocument d;
    Model m;
    FunctionDefinition fd;
    InitialAssignment ia;
    Rule r;
    String filename = new String( "../../math/test/test-data/" );
    filename += "mathML_2.xml";
    d = reader.readSBML(filename);
    if (d == null);
    {
    }
    m = d.getModel();
    assertTrue( m != null );
    assertTrue( m.getNumFunctionDefinitions() == 2 );
    assertTrue( m.getNumInitialAssignments() == 1 );
    assertTrue( m.getNumRules() == 2 );
    fd = m.getFunctionDefinition(0);
    ASTNode fd_math = fd.getMath();
    assertTrue( fd_math.getType() == libsbml.AST_LAMBDA );
    assertTrue( fd_math.getNumChildren() == 1 );
    assertTrue(libsbml.formulaToString(fd_math).equals( "lambda()"));
    ASTNode child = fd_math.getChild(0);
    assertTrue( child.getType() == libsbml.AST_UNKNOWN );
    assertTrue( child.getNumChildren() == 0 );
    assertTrue(libsbml.formulaToString(child).equals( ""));
    fd = m.getFunctionDefinition(1);
    ASTNode fd1_math = fd.getMath();
    assertTrue( fd1_math.getType() == libsbml.AST_LAMBDA );
    assertTrue( fd1_math.getNumChildren() == 2 );
    assertTrue(libsbml.formulaToString(fd1_math).equals(                           "lambda(x, piecewise(p, leq(x, 4)))"));
    ASTNode child1 = fd1_math.getRightChild();
    assertTrue( child1.getType() == libsbml.AST_FUNCTION_PIECEWISE );
    assertTrue( child1.getNumChildren() == 2 );
    assertTrue(libsbml.formulaToString(child1).equals(                                     "piecewise(p, leq(x, 4))"));
    ASTNode c1 = child1.getChild(0);
    assertTrue( c1.getType() == libsbml.AST_NAME );
    assertTrue( c1.getNumChildren() == 0 );
    assertTrue(libsbml.formulaToString(c1).equals( "p"));
    ASTNode c2 = child1.getChild(1);
    assertTrue( c2.getType() == libsbml.AST_RELATIONAL_LEQ );
    assertTrue( c2.getNumChildren() == 2 );
    assertTrue(libsbml.formulaToString(c2).equals( "leq(x, 4)"));
    ia = m.getInitialAssignment(0);
    ASTNode ia_math = ia.getMath();
    assertTrue( ia_math.getType() == libsbml.AST_FUNCTION_PIECEWISE );
    assertTrue( ia_math.getNumChildren() == 4 );
    assertTrue(libsbml.formulaToString(ia_math).equals(                     "piecewise(-x, lt(x, 0), 0, eq(x, 0))"));
    child1 = ia_math.getChild(0);
    ASTNode child2 = ia_math.getChild(1);
    ASTNode child3 = ia_math.getChild(2);
    ASTNode child4 = ia_math.getChild(3);
    assertTrue( child1.getType() == libsbml.AST_MINUS );
    assertTrue( child1.getNumChildren() == 1 );
    assertTrue(libsbml.formulaToString(child1).equals( "-x"));
    assertTrue( child2.getType() == libsbml.AST_RELATIONAL_LT );
    assertTrue( child2.getNumChildren() == 2 );
    assertTrue(libsbml.formulaToString(child2).equals( "lt(x, 0)"));
    assertTrue( child3.getType() == libsbml.AST_REAL );
    assertTrue( child3.getNumChildren() == 0 );
    assertTrue(libsbml.formulaToString(child3).equals( "0"));
    assertTrue( child4.getType() == libsbml.AST_RELATIONAL_EQ );
    assertTrue( child4.getNumChildren() == 2 );
    assertTrue(libsbml.formulaToString(child4).equals( "eq(x, 0)"));
    r = m.getRule(0);
    ASTNode r_math = r.getMath();
    assertTrue( r_math.getType() == libsbml.AST_CONSTANT_TRUE );
    assertTrue( r_math.getNumChildren() == 0 );
    assertTrue(libsbml.formulaToString(r_math).equals( "true"));
    r = m.getRule(1);
    ASTNode r1_math = r.getMath();
    assertTrue( r1_math.getType() == libsbml.AST_FUNCTION_LOG );
    assertTrue( r1_math.getNumChildren() == 2 );
    assertTrue(libsbml.formulaToString(r1_math).equals( "log(3, x)"));
    child1 = r1_math.getChild(0);
    child2 = r1_math.getChild(1);
    assertTrue( child1.getType() == libsbml.AST_REAL );
    assertTrue( child1.getNumChildren() == 0 );
    assertTrue(libsbml.formulaToString(child1).equals( "3"));
    assertTrue( child2.getType() == libsbml.AST_NAME );
    assertTrue( child2.getNumChildren() == 0 );
    assertTrue(libsbml.formulaToString(child2).equals( "x"));
    d = null;
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
