/*
 *
 * @file    TestXMLInputStream.java
 * @brief   XMLInputStream unit tests
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Sarah Keating 
 *
 * $Id:$
 * $HeadURL:$
 *
 * This test file was converted from src/sbml/test/TestXMLInputStream.c
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


package org.sbml.libsbml.test.xml;

import org.sbml.libsbml.*;

import java.io.File;
import java.lang.AssertionError;

public class TestXMLInputStream {

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

  public String LV_L1v1()
  {
    return "level=\"1\" version=\"1\">\n";
  }

  public String LV_L1v2()
  {
    return "level=\"1\" version=\"2\">\n";
  }

  public String LV_L2v1()
  {
    return "level=\"2\" version=\"1\">\n";
  }

  public String LV_L2v2()
  {
    return "level=\"2\" version=\"2\">\n";
  }

  public String NS_L1()
  {
    return "xmlns=\"http://www.sbml.org/sbml/level1\" ";
  }

  public String NS_L2v1()
  {
    return "xmlns=\"http://www.sbml.org/sbml/level2\" ";
  }

  public String NS_L2v2()
  {
    return "xmlns=\"http://www.sbml.org/sbml/level2/version2\" ";
  }

  public String SBML_END()
  {
    return "</sbml>\n";
  }

  public String SBML_START()
  {
    return "<sbml ";
  }

  public String XML_START()
  {
    return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  }

  public String wrapSBML_L1v1(String s)
  {
    String r = XML_START();
    r += SBML_START();
    r += NS_L1();
    r += LV_L1v1();
    r += s;
    r += SBML_END();
    return r;
  }

  public String wrapSBML_L1v2(String s)
  {
    String r = XML_START();
    r += SBML_START();
    r += NS_L1();
    r += LV_L1v2();
    r += s;
    r += SBML_END();
    return r;
  }

  public String wrapSBML_L2v1(String s)
  {
    String r = XML_START();
    r += SBML_START();
    r += NS_L2v1();
    r += LV_L2v1();
    r += s;
    r += SBML_END();
    return r;
  }

  public String wrapSBML_L2v2(String s)
  {
    String r = XML_START();
    r += SBML_START();
    r += NS_L2v2();
    r += LV_L2v2();
    r += s;
    r += SBML_END();
    return r;
  }

  public String wrapXML(String s)
  {
    String r = XML_START();
    r += s;
    return r;
  }

  public void test_XMLInputStream_create()
  {
    String text = wrapSBML_L2v1("  <model id=\"Branch\"/>\n");
    XMLInputStream stream = new  XMLInputStream(text,false, "");
    assertTrue( stream != null );
    assertTrue( stream.isEOF() == false );
    assertTrue( stream.isGood() == true );
    assertTrue( stream.isError() == false );
    stream.next();
    assertTrue( !stream.getEncoding().equals( "UTF-8") == false );
    stream = null;
  }

  public void test_XMLInputStream_next_peek()
  {
    String text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
    "<sbml " + 
    "xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\">\n" + 
    "  <model id=\"Branch\"/>\n" + 
    "</sbml>";
    XMLInputStream stream = new  XMLInputStream(text,false, "");
    XMLToken next0 = stream.peek();
    assertTrue( stream != null );
    assertTrue( !next0.getName().equals( "sbml") == false );
    XMLToken next1 = stream.next();
    assertTrue( !next1.getName().equals( "sbml") == false );
    stream = null;
  }

  public void test_XMLInputStream_setErrorLog()
  {
    String text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
    "<sbml " + 
    "xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\">\n" + 
    "<listOfFunctionDefinitions>\n" + 
    "<notes>My Functions</notes>\n" + 
    "<functionDefinition/>\n" + 
    "</listOfFunctionDefinitions>\n" + 
    "<listOfUnitDefinitions>\n" + 
    "<notes>My Units</notes>\n" + 
    "<unitDefinition/>\n" + 
    "</listOfUnitDefinitions>\n" + 
    "</sbml>";
    XMLInputStream stream = new  XMLInputStream(text,false, "");
    assertTrue( stream != null );
    XMLErrorLog log = new  XMLErrorLog();
    stream.setErrorLog(log);
    assertTrue( stream.getErrorLog().equals(log) );
  }

  public void test_XMLInputStream_skip()
  {
    String text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
    "<sbml " + 
    "xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\">\n" + 
    "<listOfFunctionDefinitions>\n" + 
    "<notes>My Functions</notes>\n" + 
    "<functionDefinition/>\n" + 
    "</listOfFunctionDefinitions>\n" + 
    "<listOfUnitDefinitions>\n" + 
    "<notes>My Units</notes>\n" + 
    "<unitDefinition/>\n" + 
    "</listOfUnitDefinitions>\n" + 
    "</sbml>";
    XMLInputStream stream = new  XMLInputStream(text,false, "");
    assertTrue( stream != null );
    XMLToken next0 = stream.next();
    stream.skipText();
    stream.skipPastEnd(stream.next());
    stream.skipText();
    next0 = stream.next();
    assertTrue( !next0.getName().equals( "listOfUnitDefinitions") == false );
    stream = null;
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
