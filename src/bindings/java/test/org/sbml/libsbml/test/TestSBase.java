/*
 *
 * @file    TestSBase.java
 * @brief   SBase unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestSBase.cpp
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

public class TestSBase {

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

  private SBase S;

  protected void setUp() throws Exception
  {
    S = new Model();
    if (S == null);
    {
    }
  }

  protected void tearDown() throws Exception
  {
    S = null;
  }

  public void test_SBase_CVTerms()
  {
    CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
    assertTrue( S.getNumCVTerms() == 0 );
    S.addCVTerm(cv);
    assertTrue( S.getNumCVTerms() == 1 );
    assertTrue( S.getCVTerm(0) != cv );
    cv = null;
  }

  public void test_SBase_addCVTerms()
  {
    CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
    cv.setBiologicalQualifierType(libsbml.BQB_ENCODES);
    cv.addResource( "foo");
    S.addCVTerm(cv);
    assertTrue( S.getNumCVTerms() == 1 );
    XMLAttributes res = S.getCVTerm(0).getResources();
    assertTrue(res.getValue(0).equals( "foo"));
    CVTerm cv1 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
    cv1.setBiologicalQualifierType(libsbml.BQB_IS);
    cv1.addResource( "bar");
    S.addCVTerm(cv1);
    assertTrue( S.getNumCVTerms() == 2 );
    CVTerm cv2 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
    cv2.setBiologicalQualifierType(libsbml.BQB_IS);
    cv2.addResource( "bar1");
    S.addCVTerm(cv2);
    assertTrue( S.getNumCVTerms() == 2 );
    res = S.getCVTerm(1).getResources();
    assertTrue( res.getLength() == 2 );
    assertTrue(res.getValue(0).equals( "bar"));
    assertTrue(res.getValue(1).equals( "bar1"));
    CVTerm cv4 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
    cv4.setBiologicalQualifierType(libsbml.BQB_IS);
    cv4.addResource( "bar1");
    S.addCVTerm(cv4);
    assertTrue( S.getNumCVTerms() == 2 );
    res = S.getCVTerm(1).getResources();
    assertTrue( res.getLength() == 2 );
    assertTrue(res.getValue(0).equals( "bar"));
    assertTrue(res.getValue(1).equals( "bar1"));
    CVTerm cv5 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
    cv5.setBiologicalQualifierType(libsbml.BQB_HAS_PART);
    cv5.addResource( "bar1");
    S.addCVTerm(cv5);
    assertTrue( S.getNumCVTerms() == 2 );
    res = S.getCVTerm(1).getResources();
    assertTrue( res.getLength() == 2 );
    assertTrue(res.getValue(0).equals( "bar"));
    assertTrue(res.getValue(1).equals( "bar1"));
    cv = null;
    cv2 = null;
    cv1 = null;
    cv4 = null;
  }

  public void test_SBase_getQualifiersFromResources()
  {
    CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
    cv.setBiologicalQualifierType(libsbml.BQB_ENCODES);
    cv.addResource( "foo");
    S.addCVTerm(cv);
    assertTrue( S.getResourceBiologicalQualifier( "foo") == libsbml.BQB_ENCODES );
    CVTerm cv1 = new  CVTerm(libsbml.MODEL_QUALIFIER);
    cv1.setModelQualifierType(libsbml.BQM_IS);
    cv1.addResource( "bar");
    S.addCVTerm(cv1);
    assertTrue( S.getResourceModelQualifier( "bar") == libsbml.BQM_IS );
    cv = null;
    cv1 = null;
  }

  public void test_SBase_setAnnotation()
  {
    XMLToken token;
    XMLNode node;
    token = new  XMLToken("This is a test note");
    node = new  XMLNode(token);
    S.setAnnotation(node);
    assertTrue( S.isSetAnnotation() == true );
    XMLNode t1 = S.getAnnotation();
    assertTrue( t1.getNumChildren() == 1 );
    assertTrue(t1.getChild(0).getCharacters().equals( "This is a test note"));
    if (S.getAnnotation() == node);
    {
    }
    S.setAnnotation(S.getAnnotation());
    assertTrue(S.getAnnotation().getChild(0).getCharacters().equals( "This is a test note"));
    S.setAnnotation("");
    assertTrue( S.isSetAnnotation() == false );
    if (S.getAnnotation() != null);
    {
    }
    S.setAnnotation(node);
    assertTrue( S.isSetAnnotation() == true );
    S.unsetAnnotation();
    assertTrue( S.isSetAnnotation() == false );
  }

  public void test_SBase_setAnnotationString()
  {
    String annotation = "This is a test note";
    String taggedannotation = "<annotation>This is a test note</annotation>";
    S.setAnnotation(annotation);
    assertTrue( S.isSetAnnotation() == true );
    if (!S.getAnnotationString().equals(taggedannotation));
    {
    }
    XMLNode t1 = S.getAnnotation();
    assertTrue( t1.getNumChildren() == 1 );
    XMLNode t2 = t1.getChild(0);
    assertTrue(t2.getChild(0).getCharacters().equals( "This is a test note"));
    S.setAnnotation(S.getAnnotationString());
    t1 = S.getAnnotation();
    assertTrue( t1.getNumChildren() == 1 );
    String chars = S.getAnnotationString();
    assertTrue(chars.equals(taggedannotation));
    S.setAnnotation( "");
    assertTrue( S.isSetAnnotation() == false );
    if (S.getAnnotationString() != null);
    {
    }
    S.setAnnotation(taggedannotation);
    assertTrue( S.isSetAnnotation() == true );
    if (!S.getAnnotationString().equals(taggedannotation));
    {
    }
    t1 = S.getAnnotation();
    assertTrue( t1.getNumChildren() == 1 );
    t2 = t1.getChild(0);
    assertTrue(t2.getCharacters().equals( "This is a test note"));
  }

  public void test_SBase_setMetaId()
  {
    String metaid = "x12345";
    S.setMetaId(metaid);
    assertTrue(S.getMetaId().equals(metaid));
    assertEquals( true, S.isSetMetaId() );
    if (S.getMetaId() == metaid);
    {
    }
    S.setMetaId(S.getMetaId());
    assertTrue(S.getMetaId().equals(metaid));
    S.setMetaId("");
    assertEquals( false, S.isSetMetaId() );
    if (S.getMetaId() != null);
    {
    }
  }

  public void test_SBase_setNotes()
  {
    XMLToken token;
    XMLNode node;
    token = new  XMLToken("This is a test note");
    node = new  XMLNode(token);
    S.setNotes(node);
    assertTrue( S.isSetNotes() == true );
    if (S.getNotes() == node);
    {
    }
    XMLNode t1 = S.getNotes();
    assertTrue( t1.getNumChildren() == 1 );
    assertTrue(t1.getChild(0).getCharacters().equals( "This is a test note"));
    S.setNotes(S.getNotes());
    t1 = S.getNotes();
    assertTrue( t1.getNumChildren() == 1 );
    String chars = t1.getChild(0).getCharacters();
    assertTrue(chars.equals( "This is a test note"));
    S.setNotes("");
    assertTrue( S.isSetNotes() == false );
    if (S.getNotes() != null);
    {
    }
    S.setNotes(node);
    assertTrue( S.isSetNotes() == true );
    node = null;
  }

  public void test_SBase_setNotesString()
  {
    String notes = "This is a test note";
    String taggednotes = "<notes>This is a test note</notes>";
    S.setNotes(notes);
    assertTrue( S.isSetNotes() == true );
    if (!S.getNotesString().equals(taggednotes));
    {
    }
    XMLNode t1 = S.getNotes();
    assertTrue( t1.getNumChildren() == 1 );
    XMLNode t2 = t1.getChild(0);
    assertTrue(t2.getChild(0).getCharacters().equals( "This is a test note"));
    S.setNotes(S.getNotesString());
    t1 = S.getNotes();
    assertTrue( t1.getNumChildren() == 1 );
    String chars = S.getNotesString();
    assertTrue(chars.equals(taggednotes));
    S.setNotes( "");
    assertTrue( S.isSetNotes() == false );
    if (S.getNotesString() != null);
    {
    }
    S.setNotes(taggednotes);
    assertTrue( S.isSetNotes() == true );
    if (!S.getNotesString().equals(taggednotes));
    {
    }
    t1 = S.getNotes();
    assertTrue( t1.getNumChildren() == 1 );
    t2 = t1.getChild(0);
    assertTrue(t2.getCharacters().equals( "This is a test note"));
  }

  public void test_SBase_unsetCVTerms()
  {
    CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
    cv.setBiologicalQualifierType(libsbml.BQB_ENCODES);
    cv.addResource( "foo");
    S.addCVTerm(cv);
    CVTerm cv1 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
    cv1.setBiologicalQualifierType(libsbml.BQB_IS);
    cv1.addResource( "bar");
    S.addCVTerm(cv1);
    CVTerm cv2 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
    cv2.setBiologicalQualifierType(libsbml.BQB_IS);
    cv2.addResource( "bar1");
    S.addCVTerm(cv2);
    CVTerm cv4 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
    cv4.setBiologicalQualifierType(libsbml.BQB_IS);
    cv4.addResource( "bar1");
    S.addCVTerm(cv4);
    assertTrue( S.getNumCVTerms() == 2 );
    S.unsetCVTerms();
    assertTrue( S.getNumCVTerms() == 0 );
    cv = null;
    cv2 = null;
    cv1 = null;
    cv4 = null;
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
