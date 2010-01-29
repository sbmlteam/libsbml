/*
 *
 * @file    TestRDFAnnotation2.java
 * @brief   fomula units data unit tests
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $HeadURL$
 *
 * This test file was converted from src/sbml/test/TestRDFAnnotation2.cpp
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

public class TestRDFAnnotation2 {

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
  private Model m2;
  private SBMLDocument d2;


  public double util_NaN()
  {
    double z = 0.0;
    return 0.0/z;
  }

  public double util_PosInf()
  {
    double z = 0.0;
    return 1.0/z;
  }

  public double util_NegInf()
  {
    double z = 0.0;
    return -1.0/z;
  }

//  public boolean equals(String s)
//  {
//    return s.equals(OSS.str());
//  }

  public boolean equals(String s1, String s2)
  {
    return s1.equals(s2);
  }

  protected void setUp() throws Exception
  {
    String filename = "../../annotation/test/test-data/annotation2.xml";
    d2 = libsbml.readSBML(filename);
    m2 = d2.getModel();
  }

  protected void tearDown() throws Exception
  {
  }

  public void test_RDFAnnotation2_getModelHistory()
  {
    ModelHistory history = m2.getModelHistory();
    assertTrue( history != null );
    ModelCreator mc = history.getCreator(0);
    assertTrue(mc.getFamilyName().equals( "Hucka"));
    assertTrue(mc.getGivenName().equals( "Mike"));
    assertTrue(mc.getEmail().equals( "mhucka@caltech.edu"));
    assertTrue(mc.getOrganisation().equals( "BNMC"));
    ModelCreator mc1 = history.getCreator(1);
    assertTrue(mc1.getFamilyName().equals( "Keating"));
    assertTrue(mc1.getGivenName().equals( "Sarah"));
    assertTrue(mc1.getEmail().equals( "skeating@caltech.edu"));
    assertTrue(mc1.getOrganisation().equals( "UH"));
    Date date = history.getCreatedDate();
    assertTrue( date.getYear() == 2005 );
    assertTrue( date.getMonth() == 2 );
    assertTrue( date.getDay() == 2 );
    assertTrue( date.getHour() == 14 );
    assertTrue( date.getMinute() == 56 );
    assertTrue( date.getSecond() == 11 );
    assertTrue( date.getSignOffset() == 0 );
    assertTrue( date.getHoursOffset() == 0 );
    assertTrue( date.getMinutesOffset() == 0 );
    assertTrue(date.getDateAsString().equals( "2005-02-02T14:56:11Z"));
    date = history.getModifiedDate();
    assertTrue( date.getYear() == 2006 );
    assertTrue( date.getMonth() == 5 );
    assertTrue( date.getDay() == 30 );
    assertTrue( date.getHour() == 10 );
    assertTrue( date.getMinute() == 46 );
    assertTrue( date.getSecond() == 2 );
    assertTrue( date.getSignOffset() == 0 );
    assertTrue( date.getHoursOffset() == 0 );
    assertTrue( date.getMinutesOffset() == 0 );
    assertTrue(date.getDateAsString().equals( "2006-05-30T10:46:02Z"));
    date = history.getModifiedDate(1);
    assertTrue( date.getYear() == 2007 );
    assertTrue( date.getMonth() == 1 );
    assertTrue( date.getDay() == 16 );
    assertTrue( date.getHour() == 15 );
    assertTrue( date.getMinute() == 31 );
    assertTrue( date.getSecond() == 52 );
    assertTrue( date.getSignOffset() == 0 );
    assertTrue( date.getHoursOffset() == 0 );
    assertTrue( date.getMinutesOffset() == 0 );
    assertTrue(date.getDateAsString().equals( "2007-01-16T15:31:52Z"));
  }

  public void test_RDFAnnotation2_modelWithHistoryAndCVTerms()
  {
    ModelHistory h = new ModelHistory();
    ModelCreator c = new ModelCreator();
    c.setFamilyName("Keating");
    c.setGivenName("Sarah");
    h.addCreator(c);
    Date d = new Date(2008,11,17,18,37,0,0,0,0);
    h.setCreatedDate(d);
    h.setModifiedDate(d);
    m2.unsetModelHistory();
    m2.setModelHistory(h);
    CVTerm cv = new CVTerm();
    cv.setQualifierType(libsbml.BIOLOGICAL_QUALIFIER);
    cv.setBiologicalQualifierType(libsbml.BQB_IS_VERSION_OF);
    cv.addResource("http://www.geneontology.org/#GO:0005892");
    m2.addCVTerm(cv);
    XMLNode ann = RDFAnnotationParser.parseModelHistory(m2);
    String expected = "<annotation>\n" + 
    "  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n" + 
    "    <rdf:Description rdf:about=\"#_000001\">\n" + 
    "      <dc:creator rdf:parseType=\"Resource\">\n" + 
    "        <rdf:Bag>\n" + 
    "          <rdf:li rdf:parseType=\"Resource\">\n" + 
    "            <vCard:N rdf:parseType=\"Resource\">\n" + 
    "              <vCard:Family>Keating</vCard:Family>\n" + 
    "              <vCard:Given>Sarah</vCard:Given>\n" + 
    "            </vCard:N>\n" + 
    "          </rdf:li>\n" + 
    "        </rdf:Bag>\n" + 
    "      </dc:creator>\n" + 
    "      <dcterms:created rdf:parseType=\"Resource\">\n" + 
    "        <dcterms:W3CDTF>2008-11-17T18:37:00Z</dcterms:W3CDTF>\n" + 
    "      </dcterms:created>\n" + 
    "      <dcterms:modified rdf:parseType=\"Resource\">\n" + 
    "        <dcterms:W3CDTF>2008-11-17T18:37:00Z</dcterms:W3CDTF>\n" + 
    "      </dcterms:modified>\n" + 
    "      <bqbiol:isVersionOf>\n" + 
    "        <rdf:Bag>\n" + 
    "          <rdf:li rdf:resource=\"http://www.geneontology.org/#GO:0005892\"/>\n" + 
    "        </rdf:Bag>\n" + 
    "      </bqbiol:isVersionOf>\n" + 
    "    </rdf:Description>\n" + 
    "  </rdf:RDF>\n" + 
    "</annotation>";
    if (ann != null);
    {
      assertEquals( true, equals(expected,ann.toXMLString()) );
    }
    {
    }
  }

  public void test_RDFAnnotation2_modelWithHistoryAndMultipleModifiedDates()
  {
    ModelHistory h = new ModelHistory();
    ModelCreator c = new ModelCreator();
    c.setFamilyName("Keating");
    c.setGivenName("Sarah");
    h.addCreator(c);
    Date d = new Date(2005,2,2,14,56,11);
    h.setCreatedDate(d);
    h.addModifiedDate(d);
    h.addModifiedDate(d);
    m2.unsetModelHistory();
    m2.setModelHistory(h);
    XMLNode ann = RDFAnnotationParser.parseModelHistory(m2);
    String expected = "<annotation>\n" + 
    "  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n" + 
    "    <rdf:Description rdf:about=\"#_000001\">\n" + 
    "      <dc:creator rdf:parseType=\"Resource\">\n" + 
    "        <rdf:Bag>\n" + 
    "          <rdf:li rdf:parseType=\"Resource\">\n" + 
    "            <vCard:N rdf:parseType=\"Resource\">\n" + 
    "              <vCard:Family>Keating</vCard:Family>\n" + 
    "              <vCard:Given>Sarah</vCard:Given>\n" + 
    "            </vCard:N>\n" + 
    "          </rdf:li>\n" + 
    "        </rdf:Bag>\n" + 
    "      </dc:creator>\n" + 
    "      <dcterms:created rdf:parseType=\"Resource\">\n" + 
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n" + 
    "      </dcterms:created>\n" + 
    "      <dcterms:modified rdf:parseType=\"Resource\">\n" + 
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n" + 
    "      </dcterms:modified>\n" + 
    "      <dcterms:modified rdf:parseType=\"Resource\">\n" + 
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n" + 
    "      </dcterms:modified>\n" + 
    "    </rdf:Description>\n" + 
    "  </rdf:RDF>\n" + 
    "</annotation>";
    assertEquals( true, equals(expected,ann.toXMLString()) );
  }

  public void test_RDFAnnotation2_modelWithHistoryWithCharacterReference()
  {
    ModelHistory h = new ModelHistory();
    ModelCreator c = new ModelCreator();
    c.setFamilyName("Dr&#228;ger");
    c.setGivenName("Andreas");
    h.addCreator(c);
    Date d = new Date(2005,2,2,14,56,11);
    h.setCreatedDate(d);
    h.addModifiedDate(d);
    m2.unsetModelHistory();
    m2.setModelHistory(h);
    XMLNode ann = RDFAnnotationParser.parseModelHistory(m2);
    String expected = "<annotation>\n" + 
    "  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n" + 
    "    <rdf:Description rdf:about=\"#_000001\">\n" + 
    "      <dc:creator rdf:parseType=\"Resource\">\n" + 
    "        <rdf:Bag>\n" + 
    "          <rdf:li rdf:parseType=\"Resource\">\n" + 
    "            <vCard:N rdf:parseType=\"Resource\">\n" + 
    "              <vCard:Family>Dr&#228;ger</vCard:Family>\n" + 
    "              <vCard:Given>Andreas</vCard:Given>\n" + 
    "            </vCard:N>\n" + 
    "          </rdf:li>\n" + 
    "        </rdf:Bag>\n" + 
    "      </dc:creator>\n" + 
    "      <dcterms:created rdf:parseType=\"Resource\">\n" + 
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n" + 
    "      </dcterms:created>\n" + 
    "      <dcterms:modified rdf:parseType=\"Resource\">\n" + 
    "        <dcterms:W3CDTF>2005-02-02T14:56:11Z</dcterms:W3CDTF>\n" + 
    "      </dcterms:modified>\n" + 
    "    </rdf:Description>\n" + 
    "  </rdf:RDF>\n" + 
    "</annotation>";
    assertEquals( true, equals(expected,ann.toXMLString()) );
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
