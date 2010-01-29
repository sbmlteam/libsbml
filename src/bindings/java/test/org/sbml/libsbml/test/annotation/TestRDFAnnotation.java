/*
 *
 * @file    TestRDFAnnotation.java
 * @brief   fomula units data unit tests
 *
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $HeadURL$
 *
 * This test file was converted from src/sbml/test/TestRDFAnnotation.cpp
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

public class TestRDFAnnotation {

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
  private SBMLDocument d;
  private Model m;


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
    String filename = "../../annotation/test/test-data/annotation.xml";
    d = libsbml.readSBML(filename);
    m = d.getModel();
  }

  protected void tearDown() throws Exception
  {
  }

  public void test_RDFAnnotation_delete()
  {
    XMLNode node = RDFAnnotationParser.parseCVTerms(m.getCompartment(0));
    XMLNode n1 = RDFAnnotationParser.deleteRDFAnnotation(node);
    String expected =  "<annotation/>";;
    assertTrue( n1.getNumChildren() == 0 );
    assertTrue( n1.getName().equals( "annotation") );
    assertEquals( true, equals(expected,n1.toXMLString()) );
    node = null;
  }

  public void test_RDFAnnotation_deleteWithOther()
  {
    Compartment c = m.getCompartment(1);
    XMLNode node = RDFAnnotationParser.deleteRDFAnnotation(c.getAnnotation());
    String expected = "<annotation>\n" + "  <jd2:JDesignerLayout version=\"2.0\" MajorVersion=\"2\" MinorVersion=\"0\" BuildVersion=\"41\">\n" + 
    "    <jd2:header>\n" + 
    "      <jd2:VersionHeader JDesignerVersion=\"2.0\"/>\n" + 
    "      <jd2:ModelHeader Author=\"Mr Untitled\" ModelVersion=\"0.0\" ModelTitle=\"untitled\"/>\n" + 
    "      <jd2:TimeCourseDetails timeStart=\"0\" timeEnd=\"10\" numberOfPoints=\"1000\"/>\n" + 
    "    </jd2:header>\n" + 
    "  </jd2:JDesignerLayout>\n" + 
    "</annotation>";
    assertEquals( true, equals(expected,node.toXMLString()) );
  }

  public void test_RDFAnnotation_deleteWithOutOther()
  {
    Compartment c = m.getCompartment(2);
    XMLNode node = c.getAnnotation();
    String expected = "<annotation>\n" + "  <jd2:JDesignerLayout version=\"2.0\" MajorVersion=\"2\" MinorVersion=\"0\" BuildVersion=\"41\">\n" + 
    "    <jd2:header>\n" + 
    "      <jd2:VersionHeader JDesignerVersion=\"2.0\"/>\n" + 
    "      <jd2:ModelHeader Author=\"Mr Untitled\" ModelVersion=\"0.0\" ModelTitle=\"untitled\"/>\n" + 
    "      <jd2:TimeCourseDetails timeStart=\"0\" timeEnd=\"10\" numberOfPoints=\"1000\"/>\n" + 
    "    </jd2:header>\n" + 
    "  </jd2:JDesignerLayout>\n" + 
    "</annotation>";
    assertEquals( true, equals(expected,node.toXMLString()) );
  }

  public void test_RDFAnnotation_getModelHistory()
  {
    assertTrue( ! (m == null) );
    ModelHistory history = m.getModelHistory();
    assertTrue( history != null );
    ModelCreator mc = history.getCreator(0);
    assertTrue(mc.getFamilyName().equals( "Le Novere"));
    assertTrue(mc.getGivenName().equals( "Nicolas"));
    assertTrue(mc.getEmail().equals( "lenov@ebi.ac.uk"));
    assertTrue(mc.getOrganisation().equals( "EMBL-EBI"));
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
  }

  public void test_RDFAnnotation_parseCVTerms()
  {
    XMLNode node = RDFAnnotationParser.parseCVTerms(m.getCompartment(0));
    assertTrue( node.getNumChildren() == 1 );
    XMLNode rdf = node.getChild(0);
    assertTrue(rdf.getName().equals( "RDF"));
    assertTrue(rdf.getPrefix().equals( "rdf"));
    assertTrue(rdf.getURI().equals( "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    assertTrue( rdf.getNumChildren() == 1 );
    XMLNode desc = rdf.getChild(0);
    assertTrue(desc.getName().equals( "Description"));
    assertTrue(desc.getPrefix().equals( "rdf"));
    assertTrue(desc.getURI().equals( "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    assertTrue( desc.getNumChildren() == 1 );
    XMLNode is1 = desc.getChild(0);
    assertTrue(is1.getName().equals( "is"));
    assertTrue(is1.getPrefix().equals( "bqbiol"));
    assertTrue( is1.getNumChildren() == 1 );
    XMLNode Bag = is1.getChild(0);
    assertTrue(Bag.getName().equals( "Bag"));
    assertTrue(Bag.getPrefix().equals( "rdf"));
    assertTrue(Bag.getURI().equals( "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    assertTrue( Bag.getNumChildren() == 4 );
    XMLNode li = Bag.getChild(0);
    assertTrue(li.getName().equals( "li"));
    assertTrue(li.getPrefix().equals( "rdf"));
    assertTrue(li.getURI().equals( "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    assertTrue( li.getNumChildren() == 0 );
    XMLNode li1 = Bag.getChild(1);
    assertTrue(li1.getName().equals( "li"));
    assertTrue(li1.getPrefix().equals( "rdf"));
    assertTrue(li1.getURI().equals( "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    assertTrue( li1.getNumChildren() == 0 );
    XMLNode li2 = Bag.getChild(2);
    assertTrue(li2.getName().equals( "li"));
    assertTrue(li2.getPrefix().equals( "rdf"));
    assertTrue(li2.getURI().equals( "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    assertTrue( li2.getNumChildren() == 0 );
    XMLNode li3 = Bag.getChild(3);
    assertTrue(li3.getName().equals( "li"));
    assertTrue(li3.getPrefix().equals( "rdf"));
    assertTrue(li3.getURI().equals( "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    assertTrue( li3.getNumChildren() == 0 );
    node = null;
  }

  public void test_RDFAnnotation_parseModelHistory()
  {
    XMLNode node = RDFAnnotationParser.parseModelHistory(m);
    assertTrue( node.getNumChildren() == 1 );
    XMLNode rdf = node.getChild(0);
    assertTrue(rdf.getName().equals( "RDF"));
    assertTrue(rdf.getPrefix().equals( "rdf"));
    assertTrue(rdf.getURI().equals( "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    assertTrue( rdf.getNumChildren() == 1 );
    XMLNode desc = rdf.getChild(0);
    assertTrue(desc.getName().equals( "Description"));
    assertTrue(desc.getPrefix().equals( "rdf"));
    assertTrue(desc.getURI().equals( "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    assertTrue( desc.getNumChildren() == 3 );
    XMLNode creator = desc.getChild(0);
    assertTrue(creator.getName().equals( "creator"));
    assertTrue(creator.getPrefix().equals( "dc"));
    assertTrue(creator.getURI().equals( "http://purl.org/dc/elements/1.1/"));
    assertTrue( creator.getNumChildren() == 1 );
    XMLNode Bag = creator.getChild(0);
    assertTrue(Bag.getName().equals( "Bag"));
    assertTrue(Bag.getPrefix().equals( "rdf"));
    assertTrue(Bag.getURI().equals( "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    assertTrue( Bag.getNumChildren() == 1 );
    XMLNode li = Bag.getChild(0);
    assertTrue(li.getName().equals( "li"));
    assertTrue(li.getPrefix().equals( "rdf"));
    assertTrue(li.getURI().equals( "http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    assertTrue( li.getNumChildren() == 3 );
    XMLNode N = li.getChild(0);
    assertTrue(N.getName().equals( "N"));
    assertTrue(N.getPrefix().equals( "vCard"));
    assertTrue(N.getURI().equals( "http://www.w3.org/2001/vcard-rdf/3.0#"));
    assertTrue( N.getNumChildren() == 2 );
    XMLNode Family = N.getChild(0);
    assertTrue(Family.getName().equals( "Family"));
    assertTrue(Family.getPrefix().equals( "vCard"));
    assertTrue(Family.getURI().equals( "http://www.w3.org/2001/vcard-rdf/3.0#"));
    assertTrue( Family.getNumChildren() == 1 );
    XMLNode Given = N.getChild(1);
    assertTrue(Given.getName().equals( "Given"));
    assertTrue(Given.getPrefix().equals( "vCard"));
    assertTrue(Given.getURI().equals( "http://www.w3.org/2001/vcard-rdf/3.0#"));
    assertTrue( Given.getNumChildren() == 1 );
    XMLNode EMAIL = li.getChild(1);
    assertTrue(EMAIL.getName().equals( "EMAIL"));
    assertTrue(EMAIL.getPrefix().equals( "vCard"));
    assertTrue(EMAIL.getURI().equals( "http://www.w3.org/2001/vcard-rdf/3.0#"));
    assertTrue( EMAIL.getNumChildren() == 1 );
    XMLNode ORG = li.getChild(2);
    assertTrue(ORG.getName().equals( "ORG"));
    assertTrue(ORG.getPrefix().equals( "vCard"));
    assertTrue(ORG.getURI().equals( "http://www.w3.org/2001/vcard-rdf/3.0#"));
    assertTrue( ORG.getNumChildren() == 1 );
    XMLNode Orgname = ORG.getChild(0);
    assertTrue(Orgname.getName().equals( "Orgname"));
    assertTrue(Orgname.getPrefix().equals( "vCard"));
    assertTrue(Orgname.getURI().equals( "http://www.w3.org/2001/vcard-rdf/3.0#"));
    assertTrue( Orgname.getNumChildren() == 1 );
    XMLNode created = desc.getChild(1);
    assertTrue(created.getName().equals( "created"));
    assertTrue(created.getPrefix().equals( "dcterms"));
    assertTrue(created.getURI().equals( "http://purl.org/dc/terms/"));
    assertTrue( created.getNumChildren() == 1 );
    XMLNode cr_date = created.getChild(0);
    assertTrue(cr_date.getName().equals( "W3CDTF"));
    assertTrue(cr_date.getPrefix().equals( "dcterms"));
    assertTrue(cr_date.getURI().equals( "http://purl.org/dc/terms/"));
    assertTrue( cr_date.getNumChildren() == 1 );
    XMLNode modified = desc.getChild(2);
    assertTrue(modified.getName().equals( "modified"));
    assertTrue(modified.getPrefix().equals( "dcterms"));
    assertTrue(modified.getURI().equals( "http://purl.org/dc/terms/"));
    assertTrue( modified.getNumChildren() == 1 );
    XMLNode mo_date = created.getChild(0);
    assertTrue(mo_date.getName().equals( "W3CDTF"));
    assertTrue(mo_date.getPrefix().equals( "dcterms"));
    assertTrue(mo_date.getURI().equals( "http://purl.org/dc/terms/"));
    assertTrue( mo_date.getNumChildren() == 1 );
    node = null;
  }

  public void test_RDFAnnotation_recreate()
  {
    Compartment c = m.getCompartment(1);
    String expected = "<compartment id=\"A\">\n" + 
    "  <annotation>\n" + 
    "    <jd2:JDesignerLayout version=\"2.0\" MajorVersion=\"2\" MinorVersion=\"0\" BuildVersion=\"41\">\n" + 
    "      <jd2:header>\n" + 
    "        <jd2:VersionHeader JDesignerVersion=\"2.0\"/>\n" + 
    "        <jd2:ModelHeader Author=\"Mr Untitled\" ModelVersion=\"0.0\" ModelTitle=\"untitled\"/>\n" + 
    "        <jd2:TimeCourseDetails timeStart=\"0\" timeEnd=\"10\" numberOfPoints=\"1000\"/>\n" + 
    "      </jd2:header>\n" + 
    "    </jd2:JDesignerLayout>\n" + 
    "    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n" + 
    "      <rdf:Description rdf:about=\"#\">\n" + 
    "        <bqbiol:is>\n" + 
    "          <rdf:Bag>\n" + 
    "            <rdf:li rdf:resource=\"http://www.geneontology.org/#GO:0007274\"/>\n" + 
    "          </rdf:Bag>\n" + 
    "        </bqbiol:is>\n" + 
    "      </rdf:Description>\n" + 
    "    </rdf:RDF>\n" + 
    "  </annotation>\n" + 
    "</compartment>";
    assertEquals( true, equals(expected,c.toSBML()) );
  }

  public void test_RDFAnnotation_recreateFromEmpty()
  {
    Compartment c = m.getCompartment(3);
    String expected = "<compartment id=\"C\">\n" + 
    "  <annotation>\n" + 
    "    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:vCard=\"http://www.w3.org/2001/vcard-rdf/3.0#\" xmlns:bqbiol=\"http://biomodels.net/biology-qualifiers/\" xmlns:bqmodel=\"http://biomodels.net/model-qualifiers/\">\n" + 
    "      <rdf:Description rdf:about=\"#\">\n" + 
    "        <bqbiol:is>\n" + 
    "          <rdf:Bag>\n" + 
    "            <rdf:li rdf:resource=\"http://www.geneontology.org/#GO:0007274\"/>\n" + 
    "          </rdf:Bag>\n" + 
    "        </bqbiol:is>\n" + 
    "      </rdf:Description>\n" + 
    "    </rdf:RDF>\n" + 
    "  </annotation>\n" + 
    "</compartment>";
    assertEquals( true, equals(expected,c.toSBML()) );
  }

  public void test_RDFAnnotation_recreateWithOutOther()
  {
    Compartment c = m.getCompartment(2);
    String expected = "<compartment id=\"B\">\n" + 
    "  <annotation>\n" + 
    "    <jd2:JDesignerLayout version=\"2.0\" MajorVersion=\"2\" MinorVersion=\"0\" BuildVersion=\"41\">\n" + 
    "      <jd2:header>\n" + 
    "        <jd2:VersionHeader JDesignerVersion=\"2.0\"/>\n" + 
    "        <jd2:ModelHeader Author=\"Mr Untitled\" ModelVersion=\"0.0\" ModelTitle=\"untitled\"/>\n" + 
    "        <jd2:TimeCourseDetails timeStart=\"0\" timeEnd=\"10\" numberOfPoints=\"1000\"/>\n" + 
    "      </jd2:header>\n" + 
    "    </jd2:JDesignerLayout>\n" + 
    "  </annotation>\n" + 
    "</compartment>";
    assertEquals( true, equals(expected,c.toSBML()) );
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
