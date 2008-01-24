/*
 *
 * @file    TestWriteSBML.java
 * @brief   Write SBML unit tests
 * @author  Akiya Jouraku (Java conversion)
 * @author  Ben Bornstein 
 *
 * $Id$
 * $Source$
 *
 * This test file was converted from src/sbml/test/TestWriteSBML.cpp
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

public class TestWriteSBML {

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

  private SBMLDocument D;
  private String S;
  private OStringStream OSS;
  private XMLOutputStream XOS;

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

  public boolean equals(String s)
  {
    return s.equals(OSS.str());
  }

  public boolean equals(String s1, String s2)
  {
    return s1.equals(s2);
  }

  protected void setUp() throws Exception
  {
    D = new SBMLDocument();
    S = null;
    OSS = new OStringStream();
    XOS = new XMLOutputStream(OSS);
  }

  protected void tearDown() throws Exception
  {
    D = null;
    S = null;
    OSS = null;
    XOS = null;
  }

  public void test_WriteSBML_AlgebraicRule()
  {
    D.setLevelAndVersion(1,1);
    String expected = wrapXML("<algebraicRule formula=\"x + 1\"/>");
    AlgebraicRule r = new AlgebraicRule( "x + 1" );
    r.setSBMLDocument(D);
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_AlgebraicRule_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<algebraicRule>\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <plus/>\n" + 
    "      <ci> x </ci>\n" + 
    "      <cn type=\"integer\"> 1 </cn>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</algebraicRule>");
    AlgebraicRule r = new AlgebraicRule( "x + 1" );
    r.setSBMLDocument(D);
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Compartment()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<compartment name=\"A\" volume=\"2.1\" outside=\"B\"/>");
    Compartment c = new Compartment( "A" );
    c.setSize(2.1);
    c.setOutside("B");
    c.setSBMLDocument(D);
    c.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_CompartmentVolumeRule()
  {
    D.setLevelAndVersion(1,1);
    String expected = wrapXML("<compartmentVolumeRule " + 
    "formula=\"v + c\" type=\"rate\" compartment=\"c\"/>");
    D.createModel();
    D.getModel().createCompartment().setId("c");
    Rule r = D.getModel().createRateRule();
    r.setVariable("c");
    r.setFormula("v + c");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_CompartmentVolumeRule_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<assignmentRule variable=\"c\">\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <plus/>\n" + 
    "      <ci> v </ci>\n" + 
    "      <ci> c </ci>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</assignmentRule>");
    D.createModel();
    D.getModel().createCompartment().setId("c");
    Rule r = D.getModel().createAssignmentRule();
    r.setVariable("c");
    r.setFormula("v + c");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_CompartmentVolumeRule_defaults()
  {
    D.setLevelAndVersion(1,1);
    String expected = wrapXML("<compartmentVolumeRule formula=\"v + c\" compartment=\"c\"/>");
    D.createModel();
    D.getModel().createCompartment().setId("c");
    Rule r = D.getModel().createAssignmentRule();
    r.setVariable("c");
    r.setFormula("v + c");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Compartment_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<compartment id=\"M\" spatialDimensions=\"2\" size=\"2.5\"/>");
    Compartment c = new Compartment( "M" );
    c.setSize(2.5);
    c.setSpatialDimensions(2);
    c.setSBMLDocument(D);
    c.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Compartment_L2v1_constant()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<compartment id=\"cell\" size=\"1.2\" constant=\"false\"/>");
    Compartment c = new Compartment( "cell" );
    c.setSize(1.2);
    c.setConstant(false);
    c.setSBMLDocument(D);
    c.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Compartment_L2v1_unsetSize()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<compartment id=\"A\"/>");
    Compartment c = new Compartment();
    c.setId("A");
    c.unsetSize();
    c.setSBMLDocument(D);
    c.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Compartment_unsetVolume()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<compartment name=\"A\"/>");
    Compartment c = new Compartment();
    c.setId("A");
    c.unsetVolume();
    c.setSBMLDocument(D);
    c.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Event()
  {
    String expected = wrapXML("<event id=\"e\"/>");
    Event e = new Event();
    e.setId("e");
    e.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Event_both()
  {
    String expected = wrapXML("<event id=\"e\">\n" + 
    "  <trigger>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <apply>\n" + 
    "        <leq/>\n" + 
    "        <ci> P1 </ci>\n" + 
    "        <ci> t </ci>\n" + 
    "      </apply>\n" + 
    "    </math>\n" + 
    "  </trigger>\n" + 
    "  <delay>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <cn type=\"integer\"> 5 </cn>\n" + 
    "    </math>\n" + 
    "  </delay>\n" + 
    "</event>");
    Event e = new Event( "e" );
    ASTNode node1 = libsbml.parseFormula("leq(P1,t)");
    Trigger t = new Trigger( node1 );
    ASTNode node = libsbml.parseFormula("5");
    Delay d = new Delay( node );
    e.setDelay(d);
    e.setTrigger(t);
    e.setTimeUnits("second");
    e.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Event_delay()
  {
    String expected = wrapXML("<event id=\"e\">\n" + 
    "  <delay>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <cn type=\"integer\"> 5 </cn>\n" + 
    "    </math>\n" + 
    "  </delay>\n" + 
    "</event>");
    Event e = new Event( "e" );
    ASTNode node = libsbml.parseFormula("5");
    Delay d = new Delay( node );
    e.setDelay(d);
    e.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Event_full()
  {
    String expected = wrapXML("<event id=\"e\">\n" + 
    "  <trigger>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <apply>\n" + 
    "        <leq/>\n" + 
    "        <ci> P1 </ci>\n" + 
    "        <ci> t </ci>\n" + 
    "      </apply>\n" + 
    "    </math>\n" + 
    "  </trigger>\n" + 
    "  <listOfEventAssignments>\n" + 
    "    <eventAssignment variable=\"k2\">\n" + 
    "      <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "        <cn type=\"integer\"> 0 </cn>\n" + 
    "      </math>\n" + 
    "    </eventAssignment>\n" + 
    "  </listOfEventAssignments>\n" + 
    "</event>");
    Event e = new Event( "e" );
    ASTNode node = libsbml.parseFormula("leq(P1,t)");
    Trigger t = new Trigger( node );
    ASTNode math = libsbml.parseFormula("0");
    EventAssignment ea = new EventAssignment( "k2",math );
    e.setTrigger(t);
    e.addEventAssignment(ea);
    e.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Event_trigger()
  {
    String expected = wrapXML("<event id=\"e\">\n" + 
    "  <trigger>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <apply>\n" + 
    "        <leq/>\n" + 
    "        <ci> P1 </ci>\n" + 
    "        <ci> t </ci>\n" + 
    "      </apply>\n" + 
    "    </math>\n" + 
    "  </trigger>\n" + 
    "</event>");
    Event e = new Event( "e" );
    ASTNode node = libsbml.parseFormula("leq(P1,t)");
    Trigger t = new Trigger( node );
    e.setTrigger(t);
    e.setTimeUnits("second");
    e.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_FunctionDefinition()
  {
    String expected = wrapXML("<functionDefinition id=\"pow3\">\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <lambda>\n" + 
    "      <bvar>\n" + 
    "        <ci> x </ci>\n" + 
    "      </bvar>\n" + 
    "      <apply>\n" + 
    "        <power/>\n" + 
    "        <ci> x </ci>\n" + 
    "        <cn type=\"integer\"> 3 </cn>\n" + 
    "      </apply>\n" + 
    "    </lambda>\n" + 
    "  </math>\n" + 
    "</functionDefinition>");
    FunctionDefinition fd = new FunctionDefinition( "pow3", "lambda(x, x^3)" );
    fd.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_INF()
  {
    String expected = wrapXML("<parameter id=\"p\" value=\"INF\"/>");
    Parameter p = new Parameter( "p",util_PosInf() );
    p.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_KineticLaw()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<kineticLaw formula=\"k * e\" timeUnits=\"second\" " + 
    "substanceUnits=\"item\"/>");
    KineticLaw kl = new KineticLaw( "k * e", "second", "item" );
    kl.setSBMLDocument(D);
    kl.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_KineticLaw_ListOfParameters()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<kineticLaw formula=\"nk * e\" timeUnits=\"second\" " + 
    "substanceUnits=\"item\">\n" + 
    "  <listOfParameters>\n" + 
    "    <parameter name=\"n\" value=\"1.2\"/>\n" + 
    "  </listOfParameters>\n" + 
    "</kineticLaw>");
    KineticLaw kl = new KineticLaw( "nk * e", "second", "item" );
    kl.setSBMLDocument(D);
    Parameter p = new Parameter( "n",1.2 );
    kl.addParameter(p);
    kl.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_KineticLaw_skipOptional()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<kineticLaw formula=\"k * e\"/>");
    KineticLaw kl = new KineticLaw( "k * e" );
    kl.setSBMLDocument(D);
    kl.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Model()
  {
    D.setLevelAndVersion(1,1);
    String expected = wrapSBML_L1v1("  <model name=\"Branch\"/>\n");
    D.createModel("Branch");
    S = libsbml.writeSBMLToString(D);
    assertEquals( true, equals(expected,S) );
  }

  public void test_WriteSBML_Model_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapSBML_L2v1("  <model id=\"Branch\"/>\n");
    D.createModel("Branch");
    S = libsbml.writeSBMLToString(D);
    assertEquals( true, equals(expected,S) );
  }

  public void test_WriteSBML_Model_L2v1_skipOptional()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapSBML_L2v1("  <model/>\n");
    D.createModel();
    S = libsbml.writeSBMLToString(D);
    assertEquals( true, equals(expected,S) );
  }

  public void test_WriteSBML_Model_skipOptional()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapSBML_L1v2("  <model/>\n");
    D.createModel();
    S = libsbml.writeSBMLToString(D);
    assertEquals( true, equals(expected,S) );
  }

  public void test_WriteSBML_NaN()
  {
    String expected = wrapXML("<parameter id=\"p\" value=\"NaN\"/>");
    Parameter p = new Parameter( "p",util_NaN() );
    p.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_NegINF()
  {
    String expected = wrapXML("<parameter id=\"p\" value=\"-INF\"/>");
    Parameter p = new Parameter( "p",util_NegInf() );
    p.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Parameter()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<parameter name=\"Km1\" value=\"2.3\" units=\"second\"/>");
    Parameter p = new Parameter( "Km1",2.3, "second" );
    p.setSBMLDocument(D);
    p.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_ParameterRule()
  {
    D.setLevelAndVersion(1,1);
    String expected = wrapXML("<parameterRule " + 
    "formula=\"p * t\" type=\"rate\" name=\"p\"/>");
    D.createModel();
    D.getModel().createParameter().setId("p");
    Rule r = D.getModel().createRateRule();
    r.setVariable("p");
    r.setFormula("p * t");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_ParameterRule_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<rateRule variable=\"p\">\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <times/>\n" + 
    "      <ci> p </ci>\n" + 
    "      <ci> t </ci>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</rateRule>");
    D.createModel();
    D.getModel().createParameter().setId("p");
    Rule r = D.getModel().createRateRule();
    r.setVariable("p");
    r.setFormula("p * t");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_ParameterRule_defaults()
  {
    D.setLevelAndVersion(1,1);
    String expected = wrapXML("<parameterRule formula=\"p * t\" name=\"p\"/>");
    D.createModel();
    D.getModel().createParameter().setId("p");
    Rule r = D.getModel().createAssignmentRule();
    r.setVariable("p");
    r.setFormula("p * t");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Parameter_L1v1_required()
  {
    D.setLevelAndVersion(1,1);
    String expected = wrapXML("<parameter name=\"Km1\" value=\"NaN\"/>");
    Parameter p = new Parameter();
    p.setId("Km1");
    p.unsetValue();
    p.setSBMLDocument(D);
    p.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Parameter_L1v2_skipOptional()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<parameter name=\"Km1\"/>");
    Parameter p = new Parameter();
    p.setId("Km1");
    p.unsetValue();
    p.setSBMLDocument(D);
    p.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Parameter_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<parameter id=\"Km1\" value=\"2.3\" units=\"second\"/>");
    Parameter p = new Parameter( "Km1",2.3, "second" );
    p.setSBMLDocument(D);
    p.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Parameter_L2v1_constant()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<parameter id=\"x\" constant=\"false\"/>");
    Parameter p = new Parameter( "x" );
    p.setConstant(false);
    p.setSBMLDocument(D);
    p.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Parameter_L2v1_skipOptional()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<parameter id=\"Km1\"/>");
    Parameter p = new Parameter( "Km1" );
    p.setSBMLDocument(D);
    p.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Reaction()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<reaction name=\"r\" reversible=\"false\" fast=\"true\"/>");
    Reaction r = new Reaction( "r", "",null,false );
    r.setFast(true);
    r.setSBMLDocument(D);
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Reaction_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<reaction id=\"r\" reversible=\"false\"/>");
    Reaction r = new Reaction( "r", "",null,false );
    r.setSBMLDocument(D);
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Reaction_L2v1_full()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<reaction id=\"v1\">\n" + 
    "  <listOfReactants>\n" + 
    "    <speciesReference species=\"x0\"/>\n" + 
    "  </listOfReactants>\n" + 
    "  <listOfProducts>\n" + 
    "    <speciesReference species=\"s1\"/>\n" + 
    "  </listOfProducts>\n" + 
    "  <listOfModifiers>\n" + 
    "    <modifierSpeciesReference species=\"m1\"/>\n" + 
    "  </listOfModifiers>\n" + 
    "  <kineticLaw>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <apply>\n" + 
    "        <divide/>\n" + 
    "        <apply>\n" + 
    "          <times/>\n" + 
    "          <ci> vm </ci>\n" + 
    "          <ci> s1 </ci>\n" + 
    "        </apply>\n" + 
    "        <apply>\n" + 
    "          <plus/>\n" + 
    "          <ci> km </ci>\n" + 
    "          <ci> s1 </ci>\n" + 
    "        </apply>\n" + 
    "      </apply>\n" + 
    "    </math>\n" + 
    "  </kineticLaw>\n" + 
    "</reaction>");
    D.createModel();
    Reaction r = D.getModel().createReaction();
    r.setId("v1");
    r.setReversible(true);
    r.createReactant().setSpecies("x0");
    r.createProduct().setSpecies("s1");
    r.createModifier().setSpecies("m1");
    r.createKineticLaw().setFormula("(vm * s1)/(km + s1)");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Reaction_defaults()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<reaction name=\"r\"/>");
    Reaction r = new Reaction();
    r.setId("r");
    r.setSBMLDocument(D);
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Reaction_full()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<reaction name=\"v1\">\n" + 
    "  <listOfReactants>\n" + 
    "    <speciesReference species=\"x0\"/>\n" + 
    "  </listOfReactants>\n" + 
    "  <listOfProducts>\n" + 
    "    <speciesReference species=\"s1\"/>\n" + 
    "  </listOfProducts>\n" + 
    "  <kineticLaw formula=\"(vm * s1)/(km + s1)\"/>\n" + 
    "</reaction>");
    D.createModel();
    Reaction r = D.getModel().createReaction();
    r.setId("v1");
    r.setReversible(true);
    r.createReactant().setSpecies("x0");
    r.createProduct().setSpecies("s1");
    r.createKineticLaw().setFormula("(vm * s1)/(km + s1)");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_SBMLDocument_L1v1()
  {
    D.setLevelAndVersion(1,1);
    String expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level1\" " + 
    "level=\"1\" version=\"1\"/>\n");
    S = libsbml.writeSBMLToString(D);
    assertEquals( true, equals(expected,S) );
  }

  public void test_WriteSBML_SBMLDocument_L1v2()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level1\" " + 
    "level=\"1\" version=\"2\"/>\n");
    S = libsbml.writeSBMLToString(D);
    assertEquals( true, equals(expected,S) );
  }

  public void test_WriteSBML_SBMLDocument_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\"/>\n");
    S = libsbml.writeSBMLToString(D);
    assertEquals( true, equals(expected,S) );
  }

  public void test_WriteSBML_SBMLDocument_L2v2()
  {
    D.setLevelAndVersion(2,2);
    String expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level2/version2\" " + 
    "level=\"2\" version=\"2\"/>\n");
    S = libsbml.writeSBMLToString(D);
    assertEquals( true, equals(expected,S) );
  }

  public void test_WriteSBML_Species()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"" + 
    " units=\"mole\" boundaryCondition=\"true\" charge=\"2\"/>");
    Species s = new Species( "Ca2" );
    s.setCompartment("cell");
    s.setInitialAmount(0.7);
    s.setUnits("mole");
    s.setBoundaryCondition(true);
    s.setCharge(2);
    s.setSBMLDocument(D);
    s.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_SpeciesConcentrationRule()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<speciesConcentrationRule " + 
    "formula=\"t * s\" type=\"rate\" species=\"s\"/>");
    D.createModel();
    D.getModel().createSpecies().setId("s");
    Rule r = D.getModel().createRateRule();
    r.setVariable("s");
    r.setFormula("t * s");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_SpeciesConcentrationRule_L1v1()
  {
    D.setLevelAndVersion(1,1);
    String expected = wrapXML("<specieConcentrationRule formula=\"t * s\" specie=\"s\"/>");
    D.createModel();
    D.getModel().createSpecies().setId("s");
    Rule r = D.getModel().createAssignmentRule();
    r.setVariable("s");
    r.setFormula("t * s");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_SpeciesConcentrationRule_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<assignmentRule variable=\"s\">\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <times/>\n" + 
    "      <ci> t </ci>\n" + 
    "      <ci> s </ci>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</assignmentRule>");
    D.createModel();
    D.getModel().createSpecies().setId("s");
    Rule r = D.getModel().createAssignmentRule();
    r.setVariable("s");
    r.setFormula("t * s");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_SpeciesConcentrationRule_defaults()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<speciesConcentrationRule formula=\"t * s\" species=\"s\"/>");
    D.createModel();
    D.getModel().createSpecies().setId("s");
    Rule r = D.getModel().createAssignmentRule();
    r.setVariable("s");
    r.setFormula("t * s");
    r.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_SpeciesReference()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<speciesReference species=\"s\" stoichiometry=\"3\" denominator=\"2\"/>");
    SpeciesReference sr = new SpeciesReference( "s",3,2 );
    sr.setSBMLDocument(D);
    sr.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_SpeciesReference_L1v1()
  {
    D.setLevelAndVersion(1,1);
    String expected = wrapXML("<specieReference specie=\"s\" stoichiometry=\"3\" denominator=\"2\"/>");
    SpeciesReference sr = new SpeciesReference( "s",3,2 );
    sr.setSBMLDocument(D);
    sr.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_SpeciesReference_L2v1_1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<speciesReference species=\"s\">\n" + 
    "  <stoichiometryMath>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <cn type=\"rational\"> 3 <sep/> 2 </cn>\n" + 
    "    </math>\n" + 
    "  </stoichiometryMath>\n" + 
    "</speciesReference>");
    SpeciesReference sr = new SpeciesReference( "s",3,2 );
    sr.setSBMLDocument(D);
    sr.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_SpeciesReference_L2v1_2()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<speciesReference species=\"s\" stoichiometry=\"3.2\"/>");
    SpeciesReference sr = new SpeciesReference( "s",3.2 );
    sr.setSBMLDocument(D);
    sr.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_SpeciesReference_L2v1_3()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<speciesReference species=\"s\">\n" + 
    "  <stoichiometryMath>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <apply>\n" + 
    "        <divide/>\n" + 
    "        <cn type=\"integer\"> 1 </cn>\n" + 
    "        <ci> d </ci>\n" + 
    "      </apply>\n" + 
    "    </math>\n" + 
    "  </stoichiometryMath>\n" + 
    "</speciesReference>");
    SpeciesReference sr = new SpeciesReference( "s" );
    ASTNode math = libsbml.parseFormula("1/d");
    StoichiometryMath stoich = new StoichiometryMath();
    stoich.setMath(math);
    sr.setStoichiometryMath(stoich);
    sr.setSBMLDocument(D);
    sr.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_SpeciesReference_defaults()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<speciesReference species=\"s\"/>");
    SpeciesReference sr = new SpeciesReference( "s" );
    sr.setSBMLDocument(D);
    sr.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Species_L1v1()
  {
    D.setLevelAndVersion(1,1);
    String expected = wrapXML("<specie name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"" + 
    " units=\"mole\" boundaryCondition=\"true\" charge=\"2\"/>");
    Species s = new Species( "Ca2" );
    s.setCompartment("cell");
    s.setInitialAmount(0.7);
    s.setUnits("mole");
    s.setBoundaryCondition(true);
    s.setCharge(2);
    s.setSBMLDocument(D);
    s.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Species_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<species id=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\" " + 
    "substanceUnits=\"mole\" constant=\"true\"/>");
    Species s = new Species( "Ca2" );
    s.setCompartment("cell");
    s.setInitialAmount(0.7);
    s.setSubstanceUnits("mole");
    s.setConstant(true);
    s.setSBMLDocument(D);
    s.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Species_L2v1_skipOptional()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<species id=\"Ca2\" compartment=\"cell\"/>");
    Species s = new Species( "Ca2" );
    s.setCompartment("cell");
    s.setSBMLDocument(D);
    s.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Species_defaults()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"" + 
    " units=\"mole\" charge=\"2\"/>");
    Species s = new Species( "Ca2" );
    s.setCompartment("cell");
    s.setInitialAmount(0.7);
    s.setUnits("mole");
    s.setCharge(2);
    s.setSBMLDocument(D);
    s.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Species_skipOptional()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"/>");
    Species s = new Species( "Ca2" );
    s.setCompartment("cell");
    s.setInitialAmount(0.7);
    s.setSBMLDocument(D);
    s.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Unit()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<unit kind=\"kilogram\" exponent=\"2\" scale=\"-3\"/>");
    Unit u = new Unit( "kilogram",2,-3 );
    u.setSBMLDocument(D);
    u.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_UnitDefinition()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<unitDefinition name=\"mmls\"/>");
    UnitDefinition ud = new UnitDefinition( "mmls" );
    ud.setSBMLDocument(D);
    ud.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_UnitDefinition_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<unitDefinition id=\"mmls\"/>");
    UnitDefinition ud = new UnitDefinition( "mmls" );
    ud.setSBMLDocument(D);
    ud.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_UnitDefinition_L2v1_full()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<unitDefinition id=\"Fahrenheit\">\n" + 
    "  <listOfUnits>\n" + 
    "    <unit kind=\"Celsius\" multiplier=\"1.8\" offset=\"32\"/>\n" + 
    "  </listOfUnits>\n" + 
    "</unitDefinition>");
    Unit u1 = new Unit( "Celsius",1,0,1.8 );
    u1.setOffset(32);
    UnitDefinition ud = new UnitDefinition( "Fahrenheit" );
    ud.addUnit(u1);
    ud.setSBMLDocument(D);
    ud.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_UnitDefinition_full()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<unitDefinition name=\"mmls\">\n" + 
    "  <listOfUnits>\n" + 
    "    <unit kind=\"mole\" scale=\"-3\"/>\n" + 
    "    <unit kind=\"liter\" exponent=\"-1\"/>\n" + 
    "    <unit kind=\"second\" exponent=\"-1\"/>\n" + 
    "  </listOfUnits>\n" + 
    "</unitDefinition>");
    UnitDefinition ud = new UnitDefinition( "mmls" );
    Unit u1 = new Unit( "mole"  ,1,-3 );
    Unit u2 = new Unit( "liter" ,-1 );
    Unit u3 = new Unit( "second",-1 );
    ud.addUnit(u1);
    ud.addUnit(u2);
    ud.addUnit(u3);
    ud.setSBMLDocument(D);
    ud.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Unit_L2v1()
  {
    D.setLevelAndVersion(2,1);
    String expected = wrapXML("<unit kind=\"Celsius\" multiplier=\"1.8\" offset=\"32\"/>");
    Unit u = new Unit( "Celsius",1,0,1.8 );
    u.setOffset(32);
    u.setSBMLDocument(D);
    u.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Unit_defaults()
  {
    D.setLevelAndVersion(1,2);
    String expected = wrapXML("<unit kind=\"kilogram\"/>");
    Unit u = new Unit( "kilogram",1,0 );
    u.setSBMLDocument(D);
    u.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_Unit_l2v3()
  {
    D.setLevelAndVersion(2,3);
    String expected = wrapXML("<unit kind=\"kilogram\" exponent=\"2\" scale=\"-3\"/>");
    Unit u = new Unit( "kilogram",2,-3 );
    u.setOffset(32);
    u.setSBMLDocument(D);
    u.write(XOS);
    assertEquals( true, equals(expected) );
  }

  public void test_WriteSBML_error()
  {
    SBMLDocument d = new SBMLDocument();
    SBMLWriter w = new SBMLWriter();
    assertEquals( false, w.writeSBML(d, "/tmp/impossible/path/should/fail") );
    assertTrue( d.getNumErrors() == 1 );
    assertTrue( d.getError(0).getErrorId() == libsbml.XMLFileUnwritable );
    d = null;
    w = null;
  }

  public void test_WriteSBML_locale()
  {
    String expected = wrapXML("<parameter id=\"p\" value=\"3.31\"/>");
    Parameter p = new Parameter("p",3.31);
    p.write(XOS);
    assertEquals( true, equals(expected) );
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
