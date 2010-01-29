/// 
///  @file    TestReadSBML.cs
///  @brief   Read SBML unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestReadSBML.cpp
///  with the help of conversion sciprt (ctest_converter.pl).
/// 
/// <!---------------------------------------------------------------------------
///  This file is part of libSBML.  Please visit http://sbml.org for more
///  information about SBML, and the latest version of libSBML.
/// 
///  Copyright 2005-2010 California Institute of Technology.
///  Copyright 2002-2005 California Institute of Technology and
///                      Japan Science and Technology Corporation.
///  
///  This library is free software; you can redistribute it and/or modify it
///  under the terms of the GNU Lesser General Public License as published by
///  the Free Software Foundation.  A copy of the license agreement is provided
///  in the file named "LICENSE.txt" included with this software distribution
///  and also available online as http://sbml.org/software/libsbml/license.html
/// --------------------------------------------------------------------------->*/


namespace LibSBMLCSTest {

  using libsbml;

  using  System.IO;

  public class TestReadSBML {
    public class AssertionError : System.Exception 
    {
      public AssertionError() : base()
      {
        
      }
    }


    static void assertTrue(bool condition)
    {
      if (condition == true)
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertEquals(object a, object b)
    {
      if ( (a == null) && (b == null) )
      {
        return;
      }
      else if ( (a == null) || (b == null) )
      {
        throw new AssertionError();
      }
      else if (a.Equals(b))
      {
        return;
      }
  
      throw new AssertionError();
    }

    static void assertNotEquals(object a, object b)
    {
      if ( (a == null) && (b == null) )
      {
        throw new AssertionError();
      }
      else if ( (a == null) || (b == null) )
      {
        return;
      }
      else if (a.Equals(b))
      {
        throw new AssertionError();
      }
    }

    static void assertEquals(bool a, bool b)
    {
      if ( a == b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertNotEquals(bool a, bool b)
    {
      if ( a != b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertEquals(int a, int b)
    {
      if ( a == b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertNotEquals(int a, int b)
    {
      if ( a != b )
      {
        return;
      }
      throw new AssertionError();
    }

    private SBMLDocument D;
    private Model M;

    public string SBML_FOOTER()
    {
      return "</model> </sbml>";
    }

    public string SBML_HEADER_L1v1()
    {
      return "<sbml level='1' version='1'> <model name='m'>\n";
    }

    public string SBML_HEADER_L1v2()
    {
      return "<sbml level='1' version='2'> <model name='m'>\n";
    }

    public string SBML_HEADER_L2v1()
    {
      return "<sbml level='2' version='1'> <model name='m'>\n";
    }

    public string SBML_HEADER_L2v2()
    {
      return "<sbml level='2' version='2'> <model name='m'>\n";
    }

    public string SBML_HEADER_L2v3()
    {
      return "<sbml level='2' version='3'> <model name='m'>\n";
    }

    public string XML_HEADER()
    {
      return "<?xml version='1.0' encoding='UTF-8'?>\n";
    }

    public string wrapSBML_L1v1(string s)
    {
      string r = XML_HEADER();
      r += SBML_HEADER_L1v1();
      r += s;
      r += SBML_FOOTER();
      return r;
    }

    public string wrapSBML_L1v2(string s)
    {
      string r = XML_HEADER();
      r += SBML_HEADER_L1v2();
      r += s;
      r += SBML_FOOTER();
      return r;
    }

    public string wrapSBML_L2v1(string s)
    {
      string r = XML_HEADER();
      r += SBML_HEADER_L2v1();
      r += s;
      r += SBML_FOOTER();
      return r;
    }

    public string wrapSBML_L2v2(string s)
    {
      string r = XML_HEADER();
      r += SBML_HEADER_L2v2();
      r += s;
      r += SBML_FOOTER();
      return r;
    }

    public string wrapSBML_L2v3(string s)
    {
      string r = XML_HEADER();
      r += SBML_HEADER_L2v3();
      r += s;
      r += SBML_FOOTER();
      return r;
    }

    public string wrapXML(string s)
    {
      string r = XML_HEADER();
      r += s;
      return r;
    }


  private int USE_LIBXML = 0;
  private int USE_EXPAT  = 0;
  private int USE_XERCES = 0;  

    public void setUp()
    {
      D = null;
    }

    public void tearDown()
    {
      D = null;
    }

    public void test_ReadSBML_AlgebraicRule()
    {
      Rule ar;
      string s = wrapSBML_L1v2("<listOfRules>" + 
    "  <algebraicRule formula='x + 1'/>" + 
    "</listOfRules>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumRules() == 1 );
      ar = M.getRule(0);
      assertTrue((  "x + 1" == ar.getFormula() ));
    }

    public void test_ReadSBML_AlgebraicRule_L2()
    {
      Rule ar;
      ASTNode math;
      string formula;
      string s = wrapSBML_L2v1("<listOfRules>" + 
    "  <algebraicRule>" + 
    "    <math>" + 
    "      <apply>" + 
    "        <minus/>" + 
    "        <apply>" + 
    "          <plus/>" + 
    "            <ci> S1 </ci>" + 
    "            <ci> S2 </ci>" + 
    "        </apply>" + 
    "        <ci> T </ci>" + 
    "      </apply>" + 
    "    </math>" + 
    "  </algebraicRule>" + 
    "</listOfRules>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumRules() == 1 );
      ar = M.getRule(0);
      assertTrue( ar != null );
      assertEquals( true, ar.isSetMath() );
      math = ar.getMath();
      formula = ar.getFormula();
      assertTrue( formula != null );
      assertTrue((  "S1 + S2 - T" == formula ));
    }

    public void test_ReadSBML_AssignmentRule()
    {
      Rule ar;
      ASTNode math;
      string formula;
      string s = wrapSBML_L2v1("<listOfRules>" + 
    "  <assignmentRule variable='k'>" + 
    "    <math>" + 
    "      <apply>" + 
    "        <divide/>" + 
    "        <ci> k3 </ci>" + 
    "        <ci> k2 </ci>" + 
    "      </apply>" + 
    "    </math>" + 
    "  </assignmentRule>" + 
    "</listOfRules>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumRules() == 1 );
      ar = M.getRule(0);
      assertTrue( ar != null );
      assertEquals( true, ar.isSetMath() );
      math = ar.getMath();
      formula = ar.getFormula();
      assertTrue( formula != null );
      assertTrue((  "k3 / k2" == formula ));
    }

    public void test_ReadSBML_Compartment()
    {
      Compartment c;
      string s = wrapSBML_L1v2("<listOfCompartments>" + 
    "  <compartment name='mitochondria' volume='.0001' units='milliliters'" + 
    "               outside='cell'/>" + 
    "</listOfCompartments>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumCompartments() == 1 );
      c = M.getCompartment(0);
      assertTrue((  "mitochondria"  == c.getId() ));
      assertTrue((  "milliliters"   == c.getUnits() ));
      assertTrue((  "cell"          == c.getOutside() ));
      assertTrue( c.getVolume() == .0001 );
      assertEquals( true, c.isSetVolume() );
      assertEquals( true, c.isSetSize() );
    }

    public void test_ReadSBML_CompartmentVolumeRule()
    {
      Rule cvr;
      string s = wrapSBML_L1v2("<listOfRules>" + 
    "  <compartmentVolumeRule compartment='A' formula='0.10 * t'/>" + 
    "</listOfRules>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumRules() == 1 );
      cvr = M.getRule(0);
      assertEquals( true, cvr.isCompartmentVolume() );
      assertTrue((  "A" == cvr.getVariable() ));
      assertTrue((  "0.10 * t"  == cvr.getFormula() ));
      assertTrue( cvr.getType() == libsbml.RULE_TYPE_SCALAR );
    }

    public void test_ReadSBML_Compartment_L2()
    {
      Compartment c;
      string s = wrapSBML_L2v1("<listOfCompartments>" + 
    "  <compartment id='membrane' size='.3' spatialDimensions='2'" + 
    "               units='area' outside='tissue' constant='false'/>" + 
    "</listOfCompartments>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumCompartments() == 1 );
      c = M.getCompartment(0);
      assertEquals( true, c.isSetId() );
      assertEquals( false, c.isSetName() );
      assertEquals( true, c.isSetVolume() );
      assertEquals( true, c.isSetSize() );
      assertEquals( true, c.isSetUnits() );
      assertEquals( true, c.isSetOutside() );
      assertTrue((  "membrane"  == c.getId() ));
      assertTrue((  "area"      == c.getUnits() ));
      assertTrue((  "tissue"    == c.getOutside() ));
      assertTrue( c.getSpatialDimensions() == 2 );
      assertTrue( c.getSize() == .3 );
    }

    public void test_ReadSBML_Compartment_defaults()
    {
      Compartment c;
      string s = wrapSBML_L1v2("<listOfCompartments> <compartment name='cell'/> </listOfCompartments>"  
    );
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumCompartments() == 1 );
      c = M.getCompartment(0);
      assertEquals( true, c.isSetId() );
      assertEquals( true, c.isSetVolume() );
      assertEquals( false, c.isSetSize() );
      assertEquals( false, c.isSetUnits() );
      assertEquals( false, c.isSetOutside() );
      assertTrue((  "cell"  == c.getId() ));
      assertTrue( c.getVolume() == 1.0 );
    }

    public void test_ReadSBML_Compartment_defaults_L2()
    {
      Compartment c;
      string s = wrapSBML_L2v1("<listOfCompartments> <compartment id='cell'/> </listOfCompartments>"  
    );
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumCompartments() == 1 );
      c = M.getCompartment(0);
      assertEquals( true, c.isSetId() );
      assertEquals( false, c.isSetName() );
      assertEquals( false, c.isSetSize() );
      assertEquals( false, c.isSetUnits() );
      assertEquals( false, c.isSetOutside() );
      assertTrue((  "cell"  == c.getId() ));
      assertTrue( c.getSpatialDimensions() == 3 );
      assertTrue( c.getConstant() == true );
    }

    public void test_ReadSBML_Event()
    {
      Event e;
      string s = wrapSBML_L2v2("<listOfEvents>" + 
    "  <event id='e1' name='MyEvent' timeUnits='time'/>" + 
    "</listOfEvents>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumEvents() == 1 );
      e = M.getEvent(0);
      assertTrue( e != null );
      assertEquals( true, e.isSetId() );
      assertEquals( true, e.isSetName() );
      assertEquals( true, e.isSetTimeUnits() );
      assertEquals( false, e.isSetTrigger() );
      assertEquals( false, e.isSetDelay() );
      assertTrue((  "e1"       == e.getId() ));
      assertTrue((  "MyEvent"  == e.getName() ));
      assertTrue((  "time"     == e.getTimeUnits() ));
    }

    public void test_ReadSBML_EventAssignment()
    {
      Event e;
      EventAssignment ea;
      ASTNode math;
      string formula;
      string s = wrapSBML_L2v1("<listOfEvents>" + 
    "  <event>" + 
    "    <listOfEventAssignments>" + 
    "      <eventAssignment variable='k2'>" + 
    "        <math> <cn> 0 </cn> </math>" + 
    "      </eventAssignment>" + 
    "    </listOfEventAssignments>" + 
    "  </event>" + 
    "</listOfEvents>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumEvents() == 1 );
      e = M.getEvent(0);
      assertTrue( e != null );
      assertTrue( e.getNumEventAssignments() == 1 );
      ea = e.getEventAssignment(0);
      assertTrue( ea != null );
      assertEquals( true, ea.isSetVariable() );
      assertTrue((  "k2" == ea.getVariable() ));
      assertEquals( true, ea.isSetMath() );
      math = ea.getMath();
      formula = libsbml.formulaToString(math);
      assertTrue( formula != null );
      assertTrue((  "0" == formula ));
    }

    public void test_ReadSBML_Event_delay()
    {
      Event e;
      Delay delay;
      string formula;
      string s = wrapSBML_L2v1("<listOfEvents>" + 
    "  <event> <delay> <math> <cn> 5 </cn> </math> </delay> </event>" + 
    "</listOfEvents>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumEvents() == 1 );
      e = M.getEvent(0);
      assertTrue( e != null );
      assertEquals( true, e.isSetDelay() );
      assertEquals( false, e.isSetTrigger() );
      delay = e.getDelay();
      formula = libsbml.formulaToString(delay.getMath());
      assertTrue( formula != null );
      assertTrue((  "5" == formula ));
    }

    public void test_ReadSBML_Event_trigger()
    {
      Event e;
      Trigger trigger;
      string formula;
      string s = wrapSBML_L2v1("<listOfEvents>" + 
    "  <event>" + 
    "    <trigger>" + 
    "      <math>" + 
    "        <apply>" + 
    "          <leq/>" + 
    "          <ci> P1 </ci>" + 
    "          <ci> t  </ci>" + 
    "        </apply>" + 
    "      </math>" + 
    "   </trigger>" + 
    "  </event>" + 
    "</listOfEvents>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumEvents() == 1 );
      e = M.getEvent(0);
      assertTrue( e != null );
      assertEquals( false, e.isSetDelay() );
      assertEquals( true, e.isSetTrigger() );
      trigger = e.getTrigger();
      formula = libsbml.formulaToString(trigger.getMath());
      assertTrue( formula != null );
      assertTrue((  "leq(P1, t)" == formula ));
    }

    public void test_ReadSBML_FunctionDefinition()
    {
      FunctionDefinition fd;
      ASTNode math;
      string formula;
      string s = wrapSBML_L2v1("<listOfFunctionDefinitions>" + 
    "  <functionDefinition id='pow3' name='cubed'>" + 
    "    <math>" + 
    "      <lambda>" + 
    "        <bvar><ci> x </ci></bvar>" + 
    "        <apply>" + 
    "          <power/>" + 
    "          <ci> x </ci>" + 
    "          <cn> 3 </cn>" + 
    "        </apply>" + 
    "      </lambda>" + 
    "    </math>" + 
    "  </functionDefinition>" + 
    "</listOfFunctionDefinitions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumFunctionDefinitions() == 1 );
      fd = M.getFunctionDefinition(0);
      assertTrue( fd != null );
      assertEquals( true, fd.isSetId() );
      assertEquals( true, fd.isSetName() );
      assertTrue((  "pow3"   == fd.getId() ));
      assertTrue((  "cubed"  == fd.getName() ));
      assertEquals( true, fd.isSetMath() );
      math = fd.getMath();
      formula = libsbml.formulaToString(math);
      assertTrue( formula != null );
      assertTrue((  "lambda(x, pow(x, 3))" == formula ));
    }

    public void test_ReadSBML_KineticLaw()
    {
      Reaction r;
      KineticLaw kl;
      string s = wrapSBML_L1v2("<listOfReactions>" + 
    "  <reaction name='J1'>" + 
    "    <kineticLaw formula='k1*X0'/>" + 
    "  </reaction>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      kl = r.getKineticLaw();
      assertTrue((  "k1*X0" == kl.getFormula() ));
    }

    public void test_ReadSBML_KineticLaw_L2()
    {
      Reaction r;
      KineticLaw kl;
      ASTNode math;
      string formula;
      string s = wrapSBML_L2v1("<listOfReactions>" + 
    "  <reaction id='J1'>" + 
    "    <kineticLaw>" + 
    "      <math>" + 
    "        <apply>" + 
    "          <times/>" + 
    "          <ci> k  </ci>" + 
    "          <ci> S2 </ci>" + 
    "          <ci> X0 </ci>" + 
    "        </apply>" + 
    "      </math>" + 
    "    </kineticLaw>" + 
    "  </reaction>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      assertTrue( r != null );
      kl = r.getKineticLaw();
      assertTrue( kl != null );
      assertEquals( true, kl.isSetMath() );
      math = kl.getMath();
      formula = kl.getFormula();
      assertTrue( formula != null );
      assertTrue((  "k * S2 * X0" == formula ));
    }

    public void test_ReadSBML_KineticLaw_Parameter()
    {
      Reaction r;
      KineticLaw kl;
      Parameter p;
      string s = wrapSBML_L1v2("<listOfReactions>" + 
    "  <reaction name='J1'>" + 
    "    <kineticLaw formula='k1*X0'>" + 
    "      <listOfParameters>" + 
    "        <parameter name='k1' value='0'/>" + 
    "      </listOfParameters>" + 
    "    </kineticLaw>" + 
    "  </reaction>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      kl = r.getKineticLaw();
      assertTrue((  "k1*X0" == kl.getFormula() ));
      assertTrue( kl.getNumParameters() == 1 );
      p = kl.getParameter(0);
      assertTrue((  "k1" == p.getId() ));
      assertTrue( p.getValue() == 0 );
    }

    public void test_ReadSBML_Model()
    {
      string s = wrapXML("<sbml level='1' version='1'>" + 
    "  <model name='testModel'></model>" + 
    "</sbml>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue((  "testModel" == M.getId() ));
    }

    public void test_ReadSBML_Model_L2()
    {
      string s = wrapXML("<sbml level='2' version='1'>" + 
    "  <model id='testModel'> </model>" + 
    "</sbml>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertEquals( true, M.isSetId() );
      assertEquals( false, M.isSetName() );
      assertTrue((  "testModel" == M.getId() ));
    }

    public void test_ReadSBML_Parameter()
    {
      Parameter p;
      string s = wrapSBML_L1v2("<listOfParameters>" + 
    "  <parameter name='Km1' value='2.3' units='second'/>" + 
    "</listOfParameters>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumParameters() == 1 );
      p = M.getParameter(0);
      assertTrue((  "Km1"     == p.getId() ));
      assertTrue((  "second"  == p.getUnits() ));
      assertTrue( p.getValue() == 2.3 );
      assertTrue( p.isSetValue() == true );
    }

    public void test_ReadSBML_ParameterRule()
    {
      Rule pr;
      string s = wrapSBML_L1v2("<listOfRules>" + 
    "  <parameterRule name='k' formula='k3/k2'/>" + 
    "</listOfRules>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumRules() == 1 );
      pr = M.getRule(0);
      assertEquals( true, pr.isParameter() );
      assertTrue((  "k" == pr.getVariable() ));
      assertTrue((  "k3/k2"  == pr.getFormula() ));
      assertTrue( pr.getType() == libsbml.RULE_TYPE_SCALAR );
    }

    public void test_ReadSBML_Parameter_L2()
    {
      Parameter p;
      string s = wrapSBML_L2v1("<listOfParameters>" + 
    "  <parameter id='T' value='4.6' units='Celsius' constant='false'/>" + 
    "</listOfParameters>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumParameters() == 1 );
      p = M.getParameter(0);
      assertEquals( true, p.isSetId() );
      assertEquals( false, p.isSetName() );
      assertEquals( true, p.isSetValue() );
      assertEquals( true, p.isSetUnits() );
      assertTrue((  "T"        == p.getId() ));
      assertTrue((  "Celsius"  == p.getUnits() ));
      assertTrue( p.getValue() == 4.6 );
      assertTrue( p.getConstant() == false );
    }

    public void test_ReadSBML_Parameter_L2_defaults()
    {
      Parameter p;
      string s = wrapSBML_L2v1("<listOfParameters> <parameter id='x'/> </listOfParameters>"  
    );
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumParameters() == 1 );
      p = M.getParameter(0);
      assertEquals( true, p.isSetId() );
      assertEquals( false, p.isSetName() );
      assertEquals( false, p.isSetValue() );
      assertEquals( false, p.isSetUnits() );
      assertTrue((  "x" == p.getId() ));
      assertTrue( p.getConstant() == true );
    }

    public void test_ReadSBML_RateRule()
    {
      Rule rr;
      ASTNode math;
      string formula;
      string s = wrapSBML_L2v1("<listOfRules>" + 
    "  <rateRule variable='x'>" + 
    "    <math>" + 
    "      <apply>" + 
    "        <times/>" + 
    "        <apply>" + 
    "          <minus/>" + 
    "          <cn> 1 </cn>" + 
    "          <ci> x </ci>" + 
    "        </apply>" + 
    "        <apply>" + 
    "          <ln/>" + 
    "          <ci> x </ci>" + 
    "        </apply>" + 
    "      </apply>" + 
    "    </math>" + 
    "  </rateRule>" + 
    "</listOfRules>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumRules() == 1 );
      rr = M.getRule(0);
      assertTrue( rr != null );
      assertEquals( true, rr.isSetMath() );
      math = rr.getMath();
      formula = rr.getFormula();
      assertTrue( formula != null );
      assertTrue((  "(1 - x) * log(x)" == formula ));
    }

    public void test_ReadSBML_Reaction()
    {
      Reaction r;
      string s = wrapSBML_L1v2("<listOfReactions>" + 
    "  <reaction name='reaction_1' reversible='false'/>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      assertTrue((  "reaction_1" == r.getId() ));
      assertTrue( r.getReversible() == false );
      assertTrue( r.getFast() == false );
    }

    public void test_ReadSBML_Reaction_L2()
    {
      Reaction r;
      string s = wrapSBML_L2v1("<listOfReactions>" + 
    "  <reaction id='r1' reversible='false' fast='false'/>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      assertEquals( true, r.isSetId() );
      assertEquals( false, r.isSetName() );
      assertEquals( true, r.isSetFast() );
      assertTrue((  "r1" == r.getId() ));
      assertTrue( r.getReversible() == false );
      assertTrue( r.getFast() == false );
    }

    public void test_ReadSBML_Reaction_L2_defaults()
    {
      Reaction r;
      string s = wrapSBML_L2v1("<listOfReactions> <reaction id='r1'/> </listOfReactions>"  
    );
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      assertEquals( true, r.isSetId() );
      assertEquals( false, r.isSetName() );
      assertEquals( false, r.isSetFast() );
      assertTrue((  "r1" == r.getId() ));
      assertTrue( r.getReversible() == true );
    }

    public void test_ReadSBML_Reaction_defaults()
    {
      Reaction r;
      string s = wrapSBML_L1v2("<listOfReactions>" + 
    "  <reaction name='reaction_1'/>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      assertTrue((  "reaction_1" == r.getId() ));
      assertTrue( r.getReversible() != false );
      assertTrue( r.getFast() == false );
    }

    public void test_ReadSBML_SBML()
    {
      string s = wrapXML("<sbml level='1' version='1'> </sbml>");
      D = libsbml.readSBMLFromString(s);
      assertTrue( D.getLevel() == 1 );
      assertTrue( D.getVersion() == 1 );
    }

    public void test_ReadSBML_Specie()
    {
      Species sp;
      string s = wrapSBML_L1v1("<listOfSpecie>" + 
    "  <specie name='Glucose' compartment='cell' initialAmount='4.1'" + 
    "          units='volume' boundaryCondition='false' charge='6'/>" + 
    "</listOfSpecie>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumSpecies() == 1 );
      sp = M.getSpecies(0);
      assertTrue((  "Glucose"  == sp.getId() ));
      assertTrue((  "cell"     == sp.getCompartment() ));
      assertTrue((  "volume"   == sp.getUnits() ));
      assertTrue( sp.getInitialAmount() == 4.1 );
      assertTrue( sp.getBoundaryCondition() == false );
      assertTrue( sp.getCharge() == 6 );
      assertTrue( sp.isSetInitialAmount() == true );
      assertTrue( sp.isSetCharge() == true );
    }

    public void test_ReadSBML_SpecieConcentrationRule()
    {
      Rule scr;
      string s = wrapSBML_L1v1("<listOfRules>" + 
    "  <specieConcentrationRule specie='s2' formula='k * t/(1 + k)'/>" + 
    "</listOfRules>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumRules() == 1 );
      scr = M.getRule(0);
      assertEquals( true, scr.isSpeciesConcentration() );
      assertTrue((  "s2" == scr.getVariable() ));
      assertTrue((  "k * t/(1 + k)"  == scr.getFormula() ));
      assertTrue( scr.getType() == libsbml.RULE_TYPE_SCALAR );
    }

    public void test_ReadSBML_SpecieConcentrationRule_rate()
    {
      Rule scr;
      string s = wrapSBML_L1v1("<listOfRules>" + 
    "  <specieConcentrationRule specie='s2' formula='k * t/(1 + k)' " + 
    "                           type='rate'/>" + 
    "</listOfRules>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumRules() == 1 );
      scr = M.getRule(0);
      assertEquals( true, scr.isSpeciesConcentration() );
      assertTrue((  "s2" == scr.getVariable() ));
      assertTrue((  "k * t/(1 + k)"  == scr.getFormula() ));
      assertTrue( scr.getType() == libsbml.RULE_TYPE_RATE );
    }

    public void test_ReadSBML_SpecieReference_Product()
    {
      Reaction r;
      SpeciesReference sr;
      string s = wrapSBML_L1v1("<listOfReactions>" + 
    "  <reaction name='reaction_1' reversible='false'>" + 
    "    <listOfProducts>" + 
    "      <specieReference specie='S1' stoichiometry='1'/>" + 
    "    </listOfProducts>" + 
    "  </reaction>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      assertTrue((  "reaction_1" == r.getId() ));
      assertTrue( r.getReversible() == false );
      assertTrue( r.getNumProducts() == 1 );
      sr = r.getProduct(0);
      assertTrue((  "S1" == sr.getSpecies() ));
      assertTrue( sr.getStoichiometry() == 1 );
      assertTrue( sr.getDenominator() == 1 );
    }

    public void test_ReadSBML_SpecieReference_Reactant()
    {
      Reaction r;
      SpeciesReference sr;
      string s = wrapSBML_L1v1("<listOfReactions>" + 
    "  <reaction name='reaction_1' reversible='false'>" + 
    "    <listOfReactants>" + 
    "      <specieReference specie='X0' stoichiometry='1'/>" + 
    "    </listOfReactants>" + 
    "  </reaction>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      assertTrue((  "reaction_1" == r.getId() ));
      assertTrue( r.getReversible() == false );
      assertTrue( r.getNumReactants() == 1 );
      sr = r.getReactant(0);
      assertTrue((  "X0" == sr.getSpecies() ));
      assertTrue( sr.getStoichiometry() == 1 );
      assertTrue( sr.getDenominator() == 1 );
    }

    public void test_ReadSBML_SpecieReference_defaults()
    {
      Reaction r;
      SpeciesReference sr;
      string s = wrapSBML_L1v1("<listOfReactions>" + 
    "  <reaction name='reaction_1' reversible='false'>" + 
    "    <listOfReactants>" + 
    "      <specieReference specie='X0'/>" + 
    "    </listOfReactants>" + 
    "  </reaction>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      assertTrue((  "reaction_1" == r.getId() ));
      assertTrue( r.getReversible() == false );
      assertTrue( r.getNumReactants() == 1 );
      sr = r.getReactant(0);
      assertTrue((  "X0" == sr.getSpecies() ));
      assertTrue( sr.getStoichiometry() == 1 );
      assertTrue( sr.getDenominator() == 1 );
    }

    public void test_ReadSBML_Specie_defaults()
    {
      Species sp;
      string s = wrapSBML_L1v1("<listOfSpecie>" + 
    "  <specie name='Glucose' compartment='cell' initialAmount='1.0'/>" + 
    "</listOfSpecie>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumSpecies() == 1 );
      sp = M.getSpecies(0);
      assertTrue((  "Glucose"  == sp.getId() ));
      assertTrue((  "cell"     == sp.getCompartment() ));
      assertTrue( sp.getInitialAmount() == 1.0 );
      assertTrue( sp.getBoundaryCondition() == false );
      assertTrue( sp.isSetInitialAmount() == true );
      assertTrue( sp.isSetCharge() == false );
    }

    public void test_ReadSBML_Species()
    {
      Species sp;
      string s = wrapSBML_L1v2("<listOfSpecies>" + 
    "  <species name='Glucose' compartment='cell' initialAmount='4.1'" + 
    "           units='volume' boundaryCondition='false' charge='6'/>" + 
    "</listOfSpecies>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumSpecies() == 1 );
      sp = M.getSpecies(0);
      assertTrue((  "Glucose"  == sp.getId() ));
      assertTrue((  "cell"     == sp.getCompartment() ));
      assertTrue((  "volume"   == sp.getUnits() ));
      assertTrue( sp.getInitialAmount() == 4.1 );
      assertTrue( sp.getBoundaryCondition() == false );
      assertTrue( sp.getCharge() == 6 );
      assertTrue( sp.isSetInitialAmount() == true );
      assertTrue( sp.isSetCharge() == true );
    }

    public void test_ReadSBML_SpeciesConcentrationRule()
    {
      Rule scr;
      string s = wrapSBML_L1v2("<listOfRules>" + 
    "  <speciesConcentrationRule species='s2' formula='k * t/(1 + k)'/>" + 
    "</listOfRules>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumRules() == 1 );
      scr = M.getRule(0);
      assertEquals( true, scr.isSpeciesConcentration() );
      assertTrue((  "s2" == scr.getVariable() ));
      assertTrue((  "k * t/(1 + k)"  == scr.getFormula() ));
      assertTrue( scr.getType() == libsbml.RULE_TYPE_SCALAR );
    }

    public void test_ReadSBML_SpeciesReference_StoichiometryMath_1()
    {
      Reaction r;
      SpeciesReference sr;
      StoichiometryMath math;
      string formula;
      string s = wrapSBML_L2v1("<listOfReactions>" + 
    "  <reaction name='r1'>" + 
    "    <listOfReactants>" + 
    "      <speciesReference species='X0'>" + 
    "        <stoichiometryMath>" + 
    "          <math> <ci> x </ci> </math>" + 
    "        </stoichiometryMath>" + 
    "      </speciesReference>" + 
    "    </listOfReactants>" + 
    "  </reaction>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      assertTrue( r != null );
      assertTrue( r.getNumReactants() == 1 );
      sr = r.getReactant(0);
      assertTrue( sr != null );
      assertEquals( true, sr.isSetStoichiometryMath() );
      math = sr.getStoichiometryMath();
      formula = libsbml.formulaToString(math.getMath());
      assertTrue( formula != null );
      assertTrue((  "x" == formula ));
    }

    public void test_ReadSBML_SpeciesReference_StoichiometryMath_2()
    {
      Reaction r;
      SpeciesReference sr;
      string s = wrapSBML_L2v1("<listOfReactions>" + 
    "  <reaction name='r1'>" + 
    "    <listOfReactants>" + 
    "      <speciesReference species='X0'>" + 
    "        <stoichiometryMath>" + 
    "          <math> <cn type='rational'> 3 <sep/> 2 </cn> </math>" + 
    "        </stoichiometryMath>" + 
    "      </speciesReference>" + 
    "    </listOfReactants>" + 
    "  </reaction>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      assertTrue( r != null );
      assertTrue( r.getNumReactants() == 1 );
      sr = r.getReactant(0);
      assertTrue( sr != null );
      assertEquals( false, sr.isSetStoichiometryMath() );
      assertTrue( sr.getStoichiometry() == 3 );
      assertTrue( sr.getDenominator() == 2 );
    }

    public void test_ReadSBML_SpeciesReference_defaults()
    {
      Reaction r;
      SpeciesReference sr;
      string s = wrapSBML_L1v2("<listOfReactions>" + 
    "  <reaction name='reaction_1' reversible='false'>" + 
    "    <listOfReactants>" + 
    "      <speciesReference species='X0'/>" + 
    "    </listOfReactants>" + 
    "  </reaction>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumReactions() == 1 );
      r = M.getReaction(0);
      assertTrue((  "reaction_1" == r.getId() ));
      assertTrue( r.getReversible() == false );
      assertTrue( r.getNumReactants() == 1 );
      sr = r.getReactant(0);
      assertTrue((  "X0" == sr.getSpecies() ));
      assertTrue( sr.getStoichiometry() == 1 );
      assertTrue( sr.getDenominator() == 1 );
    }

    public void test_ReadSBML_Species_L2_1()
    {
      Species sp;
      string s = wrapSBML_L2v1("<listOfSpecies>" + 
    "  <species id='Glucose' compartment='cell' initialConcentration='4.1'" + 
    "           substanceUnits='item' spatialSizeUnits='volume'" + 
    "           boundaryCondition='true' charge='6' constant='true'/>" + 
    "</listOfSpecies>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumSpecies() == 1 );
      sp = M.getSpecies(0);
      assertEquals( true, sp.isSetId() );
      assertEquals( false, sp.isSetName() );
      assertEquals( true, sp.isSetCompartment() );
      assertEquals( false, sp.isSetInitialAmount() );
      assertEquals( true, sp.isSetInitialConcentration() );
      assertEquals( true, sp.isSetSubstanceUnits() );
      assertEquals( true, sp.isSetSpatialSizeUnits() );
      assertEquals( true, sp.isSetCharge() );
      assertTrue((  "Glucose"  == sp.getId() ));
      assertTrue((  "cell"     == sp.getCompartment() ));
      assertTrue((  "item"     == sp.getSubstanceUnits() ));
      assertTrue((  "volume"   == sp.getSpatialSizeUnits() ));
      assertTrue( sp.getInitialConcentration() == 4.1 );
      assertTrue( sp.getHasOnlySubstanceUnits() == false );
      assertTrue( sp.getBoundaryCondition() == true );
      assertTrue( sp.getCharge() == 6 );
      assertTrue( sp.getConstant() == true );
    }

    public void test_ReadSBML_Species_L2_2()
    {
      Species sp;
      string s = wrapSBML_L2v1("<listOfSpecies>" + 
    "  <species id='s' compartment='c' hasOnlySubstanceUnits='true'/>" + 
    "</listOfSpecies>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumSpecies() == 1 );
      sp = M.getSpecies(0);
      assertEquals( true, sp.isSetId() );
      assertEquals( false, sp.isSetName() );
      assertEquals( true, sp.isSetCompartment() );
      assertEquals( false, sp.isSetInitialAmount() );
      assertEquals( false, sp.isSetInitialConcentration() );
      assertEquals( false, sp.isSetSubstanceUnits() );
      assertEquals( false, sp.isSetSpatialSizeUnits() );
      assertEquals( false, sp.isSetCharge() );
      assertTrue((  "s"  == sp.getId() ));
      assertTrue((  "c"  == sp.getCompartment() ));
      assertTrue( sp.getHasOnlySubstanceUnits() == true );
      assertTrue( sp.getBoundaryCondition() == false );
      assertTrue( sp.getConstant() == false );
    }

    public void test_ReadSBML_Species_L2_defaults()
    {
      Species sp;
      string s = wrapSBML_L2v1("<listOfSpecies>" + 
    "  <species id='Glucose_6_P' compartment='cell'/>" + 
    "</listOfSpecies>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumSpecies() == 1 );
      sp = M.getSpecies(0);
      assertEquals( true, sp.isSetId() );
      assertEquals( false, sp.isSetName() );
      assertEquals( true, sp.isSetCompartment() );
      assertEquals( false, sp.isSetInitialAmount() );
      assertEquals( false, sp.isSetInitialConcentration() );
      assertEquals( false, sp.isSetSubstanceUnits() );
      assertEquals( false, sp.isSetSpatialSizeUnits() );
      assertEquals( false, sp.isSetCharge() );
      assertTrue((  "Glucose_6_P"  == sp.getId() ));
      assertTrue((  "cell"         == sp.getCompartment() ));
      assertTrue( sp.getHasOnlySubstanceUnits() == false );
      assertTrue( sp.getBoundaryCondition() == false );
      assertTrue( sp.getConstant() == false );
    }

    public void test_ReadSBML_Unit()
    {
      Unit u;
      UnitDefinition ud;
      string s = wrapSBML_L1v2("<listOfUnitDefinitions>" + 
    "  <unitDefinition name='substance'>" + 
    "    <listOfUnits> <unit kind='mole' scale='-3'/> </listOfUnits>" + 
    "  </unitDefinition>" + 
    "</listOfUnitDefinitions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumUnitDefinitions() == 1 );
      ud = M.getUnitDefinition(0);
      assertTrue((  "substance" == ud.getId() ));
      assertTrue( ud.getNumUnits() == 1 );
      u = ud.getUnit(0);
      assertTrue( u.getKind() == libsbml.UNIT_KIND_MOLE );
      assertTrue( u.getExponent() == 1 );
      assertTrue( u.getScale() == -3 );
    }

    public void test_ReadSBML_UnitDefinition()
    {
      UnitDefinition ud;
      string s = wrapSBML_L1v2("<listOfUnitDefinitions>" + 
    "  <unitDefinition name='mmls'/>" + 
    "</listOfUnitDefinitions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumUnitDefinitions() == 1 );
      ud = M.getUnitDefinition(0);
      assertTrue((  "mmls" == ud.getId() ));
    }

    public void test_ReadSBML_UnitDefinition_L2()
    {
      UnitDefinition ud;
      string s = wrapSBML_L2v1("<listOfUnitDefinitions>" + 
    "  <unitDefinition id='mmls' name='mmol/ls'/>" + 
    "</listOfUnitDefinitions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumUnitDefinitions() == 1 );
      ud = M.getUnitDefinition(0);
      assertEquals( true, ud.isSetId() );
      assertEquals( true, ud.isSetName() );
      assertTrue((  "mmls" == ud.getId() ));
      assertTrue((  "mmol/ls" == ud.getName() ));
    }

    public void test_ReadSBML_Unit_L2()
    {
      Unit u;
      UnitDefinition ud;
      string s = wrapSBML_L2v1("<listOfUnitDefinitions>" + 
    "  <unitDefinition id='Fahrenheit'>" + 
    "    <listOfUnits>" + 
    "      <unit kind='Celsius' multiplier='1.8' offset='32'/>" + 
    "    </listOfUnits>" + 
    "  </unitDefinition>" + 
    "</listOfUnitDefinitions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumUnitDefinitions() == 1 );
      ud = M.getUnitDefinition(0);
      assertEquals( true, ud.isSetId() );
      assertTrue((  "Fahrenheit" == ud.getId() ));
      assertTrue( ud.getNumUnits() == 1 );
      u = ud.getUnit(0);
      assertTrue( u.getKind() == libsbml.UNIT_KIND_CELSIUS );
      assertTrue( u.getExponent() == 1 );
      assertTrue( u.getScale() == 0 );
      assertTrue( u.getMultiplier() == 1.8 );
      assertTrue( u.getOffset() == 32 );
    }

    public void test_ReadSBML_Unit_defaults_L1_L2()
    {
      Unit u;
      UnitDefinition ud;
      string s = wrapSBML_L1v2("<listOfUnitDefinitions>" + 
    "  <unitDefinition name='bogomips'>" + 
    "    <listOfUnits> <unit kind='second'/> </listOfUnits>" + 
    "  </unitDefinition>" + 
    "</listOfUnitDefinitions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNumUnitDefinitions() == 1 );
      ud = M.getUnitDefinition(0);
      assertTrue((  "bogomips" == ud.getId() ));
      assertTrue( ud.getNumUnits() == 1 );
      u = ud.getUnit(0);
      assertTrue( u.getKind() == libsbml.UNIT_KIND_SECOND );
      assertTrue( u.getExponent() == 1 );
      assertTrue( u.getScale() == 0 );
      assertTrue( u.getMultiplier() == 1.0 );
      assertTrue( u.getOffset() == 0.0 );
    }

    public void test_ReadSBML_annotation()
    {
      string s = wrapSBML_L2v3("<annotation xmlns:mysim=\"http://www.mysim.org/ns\">" + 
    "  <mysim:nodecolors mysim:bgcolor=\"green\" mysim:fgcolor=\"white\">" + 
    "  </mysim:nodecolors>" + 
    "  <mysim:timestamp>2000-12-18 18:31 PST</mysim:timestamp>" + 
    "</annotation>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getAnnotation() != null );
      XMLNode ann = M.getAnnotation();
      assertTrue( ann.getNumChildren() == 2 );
    }

    public void test_ReadSBML_annotation_sbml()
    {
      string s = wrapXML("<sbml level=\"1\" version=\"1\">" + 
    "  <annotation xmlns:jd = \"http://www.sys-bio.org/sbml\">" + 
    "    <jd:header>" + 
    "      <VersionHeader SBMLVersion = \"1.0\"/>" + 
    "    </jd:header>" + 
    "    <jd:display>" + 
    "      <SBMLGraphicsHeader BackGroundColor = \"15728639\"/>" + 
    "    </jd:display>" + 
    "  </annotation>" + 
    "</sbml>");
      D = libsbml.readSBMLFromString(s);
      assertTrue( D.getNumErrors() > 0 );
    }

    public void test_ReadSBML_annotation_sbml_L2()
    {
      string s = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level2\" level=\"2\" version=\"1\"> " + 
    "  <annotation>" + 
    "    <rdf xmlns=\"http://www.w3.org/1999/anything\">" + 
    "		 </rdf>" + 
    "	  </annotation>" + 
    "	  <model>" + 
    "   </model>" + 
    " </sbml>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( D.getNumErrors() == 0 );
    }

    public void test_ReadSBML_invalid_default_namespace()
    {
      string valid = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level2/version4\" level=\"2\" version=\"4\"> " + 
    "   <model>" + 
    "     <notes>" + 
    "       <p xmlns=\"http://www.w3.org/1999/xhtml\">Some text.</p>" + 
    "     </notes>" + 
    "     <annotation>" + 
    "       <example xmlns=\"http://www.example.org/\"/>" + 
    "     </annotation>" + 
    "     <listOfCompartments>" + 
    "       <compartment id=\"compartmentOne\" size=\"1\"/>" + 
    "     </listOfCompartments>" + 
    "     <listOfSpecies>" + 
    "       <species id=\"S1\" initialConcentration=\"1\" compartment=\"compartmentOne\"/>" + 
    "       <species id=\"S2\" initialConcentration=\"0\" compartment=\"compartmentOne\"/>" + 
    "     </listOfSpecies>" + 
    "     <listOfParameters>" + 
    "       <parameter id=\"t\" value = \"1\" units=\"second\"/>" + 
    "     </listOfParameters>" + 
    "     <listOfConstraints>" + 
    "       <constraint sboTerm=\"SBO:0000064\">" + 
    "         <math xmlns=\"http://www.w3.org/1998/Math/MathML\">" + 
    "           <apply>" + 
    "             <leq/>" + 
    "             <ci> S1 </ci>" + 
    "             <ci> t </ci>" + 
    "           </apply>" + 
    "         </math>" + 
    "         <message>" + 
    "           <p xmlns=\"http://www.w3.org/1999/xhtml\"> Species S1 is out of range </p>" + 
    "         </message>" + 
    "       </constraint>" + 
    "     </listOfConstraints>" + 
    "     <listOfReactions>" + 
    "       <reaction id=\"reaction_1\" reversible=\"false\">" + 
    "           <listOfReactants>" + 
    "             <speciesReference species=\"S1\"/>" + 
    "           </listOfReactants>" + 
    "           <listOfProducts>" + 
    "             <speciesReference species=\"S2\">" + 
    "             </speciesReference>" + 
    "           </listOfProducts>" + 
    "       </reaction>" + 
    "     </listOfReactions>" + 
    "   </model>" + 
    " </sbml>");
      string invalid = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level2/version4\" level=\"2\" version=\"4\"> " + 
    "   <model xmlns=\"http://invalid/custom/default/uri\">" + 
    "     <notes xmlns=\"http://invalid/custom/default/uri/in/notes\">" + 
    "       <p xmlns=\"http://www.w3.org/1999/xhtml\">Some text.</p>" + 
    "     </notes>" + 
    "     <annotation xmlns=\"http://invalid/custom/default/uri/in/annotation\">" + 
    "       <example xmlns=\"http://www.example.org/\"/>" + 
    "     </annotation>" + 
    "     <listOfCompartments>" + 
    "       <compartment id=\"compartmentOne\" size=\"1\"/>" + 
    "     </listOfCompartments>" + 
    "     <listOfSpecies>" + 
    "       <notes xmlns=\"http://invalid/custom/default/uri/in/notes\">" + 
    "         <p xmlns=\"http://www.w3.org/1999/xhtml\">Some text.</p>" + 
    "       </notes>" + 
    "       <annotation xmlns=\"http://invalid/custom/default/uri/in/annotation\">" + 
    "         <example xmlns=\"http://www.example.org/\"/>" + 
    "       </annotation>" + 
    "       <species id=\"S1\" initialConcentration=\"1\" compartment=\"compartmentOne\"/>" + 
    "       <species id=\"S2\" initialConcentration=\"0\" compartment=\"compartmentOne\"/>" + 
    "     </listOfSpecies>" + 
    "     <listOfParameters>" + 
    "       <parameter id=\"t\" value = \"1\" units=\"second\"/>" + 
    "     </listOfParameters>" + 
    "     <listOfConstraints>" + 
    "       <constraint sboTerm=\"SBO:0000064\">" + 
    "         <math xmlns=\"http://www.w3.org/1998/Math/MathML\">" + 
    "           <apply>" + 
    "             <leq/>" + 
    "             <ci> S1 </ci>" + 
    "             <ci> t </ci>" + 
    "           </apply>" + 
    "         </math>" + 
    "         <message xmlns=\"http://invalid/custom/default/uri/in/message\">" + 
    "           <p xmlns=\"http://www.w3.org/1999/xhtml\"> Species S1 is out of range </p>" + 
    "         </message>" + 
    "       </constraint>" + 
    "     </listOfConstraints>" + 
    "     <listOfReactions>" + 
    "       <reaction id=\"reaction_1\" reversible=\"false\">" + 
    "           <listOfReactants>" + 
    "             <speciesReference xmlns=\"http://invalid/custom/default/uri\" species=\"S1\"/>" + 
    "           </listOfReactants>" + 
    "           <listOfProducts>" + 
    "             <speciesReference species=\"S2\">" + 
    "               <notes xmlns=\"http://invalid/custom/default/uri/in/notes\">" + 
    "                 <p xmlns=\"http://www.w3.org/1999/xhtml\">Some text.</p>" + 
    "               </notes>" + 
    "               <annotation xmlns=\"http://invalid/custom/default/uri/in/annotation\">" + 
    "                 <example xmlns=\"http://www.example.org/\"/>" + 
    "               </annotation>" + 
    "             </speciesReference>" + 
    "           </listOfProducts>" + 
    "       </reaction>" + 
    "     </listOfReactions>" + 
    "   </model>" + 
    " </sbml>");
      D = libsbml.readSBMLFromString(valid);
      assertTrue( D.getNumErrors() == 0 );
      D = null;
      D = libsbml.readSBMLFromString(invalid);
      assertTrue( D.getNumErrors() == 9 );
    }

    public void test_ReadSBML_line_col_numbers()
    {
      //setXMLParser();

      SBase sb;
      string s = "<?xml version='1.0' encoding='UTF-8'?>\n" + 
    "<sbml xmlns='http://www.sbml.org/sbml/level2' level='2' version='1'>\n" + 
    "  <model id='testModel' name='testModel'>\n" + 
    "    <listOfReactions> <reaction/> </listOfReactions>\n" + 
    "  </model>\n" + 
    "</sbml>\n";
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M != null );
      sb = M;
      sb = M.getListOfReactions();
      sb = M.getReaction(0);
    }

    public void test_ReadSBML_metaid()
    {
      SBase sb;
      string s = wrapSBML_L2v1("<listOfFunctionDefinitions>" + 
    "  <functionDefinition metaid='fd'/>" + 
    "</listOfFunctionDefinitions>" + 
    "<listOfUnitDefinitions>" + 
    "  <unitDefinition metaid='ud'/>" + 
    "</listOfUnitDefinitions>" + 
    "<listOfCompartments>" + 
    "  <compartment metaid='c'/>" + 
    "</listOfCompartments>" + 
    "<listOfSpecies>" + 
    "  <species metaid='s'/>" + 
    "</listOfSpecies>" + 
    "<listOfParameters>" + 
    "  <parameter metaid='p'/>" + 
    "</listOfParameters>" + 
    "<listOfRules>" + 
    "  <rateRule metaid='rr'/>" + 
    "</listOfRules>" + 
    "<listOfReactions>" + 
    "  <reaction metaid='rx'/>" + 
    "</listOfReactions>" + 
    "<listOfEvents>" + 
    " <event metaid='e'/>" + 
    "</listOfEvents>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M != null );
      sb = M.getFunctionDefinition(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "fd" == sb.getMetaId() ));
      sb = M.getUnitDefinition(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "ud" == sb.getMetaId() ));
      sb = M.getCompartment(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "c" == sb.getMetaId() ));
      sb = M.getSpecies(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "s" == sb.getMetaId() ));
      sb = M.getParameter(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "p" == sb.getMetaId() ));
      sb = M.getRule(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "rr" == sb.getMetaId() ));
      sb = M.getReaction(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "rx" == sb.getMetaId() ));
      sb = M.getEvent(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "e" == sb.getMetaId() ));
    }

    public void test_ReadSBML_metaid_Event()
    {
      SBase sb;
      Event e;
      string s = wrapSBML_L2v1("<listOfEvents>" + 
    "  <event metaid='e'>" + 
    "    <listOfEventAssignments metaid='loea'>" + 
    "      <eventAssignment metaid='ea'/>" + 
    "    </listOfEventAssignments>" + 
    "  </event>" + 
    "</listOfEvents>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M != null );
      e = M.getEvent(0);
      sb = e;
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "e" == sb.getMetaId() ));
      sb = e.getListOfEventAssignments();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "loea" == sb.getMetaId() ));
      sb = e.getEventAssignment(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "ea" == sb.getMetaId() ));
    }

    public void test_ReadSBML_metaid_ListOf()
    {
      SBase sb;
      string s = wrapSBML_L2v1("<listOfFunctionDefinitions metaid='lofd'/>" + 
    "<listOfUnitDefinitions     metaid='loud'/>" + 
    "<listOfCompartments        metaid='loc'/>" + 
    "<listOfSpecies             metaid='los'/>" + 
    "<listOfParameters          metaid='lop'/>" + 
    "<listOfRules               metaid='lor'/>" + 
    "<listOfReactions           metaid='lorx'/>" + 
    "<listOfEvents              metaid='loe'/>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M != null );
      sb = M.getListOfFunctionDefinitions();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "lofd" == sb.getMetaId() ));
      sb = M.getListOfUnitDefinitions();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "loud" == sb.getMetaId() ));
      sb = M.getListOfCompartments();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "loc" == sb.getMetaId() ));
      sb = M.getListOfSpecies();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "los" == sb.getMetaId() ));
      sb = M.getListOfParameters();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "lop" == sb.getMetaId() ));
      sb = M.getListOfRules();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "lor" == sb.getMetaId() ));
      sb = M.getListOfReactions();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "lorx" == sb.getMetaId() ));
      sb = M.getListOfEvents();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "loe" == sb.getMetaId() ));
    }

    public void test_ReadSBML_metaid_Reaction()
    {
      SBase sb;
      Reaction r;
      string s = wrapSBML_L2v1("<listOfReactions>" + 
    "  <reaction metaid='r'>" + 
    "    <listOfReactants metaid='lor'>" + 
    "      <speciesReference metaid='sr1'/>" + 
    "    </listOfReactants>" + 
    "    <listOfProducts metaid='lop'>" + 
    "      <speciesReference metaid='sr2'/>" + 
    "    </listOfProducts>" + 
    "    <listOfModifiers metaid='lom'>" + 
    "      <modifierSpeciesReference metaid='msr'/>" + 
    "    </listOfModifiers>" + 
    "    <kineticLaw metaid='kl'/>" + 
    "  </reaction>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M != null );
      r = M.getReaction(0);
      sb = r;
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "r" == sb.getMetaId() ));
      sb = r.getListOfReactants();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "lor" == sb.getMetaId() ));
      sb = r.getReactant(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "sr1" == sb.getMetaId() ));
      sb = r.getListOfProducts();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "lop" == sb.getMetaId() ));
      sb = r.getProduct(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "sr2" == sb.getMetaId() ));
      sb = r.getListOfModifiers();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "lom" == sb.getMetaId() ));
      sb = r.getModifier(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "msr" == sb.getMetaId() ));
      sb = r.getKineticLaw();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "kl" == sb.getMetaId() ));
    }

    public void test_ReadSBML_metaid_Unit()
    {
      SBase sb;
      UnitDefinition ud;
      string s = wrapSBML_L2v1("<listOfUnitDefinitions>" + 
    "  <unitDefinition metaid='ud'>" + 
    "    <listOfUnits metaid='lou'>" + 
    "      <unit metaid='u'/>" + 
    "    </listOfUnits>" + 
    "  </unitDefinition>" + 
    "</listOfUnitDefinitions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M != null );
      ud = M.getUnitDefinition(0);
      sb = ud;
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "ud" == sb.getMetaId() ));
      sb = ud.getListOfUnits();
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "lou" == sb.getMetaId() ));
      sb = ud.getUnit(0);
      assertEquals( true, sb.isSetMetaId() );
      assertTrue((  "u" == sb.getMetaId() ));
    }

    public void test_ReadSBML_notes()
    {
      Reaction r;
      KineticLaw kl;
      string s = wrapSBML_L2v3("<listOfReactions>" + 
    "<reaction name='J1'>" + 
    "  <kineticLaw formula='k1*X0'>" + 
    "    <notes>This is a test note.</notes>" + 
    "    <listOfParameters>" + 
    "      <parameter name='k1' value='0'/>" + 
    "    </listOfParameters>" + 
    "  </kineticLaw>" + 
    "</reaction>" + 
    "</listOfReactions>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      r = M.getReaction(0);
      kl = r.getKineticLaw();
      assertTrue( kl.getNotes() != null );
      string notes = kl.getNotes().getChild(0).getCharacters();
      assertTrue( (  "This is a test note." != notes ) == false );
    }

    public void test_ReadSBML_notes_ListOf()
    {
      SBase sb;
      string s = wrapSBML_L2v1("<listOfFunctionDefinitions>" + 
    "  <notes>My Functions</notes>" + 
    "  <functionDefinition/>" + 
    "</listOfFunctionDefinitions>" + 
    "<listOfUnitDefinitions>" + 
    "  <notes>My Units</notes>" + 
    "  <unitDefinition/>" + 
    "</listOfUnitDefinitions>" + 
    "<listOfCompartments>" + 
    "  <notes>My Compartments</notes>" + 
    "  <compartment/>" + 
    "</listOfCompartments>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M != null );
      sb = M.getListOfFunctionDefinitions();
      assertEquals( true, sb.isSetNotes() );
      string notes = sb.getNotes().getChild(0).getCharacters();
      assertTrue( (  "My Functions" != notes ) == false );
      sb = M.getListOfUnitDefinitions();
      assertEquals( true, sb.isSetNotes() );
      notes = sb.getNotes().getChild(0).getCharacters();
      assertTrue( (  "My Units" != notes ) == false );
      sb = M.getListOfCompartments();
      assertEquals( true, sb.isSetNotes() );
      notes = sb.getNotes().getChild(0).getCharacters();
      assertTrue( (  "My Compartments" != notes ) == false );
    }

    public void test_ReadSBML_notes_sbml()
    {
      string s = wrapXML("<sbml level='1' version='1'>" + 
    "  <notes>Notes are not allowed as part of the SBML element.</notes>" + 
    "</sbml>");
      D = libsbml.readSBMLFromString(s);
      assertTrue( D.getNotes() != null );
      string notes = D.getNotes().getChild(0).getCharacters();
      assertTrue( (  "Notes are not allowed as part of the SBML element." != notes ) == false );
      assertTrue( D.getNumErrors() > 0 );
    }

    public void test_ReadSBML_notes_sbml_L2()
    {
      string s = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level2\" level=\"2\" version=\"1\"> " + 
    "  <notes>" + 
    "    <html xmlns=\"http://www.w3.org/1999/xhtml\">" + 
    "		 </html>" + 
    "	  </notes>" + 
    "	  <model>" + 
    "   </model>" + 
    " </sbml>");
      D = libsbml.readSBMLFromString(s);
      assertTrue( D.getNotes() != null );
      assertTrue( D.getNumErrors() == 0 );
    }

    public void test_ReadSBML_notes_xmlns()
    {
      string s = wrapSBML_L2v3("<notes>" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">Some text.</body>" + 
    "</notes>");
      D = libsbml.readSBMLFromString(s);
      M = D.getModel();
      assertTrue( M.getNotes() != null );
      XMLNamespaces ns = M.getNotes().getChild(0).getNamespaces();
      assertTrue( ns.getLength() == 1 );
      assertTrue((  "http://www.w3.org/1999/xhtml" == ns.getURI(0) ));
      string notes = M.getNotes().getChild(0).getChild(0).getCharacters();
      assertTrue( (  "Some text." != notes ) == false );
    }

  }
}
