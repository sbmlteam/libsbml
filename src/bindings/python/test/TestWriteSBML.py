#
# @file    TestWriteSBML.py
# @brief   Write SBML unit tests
# @author  Akiya Jouraku (Python conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestWriteSBML.cpp
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2008 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#--------------------------------------------------------------------------->*/
#
import sys
import unittest
import libsbml

def util_NaN():
  z = 1e300
  z = z * z

  return z - z

def util_PosInf():
  z = 1e300
  z = z * z

  return z

def util_NegInf():
  z = 1e300
  z = z * z

  return -z 

def LV_L1v1():
  return "level=\"1\" version=\"1\">\n"
  pass

def LV_L1v2():
  return "level=\"1\" version=\"2\">\n"
  pass

def LV_L2v1():
  return "level=\"2\" version=\"1\">\n"
  pass

def LV_L2v2():
  return "level=\"2\" version=\"2\">\n"
  pass

def NS_L1():
  return "xmlns=\"http://www.sbml.org/sbml/level1\" "
  pass

def NS_L2v1():
  return "xmlns=\"http://www.sbml.org/sbml/level2\" "
  pass

def NS_L2v2():
  return "xmlns=\"http://www.sbml.org/sbml/level2/version2\" "
  pass

def SBML_END():
  return "</sbml>\n"
  pass

def SBML_START():
  return "<sbml "
  pass

def XML_START():
  return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  pass

def wrapSBML_L1v1(s):
  r = XML_START()
  r += SBML_START()
  r += NS_L1()
  r += LV_L1v1()
  r += s
  r += SBML_END()
  return r
  pass

def wrapSBML_L1v2(s):
  r = XML_START()
  r += SBML_START()
  r += NS_L1()
  r += LV_L1v2()
  r += s
  r += SBML_END()
  return r
  pass

def wrapSBML_L2v1(s):
  r = XML_START()
  r += SBML_START()
  r += NS_L2v1()
  r += LV_L2v1()
  r += s
  r += SBML_END()
  return r
  pass

def wrapSBML_L2v2(s):
  r = XML_START()
  r += SBML_START()
  r += NS_L2v2()
  r += LV_L2v2()
  r += s
  r += SBML_END()
  return r
  pass

def wrapXML(s):
  r = XML_START()
  r += s
  return r
  pass

class TestWriteSBML(unittest.TestCase):

  S = None
  D = None
  XOS = None
  OSS = None

  def equals(self, *x):
    if len(x) == 2:
      return x[0] == x[1]
    elif len(x) == 1:
      return x[0] == self.OSS.str()

  def setUp(self):
    self.D = libsbml.SBMLDocument()
    self.S = 0
    self.OSS = libsbml.ostringstream()
    self.XOS = libsbml.XMLOutputStream(self.OSS)
    pass  

  def tearDown(self):
    self.D = None
    self.S = None
    self.OSS = None
    self.XOS = None
    pass  

  def test_WriteSBML_AlgebraicRule(self):
    self.D.setLevelAndVersion(1,1)
    expected = wrapXML("<algebraicRule formula=\"x + 1\"/>")
    r = libsbml.AlgebraicRule( "x + 1" )
    r.setSBMLDocument(self.D)
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_AlgebraicRule_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<algebraicRule>\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <plus/>\n" + 
    "      <ci> x </ci>\n" + 
    "      <cn type=\"integer\"> 1 </cn>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</algebraicRule>")
    r = libsbml.AlgebraicRule( "x + 1" )
    r.setSBMLDocument(self.D)
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Compartment(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<compartment name=\"A\" volume=\"2.1\" outside=\"B\"/>")
    c = libsbml.Compartment( "A" )
    c.setSize(2.1)
    c.setOutside("B")
    c.setSBMLDocument(self.D)
    c.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_CompartmentVolumeRule(self):
    self.D.setLevelAndVersion(1,1)
    expected = wrapXML("<compartmentVolumeRule " + 
    "formula=\"v + c\" type=\"rate\" compartment=\"c\"/>")
    self.D.createModel()
    self.D.getModel().createCompartment().setId("c")
    r = self.D.getModel().createRateRule()
    r.setVariable("c")
    r.setFormula("v + c")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_CompartmentVolumeRule_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<assignmentRule variable=\"c\">\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <plus/>\n" + 
    "      <ci> v </ci>\n" + 
    "      <ci> c </ci>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</assignmentRule>")
    self.D.createModel()
    self.D.getModel().createCompartment().setId("c")
    r = self.D.getModel().createAssignmentRule()
    r.setVariable("c")
    r.setFormula("v + c")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_CompartmentVolumeRule_defaults(self):
    self.D.setLevelAndVersion(1,1)
    expected = wrapXML("<compartmentVolumeRule formula=\"v + c\" compartment=\"c\"/>")
    self.D.createModel()
    self.D.getModel().createCompartment().setId("c")
    r = self.D.getModel().createAssignmentRule()
    r.setVariable("c")
    r.setFormula("v + c")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Compartment_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<compartment id=\"M\" spatialDimensions=\"2\" size=\"2.5\"/>")
    c = libsbml.Compartment( "M" )
    c.setSize(2.5)
    c.setSpatialDimensions(2)
    c.setSBMLDocument(self.D)
    c.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Compartment_L2v1_constant(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<compartment id=\"cell\" size=\"1.2\" constant=\"false\"/>")
    c = libsbml.Compartment( "cell" )
    c.setSize(1.2)
    c.setConstant(False)
    c.setSBMLDocument(self.D)
    c.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Compartment_L2v1_unsetSize(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<compartment id=\"A\"/>")
    c = libsbml.Compartment()
    c.setId("A")
    c.unsetSize()
    c.setSBMLDocument(self.D)
    c.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Compartment_unsetVolume(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<compartment name=\"A\"/>")
    c = libsbml.Compartment()
    c.setId("A")
    c.unsetVolume()
    c.setSBMLDocument(self.D)
    c.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Event(self):
    expected = wrapXML("<event id=\"e\"/>")
    e = libsbml.Event()
    e.setId("e")
    e.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Event_both(self):
    expected = wrapXML("<event id=\"e\">\n" + 
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
    "</event>")
    e = libsbml.Event( "e" )
    node1 = libsbml.parseFormula("leq(P1,t)")
    t = libsbml.Trigger( node1 )
    node = libsbml.parseFormula("5")
    d = libsbml.Delay( node )
    e.setDelay(d)
    e.setTrigger(t)
    e.setTimeUnits("second")
    e.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Event_delay(self):
    expected = wrapXML("<event id=\"e\">\n" + 
    "  <delay>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <cn type=\"integer\"> 5 </cn>\n" + 
    "    </math>\n" + 
    "  </delay>\n" + 
    "</event>")
    e = libsbml.Event( "e" )
    node = libsbml.parseFormula("5")
    d = libsbml.Delay( node )
    e.setDelay(d)
    e.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Event_full(self):
    expected = wrapXML("<event id=\"e\">\n" + 
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
    "</event>")
    e = libsbml.Event( "e" )
    node = libsbml.parseFormula("leq(P1,t)")
    t = libsbml.Trigger( node )
    math = libsbml.parseFormula("0")
    ea = libsbml.EventAssignment( "k2",math )
    e.setTrigger(t)
    e.addEventAssignment(ea)
    e.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Event_trigger(self):
    expected = wrapXML("<event id=\"e\">\n" + 
    "  <trigger>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <apply>\n" + 
    "        <leq/>\n" + 
    "        <ci> P1 </ci>\n" + 
    "        <ci> t </ci>\n" + 
    "      </apply>\n" + 
    "    </math>\n" + 
    "  </trigger>\n" + 
    "</event>")
    e = libsbml.Event( "e" )
    node = libsbml.parseFormula("leq(P1,t)")
    t = libsbml.Trigger( node )
    e.setTrigger(t)
    e.setTimeUnits("second")
    e.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_FunctionDefinition(self):
    expected = wrapXML("<functionDefinition id=\"pow3\">\n" + 
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
    "</functionDefinition>")
    fd = libsbml.FunctionDefinition( "pow3", "lambda(x, x^3)" )
    fd.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_INF(self):
    expected = wrapXML("<parameter id=\"p\" value=\"INF\"/>")
    p = libsbml.Parameter( "p",util_PosInf() )
    p.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_KineticLaw(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<kineticLaw formula=\"k * e\" timeUnits=\"second\" " + 
    "substanceUnits=\"item\"/>")
    kl = libsbml.KineticLaw( "k * e", "second", "item" )
    kl.setSBMLDocument(self.D)
    kl.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_KineticLaw_ListOfParameters(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<kineticLaw formula=\"nk * e\" timeUnits=\"second\" " + 
    "substanceUnits=\"item\">\n" + 
    "  <listOfParameters>\n" + 
    "    <parameter name=\"n\" value=\"1.2\"/>\n" + 
    "  </listOfParameters>\n" + 
    "</kineticLaw>")
    kl = libsbml.KineticLaw( "nk * e", "second", "item" )
    kl.setSBMLDocument(self.D)
    p = libsbml.Parameter( "n",1.2 )
    kl.addParameter(p)
    kl.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_KineticLaw_skipOptional(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<kineticLaw formula=\"k * e\"/>")
    kl = libsbml.KineticLaw( "k * e" )
    kl.setSBMLDocument(self.D)
    kl.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Model(self):
    self.D.setLevelAndVersion(1,1)
    expected = wrapSBML_L1v1("  <model name=\"Branch\"/>\n")
    self.D.createModel("Branch")
    self.S = libsbml.writeSBMLToString(self.D)
    self.assertEqual( True, self.equals(expected,self.S) )
    pass  

  def test_WriteSBML_Model_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapSBML_L2v1("  <model id=\"Branch\"/>\n")
    self.D.createModel("Branch")
    self.S = libsbml.writeSBMLToString(self.D)
    self.assertEqual( True, self.equals(expected,self.S) )
    pass  

  def test_WriteSBML_Model_L2v1_skipOptional(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapSBML_L2v1("  <model/>\n")
    self.D.createModel()
    self.S = libsbml.writeSBMLToString(self.D)
    self.assertEqual( True, self.equals(expected,self.S) )
    pass  

  def test_WriteSBML_Model_skipOptional(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapSBML_L1v2("  <model/>\n")
    self.D.createModel()
    self.S = libsbml.writeSBMLToString(self.D)
    self.assertEqual( True, self.equals(expected,self.S) )
    pass  

  def test_WriteSBML_NaN(self):
    expected = wrapXML("<parameter id=\"p\" value=\"NaN\"/>")
    p = libsbml.Parameter( "p",util_NaN() )
    p.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_NegINF(self):
    expected = wrapXML("<parameter id=\"p\" value=\"-INF\"/>")
    p = libsbml.Parameter( "p",util_NegInf() )
    p.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Parameter(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<parameter name=\"Km1\" value=\"2.3\" units=\"second\"/>")
    p = libsbml.Parameter( "Km1",2.3, "second" )
    p.setSBMLDocument(self.D)
    p.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_ParameterRule(self):
    self.D.setLevelAndVersion(1,1)
    expected = wrapXML("<parameterRule " + 
    "formula=\"p * t\" type=\"rate\" name=\"p\"/>")
    self.D.createModel()
    self.D.getModel().createParameter().setId("p")
    r = self.D.getModel().createRateRule()
    r.setVariable("p")
    r.setFormula("p * t")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_ParameterRule_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<rateRule variable=\"p\">\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <times/>\n" + 
    "      <ci> p </ci>\n" + 
    "      <ci> t </ci>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</rateRule>")
    self.D.createModel()
    self.D.getModel().createParameter().setId("p")
    r = self.D.getModel().createRateRule()
    r.setVariable("p")
    r.setFormula("p * t")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_ParameterRule_defaults(self):
    self.D.setLevelAndVersion(1,1)
    expected = wrapXML("<parameterRule formula=\"p * t\" name=\"p\"/>")
    self.D.createModel()
    self.D.getModel().createParameter().setId("p")
    r = self.D.getModel().createAssignmentRule()
    r.setVariable("p")
    r.setFormula("p * t")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Parameter_L1v1_required(self):
    self.D.setLevelAndVersion(1,1)
    expected = wrapXML("<parameter name=\"Km1\" value=\"NaN\"/>")
    p = libsbml.Parameter()
    p.setId("Km1")
    p.unsetValue()
    p.setSBMLDocument(self.D)
    p.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Parameter_L1v2_skipOptional(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<parameter name=\"Km1\"/>")
    p = libsbml.Parameter()
    p.setId("Km1")
    p.unsetValue()
    p.setSBMLDocument(self.D)
    p.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Parameter_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<parameter id=\"Km1\" value=\"2.3\" units=\"second\"/>")
    p = libsbml.Parameter( "Km1",2.3, "second" )
    p.setSBMLDocument(self.D)
    p.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Parameter_L2v1_constant(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<parameter id=\"x\" constant=\"false\"/>")
    p = libsbml.Parameter( "x" )
    p.setConstant(False)
    p.setSBMLDocument(self.D)
    p.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Parameter_L2v1_skipOptional(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<parameter id=\"Km1\"/>")
    p = libsbml.Parameter( "Km1" )
    p.setSBMLDocument(self.D)
    p.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Reaction(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<reaction name=\"r\" reversible=\"false\" fast=\"true\"/>")
    r = libsbml.Reaction( "r", "",None,False )
    r.setFast(True)
    r.setSBMLDocument(self.D)
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Reaction_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<reaction id=\"r\" reversible=\"false\"/>")
    r = libsbml.Reaction( "r", "",None,False )
    r.setSBMLDocument(self.D)
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Reaction_L2v1_full(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<reaction id=\"v1\">\n" + 
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
    "</reaction>")
    self.D.createModel()
    r = self.D.getModel().createReaction()
    r.setId("v1")
    r.setReversible(True)
    r.createReactant().setSpecies("x0")
    r.createProduct().setSpecies("s1")
    r.createModifier().setSpecies("m1")
    r.createKineticLaw().setFormula("(vm * s1)/(km + s1)")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Reaction_defaults(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<reaction name=\"r\"/>")
    r = libsbml.Reaction()
    r.setId("r")
    r.setSBMLDocument(self.D)
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Reaction_full(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<reaction name=\"v1\">\n" + 
    "  <listOfReactants>\n" + 
    "    <speciesReference species=\"x0\"/>\n" + 
    "  </listOfReactants>\n" + 
    "  <listOfProducts>\n" + 
    "    <speciesReference species=\"s1\"/>\n" + 
    "  </listOfProducts>\n" + 
    "  <kineticLaw formula=\"(vm * s1)/(km + s1)\"/>\n" + 
    "</reaction>")
    self.D.createModel()
    r = self.D.getModel().createReaction()
    r.setId("v1")
    r.setReversible(True)
    r.createReactant().setSpecies("x0")
    r.createProduct().setSpecies("s1")
    r.createKineticLaw().setFormula("(vm * s1)/(km + s1)")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_SBMLDocument_L1v1(self):
    self.D.setLevelAndVersion(1,1)
    expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level1\" " + 
    "level=\"1\" version=\"1\"/>\n")
    self.S = libsbml.writeSBMLToString(self.D)
    self.assertEqual( True, self.equals(expected,self.S) )
    pass  

  def test_WriteSBML_SBMLDocument_L1v2(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level1\" " + 
    "level=\"1\" version=\"2\"/>\n")
    self.S = libsbml.writeSBMLToString(self.D)
    self.assertEqual( True, self.equals(expected,self.S) )
    pass  

  def test_WriteSBML_SBMLDocument_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\"/>\n")
    self.S = libsbml.writeSBMLToString(self.D)
    self.assertEqual( True, self.equals(expected,self.S) )
    pass  

  def test_WriteSBML_SBMLDocument_L2v2(self):
    self.D.setLevelAndVersion(2,2)
    expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level2/version2\" " + 
    "level=\"2\" version=\"2\"/>\n")
    self.S = libsbml.writeSBMLToString(self.D)
    self.assertEqual( True, self.equals(expected,self.S) )
    pass  

  def test_WriteSBML_Species(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"" + 
    " units=\"mole\" boundaryCondition=\"true\" charge=\"2\"/>")
    s = libsbml.Species( "Ca2" )
    s.setCompartment("cell")
    s.setInitialAmount(0.7)
    s.setUnits("mole")
    s.setBoundaryCondition(True)
    s.setCharge(2)
    s.setSBMLDocument(self.D)
    s.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_SpeciesConcentrationRule(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<speciesConcentrationRule " + 
    "formula=\"t * s\" type=\"rate\" species=\"s\"/>")
    self.D.createModel()
    self.D.getModel().createSpecies().setId("s")
    r = self.D.getModel().createRateRule()
    r.setVariable("s")
    r.setFormula("t * s")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_SpeciesConcentrationRule_L1v1(self):
    self.D.setLevelAndVersion(1,1)
    expected = wrapXML("<specieConcentrationRule formula=\"t * s\" specie=\"s\"/>")
    self.D.createModel()
    self.D.getModel().createSpecies().setId("s")
    r = self.D.getModel().createAssignmentRule()
    r.setVariable("s")
    r.setFormula("t * s")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_SpeciesConcentrationRule_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<assignmentRule variable=\"s\">\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <times/>\n" + 
    "      <ci> t </ci>\n" + 
    "      <ci> s </ci>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</assignmentRule>")
    self.D.createModel()
    self.D.getModel().createSpecies().setId("s")
    r = self.D.getModel().createAssignmentRule()
    r.setVariable("s")
    r.setFormula("t * s")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_SpeciesConcentrationRule_defaults(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<speciesConcentrationRule formula=\"t * s\" species=\"s\"/>")
    self.D.createModel()
    self.D.getModel().createSpecies().setId("s")
    r = self.D.getModel().createAssignmentRule()
    r.setVariable("s")
    r.setFormula("t * s")
    r.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_SpeciesReference(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<speciesReference species=\"s\" stoichiometry=\"3\" denominator=\"2\"/>")
    sr = libsbml.SpeciesReference( "s",3,2 )
    sr.setSBMLDocument(self.D)
    sr.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_SpeciesReference_L1v1(self):
    self.D.setLevelAndVersion(1,1)
    expected = wrapXML("<specieReference specie=\"s\" stoichiometry=\"3\" denominator=\"2\"/>")
    sr = libsbml.SpeciesReference( "s",3,2 )
    sr.setSBMLDocument(self.D)
    sr.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_SpeciesReference_L2v1_1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<speciesReference species=\"s\">\n" + 
    "  <stoichiometryMath>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <cn type=\"rational\"> 3 <sep/> 2 </cn>\n" + 
    "    </math>\n" + 
    "  </stoichiometryMath>\n" + 
    "</speciesReference>")
    sr = libsbml.SpeciesReference( "s",3,2 )
    sr.setSBMLDocument(self.D)
    sr.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_SpeciesReference_L2v1_2(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<speciesReference species=\"s\" stoichiometry=\"3.2\"/>")
    sr = libsbml.SpeciesReference( "s",3.2 )
    sr.setSBMLDocument(self.D)
    sr.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_SpeciesReference_L2v1_3(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<speciesReference species=\"s\">\n" + 
    "  <stoichiometryMath>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <apply>\n" + 
    "        <divide/>\n" + 
    "        <cn type=\"integer\"> 1 </cn>\n" + 
    "        <ci> d </ci>\n" + 
    "      </apply>\n" + 
    "    </math>\n" + 
    "  </stoichiometryMath>\n" + 
    "</speciesReference>")
    sr = libsbml.SpeciesReference( "s" )
    math = libsbml.parseFormula("1/d")
    stoich = libsbml.StoichiometryMath()
    stoich.setMath(math)
    sr.setStoichiometryMath(stoich)
    sr.setSBMLDocument(self.D)
    sr.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_SpeciesReference_defaults(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<speciesReference species=\"s\"/>")
    sr = libsbml.SpeciesReference( "s" )
    sr.setSBMLDocument(self.D)
    sr.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Species_L1v1(self):
    self.D.setLevelAndVersion(1,1)
    expected = wrapXML("<specie name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"" + 
    " units=\"mole\" boundaryCondition=\"true\" charge=\"2\"/>")
    s = libsbml.Species( "Ca2" )
    s.setCompartment("cell")
    s.setInitialAmount(0.7)
    s.setUnits("mole")
    s.setBoundaryCondition(True)
    s.setCharge(2)
    s.setSBMLDocument(self.D)
    s.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Species_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<species id=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\" " + 
    "substanceUnits=\"mole\" constant=\"true\"/>")
    s = libsbml.Species( "Ca2" )
    s.setCompartment("cell")
    s.setInitialAmount(0.7)
    s.setSubstanceUnits("mole")
    s.setConstant(True)
    s.setSBMLDocument(self.D)
    s.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Species_L2v1_skipOptional(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<species id=\"Ca2\" compartment=\"cell\"/>")
    s = libsbml.Species( "Ca2" )
    s.setCompartment("cell")
    s.setSBMLDocument(self.D)
    s.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Species_defaults(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"" + 
    " units=\"mole\" charge=\"2\"/>")
    s = libsbml.Species( "Ca2" )
    s.setCompartment("cell")
    s.setInitialAmount(0.7)
    s.setUnits("mole")
    s.setCharge(2)
    s.setSBMLDocument(self.D)
    s.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Species_skipOptional(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"/>")
    s = libsbml.Species( "Ca2" )
    s.setCompartment("cell")
    s.setInitialAmount(0.7)
    s.setSBMLDocument(self.D)
    s.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Unit(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<unit kind=\"kilogram\" exponent=\"2\" scale=\"-3\"/>")
    u = libsbml.Unit( "kilogram",2,-3 )
    u.setSBMLDocument(self.D)
    u.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_UnitDefinition(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<unitDefinition name=\"mmls\"/>")
    ud = libsbml.UnitDefinition( "mmls" )
    ud.setSBMLDocument(self.D)
    ud.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_UnitDefinition_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<unitDefinition id=\"mmls\"/>")
    ud = libsbml.UnitDefinition( "mmls" )
    ud.setSBMLDocument(self.D)
    ud.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_UnitDefinition_L2v1_full(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<unitDefinition id=\"Fahrenheit\">\n" + 
    "  <listOfUnits>\n" + 
    "    <unit kind=\"Celsius\" multiplier=\"1.8\" offset=\"32\"/>\n" + 
    "  </listOfUnits>\n" + 
    "</unitDefinition>")
    u1 = libsbml.Unit( "Celsius",1,0,1.8 )
    u1.setOffset(32)
    ud = libsbml.UnitDefinition( "Fahrenheit" )
    ud.addUnit(u1)
    ud.setSBMLDocument(self.D)
    ud.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_UnitDefinition_full(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<unitDefinition name=\"mmls\">\n" + 
    "  <listOfUnits>\n" + 
    "    <unit kind=\"mole\" scale=\"-3\"/>\n" + 
    "    <unit kind=\"liter\" exponent=\"-1\"/>\n" + 
    "    <unit kind=\"second\" exponent=\"-1\"/>\n" + 
    "  </listOfUnits>\n" + 
    "</unitDefinition>")
    ud = libsbml.UnitDefinition( "mmls" )
    u1 = libsbml.Unit( "mole"  ,1,-3 )
    u2 = libsbml.Unit( "liter" ,-1 )
    u3 = libsbml.Unit( "second",-1 )
    ud.addUnit(u1)
    ud.addUnit(u2)
    ud.addUnit(u3)
    ud.setSBMLDocument(self.D)
    ud.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Unit_L2v1(self):
    self.D.setLevelAndVersion(2,1)
    expected = wrapXML("<unit kind=\"Celsius\" multiplier=\"1.8\" offset=\"32\"/>")
    u = libsbml.Unit( "Celsius",1,0,1.8 )
    u.setOffset(32)
    u.setSBMLDocument(self.D)
    u.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Unit_defaults(self):
    self.D.setLevelAndVersion(1,2)
    expected = wrapXML("<unit kind=\"kilogram\"/>")
    u = libsbml.Unit( "kilogram",1,0 )
    u.setSBMLDocument(self.D)
    u.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_Unit_l2v3(self):
    self.D.setLevelAndVersion(2,3)
    expected = wrapXML("<unit kind=\"kilogram\" exponent=\"2\" scale=\"-3\"/>")
    u = libsbml.Unit( "kilogram",2,-3 )
    u.setOffset(32)
    u.setSBMLDocument(self.D)
    u.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

  def test_WriteSBML_error(self):
    d = libsbml.SBMLDocument()
    w = libsbml.SBMLWriter()
    self.assertEqual( False, w.writeSBML(d, "/tmp/impossible/path/should/fail") )
    self.assert_( d.getNumErrors() == 1 )
    self.assert_( d.getError(0).getErrorId() == libsbml.XMLFileUnwritable )
    d = None
    w = None
    pass  

  def test_WriteSBML_locale(self):
    expected = wrapXML("<parameter id=\"p\" value=\"3.31\"/>")
    p = libsbml.Parameter("p",3.31)
    p.write(self.XOS)
    self.assertEqual( True, self.equals(expected) )
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestWriteSBML))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
