#
# @file    TestWriteSBML.rb
# @brief   Write SBML unit tests
# @author  Akiya Jouraku (Ruby conversion)
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
require 'test/unit'
require 'libSBML'

class TestWriteSBML < Test::Unit::TestCase

def LV_L1v1
  return "level=\"1\" version=\"1\">\n"
end

def LV_L1v2
  return "level=\"1\" version=\"2\">\n"
end

def LV_L2v1
  return "level=\"2\" version=\"1\">\n"
end

def LV_L2v2
  return "level=\"2\" version=\"2\">\n"
end

def NS_L1
  return "xmlns=\"http://www.sbml.org/sbml/level1\" "
end

def NS_L2v1
  return "xmlns=\"http://www.sbml.org/sbml/level2\" "
end

def NS_L2v2
  return "xmlns=\"http://www.sbml.org/sbml/level2/version2\" "
end

def SBML_END
  return "</sbml>\n"
end

def SBML_START
  return "<sbml "
end

def XML_START
  return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
end

def wrapSBML_L1v1(s)
  r = XML_START()
  r += SBML_START()
  r += NS_L1()
  r += LV_L1v1()
  r += s
  r += SBML_END()
  return r
end

def wrapSBML_L1v2(s)
  r = XML_START()
  r += SBML_START()
  r += NS_L1()
  r += LV_L1v2()
  r += s
  r += SBML_END()
  return r
end

def wrapSBML_L2v1(s)
  r = XML_START()
  r += SBML_START()
  r += NS_L2v1()
  r += LV_L2v1()
  r += s
  r += SBML_END()
  return r
end

def wrapSBML_L2v2(s)
  r = XML_START()
  r += SBML_START()
  r += NS_L2v2()
  r += LV_L2v2()
  r += s
  r += SBML_END()
  return r
end

def wrapXML(s)
  r = XML_START()
  r += s
  return r
end

  def util_NaN
    z = 0.0
    return 0.0/z
  end

  def util_PosInf
    z = 0.0
    return 1.0/z
  end

  def util_NegInf
    z = 0.0
    return -1.0/z
  end

  def equals(*x)
    case x.size
    when 2
      e, s = x
      return e == s
    when 1
      e, = x
      return e == @@oss.str()
    end
  end

  def setup
    @@d = LibSBML::SBMLDocument.new()
    @@s = 0
    @@oss = LibSBML::Ostringstream.new()
    @@xos = LibSBML::XMLOutputStream.new(@@oss)
  end

  def teardown
    @@d = nil
    @@s = nil
    @@oss = nil
    @@xos = nil
  end

  def test_WriteSBML_AlgebraicRule
    @@d.setLevelAndVersion(1,1)
    expected = wrapXML("<algebraicRule formula=\"x + 1\"/>")
    r = LibSBML::AlgebraicRule.new( "x + 1" )
    r.setSBMLDocument(@@d)
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_AlgebraicRule_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<algebraicRule>\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <plus/>\n" + 
    "      <ci> x </ci>\n" + 
    "      <cn type=\"integer\"> 1 </cn>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</algebraicRule>")
    r = LibSBML::AlgebraicRule.new( "x + 1" )
    r.setSBMLDocument(@@d)
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Compartment
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<compartment name=\"A\" volume=\"2.1\" outside=\"B\"/>")
    c = LibSBML::Compartment.new( "A" )
    c.setSize(2.1)
    c.setOutside("B")
    c.setSBMLDocument(@@d)
    c.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_CompartmentVolumeRule
    @@d.setLevelAndVersion(1,1)
    expected = wrapXML("<compartmentVolumeRule " + 
    "formula=\"v + c\" type=\"rate\" compartment=\"c\"/>")
    @@d.createModel()
    @@d.getModel().createCompartment().setId("c")
    r = @@d.getModel().createRateRule()
    r.setVariable("c")
    r.setFormula("v + c")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_CompartmentVolumeRule_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<assignmentRule variable=\"c\">\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <plus/>\n" + 
    "      <ci> v </ci>\n" + 
    "      <ci> c </ci>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</assignmentRule>")
    @@d.createModel()
    @@d.getModel().createCompartment().setId("c")
    r = @@d.getModel().createAssignmentRule()
    r.setVariable("c")
    r.setFormula("v + c")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_CompartmentVolumeRule_defaults
    @@d.setLevelAndVersion(1,1)
    expected = wrapXML("<compartmentVolumeRule formula=\"v + c\" compartment=\"c\"/>")
    @@d.createModel()
    @@d.getModel().createCompartment().setId("c")
    r = @@d.getModel().createAssignmentRule()
    r.setVariable("c")
    r.setFormula("v + c")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Compartment_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<compartment id=\"M\" spatialDimensions=\"2\" size=\"2.5\"/>")
    c = LibSBML::Compartment.new( "M" )
    c.setSize(2.5)
    c.setSpatialDimensions(2)
    c.setSBMLDocument(@@d)
    c.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Compartment_L2v1_constant
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<compartment id=\"cell\" size=\"1.2\" constant=\"false\"/>")
    c = LibSBML::Compartment.new( "cell" )
    c.setSize(1.2)
    c.setConstant(false)
    c.setSBMLDocument(@@d)
    c.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Compartment_L2v1_unsetSize
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<compartment id=\"A\"/>")
    c = LibSBML::Compartment.new()
    c.setId("A")
    c.unsetSize()
    c.setSBMLDocument(@@d)
    c.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Compartment_unsetVolume
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<compartment name=\"A\"/>")
    c = LibSBML::Compartment.new()
    c.setId("A")
    c.unsetVolume()
    c.setSBMLDocument(@@d)
    c.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Event
    expected = wrapXML("<event id=\"e\"/>")
    e = LibSBML::Event.new()
    e.setId("e")
    e.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Event_both
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
    e = LibSBML::Event.new( "e" )
    node1 = LibSBML::parseFormula("leq(P1,t)")
    t = LibSBML::Trigger.new( node1 )
    node = LibSBML::parseFormula("5")
    d = LibSBML::Delay.new( node )
    e.setDelay(d)
    e.setTrigger(t)
    e.setTimeUnits("second")
    e.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Event_delay
    expected = wrapXML("<event id=\"e\">\n" + 
    "  <delay>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <cn type=\"integer\"> 5 </cn>\n" + 
    "    </math>\n" + 
    "  </delay>\n" + 
    "</event>")
    e = LibSBML::Event.new( "e" )
    node = LibSBML::parseFormula("5")
    d = LibSBML::Delay.new( node )
    e.setDelay(d)
    e.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Event_full
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
    e = LibSBML::Event.new( "e" )
    node = LibSBML::parseFormula("leq(P1,t)")
    t = LibSBML::Trigger.new( node )
    math = LibSBML::parseFormula("0")
    ea = LibSBML::EventAssignment.new( "k2",math )
    e.setTrigger(t)
    e.addEventAssignment(ea)
    e.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Event_trigger
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
    e = LibSBML::Event.new( "e" )
    node = LibSBML::parseFormula("leq(P1,t)")
    t = LibSBML::Trigger.new( node )
    e.setTrigger(t)
    e.setTimeUnits("second")
    e.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_FunctionDefinition
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
    fd = LibSBML::FunctionDefinition.new( "pow3", "lambda(x, x^3)" )
    fd.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_INF
    expected = wrapXML("<parameter id=\"p\" value=\"INF\"/>")
    p = LibSBML::Parameter.new( "p",util_PosInf )
    p.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_KineticLaw
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<kineticLaw formula=\"k * e\" timeUnits=\"second\" " + 
    "substanceUnits=\"item\"/>")
    kl = LibSBML::KineticLaw.new( "k * e", "second", "item" )
    kl.setSBMLDocument(@@d)
    kl.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_KineticLaw_ListOfParameters
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<kineticLaw formula=\"nk * e\" timeUnits=\"second\" " + 
    "substanceUnits=\"item\">\n" + 
    "  <listOfParameters>\n" + 
    "    <parameter name=\"n\" value=\"1.2\"/>\n" + 
    "  </listOfParameters>\n" + 
    "</kineticLaw>")
    kl = LibSBML::KineticLaw.new( "nk * e", "second", "item" )
    kl.setSBMLDocument(@@d)
    p = LibSBML::Parameter.new( "n",1.2 )
    kl.addParameter(p)
    kl.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_KineticLaw_skipOptional
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<kineticLaw formula=\"k * e\"/>")
    kl = LibSBML::KineticLaw.new( "k * e" )
    kl.setSBMLDocument(@@d)
    kl.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Model
    @@d.setLevelAndVersion(1,1)
    expected = wrapSBML_L1v1("  <model name=\"Branch\"/>\n")
    @@d.createModel("Branch")
    @@s = LibSBML::writeSBMLToString(@@d)
    assert_equal true, equals(expected,@@s)
  end

  def test_WriteSBML_Model_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapSBML_L2v1("  <model id=\"Branch\"/>\n")
    @@d.createModel("Branch")
    @@s = LibSBML::writeSBMLToString(@@d)
    assert_equal true, equals(expected,@@s)
  end

  def test_WriteSBML_Model_L2v1_skipOptional
    @@d.setLevelAndVersion(2,1)
    expected = wrapSBML_L2v1("  <model/>\n")
    @@d.createModel()
    @@s = LibSBML::writeSBMLToString(@@d)
    assert_equal true, equals(expected,@@s)
  end

  def test_WriteSBML_Model_skipOptional
    @@d.setLevelAndVersion(1,2)
    expected = wrapSBML_L1v2("  <model/>\n")
    @@d.createModel()
    @@s = LibSBML::writeSBMLToString(@@d)
    assert_equal true, equals(expected,@@s)
  end

  def test_WriteSBML_NaN
    expected = wrapXML("<parameter id=\"p\" value=\"NaN\"/>")
    p = LibSBML::Parameter.new( "p",util_NaN )
    p.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_NegINF
    expected = wrapXML("<parameter id=\"p\" value=\"-INF\"/>")
    p = LibSBML::Parameter.new( "p",util_NegInf )
    p.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Parameter
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<parameter name=\"Km1\" value=\"2.3\" units=\"second\"/>")
    p = LibSBML::Parameter.new( "Km1",2.3, "second" )
    p.setSBMLDocument(@@d)
    p.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_ParameterRule
    @@d.setLevelAndVersion(1,1)
    expected = wrapXML("<parameterRule " + 
    "formula=\"p * t\" type=\"rate\" name=\"p\"/>")
    @@d.createModel()
    @@d.getModel().createParameter().setId("p")
    r = @@d.getModel().createRateRule()
    r.setVariable("p")
    r.setFormula("p * t")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_ParameterRule_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<rateRule variable=\"p\">\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <times/>\n" + 
    "      <ci> p </ci>\n" + 
    "      <ci> t </ci>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</rateRule>")
    @@d.createModel()
    @@d.getModel().createParameter().setId("p")
    r = @@d.getModel().createRateRule()
    r.setVariable("p")
    r.setFormula("p * t")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_ParameterRule_defaults
    @@d.setLevelAndVersion(1,1)
    expected = wrapXML("<parameterRule formula=\"p * t\" name=\"p\"/>")
    @@d.createModel()
    @@d.getModel().createParameter().setId("p")
    r = @@d.getModel().createAssignmentRule()
    r.setVariable("p")
    r.setFormula("p * t")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Parameter_L1v1_required
    @@d.setLevelAndVersion(1,1)
    expected = wrapXML("<parameter name=\"Km1\" value=\"NaN\"/>")
    p = LibSBML::Parameter.new()
    p.setId("Km1")
    p.unsetValue()
    p.setSBMLDocument(@@d)
    p.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Parameter_L1v2_skipOptional
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<parameter name=\"Km1\"/>")
    p = LibSBML::Parameter.new()
    p.setId("Km1")
    p.unsetValue()
    p.setSBMLDocument(@@d)
    p.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Parameter_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<parameter id=\"Km1\" value=\"2.3\" units=\"second\"/>")
    p = LibSBML::Parameter.new( "Km1",2.3, "second" )
    p.setSBMLDocument(@@d)
    p.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Parameter_L2v1_constant
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<parameter id=\"x\" constant=\"false\"/>")
    p = LibSBML::Parameter.new( "x" )
    p.setConstant(false)
    p.setSBMLDocument(@@d)
    p.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Parameter_L2v1_skipOptional
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<parameter id=\"Km1\"/>")
    p = LibSBML::Parameter.new( "Km1" )
    p.setSBMLDocument(@@d)
    p.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Reaction
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<reaction name=\"r\" reversible=\"false\" fast=\"true\"/>")
    r = LibSBML::Reaction.new( "r", "",nil,false )
    r.setFast(true)
    r.setSBMLDocument(@@d)
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Reaction_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<reaction id=\"r\" reversible=\"false\"/>")
    r = LibSBML::Reaction.new( "r", "",nil,false )
    r.setSBMLDocument(@@d)
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Reaction_L2v1_full
    @@d.setLevelAndVersion(2,1)
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
    @@d.createModel()
    r = @@d.getModel().createReaction()
    r.setId("v1")
    r.setReversible(true)
    r.createReactant().setSpecies("x0")
    r.createProduct().setSpecies("s1")
    r.createModifier().setSpecies("m1")
    r.createKineticLaw().setFormula("(vm * s1)/(km + s1)")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Reaction_defaults
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<reaction name=\"r\"/>")
    r = LibSBML::Reaction.new()
    r.setId("r")
    r.setSBMLDocument(@@d)
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Reaction_full
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<reaction name=\"v1\">\n" + 
    "  <listOfReactants>\n" + 
    "    <speciesReference species=\"x0\"/>\n" + 
    "  </listOfReactants>\n" + 
    "  <listOfProducts>\n" + 
    "    <speciesReference species=\"s1\"/>\n" + 
    "  </listOfProducts>\n" + 
    "  <kineticLaw formula=\"(vm * s1)/(km + s1)\"/>\n" + 
    "</reaction>")
    @@d.createModel()
    r = @@d.getModel().createReaction()
    r.setId("v1")
    r.setReversible(true)
    r.createReactant().setSpecies("x0")
    r.createProduct().setSpecies("s1")
    r.createKineticLaw().setFormula("(vm * s1)/(km + s1)")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_SBMLDocument_L1v1
    @@d.setLevelAndVersion(1,1)
    expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level1\" " + 
    "level=\"1\" version=\"1\"/>\n")
    @@s = LibSBML::writeSBMLToString(@@d)
    assert_equal true, equals(expected,@@s)
  end

  def test_WriteSBML_SBMLDocument_L1v2
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level1\" " + 
    "level=\"1\" version=\"2\"/>\n")
    @@s = LibSBML::writeSBMLToString(@@d)
    assert_equal true, equals(expected,@@s)
  end

  def test_WriteSBML_SBMLDocument_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level2\" " + 
    "level=\"2\" version=\"1\"/>\n")
    @@s = LibSBML::writeSBMLToString(@@d)
    assert_equal true, equals(expected,@@s)
  end

  def test_WriteSBML_SBMLDocument_L2v2
    @@d.setLevelAndVersion(2,2)
    expected = wrapXML("<sbml xmlns=\"http://www.sbml.org/sbml/level2/version2\" " + 
    "level=\"2\" version=\"2\"/>\n")
    @@s = LibSBML::writeSBMLToString(@@d)
    assert_equal true, equals(expected,@@s)
  end

  def test_WriteSBML_Species
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"" + 
    " units=\"mole\" boundaryCondition=\"true\" charge=\"2\"/>")
    s = LibSBML::Species.new( "Ca2" )
    s.setCompartment("cell")
    s.setInitialAmount(0.7)
    s.setUnits("mole")
    s.setBoundaryCondition(true)
    s.setCharge(2)
    s.setSBMLDocument(@@d)
    s.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_SpeciesConcentrationRule
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<speciesConcentrationRule " + 
    "formula=\"t * s\" type=\"rate\" species=\"s\"/>")
    @@d.createModel()
    @@d.getModel().createSpecies().setId("s")
    r = @@d.getModel().createRateRule()
    r.setVariable("s")
    r.setFormula("t * s")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_SpeciesConcentrationRule_L1v1
    @@d.setLevelAndVersion(1,1)
    expected = wrapXML("<specieConcentrationRule formula=\"t * s\" specie=\"s\"/>")
    @@d.createModel()
    @@d.getModel().createSpecies().setId("s")
    r = @@d.getModel().createAssignmentRule()
    r.setVariable("s")
    r.setFormula("t * s")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_SpeciesConcentrationRule_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<assignmentRule variable=\"s\">\n" + 
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "    <apply>\n" + 
    "      <times/>\n" + 
    "      <ci> t </ci>\n" + 
    "      <ci> s </ci>\n" + 
    "    </apply>\n" + 
    "  </math>\n" + 
    "</assignmentRule>")
    @@d.createModel()
    @@d.getModel().createSpecies().setId("s")
    r = @@d.getModel().createAssignmentRule()
    r.setVariable("s")
    r.setFormula("t * s")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_SpeciesConcentrationRule_defaults
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<speciesConcentrationRule formula=\"t * s\" species=\"s\"/>")
    @@d.createModel()
    @@d.getModel().createSpecies().setId("s")
    r = @@d.getModel().createAssignmentRule()
    r.setVariable("s")
    r.setFormula("t * s")
    r.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_SpeciesReference
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<speciesReference species=\"s\" stoichiometry=\"3\" denominator=\"2\"/>")
    sr = LibSBML::SpeciesReference.new( "s",3,2 )
    sr.setSBMLDocument(@@d)
    sr.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_SpeciesReference_L1v1
    @@d.setLevelAndVersion(1,1)
    expected = wrapXML("<specieReference specie=\"s\" stoichiometry=\"3\" denominator=\"2\"/>")
    sr = LibSBML::SpeciesReference.new( "s",3,2 )
    sr.setSBMLDocument(@@d)
    sr.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_SpeciesReference_L2v1_1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<speciesReference species=\"s\">\n" + 
    "  <stoichiometryMath>\n" + 
    "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n" + 
    "      <cn type=\"rational\"> 3 <sep/> 2 </cn>\n" + 
    "    </math>\n" + 
    "  </stoichiometryMath>\n" + 
    "</speciesReference>")
    sr = LibSBML::SpeciesReference.new( "s",3,2 )
    sr.setSBMLDocument(@@d)
    sr.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_SpeciesReference_L2v1_2
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<speciesReference species=\"s\" stoichiometry=\"3.2\"/>")
    sr = LibSBML::SpeciesReference.new( "s",3.2 )
    sr.setSBMLDocument(@@d)
    sr.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_SpeciesReference_L2v1_3
    @@d.setLevelAndVersion(2,1)
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
    sr = LibSBML::SpeciesReference.new( "s" )
    math = LibSBML::parseFormula("1/d")
    stoich = LibSBML::StoichiometryMath.new()
    stoich.setMath(math)
    sr.setStoichiometryMath(stoich)
    sr.setSBMLDocument(@@d)
    sr.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_SpeciesReference_defaults
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<speciesReference species=\"s\"/>")
    sr = LibSBML::SpeciesReference.new( "s" )
    sr.setSBMLDocument(@@d)
    sr.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Species_L1v1
    @@d.setLevelAndVersion(1,1)
    expected = wrapXML("<specie name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"" + 
    " units=\"mole\" boundaryCondition=\"true\" charge=\"2\"/>")
    s = LibSBML::Species.new( "Ca2" )
    s.setCompartment("cell")
    s.setInitialAmount(0.7)
    s.setUnits("mole")
    s.setBoundaryCondition(true)
    s.setCharge(2)
    s.setSBMLDocument(@@d)
    s.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Species_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<species id=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\" " + 
    "substanceUnits=\"mole\" constant=\"true\"/>")
    s = LibSBML::Species.new( "Ca2" )
    s.setCompartment("cell")
    s.setInitialAmount(0.7)
    s.setSubstanceUnits("mole")
    s.setConstant(true)
    s.setSBMLDocument(@@d)
    s.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Species_L2v1_skipOptional
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<species id=\"Ca2\" compartment=\"cell\"/>")
    s = LibSBML::Species.new( "Ca2" )
    s.setCompartment("cell")
    s.setSBMLDocument(@@d)
    s.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Species_defaults
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"" + 
    " units=\"mole\" charge=\"2\"/>")
    s = LibSBML::Species.new( "Ca2" )
    s.setCompartment("cell")
    s.setInitialAmount(0.7)
    s.setUnits("mole")
    s.setCharge(2)
    s.setSBMLDocument(@@d)
    s.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Species_skipOptional
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\"/>")
    s = LibSBML::Species.new( "Ca2" )
    s.setCompartment("cell")
    s.setInitialAmount(0.7)
    s.setSBMLDocument(@@d)
    s.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Unit
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<unit kind=\"kilogram\" exponent=\"2\" scale=\"-3\"/>")
    u = LibSBML::Unit.new( "kilogram",2,-3 )
    u.setSBMLDocument(@@d)
    u.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_UnitDefinition
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<unitDefinition name=\"mmls\"/>")
    ud = LibSBML::UnitDefinition.new( "mmls" )
    ud.setSBMLDocument(@@d)
    ud.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_UnitDefinition_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<unitDefinition id=\"mmls\"/>")
    ud = LibSBML::UnitDefinition.new( "mmls" )
    ud.setSBMLDocument(@@d)
    ud.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_UnitDefinition_L2v1_full
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<unitDefinition id=\"Fahrenheit\">\n" + 
    "  <listOfUnits>\n" + 
    "    <unit kind=\"Celsius\" multiplier=\"1.8\" offset=\"32\"/>\n" + 
    "  </listOfUnits>\n" + 
    "</unitDefinition>")
    u1 = LibSBML::Unit.new( "Celsius",1,0,1.8 )
    u1.setOffset(32)
    ud = LibSBML::UnitDefinition.new( "Fahrenheit" )
    ud.addUnit(u1)
    ud.setSBMLDocument(@@d)
    ud.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_UnitDefinition_full
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<unitDefinition name=\"mmls\">\n" + 
    "  <listOfUnits>\n" + 
    "    <unit kind=\"mole\" scale=\"-3\"/>\n" + 
    "    <unit kind=\"liter\" exponent=\"-1\"/>\n" + 
    "    <unit kind=\"second\" exponent=\"-1\"/>\n" + 
    "  </listOfUnits>\n" + 
    "</unitDefinition>")
    ud = LibSBML::UnitDefinition.new( "mmls" )
    u1 = LibSBML::Unit.new( "mole"  ,1,-3 )
    u2 = LibSBML::Unit.new( "liter" ,-1 )
    u3 = LibSBML::Unit.new( "second",-1 )
    ud.addUnit(u1)
    ud.addUnit(u2)
    ud.addUnit(u3)
    ud.setSBMLDocument(@@d)
    ud.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Unit_L2v1
    @@d.setLevelAndVersion(2,1)
    expected = wrapXML("<unit kind=\"Celsius\" multiplier=\"1.8\" offset=\"32\"/>")
    u = LibSBML::Unit.new( "Celsius",1,0,1.8 )
    u.setOffset(32)
    u.setSBMLDocument(@@d)
    u.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Unit_defaults
    @@d.setLevelAndVersion(1,2)
    expected = wrapXML("<unit kind=\"kilogram\"/>")
    u = LibSBML::Unit.new( "kilogram",1,0 )
    u.setSBMLDocument(@@d)
    u.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_Unit_l2v3
    @@d.setLevelAndVersion(2,3)
    expected = wrapXML("<unit kind=\"kilogram\" exponent=\"2\" scale=\"-3\"/>")
    u = LibSBML::Unit.new( "kilogram",2,-3 )
    u.setOffset(32)
    u.setSBMLDocument(@@d)
    u.write(@@xos)
    assert_equal true, equals(expected)
  end

  def test_WriteSBML_error
    d = LibSBML::SBMLDocument.new()
    w = LibSBML::SBMLWriter.new()
    assert_equal false, w.writeSBML(d, "/tmp/impossible/path/should/fail")
    assert( d.getNumErrors() == 1 )
    assert( d.getError(0).getErrorId() == LibSBML::XMLFileUnwritable )
    d = nil
    w = nil
  end

  def test_WriteSBML_locale
    expected = wrapXML("<parameter id=\"p\" value=\"3.31\"/>")
    p = LibSBML::Parameter.new("p",3.31)
    p.write(@@xos)
    assert_equal true, equals(expected)
  end

end
