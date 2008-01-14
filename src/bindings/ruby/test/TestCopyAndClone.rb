#
# @file    TestCopyAndClone.rb
# @brief   Read SBML unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestCopyAndClone.cpp
# wiht the help of conversion sciprt (ctest_converter.pl).
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

class TestCopyAndClone < Test::Unit::TestCase

  def test_CompartmentType_assignmentOperator
    o1 = LibSBML::CompartmentType.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::CompartmentType.new()
    o2 = o1
    assert( o2.getId() == "c" )
  end

  def test_CompartmentType_clone
    o1 = LibSBML::CompartmentType.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
  end

  def test_CompartmentType_copyConstructor
    o1 = LibSBML::CompartmentType.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::CompartmentType.new(o1)
    assert( o2.getId() == "c" )
  end

  def test_Compartment_assignmentOperator
    o1 = LibSBML::Compartment.new()
    o1.setId("c")
    o1.setOutside("c2")
    assert( o1.getId() == "c" )
    assert( o1.getOutside() == "c2" )
    o2 = LibSBML::Compartment.new()
    o2 = o1
    assert( o2.getId() == "c" )
    assert( o2.getOutside() == "c2" )
  end

  def test_Compartment_clone
    o1 = LibSBML::Compartment.new()
    o1.setId("c")
    o1.setOutside("c2")
    assert( o1.getId() == "c" )
    assert( o1.getOutside() == "c2" )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
    assert( o2.getOutside() == "c2" )
  end

  def test_Compartment_copyConstructor
    o1 = LibSBML::Compartment.new()
    o1.setId("c")
    o1.setOutside("c2")
    assert( o1.getId() == "c" )
    assert( o1.getOutside() == "c2" )
    o2 = LibSBML::Compartment.new(o1)
    assert( o2.getId() == "c" )
    assert( o2.getOutside() == "c2" )
  end

  def test_Constraint_assignmentOperator
    o1 = LibSBML::Constraint.new()
    o1.setMetaId("c")
    assert( o1.getMetaId() == "c" )
    math = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    message = LibSBML::XMLNode.new()
    o1.setMath(math)
    o1.setMessage(message)
    assert( o1.getMath() != nil )
    assert( o1.getMessage() != nil )
    o2 = LibSBML::Constraint.new()
    o2 = o1
    assert( o2.getMetaId() == "c" )
    assert( o1.getMath() != nil )
    assert( o1.getMessage() != nil )
  end

  def test_Constraint_clone
    o1 = LibSBML::Constraint.new()
    o1.setMetaId("c")
    assert( o1.getMetaId() == "c" )
    math = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    message = LibSBML::XMLNode.new()
    o1.setMath(math)
    o1.setMessage(message)
    assert( o1.getMath() != nil )
    assert( o1.getMessage() != nil )
    o2 = o1.clone()
    assert( o2.getMetaId() == "c" )
    assert( o1.getMath() != nil )
    assert( o1.getMessage() != nil )
  end

  def test_Constraint_copyConstructor
    o1 = LibSBML::Constraint.new()
    o1.setMetaId("c")
    assert( o1.getMetaId() == "c" )
    math = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    message = LibSBML::XMLNode.new()
    o1.setMath(math)
    o1.setMessage(message)
    assert( o1.getMath() != nil )
    assert( o1.getMessage() != nil )
    o2 = LibSBML::Constraint.new(o1)
    assert( o2.getMetaId() == "c" )
    assert( o1.getMath() != nil )
    assert( o1.getMessage() != nil )
  end

  def test_Delay_assignmentOperator
    o1 = LibSBML::Delay.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = LibSBML::Delay.new()
    o2 = o1
    assert( o1.getMath() != nil )
  end

  def test_Delay_clone
    o1 = LibSBML::Delay.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = o1.clone()
    assert( o1.getMath() != nil )
  end

  def test_Delay_copyConstructor
    o1 = LibSBML::Delay.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = LibSBML::Delay.new(o1)
    assert( o2.getMath() != nil )
  end

  def test_EventAssignment_assignmentOperator
    o1 = LibSBML::EventAssignment.new()
    o1.setVariable("c2")
    assert( o1.getVariable() == "c2" )
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = LibSBML::EventAssignment.new()
    o2 = o1
    assert( o2.getVariable() == "c2" )
    assert( o2.getMath() != nil )
  end

  def test_EventAssignment_clone
    o1 = LibSBML::EventAssignment.new()
    o1.setVariable("c2")
    assert( o1.getVariable() == "c2" )
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = o1.clone()
    assert( o2.getVariable() == "c2" )
    assert( o2.getMath() != nil )
  end

  def test_EventAssignment_copyConstructor
    o1 = LibSBML::EventAssignment.new()
    o1.setVariable("c2")
    assert( o1.getVariable() == "c2" )
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = LibSBML::EventAssignment.new(o1)
    assert( o2.getVariable() == "c2" )
    assert( o2.getMath() != nil )
  end

  def test_Event_assignmentOperator
    o1 = LibSBML::Event.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::Event.new()
    o2 = o1
    assert( o2.getId() == "c" )
  end

  def test_Event_clone
    o1 = LibSBML::Event.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
  end

  def test_Event_copyConstructor
    o1 = LibSBML::Event.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::Event.new(o1)
    assert( o2.getId() == "c" )
  end

  def test_FunctionDefinition_assignmentOperator
    o1 = LibSBML::FunctionDefinition.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = LibSBML::FunctionDefinition.new()
    o2 = o1
    assert( o2.getId() == "c" )
    assert( o2.getMath() != nil )
  end

  def test_FunctionDefinition_clone
    o1 = LibSBML::FunctionDefinition.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
    assert( o2.getMath() != nil )
  end

  def test_FunctionDefinition_copyConstructor
    o1 = LibSBML::FunctionDefinition.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = LibSBML::FunctionDefinition.new(o1)
    assert( o2.getId() == "c" )
    assert( o2.getMath() != nil )
  end

  def test_InitialAssignment_assignmentOperator
    o1 = LibSBML::InitialAssignment.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::InitialAssignment.new()
    o2 = o1
    assert( o2.getId() == "c" )
  end

  def test_InitialAssignment_clone
    o1 = LibSBML::InitialAssignment.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
  end

  def test_InitialAssignment_copyConstructor
    o1 = LibSBML::InitialAssignment.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::InitialAssignment.new(o1)
    assert( o2.getId() == "c" )
  end

  def test_KineticLaw_assignmentOperator
    o1 = LibSBML::KineticLaw.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("jake")
    o1.addParameter(p)
    assert( o1.getNumParameters() == 1 )
    assert( o1.getParameter(0).getId() == "jake" )
    assert( o1.getId() == "c" )
    o2 = LibSBML::KineticLaw.new()
    o2 = o1
    assert( o2.getNumParameters() == 1 )
    assert( o2.getParameter(0).getId() == "jake" )
    assert( o2.getId() == "c" )
  end

  def test_KineticLaw_clone
    o1 = LibSBML::KineticLaw.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("jake")
    o1.addParameter(p)
    assert( o1.getNumParameters() == 1 )
    assert( o1.getParameter(0).getId() == "jake" )
    assert( o1.getId() == "c" )
    o2 = o1.clone()
    assert( o2.getNumParameters() == 1 )
    assert( o2.getParameter(0).getId() == "jake" )
    assert( o2.getId() == "c" )
  end

  def test_KineticLaw_copyConstructor
    o1 = LibSBML::KineticLaw.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("jake")
    o1.addParameter(p)
    assert( o1.getNumParameters() == 1 )
    assert( o1.getParameter(0).getId() == "jake" )
    assert( o1.getId() == "c" )
    o2 = LibSBML::KineticLaw.new(o1)
    assert( o2.getNumParameters() == 1 )
    assert( o2.getParameter(0).getId() == "jake" )
    assert( o2.getId() == "c" )
  end

  def test_ListOf_assignmentOperator
    o1 = LibSBML::ListOf.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    s = LibSBML::Species.new("species_1")
    o1.append(s)
    o2 = LibSBML::ListOf.new()
    o2 = o1
    assert( o2.size() == 1 )
    assert( o2.getId() == "c" )
    assert( o2.get(0).getId() == "species_1" )
  end

  def test_ListOf_clone
    o1 = LibSBML::ListOf.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    s = LibSBML::Species.new("species_1")
    o1.append(s)
    o2 = o1.clone()
    assert( o2.size() == 1 )
    assert( o2.getId() == "c" )
    assert( o2.get(0).getId() == "species_1" )
  end

  def test_ListOf_copyConstructor
    o1 = LibSBML::ListOf.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    s = LibSBML::Species.new("species_1")
    o1.append(s)
    o2 = LibSBML::ListOf.new(o1)
    assert( o2.size() == 1 )
    assert( o2.getId() == "c" )
    assert( o2.get(0).getId() == "species_1" )
  end

  def test_Model_assignmentOperator
    o1 = LibSBML::Model.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("alex")
    o1.addParameter(p)
    fud = LibSBML::FormulaUnitsData.new()
    o1.addFormulaUnitsData(fud)
    assert( o1.getId() == "c" )
    assert( o1.getNumParameters() == 1 )
    assert( o1.getNumFormulaUnitsData() == 1 )
    assert( o1.getParameter(0).getId() == "alex" )
    o2 = LibSBML::Model.new()
    o2 = o1
    assert( o2.getId() == "c" )
    assert( o2.getNumParameters() == 1 )
    assert( o2.getNumFormulaUnitsData() == 1 )
    assert( o2.getParameter(0).getId() == "alex" )
  end

  def test_Model_clone
    o1 = LibSBML::Model.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("alex")
    o1.addParameter(p)
    fud = LibSBML::FormulaUnitsData.new()
    o1.addFormulaUnitsData(fud)
    assert( o1.getId() == "c" )
    assert( o1.getNumParameters() == 1 )
    assert( o1.getNumFormulaUnitsData() == 1 )
    assert( o1.getParameter(0).getId() == "alex" )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
    assert( o2.getNumParameters() == 1 )
    assert( o2.getNumFormulaUnitsData() == 1 )
    assert( o2.getParameter(0).getId() == "alex" )
  end

  def test_Model_copyConstructor
    o1 = LibSBML::Model.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("alex")
    o1.addParameter(p)
    fud = LibSBML::FormulaUnitsData.new()
    o1.addFormulaUnitsData(fud)
    assert( o1.getId() == "c" )
    assert( o1.getNumParameters() == 1 )
    assert( o1.getNumFormulaUnitsData() == 1 )
    assert( o1.getParameter(0).getId() == "alex" )
    o2 = LibSBML::Model.new(o1)
    assert( o2.getId() == "c" )
    assert( o2.getNumParameters() == 1 )
    assert( o2.getNumFormulaUnitsData() == 1 )
    assert( o2.getParameter(0).getId() == "alex" )
  end

  def test_Parameter_assignmentOperator
    o1 = LibSBML::Parameter.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::Parameter.new()
    o2 = o1
    assert( o2.getId() == "c" )
  end

  def test_Parameter_clone
    o1 = LibSBML::Parameter.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
  end

  def test_Parameter_copyConstructor
    o1 = LibSBML::Parameter.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::Parameter.new(o1)
    assert( o2.getId() == "c" )
  end

  def test_Reaction_assignmentOperator
    o1 = LibSBML::Reaction.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    kl = LibSBML::KineticLaw.new()
    o1.setKineticLaw(kl)
    assert( o1.isSetKineticLaw() == true )
    assert( o1.getKineticLaw() != nil )
    o2 = LibSBML::Reaction.new()
    o2 = o1
    assert( o2.getId() == "c" )
    assert( o2.isSetKineticLaw() == true )
    assert( o2.getKineticLaw() != nil )
  end

  def test_Reaction_clone
    o1 = LibSBML::Reaction.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    kl = LibSBML::KineticLaw.new()
    o1.setKineticLaw(kl)
    assert( o1.isSetKineticLaw() == true )
    assert( o1.getKineticLaw() != nil )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
    assert( o2.isSetKineticLaw() == true )
    assert( o2.getKineticLaw() != nil )
  end

  def test_Reaction_copyConstructor
    o1 = LibSBML::Reaction.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    kl = LibSBML::KineticLaw.new()
    o1.setKineticLaw(kl)
    assert( o1.isSetKineticLaw() == true )
    assert( o1.getKineticLaw() != nil )
    o2 = LibSBML::Reaction.new(o1)
    assert( o2.getId() == "c" )
    assert( o2.isSetKineticLaw() == true )
    assert( o2.getKineticLaw() != nil )
  end

  def test_Rule_assignmentOperator
    o1 = LibSBML::RateRule.new("a")
    assert( o1.getId() == "a" )
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.isSetMath() == true )
    o2 = LibSBML::RateRule.new()
    o2 = o1
    assert( o2.getId() == "a" )
    assert( o2.isSetMath() == true )
  end

  def test_Rule_clone
    o1 = LibSBML::RateRule.new("a")
    assert( o1.getId() == "a" )
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.isSetMath() == true )
    o2 = o1.clone()
    assert( o2.getId() == "a" )
    assert( o2.isSetMath() == true )
  end

  def test_Rule_copyConstructor
    o1 = LibSBML::RateRule.new("a")
    assert( o1.getId() == "a" )
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.isSetMath() == true )
    o2 = LibSBML::Rule.new(o1)
    assert( o2.getId() == "a" )
    assert( o2.isSetMath() == true )
  end

  def test_SBMLDocument_assignmentOperator
    o1 = LibSBML::SBMLDocument.new()
    o1.setLevelAndVersion(2,1)
    assert( o1.getLevel() == 2 )
    assert( o1.getVersion() == 1 )
    o2 = LibSBML::SBMLDocument.new()
    o2 = o1
    assert( o2.getLevel() == 2 )
    assert( o2.getVersion() == 1 )
  end

  def test_SBMLDocument_clone
    o1 = LibSBML::SBMLDocument.new()
    o1.setLevelAndVersion(1,1)
    m = LibSBML::Model.new()
    m.setId("foo")
    o1.setModel(m)
    assert( o1.getLevel() == 1 )
    assert( o1.getVersion() == 1 )
    assert( o1.getModel().getId() == "foo" )
    assert( o1.getModel().getLevel() == 1 )
    assert( o1.getModel().getVersion() == 1 )
    assert( o1.getModel().getSBMLDocument() == o1 )
    o2 = o1.clone()
    assert( o2.getLevel() == 1 )
    assert( o2.getVersion() == 1 )
    assert( o2.getModel().getId() == "foo" )
    assert( o2.getModel().getLevel() == 1 )
    assert( o2.getModel().getVersion() == 1 )
    assert( o2.getModel().getSBMLDocument() == o2 )
  end

  def test_SBMLDocument_copyConstructor
    o1 = LibSBML::SBMLDocument.new()
    o1.setLevelAndVersion(2,1)
    assert( o1.getLevel() == 2 )
    assert( o1.getVersion() == 1 )
    o2 = LibSBML::SBMLDocument.new(o1)
    assert( o2.getLevel() == 2 )
    assert( o2.getVersion() == 1 )
  end

  def test_SpeciesReference_assignmentOperator
    o1 = LibSBML::SpeciesReference.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::SpeciesReference.new()
    o2 = o1
    assert( o2.getId() == "c" )
  end

  def test_SpeciesReference_clone
    o1 = LibSBML::SpeciesReference.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
  end

  def test_SpeciesReference_copyConstructor
    o1 = LibSBML::SpeciesReference.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::SpeciesReference.new(o1)
    assert( o2.getId() == "c" )
  end

  def test_SpeciesType_assignmentOperator
    o1 = LibSBML::SpeciesType.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::SpeciesType.new()
    o2 = o1
    assert( o2.getId() == "c" )
  end

  def test_SpeciesType_clone
    o1 = LibSBML::SpeciesType.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
  end

  def test_SpeciesType_copyConstructor
    o1 = LibSBML::SpeciesType.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::SpeciesType.new(o1)
    assert( o2.getId() == "c" )
  end

  def test_Species_assignmentOperator
    o1 = LibSBML::Species.new()
    o1.setId("c")
    o1.setSpeciesType("c1")
    assert( o1.getId() == "c" )
    assert( o1.getSpeciesType() == "c1" )
    o2 = LibSBML::Species.new()
    o2 = o1
    assert( o2.getId() == "c" )
    assert( o2.getSpeciesType() == "c1" )
  end

  def test_Species_clone
    o1 = LibSBML::Species.new()
    o1.setId("c")
    o1.setSpeciesType("c1")
    assert( o1.getId() == "c" )
    assert( o1.getSpeciesType() == "c1" )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
    assert( o2.getSpeciesType() == "c1" )
  end

  def test_Species_copyConstructor
    o1 = LibSBML::Species.new()
    o1.setId("c")
    o1.setSpeciesType("c1")
    assert( o1.getId() == "c" )
    assert( o1.getSpeciesType() == "c1" )
    o2 = LibSBML::Species.new(o1)
    assert( o2.getId() == "c" )
    assert( o2.getSpeciesType() == "c1" )
  end

  def test_Trigger_assignmentOperator
    o1 = LibSBML::Trigger.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = LibSBML::Trigger.new()
    o2 = o1
    assert( o1.getMath() != nil )
  end

  def test_Trigger_clone
    o1 = LibSBML::Trigger.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = o1.clone()
    assert( o1.getMath() != nil )
  end

  def test_Trigger_copyConstructor
    o1 = LibSBML::Trigger.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert( o1.getMath() != nil )
    o2 = LibSBML::Trigger.new(o1)
    assert( o2.getMath() != nil )
  end

  def test_UnitDefinition_assignmentOperator
    o1 = LibSBML::UnitDefinition.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::UnitDefinition.new()
    o2 = o1
    assert( o2.getId() == "c" )
  end

  def test_UnitDefinition_clone
    o1 = LibSBML::UnitDefinition.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
  end

  def test_UnitDefinition_copyConstructor
    o1 = LibSBML::UnitDefinition.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::UnitDefinition.new(o1)
    assert( o2.getId() == "c" )
  end

  def test_Unit_assignmentOperator
    o1 = LibSBML::Unit.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::Unit.new()
    o2 = o1
    assert( o2.getId() == "c" )
  end

  def test_Unit_clone
    o1 = LibSBML::Unit.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = o1.clone()
    assert( o2.getId() == "c" )
  end

  def test_Unit_copyConstructor
    o1 = LibSBML::Unit.new()
    o1.setId("c")
    assert( o1.getId() == "c" )
    o2 = LibSBML::Unit.new(o1)
    assert( o2.getId() == "c" )
  end

end
