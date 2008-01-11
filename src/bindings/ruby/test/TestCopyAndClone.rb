#
# This file was converted from libsbml/src/sbml/test/TestCopyAndClone.cpp
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestCopyAndClone < Test::Unit::TestCase

  def test_Constraint_copyConstructor
    o1 = LibSBML::Constraint.new
    o1.setMetaId("c")
    assert_equal "c", o1.getMetaId()
    math = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    message = LibSBML::XMLNode.new
    o1.setMath(math)
    o1.setMessage(message)
    assert_not_equal nil, o1.getMath()
    assert_not_equal nil, o1.getMessage()
    o2 = LibSBML::Constraint.new(o1)
    assert_equal "c", o2.getMetaId()
    assert_not_equal nil, o1.getMath()
    assert_not_equal nil, o1.getMessage()
  end

  def test_Model_assignmentOperator
    o1 = LibSBML::Model.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("alex")
    o1.addParameter(p)
    fud = LibSBML::FormulaUnitsData.new()
    o1.addFormulaUnitsData(fud)
    assert_equal "c", o1.getId()
    assert_equal 1, o1.getNumParameters()
    assert_equal 1, o1.getNumFormulaUnitsData()
    assert_equal "alex", o1.getParameter(0).getId()
    o2 = LibSBML::Model.new()
    o2 = o1
    assert_equal "c", o2.getId()
    assert_equal 1, o2.getNumParameters()
    assert_equal 1, o2.getNumFormulaUnitsData()
    assert_equal "alex", o2.getParameter(0).getId()
  end

  def test_UnitDefinition_clone
    o1 = LibSBML::UnitDefinition.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
  end

  def test_SBMLDocument_clone
    o1 = LibSBML::SBMLDocument.new()
    o1.setLevelAndVersion(1,1)
    m = LibSBML::Model.new()
    m.setId("foo")
    o1.setModel(m)
    assert_equal 1, o1.getLevel()
    assert_equal 1, o1.getVersion()
    assert_equal "foo", o1.getModel().getId()
    assert_equal 1, o1.getModel().getLevel()
    assert_equal 1, o1.getModel().getVersion()
    assert_equal o1, o1.getModel().getSBMLDocument()
    o2 = o1.clone()
    assert_equal 1, o2.getLevel()
    assert_equal 1, o2.getVersion()
    assert_equal "foo", o2.getModel().getId()
    assert_equal 1, o2.getModel().getLevel()
    assert_equal 1, o2.getModel().getVersion()
    assert_equal o2, o2.getModel().getSBMLDocument()
  end

  def test_Delay_clone
    o1 = LibSBML::Delay.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = o1.clone()
    assert_not_equal nil, o1.getMath()
  end

  def test_Unit_clone
    o1 = LibSBML::Unit.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
  end

  def test_FunctionDefinition_assignmentOperator
    o1 = LibSBML::FunctionDefinition.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = LibSBML::FunctionDefinition.new();
    o2 = o1
    assert_equal "c", o2.getId()
    assert_not_equal nil, o2.getMath()
  end

  def test_Constraint_clone
    o1 = LibSBML::Constraint.new()
    o1.setMetaId("c")
    assert_equal "c", o1.getMetaId()
    math = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    message = LibSBML::XMLNode.new()
    o1.setMath(math)
    o1.setMessage(message)
    assert_not_equal nil, o1.getMath()
    assert_not_equal nil, o1.getMessage()
    o2 = o1.clone()
    assert_equal "c", o2.getMetaId()
    assert_not_equal nil, o1.getMath()
    assert_not_equal nil, o1.getMessage()
  end

  def test_KineticLaw_copyConstructor
    o1 = LibSBML::KineticLaw.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("jake")
    o1.addParameter(p)
    assert_equal 1, o1.getNumParameters()
    assert_equal "jake", o1.getParameter(0).getId()
    assert_equal "c", o1.getId()
    o2 = LibSBML::KineticLaw.new(o1)
    assert_equal 1, o2.getNumParameters()
    assert_equal "jake", o2.getParameter(0).getId()
    assert_equal "c", o2.getId()
  end

  def test_SpeciesReference_assignmentOperator
    o1 = LibSBML::SpeciesReference.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::SpeciesReference.new();
    o2 = o1
    assert_equal "c", o2.getId()
  end

  def test_SpeciesReference_copyConstructor
    o1 = LibSBML::SpeciesReference.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::SpeciesReference.new(o1)
    assert_equal "c", o2.getId()
  end

  def test_Species_copyConstructor
    o1 = LibSBML::Species.new()
    o1.setId("c")
    o1.setSpeciesType("c1")
    assert_equal "c", o1.getId()
    assert_equal "c1", o1.getSpeciesType()
    o2 = LibSBML::Species.new(o1)
    assert_equal "c", o2.getId()
    assert_equal "c1", o2.getSpeciesType()
  end

  def test_Trigger_copyConstructor
    o1 = LibSBML::Trigger.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = LibSBML::Trigger.new(o1)
    assert_not_equal nil, o2.getMath()
  end

  def test_CompartmentType_assignmentOperator
    o1 = LibSBML::CompartmentType.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::CompartmentType.new();
    o2 = o1
    assert_equal "c", o2.getId()
  end

  def test_InitialAssignment_copyConstructor
    o1 = LibSBML::InitialAssignment.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::InitialAssignment.new(o1)
    assert_equal "c", o2.getId()
  end

  def test_Compartment_copyConstructor
    o1 = LibSBML::Compartment.new()
    o1.setId("c")
    o1.setOutside("c2")
    assert_equal "c", o1.getId()
    assert_equal "c2", o1.getOutside()
    o2 = LibSBML::Compartment.new(o1)
    assert_equal "c", o2.getId()
    assert_equal "c2", o2.getOutside()
  end

  def test_Trigger_assignmentOperator
    o1 = LibSBML::Trigger.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = LibSBML::Trigger.new()
    o2 = o1
    assert_not_equal nil, o1.getMath()
  end

  def test_Rule_copyConstructor
    o1 = LibSBML::RateRule.new("a")
    assert_equal "a", o1.getId()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_equal true, o1.isSetMath()
    o2 = LibSBML::Rule.new(o1)
    assert_equal "a", o2.getId()
    assert_equal true, o2.isSetMath()
  end

  def test_Parameter_clone
    o1 = LibSBML::Parameter.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
  end

  def test_Parameter_copyConstructor
    o1 = LibSBML::Parameter.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::Parameter.new(o1)
    assert_equal "c", o2.getId()
  end

  def test_Event_copyConstructor
    o1 = LibSBML::Event.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::Event.new(o1)
    assert_equal "c", o2.getId()
  end

  def test_EventAssignment_copyConstructor
    o1 = LibSBML::EventAssignment.new()
    o1.setVariable("c2")
    assert_equal "c2", o1.getVariable()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = LibSBML::EventAssignment.new(o1)
    assert_equal "c2", o2.getVariable()
    assert_not_equal nil, o2.getMath()
  end

  def test_Constraint_assignmentOperator
    o1 = LibSBML::Constraint.new()
    o1.setMetaId("c")
    assert_equal "c", o1.getMetaId()
    math = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    message = LibSBML::XMLNode.new()
    o1.setMath(math)
    o1.setMessage(message)
    assert_not_equal nil, o1.getMath()
    assert_not_equal nil, o1.getMessage()
    o2 = LibSBML::Constraint.new();
    o2 = o1
    assert_equal "c", o2.getMetaId()
    assert_not_equal nil, o1.getMath()
    assert_not_equal nil, o1.getMessage()
  end

  def test_Model_copyConstructor
    o1 = LibSBML::Model.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("alex")
    o1.addParameter(p)
    fud = LibSBML::FormulaUnitsData.new()
    o1.addFormulaUnitsData(fud)
    assert_equal "c", o1.getId()
    assert_equal 1, o1.getNumParameters()
    assert_equal 1, o1.getNumFormulaUnitsData()
    assert_equal "alex", o1.getParameter(0).getId()
    o2 = LibSBML::Model.new(o1)
    assert_equal "c", o2.getId()
    assert_equal 1, o2.getNumParameters()
    assert_equal 1, o2.getNumFormulaUnitsData()
    assert_equal "alex", o2.getParameter(0).getId()
  end

  def test_Species_assignmentOperator
    o1 = LibSBML::Species.new()
    o1.setId("c")
    o1.setSpeciesType("c1")
    assert_equal "c", o1.getId()
    assert_equal "c1", o1.getSpeciesType()
    o2 = LibSBML::Species.new();
    o2 = o1
    assert_equal "c", o2.getId()
    assert_equal "c1", o2.getSpeciesType()
  end

  def test_SpeciesType_assignmentOperator
    o1 = LibSBML::SpeciesType.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::SpeciesType.new();
    o2 = o1
    assert_equal "c", o2.getId()
  end

  def test_SpeciesReference_clone
    o1 = LibSBML::SpeciesReference.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
  end

  def test_Species_clone
    o1 = LibSBML::Species.new()
    o1.setId("c")
    o1.setSpeciesType("c1")
    assert_equal "c", o1.getId()
    assert_equal "c1", o1.getSpeciesType()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
    assert_equal "c1", o2.getSpeciesType()
  end

  def test_Parameter_assignmentOperator
    o1 = LibSBML::Parameter.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::Parameter.new();
    o2 = o1
    assert_equal "c", o2.getId()
  end

  def test_Unit_assignmentOperator
    o1 = LibSBML::Unit.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::Unit.new();
    o2 = o1
    assert_equal "c", o2.getId()
  end

  def test_InitialAssignment_clone
    o1 = LibSBML::InitialAssignment.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
  end

  def test_Model_clone
    o1 = LibSBML::Model.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("alex")
    o1.addParameter(p)
    fud = LibSBML::FormulaUnitsData.new()
    o1.addFormulaUnitsData(fud)
    assert_equal "c", o1.getId()
    assert_equal 1, o1.getNumParameters()
    assert_equal 1, o1.getNumFormulaUnitsData()
    assert_equal "alex", o1.getParameter(0).getId()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
    assert_equal 1, o2.getNumParameters()
    assert_equal 1, o2.getNumFormulaUnitsData()
    assert_equal "alex", o2.getParameter(0).getId()
  end

  def test_Reaction_copyConstructor
    o1 = LibSBML::Reaction.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    kl = LibSBML::KineticLaw.new()
    o1.setKineticLaw(kl)
    assert_equal true, o1.isSetKineticLaw()
    assert_not_equal nil, o1.getKineticLaw()
    o2 = LibSBML::Reaction.new(o1)
    assert_equal "c", o2.getId()
    assert_equal true, o2.isSetKineticLaw()
    assert_not_equal nil, o2.getKineticLaw()
  end

  def test_FunctionDefinition_clone
    o1 = LibSBML::FunctionDefinition.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
    assert_not_equal nil, o2.getMath()
  end

  def test_Compartment_clone
    o1 = LibSBML::Compartment.new()
    o1.setId("c")
    o1.setOutside("c2")
    assert_equal "c", o1.getId()
    assert_equal "c2", o1.getOutside()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
    assert_equal "c2", o2.getOutside()
  end

  def test_InitialAssignment_assignmentOperator
    o1 = LibSBML::InitialAssignment.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::InitialAssignment.new();
    o2 = o1
    assert_equal "c", o2.getId()
  end

  def test_Unit_copyConstructor
    o1 = LibSBML::Unit.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::Unit.new(o1)
    assert_equal "c", o2.getId()
  end

  def test_KineticLaw_assignmentOperator
    o1 = LibSBML::KineticLaw.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("jake")
    o1.addParameter(p)
    assert_equal 1, o1.getNumParameters()
    assert_equal "jake", o1.getParameter(0).getId()
    assert_equal "c", o1.getId()
    o2 = LibSBML::KineticLaw.new();
    o2 = o1
    assert_equal 1, o2.getNumParameters()
    assert_equal "jake", o2.getParameter(0).getId()
    assert_equal "c", o2.getId()
  end

  def test_SBMLDocument_copyConstructor
    o1 = LibSBML::SBMLDocument.new()
    o1.setLevelAndVersion(2,1)
    assert_equal 2, o1.getLevel()
    assert_equal 1, o1.getVersion()
    o2 = LibSBML::SBMLDocument.new(o1)
    assert_equal 2, o2.getLevel()
    assert_equal 1, o2.getVersion()
  end

  def test_Trigger_clone
    o1 = LibSBML::Trigger.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = o1.clone()
    assert_not_equal nil, o1.getMath()
  end

  def test_CompartmentType_copyConstructor
    o1 = LibSBML::CompartmentType.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::CompartmentType.new(o1)
    assert_equal "c", o2.getId()
  end

  def test_ListOf_assignmentOperator
    o1 = LibSBML::ListOf.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    s = LibSBML::Species.new("species_1")
    o1.append(s)
    o2 = LibSBML::ListOf.new();
    o2 = o1
    assert_equal 1, o2.size()
    assert_equal "c", o2.getId()
    assert_equal "species_1", o2.get(0).getId()
  end

  def test_UnitDefinition_assignmentOperator
    o1 = LibSBML::UnitDefinition.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::UnitDefinition.new();
    o2 = o1
    assert_equal "c", o2.getId()
  end

  def test_CompartmentType_clone
    o1 = LibSBML::CompartmentType.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
  end

  def test_EventAssignment_assignmentOperator
    o1 = LibSBML::EventAssignment.new()
    o1.setVariable("c2")
    assert_equal "c2", o1.getVariable()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = LibSBML::EventAssignment.new();
    o2 = o1
    assert_equal "c2", o2.getVariable()
    assert_not_equal nil, o2.getMath()
  end

  def test_EventAssignment_clone
    o1 = LibSBML::EventAssignment.new()
    o1.setVariable("c2")
    assert_equal "c2", o1.getVariable()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = o1.clone()
    assert_equal "c2", o2.getVariable()
    assert_not_equal nil, o2.getMath()
  end

  def test_Reaction_clone
    o1 = LibSBML::Reaction.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    kl = LibSBML::KineticLaw.new()
    o1.setKineticLaw(kl)
    assert_equal true, o1.isSetKineticLaw()
    assert_not_equal nil, o1.getKineticLaw()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
    assert_equal true, o2.isSetKineticLaw()
    assert_not_equal nil, o2.getKineticLaw()
  end

  def test_SpeciesType_clone
    o1 = LibSBML::SpeciesType.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
  end

  def test_Event_assignmentOperator
    o1 = LibSBML::Event.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::Event.new();
    o2 = o1
    assert_equal "c", o2.getId()
  end

  def test_SBMLDocument_assignmentOperator
    o1 = LibSBML::SBMLDocument.new()
    o1.setLevelAndVersion(2,1)
    assert_equal 2, o1.getLevel()
    assert_equal 1, o1.getVersion()
    o2 = LibSBML::SBMLDocument.new();
    o2 = o1
    assert_equal 2, o2.getLevel()
    assert_equal 1, o2.getVersion()
  end

  def test_SpeciesType_copyConstructor
    o1 = LibSBML::SpeciesType.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::SpeciesType.new(o1)
    assert_equal "c", o2.getId()
  end

  def test_ListOf_clone
    o1 = LibSBML::ListOf.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    s = LibSBML::Species.new("species_1")
    o1.append(s)
    o2 = o1.clone()
    assert_equal 1, o2.size()
    assert_equal "c", o2.getId()
    assert_equal "species_1", o2.get(0).getId()
  end

  def test_Rule_clone
    o1 = LibSBML::RateRule.new("a")
    assert_equal "a", o1.getId()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_equal true, o1.isSetMath()
    o2 = o1.clone()
    assert_equal "a", o2.getId()
    assert_equal true, o2.isSetMath()
  end

  def test_Reaction_assignmentOperator
    o1 = LibSBML::Reaction.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    kl = LibSBML::KineticLaw.new()
    o1.setKineticLaw(kl)
    assert_equal true, o1.isSetKineticLaw()
    assert_not_equal nil, o1.getKineticLaw()
    o2 = LibSBML::Reaction.new();
    o2 = o1
    assert_equal "c", o2.getId()
    assert_equal true, o2.isSetKineticLaw()
    assert_not_equal nil, o2.getKineticLaw()
  end

  def test_UnitDefinition_copyConstructor
    o1 = LibSBML::UnitDefinition.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = LibSBML::UnitDefinition.new(o1)
    assert_equal "c", o2.getId()
  end

  def test_Compartment_assignmentOperator
    o1 = LibSBML::Compartment.new()
    o1.setId("c")
    o1.setOutside("c2")
    assert_equal "c", o1.getId()
    assert_equal "c2", o1.getOutside()
    o2 = LibSBML::Compartment.new();
    o2 = o1
    assert_equal "c", o2.getId()
    assert_equal "c2", o2.getOutside()
  end

  def test_Delay_assignmentOperator
    o1 = LibSBML::Delay.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = LibSBML::Delay.new()
    o2 = o1
    assert_not_equal nil, o1.getMath()
  end

  def test_Delay_copyConstructor
    o1 = LibSBML::Delay.new()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = LibSBML::Delay.new(o1)
    assert_not_equal nil, o2.getMath()
  end

  def test_KineticLaw_clone
    o1 = LibSBML::KineticLaw.new()
    o1.setId("c")
    p = LibSBML::Parameter.new("jake")
    o1.addParameter(p)
    assert_equal 1, o1.getNumParameters()
    assert_equal "jake", o1.getParameter(0).getId()
    assert_equal "c", o1.getId()
    o2 = o1.clone()
    assert_equal 1, o2.getNumParameters()
    assert_equal "jake", o2.getParameter(0).getId()
    assert_equal "c", o2.getId()
  end

  def test_Event_clone
    o1 = LibSBML::Event.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    o2 = o1.clone()
    assert_equal "c", o2.getId()
  end

  def test_Rule_assignmentOperator
    o1 = LibSBML::RateRule.new("a")
    assert_equal "a", o1.getId()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_equal true, o1.isSetMath()
    o2 = LibSBML::RateRule.new()
    o2 = o1
    assert_equal "a", o2.getId()
    assert_equal true, o2.isSetMath()
  end

  def test_FunctionDefinition_copyConstructor
    o1 = LibSBML::FunctionDefinition.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    node = LibSBML::ASTNode.new(LibSBML::AST_CONSTANT_PI)
    o1.setMath(node)
    assert_not_equal nil, o1.getMath()
    o2 = LibSBML::FunctionDefinition.new(o1)
    assert_equal "c", o2.getId()
    assert_not_equal nil, o2.getMath()
  end

  def test_ListOf_copyConstructor
    o1 = LibSBML::ListOf.new()
    o1.setId("c")
    assert_equal "c", o1.getId()
    s = LibSBML::Species.new("species_1")
    o1.append(s)
    o2 = LibSBML::ListOf.new(o1)
    assert_equal 1, o2.size()
    assert_equal "c", o2.getId()
    assert_equal "species_1", o2.get(0).getId()
  end

end
