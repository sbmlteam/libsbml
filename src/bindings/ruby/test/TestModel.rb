#
# This file was converted from libsbml/src/sbml/test/TestModel.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestModel < Test::Unit::TestCase

  def test_Model_createConstraint
    c = @@m.createConstraint
    assert_not_equal nil, c
    assert_equal 1, @@m.getNumConstraints
    assert_equal c, @@m.getConstraint(0)
  end

  def test_Model_createModifier
    @@m.createReaction
    @@m.createReaction
    msr = @@m.createModifier
    assert_not_equal nil, msr
    assert_equal 2, @@m.getNumReactions
    r = @@m.getReaction(1)
    assert_equal 1, r.getNumModifiers
    assert_equal msr, r.getModifier(0)
  end

  def test_Model_getParameterById
    p1 = LibSBML::Parameter.new
    p2 = LibSBML::Parameter.new
    p1.setId( "Km1" )
    p2.setId( "Km2" )
    @@m.addParameter(p1)
    @@m.addParameter(p2)
    assert_equal 2, @@m.getNumParameters
    assert_not_equal p1, @@m.getParameter( "Km1" )
    assert_not_equal p2, @@m.getParameter( "Km2" )
    assert_equal nil, @@m.getParameter( "Km3" )
  end

  def test_Model_createCompartment
    c = @@m.createCompartment
    assert_not_equal nil, c
    assert_equal 1, @@m.getNumCompartments
    assert_equal c, @@m.getCompartment(0)
  end

  def test_Model_createWith
    m = LibSBML::Model.new("repressilator", "")
    assert_equal LibSBML::SBML_MODEL, m.getTypeCode
    assert_equal "", m.getMetaId
    assert_equal nil, m.getNotes
    assert_equal nil, m.getAnnotation
    assert_equal "", m.getName
    assert_equal  "repressilator",m.getId
    assert_equal true, m.isSetId
    assert_equal 0, m.getNumUnitDefinitions
    assert_equal 0, m.getNumFunctionDefinitions
    assert_equal 0, m.getNumCompartments
    assert_equal 0, m.getNumSpecies
    assert_equal 0, m.getNumParameters
    assert_equal 0, m.getNumReactions
    assert_equal 0, m.getNumRules
    assert_equal 0, m.getNumConstraints
    assert_equal 0, m.getNumEvents
    assert_equal 0, m.getNumCompartmentTypes
    assert_equal 0, m.getNumSpeciesTypes
    assert_equal 0, m.getNumInitialAssignments
  end

  def test_Model_getNumSpeciesWithBoundaryCondition
    s1 = LibSBML::Species.new("s1", "c")
    s2 = LibSBML::Species.new("s2", "c")
    s3 = LibSBML::Species.new("s3", "c")
    s1.setBoundaryCondition(1)
    s2.setBoundaryCondition(0)
    s3.setBoundaryCondition(1)
    assert_equal 0, @@m.getNumSpecies
    assert_equal 0, @@m.getNumSpeciesWithBoundaryCondition
    @@m.addSpecies(s1)
    assert_equal 1, @@m.getNumSpecies
    assert_equal 1, @@m.getNumSpeciesWithBoundaryCondition
    @@m.addSpecies(s2)
    assert_equal 2, @@m.getNumSpecies
    assert_equal 1, @@m.getNumSpeciesWithBoundaryCondition
    @@m.addSpecies(s3)
    assert_equal 3, @@m.getNumSpecies
    assert_equal 2, @@m.getNumSpeciesWithBoundaryCondition
  end

  def test_Model_createKineticLawParameter_noKineticLaw
    r = @@m.createReaction
    assert_equal nil, r.getKineticLaw
    assert_equal nil, @@m.createKineticLawParameter
  end

  def test_Model_createKineticLaw_noReaction
    assert_equal 0, @@m.getNumReactions
    assert_equal nil, @@m.createKineticLaw
  end

  def test_Model_createReaction
    r = @@m.createReaction
    assert_not_equal nil, r
    assert_equal 1, @@m.getNumReactions
    assert_equal r, @@m.getReaction(0)
  end

  def test_Model_getParameter
    p1 = LibSBML::Parameter.new
    p2 = LibSBML::Parameter.new
    p1.setName( "Km1")
    p2.setName( "Km2")
    @@m.addParameter(p1)
    @@m.addParameter(p2)
    assert_equal 2, @@m.getNumParameters
    p1 = @@m.getParameter(0)
    p2 = @@m.getParameter(1)
    assert_equal  "Km1",p1.getName
    assert_equal  "Km2",p2.getName
  end

  def test_Model_createAlgebraicRule
    ar = @@m.createAlgebraicRule
    assert_not_equal nil, ar
    assert_equal 1, @@m.getNumRules
    assert_equal ar, @@m.getRule(0)
  end

  def test_Model_getReaction
    r1 = LibSBML::Reaction.new
    r2 = LibSBML::Reaction.new
    r1.setName( "reaction_1")
    r2.setName( "reaction_2")
    @@m.addReaction(r1)
    @@m.addReaction(r2)
    assert_equal 2, @@m.getNumReactions
    r1 = @@m.getReaction(0)
    r2 = @@m.getReaction(1)
    assert_equal  "reaction_1",r1.getName
    assert_equal  "reaction_2",r2.getName
  end

  def test_Model_createCompartmentType
    c = @@m.createCompartmentType
    assert_not_equal nil, c
    assert_equal 1, @@m.getNumCompartmentTypes
    assert_equal c, @@m.getCompartmentType(0)
  end

  def test_Model_getUnitDefinition
    ud1 = LibSBML::UnitDefinition.new
    ud2 = LibSBML::UnitDefinition.new
    ud1.setName( "mmls"   )
    ud2.setName( "volume" )
    @@m.addUnitDefinition(ud1)
    @@m.addUnitDefinition(ud2)
    assert_equal 2, @@m.getNumUnitDefinitions
    ud1 = @@m.getUnitDefinition(0)
    ud2 = @@m.getUnitDefinition(1)
    assert_equal  "mmls"   ,ud1.getName
    assert_equal  "volume" ,ud2.getName
  end

  def test_Model_createProduct
    @@m.createReaction
    @@m.createReaction
    sr = @@m.createProduct
    assert_not_equal nil, sr
    assert_equal 2, @@m.getNumReactions
    r = @@m.getReaction(1)
    assert_equal 1, r.getNumProducts
    assert_equal sr, r.getProduct(0)
  end

  def test_Model_addRules
    @@m.addRule(LibSBML::AlgebraicRule.new())
    @@m.addRule(LibSBML::AssignmentRule.new())
    @@m.addRule(LibSBML::RateRule.new())
    assert_equal 3, @@m.getNumRules
  end

  def test_Model_create
    assert_equal LibSBML::SBML_MODEL, @@m.getTypeCode
    assert_equal "", @@m.getMetaId
    assert_equal nil, @@m.getNotes
    assert_equal nil, @@m.getAnnotation
    assert_equal "", @@m.getId
    assert_equal "", @@m.getName
    assert_equal false, @@m.isSetId
    assert_equal false, @@m.isSetName
    assert_equal 0, @@m.getNumUnitDefinitions
    assert_equal 0, @@m.getNumCompartments
    assert_equal 0, @@m.getNumSpecies
    assert_equal 0, @@m.getNumParameters
    assert_equal 0, @@m.getNumReactions
  end

  def test_Model_getCompartment
    c1 = LibSBML::Compartment.new
    c2 = LibSBML::Compartment.new
    c1.setName( "A")
    c2.setName( "B")
    @@m.addCompartment(c1)
    @@m.addCompartment(c2)
    assert_equal 2, @@m.getNumCompartments
    c1 = @@m.getCompartment(0)
    c2 = @@m.getCompartment(1)
    assert_equal  "A",c1.getName
    assert_equal  "B",c2.getName
  end

  def test_Model_createReactant
    @@m.createReaction
    @@m.createReaction
    sr = @@m.createReactant
    assert_not_equal nil, sr
    assert_equal 2, @@m.getNumReactions
    r = @@m.getReaction(1)
    assert_equal 1, r.getNumReactants
    assert_equal sr, r.getReactant(0)
  end

  def test_Model_createKineticLawParameter_noReaction
    assert_equal 0, @@m.getNumReactions
    assert_equal nil, @@m.createKineticLawParameter
  end

  def test_Model_add_get_FunctionDefinitions
    fd1 = LibSBML::FunctionDefinition.new
    fd2 = LibSBML::FunctionDefinition.new
    @@m.addFunctionDefinition(fd1)
    @@m.addFunctionDefinition(fd2)
    assert_equal 2, @@m.getNumFunctionDefinitions
    assert_not_equal fd1, @@m.getFunctionDefinition(0)
    assert_not_equal fd2, @@m.getFunctionDefinition(1)
    assert_equal nil, @@m.getFunctionDefinition(2)
    assert_equal nil, @@m.getFunctionDefinition(-2)
  end

  def test_Model_getSpeciesById
    s1 = LibSBML::Species.new
    s2 = LibSBML::Species.new
    s1.setId( "Glucose"     )
    s2.setId( "Glucose_6_P" )
    @@m.addSpecies(s1)
    @@m.addSpecies(s2)
    assert_equal 2, @@m.getNumSpecies
    assert_not_equal s1, @@m.getSpecies( "Glucose"    )
    assert_not_equal s2, @@m.getSpecies( "Glucose_6_P")
    assert_equal nil, @@m.getSpecies( "Glucose2"   )
  end

  def test_Model_getFunctionDefinitionById
    fd1 = LibSBML::FunctionDefinition.new
    fd2 = LibSBML::FunctionDefinition.new
    fd1.setId( "sin" )
    fd2.setId( "cos" )
    @@m.addFunctionDefinition(fd1)
    @@m.addFunctionDefinition(fd2)
    assert_equal 2, @@m.getNumFunctionDefinitions
    assert_not_equal fd1, @@m.getFunctionDefinition( "sin" )
    assert_not_equal fd2, @@m.getFunctionDefinition( "cos" )
    assert_equal nil, @@m.getFunctionDefinition( "tan" )
  end

  def test_Model_addSpecies
    @@m.addSpecies(LibSBML::Species.new)
    assert_equal 1, @@m.getNumSpecies
  end

  def test_KineticLaw_getParameterById
    k1 = LibSBML::Parameter.new
    k2 = LibSBML::Parameter.new
    k1.setId( "k1")
    k2.setId( "k2")
    k1.setValue(3.14)
    k2.setValue(2.72)
    @@m.addParameter(k1)
    @@m.addParameter(k2)
    r1 = LibSBML::Reaction.new
    r1.setId( "reaction_1" )
    kl = LibSBML::KineticLaw.new("k1 * X0")
    k3 = LibSBML::Parameter.new
    k4 = LibSBML::Parameter.new
    k3.setId( "k1")
    k4.setId( "k2")
    k3.setValue(2.72)
    k4.setValue(3.14)
    kl.addParameter(k3)
    kl.addParameter(k4)
    r1.setKineticLaw(kl)
    @@m.addReaction(r1)
    kl1 = @@m.getReaction(0).getKineticLaw
    assert_not_equal k3, kl1.getParameter( "k1" )
    assert_not_equal k1, kl1.getParameter( "k1" )
    assert_not_equal k4, kl1.getParameter( "k2" )
    assert_equal nil, kl1.getParameter( "k3" )
  end

  def test_Model_addReaction
    @@m.addReaction(LibSBML::Reaction.new)
    assert_equal 1, @@m.getNumReactions
  end

  def test_Model_createKineticLawParameter
    @@m.createReaction
    @@m.createReaction
    @@m.createKineticLaw
    p = @@m.createKineticLawParameter
    assert_equal 2, @@m.getNumReactions
    r = @@m.getReaction(0)
    assert_equal nil, r.getKineticLaw
    r = @@m.getReaction(1)
    assert_not_equal nil, r.getKineticLaw
    kl = r.getKineticLaw
    assert_equal 1, kl.getNumParameters
    assert_equal p, kl.getParameter(0)
  end

  def test_Model_createKineticLaw
    @@m.createReaction
    @@m.createReaction
    kl = @@m.createKineticLaw
    assert_not_equal nil, kl
    assert_equal 2, @@m.getNumReactions
    r = @@m.getReaction(0)
    assert_equal nil, r.getKineticLaw
    r = @@m.getReaction(1)
    assert_equal kl, r.getKineticLaw
  end

  def test_Model_createAssignmentRule
    ar = @@m.createAssignmentRule
    assert_not_equal nil, ar
    assert_equal 1, @@m.getNumRules
    assert_equal ar, @@m.getRule(0)
  end

  def test_Model_createUnit
    @@m.createUnitDefinition
    @@m.createUnitDefinition
    u = @@m.createUnit
    assert_not_equal nil, u
    assert_equal 2, @@m.getNumUnitDefinitions
    ud = @@m.getUnitDefinition(1)
    assert_equal 1, ud.getNumUnits
    assert_equal u, ud.getUnit(0)
  end

  def test_Model_createSpecies
    s = @@m.createSpecies
    assert_not_equal nil, s
    assert_equal 1, @@m.getNumSpecies
    assert_equal s, @@m.getSpecies(0)
  end

  def test_Model_createEventAssignment_noEvent
    assert_equal 0, @@m.getNumEvents
    assert_equal nil, @@m.createEventAssignment
  end

  def test_Model_createKineticLaw_alreadyExists
    r = @@m.createReaction
    kl = @@m.createKineticLaw
    assert_equal kl, @@m.createKineticLaw
    assert_equal kl, r.getKineticLaw
  end

  def test_Model_getSpecies
    s1 = LibSBML::Species.new
    s2 = LibSBML::Species.new
    s1.setName( "Glucose"     )
    s2.setName( "Glucose_6_P" )
    @@m.addSpecies(s1)
    @@m.addSpecies(s2)
    assert_equal 2, @@m.getNumSpecies
    s1 = @@m.getSpecies(0)
    s2 = @@m.getSpecies(1)
    assert_equal  "Glucose"     ,s1.getName
    assert_equal  "Glucose_6_P" ,s2.getName
  end

  def test_Model_setId
    id = "Branch"
    @@m.setId(id)
    assert_equal id,@@m.getId
    assert_equal true, @@m.isSetId
      @@m.setId(@@m.getId)
      assert_equal id,@@m.getId
      @@m.setId("")
      assert_equal false, @@m.isSetId
        @@m.setId(id)
        @@m.unsetId
        assert_equal false, @@m.isSetId
  end

  def test_Model_setName
    name = "My Branch Model"
    @@m.setName(name)
    assert_equal name,@@m.getName
    assert_equal true, @@m.isSetName
      @@m.setName(@@m.getName)
      assert_equal name,@@m.getName
      @@m.setName("")
      assert_equal false, @@m.isSetName
  end

  def test_Model_createSpeciesType
    c = @@m.createSpeciesType
    assert_not_equal nil, c
    assert_equal 1, @@m.getNumSpeciesTypes
    assert_equal c, @@m.getSpeciesType(0)
  end

  def test_Model_addParameter
    @@m.addParameter(LibSBML::Parameter.new)
    assert_equal 1, @@m.getNumParameters
  end

  def test_Model_createEvent
    e = @@m.createEvent
    assert_not_equal nil, e
    assert_equal 1, @@m.getNumEvents
    assert_equal e, @@m.getEvent(0)
  end

  def test_Model_createInitialAssignment
    c = @@m.createInitialAssignment
    assert_not_equal nil, c
    assert_equal 1, @@m.getNumInitialAssignments
    assert_equal c, @@m.getInitialAssignment(0)
  end

  def test_Model_createModifier_noReaction
    assert_equal 0, @@m.getNumReactions
    assert_equal nil, @@m.createModifier
  end

  def test_Model_getRules
    ar = LibSBML::AlgebraicRule.new()
    scr = LibSBML::AssignmentRule.new()
    cvr = LibSBML::AssignmentRule.new()
    pr = LibSBML::AssignmentRule.new()
    ar.setFormula( "x + 1"         )
    scr.setFormula( "k * t/(1 + k)" )
    cvr.setFormula( "0.10 * t"      )
    pr.setFormula( "k3/k2"         )
    @@m.addRule(ar)
    @@m.addRule(scr)
    @@m.addRule(cvr)
    @@m.addRule(pr)
    assert_equal 4, @@m.getNumRules
    ar = @@m.getRule(0)
    scr = @@m.getRule(1)
    cvr = @@m.getRule(2)
    pr = @@m.getRule(3)
    assert_equal  "x + 1"        ,ar.getFormula
    assert_equal  "k * t/(1 + k)",scr.getFormula
    assert_equal  "0.10 * t"     ,cvr.getFormula
    assert_equal  "k3/k2"        ,pr.getFormula
  end

  def test_Model_createUnitDefinition
    ud = @@m.createUnitDefinition
    assert_not_equal nil, ud
    assert_equal 1, @@m.getNumUnitDefinitions
    assert_equal ud, @@m.getUnitDefinition(0)
  end

  def test_Model_addCompartment
    @@m.addCompartment(LibSBML::Compartment.new)
    assert_equal 1, @@m.getNumCompartments
  end

  def test_Model_createFunctionDefinition
    fd = @@m.createFunctionDefinition
    assert_not_equal nil, fd
    assert_equal 1, @@m.getNumFunctionDefinitions
    assert_equal fd, @@m.getFunctionDefinition(0)
  end

  def test_Model_getUnitDefinitionById
    ud1 = LibSBML::UnitDefinition.new
    ud2 = LibSBML::UnitDefinition.new
    ud1.setId( "mmls"   )
    ud2.setId( "volume" )
    @@m.addUnitDefinition(ud1)
    @@m.addUnitDefinition(ud2)
    assert_equal 2, @@m.getNumUnitDefinitions
    assert_not_equal ud1, @@m.getUnitDefinition( "mmls"       )
    assert_not_equal ud2, @@m.getUnitDefinition( "volume"     )
    assert_equal nil, @@m.getUnitDefinition( "rototillers")
  end

  def test_Model_add_get_UnitDefinitions
    ud1 = LibSBML::UnitDefinition.new
    ud2 = LibSBML::UnitDefinition.new
    @@m.addUnitDefinition(ud1)
    @@m.addUnitDefinition(ud2)
    assert_equal 2, @@m.getNumUnitDefinitions
    assert_not_equal ud1, @@m.getUnitDefinition(0)
    assert_not_equal ud2, @@m.getUnitDefinition(1)
    assert_equal nil, @@m.getUnitDefinition(2)
    assert_equal nil, @@m.getUnitDefinition(-2)
  end

  def test_Model_setgetModelHistory
    history = LibSBML::ModelHistory.new
    mc = LibSBML::ModelCreator.new
    mc.setFamilyName( "Keating")
    mc.setGivenName( "Sarah")
    mc.setEmail( "sbml-team@caltech.edu")
    mc.setOrganisation( "UH")
    history.addCreator(mc)
    assert_equal false, @@m.isSetModelHistory
    @@m.setModelHistory(history)
    assert_equal true, @@m.isSetModelHistory
    newMC = history.getCreator(0)
    assert_not_equal nil, newMC
    assert_equal  "Keating",newMC.getFamilyName
    assert_equal  "Sarah",newMC.getGivenName
    assert_equal  "sbml-team@caltech.edu",newMC.getEmail
    assert_equal  "UH",newMC.getOrganisation
    @@m.unsetModelHistory
    assert_equal false, @@m.isSetModelHistory
  end

  def test_Model_createProduct_noReaction
    assert_equal 0, @@m.getNumReactions
    assert_equal nil, @@m.createProduct
  end

  def test_Model_getEventById
    e1 = LibSBML::Event.new
    e2 = LibSBML::Event.new
    e1.setId( "e1" )
    e2.setId( "e2" )
    @@m.addEvent(e1)
    @@m.addEvent(e2)
    assert_equal 2, @@m.getNumEvents
    assert_not_equal e1, @@m.getEvent( "e1" )
    assert_not_equal e2, @@m.getEvent( "e2" )
    assert_equal nil, @@m.getEvent( "e3" )
  end

  def test_Model_createUnit_noUnitDefinition
    assert_equal 0, @@m.getNumUnitDefinitions
    assert_equal nil, @@m.createUnit
  end

  def test_Model_createReactant_noReaction
    assert_equal 0, @@m.getNumReactions
    assert_equal nil, @@m.createReactant
  end

  def test_Model_createEventAssignment
    @@m.createEvent
    @@m.createEvent
    ea = @@m.createEventAssignment
    assert_not_equal nil, ea
    assert_equal 2, @@m.getNumEvents
    e = @@m.getEvent(1)
    assert_equal 1, e.getNumEventAssignments
    assert_equal ea, e.getEventAssignment(0)
  end

  def test_Model_free_NULL
  end

  def test_Model_getReactionById
    r1 = LibSBML::Reaction.new
    r2 = LibSBML::Reaction.new
    r1.setId( "reaction_1" )
    r2.setId( "reaction_2" )
    @@m.addReaction(r1)
    @@m.addReaction(r2)
    assert_equal 2, @@m.getNumReactions
    assert_not_equal r1, @@m.getReaction( "reaction_1" )
    assert_not_equal r2, @@m.getReaction( "reaction_2" )
    assert_equal nil, @@m.getReaction( "reaction_3" )
  end

  def test_Model_createParameter
    p = @@m.createParameter
    assert_not_equal nil, p
    assert_equal 1, @@m.getNumParameters
    assert_equal p, @@m.getParameter(0)
  end

  def setup
    @@m = LibSBML::Model.new
  end

  def test_Model_add_get_Event
    e1 = LibSBML::Event.new
    e2 = LibSBML::Event.new
    @@m.addEvent(e1)
    @@m.addEvent(e2)
    assert_equal 2, @@m.getNumEvents
    assert_not_equal e1, @@m.getEvent(0)
    assert_not_equal e2, @@m.getEvent(1)
    assert_equal nil, @@m.getEvent(2)
    assert_equal nil, @@m.getEvent(-2)
  end

  def test_Model_getCompartmentById
    c1 = LibSBML::Compartment.new
    c2 = LibSBML::Compartment.new
    c1.setId( "A" )
    c2.setId( "B" )
    @@m.addCompartment(c1)
    @@m.addCompartment(c2)
    assert_equal 2, @@m.getNumCompartments
    assert_not_equal c1, @@m.getCompartment( "A" )
    assert_not_equal c2, @@m.getCompartment( "B" )
    assert_equal nil, @@m.getCompartment( "C" )
  end

  def test_Model_createRateRule
    rr = @@m.createRateRule
    assert_not_equal nil, rr
    assert_equal 1, @@m.getNumRules
    assert_equal rr, @@m.getRule(0)
  end

end
