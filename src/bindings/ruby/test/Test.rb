require 'test/unit'
require 'libSBML'

class TestSBML < Test::Unit::TestCase

  @@sbaseClass = [
    "Compartment",
    "CompartmentType",
    "Constraint",
    "Delay",
    "Event",
    "EventAssignment",
    "FormulaUnitsData",
    "FunctionDefinition",
    "InitialAssignment",
    "KineticLaw",
    "ListOf",
    "ListOfCompartmentTypes",
    "ListOfCompartments",
    "ListOfConstraints",
    "ListOfEventAssignments",
    "ListOfEvents",
    "ListOfFunctionDefinitions",
    "ListOfInitialAssignments",
    "ListOfParameters",
    "ListOfReactions",
    "ListOfRules",
    "ListOfSpecies",
    "ListOfSpeciesReferences",
    "ListOfSpeciesTypes",
    "ListOfUnitDefinitions",
    "ListOfUnits",
    "ModifierSpeciesReference",
    "Model",
    "Parameter",
    "AlgebraicRule",
    "AssignmentRule",
    "RateRule",
    "Reaction",
    "SBMLDocument",
    "Species",
    "SpeciesReference",
    "SpeciesType",
    "StoichiometryMath",
    "Trigger",
    "Unit",
    "UnitDefinition"
  ]

  def test_equality
    lo = LibSBML::ListOf.new
    c1 = LibSBML::Compartment.new
    c1.setId("c1")
    lo.append(c1)
    c1c = lo[0]

    assert_equal     c1, c1c
    assert_not_same  c1, c1c

    c2 = LibSBML::Compartment.new
    c2.setId("c2")

    assert_not_equal c1, c2
    
  end

  def test_ImplicitDowncastOfRule
   model = LibSBML::Model.new
   rule  = LibSBML::AssignmentRule.new("x", "1 + 1")

   model.addRule(rule)
   assert_equal 1, model.getNumRules

   rule = model.getRule(0)
   assert_instance_of LibSBML::AssignmentRule, rule
   assert_equal 'x', rule.getVariable
   assert_equal '1 + 1', rule.getFormula

  end

  def test_ImplicitDowncastOfSBase
    species = LibSBML::Species.new

    species.setMetaId('foo')
    assert_equal 'foo', species.getMetaId
    
    lst = LibSBML::ListOf.new
    assert_equal 0, lst.size
    
    lst.append(species)
    assert_equal 1, lst.size
    
    species = lst.get(0)

    assert_instance_of LibSBML::Species, species
    assert_equal 'foo', species.getMetaId
    
    noElement = lst.get(51)
    assert_nil noElement
  end

  def test_Species
    species = LibSBML::Species.new

    # Species_create

    assert_equal LibSBML::SBML_SPECIES, species.getTypeCode()

    assert_equal "", species.getMetaId()
    assert_equal "", species.getId()
    assert_equal "", species.getName()
    assert_equal "", species.getCompartment()
    assert_equal "", species.getSubstanceUnits()
    assert_equal "", species.getSpatialSizeUnits()

    assert_nil species.getNotes()
    assert_nil species.getAnnotation()

    assert_equal 0, species.getCharge()
    assert_equal 0.0, species.getInitialAmount()
    assert_equal 0.0, species.getInitialConcentration()

    assert !species.getHasOnlySubstanceUnits()
    assert !species.getBoundaryCondition()
    assert !species.getConstant()
    assert !species.isSetId()
    assert !species.isSetName()
    assert !species.isSetCompartment()
    assert !species.isSetInitialAmount()
    assert !species.isSetInitialConcentration()
    assert !species.isSetSubstanceUnits()
    assert !species.isSetSpatialSizeUnits()
    assert !species.isSetUnits()
    assert !species.isSetCharge()

    # Species_createWith
    
    species = LibSBML::Species.new("Ca","Calcium");

    assert_equal "Ca",      species.getId()
    assert_equal "Calcium", species.getName()
    assert species.isSetId()
    assert species.isSetName()

    # Species_setMetaId

    species.setMetaId('foo')
    assert_equal 'foo', species.getMetaId()

    species.setMetaId(species.getMetaId())
    assert_equal 'foo', species.getMetaId()

    # Species_setId

    id = "Glucose"

    species.setId(id)
    assert_equal id, species.getId()

    species.setId(species.getId())
    assert_equal id, species.getId()

    species.unsetId()
    assert !species.isSetId()
    assert_equal "", species.getId()

    # Species_setName    

    name = "So Sweet"

    species.setName(name)
    assert_equal name, species.getName()

    species.setName(species.getName())
    assert_equal name, species.getName()

    species.unsetName()
    assert !species.isSetName()
    assert_equal "", species.getName()

    # Species_setCompartment    

    compartment = "cell"

    species.setCompartment(compartment)
    assert_equal compartment, species.getCompartment()

    species.setCompartment(species.getCompartment())
    assert_equal compartment, species.getCompartment()

    species.setCompartment("")
    assert !species.isSetCompartment()
    assert_equal "", species.getCompartment()

    # Species_setInitialAmount 

    species.setInitialAmount(1.2)
    assert species.isSetInitialAmount()
    assert !species.isSetInitialConcentration()
    assert_equal 1.2, species.getInitialAmount()

    # Species_setInitialConcentration 

    species.setInitialConcentration(3.4)
    assert species.isSetInitialConcentration()
    assert !species.isSetInitialAmount()
    assert_equal 3.4, species.getInitialConcentration()

    # Species_setSpatialSizeUnits

    units = "volume"

    species.setSpatialSizeUnits(units)
    assert_equal units, species.getSpatialSizeUnits()
    assert species.isSetSpatialSizeUnits()

    species.setSpatialSizeUnits(species.getSpatialSizeUnits())
    assert_equal units, species.getSpatialSizeUnits()

    species.unsetSpatialSizeUnits()
    assert_equal "", species.getSpatialSizeUnits()

    # Species_setUnits

    units = "mole"

    species.setUnits(units)
    assert_equal units, species.getUnits()
    assert species.isSetUnits()

    species.setUnits(species.getSubstanceUnits())
    assert_equal units, species.getUnits()

    species.unsetUnits()
    assert_equal "", species.getSubstanceUnits()

  end

  def test_Reaction

    reaction = LibSBML::Reaction.new()

    # Reaction_create

    assert_equal LibSBML::SBML_REACTION, reaction.getTypeCode()
    assert_equal "",  reaction.getMetaId()
    assert_equal "",  reaction.getId()
    assert_equal "",  reaction.getName()
    
    assert_nil reaction.getNotes()
    assert_nil reaction.getAnnotation()
    assert_nil reaction.getKineticLaw()
    
    assert reaction.getReversible()
    assert !reaction.getFast()
    assert !reaction.isSetId()
    assert !reaction.isSetName()
    assert !reaction.isSetKineticLaw()
    
    assert_equal 0, reaction.getNumReactants()
    assert_equal 0, reaction.getNumProducts()
    assert_equal 0, reaction.getNumModifiers()

    # Reaction_createWith

    reaction = LibSBML::Reaction.new("r1", "", LibSBML::KineticLaw.new(), false)

    assert_equal "r1", reaction.getId()
    assert !reaction.getReversible()
    assert reaction.isSetKineticLaw()

    # Reaction_setId

    id = "J1"

    reaction.setId(id)
    assert_equal id, reaction.getId()

    reaction.setId(reaction.getId())
    assert_equal id, reaction.getId()

    reaction.unsetId()
    assert !reaction.isSetId()

    # Reaction_setName

    name = "MapK Cascade";

    reaction.setName(name)
    assert_equal name, reaction.getName()

    reaction.setName(reaction.getName())
    assert_equal name, reaction.getName()

    reaction.unsetName()
    assert !reaction.isSetName()

    # Reaction_addReactant/getReactant

    sr1 = LibSBML::SpeciesReference.new()
    sr1.setSpecies("R1")
    reaction.addReactant(sr1)
    assert_equal 1, reaction.getNumReactants()
    assert_equal "R1", reaction.getReactant(0).getSpecies()
    assert_equal "R1", reaction.getReactant("R1").getSpecies()
    assert_equal sr1, reaction.getReactant("R1")
    assert_not_same sr1, reaction.getReactant("R1")

    # Reaction_addProduct/getProduct

    sr2 = LibSBML::SpeciesReference.new()
    sr2.setSpecies("P1")
    reaction.addProduct(sr2)
    assert_equal 1, reaction.getNumProducts()
    assert_equal "P1", reaction.getProduct(0).getSpecies()
    assert_equal "P1", reaction.getProduct("P1").getSpecies()
    assert_equal sr2, reaction.getProduct("P1")
    assert_not_same sr2, reaction.getProduct("P1")

    # Reaction_addModifier/getModifier

    msr1 = LibSBML::ModifierSpeciesReference.new()
    msr1.setSpecies("M1")
    reaction.addModifier(msr1)
    assert_equal 1, reaction.getNumModifiers()
    assert_equal "M1", reaction.getModifier(0).getSpecies()
    assert_equal "M1", reaction.getModifier("M1").getSpecies()
    assert_equal msr1, reaction.getModifier("M1")
    assert_not_same msr1, reaction.getModifier("M1")

   # Reaction_KineticLaw

    kineticLaw = LibSBML::KineticLaw.new("1 + 1")
    reaction.setKineticLaw(kineticLaw)
    assert_equal '1 + 1',  reaction.getKineticLaw().getFormula()
    assert_equal kineticLaw, reaction.getKineticLaw()
    assert_not_same kineticLaw, reaction.getKineticLaw()

  end

  def testListOf

    # ListOf_create

    lo = LibSBML::ListOf.new

    assert_equal 0, lo.size
    assert_equal LibSBML::SBML_LIST_OF, lo.getTypeCode()
    assert_equal "",  lo.getMetaId()
    assert_equal "",  lo.getId()
    assert_equal "",  lo.getName()
    
    assert_nil lo.getNotes()
    assert_nil lo.getAnnotation()
    
    # ListOf_append

    c1 = LibSBML::Compartment.new("c1")
    lo.append(c1)

    assert_equal 1, lo.size
    assert_equal    c1, lo[0]
    assert_not_same c1, lo[0]

    c2 = LibSBML::Compartment.new("c2")
    lo.append(c2)

    assert_equal 2, lo.size
    assert_equal    c2, lo[1]
    assert_not_same c2, lo[1]
    assert_not_equal lo[0], lo[1]

    # ListOf_remove
    
    c1Clone = lo.remove(0)
    assert_equal 1, lo.size
    assert_nil lo[1]
    assert_equal    c1, c1Clone
    assert_not_same c1, c1Clone

    c2Clone = lo.remove(0)
    assert_equal 0, lo.size
    assert_nil lo[0]
    assert_equal    c2, c2Clone
    assert_not_same c2, c2Clone

  end

  def test_Model
    model = LibSBML::Model.new()
    
    # Model_create

    assert_equal LibSBML::SBML_MODEL, model.getTypeCode()
    assert_equal "",  model.getMetaId()
    assert_equal "",  model.getId()
    assert_equal "",  model.getName()
    
    assert_nil model.getNotes()
    assert_nil model.getAnnotation()

    assert !model.isSetId()
    assert !model.isSetName()

    assert_equal 0, model.getNumUnitDefinitions()
    assert_equal 0, model.getNumCompartments()   
    assert_equal 0, model.getNumSpecies()        
    assert_equal 0, model.getNumParameters()     
    assert_equal 0, model.getNumReactions()      
    
    # Model_createWith

    model = LibSBML::Model.new("repressilator","repressilator")

    assert_equal "repressilator", model.getId()
    assert_equal "repressilator", model.getName()
    assert model.isSetId()
    assert model.isSetName()

    # Model_setId

    id = "Branch"

    model.setId(id)
    assert_equal id, model.getId()
    model.setId(model.getId())
    assert_equal id, model.getId()
    model.unsetId()
    assert !model.isSetId()

    # Model_setname

    name = "My Branch Model"

    model.setName(name)
    assert_equal name, model.getName()
    model.setName(model.getName())
    assert_equal name, model.getName()
    model.unsetName()
    assert !model.isSetName()

    # Model_ModelHistory

    mh = LibSBML::ModelHistory.new()
    mc = LibSBML::ModelCreator.new()

    mc.setFamilyName("Keating");
    mc.setGivenName("Sarah");
    mc.setEmail("sbml-team@caltech.edu");
    mc.setOrganisation("UH");

    mh.addCreator(mc)

    assert !model.isSetModelHistory()
    model.setModelHistory(mh)
    assert model.isSetModelHistory()

    newMC = mh.getCreator(0)

    assert_equal "Keating", newMC.getFamilyName()
    assert_equal "Sarah", newMC.getGivenName()
    assert_equal "sbml-team@caltech.edu", newMC.getEmail()
    assert_equal "UH", newMC.getOrganisation()

    model.unsetModelHistory()
    assert !model.isSetModelHistory()

    # Model_FunctionDefinition

    fd1 = model.createFunctionDefinition()
    fd2 = LibSBML::FunctionDefinition.new("fd2")
    model.addFunctionDefinition(fd2)
    assert_equal 2, model.getNumFunctionDefinitions()
    assert_equal fd1, model.getFunctionDefinition(0)
    assert_equal fd2, model.getFunctionDefinition("fd2")
    
    # Model_UnitDefinition

    ud1 = model.createUnitDefinition()
    ud2 = LibSBML::UnitDefinition.new("ud2")
    model.addUnitDefinition(ud2)
    assert_equal 2, model.getNumUnitDefinitions()
    assert_equal ud1, model.getUnitDefinition(0)
    assert_equal ud2, model.getUnitDefinition("ud2")

    # Model_Unit

    u = model.createUnit()
    assert u
    ud2 = model.getUnitDefinition("ud2")
    assert_equal 1, ud2.getNumUnits()
    assert_equal u, ud2.getUnit(0)
    
    # Model_Compartment

    c1 = model.createCompartment()
    c2 = LibSBML::Compartment.new("c2")
    model.addCompartment(c2)
    assert_equal 2, model.getNumCompartments()
    assert_equal c1, model.getCompartment(0)
    assert_equal c2, model.getCompartment("c2")

    # Model_CompartmentType

    ct1 = model.createCompartmentType()
    ct2 = LibSBML::CompartmentType.new("ct2")
    model.addCompartmentType(ct2)
    assert_equal 2, model.getNumCompartmentTypes()
    assert_equal ct1, model.getCompartmentType(0)
    assert_equal ct2, model.getCompartmentType("ct2")

    # Model_SpeciesType

    st1 = model.createSpeciesType()
    st2 = LibSBML::SpeciesType.new("st2")
    model.addSpeciesType(st2)
    assert_equal 2, model.getNumSpeciesTypes()
    assert_equal st1, model.getSpeciesType(0)
    assert_equal st2, model.getSpeciesType("st2")

    # Model_InitialAssignment

    ia1 = model.createInitialAssignment()
    ia2 = LibSBML::InitialAssignment.new("ia2")
    model.addInitialAssignment(ia2)
    assert_equal 2, model.getNumInitialAssignments()
    assert_equal ia1, model.getInitialAssignment(0)
    assert_equal ia2, model.getInitialAssignment("ia2")

    # Model_Constraint

    cst1 = model.createConstraint()
    cst2 = LibSBML::Constraint.new()
    model.addConstraint(cst2)
    assert_equal 2, model.getNumConstraints()
    assert_equal cst1, model.getConstraint(0)
    assert_equal cst2, model.getConstraint(1)

    # Model_Species

    s1 = model.createSpecies()
    s2 = LibSBML::Species.new("s2")
    s1.setId("s1")
    model.addSpecies(s2)
    assert_equal 2, model.getNumSpecies()
    assert_equal s1, model.getSpecies(0)
    assert_equal s2, model.getSpecies("s2")

    # Model_Parameter

    p1 = model.createParameter()
    p2 = LibSBML::Parameter.new("p2")
    model.addParameter(p2)
    assert_equal 2, model.getNumParameters()
    assert_equal p1, model.getParameter(0)
    assert_equal p2, model.getParameter("p2")

    # Model_AssignmentRule

    asr1 = model.createAssignmentRule()
    asr2 = LibSBML::AssignmentRule.new("x","1+1")
    model.addRule(asr2)
    assert_equal 2, model.getNumRules()
    assert_equal asr1, model.getRule(0)
    assert_equal asr2, model.getRule("x")

    # Model_RateRule

    rr1 = model.createRateRule()
    rr2 = LibSBML::RateRule.new("y","1 + 1")
    model.addRule(rr2)
    assert_equal 4, model.getNumRules()
    assert_equal rr1, model.getRule(2)
    assert_equal rr2, model.getRule("y")

    # Model_AlgebraicRule

    alr1 = model.createAlgebraicRule()
    alr2 = LibSBML::AlgebraicRule.new("1 + 1")
    model.addRule(alr2)
    assert_equal 6, model.getNumRules()
    assert_equal alr1, model.getRule(4)
    assert_equal alr2, model.getRule(5)

    # Model_Reaction

    r1 = model.createReaction()
    r2 = LibSBML::Reaction.new("r2")
    model.addReaction(r2)
    assert_equal 2, model.getNumReactions()
    assert_equal r1, model.getReaction(0)
    assert_equal r2, model.getReaction("r2")

    # Model_Reactant

    ra1 = model.createReactant()
    ra1.setSpecies("s1")

    assert_equal 1,   model.getReaction("r2").getNumReactants()
    assert_equal ra1, model.getReaction("r2").getReactant("s1")
    assert_nil model.getReaction(0).getReactant("s1")

    # Model_Product

    pd1 = model.createProduct()
    pd1.setSpecies("s2")

    assert_equal 1,   model.getReaction("r2").getNumProducts()
    assert_equal pd1, model.getReaction("r2").getProduct("s2")
    assert_nil model.getReaction(0).getProduct("s2")

    # Model_Modifier

    md1 = model.createModifier()
    md1.setSpecies("s2")

    assert_equal 1,   model.getReaction("r2").getNumModifiers()
    assert_equal md1, model.getReaction("r2").getModifier("s2")
    assert_nil model.getReaction(0).getModifier("s2")

    # Model_KineticLaw

    kl1 = model.createKineticLaw()
    assert_equal kl1, model.getReaction("r2").getKineticLaw()
    assert_nil model.getReaction(0).getKineticLaw()

    # Model_KineticLawParameter

    kp1 = model.createKineticLawParameter()
    assert_equal kp1, model.getReaction("r2").getKineticLaw().getParameter(0)

    # Model_Event

    e1 = model.createEvent()
    e2 = LibSBML::Event.new("e2")
    model.addEvent(e2)
    assert_equal 2, model.getNumEvents()
    assert_equal e1, model.getEvent(0)
    assert_equal e2, model.getEvent("e2")

    # Model_EventAssignment

    ea1 = model.createEventAssignment()
    assert_equal ea1, model.getEvent("e2").getEventAssignment(0)

  end

  def test_Clone()
    @@sbaseClass.each {|x| checkClone(x)}
  end

  def checkClone(name)
     obj = eval("LibSBML::"+name).new
     cobj= obj.clone()
     assert_instance_of eval("LibSBML::"+name), cobj
     assert_equal    obj, cobj
     assert_not_same obj, cobj
  end

end
