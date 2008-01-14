#
# @file    TestModel.rb
# @brief   SBML Model unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestModel.c
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

class TestModel < Test::Unit::TestCase

  def setup
    @@m = LibSBML::Model.new
    if (@@m == nil)
    end
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
    assert( kl1.getParameter( "k1" ) != k3 )
    assert( kl1.getParameter( "k1" ) != k1 )
    assert( kl1.getParameter( "k2" ) != k4 )
    assert( kl1.getParameter( "k3" ) == nil )
  end

  def test_Model_addCompartment
    @@m.addCompartment(LibSBML::Compartment.new)
    assert( @@m.getNumCompartments == 1 )
  end

  def test_Model_addParameter
    @@m.addParameter(LibSBML::Parameter.new)
    assert( @@m.getNumParameters == 1 )
  end

  def test_Model_addReaction
    @@m.addReaction(LibSBML::Reaction.new)
    assert( @@m.getNumReactions == 1 )
  end

  def test_Model_addRules
    @@m.addRule(LibSBML::AlgebraicRule.new())
    @@m.addRule(LibSBML::AssignmentRule.new())
    @@m.addRule(LibSBML::RateRule.new())
    assert( @@m.getNumRules == 3 )
  end

  def test_Model_addSpecies
    @@m.addSpecies(LibSBML::Species.new)
    assert( @@m.getNumSpecies == 1 )
  end

  def test_Model_add_get_Event
    e1 = LibSBML::Event.new
    e2 = LibSBML::Event.new
    @@m.addEvent(e1)
    @@m.addEvent(e2)
    assert( @@m.getNumEvents == 2 )
    assert( @@m.getEvent(0) != e1 )
    assert( @@m.getEvent(1) != e2 )
    assert( @@m.getEvent(2) == nil )
    assert( @@m.getEvent(-2) == nil )
  end

  def test_Model_add_get_FunctionDefinitions
    fd1 = LibSBML::FunctionDefinition.new
    fd2 = LibSBML::FunctionDefinition.new
    @@m.addFunctionDefinition(fd1)
    @@m.addFunctionDefinition(fd2)
    assert( @@m.getNumFunctionDefinitions == 2 )
    assert( @@m.getFunctionDefinition(0) != fd1 )
    assert( @@m.getFunctionDefinition(1) != fd2 )
    assert( @@m.getFunctionDefinition(2) == nil )
    assert( @@m.getFunctionDefinition(-2) == nil )
  end

  def test_Model_add_get_UnitDefinitions
    ud1 = LibSBML::UnitDefinition.new
    ud2 = LibSBML::UnitDefinition.new
    @@m.addUnitDefinition(ud1)
    @@m.addUnitDefinition(ud2)
    assert( @@m.getNumUnitDefinitions == 2 )
    assert( @@m.getUnitDefinition(0) != ud1 )
    assert( @@m.getUnitDefinition(1) != ud2 )
    assert( @@m.getUnitDefinition(2) == nil )
    assert( @@m.getUnitDefinition(-2) == nil )
  end

  def test_Model_create
    assert( @@m.getTypeCode == LibSBML::SBML_MODEL )
    assert( @@m.getMetaId == "" )
    assert( @@m.getNotes == nil )
    assert( @@m.getAnnotation == nil )
    assert( @@m.getId == "" )
    assert( @@m.getName == "" )
    assert_equal false, @@m.isSetId
    assert_equal false, @@m.isSetName
    assert( @@m.getNumUnitDefinitions == 0 )
    assert( @@m.getNumCompartments == 0 )
    assert( @@m.getNumSpecies == 0 )
    assert( @@m.getNumParameters == 0 )
    assert( @@m.getNumReactions == 0 )
  end

  def test_Model_createAlgebraicRule
    ar = @@m.createAlgebraicRule
    assert( ar != nil )
    assert( @@m.getNumRules == 1 )
    assert( @@m.getRule(0) == ar )
  end

  def test_Model_createAssignmentRule
    ar = @@m.createAssignmentRule
    assert( ar != nil )
    assert( @@m.getNumRules == 1 )
    assert( @@m.getRule(0) == ar )
  end

  def test_Model_createCompartment
    c = @@m.createCompartment
    assert( c != nil )
    assert( @@m.getNumCompartments == 1 )
    assert( @@m.getCompartment(0) == c )
  end

  def test_Model_createCompartmentType
    c = @@m.createCompartmentType
    assert( c != nil )
    assert( @@m.getNumCompartmentTypes == 1 )
    assert( @@m.getCompartmentType(0) == c )
  end

  def test_Model_createConstraint
    c = @@m.createConstraint
    assert( c != nil )
    assert( @@m.getNumConstraints == 1 )
    assert( @@m.getConstraint(0) == c )
  end

  def test_Model_createEvent
    e = @@m.createEvent
    assert( e != nil )
    assert( @@m.getNumEvents == 1 )
    assert( @@m.getEvent(0) == e )
  end

  def test_Model_createEventAssignment
    @@m.createEvent
    @@m.createEvent
    ea = @@m.createEventAssignment
    assert( ea != nil )
    assert( @@m.getNumEvents == 2 )
    e = @@m.getEvent(1)
    assert( e.getNumEventAssignments == 1 )
    assert( e.getEventAssignment(0) == ea )
  end

  def test_Model_createEventAssignment_noEvent
    assert( @@m.getNumEvents == 0 )
    assert( @@m.createEventAssignment == nil )
  end

  def test_Model_createFunctionDefinition
    fd = @@m.createFunctionDefinition
    assert( fd != nil )
    assert( @@m.getNumFunctionDefinitions == 1 )
    assert( @@m.getFunctionDefinition(0) == fd )
  end

  def test_Model_createInitialAssignment
    c = @@m.createInitialAssignment
    assert( c != nil )
    assert( @@m.getNumInitialAssignments == 1 )
    assert( @@m.getInitialAssignment(0) == c )
  end

  def test_Model_createKineticLaw
    @@m.createReaction
    @@m.createReaction
    kl = @@m.createKineticLaw
    assert( kl != nil )
    assert( @@m.getNumReactions == 2 )
    r = @@m.getReaction(0)
    assert( r.getKineticLaw == nil )
    r = @@m.getReaction(1)
    assert( r.getKineticLaw == kl )
  end

  def test_Model_createKineticLawParameter
    @@m.createReaction
    @@m.createReaction
    @@m.createKineticLaw
    p = @@m.createKineticLawParameter
    assert( @@m.getNumReactions == 2 )
    r = @@m.getReaction(0)
    assert( r.getKineticLaw == nil )
    r = @@m.getReaction(1)
    assert( r.getKineticLaw != nil )
    kl = r.getKineticLaw
    assert( kl.getNumParameters == 1 )
    assert( kl.getParameter(0) == p )
  end

  def test_Model_createKineticLawParameter_noKineticLaw
    r = @@m.createReaction
    assert( r.getKineticLaw == nil )
    assert( @@m.createKineticLawParameter == nil )
  end

  def test_Model_createKineticLawParameter_noReaction
    assert( @@m.getNumReactions == 0 )
    assert( @@m.createKineticLawParameter == nil )
  end

  def test_Model_createKineticLaw_alreadyExists
    r = @@m.createReaction
    kl = @@m.createKineticLaw
    assert( @@m.createKineticLaw == kl )
    assert( r.getKineticLaw == kl )
  end

  def test_Model_createKineticLaw_noReaction
    assert( @@m.getNumReactions == 0 )
    assert( @@m.createKineticLaw == nil )
  end

  def test_Model_createModifier
    @@m.createReaction
    @@m.createReaction
    msr = @@m.createModifier
    assert( msr != nil )
    assert( @@m.getNumReactions == 2 )
    r = @@m.getReaction(1)
    assert( r.getNumModifiers == 1 )
    assert( r.getModifier(0) == msr )
  end

  def test_Model_createModifier_noReaction
    assert( @@m.getNumReactions == 0 )
    assert( @@m.createModifier == nil )
  end

  def test_Model_createParameter
    p = @@m.createParameter
    assert( p != nil )
    assert( @@m.getNumParameters == 1 )
    assert( @@m.getParameter(0) == p )
  end

  def test_Model_createProduct
    @@m.createReaction
    @@m.createReaction
    sr = @@m.createProduct
    assert( sr != nil )
    assert( @@m.getNumReactions == 2 )
    r = @@m.getReaction(1)
    assert( r.getNumProducts == 1 )
    assert( r.getProduct(0) == sr )
  end

  def test_Model_createProduct_noReaction
    assert( @@m.getNumReactions == 0 )
    assert( @@m.createProduct == nil )
  end

  def test_Model_createRateRule
    rr = @@m.createRateRule
    assert( rr != nil )
    assert( @@m.getNumRules == 1 )
    assert( @@m.getRule(0) == rr )
  end

  def test_Model_createReactant
    @@m.createReaction
    @@m.createReaction
    sr = @@m.createReactant
    assert( sr != nil )
    assert( @@m.getNumReactions == 2 )
    r = @@m.getReaction(1)
    assert( r.getNumReactants == 1 )
    assert( r.getReactant(0) == sr )
  end

  def test_Model_createReactant_noReaction
    assert( @@m.getNumReactions == 0 )
    assert( @@m.createReactant == nil )
  end

  def test_Model_createReaction
    r = @@m.createReaction
    assert( r != nil )
    assert( @@m.getNumReactions == 1 )
    assert( @@m.getReaction(0) == r )
  end

  def test_Model_createSpecies
    s = @@m.createSpecies
    assert( s != nil )
    assert( @@m.getNumSpecies == 1 )
    assert( @@m.getSpecies(0) == s )
  end

  def test_Model_createSpeciesType
    c = @@m.createSpeciesType
    assert( c != nil )
    assert( @@m.getNumSpeciesTypes == 1 )
    assert( @@m.getSpeciesType(0) == c )
  end

  def test_Model_createUnit
    @@m.createUnitDefinition
    @@m.createUnitDefinition
    u = @@m.createUnit
    assert( u != nil )
    assert( @@m.getNumUnitDefinitions == 2 )
    ud = @@m.getUnitDefinition(1)
    assert( ud.getNumUnits == 1 )
    assert( ud.getUnit(0) == u )
  end

  def test_Model_createUnitDefinition
    ud = @@m.createUnitDefinition
    assert( ud != nil )
    assert( @@m.getNumUnitDefinitions == 1 )
    assert( @@m.getUnitDefinition(0) == ud )
  end

  def test_Model_createUnit_noUnitDefinition
    assert( @@m.getNumUnitDefinitions == 0 )
    assert( @@m.createUnit == nil )
  end

  def test_Model_createWith
    m = LibSBML::Model.new("repressilator", "")
    assert( m.getTypeCode == LibSBML::SBML_MODEL )
    assert( m.getMetaId == "" )
    assert( m.getNotes == nil )
    assert( m.getAnnotation == nil )
    assert( m.getName == "" )
    assert ((  "repressilator" == m.getId ))
    assert_equal true, m.isSetId
    assert( m.getNumUnitDefinitions == 0 )
    assert( m.getNumFunctionDefinitions == 0 )
    assert( m.getNumCompartments == 0 )
    assert( m.getNumSpecies == 0 )
    assert( m.getNumParameters == 0 )
    assert( m.getNumReactions == 0 )
    assert( m.getNumRules == 0 )
    assert( m.getNumConstraints == 0 )
    assert( m.getNumEvents == 0 )
    assert( m.getNumCompartmentTypes == 0 )
    assert( m.getNumSpeciesTypes == 0 )
    assert( m.getNumInitialAssignments == 0 )
  end

  def test_Model_free_NULL
  end

  def test_Model_getCompartment
    c1 = LibSBML::Compartment.new
    c2 = LibSBML::Compartment.new
    c1.setName( "A")
    c2.setName( "B")
    @@m.addCompartment(c1)
    @@m.addCompartment(c2)
    assert( @@m.getNumCompartments == 2 )
    c1 = @@m.getCompartment(0)
    c2 = @@m.getCompartment(1)
    assert ((  "A" == c1.getName ))
    assert ((  "B" == c2.getName ))
  end

  def test_Model_getCompartmentById
    c1 = LibSBML::Compartment.new
    c2 = LibSBML::Compartment.new
    c1.setId( "A" )
    c2.setId( "B" )
    @@m.addCompartment(c1)
    @@m.addCompartment(c2)
    assert( @@m.getNumCompartments == 2 )
    assert( @@m.getCompartment( "A" ) != c1 )
    assert( @@m.getCompartment( "B" ) != c2 )
    assert( @@m.getCompartment( "C" ) == nil )
  end

  def test_Model_getEventById
    e1 = LibSBML::Event.new
    e2 = LibSBML::Event.new
    e1.setId( "e1" )
    e2.setId( "e2" )
    @@m.addEvent(e1)
    @@m.addEvent(e2)
    assert( @@m.getNumEvents == 2 )
    assert( @@m.getEvent( "e1" ) != e1 )
    assert( @@m.getEvent( "e2" ) != e2 )
    assert( @@m.getEvent( "e3" ) == nil )
  end

  def test_Model_getFunctionDefinitionById
    fd1 = LibSBML::FunctionDefinition.new
    fd2 = LibSBML::FunctionDefinition.new
    fd1.setId( "sin" )
    fd2.setId( "cos" )
    @@m.addFunctionDefinition(fd1)
    @@m.addFunctionDefinition(fd2)
    assert( @@m.getNumFunctionDefinitions == 2 )
    assert( @@m.getFunctionDefinition( "sin" ) != fd1 )
    assert( @@m.getFunctionDefinition( "cos" ) != fd2 )
    assert( @@m.getFunctionDefinition( "tan" ) == nil )
  end

  def test_Model_getNumSpeciesWithBoundaryCondition
    s1 = LibSBML::Species.new("s1", "c")
    s2 = LibSBML::Species.new("s2", "c")
    s3 = LibSBML::Species.new("s3", "c")
    s1.setBoundaryCondition(1)
    s2.setBoundaryCondition(0)
    s3.setBoundaryCondition(1)
    assert( @@m.getNumSpecies == 0 )
    assert( @@m.getNumSpeciesWithBoundaryCondition == 0 )
    @@m.addSpecies(s1)
    assert( @@m.getNumSpecies == 1 )
    assert( @@m.getNumSpeciesWithBoundaryCondition == 1 )
    @@m.addSpecies(s2)
    assert( @@m.getNumSpecies == 2 )
    assert( @@m.getNumSpeciesWithBoundaryCondition == 1 )
    @@m.addSpecies(s3)
    assert( @@m.getNumSpecies == 3 )
    assert( @@m.getNumSpeciesWithBoundaryCondition == 2 )
  end

  def test_Model_getParameter
    p1 = LibSBML::Parameter.new
    p2 = LibSBML::Parameter.new
    p1.setName( "Km1")
    p2.setName( "Km2")
    @@m.addParameter(p1)
    @@m.addParameter(p2)
    assert( @@m.getNumParameters == 2 )
    p1 = @@m.getParameter(0)
    p2 = @@m.getParameter(1)
    assert ((  "Km1" == p1.getName ))
    assert ((  "Km2" == p2.getName ))
  end

  def test_Model_getParameterById
    p1 = LibSBML::Parameter.new
    p2 = LibSBML::Parameter.new
    p1.setId( "Km1" )
    p2.setId( "Km2" )
    @@m.addParameter(p1)
    @@m.addParameter(p2)
    assert( @@m.getNumParameters == 2 )
    assert( @@m.getParameter( "Km1" ) != p1 )
    assert( @@m.getParameter( "Km2" ) != p2 )
    assert( @@m.getParameter( "Km3" ) == nil )
  end

  def test_Model_getReaction
    r1 = LibSBML::Reaction.new
    r2 = LibSBML::Reaction.new
    r1.setName( "reaction_1")
    r2.setName( "reaction_2")
    @@m.addReaction(r1)
    @@m.addReaction(r2)
    assert( @@m.getNumReactions == 2 )
    r1 = @@m.getReaction(0)
    r2 = @@m.getReaction(1)
    assert ((  "reaction_1" == r1.getName ))
    assert ((  "reaction_2" == r2.getName ))
  end

  def test_Model_getReactionById
    r1 = LibSBML::Reaction.new
    r2 = LibSBML::Reaction.new
    r1.setId( "reaction_1" )
    r2.setId( "reaction_2" )
    @@m.addReaction(r1)
    @@m.addReaction(r2)
    assert( @@m.getNumReactions == 2 )
    assert( @@m.getReaction( "reaction_1" ) != r1 )
    assert( @@m.getReaction( "reaction_2" ) != r2 )
    assert( @@m.getReaction( "reaction_3" ) == nil )
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
    assert( @@m.getNumRules == 4 )
    ar = @@m.getRule(0)
    scr = @@m.getRule(1)
    cvr = @@m.getRule(2)
    pr = @@m.getRule(3)
    assert ((  "x + 1"         == ar.getFormula ))
    assert ((  "k * t/(1 + k)" == scr.getFormula ))
    assert ((  "0.10 * t"      == cvr.getFormula ))
    assert ((  "k3/k2"         == pr.getFormula ))
  end

  def test_Model_getSpecies
    s1 = LibSBML::Species.new
    s2 = LibSBML::Species.new
    s1.setName( "Glucose"     )
    s2.setName( "Glucose_6_P" )
    @@m.addSpecies(s1)
    @@m.addSpecies(s2)
    assert( @@m.getNumSpecies == 2 )
    s1 = @@m.getSpecies(0)
    s2 = @@m.getSpecies(1)
    assert ((  "Glucose"      == s1.getName ))
    assert ((  "Glucose_6_P"  == s2.getName ))
  end

  def test_Model_getSpeciesById
    s1 = LibSBML::Species.new
    s2 = LibSBML::Species.new
    s1.setId( "Glucose"     )
    s2.setId( "Glucose_6_P" )
    @@m.addSpecies(s1)
    @@m.addSpecies(s2)
    assert( @@m.getNumSpecies == 2 )
    assert( @@m.getSpecies( "Glucose"    ) != s1 )
    assert( @@m.getSpecies( "Glucose_6_P") != s2 )
    assert( @@m.getSpecies( "Glucose2"   ) == nil )
  end

  def test_Model_getUnitDefinition
    ud1 = LibSBML::UnitDefinition.new
    ud2 = LibSBML::UnitDefinition.new
    ud1.setName( "mmls"   )
    ud2.setName( "volume" )
    @@m.addUnitDefinition(ud1)
    @@m.addUnitDefinition(ud2)
    assert( @@m.getNumUnitDefinitions == 2 )
    ud1 = @@m.getUnitDefinition(0)
    ud2 = @@m.getUnitDefinition(1)
    assert ((  "mmls"    == ud1.getName ))
    assert ((  "volume"  == ud2.getName ))
  end

  def test_Model_getUnitDefinitionById
    ud1 = LibSBML::UnitDefinition.new
    ud2 = LibSBML::UnitDefinition.new
    ud1.setId( "mmls"   )
    ud2.setId( "volume" )
    @@m.addUnitDefinition(ud1)
    @@m.addUnitDefinition(ud2)
    assert( @@m.getNumUnitDefinitions == 2 )
    assert( @@m.getUnitDefinition( "mmls"       ) != ud1 )
    assert( @@m.getUnitDefinition( "volume"     ) != ud2 )
    assert( @@m.getUnitDefinition( "rototillers") == nil )
  end

  def test_Model_setId
    id = "Branch"
    @@m.setId(id)
    assert (( id == @@m.getId ))
    assert_equal true, @@m.isSetId
    if (@@m.getId == id)
    end
    @@m.setId(@@m.getId)
    assert (( id == @@m.getId ))
    @@m.setId("")
    assert_equal false, @@m.isSetId
    if (@@m.getId != nil)
    end
    @@m.setId(id)
    @@m.unsetId
    assert_equal false, @@m.isSetId
  end

  def test_Model_setName
    name = "My Branch Model"
    @@m.setName(name)
    assert (( name == @@m.getName ))
    assert_equal true, @@m.isSetName
    if (@@m.getName == name)
    end
    @@m.setName(@@m.getName)
    assert (( name == @@m.getName ))
    @@m.setName("")
    assert_equal false, @@m.isSetName
    if (@@m.getName != nil)
    end
  end

  def test_Model_setgetModelHistory
    history = LibSBML::ModelHistory.new
    mc = LibSBML::ModelCreator.new
    mc.setFamilyName( "Keating")
    mc.setGivenName( "Sarah")
    mc.setEmail( "sbml-team@caltech.edu")
    mc.setOrganisation( "UH")
    history.addCreator(mc)
    assert( @@m.isSetModelHistory == false )
    @@m.setModelHistory(history)
    assert( @@m.isSetModelHistory == true )
    newMC = history.getCreator(0)
    assert( newMC != nil )
    assert ((  "Keating" == newMC.getFamilyName ))
    assert ((  "Sarah" == newMC.getGivenName ))
    assert ((  "sbml-team@caltech.edu" == newMC.getEmail ))
    assert ((  "UH" == newMC.getOrganisation ))
    @@m.unsetModelHistory
    assert( @@m.isSetModelHistory == false )
  end

end
